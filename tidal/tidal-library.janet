#!/usr/bin/env janet
# Mirror a Tidal library with tiddl. See `tidal-library --help`.

(def default-library "/media/music/tidal")
(def default-staging "/media/tidal-staging")

(def usage
  (string/trim
    ``
usage: tidal-library [options] list|sync

Mirror a Tidal library into a music folder with tiddl, filing each album the
way the rest of the library is laid out:

  Artist/artist/album[year]digital media/NN-title.flac
  Artist/artist/album[year]digital media/DD/NN-title.flac   (multi-disc)

Albums: saved albums, plus the whole album of every saved track and of every
track in the playlists in $TIDAL_PLAYLISTS (space-separated UUIDs).

commands:
  list   print the IDs of the albums to mirror
  sync   download and file every album not yet in the library

options:
  --library DIR   where finished albums go (default: /media/music/tidal)
  --staging DIR   where tiddl downloads to first; must be on the library's
                  filesystem (default: /media/tidal-staging)
  -v, --verbose   also show tiddl's own output and the API paging
  -h, --help      show this help

Re-running sync is safe: albums already done or already in the library are
skipped, a failed album keeps its finished tracks for the next try, and an
album interrupted mid-download starts over. Only one sync runs at a time.
Done and failed album IDs, and a log per failure, are kept in
$XDG_STATE_HOME/tidal-library/.

Needs tiddl (logged in with `tiddl auth login`), curl, jq and GNU sed. Track
quality and cover embedding come from tiddl's own config.
``))

(def state
  (string (or (os/getenv "XDG_STATE_HOME")
              (string (os/getenv "HOME") "/.local/state"))
          "/tidal-library"))
(def playlists
  (filter |(not (empty? $)) (string/split " " (or (os/getenv "TIDAL_PLAYLISTS") ""))))

# tiddl downloads into staging under names carrying what file-album needs
(def template
  (string "{album.id}§{album.artist}§{album.title}§{album.date:%Y}"
          "/{item.volume:02d}/{item.number:02d}§{item.title_version}"))

(var verbose false)

(defn die [& msg]
  (eprint ;msg)
  (os/exit 1))

(defn note
  "Print only with --verbose."
  [& msg]
  (when verbose (eprint ;msg)))

(defn sh
  "Run a command found in PATH, feeding it `input` if given, and return its
  output. Errors if the command fails."
  [input & args]
  (def proc (os/spawn args :p {:in (if input :pipe) :out :pipe}))
  (when input
    (ev/write (proc :in) input)
    (ev/close (proc :in)))
  (def out (or (ev/read (proc :out) :all) ""))
  (def code (os/proc-wait proc))
  (unless (zero? code)
    (error (string/format "%s exited with %d" (first args) code)))
  out)

(defn lines [s]
  (filter |(not (empty? $)) (string/split "\n" s)))

(defn rm-rf [path]
  (sh nil "rm" "-rf" path))

(defn mkdirs [path]
  (var dir "")
  (each part (lines (string/replace-all "/" "\n" path))
    (set dir (string dir "/" part))
    (os/mkdir dir)))

(defn duration [secs]
  (def s (math/floor secs))
  (cond
    (>= s 86400) (string/format "%dd %dh" (div s 86400) (div (% s 86400) 3600))
    (>= s 3600) (string/format "%dh %02dm" (div s 3600) (div (% s 3600) 60))
    (>= s 60) (string/format "%dm %02ds" (div s 60) (% s 60))
    (string/format "%ds" s)))

(defn lowercase
  "Lowercase strings with sed: Janet's own string/ascii-lower skips non-ASCII
  letters."
  [strs]
  (string/split "\n" (string/trimr (sh (string/join strs "\n")
                                       "env" "LC_ALL=C.UTF-8" "sed" `s/.*/\L&/`)
                                   "\n")))

(defn clean
  "tiddl's cleanup of one path segment (tiddl/core/utils/format.py)."
  [s]
  (as-> s s
    (peg/replace-all '(set `\/:"*?<>|`) "" s)
    (peg/replace-all '(at-least 2 ".") "." s)
    (string/trimr s " .")
    (peg/replace-all '(at-least 2 :s) " " s)
    (string/trim s)
    (if (empty? s) "_" s)))

(defn album-dir
  "Where an album goes in the library, from tiddl's names for it."
  [library artist album year]
  (def [artist-lc album-lc] (lowercase [artist album]))
  (string library "/" artist "/" artist-lc "/" album-lc "[" year "]digital media"))

## Tidal API

(defn tidal
  "Log in through tiddl's saved session; return the user ID and a function
  that GETs a Tidal API path."
  []
  (sh nil "tiddl" "auth" "refresh")
  (def [token user country]
    (lines (sh nil "jq" "-r" ".token, .user_id, .country_code"
               (string (os/getenv "HOME") "/.tiddl/auth.json"))))
  (defn get [path]
    (def sep (if (string/find "?" path) "&" "?"))
    # The token goes in through stdin so it never shows up in `ps`
    (sh (string "Authorization: Bearer " token)
        "curl" "-sf" "-m" "30" "-H" "@-"
        (string "https://api.tidal.com/v1/" path sep "countryCode=" country)))
  [user get])

(defn album-ids
  "Album IDs from every page of a paginated Tidal endpoint; `pick` is a jq
  filter that extracts one from an item."
  [get endpoint pick]
  (def ids @[])
  (var offset 0)
  (var total 1)
  (while (< offset total)
    (def page (get (string endpoint "?limit=100&offset=" offset)))
    (def found
      (lines (sh page "jq" "-r"
                 (string ".totalNumberOfItems, (.items[] | " pick ")"))))
    (set total (scan-number (first found)))
    (array/concat ids (slice found 1))
    (+= offset 100)
    (note "  " endpoint ": " (min offset total) "/" total)
    (ev/sleep 0.25))
  ids)

(defn list-albums [[user get]]
  (distinct
    [;(album-ids get (string "users/" user "/favorites/albums") ".item.id")
     ;(album-ids get (string "users/" user "/favorites/tracks") ".item.album.id")
     ;(mapcat |(album-ids get (string "playlists/" $ "/items")
                          `select(.type == "track") | .item.album.id`)
              playlists)]))

(defn album-info
  "Artist, title and year of album `id`, named the way tiddl will name them,
  or nil if Tidal doesn't have the album."
  [get id]
  (def json (try (get (string "albums/" id)) ([_] nil)))
  (when json
    (def [artist title date]
      (string/split "\n" (string/trimr (sh json "jq" "-r"
                                           `.artist.name // "", .title // "", .releaseDate // ""`)
                                       "\n")))
    (def year (if (>= (length date) 4) (string/slice date 0 4) "1"))
    # tiddl cleans the whole staging folder name, so do the same before splitting
    (def [_ a t y] (string/split "§" (clean (string id "§" artist "§" title "§" year))))
    [a t y]))

## Downloading and filing

(defn file-album
  "Move album `id` from `staging` into the library layout under `library`."
  [id staging library]
  (def prefix (string id "§"))
  (def name (find |(string/has-prefix? prefix $) (os/dir staging)))
  (unless name (error (string "no staging folder for album " id)))
  (def [_ artist album year] (string/split "§" name))
  (def src (string staging "/" name))
  (def discs (sort (os/dir src)))
  (def tracks @[])
  (each disc discs
    (each file (sort (os/dir (string src "/" disc)))
      # Tracks are `NN§Title.ext`; anything else is a temp file tiddl left
      # behind when a download failed partway
      (if (string/find "§" file)
        (array/push tracks [disc file])
        (os/rm (string src "/" disc "/" file)))))
  (def titles-lc (lowercase (map |(get (string/split "§" ($ 1) 0 2) 1) tracks)))
  (def dest (album-dir library artist album year))
  (eachp [i [disc file]] tracks
    (def number (first (string/split "§" file)))
    (def dir (if (> (length discs) 1) (string dest "/" disc) dest))
    (mkdirs dir)
    (os/rename (string src "/" disc "/" file)
               (string dir "/" number "-" (titles-lc i))))
  (each disc discs (os/rmdir (string src "/" disc)))
  (os/rmdir src))

(defn download
  "Download album `id` into `staging` with tiddl; true on success. Without
  --verbose, tiddl's output goes to `logfile`."
  [id staging logfile]
  (def args ["tiddl" "download" "--raise-errors"
             "--path" staging "--scan-path" staging "--output" template
             "url" (string "album/" id)])
  (if verbose
    (zero? (os/execute args :p))
    (with [f (file/open logfile :w)]
      (zero? (os/execute args :p {:out f :err f})))))

## sync

(def lock-dir (string state "/lock"))

(defn lock []
  (unless (os/mkdir lock-dir)
    (def pid (try (string/trim (slurp (string lock-dir "/pid"))) ([_] "")))
    (when (and (not (empty? pid)) (os/stat (string "/proc/" pid)))
      (die "a sync is already running (pid " pid ")")))
  (spit (string lock-dir "/pid") (string (os/getpid))))

(defn sync [staging library]
  (def done-file (string state "/done"))
  (def failed-file (string state "/failed"))
  (def current-file (string state "/current"))
  (def logs (string state "/logs"))
  (mkdirs logs)
  (lock)
  (defer (rm-rf lock-dir)
    # An album interrupted mid-download may have a half-written last track
    # (tiddl tags files in place after downloading them): start it over
    (when (os/stat current-file)
      (def prefix (string (string/trim (slurp current-file)) "§"))
      (each name (os/dir staging)
        (when (string/has-prefix? prefix name)
          (print "Restarting " name ", interrupted last time")
          (rm-rf (string staging "/" name))))
      (os/rm current-file))

    (def session (tidal))
    (def [_ get] session)
    (print "Listing albums...")
    (def all (list-albums session))
    (def done (tabseq [id :in (if (os/stat done-file) (lines (slurp done-file)) [])]
                id true))
    (def todo (filter |(not (done $)) all))
    (print (length all) " albums: " (- (length all) (length todo)) " done, "
           (length todo) " to go")
    (spit failed-file "")

    (var downloaded 0)
    (var present 0)
    (var failed 0)
    (var download-time 0)
    (def started (os/clock))
    (eachp [i id] todo
      (def left (- (length todo) i 1))
      (def info (album-info get id))
      (print "[" (inc i) "/" (length todo) "] "
             (if info (let [[a t y] info] (string a " - " t " (" y ")")) (string "album/" id)))
      (defn report [& msg] (print "  " ;msg ", " left " left"))
      (cond
        (nil? info)
        (do
          (++ failed)
          (spit failed-file (string id "\n") :ab)
          (report "not available on Tidal"))

        (os/stat (album-dir library ;info))
        (do
          (++ present)
          (spit done-file (string id "\n") :ab)
          (report "already in the library"))

        (do
          (def logfile (string logs "/" id ".log"))
          (def start (os/clock))
          (spit current-file id)
          (def ok (and (download id staging logfile)
                       (try (do (file-album id staging library) true)
                         ([err] (spit logfile (string "filing failed: " err "\n") :ab)
                                false))))
          (os/rm current-file)
          (def took (- (os/clock) start))
          (if ok
            (do
              (++ downloaded)
              (+= download-time took)
              (spit done-file (string id "\n") :ab)
              (when (os/stat logfile) (os/rm logfile))
              (report "done in " (duration took) ", ETA "
                      (duration (* left (/ download-time downloaded)))))
            (do
              (++ failed)
              (spit failed-file (string id "\n") :ab)
              (report "FAILED" (if verbose "" (string ", see " logfile)))))
          # Pause after albums that actually downloaded something, to stay
          # under Tidal's rate limits
          (when (> took 5) (ev/sleep 3)))))

    (print "Finished in " (duration (- (os/clock) started)) ": "
           downloaded " downloaded, " present " already in the library, "
           failed " failed"
           (if (pos? failed) (string " (IDs in " failed-file ")") ""))))

(defn main [_ & args]
  (var library default-library)
  (var staging default-staging)
  (var cmd nil)
  (var i 0)
  (defn value []
    (++ i)
    (or (get args i) (die (args (dec i)) " needs a directory")))
  (while (< i (length args))
    (def arg (args i))
    (case arg
      "-h" (do (print usage) (os/exit 0))
      "--help" (do (print usage) (os/exit 0))
      "-v" (set verbose true)
      "--verbose" (set verbose true)
      "--library" (set library (value))
      "--staging" (set staging (value))
      (if cmd
        (die "unexpected argument: " arg "\n\n" usage)
        (set cmd arg)))
    (++ i))
  (case cmd
    "list" (each id (list-albums (tidal)) (print id))
    "sync" (sync staging library)
    (die usage)))
