#!/usr/bin/env janet
# Mirror a Tidal library with tiddl. See `tidal-library --help`.

(def default-library "/media/music/tidal")
(def default-staging "/media/tidal-staging")

(def usage
  (string/trim
    ``
usage: tidal-library [options] list|sync|export FILE

Mirror a Tidal library into a music folder with tiddl, filing each album the
way the rest of the library is laid out:

  Artist/artist/album[year]digital media/NN-title.flac
  Artist/artist/album[year]digital media/DD/NN-title.flac   (multi-disc)

Albums: saved albums, plus the whole album of every saved track and of every
track in the playlists in $TIDAL_PLAYLISTS (space-separated UUIDs), as Tidal
lists them now or, with --from, as a backup made by export does.

commands:
  list          print the IDs of the albums to mirror
  sync          download and file every album not yet in the library
  export FILE   back the library up to FILE, metadata only, as JSON: saved
                albums, tracks, artists and videos, every playlist made or
                saved with its items, each as Tidal's API gives it

options:
  --library DIR   where finished albums go (default: /media/music/tidal)
  --staging DIR   where tiddl downloads to first; must be on the library's
                  filesystem (default: /media/tidal-staging)
  --from FILE     take the albums from a library backup instead of Tidal
  --quality Q     high (16-bit FLAC) or max (up to hi-res); default: tiddl's
                  config
  --per-day N     start at most N album downloads in any 24 hours, earlier
                  runs' included; sync waits once it has started that many
  --pause SECS    pause after each album download, failed or not (default: 3)
  -v, --verbose   also show tiddl's own output and the API paging
  -h, --help      show this help

Re-running sync is safe: albums already done or already in the library are
skipped, a failed album keeps its finished tracks for the next try, and an
album interrupted mid-download starts over. Only one sync runs at a time.
After 5 albums in a row that Tidal can't be asked about (network or API
errors; not albums it no longer has), sync stops: Tidal or the connection is
probably down, so the rest would fail too.

An album tiddl downloads without errors but with fewer tracks than Tidal
lists is incomplete: tiddl skips tracks Tidal only serves it in Dolby Atmos.
It stays out of the library, keeps its tracks in staging, and is listed in
the incomplete file with how many tracks it got; sync doesn't try it again
unless its line is deleted there.

Done, incomplete and failed albums, when the last day's downloads started,
and a log per failed or incomplete album, are kept in
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
(var per-day nil)
(var pause 3)
(var quality nil)
(def max-lookups-failed 5)

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
  that GETs a Tidal API path. The session is renewed whenever it's about to
  expire, since a sync can run for days."
  []
  (def auth-file (string (os/getenv "HOME") "/.tiddl/auth.json"))
  (var token nil)
  (var expires 0)
  (defn renew []
    # Only asks Tidal for a new token within 10 minutes of expiry
    (sh nil "tiddl" "auth" "refresh" "--early-expire" "600")
    (def [t e] (lines (sh nil "jq" "-r" ".token, .expires_at" auth-file)))
    (set token t)
    (set expires (scan-number e)))
  (renew)
  (def [user country]
    (lines (sh nil "jq" "-r" ".user_id, .country_code" auth-file)))
  (defn get
    "The body of Tidal's answer, or nil if it has no such thing (HTTP 404).
    Errors on any other failure."
    [path]
    (when (> (os/time) (- expires 600)) (renew))
    (def sep (if (string/find "?" path) "&" "?"))
    # The token goes in through stdin so it never shows up in `ps`
    (def out (sh (string "Authorization: Bearer " token)
                 "curl" "-s" "-m" "30" "-H" "@-" "-w" "\n%{http_code}"
                 (string "https://api.tidal.com/v1/" path sep "countryCode=" country)))
    (def end (last (string/find-all "\n" out)))
    (def status (scan-number (string/slice out (inc end))))
    (cond
      (= status 404) nil
      (<= 200 status 299) (string/slice out 0 end)
      (error (string "HTTP " status " for " path))))
  [user get])

(defn paged
  "Every item of a paginated Tidal endpoint, through the jq filter `pick`:
  one line each, compact if JSON."
  [get endpoint pick]
  (def items @[])
  (var offset 0)
  (var total 1)
  (while (< offset total)
    (def page (get (string endpoint "?limit=100&offset=" offset)))
    (unless page (error (string endpoint " not found")))
    (def found
      (lines (sh page "jq" "-rc"
                 (string ".totalNumberOfItems, (.items[] | " pick ")"))))
    (set total (scan-number (first found)))
    (array/concat items (slice found 1))
    (+= offset 100)
    (note "  " endpoint ": " (min offset total) "/" total)
    (ev/sleep 0.25))
  items)

(defn list-albums [[user get]]
  (distinct
    [;(paged get (string "users/" user "/favorites/albums") ".item.id")
     ;(paged get (string "users/" user "/favorites/tracks") ".item.album.id")
     ;(mapcat |(paged get (string "playlists/" $ "/items")
                      `select(.type == "track") | .item.album.id`)
              playlists)]))

(defn backup-albums
  "list-albums, from the library backup `file`."
  [file]
  (distinct
    (lines (sh nil "jq" "-r"
               ``
               .favorites.albums[].item.id,
               .favorites.tracks[].item.album.id,
               (.playlists[] | select(.uuid | IN($ARGS.positional[]))
                | .items[] | select(.type == "track") | .item.album.id)
               | values
               ``
               file "--args" ;playlists))))

(defn export
  "Back the library up to `file` (see usage)."
  [[user get] file]
  (defn json-array [items] (string "[" (string/join items ",") "]"))
  (def favorites
    (seq [kind :in ["albums" "tracks" "artists" "videos"]]
      (print "Saved " kind "...")
      (string `"` kind `":`
              (json-array (paged get (string "users/" user "/favorites/" kind) ".")))))
  (print "Playlists...")
  (def playlists
    (lines (sh (string (json-array (paged get (string "users/" user "/playlists") "."))
                       "\n"
                       (json-array (paged get (string "users/" user "/favorites/playlists")
                                          ".item")))
               "jq" "-cs" "add | unique_by(.uuid) | .[]")))
  (def with-items
    (seq [i :range [0 (length playlists)]
          :let [playlist (playlists i)
                [uuid title] (string/split "\n" (sh playlist "jq" "-r"
                                                    `.uuid, (.title // "" | gsub("\\s+"; " "))`))]]
      (print "[" (inc i) "/" (length playlists) "] " title)
      (def items
        (try (json-array (paged get (string "playlists/" uuid "/items") "."))
          ([err] (print "  couldn't read its items: " err) nil)))
      (sh (string playlist "\n" (or items "null")) "jq" "-cs"
          `.[0] + if .[1] then {items: .[1]} else {items: [], items_error: "could not be read"} end`)))
  (print "Saved IDs...")
  (def ids (or (get (string "users/" user "/favorites/ids"))
               (error "favorites/ids not found")))
  (def json
    (string `{"exported":"` (string/trim (sh nil "date" "-Iseconds")) `",`
            `"user":"` user `",`
            `"favorites":{` (string/join favorites ",") `},`
            `"playlists":` (json-array (map string/trimr with-items)) `,`
            `"favorite_ids":` ids `}`))
  # Checked by jq on the way, and never half-written over an older backup
  (def tmp (string file ".tmp"))
  (spit tmp (sh json "jq" "-c" "."))
  (os/rename tmp file)
  (print "Wrote " file))

(defn album-info
  "Artist, title and year of album `id`, named the way tiddl will name them,
  and its number of tracks; nil if Tidal doesn't have the album."
  [get id]
  (when-let [json (get (string "albums/" id))]
    (def [artist title date tracks]
      (string/split "\n" (string/trimr (sh json "jq" "-r"
                                           ``
                                           .artist.name // "", .title // "",
                                           .releaseDate // "", .numberOfTracks // 0
                                           ``)
                                       "\n")))
    (def year (if (>= (length date) 4) (string/slice date 0 4) "1"))
    # tiddl cleans the whole staging folder name, so do the same before splitting
    (def [_ a t y] (string/split "§" (clean (string id "§" artist "§" title "§" year))))
    {:artist a :title t :year y :tracks (scan-number tracks)}))

## Downloading and filing

(defn staging-folder
  "The name of album `id`'s folder in `staging`, or nil."
  [staging id]
  (def prefix (string id "§"))
  (find |(string/has-prefix? prefix $) (os/dir staging)))

(defn staged-tracks
  "How many finished tracks of album `id` are in `staging`."
  [staging id]
  (def name (staging-folder staging id))
  (if name
    (let [src (string staging "/" name)]
      # Tracks are `NN§Title.ext` (see file-album)
      (sum (map (fn [disc] (count |(string/find "§" $) (os/dir (string src "/" disc))))
                (os/dir src))))
    0))

(defn file-album
  "Move album `id` from `staging` into the library layout under `library`."
  [id staging library]
  (def name (staging-folder staging id))
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
             ;(if quality ["--track-quality" quality] [])
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

(defn pace
  "Wait until one more album download fits in --per-day, then add now to
  `file`, the start times of the last day's downloads."
  [file]
  (defn recent []
    (def since (- (os/time) 86400))
    (sort (filter |(> $ since)
                  (keep scan-number (if (os/stat file) (lines (slurp file)) [])))))
  (var starts (recent))
  (when per-day
    (while (>= (length starts) per-day)
      # Until enough of them are a day old to leave room for one more
      (def wait (max 1 (- (+ (starts (- (length starts) per-day)) 86400)
                          (os/time))))
      (print "  " (length starts) " downloads started in the last 24 hours, "
             "waiting " (duration wait))
      (ev/sleep wait)
      (set starts (recent))))
  (spit file (string (string/join (map string [;starts (os/time)]) "\n") "\n")))

(defn eta
  "Roughly how long `left` more downloads take at `avg` seconds each."
  [left avg]
  (def per (+ avg pause))
  (def steady (* left per))
  (if per-day
    (max steady (+ (* (div left per-day) 86400) (* (% left per-day) per)))
    steady))

(defn sync
  "Download every album not done yet, taking the list from the library backup
  `from` if given."
  [staging library from]
  (def done-file (string state "/done"))
  (def downloads-file (string state "/downloads"))
  (def failed-file (string state "/failed"))
  # One line per album: its ID, a tab, then what it is and how many tracks
  (def incomplete-file (string state "/incomplete"))
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
    (def all
      (if from
        (do (print "Reading albums from " from "...") (backup-albums from))
        (do (print "Listing albums...") (list-albums session))))
    (defn ids [file]
      (if (os/stat file) (map |(first (string/split "\t" $)) (lines (slurp file))) []))
    (def done (tabseq [id :in (ids done-file)] id true))
    (def skip (tabseq [id :in (ids incomplete-file)] id true))
    (def todo (filter |(not (or (done $) (skip $))) all))
    (print (length all) " albums: " (count done all) " done, "
           (count skip all) " incomplete, " (length todo) " to go")
    (spit failed-file "")

    (var downloaded 0)
    (var present 0)
    (var incomplete 0)
    (var failed 0)
    (var lookups-failed-in-a-row 0)
    (var download-time 0)
    (def started (os/clock))
    (eachp [i id] todo
      (def left (- (length todo) i 1))
      (defn report [& msg] (print "  " ;msg ", " left " left"))
      (defn fail [& msg]
        (++ failed)
        (spit failed-file (string id "\n") :ab)
        (report ;msg))
      (var lookup-error nil)
      (def info (try (album-info get id) ([err] (set lookup-error err) nil)))
      (def name (if info (string (info :artist) " - " (info :title) " (" (info :year) ")")))
      (print "[" (inc i) "/" (length todo) "] " (or name (string "album/" id)))
      (if lookup-error
        (++ lookups-failed-in-a-row)
        (set lookups-failed-in-a-row 0))
      (cond
        lookup-error (fail "couldn't look it up: " lookup-error)

        (nil? info) (fail "not available on Tidal")

        (os/stat (album-dir library (info :artist) (info :title) (info :year)))
        (do
          (++ present)
          (spit done-file (string id "\n") :ab)
          (report "already in the library"))

        (do
          (def logfile (string logs "/" id ".log"))
          (def see-log (if verbose "" (string ", see " logfile)))
          (pace downloads-file)
          (def start (os/clock))
          (spit current-file id)
          (def ok (download id staging logfile))
          (def got (staged-tracks staging id))
          (cond
            (not ok) (fail "FAILED" see-log)

            (< got (info :tracks))
            (do
              (++ incomplete)
              (spit incomplete-file
                    (string id "\t" name ": " got " of " (info :tracks) " tracks\n") :ab)
              (report "only " got " of " (info :tracks) " tracks (Dolby Atmos?), "
                      "listed in " incomplete-file see-log))

            (try
              (do
                (file-album id staging library)
                (def took (- (os/clock) start))
                (++ downloaded)
                (+= download-time took)
                (spit done-file (string id "\n") :ab)
                (when (os/stat logfile) (os/rm logfile))
                (report "done in " (duration took) ", ETA "
                        (duration (eta left (/ download-time downloaded)))))
              ([err]
                (spit logfile (string "filing failed: " err "\n") :ab)
                (fail "FAILED" see-log))))
          (os/rm current-file)
          # Pause after every download, failed or not, to go easy on Tidal
          (ev/sleep pause)))

      (when (>= lookups-failed-in-a-row max-lookups-failed)
        (print lookups-failed-in-a-row " albums in a row couldn't be looked up: "
               "Tidal or the connection is probably down. Stopping; re-run "
               "sync to go on.")
        (break)))

    (print "Finished in " (duration (- (os/clock) started)) ": "
           downloaded " downloaded, " present " already in the library, "
           incomplete " incomplete, " failed " failed"
           (if (pos? failed) (string " (IDs in " failed-file ")") ""))))

(defn main [_ & args]
  (var library default-library)
  (var staging default-staging)
  (var from nil)
  (def positional @[])
  (var i 0)
  (defn value []
    (++ i)
    (or (get args i) (die (args (dec i)) " needs a value")))
  (defn number [least]
    (def flag (args i))
    (def n (scan-number (value)))
    (unless (and n (>= n least)) (die flag " needs a number, at least " least))
    n)
  (while (< i (length args))
    (def arg (args i))
    (case arg
      "-h" (do (print usage) (os/exit 0))
      "--help" (do (print usage) (os/exit 0))
      "-v" (set verbose true)
      "--verbose" (set verbose true)
      "--library" (set library (value))
      "--staging" (set staging (value))
      "--from" (set from (value))
      "--quality" (do
                    (set quality (value))
                    (unless (index-of quality ["high" "max"])
                      (die "--quality is high or max")))
      "--per-day" (set per-day (number 1))
      "--pause" (set pause (number 0))
      (array/push positional arg))
    (++ i))
  (when (and from (not (os/stat from)))
    (die "no such file: " from))
  (def [cmd file] positional)
  (unless (= (length positional) (if (= cmd "export") 2 1))
    (die usage))
  (case cmd
    "list" (each id (if from (backup-albums from) (list-albums (tidal)))
             (print id))
    "sync" (sync staging library from)
    "export" (export (tidal) file)
    (die usage)))
