#!/usr/bin/env janet
# Mirror a Tidal library with tiddl, filing each album the way the rest of the
# music library is laid out:
#
#   Artist/artist/album[year]digital media/NN-title.flac
#   Artist/artist/album[year]digital media/DD/NN-title.flac   (multi-disc)
#
# Albums: saved albums, plus the whole album of every saved track and of every
# track in the playlists in $TIDAL_PLAYLISTS (space-separated UUIDs).
#
#   tidal-library list   print the album IDs
#   tidal-library sync   download and file every album not done yet; safe to
#                        re-run, failures are listed in $TIDAL_STATE/failed
#
# Needs tiddl (logged in with `tiddl auth login`), curl, jq and GNU sed. Track
# quality and cover embedding come from tiddl's own config.

(defn env [name default] (or (os/getenv name) default))

(def staging (env "TIDAL_STAGING" "/media/tidal-staging"))
(def library (env "TIDAL_LIBRARY" "/media/music/tidal"))
(def state
  (string (env "XDG_STATE_HOME" (string (os/getenv "HOME") "/.local/state"))
          "/tidal-library"))
(def playlists
  (filter |(not (empty? $)) (string/split " " (env "TIDAL_PLAYLISTS" ""))))

# tiddl downloads into staging under names carrying what file-album needs
(def template
  (string "{album.id}§{album.artist}§{album.title}§{album.date:%Y}"
          "/{item.volume:02d}/{item.number:02d}§{item.title_version}"))

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

(defn lowercase
  "Lowercase strings with sed: Janet's own string/ascii-lower skips non-ASCII
  letters."
  [strs]
  (string/split "\n" (string/trimr (sh (string/join strs "\n")
                                       "env" "LC_ALL=C.UTF-8" "sed" `s/.*/\L&/`)
                                   "\n")))

(defn tidal
  "Log in through tiddl's saved session; return the user ID and a function
  that GETs a Tidal API path."
  []
  (sh nil "tiddl" "auth" "refresh")
  (def [token user country]
    (lines (sh nil "jq" "-r" ".token, .user_id, .country_code"
               (string (os/getenv "HOME") "/.tiddl/auth.json"))))
  (defn get [path]
    # The token goes in through stdin so it never shows up in `ps`
    (sh (string "Authorization: Bearer " token)
        "curl" "-sf" "-m" "30" "-H" "@-"
        (string "https://api.tidal.com/v1/" path "&countryCode=" country)))
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
    (ev/sleep 0.25))
  ids)

(defn list-albums []
  (def [user get] (tidal))
  (distinct
    [;(album-ids get (string "users/" user "/favorites/albums") ".item.id")
     ;(album-ids get (string "users/" user "/favorites/tracks") ".item.album.id")
     ;(mapcat |(album-ids get (string "playlists/" $ "/items")
                          `select(.type == "track") | .item.album.id`)
              playlists)]))

(defn mkdirs [path]
  (var dir "")
  (each part (lines (string/replace-all "/" "\n" path))
    (set dir (string dir "/" part))
    (os/mkdir dir)))

(defn file-album
  "Move album `id` from staging into the library layout."
  [id]
  (def prefix (string id "§"))
  (def name (find |(string/has-prefix? prefix $) (os/dir staging)))
  (unless name (error (string "no staging folder for album " id)))
  (def [_ artist album year] (string/split "§" name))
  (def src (string staging "/" name))
  (def discs (sort (os/dir src)))
  (def tracks (seq [disc :in discs
                    file :in (sort (os/dir (string src "/" disc)))]
                [disc file]))
  # file is `NN§Title.ext`
  (def [artist-lc album-lc & titles-lc]
    (lowercase [artist album ;(map |(get (string/split "§" ($ 1) 0 2) 1) tracks)]))
  (def dest (string library "/" artist "/" artist-lc "/"
                    album-lc "[" year "]digital media"))
  (eachp [i [disc file]] tracks
    (def number (first (string/split "§" file)))
    (def dir (if (> (length discs) 1) (string dest "/" disc) dest))
    (mkdirs dir)
    (os/rename (string src "/" disc "/" file)
               (string dir "/" number "-" (titles-lc i))))
  (each disc discs (os/rmdir (string src "/" disc)))
  (os/rmdir src))

(defn download [id]
  (zero? (os/execute ["tiddl" "download" "--raise-errors"
                      "--path" staging "--scan-path" staging
                      "--output" template
                      "url" (string "album/" id)]
                     :p)))

(defn sync []
  (mkdirs state)
  (def done-file (string state "/done"))
  (def failed-file (string state "/failed"))
  (def done (tabseq [id :in (if (os/stat done-file) (lines (slurp done-file)) [])]
              id true))
  (def todo (filter |(not (done $)) (list-albums)))
  (spit failed-file "")
  (eachp [i id] todo
    (print "== [" (inc i) "/" (length todo) "] album/" id)
    (def start (os/clock))
    (if (and (download id)
             (try (do (file-album id) true)
               ([err] (eprint "filing failed: " err) false)))
      (spit done-file (string id "\n") :ab)
      (spit failed-file (string id "\n") :ab))
    # Pause after albums that actually downloaded something, to stay under
    # Tidal's rate limits; albums with nothing new go by quickly
    (when (> (- (os/clock) start) 5)
      (ev/sleep 3)))
  (def failed (lines (slurp failed-file)))
  (print "done; " (length failed) " failed (" failed-file ")"))

(defn main [_ &opt cmd]
  (case cmd
    "list" (each id (list-albums) (print id))
    "sync" (sync)
    (do (eprint "usage: tidal-library list|sync") (os/exit 1))))
