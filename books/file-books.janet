#!/usr/bin/env janet
# File finished book downloads into the library. See `file-books --help`.

(def default-downloads "/media/download/books")
(def default-library "/media/books")

(def usage
  (string/trim
    ``
usage: file-books [options]

Copy each finished book download into the library with a tidy name:

  Author/Title/Author - Title.ext

Author and title come from the file's own metadata (EPUB, PDF, MOBI, AZW3,
DJVU), ignoring placeholders like "Untitled" or "Microsoft Word - x.docx".
Files without usable metadata are named after their download instead:

  Unsorted/Download name/Download name.ext
  Unsorted/Download name/Download name - original name.ext   (several such)

The download itself is left untouched, so it keeps seeding.

Which files are taken from a download:
  - exactly one EPUB: just that EPUB
  - otherwise files are grouped by name (minus extension), and each group
    gives its best format: epub, pdf, azw3, mobi, djvu
MOBI/AZW3/DJVU are filed as-is (Kavita can't show them) and reported.

Only downloads marked finished are filed. A mark is an empty file named like
the download in DIR/.finished; qBittorrent's "run on torrent finished" hook
creates it once all of a torrent's files are in place (mark one by hand with
`touch DIR/.finished/NAME`). Filing a download removes its mark; one that
fails keeps it, for the next run. Running it again is always safe: a file
already in the library is never copied twice.

options:
  --downloads DIR   book downloads (default: /media/download/books)
  --library DIR     the library (default: /media/books)
  -n, --dry-run     only show what would be filed
  -h, --help        show this help
``))

(def formats ["epub" "pdf" "azw3" "mobi" "djvu"])

(defn die [& msg]
  (eprint ;msg)
  (os/exit 1))

(defn sh
  "Run a command found in PATH, feeding it `input` if given, and return its
  output. Errors if the command fails."
  [input & args]
  (def proc (os/spawn args :p {:in (if input :pipe) :out :pipe :err :pipe}))
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

(defn mkdirs [path]
  (var dir "")
  (each part (lines (string/replace-all "/" "\n" path))
    (set dir (string dir "/" part))
    (os/mkdir dir)))

(defn files-under
  "Every regular file at or below `path`."
  [path]
  (case (os/stat path :mode)
    :file @[path]
    :directory (mapcat |(files-under (string path "/" $)) (sort (os/dir path)))
    @[]))

(defn basename [path] (last (string/split "/" path)))

(defn split-ext
  "[stem ext] of a file name, ext lowercased and without the dot."
  [name]
  (def i (last (string/find-all "." name)))
  (if (and i (pos? i))
    [(string/slice name 0 i) (string/ascii-lower (string/slice name (inc i)))]
    [name ""]))

(defn rank [ext] (index-of ext formats))

(defn choose
  "The book files to file from a download's `files`."
  [files]
  (def books (filter |(rank ((split-ext (basename $)) 1)) files))
  (def epubs (filter |(= "epub" ((split-ext (basename $)) 1)) books))
  (if (= 1 (length epubs))
    epubs
    (let [groups (group-by |((split-ext (basename $)) 0) books)]
      (sort (map (fn [group]
                   (first (sort-by |(rank ((split-ext (basename $)) 1)) group)))
                 (values groups))))))

(defn clean
  "A string made safe as one path component, also on FAT-style storage
  (e-readers, phones) that rejects `\\/:*?\"<>|`."
  [s]
  (as-> s s
    (peg/replace-all '(* (any " ") ":") " -" s)
    (peg/replace-all "/" "-" s)
    (peg/replace-all '(set "\\*?\"<>|\0") "" s)
    (peg/replace-all '(range "\x01\x1f") "" s)
    (peg/replace-all '(some :s) " " s)
    (string/trim s " .")
    # Keep well under the filesystem's 255-byte limit, without splitting a
    # UTF-8 character
    (if (> (length s) 150)
      (do
        (var t (string/slice s 0 150))
        (while (and (pos? (length t)) (= 0x80 (band (last t) 0xC0)))
          (set t (string/slice t 0 -2)))
        (when (and (pos? (length t)) (>= (last t) 0xC0))
          (set t (string/slice t 0 -2)))
        (string/trim t " ."))
      s)))

(defn xpath [xml expr]
  (string/trim (try (sh xml "xmllint" "--xpath" expr "-") ([_] ""))))

(defn epub-meta
  "[author title] as written in an EPUB's metadata (OPF)."
  [file]
  (try
    (let [container (sh nil "unzip" "-p" file "META-INF/container.xml")
          opf-path (xpath container `string(//*[local-name()="rootfile"]/@full-path)`)
          opf (sh nil "unzip" "-p" file opf-path)]
      [(xpath opf `string(//*[local-name()="metadata"]/*[local-name()="creator"][1])`)
       (xpath opf `string(//*[local-name()="metadata"]/*[local-name()="title"][1])`)])
    ([_] ["" ""])))

(defn other-meta
  "[author title] from a PDF, MOBI, AZW3 or DJVU file's metadata, via exiftool.
  Not PDF's Creator tag: that's the program that made the file."
  [file]
  (def tags ["$UpdatedTitle" "$Title" "$BookName" "$Author" "$XMP-dc:Creator"])
  (def out (try (sh nil "exiftool" "-m" "-q" "-f" "-p" (string/join tags "\x1f") file)
             ([_] "")))
  (def [updated title book-name author creator]
    (map |(if (= $ "-") "" $) (map string/trim (string/split "\x1f" (string/trim out)))))
  [(find |(not (empty? $)) [(or author "") (or creator "")] "")
   (find |(not (empty? $)) [(or updated "") (or title "") (or book-name "")] "")])

(def file-exts ["doc" "docx" "pdf" "rtf" "tex" "indd" "qxd" "odt" "txt" "html"
                "epub" "mobi" "azw3" "djvu"])

(defn junk-title? [s]
  (def l (string/ascii-lower s))
  (or (empty? s)
      (index-of l ["untitled" "unknown" "title" "document" "book" "ebook"])
      (string/has-prefix? "microsoft word" l)
      (some |(string/has-suffix? (string "." $) l) file-exts)))

(defn junk-author? [s]
  (def l (string/ascii-lower s))
  (or (empty? s)
      (index-of l ["unknown" "author" "admin" "administrator" "user" "owner"
                   "calibre"])))

(defn book-meta
  "[author title] for `file`, cleaned, or nil if its metadata isn't usable."
  [file]
  (def ext ((split-ext (basename file)) 1))
  (def [author title] (map clean (if (= ext "epub") (epub-meta file) (other-meta file))))
  (unless (or (junk-author? author) (junk-title? title))
    [author title]))

(defn same-file? [a b]
  (zero? (os/execute ["cmp" "-s" a b] :p)))

(defn copy
  "Copy `src` to `dest` without ever exposing a half-written file; a different
  file already at `dest` gets a numbered name instead. Returns where it went,
  or nil if an identical copy was already there."
  [src dest]
  (def [stem ext] (split-ext dest))
  (var target dest)
  (var n 1)
  (while (and (os/stat target) (not (same-file? src target)))
    (++ n)
    (set target (string stem " (" n ")." ext)))
  (unless (os/stat target)
    (def dir (string/join (slice (string/split "/" target) 0 -2) "/"))
    (mkdirs dir)
    (def tmp (string dir "/.file-books.tmp"))
    (sh nil "cp" "--no-preserve=mode,ownership" src tmp)
    (os/rename tmp target)
    target))

(defn destinations
  "Where each of `files` from download `download` goes in `library`."
  [library download files]
  (def metas (map book-meta files))
  (def unsorted (count nil? metas))
  (map (fn [file meta]
         (def [stem ext] (split-ext (basename file)))
         (if meta
           (let [[author title] meta]
             (string library "/" author "/" title "/"
                     (clean (string author " - " title)) "." ext))
           # A single-file download's name is the file's own: drop its
           # extension
           (let [name (clean (if (= download (basename file)) stem download))]
             (string library "/Unsorted/" name "/"
                     (if (= 1 unsorted) name (clean (string name " - " stem)))
                     "." ext))))
       files metas))

(defn main [_ & args]
  (var downloads default-downloads)
  (var library default-library)
  (var dry-run false)
  (var i 0)
  (defn value []
    (++ i)
    (or (get args i) (die (args (dec i)) " needs a directory")))
  (while (< i (length args))
    (case (args i)
      "-h" (do (print usage) (os/exit 0))
      "--help" (do (print usage) (os/exit 0))
      "-n" (set dry-run true)
      "--dry-run" (set dry-run true)
      "--downloads" (set downloads (value))
      "--library" (set library (value))
      (die "unexpected argument: " (args i) "\n\n" usage))
    (++ i))

  (def marks (string downloads "/.finished"))
  (defn marked [] (if (os/stat marks) (sort (os/dir marks)) @[]))
  (def attempted @{})
  (var failures 0)
  # Until nothing new is marked: downloads that finish while this runs are
  # filed too
  (forever
    (def todo (filter |(not (attempted $)) (marked)))
    (when (empty? todo) (break))
    (each download todo
      (put attempted download true)
      (def path (string downloads "/" download))
      (def mark (string marks "/" download))
      (print download)
      (def ok
        (if (not (os/stat path))
          (do (print "  no longer in the downloads, dropping its mark") true)
          (let [chosen (choose (files-under path))]
            (when (empty? chosen) (print "  no book files"))
            (try
              (all (fn [[file dest]]
                     (try
                       (let [ext ((split-ext (basename file)) 1)]
                         (when (index-of ext ["azw3" "mobi" "djvu"])
                           (print "  note: " ext " only, Kavita won't show it"))
                         (if dry-run
                           (print "  would file " dest)
                           (if-let [target (copy file dest)]
                             (print "  -> " target)
                             (print "  already in the library: " dest)))
                         true)
                       ([err] (eprint "  failed: " (basename file) ": " err) false)))
                   (map tuple chosen (destinations library download chosen)))
              ([err] (eprint "  failed: " err) false)))))
      (if ok
        (unless dry-run (os/rm mark))
        (++ failures))))
  (when (pos? failures)
    (die failures " download(s) failed; they keep their mark for the next run")))
