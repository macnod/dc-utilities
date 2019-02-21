;; Copyright © 2003-2013 Donnie Cameron

;; Stuff that should be a part of Common Lisp. These routines are
;; general enough to be needed in most of my programs.

(in-package :dc-utilities)

(set-dispatch-macro-character #\# #\%
  (lambda (s c n)
    (declare (ignore c n))
    (let ((list (read s nil (values) t)))
      (when (consp list)
        (ds (cons :map list))))))

(defparameter *interruptible-sleep-hash* (make-hash-table :test #'equal))
(defparameter *settings* nil)
(defparameter *dc-job-queue-mutex* nil)
(defparameter *dc-progress-mutex* nil)
(defparameter *dc-job-queue* nil)
(defparameter *dc-thread-pool-progress* nil)
(defparameter *dc-thread-pool-start-time* nil)
(defparameter *dc-thread-pool-stop-time* nil)
(defparameter *dc-thread-pool-done* nil)
(defparameter *dc-timings* (make-hash-table :test #'equal :synchronized t))

(defun to-ascii (s)
  "Converts the string S, which may contain non-ascii characters, into a string with nothing but ascii characters.  Non ascii characters are converted into spaces.  If S is a list, this function converts each element of the list into an all-ascii string."
  (if (atom s)
      (map 'string (lambda (c) (if (> (char-code c) 127) #\Space c)) s)
      (loop for a in s collect
           (if (stringp a)
               (map 'string (lambda (c) (if (> (char-code c) 127) #\Space c)) a)
               (format nil "~a" a)))))

(defun timestamp (&key
                    (time (get-universal-time))
                    string
                    (format "Y-M-DTh:m:s"))
  "Returns the given time (or the current time) formatted according to the FORMAT parameter, followed by an optional value for STRING.  If STRING is provided, the function adds a space to the result and then appends the string to that.  The FORMAT string can contain any characters.  This function will replace the format characters Y, M, D, h, m, and s, respectively, with numbers representing the year,month, day, hour, minute, and second.  All the numbers are 2 digits long, except for the year, which is 4 digits long."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time)
    (let* ((space-string (if string (format nil " ~a" string) ""))
           (parts (ds (list :map
                            "Y" (format nil "~d"     year)
                            "M" (format nil "~2,'0d" month)
                            "D" (format nil "~2,'0d" day)
                            "h" (format nil "~2,'0d" hour)
                            "m" (format nil "~2,'0d" minute)
                            "s" (format nil "~2,'0d" second))))
           (format-1 (loop for c across format
                        for s = (string c)
                        for d = (ds-get parts s)
                        collect (if d d s))))
      (concatenate 'string (format nil "~{~a~}" format-1) space-string))))

(defun log-entry (&rest messages)
  "Accepts one or more strings, concatenates them, precedes the result with a timestamp, and returns a string that looks like a log entry."
  (timestamp :string (format nil "~{~a~}~%" messages)))

(defun write-log-entry (stream &rest messages)
  "Accepts one or more strings, concatenates them, precedes the result with a timestamp, and writes a string that looks like a lo
g entry to the given stream."
  (format stream "~a" (apply #'log-entry messages)))

(defun replace-regexs (text list-of-regex-replacement-pairs &key ignore-case)
  "Searches through TEXT for substrings that match the regexs in LIST-OF-REGEX-REPLACEMENTS and replaces those substrings with the corresponding replacements in LIST-OF-REGEX-REPLACEMENTS.  Use the IGNORE-CASE parameter if you want case-insensitive matches.  Here's an example:

    (replace-regexs
         \"She was beautiful.  She was smart.  She was sexy\"
         '((\"sh[aeiou]\" \"Tracy\")
           (\"wa[a-z]\" \"is\"))
         :ignore-case t)

    ==> \"Tracy is beautiful.  Tracy is smart.  Tracy is sexy\""
  (let ((ttext text))
    (loop for rp in list-of-regex-replacement-pairs
       do (setf ttext (ppcre:regex-replace-all
                       (format nil "~a~a" (if ignore-case "(?i)" "") (car rp))
                       ttext
                       (cdr rp))))
   ttext))

(defun scrape-string (regex string &key ignore-case)
  "Returns a list of the substrings in STRING that match REGEX.  Use the IGNORE-CASE parameter if you want case-insensitive matches."
  (map 'list 'identity
       (multiple-value-bind (whole parts)
           (ppcre:scan-to-strings
            (if ignore-case (concatenate 'string "(?is)" regex) regex)
            string)
         (declare (ignore whole))
         parts)))

(defun verify-string (string regex &key ignore-case)
  "Return t if STRING matches the REGEX exactly.  Use the IGNORE-CASE parameter if you want case-insensitve matches."
  (multiple-value-bind (a b)
      (scan
       (if ignore-case (concatenate 'string "(?is)" regex) regex)
       string)
    (and a b (zerop a) (= b (length string)))))

(defun shell-execute (program &optional parameters (input-pipe-data ""))
  "Run PROGRAM and return the output of the program as a string.  You can pass an atom or a list for PARAMETERS (the command-line options for the program). You can also pipe data to the program by passing the INPUT-PIPE-DATA parameter with a string containing the data you want to pipe.  The INPUT-PIPE-DATA parameter defaults to the empty string."
  (let ((parameters (cond ((null parameters) nil)
                          ((atom parameters) (list parameters))
                          (t parameters))))
    (with-output-to-string (output-stream)
      (with-input-from-string (input-stream input-pipe-data)
        (sb-ext:run-program program parameters :search t
                            :output output-stream :input input-stream)))))

(defun file-line-count (filename)
  "Obtain a count of the lines in the file FILENAME using the Linux wc program."
  (values (parse-integer
           (shell-execute "wc" `("-l" ,filename)) :junk-allowed t)))

(defmacro with-lines-in-file ((line filename) &body body)
  "Lambda list is `((LINE FILENAME) &BODY BODY)`.  Sequentially assigns each line in the file given by FILENAME to LINE and runs BODY for each line."
  (let ((file (gensym)))
    `(with-open-file (,file ,filename)
      (do ((,line (read-line ,file nil) (read-line ,file nil)))
          ((null ,line) nil)
        ,@body))))

(defun join-paths (&rest path-parts)
  "Joins elements of PATH-PARTS into a file path, inserting slashes where necessary."
  (let ((path (format nil "~{~a~^/~}"
                      (loop for part in path-parts collect
                           (regex-replace-all "^/|/$" part "")))))
    (format nil "~a~a"
            (if (verify-string (car path-parts) "^/.*$") "/" "")
            path)))

(defun path-only (filename)
  "Retrieves the path (path only, without the filename) of FILENAME."
  (multiple-value-bind (match strings)
      (scan-to-strings "(.+)\/[^\/]*$" filename)
    (declare (ignore match))
    (if (null strings) "/" (elt strings 0))))

(defun create-directory (dir &key with-parents)
  "Works just like the mkdir shell command.  DIR is the directory you want to create. Use WITH-PARENTS if you want the function to create parent directories as necessary."
  (unless (directory-exists dir)
    (when
        (zerop
         (length
          (shell-execute
           "mkdir" (if with-parents (list "-p" dir) (list dir)))))
    dir)))

(defmacro filter-file ((line input-filename output-filename) &body body)
  "Copies lines from the file INPUT-FILENAME to the file OUTPUT-FILENAME, omitting lines for which BODY returns nil."
  (let ((output (gensym))
        (transformed-line (gensym)))
    `(with-open-file (,output ,output-filename
                              :direction :output
                              :if-exists :supersede)
       (with-lines-in-file (,line ,input-filename)
         (let ((,transformed-line ,@body))
           (when ,transformed-line (write-line ,transformed-line ,output)))))))

(defun freeze (object)
  "Serializes OBJECT into a string, returning the string."
  (with-output-to-string (s) (write object :stream s :readably t)))

(defun thaw (string)
  "Deserializes an object (or data structure) from the string expression in STRING, returning the object."
  (with-input-from-string (s string) (read s)))

(defun slurp (filename)
  "Reads the entire file FILENAME into a string and returns the string."
  (with-open-file (stream filename)
    (let ((seq (make-array (file-length stream)
                           :element-type 'character :fill-pointer t)))
      (handler-bind ((sb-int:stream-decoding-error
                      (lambda (c)
                        (declare (ignore c))
                        (invoke-restart 'sb-int:attempt-resync))))
        (setf (fill-pointer seq) (read-sequence seq stream)))
      seq)))

(defun slurp-binary (filename)
  "Reads the entire binary file given by FILENAME and returns an array with the bytes of the file."
  (with-open-file (s filename :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length s) :element-type 'unsigned-byte)))
      (read-sequence seq s)
      seq)))

(defun read-one-line (stream &key (eol :unix) (max-length 500))
  "Reads a single line from STREAM and returns the line as a string.  You can specify the end-of-line character with EOL, which defaults to :unix.  The other option is :dos.  If no end-of-line character is found before the line reaches a length of MAX-LENGTH, a line of length MAX-LENGTH is returned."
  (handler-bind ((sb-int:stream-decoding-error
                  (lambda (c)
                    (declare (ignore c))
                    (invoke-restart 'sb-int:attempt-resync))))
    (loop
       with eol = (case eol
                    (:dos (reverse '(#\Return #\Newline)))
                    (:unix '(#\Newline))
                    (otherwise (if (listp eol) (reverse eol) (list eol))))
       with chars = nil
       with char-count = 0
       for char = (read-char stream nil nil)
       for potential-eol = (list char)
       then (if (< (length potential-eol) (length eol))
                (cons char potential-eol)
                (cons char (butlast potential-eol)))
       while (and (not (equal potential-eol eol))
                  (< char-count max-length)
                  (not (null char)))
       do
         (setf chars (cons char chars))
         (incf char-count)
       finally
         (return (if (zerop (length chars))
                     nil
                     (string-right-trim (nreverse eol)
                                        (coerce (nreverse chars) 'string)))))))

(defun spew (string filename &key create-directories append)
  "Writes the contents of STRING to the file specified by FILENAME.  Use the CREATE-DIRECTORIES parameter if any of the directories in the path in FILENAME don't exist and you want to create them.  Use the APPEND parameter if you want to append STRING to an existing file."
  (when create-directories
    (create-directory (directory-namestring filename) :with-parents t))
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string string stream)))

(defun slurp-n-thaw (filename)
  "Reads and brings to life serialized objects from the file FILENAME."
  (with-open-file (stream filename) (read stream)))

(defun freeze-n-spew (object filename)
  "Serializes OBJECT into a string and writes the string to the file specified by FILENAME."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write object :stream stream :readably t)))

(defun lof (filename)
  "Returns the length of the file FILENAME."
  (with-open-file (f filename) (file-length f)))

(defun unique-file-name (&key (directory "/tmp") (extension ".tmp"))
  "Returns a made-up, unique file name.  DIRECTORY defaults to /tmp and EXTENSION to .tmp."
  (join-paths
   directory
   (format nil "~a~a"
           (unique-name)
           (if (scan "^\\." extension)
               extension
               (format nil ".~a" extension)))))

(defun unique-name ()
  "Returns a fairly unique short string"
  (string-downcase
   (format nil "~a~32R~32R"
           (gensym)
           (get-universal-time)
           (random 1000000000))))

(defun split-n-trim (string &key (on-regex "\\s+") (fat "^\\s+|\\s+$"))
  "Splits STRING into substrings on ON-REGEX, then trims FAT from each substring.  The ON-REGEX parameter value, which is optional, defaults to \"\\s+\", which is to say that the string is split into a list of words at the whitespace boundaries.  The default value for FAT, which is also optional, \"\\s+|\\s+$\", causes this function to trim whitespace from the beggining and end of each substring.  Here's an example:

    (split-n-trim \"Hello  beautiful      world!\")

    => '(\"Hello\" \"beautiful\" \"world!\")"
  (remove-if (lambda (s) (zerop (length s)))
             (mapcar (lambda (x) (trim x fat))
                     (split on-regex string))))

(defun trim (s &optional (fat "^\\s+|\\s+$"))
  "Trim FAT from the string in S.  The FAT parameter is optional and defaults to \"^\\s+|\\s+$\", which means \"Whitespace at the beginning or end of the string\"."
  (regex-replace-all fat s ""))

(defun flatten (l)
  "Given a nested list L, return a flat list."
  (cond
    ((null l) nil)
    ((atom l) (list l))
    (t (loop for i in l append (flatten i)))))

(defun command-line-options (short-long-keyword-list)
  (loop for v in (cdr sb-ext:*posix-argv*)
     collect
       (loop named slk-loop for slk in short-long-keyword-list
          when (member v slk :test 'equal) do (return-from slk-loop (third slk))
          finally (return-from slk-loop v))))

(defun shuffle (seq)
  "Return a sequence with the same elements as the given sequence S, but in random order (shuffled)."
  (loop
     with l = (length seq) with w = (make-array l :initial-contents seq)
     for i from 0 below l for r = (random l) for h = (aref w i)
     do (setf (aref w i) (aref w r)) (setf (aref w r) h)
     finally (return (if (listp seq) (map 'list 'identity w) w))))

(defun memoize (function-symbol)
  "Incorporate caching into a function, specified by the symbol FUNCTION-SYMBOL, so that when the function is called with the same parameter a second time, it can retrieve the result from the cache instead of having to compute the result again."
  (let ((cache (make-hash-table :test 'equal))
        (g (symbol-function function-symbol)))
    (setf (symbol-function function-symbol)
          (lambda (&rest p)
            (let ((v (gethash p cache)))
              (if v v (setf (gethash p cache)
                            (apply g p))))))))

(defun memoize-with-limit (function-symbol limit)
  "Like memoize, but limits the size of the cache.  If more elements than LIMIT are cached when a new element needs to be cached, the oldest element is evicted from the cache to make room for the new one.  This is an excellent memoizing function to use when the function frequently returns a limited set of values, but has an infinite range."
  (let ((cache (make-hash-table :test 'equal :size limit))
        (fifo nil)
        (g (symbol-function function-symbol)))
    (setf (symbol-function function-symbol)
          (lambda (&rest p)
            (let ((v (gethash p cache)))
              (if v v (progn
                        (when (>= (length fifo) limit)
                          (remhash (shift fifo) cache)
                          (nbutlast fifo))
                        (setf (gethash p cache) (apply g p)))))))))

(defun shift (list)
  "This function is like the Common Lisp pop function, but takes an element from the end of LIST instead of from the front of LIST."
  (let ((value (car (last list))))
    (nbutlast list)
    value))

(defun fib (x)
  "Compute the sum of the first X numbers in the Fibonnacci series."
  (cond
    ((zerop x) 0)
    ((= x 1) 1)
    (t (+ (fib (1- x)) (fib (- x 2))))))

(defun alist-values (alist &rest keys)
  "Returns the values associated with KEYS in ALIST.  ALIST is an associative list."
  (loop for key in keys collect (cdr (assoc key alist))))

(defun cull-named-params (named-params cull-keys)
  "Given a value for NAMED-PARAMS like this one

    '(:one 1 :two 2 :three 3)

and a list of CULL-KEYS like this one

    '(:one :two)

this function returns a list of named parameters that excludes the names (and their values) that match the names in CULL-KEYS.  In the above example, the result is

    '(:three 3)"
  (let ((cull-keys (if (listp cull-keys) cull-keys (list cull-keys))))
    (loop for key in
         (remove-if (lambda (x) (member x cull-keys))
                    (loop for i from 0 below (length named-params) by 2
                       collect (elt named-params i)))
         appending (list key (getf named-params key)))))

(defun list-keys (plist)
  "Returns the keys (properties) of the property list PLIST"
  (loop for (k v) on plist by #'cddr collect k))

(defun list-values (plist)
  "Returns the values of the property list PLIST"
  (loop for (k v) on plist by #'cddr collect v))

(defun hash-keys (hash)
  "Returns a list of all the keys in HASH, which is a hash table."
  (loop for a being the hash-keys in hash collect a))

(defun hash-values (hash)
  "Returns a list of all the values in HASH, which is a hash table."
  (loop for a being the hash-values in hash collect a))

(defun interruptible-sleep (secs name)
  "Sets up a named timer and sleeps for SECS seconds or until another thread calls the interrupt-sleep function with NAME.  This function checks once per second to see if the timer has been reached or interrupted."
  (let ((target (+ (get-universal-time) secs)))
    (setf (gethash name *interruptible-sleep-hash*) nil)
    (loop while (and (< (get-universal-time) target)
                     (not (gethash name *interruptible-sleep-hash*)))
       do (sleep 1))
    (remhash name *interruptible-sleep-hash*)))

(defun interrupt-sleep (name)
  "Interrupts an active timer set with another thread using the interruptible-sleep function.  The NAME parameter specifies the name of the timer to interrupt."
  (setf (gethash name *interruptible-sleep-hash*) t))

(defun ds (list-or-atom &optional type)
  "Create a dc-utilities nested data structure.  Each node in LIST-OR-ATOM can be a scalar value or object, a map (hash table), an array, or a list.  Here's an example:

    (ds '(:array (:map :name \"Donnie\" :age 50 :height \"6'4\" :weight 225)
                 (:map :name \"Tracy\" :age 45 :height \"5'0'\" :weight 120)))

When you create a dc-utilities data structure like the one above, you can use other data-structure functions to easily access and manipulate the data."
  (let ((l (if (and type (listp list-or-atom) (not (null list-or-atom)))
               (cons type list-or-atom)
               list-or-atom)))
    (if (atom l)
        l
        (let ((type (pop l)))
          (case type
            (:map (loop with h = (make-hash-table :test #'equal)
                     while l
                     for key = (pop l)
                     for val = (ds (pop l))
                     do (setf (gethash key h) val)
                     finally (return h)))
            (:array (apply #'vector (mapcar 'ds l)))
            (:list (mapcar #'ds l))
            (t (error (format nil "Unknown collection type ~a" type))))))))

(defun ds-get (ds &rest keys)
  "Get a node (a leaf or a subtree) of DS, a dc-utilities data structure.  The parameters that follow ds, collected in KEYS, describe the path to the node.  For example, given the following data structure in bogus-ds:

    (ds '(:array (:map :name \"Donnie\" :age 50 :height \"6'4\" :weight 225)
                 (:map :name \"Tracy\" :age 45 :height \"5'0'\" :weight 120)))

You can get Tracy's weight like this:

    (ds-get bogus-ds 1 :weight)

or like this:

    (ds-get (elt (remove-if-not (lambda (x) (string= (ds-get x :name) \"Tracy\"))
                                bogus-ds)
                 0)
            :weight)"
  (if keys
      (case (ds-type ds)
        (hash-table
         (multiple-value-bind (value exists)
             (gethash (car keys) ds)
           (if exists
               (if (= (length keys) 1)
                   (values value t)
                   (values (apply #'ds-get (cons value (cdr keys))) t))
               (values nil nil))))
        (sequence
         (if (< (car keys) (length ds))
             (if (= (length keys) 1)
                 (values (elt ds (car keys)) t)
                 (values (apply #'ds-get (cons (elt ds (car keys))
                                               (cdr keys)))
                         t))
             (values nil nil)))
        (t (values nil nil)))
      (values ds t)))

(defun read-settings-file (&rest filepaths)
  "Accepts one or more parameters, collected in FILEPATHS, that are the names of settings files.  Reads the settings files in the order provided, with settings in later files overriding settings in earlier files.  A settings file is a Lisp file with a dc-utilities data structure (see the function ds).  This function returns a settings data structure.  Normally, you wouldn't use this function.  Instead, use the load-settings function at the beginning of your program (or when it needs to reload settings) and then use the setting function to retrieve values."
    (loop
       with settings-ds = (loop for filepath in filepaths
                             collect (ds (slurp-n-thaw filepath)))
       with settings = (car settings-ds)
       for ds in (cdr settings-ds)
       do (setf settings (ds-merge settings ds))
       finally (return settings)))

(defun load-settings (&rest filepaths)
  "Accepts one or more file paths, collected in FILEPATHS, and reads settings from the given files, with settings in later files overriding the same settings in earlier files.  Each settings file is a Lisp file with a dc-utilities data structure."
  (setf *settings*
        (apply #'read-settings-file filepaths)))

(defun setting (&rest keys)
  "Accepts one or more parameters, collected in KEYS, which are used to traverse the settings data structure to locate the desired value."
  (apply #'ds-get (cons *settings* keys)))

(defun ds-keys (ds &optional parent-keys)
  "Given a dc-utilities data structure DS, this function returns the path to every leaf.  If you provide a key or list of keys in PARENT-KEYS, those keys are prepended to the path to every leaf."
  (when (and parent-keys (atom parent-keys))
    (setf parent-keys (list parent-keys)))
  (case (ds-type ds)
    (hash-table
     (loop for k being the hash-keys in ds
        for new-parent-keys = (append parent-keys (list k))
        for child-ds = (gethash k ds)
        for child-keys = (ds-keys child-ds new-parent-keys)
        append child-keys))
    (sequence
     (loop for i from 0 below (length ds)
        for new-parent-keys = (append parent-keys (list i))
        for child-ds = (elt ds i)
        append (ds-keys child-ds new-parent-keys)))
    (t (list parent-keys))))

(defun ds-type (ds)
  "Given a dc-utilities data structure DS, this function returns the type of the data structure.  Valid return values include 'string, 'sequence, 'hash-table, and some Common Lisp types."
  (let* ((a (type-of ds))
         (b (string-downcase (format nil "~a" a))))
    (cond ((ppcre:scan
            "simple-array character|vector character"
            b)
           'string)
          ((or (string= b "cons")
               (ppcre:scan "vector|array" b))
           'sequence)
          ((atom a) a)
          (t (car a)))))

(defun ds-set (ds location-key-path value)
  "In the given dc-utilities data structure DS, this function sets the value of the node at LOCATION-KEY-PATH, which is a key or a list of keys, to VALUE."
  (let* ((keys (if (atom location-key-path)
                   (list location-key-path)
                   location-key-path))
        (key (car keys)))
    (if (= (length keys) 1)
        (progn
          (case (ds-type ds)
            (hash-table (setf (gethash key ds) value))
            (sequence (setf (elt ds key) value))
            (t (setf ds (make-hash-table)) (ds-set ds key value)))
          ds)
        (multiple-value-bind (target-ds exists)
            (ds-get ds key)
          (if exists
              (ds-set target-ds (cdr keys) value)
              (progn
                (case (ds-type ds)
                  (hash-table (setf (gethash key ds) (make-hash-table)))
                  (sequence (setf (elt ds key) (make-hash-table))))
                (setf target-ds (ds-get ds key))
                (ds-set target-ds (cdr keys) value)))))))

(defun ds-merge (ds-base &rest ds-set)
  "Merges dc-utilities data structures, starting with DS-BASE and then progressing through the rest of the data structures, collected in ds-set, in order.  Values in later data structures override values in earlier data structures when the paths of the values coincide."
  (loop with ds-main = (ds-clone ds-base)
     for ds in ds-set
     do (loop for key-path in (ds-keys ds)
           do (ds-set ds-main key-path (apply #'ds-get (cons ds key-path))))
     finally (return ds-main)))

(defun ds-clone (ds)
  "Clone the dc-utilities data structure DS."
  (case (ds-type ds)
    (hash-table
     (loop with ds-new = (make-hash-table :test 'equal)
        for key being the hash-keys in ds
          do (setf (gethash key ds-new) (ds-clone (gethash key ds)))
          finally (return ds-new)))
    (string
     (copy-seq ds))
    (sequence
     (if (equal (type-of ds) 'cons)
         (loop
            with ds-new = nil
            for i from 0 below (length ds)
            do (push (ds-clone (elt ds i)) ds-new)
            finally (return ds-new))
         (loop
            with l = (length ds)
            with ds-new = (make-array l)
            for i from 0 below l
            do (setf (elt ds-new i) (ds-clone (elt ds i)))
            finally (return ds-new))))
    (t ds)))

(defun ds-list (ds)
  "Render the dc-utilities data structure DS in a human-readable way"
  (case (ds-type ds)
    (hash-table
     (loop with list = (list :map)
        for k being the hash-keys in ds
        for v = (gethash k ds)
        do (push k list)
          (push (ds-list v) list)
        finally (return (nreverse list))))
    (string
     (map 'string 'identity (copy-seq ds)))
    (sequence
     (if (equal (type-of ds) 'cons)
         (loop
            with list = (list :list)
            for a in ds
            do (push (ds-list a) list)
            finally (return (nreverse list)))
         (loop
            with list = (list :array)
            for a across ds
            do (push (ds-list a) list)
            finally (return (nreverse list)))))
    (otherwise ds)))

(defun ds-from-json (json)
  "Creates a dc-utilities data structure from JSON.  This is useful if you want to easily traverse the JSON data structure."
  (let* ((data (yason:parse json)))
    (ds (if (hash-table-p data)
            (ds data)
            (ds (cons :array data))))))

(defun ds-to-json (ds)
  "Converts the dc-utilities data structure DS into JSON."
  (case (ds-type ds)
    (hash-table
     (format nil "{~{~a~^,~}}"
             (loop for k being the hash-keys in ds using (hash-value v)
                for v-json = (ds-to-json v)
                for k-json = (if (symbolp k) (string-downcase (format nil "~a" k)) k)
                collect (format nil "\"~a\":~a" k-json v-json))))
    (sequence
     (format nil "[~{~a~^,~}]"
             (if (consp ds)
                 (loop with list = nil
                    for a in ds do (push (ds-to-json a) list)
                    finally (return (nreverse list)))
                 (loop with list = nil
                    for a across ds do (push (ds-to-json a) list)
                    finally (return (nreverse list))))))
    (otherwise
     (let ((v (if (and ds (symbolp ds)) (string-downcase (format nil "~a" ds)) ds)))
       (format nil
               (cond
                 ((floatp v) "~,9f")
                 ((numberp v) "~a")
                 ((null v) "null")
                 (t "~s")) v)))))

(defun hash-string (string)
  "Hash STRING and return a hex representation of the hash"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    'ironclad:sha512 (string-to-utf-8-bytes string))))

(defun index-values (l)
  "Accepts a list of values L and puts the values in a hash table, keying each value with the index of the value in the list."
  (loop with hash = (make-hash-table :test 'equal)
     for value in l
     for key = 0 then (1+ key)
     do (setf (gethash key hash) value)
     finally (return hash)))

(defun distinct-elements (list)
  "Accepts a list of elements LIST and returns a new list with distinct elements from the first list.  (Copies the original list, removes duplicate elements from the copy, and returns the copy.)"
  (loop with h = (make-hash-table :test 'equal)
     for v = 0 then (1+ v)
     for k in list do (unless (gethash k h)
                        (setf (gethash k h) v))
     finally
       (return (loop for u being the hash-keys of h
                  collect u into distinct-list
                  finally
                    (return (sort distinct-list
                                  (lambda (a b)
                                    (< (gethash a h) (gethash b h)))))))))

(defun range (start end &key (step 1) (filter #'identity) shuffle)
  "Returns a list of values between START and END (inclusive), skipping values by STEP, filtering remaining values with the function in FILTER, and shuffling the remaining values if SHUFFLE is true.  STEP defaults to 1, FILTER defaults to allowing all values through, and SHUFFLE default to nil."
  (let ((range (loop for a from start to end by step
                  when (funcall filter a) collect a)))
    (if shuffle (shuffle range) range)))

(defun change-per-second (function-or-symbol &optional (seconds 1))
  "Given the function FUNCTION-OR-SYMBOL, who's return value changes over time, or a variable who's value changes over time, with the change being unidirectional, this function computes the rate of change by calling the function, sleeping SECONDS seconds, calling the function again, then computing the rate of change per second.  You can optionally specify the number of seconds to wait between calls with the SECONDS parameter, which defaults to 1.  If FUNCTION-OR-SYMBOL is a variable, then this function retrieves the value of the variable, sleeps, then retrieves the value of the variable again."
  (let ((v1 (if (functionp function-or-symbol)
                (funcall function-or-symbol)
                (symbol-value function-or-symbol)))
        (v2 (progn (sleep seconds)
                   (if (functionp function-or-symbol)
                       (funcall function-or-symbol)
                       (symbol-value function-or-symbol)))))
    (/ (abs (- v1 v2)) (float seconds))))

(defun time-to-go (change-per-second record-count)
  "Given the number of records per second that are being processed (given in CHANGE-PER-SECOND) and the nuber of records remaining (given in RECORD-COUNT), this function computes the amount of time still left before all the records have been processed."
  (let* ((seconds (/ record-count (float change-per-second)))
         (hours (/ seconds 3600.0))
         (days (/ hours 24.0)))
    (list :@ change-per-second :to-go record-count
          :seconds seconds :hours hours :days days)))

(defun thread-pool-time-to-go (pool-name total-record-count)
  "Returns the amount of time left for the thread pool given by POOL-NAME to complete processing all the records, the total number of which is given in TOTAL-RECORD-COUNT."
  (time-to-go
   (change-per-second (lambda () (thread-pool-progress pool-name)) 10)
   (- total-record-count (thread-pool-progress pool-name))))

(defun thread-pool-start-time (pool-name)
  "Retrieves the start-time for thread-pool named in POOL-NAME."
  (getf *dc-thread-pool-start-time* pool-name))

(defun thread-pool-stop-time (pool-name)
  "Retrieves the stop-time for the thread-pool named in POOL-NAME."
  (getf *dc-thread-pool-stop-time* pool-name))

(defun thread-pool-run-time (pool-name)
  "Computes the number of seconds that the thread-pool named in POOL-NAME has been running."
  (let ((start (thread-pool-start-time pool-name))
        (stop (thread-pool-stop-time pool-name)))
    (if start (- (if stop stop (get-universal-time)) start) 0)))

(defun thread-pool-progress (pool-name)
  "Retrieves the count of the records that the thread pool given by POOL-NAME has already processed."
  (getf *dc-thread-pool-progress* pool-name))

(defun job-queue (pool-name)
  "Retrieve the job-queue for the thread pool given by POOL-NAME."
  (getf *dc-job-queue* pool-name))

(defun thread-pool-start
    (&key (pool-name (error "pool-name parameter is required"))
       (thread-count (error "thread-count parameter is required"))
       (job-queue (error "job-queue paramater is required"))
       (fn-job (error "fn-job parameter is required"))
       (standard-output *standard-output*)
       fn-finally)
  "Starts THREAD-COUNT threads using POOL-NAME (a keyword symbol) to name the threads and runs FN-JOB with those threads.  Each thread runs FN-JOB, which takes no parameters, in a loop.  When all the threads are done, this function checks FN-FINALLY.  If the caller provides FN-FINALLY, then this function returns with the result of calling FN-FINALLY.  If the caller doesn't provide FN-FINALLY, then the this function exits with a sum of the return values of all the threads that ran."
  (setf (getf *dc-thread-pool-progress* pool-name) 0)
  (setf (getf *dc-progress-mutex* pool-name)
        (make-mutex :name (symbol-name pool-name)))
  (setf (getf *dc-job-queue-mutex* pool-name)
        (make-mutex :name (symbol-name pool-name)))
  (setf (getf *dc-thread-pool-start-time* pool-name)
        (get-universal-time))
  (setf (getf *dc-thread-pool-stop-time* pool-name) nil)
  (setf (getf *dc-thread-pool-done* pool-name) nil)
  (make-thread
   (lambda ()
     (let* ((get-job (if (eql (type-of job-queue) 'function)
                         job-queue
                         (progn
                           (setf (getf *dc-job-queue* pool-name)
                                 (copy-list job-queue))
                           (lambda ()
                             (with-mutex ((getf *dc-job-queue-mutex* pool-name))
                               (pop (getf *dc-job-queue* pool-name)))))))
            (threads
             (loop
                for a from 1 to thread-count
                for name = (format nil "~a-~3,'0d" pool-name a)
                collect
                  (make-thread
                   (lambda ()
                     (loop for job = (funcall get-job)
                        while (and job (not (getf *dc-thread-pool-done* pool-name)))
                        do (funcall fn-job standard-output job)
                          (with-mutex
                              ((getf *dc-progress-mutex* pool-name))
                            (incf (getf *dc-thread-pool-progress* pool-name)))
                        summing 1))
                   :name name))))
       (loop for thread in threads
          summing (or (sb-thread:join-thread thread) 0) into total
          finally (progn
                    (setf (getf *dc-thread-pool-stop-time* pool-name)
                          (get-universal-time))
                    (return (if fn-finally
                                (funcall fn-finally standard-output)
                                total))))))
   :name (format nil "~a-000" pool-name)))

(defun thread-pool-stop (pool-name)
  "Stops all the threads in the thread-pool POOL-NAME."
  (loop for threads = (remove-if-not
                       (lambda (x)
                         (scan (format nil "^~a" pool-name)
                               (sb-thread:thread-name x)))
                       (sb-thread:list-all-threads))
     while threads do
       (loop for thread in threads
          do (sb-thread:terminate-thread thread))
       (sleep 3)
     finally (sb-thread:list-all-threads)))


;;
;; Math
;;

(defun factorial (n)
  "Computes the factorial for N."
  (loop for a from n downto 1
        for b = a then (* b a)
        finally (return b)))

(defun k-combination (k n)
  "Computes the k-combination value for K and N."
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

;;
;; dc-store
;;

(defun file-exists (path)
  "Returns a boolean value indicating if the file specified by PATH exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (file-namestring path) "")))))

(defun directory-exists (path)
  "Returns a boolean value indicating if the directory specified by PATH exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (directory-namestring path) ""))
         (equal (file-namestring path) ""))))

(defun file-extension (path)
  "Returns a string consisting of the file extension for the file name given in PATH."
  (multiple-value-bind (a b)
      (ppcre:scan-to-strings "\\.([a-z0-9]+)$" path)
    (when a (aref b 0))))

(defun replace-extension (filename new-extension)
  "This function replaces the file extension in FILENAME with the file extension provided in NEW-EXTENSION."
  (let* ((new-extension (if (scan "^\\." new-extension)
                            (subseq new-extension 1)
                            new-extension))
         (new-filename (multiple-value-bind (a b)
                           (scan-to-strings "^(.*)\\.[^.]+$" filename)
                         (declare (ignore a))
                         (if b (elt b 0) filename))))
    (when (and new-filename (not (zerop (length new-extension))))
      (setf new-filename (format nil "~a.~a" new-filename new-extension)))
    new-filename))

(defun store-path (root filename)
  "Computes a path from ROOT (a root folder) and FILENAME, a regular file.  This is useful for when you plan to write many more files than can be held in a single directory.  This function will help you create a tree of directories for the files that you want to store.  FILENAME must have at least 15 characters, and the last 15 characters of FILENAME must be alphanumeric characters."
  (when (string= root "/") (setf root ""))
  (loop for a from 0 below 5
     for b = (* a 3)
     collect (subseq filename b (+ b 3)) into folders
     finally
       (return
         (join-paths
          root
          (apply #'join-paths
                 (reverse folders))))))

(defun store-save (root key object)
  "Freezes (serializes into a string) the object in OBJECT, then stores the string in the file specified by ROOT and KEY.  This function calls the store-path function to create the path where the object is going to be stored.  Therefore, KEY must have at least 15 characters and the last 15 characters of KEY must be alphanumeric characters."
  (let* ((contents (freeze object))
         (path (store-path root key))
         (abs-filename (join-paths path key)))
    (spew contents abs-filename :create-directories t)
    key))

(defun store-fetch (root key)
  "Reads the content of the file specified by ROOT and KEY, and deserializes that content into an object.  See the store-save and and store-path functions for information about how ROOT and KEY are treated."
  (let* ((path (store-path root key))
         (abs-filename (join-paths path key)))
    (when (file-exists abs-filename)
      (slurp-n-thaw abs-filename))))

(defun store-delete (root key)
  "Deletes the file specified by ROOT and KEY.  See the store-save and store-path functions for information about how ROOT and KEY are treated."
  (let* ((path (store-path root key))
         (abs-filename (join-paths path key))
         (article (store-fetch root key)))
    (when article
      (delete-file abs-filename)
      article)))

(defun uint-to-bytes (i &optional (size 4))
  "Converts the unsigned integer I into a list of bytes.  The SIZE parameter specifies the byte-size of the integer in I and defaults to 4."
  (loop with ff = 255
     for a = i then (ash a -8)
     for b from 1 to size
     collect (logand a ff)))

(defun bytes-to-uint (byte-list)
  "Converts the list of bytes BYTE-LIST into an unsigned integer."
  (loop for a in byte-list
     for b from 0 below (length byte-list)
     summing (* a (expt 2 (* b 8)))))

(defun sequence-uint-to-bytes (sequence &optional (size 4))
  "Converts SEQUENCE, a sequence of unsigned integers, into a list of bytes.  The SIZE parameter specifies the byte-size of the integers in SEQUENCE, and defaults to 4."
  (if (vectorp sequence)
      (loop with result = (make-array (* size (length sequence)))
         for a across sequence
         for index from 0 below (length sequence)
         do (loop with bytes = (uint-to-bytes a size)
               for byte in bytes
               for byte-index from 0 below size
               do (setf (aref result (+ (* index size) byte-index)) byte))
         finally (return result))
      (loop for a in sequence
           appending (uint-to-bytes a size))))

(defun sequence-bytes-to-uint (sequence &optional (size 4))
  "Converts SEQUENCE, a sequence of bytes, into a sequence of unsigned integers of byte-size SIZE, which defaults to 4."
  (if (vectorp sequence)
      (loop with result = (make-array (/ (length sequence) size))
         for source-index from 0 below (length sequence) by size
         for result-index from 0 below (length result)
         do (setf (aref result result-index)
                  (bytes-to-uint
                   (map 'list 'identity
                        (subseq sequence source-index (+ source-index size)))))
         finally (return result))
      (loop for index from 0 below (length sequence) by size
           collecting (bytes-to-uint (subseq sequence index (+ index size))))))

(defun fast-compress (v)
  "Uses a simple compression mechanism to very quickly compress the vector in V into a list."
  (loop with c = nil
     with l = (length v)
     for i from 0 below l
     for n = (aref v i)
     do (if (zerop n)
            (let ((run (loop for j = i then (1+ j)
                          while (< j l)
                          for m = n then (aref v j)
                          while (zerop m)
                          counting m into run
                          finally (return (list m run)))))
              (push (- (second run)) c)
              (unless (zerop (first run)) (push (first run) c))
              (incf i (second run)))
            (push n c))
     finally (return (nreverse c))))

(defun fast-decompress (l)
  "Quickly decompresses the list or vector L, created by fast-compress, into the original vector."
  (map 'vector 'identity
       (if (listp l)
           (loop for n in l appending
                (if (< n 0) (loop for a from 1 to (- n) collect 0) (list n)))
           (loop for n across l appending
                (if (< n 0) (loop for a from 1 to (- n) collect 0) (list n))))))

(defun parse-number (string)
  "Converts STRING, which contains a number, into the number."
  (with-input-from-string (s string) (read s)))

(defun home-based (path)
  "Prepends the user's home directory to PATH."
  (join-paths (namestring (user-homedir-pathname)) path))

(defun make-time-tracker ()
  "Creates a time-tracker object that you can use later in calls to mark-time and elapsed-time."
  (ds `(:map :mutex ,(make-mutex :name "time-tracker")
             :time-tracker ,(make-hash-table :test 'equal :synchronized t))))

(defun mark-time (time-tracker tag &key any-thread)
  "Marks the current time with TAG, for the purpose of later retrieving elapsed time.  You must pass in a TIME-TRACKER object, which you can create with the make-time-tracker function.  When you call mark-time from multiple threads, mark-time makes TAG visible only to the calling thread.  If two threads use the same TAG value to mark the time, the mark-time and elapsed time functions behave as if the TAG values were different.  You can change this behavior by passing T for ANY-THREAD, which causes a TAG to be global across threads.  See the elapsed-time function."
  (let* ((time-tracker (ds-get time-tracker :time-tracker))
         (mark (get-internal-real-time))
         (mark-name (if any-thread
                        tag
                        (format nil "~a-~a"
                            (sb-thread:thread-name sb-thread:*current-thread*)
                            tag))))
    (with-locked-hash-table (time-tracker)
      (setf (gethash mark-name time-tracker) mark))
    mark))

(defun elapsed-time (time-tracker tag &key any-thread)
  "Computes time elapsed since calling mark-time with TAG.  You must pass in a TIME-TRACKER object, which you can create with the make-time-tracker function.  When you call elapsed-time from multiple threads, elapsed-time associates TAG with the calling thread.  If two threads use the same TAG value to fetch elapsed time, the mark-time and elapsed time functions behave as if the TAG values were different.  You can change this behavior by passing T for ANY-THREAD, which causes TAG to be global across threads.  See the mark-time function."
  (let* ((time-tracker (ds-get time-tracker :time-tracker))
         (mark-name (if any-thread
                        tag
                        (format nil "~a-~a"
                            (sb-thread:thread-name sb-thread:*current-thread*)
                            tag))))
    (/ (- (get-internal-real-time)
          (with-locked-hash-table (time-tracker)
            (gethash mark-name time-tracker)))
       (float internal-time-units-per-second))))

(defun document-package (package output-filename &key overview-file license-file)
  "Documents the Common Lisp package PACKAGE and writes that documentation to the file given by OUTPUT-FILENAME.  If you provide file name for overview-file or license-file, document-package includes the contents of those files in the documentation it creates."
  (loop for function being the external-symbols of (find-package package)
     when (and (fboundp function)
               (eql (type-of (symbol-function function)) 'function)
               (documentation function 'function))
     collect
       (list :function function
             :function-name (string-downcase function)
             :function-type (cond ((macro-function function) "macro ")
                                  ((regular-function-p function) "function ")
                                  (t ""))
             :documentation (documentation function 'function))
     into functions
     finally
       (return (loop for function in
                    (sort functions #'string<
                          :key (lambda (x) (getf x :function-name)))
                  for lambda-list = (mapcar
                                     (lambda (x)
                                       (if (and (listp x)
                                                (stringp (second x)))
                                             (format nil "(~a ~s)" (first x) (second x))
                                           (format nil "~a" x)))
                                     (sb-introspect:function-lambda-list
                                      (symbol-function (getf function :function))))
                  collect (format nil "### ~a ~a ~a~%~a~%"
                                  (getf function :function-type)
                                  (string-downcase (getf function :function-name))
                                  (if (> (length lambda-list) 2)
                                      (format nil "~%(~%~{~%&nbsp;&nbsp;&nbsp;&nbsp;**~a**~%~}~%~%)~%"
                                              lambda-list)
                                      (format nil "~%(~{**~a**~^ ~})~%" lambda-list))
                                  (replace-regexs
                                   (getf function :documentation)
                                   '(("\\s\\s+" " "))))
                  into function-docs
                  finally
                    (return
                      (let* ((parts (remove
                                     nil
                                     (list (list "# ~a~%" package)
                                           (when license-file
                                             (list "## License~%~a~%"
                                                   (slurp license-file)))
                                           (when overview-file
                                             (list "## Overview~%~a~%"
                                                   (slurp overview-file)))
                                           (list "## API~%~{~a~^~%~}~%" function-docs))))
                             (format-string (format nil "~{~a~}" (mapcar #'car parts)))
                             (format-values (mapcar #'second parts))
                             (format-parameters (append (list nil format-string)
                                                        format-values)))
                        (spew (apply #'format format-parameters)
                              output-filename)
                        package))))))

(defun regular-function-p (symbol)
  "Returns t if SYMBOL is a regular function and not a macro or a special operator."
  (and (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun hashify-list (list
                     &key (method :count)
                       (f-key (lambda (x) x))
                       (f-value (lambda (key-raw key-clean value)
                                  (declare (ignore key-raw key-clean))
                                  value))
                       (initial-value 0))
  "Takes a list and returns a hash table, using the specified method. Supported methods, specified via the :method key,are :count, :merged-pairs, :pairs, and :custom.  With the :count method, which the function uses by default if no method is specified,causes the function to create a hash table in which the keys are the distinct items of the list and the value for each key is the count of that distinct element in the list.  The :pairs method assumes that the list contains key/value pairs and looks like this: '((key1 value1) (key2 value2) (key3 value3)...).  The :merged-pairs method works just like the :pairs method, but expects a list that looks like this: '(key1 value1 key2 value2 key3 value3 ...).  The :custom method requires that you provide functions for computing the key from the element in the list and for computing the value given the element, the computed key, and the existing hash value currently associated with the computed key.  If there's no hash value associated with the computed key, then the value specified via :initial-value is used. The :count, :pairs, and :merged-pairs methods allow you to specify functions for computing the key (given the element) and the value (given the element, the computed key, and the existing value)."
  (let ((h (make-hash-table :test 'equal)))
    (case method
      (:count (loop for k-raw in list
                 for k-clean = (funcall f-key k-raw)
                 do (incf (gethash k-clean h 0))))
      (:custom (loop for k-raw in list
                  for k-clean = (funcall f-key k-raw)
                  for value-old = (gethash k-clean h initial-value)
                  for value-new = (funcall f-value k-raw k-clean value-old)
                  do (setf (gethash k-clean h) value-new)))
      (:merged-pairs (loop for (k-raw value) in (find-pairs list)
                        for k-clean = (funcall f-key k-raw)
                        for value-new = (funcall f-value k-raw k-clean value)
                        do (setf (gethash k-clean h) value-new)))
      (:pairs (loop for (k-raw value) in list
                   for k-clean = (funcall f-key k-raw)
                   for value-new = (funcall f-value k-raw k-clean value)
                   do (setf (gethash k-clean h) value-new))))
    h))

(defun hash-to-list (hash)
  (loop for k being the hash-keys in hash using (hash-value v)
       collect (list k v)))

(defun find-pairs (list)
  (if (null list) nil
      (let ((k (pop list))
            (v (pop list)))
        (cons (list k v) (find-pairs list)))))
              
(defun choose-from-list (list n)
  (loop with h = (make-hash-table :test 'equal)
     and l = (length list)
     for a from 1 to n
     for b = (loop for c = (random l)
                while (gethash c h)
                finally (setf (gethash c h) t)
                  (return c))
     collect b))

(defun index-of-max (list)
  (if (arrayp list)
      (loop for index from 0 below (length list)
         for item across list
         for max-index = 0 then (if (> item max) index max-index)
         for max = item then (if (> item max) item max)
         finally (return max-index))
      (loop for index from 0 below (length list)
         for item in list
         for max-index = 0 then (if (> item max) index max-index)
         for max = item then (if (> item max) item max)
         finally (return max-index))))

(defun partition (sequence cell-size)
  (let ((list (if (vectorp sequence)
                  (map 'list 'identity sequence)
                  sequence)))
    (loop for cell on list by #'(lambda (list) (nthcdr cell-size list))
       collecting (subseq cell 0 cell-size))))
