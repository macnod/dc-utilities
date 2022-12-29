; Copyright © 2003-2013 Donnie Cameron

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
(defparameter *dc-thread-pool-done-mutex* nil)
(defparameter *dc-thread-pool-collector* nil)
(defparameter *dc-thread-pool-collector-mutex* nil)
(defparameter *dc-timings* (make-hash-table :test #'equal :synchronized t))
(defvar *unix-epoch-difference* (encode-universal-time 0 0 0 1 1 1970 0))
(defparameter *dc-aws-settings* nil)
(defparameter *dc-progress-hash* (make-hash-table :test 'equal))
(defparameter *tz-names* (make-hash-table :test 'equal))
(defparameter *tz-names-plist*
  '("UTC" 0 "GMT" 0 "PST" -8 "IST" 5.5 "EST" -5 "CST" -6))
(defparameter *hprimes* (make-hash-table :size 78498))
(defparameter *aprimes* (make-array 78498 :element-type 'integer))

(loop for (tz offset) on *tz-names-plist* by #'cddr
     do (setf (gethash tz *tz-names*) offset))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun month-string-to-int (month)
  (1+ (position (string-downcase (subseq month 0 3))
                (list "jan" "feb" "mar" "apr" "may" "jun" "jul"
                      "aug" "sep" "oct" "nov" "dec")
                :test 'equal)))

(defun time-zone-to-int (time-zone)
  (gethash (string-downcase time-zone) *tz-names*))

(defun parse-date-1 (string)
  "Parses a date of the format Sat, 29 Jun 2019 00:00:00 GMT.  Returns a universal-time integer."
  (let ((parts (split "[, :]+" string)))
    (when (= (length parts) 8)
      (destructuring-bind (wday mday month year hour minute second time-zone)
          parts
        (declare (ignore wday))
        (encode-universal-time
         (parse-integer second)
         (parse-integer minute)
         (parse-integer hour)
         (parse-integer mday)
         (month-string-to-int month)
         (parse-integer year)
         (time-zone-to-int time-zone))))))

(defun to-ascii (s)
  "Converts the string S, which may contain non-ascii characters, into a string with nothing but ascii characters.  Non ascii characters are converted into spaces.  If S is a list, this function converts each element of the list into an all-ascii string."
  (if (atom s)
      (map 'string (lambda (c) (if (> (char-code c) 127) #\Space c)) s)
      (loop for a in s collect
           (if (stringp a)
               (map 'string (lambda (c) (if (> (char-code c) 127) #\Space c)) a)
               (format nil "~a" a)))))

(defun dc-timestamp (&key
                       (time (get-universal-time))
                       string
                       time-zone
                       (format "Y-M-DTh:m:s"))
  "Returns the given time (or the current time) formatted according to the FORMAT parameter, followed by an optional value for STRING.  If STRING is provided, the function adds a space to the result and then appends the string to that.  The FORMAT string can contain any characters.  This function will replace the format characters Y, M, D, h, m, and s, respectively, with numbers representing the year,month, day, hour, minute, and second.  All the numbers are 2 digits long, except for the year, which is 4 digits long."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time time time-zone)
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
  (dc-timestamp :string (format nil "~{~a~}~%" messages)))

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

(defun replace-all-in-string (string part replacement)
  "Returns a new string created by replacing each occurrence of the part 
parameter (a string) with the value of the replacement parameter (another 
string)."
  (with-output-to-string (out)
    (loop with part-length = (length part)
			 for old-pos = 0 then (+ pos part-length)
			 for pos = (search part string
												 :start2 old-pos
												 :test #'char=)
			 do (write-string string out
												:start old-pos
												:end (or pos (length string)))
			 when pos do (write-string replacement out)
			 while pos)))

(defun shell-execute (program &optional parameters (input-pipe-data ""))
  "Run PROGRAM and return the output of the program as a string.  You can pass an atom or a list for PARAMETERS (the command-line options for the program). You can also pipe data to the program by passing the INPUT-PIPE-DATA parameter with a string containing the data you want to pipe.  The INPUT-PIPE-DATA parameter defaults to the empty string."
  (let ((parameters (cond ((null parameters) nil)
                          ((atom parameters) (list parameters))
                          (t parameters))))
    (with-output-to-string (output-stream)
      (with-output-to-string (error-stream)
        (with-input-from-string (input-stream input-pipe-data)
          (sb-ext:run-program program parameters
                              :search t
                              :output output-stream
                              :error error-stream
                              :input input-stream))))))

(defun shell-to-list (&rest program)
  (let* ((error-string nil)
	 (output-string (with-output-to-string (out)
			  (setf error-string
				(with-output-to-string (err)
				  (uiop:run-program
				   (format nil "~{~a~^ ~}" program) 
				   :output out 
				   :error-output err
				   :ignore-error-status t))))))
    (values (split #\Newline output-string) 
	    (unless (zerop (length error-string)) error-string))))

(defun shell-to-list-default (default &rest program)
  (multiple-value-bind (out err)
      (apply #'shell-to-list program)
    (values (if err default out) err)))

(defun shell-to-list-debug (&rest program)
  (let ((program (format nil "~{~a~^ ~}" program)))
    (format t "~a~%" program)
    (split #\Newline (with-output-to-string (out)
		       (uiop:run-program program :output out)))))

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
  (unless (directory-exists-p dir)
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

(defun spew (string filename &key create-directories)
  "Writes the contents of STRING to the file specified by FILENAME.  Use the CREATE-DIRECTORIES parameter if any of the directories in the path in FILENAME don't exist and you want to create them.  Use the APPEND parameter if you want to append STRING to an existing file."
  (when create-directories
		(ensure-directories-exist filename))
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (write-string string stream))
  nil)

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

(defun unique-file-name (&key (directory "/tmp") (prefix "") (extension ".tmp"))
  "Returns a made-up, unique file name.  DIRECTORY defaults to /tmp, prefix to '', and EXTENSION to .tmp."
  (join-paths
   directory
   (format nil "~a~a~a"
	   prefix
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

(defun trim-whitespace (s)
	(string-trim '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return 
								 #\Rubout)
							 s))

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

(defun plist-keys (plist)
  "Returns the keys (properties) of the property list PLIST"
  (loop for (k v) on plist by #'cddr collect k))

(defun list-keys (plist) (plist-keys plist))

(defun plist-values (plist)
  "Returns the values of the property list PLIST"
  (loop for (k v) on plist by #'cddr collect v))

(defun list-values (plist) (plist-values plist))

(defun hash-keys (hash &optional sort-predicate)
  "Returns a list of all the keys in HASH, which is a hash table."
  (loop for a being the hash-keys in hash 
     collect a into keys
     finally (if sort-predicate
                 (sort keys sort-predicate)
                 keys)))

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
  (when (null filepaths)
    (setf filepaths (list (home-based "common-lisp/settings.lisp"))))
  (setf *settings*
        (apply #'read-settings-file filepaths))
  (replace-settings-vars *settings*))

(defun replace-settings-vars (settings)
  (loop for k being the hash-keys in settings
     using (hash-value v)
     when (and (hash-table-p v)
               (not (equal k :vars)))
     do (replace-settings-vars v)
     when (stringp v)
     do (ds-set settings k (replace-regexs v (hash-to-list (setting :vars))))))

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

(defun hash-hmac-256 (secret text)
  (let ((hmac (ironclad:make-hmac
               (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array text))
    (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))

(defun index-values (l)
  "Accepts a list of values L and puts the values in a hash table, keying each value with the index of the value in the list."
  (loop with hash = (make-hash-table :test 'equal)
     for value in l
     for key = 0 then (1+ key)
     do (setf (gethash key hash) value)
     finally (return hash)))

;; (defun distinct-elements (list)
;;   "Accepts a list of elements LIST and returns a new list with distinct elements from the first list.  The function Copies the original list, removes duplicate elements from the copy, and returns the copy, all while preserving the order of the original list."
;;   (loop with h = (make-hash-table :test 'equal)
;;      for v = 0 then (1+ v)
;;      for k in list
;;      when (not (gethash k h)) do (setf (gethash k h) v)
;;      finally
;;        (return (sort (hash-keys h)
;;                      (lambda (a b) (< (gethash a h) (gethash b h)))))))

(defun distinct-elements (sequence &key (key (lambda (x) x)))
  "Accespts a sequence of elements (list or vector) and returns a new sequence of the same type with distinct elements from the original sequence.  If the elements in the sequence are hash tables, plists, or objects with methods, then you can provide a value or function for the :key parameter.  If you provide a value, the function will use the value as the key of the element, and the value of the key will represent the unique signature of the element.  If you provide a function, then the function will be applied to the element to compute the elements unique signature."
  (let* ((list (if (vectorp sequence)
                   (map 'list 'identity sequence)
                   sequence))
         (f-key (if (functionp key)
                    key
                    (cond ((hash-table-p (car list))
                           (lambda (x) (gethash key x)))
                          ((listp (car list))
                           (lambda (x) (getf x key)))
                          (t (lambda (x) (funcall key x))))))
         (distinct (hash-values
                    (hashify-list list :method :index :f-key f-key))))
    (if (vectorp sequence)
        (map 'vector 'identity distinct)
        distinct)))

(defun distinct-values (list)
  (distinct-elements list))

(defun numeric-range (start end &key (step 1) (filter #'identity) shuffle)
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
       (fn-job (error "fn-job parameter is required")))
  "Starts THREAD-COUNT threads using POOL-NAME (a keyword symbol) to name the threads and runs FN-JOB with those threads.  Each thread runs FN-JOB, which takes no parameters, in a loop.  When all the threads are done, this function checks FN-FINALLY.  If the caller provides FN-FINALLY, then this function returns with the result of calling FN-FINALLY.  If the caller doesn't provide FN-FINALLY, then the this function exits with a sum of the return values of all the threads that ran."
  (thread-pool-init-variables pool-name)
  (make-thread
   (lambda ()
     (let* ((fn-collect
             (lambda (x)
							 (let ((collection (getf *dc-thread-pool-collector* pool-name)))
								 (setf (getf *dc-thread-pool-collector* pool-name)
											 (cons x collection)))))
            (fn-get-next-job
             (if (eql (type-of job-queue) 'function)
                 job-queue
                 (progn (setf (getf *dc-job-queue* pool-name)
                              (copy-list job-queue))
                        (lambda ()
                          (let ((job (with-mutex
                                         ((getf *dc-job-queue-mutex* pool-name))
                                       (pop (getf *dc-job-queue* pool-name)))))
                            (when job
                              (incf (getf *dc-thread-pool-progress* pool-name)))
                            job)))))
            (fn-mark-done
             (lambda ()
               (with-mutex ((getf *dc-thread-pool-done-mutex* pool-name))
                 (setf (getf *dc-thread-pool-done* pool-name) t))))
            (fn-check-done
             (lambda () (getf *dc-thread-pool-done* pool-name)))
            (threads
             (loop for a from 1 to thread-count
                for name = (format nil "~a-~3,'0d" pool-name a)
                collect
                  (make-thread (job-thread fn-get-next-job
                                           fn-job
                                           pool-name
                                           fn-collect
                                           fn-mark-done
                                           fn-check-done)
                               :name name))))
       (loop for thread in threads do (sb-thread:join-thread thread))))
	 :name (format nil "~a-000" pool-name)))

(defun thread-pool-init-variables (pool-name)
  (setf (getf *dc-thread-pool-progress* pool-name) 0

        (getf *dc-progress-mutex* pool-name)
        (make-mutex :name (symbol-name pool-name))

        (getf *dc-job-queue-mutex* pool-name)
        (make-mutex :name (symbol-name pool-name))

        (getf *dc-thread-pool-start-time* pool-name)
        (get-universal-time)

        (getf *dc-thread-pool-stop-time* pool-name) nil

        (getf *dc-thread-pool-done-mutex* pool-name)
        (make-mutex :name (symbol-name pool-name))

        (getf *dc-thread-pool-collector-mutex* pool-name)
        (make-mutex :name (symbol-name pool-name))

        (getf *dc-thread-pool-collector* pool-name) nil

        (getf *dc-thread-pool-done* pool-name) nil))

(defun job-thread (fn-get-next-job
                   fn-job
                   pool-name
                   fn-collect
                   fn-mark-done
                   fn-check-done)
  (lambda ()
    (loop for job = (funcall fn-get-next-job)
       while (and job (not (getf *dc-thread-pool-done* pool-name)))
       for result = (funcall fn-job job fn-check-done fn-mark-done)
       when result do (funcall fn-collect result))))

(defun thread-pool-stop (pool-name)
  "Stops all the threads in the thread-pool POOL-NAME."
  (loop for threads = (remove-if-not
                       (lambda (x)
                         (scan (format nil "^~a" pool-name)
                               (sb-thread:thread-name x)))
                       (sb-thread:list-all-threads))
     while threads do
       (loop for thread in threads
          do (ignore-errors (sb-thread:terminate-thread thread)))
       (sleep 3)
     finally (sb-thread:list-all-threads)))

(defun thread-pool-result (pool-name)
	(let ((mutex (getf *dc-thread-pool-collector-mutex* pool-name)))
		(when mutex
			(with-mutex (mutex)
				(getf *dc-thread-pool-collector* pool-name)))))

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

(defun file-exists-p (path)
  "Returns a boolean value indicating if the file specified by PATH exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (file-namestring path) "")))))

(defun directory-exists-p (path)
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
    (when (file-exists-p abs-filename)
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
                            tag)))
         (mark (with-locked-hash-table (time-tracker)
                 (gethash mark-name time-tracker))))
    (when (null mark) (error "Unknown tag :~(~a~).  Call mark-time with this tag first." tag))
    (/ (- (get-internal-real-time)
          (with-locked-hash-table (time-tracker)
            (gethash mark-name time-tracker)))
       (float internal-time-units-per-second))))

;; (defun document-package (package output-filename &key overview-file license-file)
;;   "Documents the Common Lisp package PACKAGE and writes that documentation to the file given by OUTPUT-FILENAME.  If you provide file name for overview-file or license-file, document-package includes the contents of those files in the documentation it creates."
;;   (loop for function being the external-symbols of (find-package package)
;;      when (and (fboundp function)
;;                (eql (type-of (symbol-function function)) 'function)
;;                (documentation function 'function))
;;      collect
;;        (list :function function
;;              :function-name (string-downcase function)
;;              :function-type (cond ((macro-function function) "macro ")
;;                                   ((regular-function-p function) "function ")
;;                                   (t ""))
;;              :documentation (documentation function 'function))
;;      into functions
;;      finally
;;        (return (loop for function in
;;                     (sort functions #'string<
;;                           :key (lambda (x) (getf x :function-name)))
;;                   for lambda-list = (mapcar
;;                                      (lambda (x)
;;                                        (if (and (listp x)
;;                                                 (stringp (second x)))
;;                                              (format nil "(~a ~s)" (first x) (second x))
;;                                            (format nil "~a" x)))
;;                                      (sb-introspect:function-lambda-list
;;                                       (symbol-function (getf function :function))))
;;                   collect (format nil "### ~a ~a ~a~%~a~%"
;;                                   (getf function :function-type)
;;                                   (string-downcase (getf function :function-name))
;;                                   (if (> (length lambda-list) 2)
;;                                       (format nil "~%(~%~{~%&nbsp;&nbsp;&nbsp;&nbsp;**~a**~%~}~%~%)~%"
;;                                               lambda-list)
;;                                       (format nil "~%(~{**~a**~^ ~})~%" lambda-list))
;;                                   (replace-regexs
;;                                    (getf function :documentation)
;;                                    '(("\\s\\s+" " "))))
;;                   into function-docs
;;                   finally
;;                     (return
;;                       (let* ((parts (remove
;;                                      nil
;;                                      (list (list "# ~a~%" package)
;;                                            (when license-file
;;                                              (list "## License~%~a~%"
;;                                                    (slurp license-file)))
;;                                            (when overview-file
;;                                              (list "## Overview~%~a~%"
;;                                                    (slurp overview-file)))
;;                                            (list "## API~%~{~a~^~%~}~%" function-docs))))
;;                              (format-string (format nil "~{~a~}" (mapcar #'car parts)))
;;                              (format-values (mapcar #'second parts))
;;                              (format-parameters (append (list nil format-string)
;;                                                         format-values)))
;;                         (spew (apply #'format format-parameters)
;;                               output-filename)
;;                         package))))))

(defun regular-function-p (symbol)
  "Returns t if SYMBOL is a regular function and not a macro or a special operator."
  (and (fboundp symbol)
       (not (macro-function symbol))
       (not (special-operator-p symbol))))

(defun hashify-list (list
                     &key (method :count)
                       f-key
                       hash-key
                       plist-key
                       (f-value (lambda (key-raw key-clean value)
                                  (declare (ignore key-raw key-clean))
                                  value))
                       (initial-value 0))
  "Takes a list and returns a hash table, using the specified
method. Supported methods, specified via the :method key, are :count,
:plist, :alist, :index, and :custom.  With the :count method, which
the function uses by default if no method is specified, the function
to creates a hash table in which the keys are the distinct items of
the list and the value for each key is the count of that distinct
element in the list.  The :alist and :plist methods do the same thing,
but with alists and plists.  The :alist method assumes that the list
contains key/value pairs and looks like this: '((key1 . value1) (key2
. value2) (key3 . value3)...).  The :plist method works just like the
:alist method, but expects a list that looks like this: '(key1 value1
key2 value2 key3 value3 ...).  The :index method tells this function
that you to specify the key with one of the f-key, hash-key, and
plist-key parameters, and that the value should be the list value.  If
the objects in the list that you're indexing are hash tables, then you
can specify the index key with hash-key.  If the objects are plists,
then you can specify the index with plist-key.  hash-key and plist-key
are just shortcuts to save you from having to write some code for
f-key.  This allows you to later look up an element in the list, by
the given key, in O(1) time.  The :custom method requires that you
provide functions for computing the key from the element in the list
and for computing the value given the element, the computed key, and
the existing hash value currently associated with the computed key.
If there's no hash value associated with the computed key, then the
value specified via :initial-value is used. The :count, :pairs, and
:merged-pairs methods allow you to specify functions for computing the
key (given the element) and the value (given the element, the computed
key, and the existing value)."
  (let ((h (make-hash-table :test 'equal))
        (counter 0))
    (case method
      (:count (loop for k-raw in list
                 for key-function =
                   (cond (hash-key (lambda (x) (gethash hash-key x)))
                         (plist-key (lambda (x) (getf x plist-key)))
                         (f-key (if f-key f-key (lambda (x) x))))
                 for k-clean = (funcall key-function k-raw)
                 do (incf (gethash k-clean h 0))))
      (:custom (loop with key-function = (or f-key (lambda (x) x))
                  for k-raw in list
                  for k-clean = (funcall key-function k-raw)
                  for value-old = (gethash k-clean h initial-value)
                  for value-new = (funcall f-value k-raw k-clean value-old)
                  do (setf (gethash k-clean h) value-new)))
      (:plist (loop with key-function = (or f-key (lambda (x) x))
                 for (k-raw value) on list by #'cddr
                 for k-clean = (funcall key-function k-raw)
                 for value-new = (funcall f-value k-raw k-clean value)
                 do (setf (gethash k-clean h) value-new)))
      (:alist (loop with key-function = (or f-key (lambda (x) x))
                 for (k-raw value) in list
                 for k-clean = (funcall key-function k-raw)
                 for value-new = (funcall f-value k-raw k-clean value)
                 do (setf (gethash k-clean h) value-new)))
      (:index (loop with key-function =
                   (cond (hash-key (lambda (x) (gethash hash-key x)))
                         (plist-key (lambda (x) (getf x plist-key)))
                         (f-key f-key)
                         (t (lambda (x) (declare (ignore x)) (incf counter))))
                 for value in list
                 for k-raw = (funcall key-function value)
                 for k-clean = k-raw
                 for value-new = (funcall f-value k-raw k-clean value)
                 do (setf (gethash k-clean h) value-new))))
    h))

(defun hash-to-string (hash &key values-first (separator " "))
  (when hash
    (loop for key in (sort (loop for key being the hash-key in hash collect key) #'string<)
       for value = (gethash key hash)
       for format-string = (concatenate 'string "~{~a~^" separator "~}")
       collect (format nil format-string (if values-first 
                                             (list value key) 
                                             (list key value)))
       into lines
       finally (return (format nil "~{~a~%~}" lines)))))


(defun hash-to-list (hash &key values-first)
  (when hash
    (loop for k being the hash-keys in hash using (hash-value v)
       collect (if values-first (list v k) (list k v)))))

(defun hash-to-plist (hash)
  (when hash
    (loop for k being the hash-keys in hash using (hash-value v)
       appending (list k v))))

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
     collect (elt list b)))

(defgeneric index-of-max (vector)
  (:method ((vector vector))
    (index-of-max (map 'list 'identity vector)))
  (:method ((vector list))
    (loop with max-index = 0 and max-value = (elt vector 0)
       for value in vector
       for index = 0 then (1+ index)
       when (> value max-value)
       do 
         (setf max-index index)
         (setf max-value value)
       finally (return max-index))))

(defun string-to-keyword (string &optional (clean t))
  (let ((s (if clean
               (let* ((s1 (string-upcase
                           (regex-replace-all "[^-a-zA-Z0-9]" string "-")))
                      (s2 (regex-replace-all "--+" s1 "-"))
                      (s3 (regex-replace-all "^-+|-+$" s2 ""))
                      (s4 (if (zerop (length s3)) (unique-name) s3)))
                 (if (scan "^[A-Z]" s4) s4 (format nil "X-~a" s4)))
               string)))
    (intern s :keyword)))

(defun alist-to-plist (alist &optional remove-star-prefix)
  (loop for (key . value) in alist
     appending (list
                (if (and remove-star-prefix
                         (scan "^\\*.+" (format nil "~a" key)))
                    (string-to-keyword
                     (subseq (format nil "~a" key) 1))
                    key)
                (if (and value (listp value))
                    (alist-to-plist value)
                    value))))

(defun parse-if-number (value)
  (if (scan "^(?=.)([+-]?([0-9]*)(\.([0-9]+))?)$" value)
      (parse-number value)
      value))

(defun hash-list-from-csv (filename &key headers-in-file header-list
                                      (clean-headers t)
                                      (parse-numbers t))
  (let* ((rows (cl-csv:read-csv
                (if (pathnamep filename) filename (pathname filename))))
         (fields (cond
                   (header-list
                    (mapcar (lambda (k) (string-to-keyword k clean-headers))
                            header-list))
                   (headers-in-file
                    (mapcar (lambda (k) (string-to-keyword k clean-headers))
                            (car rows)))
                   (t (numeric-range 0 (1- (length (car rows))))))))
    (when (not (= (length (distinct-elements fields))
                  (length fields)))
      (setf fields
            (let ((hash (hashify-list fields)))
              (loop for field being the hash-keys in hash
                 using (hash-value count) appending
                   (if (= count 1)
                       field
                       (loop for a from 1 to count
                          collect (format nil "~a-~a" field a)))))))
    (loop for row in (if headers-in-file (cdr rows) rows)
       collect
         (loop with hash = (make-hash-table :test 'equal)
            for value in row
            for clean-value = (cond ((equal value "{{dcu-lisp-t}}") t)
                                    ((equal value "{{dcu-lisp-nil}}") nil)
                                    (parse-numbers (parse-if-number value))
                                    (t value))
            for field in fields
            do (setf (gethash field hash) clean-value)
            finally (return hash)))))

(defun hash-list-from-plists (plists)
  (loop for plist in plists collecting
     (loop with h = (make-hash-table :test 'equal)
        for (key value) on plist by #'cddr
        do (setf (gethash key h) value)
        finally (return h))))

(defun hash-list-from-list (key list &key stringify)
  (hash-list-from-plists
   (mapcar (lambda (x) (list key (if stringify (format nil "~a" x) x)))
           list)))

(defun hash-list-from-json-objects (json)
  (let* ((hash-list (loop
                       for line in (split "\\n" (trim json))
                       while (and line (not (equal (trim line) "")))
                       for record = (ds-from-json (trim line))
                       collect record))
         (old-keys (hash-list-keys hash-list))
         (new-keys (mapcar (lambda (k) (string-to-keyword k)) old-keys)))
    (apply #'hash-list-rename-columns
           (cons hash-list (loop for a in old-keys
                              for b in new-keys
                              appending (list a b))))
    hash-list))

(defun hash-list-from-json-objects-file (file &key limit)
  (with-open-file (s file)
    (loop for line = (read-line s nil)
       for line-number = 0 then (1+ line-number)
       while (and line (if limit (< line-number limit) t))
       collect line into lines
       finally (return (hash-list-from-json-objects
                        (format nil "~{~a~^~%~}" lines))))))

(defun hash-list-to-plists (hash-list)
  (loop with keys = (hash-keys (car hash-list))
     for row in hash-list
     collect (loop for key in keys appending (list key (gethash key row)))))

(defun plist-list-to-csv (plists filename)
  (hash-list-to-csv (hash-list-from-plists plists) filename))

(defun hash-list-to-csv (hash-list filename)
  (with-open-file (csv filename :direction :output :if-exists :supersede)
    (let* ((headers (hash-keys (car hash-list)))
           (escaped-headers (mapcar
                             (lambda (h)
                               (let* ((h1 (format nil "~(~a~)" h))
                                      (quote (scan ",|\"|\\n" h1))
                                      (h2 (regex-replace "\"" h1 "\"\"")))
                                 (if quote (format nil "\"~a\"" h2) h2)))
                             headers)))
      (format csv "~{~a~^,~}~%" escaped-headers)
      (loop for row in hash-list
         for data = (loop for key in headers
                       for value = (let ((v (gethash key row)))
                                     (cond ((eq v t) "{{dcu-lisp-t}}")
                                           ((eq v nil) "{{dcu-lisp-nil}}")
                                           (t (format nil "~a" v))))
                       collect (if (scan "," value)
                                   (format nil "\"~a\"" value)
                                   value))
         do (format csv "~{~a~^,~}~%" data)))))

(defun hash-list-to-table (hash-list)
  (let ((keys (hash-keys (car hash-list))))
    (format nil "|~{ ~a |~}~%|---~%~{~a~%~}"
            keys
            (loop with keys = (hash-keys (car hash-list))
               for hash-table in hash-list
               collect (format nil "|~{ ~a |~}"
                               (loop for key in keys collect
                                    (gethash key hash-table)))))))

(defun hash-list-to-json (hash-list)
  (format nil "[~{~a~^,~}]"
          (loop for row in hash-list collect
               (format nil "{~{\"~(~a~)\":\"~a\"~^,~}}"
                       (loop for k being the hash-keys in row
                          using (hash-value v)
                          appending (list k v))))))

(defun hash-list-to-json-objects (hash-list &optional file)
  (let ((data (format nil "~{~a~%~}"
                      (loop for row in hash-list collect
                           (format nil "{~{\"~(~a~)\":\"~a\"~^,~}}"
                                   (loop for k being the hash-keys in row
                                      using (hash-value v)
                                      appending (list k v)))))))
    (if file
        (with-open-file (s file :direction :output :if-exists :supersede)
          (write-string data s))
        data)))

(defun hash-list-rename-columns (hash-list &rest old-new)
  (unless (zerop (mod (length old-new) 2))
    (error "old-new must be an even number of parameters."))
  (loop for row in hash-list do
       (loop for index from 0 below (1- (length old-new)) by 2
          for old = (elt old-new index)
          for new = (elt old-new (1+ index))
          for value = (gethash old row)
          do (setf (gethash new row) value)
            (remhash old row))))

(defun plist-clean-keys (list)
  (loop for (k v) on list by #'cddr
     appending (list (string-to-keyword (format nil "~a" k)) v)))

(defun hash-list-columns (hash-list &rest column-names)
  "The result looks like the original hash-list, but with the specified columns only."
  (loop for row in hash-list
     collect (loop with h = (make-hash-table :test 'equal)
                for column in column-names
                do (setf (gethash column h) (gethash column row))
                finally (return h))))

(defun hash-list-column (hash-list column-name)
  "The result looks a list of the values in the given column of hash-list."
  (loop for row in hash-list collect (gethash column-name row)))

(defun hash-list-add-columns (hash-list &rest key-value-pairs)
  (unless (zerop (mod (length key-value-pairs) 2))
    (error "key-value-pairs must be an even number of parameters."))
  (loop for hash-table in hash-list
     for index = 0 then (1+ index)
     do (loop for (key value) on key-value-pairs by #'cddr
           do (setf (gethash key hash-table)
                    (if (functionp value)
                        (funcall value index hash-table)
                        value)))
     summing 1))

(defun hash-list-remove-columns (hash-list &rest columns)
  (loop for hash in hash-list
     when (loop for column in columns
             thereis (remhash column hash))
     summing 1))


(defun hash-list-first-index-of (hash-list &rest key-value-pairs)
  (loop for row in hash-list
     for index = 0 then (1+ index)
     when (loop for (key value) on key-value-pairs by #'cddr
             always (equal (gethash key row) value))
     do (return index)))

(defun hash-list-first-record-of (hash-list &rest key-value-pairs)
  (let ((index (apply #'hash-list-first-index-of
                      (cons hash-list key-value-pairs))))
    (when index (elt hash-list index))))

(defun hash-list-filter (hash-list &rest filters)
  "This function accepts HASH-LIST and multiple FILTERS. HASH-LIST is a list of hash-table objects. The return value consists of a list of items from the HASH-LIST that pass all the functions in FILTERS.  If you want HASH-LIST-FILTER to return items that pass one condition or another, then you have to include a function in FILTERS that implements that OR operation.  Each filter must be a key/value pair or a function.  A key/value pair filter must be expressed as a list with 2 items: key and value.  A function filter can be a reference to a function or a lambda.  To serve as a function filter, the function must accept the zero-based index of the hash table in HASH-LIST and the hash-table itself.  The signature of a function filter is: LAMBDA (INDEX HASH-TABLE) -> T.  The function filter must return T, to indicate that the hash table passes the filter, or nil otherwise."
  (loop 
		with keys = (hash-keys (car hash-list))
    and functions = (remove-if-not #'functionp filters)
    with closures = (loop for condition in (remove-if #'functionp filters)
                          when (not (member (car condition) keys :test
                                      (declare (ignore i))
                                      (equal (gethash key x) value))))
    with conditions = (concatenate 'list functions closures)
    for hash-table in hash-list
    for index = 0 then (1+ index)
    when (loop for c in conditions
               always (funcall c index hash-table))
      collect hash-table))

(defun hash-list-set (hash-list set &rest filters)
  "This function accepts a hash-list, a function that sets fields in each selected hash table, and one or more filters to select the hash tables that need the update.  The filters parameter works just like the filters parameter in the hash-list-filter.  The set function accepts 2 parameters: selection-index and hash-table.  The selection-index value is the index of the hash table in the selected subset of hash-list.  The hash-table value is the hash-list hash table that the function will change.  The function can change any field in the hash table, but should avoid adding or removing fields, because a hash-list works best when all the hash tables in the list have the same fields.  This function also allows you to pass a list for the set parameter instead of a function.  If you do that, then the list is a plist with key value pairs, where each key exists in every hash-list record and each corresponding value is the new value that this function will assign to that key in each of the selected hash tables."
  (loop for hash-table in (apply #'hash-list-filter (cons hash-list filters))
     for index = 0 then (1+ index)
     if (functionp set)
     do (funcall set index hash-table)
     else
     do (loop for (key value) on set by #'cddr
           do (setf (gethash key hash-table) value))
     summing 1))

(defun hash-list-keys (hash-list)
  "This function returns a list of the keys that are present in every element of the list."
  (loop with counts = (make-hash-table :test 'equal)
     for hash in hash-list
     do (loop for key being the hash-keys in hash
           do (incf (gethash key counts 0)))
     finally
       (return
         (loop with l = (length hash-list)
            for key being the hash-keys in counts using (hash-value val)
            when (= val l) collect key))))

(defun hash-list-stray-keys (hash-list)
  "This function returns a list of keys that appear in some but not all of the elements in the hash-list."
  (loop with target-count = (length hash-list)
     and counts = (make-hash-table :test 'equal)
     for hash in hash-list
     do (loop for k being the hash-keys in hash
           do (incf (gethash k counts 0)))
     finally
       (return
         (list :list-size target-count
               :stray-keys
               (loop for k being the hash-keys in counts
                  using (hash-value v)
                  when (not (= (gethash k counts) target-count))
                  collect (list :key k
                                :records (gethash k counts)))))))

(defun qsort (sequence predicate &key (key 'identity))
  "Non-destructive Quicksort.  The sequence, predicate, and key parameters are the same as in the Common Lisp sort function."
  (when sequence
    (let* ((pivot (first sequence))
           (rest (rest sequence))
           (lesser (remove-if-not
                    #'(lambda (x) (funcall predicate
                                           (funcall key x)
                                           (funcall key pivot)))
                    rest))
           (greater (remove-if-not
                     #'(lambda (x)
                         (not (funcall predicate
                                       (funcall key x)
                                       (funcall key pivot))))
                     rest)))
      (append (qsort lesser predicate :key key)
              (list pivot)
              (qsort greater predicate :key key)))))

(defun partition-by-lambdas (sequence lambdas &key exclusive)
  "Returns PARTITIONS, a list of lists, with each list corresponding to a partition associated with a function in LAMBDAS.  Each list is a partition of SEQUENCE.  When a function from LAMBDAS returns T for an item of sequence, that item goes into the partition that corresponds to the function.  The number of partitions is the number of LAMBDAS + 1.  The partitions are in the same order as the functions in LAMBDAS, with an additional partition for non-classified items at the end of PARTITIONS.  An item from SEQUENCE can land in more than one partition, unless EXCLUSIVE is T, in which case the item will go into a single partition, even if more than one function from LAMBDAS returns T for the item. The item will go into the partition of the first function that returns T for the item."
  (loop
    with partition-count = (1+ (length lambdas))
    with result = (make-list partition-count)
    with list = (if (listp sequence)
                    sequence
                    (map 'list 'identity sequence))
    for item in list
    do (loop with found = nil
             for lambda in lambdas
             for index from 0 below (1- partition-count)
             when (funcall lambda item) do
               (push item (elt result index))
               (setf found t)
             when (and exclusive found) do (return)
               finally (progn
                         (unless found 
                           (push item (elt result (1- partition-count))))
                         (return)))
    finally
       (return (mapcar (lambda (r) (nreverse r)) 
                       (if (null (car (last result)))
                           (butlast result)
                           result)))))
         

(defun partition-by-size (sequence cell-size)
  (let ((list (if (vectorp sequence)
                  (map 'list 'identity sequence)
                  sequence)))
    (loop for cell on list by #'(lambda (list) (nthcdr cell-size list))
       collecting (subseq cell 0 cell-size))))

(defun sort-keywords (keywords &key descending)
  (sort keywords
        (if descending #'string> #'string<)
        :key (lambda (x) (format nil "~a" x))))

(defun subtract-lists (list-1 list-2 &key (key (lambda (x) x)))
  (loop with h = (hashify-list list-1 :f-key key)
     for x in list-2
     for xkey = (funcall key x)
     when (gethash xkey h)
     do (decf (gethash xkey h))
     finally
       (return
         (loop for k being the hash-keys in h using (hash-value v)
            when (> v 0) collect k))))

(defun aws-load-settings (&key (filename (home-based ".aws/credentials")))
  (when (not (file-exists-p filename))
    (error "File not found: ~s" filename))
  (with-open-file (file filename)
    (loop
       with settings = (ds '(:map))
       and profile-key
       for line = (read-line file nil)
       for clean-line = (trim line)
       while clean-line
       do (if (scan "^\\[" clean-line)
              (setf profile-key
                    (multiple-value-bind (a b)
                        (scan-to-strings "\\[([^\\]]+)\\]" clean-line)
                      (declare (ignore a))
                      (string-to-keyword (elt b 0))))
              (when (not (zerop (length clean-line)))
                (let* ((key-value (split-n-trim clean-line
                                                :on-regex "\\s*=\\s*"))
                       (key (string-to-keyword (car key-value)))
                       (value (second key-value)))
                  (ds-set settings (list profile-key key) value))))
       finally (return (setf *dc-aws-settings* settings)))))

(defun aws-setting (key &optional (profile :default))
  (ds-get *dc-aws-settings* profile key))

(defun initialize-progress-report (name max-count)
  (setf (gethash name *dc-progress-hash*)
        (list :max-count max-count :start-time (get-universal-time))))

(defun report-progress (name count &key (stream t))
  (let* ((initial (gethash name *dc-progress-hash*))
         (max (getf initial :max-count))
         (start (getf initial :start-time))
         (percent-done (* (/ (float count) max) 100))
         (et (- (get-universal-time) start))
         (tt (* (/ et count) max))
         (rt (max (- tt et) 0))
         (eta (+ start tt)))
    (format stream "~a ~a/~a (~,2f%) et=~,2fh rt=~,2fh eta=~a~%"
            (dc-timestamp)
            count
            max
            percent-done
            (/ (float et) 3600)
            (/ (float rt) 3600)
            (dc-timestamp :time (truncate eta) :format "h:m:s"))))

(defun primep (n)
  (cond ((< n 2) nil)
				((< n 4) t)
				((or (zerop (mod n 2)) (zerop (mod n 3))) nil)
				(t (loop for a from 5 to (sqrt n) by 6
								never (or (zerop (mod n a)) (zerop (mod n (+ a 2))))))))

(defun next-prime (n)
	(if (< n 2)
			2
		(loop for a = (if (evenp n) (1+ n) (+ n 2)) then (+ a 2)
					when (primep a) return a)))

(defun prime-factors (x)
  (if (primep x)
      (list x)
    (loop for a from 2 to (/ x 2)
          when (zerop (mod x a))
          do (return (append (prime-factors a) (prime-factors (/ x a)))))))

(defun prime-game ()
  (let ((max (truncate 1e6)))
    (when (zerop (hash-table-count *hprimes*))
      (loop for prime in (numeric-range 1 max :filter #'primep)
         for index = 0 then (1+ index)
         do
           (setf (gethash prime *hprimes*) t)
           (setf (aref *aprimes* index) prime)))
    (loop
       for want-prime = (not (zerop (random 2)))
       for question = (if want-prime
                          (aref *aprimes* (random (length *aprimes*)))
                          (loop for x = (1+ (random max))
                             while (or (evenp x) (gethash x *hprimes*))
                             finally (return x)))
       for answer = (progn (format t "~d prime (y/n/q)? " question)
                           (read-line))
       for correct-answer = (if want-prime "y" "n")
       while (not (equal answer "q"))
       do (format t "~a. ~d is ~a.~%~%"
                  (if (equal answer correct-answer) "Correct" "Fail")
                  question
                  (if want-prime
                      "prime"
                      (format nil "not prime (~{~a~^ X ~})"
                              (prime-factors question)))))))

(defun all-permutations-base (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for item in list appending
                (mapcar (lambda (sublist) (cons item sublist))
                        (all-permutations-base (remove item list)))))))

(defun all-permutations (list)
  (let ((h (hashify-list list :method :index)))
    (distinct-values 
     (mapcar (lambda (list) (mapcar (lambda (x) (gethash x h)) list))
             (all-permutations-base (hash-keys h))))))

(defun all-permutations-of-string (s)
  (mapcar (lambda (list) (map 'string 'identity list)) 
          (all-permutations (map 'list 'identity s))))

(defun existing-permutations-of-string (s hash)
  (loop for word in (all-permutations-of-string s)
     when (gethash word hash)
     collect word))
