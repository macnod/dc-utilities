# DC-UTILITIES
## alist-values (ALIST &REST KEYS)
Returns the values associated with KEYS in ALIST.  ALIST is an associative list.

## bytes-to-uint (BYTE-LIST)
Converts the list of bytes BYTE-LIST into an unsigned integer.

## change-per-second (FUNCTION-OR-SYMBOL &OPTIONAL (SECONDS 1))
Given the function FUNCTION-OR-SYMBOL, who's return value changes over time, or a variable who's value changes over time, with the change being unidirectional, this function computes the rate of change by calling the function, sleeping, then calling the function again, then computing the rate of change per second.  You can optionally specify the number of seconds to wait between calls.  If FUNCTION-OR-SYMBOL is a variable, then this function retrieves the value of the variable, sleeps, then retrieves the value of the variable again.

## create-directory (DIR &KEY WITH-PARENTS)
Works just like the mkdir shell command.  DIR is the directory you want to create. Use WITH-PARENTS if you want the function to create parent directories as necessary.

## cull-named-params (NAMED-PARAMS CULL-KEYS)
Given a value for NAMED-PARAMS like this one

    '(:one 1 :two 2 :three 3)

and a list of CULL-KEYS like this one

    '(:one :two)

this function returns a list of named parameters that excludes the names (and their values) that match the names in CULL-KEYS.  In the above example, the result is

    '(:three 3)

## directory-exists (PATH)
Returns a boolean value indicating if the directory specified by PATH exists.

## distinct-elements (LIST)
Accepts a list of elements LIST and returns a new list with distinct elements from the first list.  (Copies the original list, removes duplicate elements from the copy, and returns the copy.)

## document-package (PACKAGE OUTPUT-FILENAME)
Documents the Common Lisp package PACKAGE and writes that documentation to the file given by OUTPUT-FILENAME.

## ds (LIST-OR-ATOM &OPTIONAL TYPE)
Create a dc-utilities nested data structure.  Each node in LIST-OR-ATOM can be a scalar value or object, a map (hash table), an array, or a list.  Here's an example:

    (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225)
                 (:map :name "Tracy" :age 45 :height "5'0'" :weight 120)))

When you create a dc-utilities data structure like the one above, you can use other data-structure functions to easily access and manipulate the data.

## ds-clone (DC-UTILITIES:DS)
Clone the dc-utilities data structure DS.

## ds-from-json (JSON)
Creates a dc-utilities data structure from JSON.  This is useful if you want to easily traverse the JSON data structure.

## ds-get (DC-UTILITIES:DS &REST KEYS)
Get a node (a leaf or a subtree) of DS, a dc-utilities data structure.  The parameters that follow ds, collected in KEYS, describe the path to the node.  For example, given the following data structure in bogus-ds:

    (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225)
                 (:map :name "Tracy" :age 45 :height "5'0'" :weight 120)))

You can get Tracy's weight like this:

    (ds-get bogus-ds 1 :weight)

or like this:

    (ds-get (elt (remove-if-not (lambda (x) (string= (ds-get x :name) "Tracy"))
                                bogus-ds)
                 0)
            :weight)

## ds-keys (DC-UTILITIES:DS &OPTIONAL PARENT-KEYS)
Given a dc-utilities data structure DS, this function returns the path to every leaf.  If you provide a key or list of keys in PARENT-KEYS, those keys are prepended to the path to every leaf.

## ds-list (DC-UTILITIES:DS)
Render the dc-utilities data structure DS in a human-readable way

## ds-merge (DS-BASE &REST DC-UTILITIES:DS-SET)
Merges dc-utilities data structures, starting with DS-BASE and then progressing through the rest of the data structures, collected in ds-set, in order.  Values in later data structures override values in earlier data structures when the paths of the values coincide.

## ds-set (DC-UTILITIES:DS LOCATION-KEY-PATH VALUE)
In the given dc-utilities data structure DS, this function sets the value of the node at LOCATION-KEY-PATH, which is a key or a list of keys, to VALUE.

## ds-to-json (DC-UTILITIES:DS)
Converts the dc-utilities data structure DS into JSON.

## ds-type (DC-UTILITIES:DS)
Given a dc-utilities data structure DS, this function returns the type of the data structure.  Valid return values include 'string, 'sequence, 'hash-table, and some Common Lisp types.

## factorial (N)
Computes the factorial for N.

## fast-compress (V)
Uses a simple compression mechanism to very quickly compress the vector in V into a list.

## fast-decompress (L)
Quickly decompresses the list or vector L, created by fast-compress, into the original vector.

## fib (X)
Compute the sum of the first X numbers in the Fibonnacci series.

## file-exists (PATH)
Returns a boolean value indicating if the file specified by PATH exists.

## file-extension (PATH)
Returns a string consisting of the file extension for the file name given in PATH.

## file-line-count (FILENAME)
Obtain a count of the lines in the file FILENAME using the Linux wc program.

## flatten (L)
Given a nested list L, return a flat list.

## freeze (OBJECT)
Serializes OBJECT into a string, returning the string.

## freeze-n-spew (OBJECT FILENAME)
Serializes OBJECT into a string and writes the string to the file specified by FILENAME.

## hash-keys (HASH)
Returns a list of all the keys in HASH, which is a hash table.

## hash-string (STRING)
Hash STRING and return a hex representation of the hash

## hash-values (HASH)
Returns a list of all the values in HASH, which is a hash table.

## home-based (PATH)
Prepends the user's home directory to PATH.

## index-values (L)
Accepts a list of values L and puts the values in a hash table, keying each value with the index of the value in the list.

## interrupt-sleep (NAME)
Interrupts an active timer set with another thread using the interruptible-sleep function.  The NAME parameter specifies the name of the timer to interrupt.

## interruptible-sleep (SECS NAME)
Sets up a named timer and sleeps for SECS seconds or until another thread calls the interrupt-sleep function with NAME.  This function checks once per second to see if the timer has been reached or interrupted.

## join-paths (&REST PATH-PARTS)
Joins elements of PATH-PARTS into a file path, inserting slashes where necessary.

## k-combination (K N)
Computes the k-combination value for K and N.

## load-settings (&REST FILEPATHS)
Accepts one or more file paths, collected in FILEPATHS, and reads settings from the given files, with settings in later files overriding the same settings in earlier files.  Each settings file is a Lisp file with a dc-utilities data structure.

## lof (FILENAME)
Returns the length of the file FILENAME.

## log-entry (&REST MESSAGES)
Accepts one or more strings, concatenates them, precedes the result with a timestamp, and returns a string that looks like a log entry.

## mark-time (TAG)
Marks the current time and stores it with the name given in TAG.  You can later read this time by passing TAG to the read-time function.

## memoize (FUNCTION-SYMBOL)
Incorporate caching into a function, specified by the symbol FUNCTION-SYMBOL, so that when the function is called with the same parameter a second time, it can retrieve the result from the cache instead of having to compute the result again.

## memoize-with-limit (FUNCTION-SYMBOL LIMIT)
Like memoize, but limits the size of the cache.  If more elements than LIMIT are cached when a new element needs to be cached, the oldest element is evicted from the cache to make room for the new one.  This is an excellent memoizing function to use when the function frequently returns a limited set of values, but has an infinite range.

## parse-number (STRING)
Converts STRING, which contains a number, into the number.

## path-only (FILENAME)
Retrieves the path (path only, without the filename) of FILENAME.

## range (START END &KEY (STEP 1) (FILTER #'IDENTITY) DC-UTILITIES:SHUFFLE)
Returns a list of values between START and END (inclusive), skipping values by STEP, filtering remaining values with the function in FILTER, and shuffling the remaining values if SHUFFLE is true.  STEP defaults to 1, FILTER defaults to allowing all values through, and SHUFFLE default to nil.

## read-one-line (STREAM &KEY (EOL :UNIX) (MAX-LENGTH 500))
Reads a single line from STREAM and returns the line as a string.  You can specify the end-of-line character with EOL, which defaults to :unix.  The other option is :dos.  If no end-of-line character is found before the line reaches a length of MAX-LENGTH, a line of length MAX-LENGTH is returned.

## read-settings-file (&REST FILEPATHS)
Accepts one or more parameters, collected in FILEPATHS, that are the names of settings files.  Reads the settings files in the order provided, with settings in later files overriding settings in earlier files.  A settings file is a Lisp file with a dc-utilities data structure (see the function ds).  This function returns a settings data structure.  Normally, you wouldn't use this function.  Instead, use the load-settings function at the beginning of your program (or when it needs to reload settings) and then use the setting function to retrieve values.

## read-time (TAG)
Reads the time that was marked with the name given in TAG. See the mark-time function.

## replace-extension (FILENAME NEW-EXTENSION)
This function replaces the file extension in FILENAME with the file extension provided in NEW-EXTENSION.

## replace-regexs (TEXT LIST-OF-REGEX-REPLACEMENT-PAIRS &KEY IGNORE-CASE)
Searches through TEXT for substrings that match the regexs in LIST-OF-REGEX-REPLACEMENTS and replaces those substrings with the corresponding replacements in LIST-OF-REGEX-REPLACEMENTS.  Use the IGNORE-CASE parameter if you want case-insensitive matches.  Here's an example:

    (replace-regexs
         "She was beautiful.  She was smart.  She was sexy"
         '(("sh[aeiou]" "Tracy")
           ("wa[a-z]" "is"))
         :ignore-case t)

==> "Tracy is beautiful.  Tracy is smart.  Tracy is sexy"

## scrape-string (REGEX STRING &KEY IGNORE-CASE)
Returns a list of the substrings in STRING that match REGEX.  Use the IGNORE-CASE parameter if you want case-insensitive matches.

## sequence-bytes-to-uint (SEQUENCE &OPTIONAL (SIZE 4))
Converts SEQUENCE, a sequence of bytes, into a sequence of unsigned integers of byte-size SIZE.

## sequence-uint-to-bytes (SEQUENCE &OPTIONAL (SIZE 4))
Converts SEQUENCE, as sequence of unsigned integers into a list of bytes.  The SIZE parameter specifies the byte-size of the integers in SEQUENCE.

## setting (&REST KEYS)
Accepts one or more parameters, collected in KEYS, which are used to traverse the settings data structure to locate the desired value.

## shell-execute (PROGRAM &OPTIONAL (PARAMETERS NIL) (INPUT-PIPE-DATA ""))
Run PROGRAM and return the output of the program as a string.  You can pass an atom or a list for PARAMETERS (the command-line options for the program). You can also pipe data to the program by passing the INPUT-PIPE-DATA parameter with a string containing the data you want to pipe.

## shift (LIST)
This function is like the Common Lisp pop function, but takes an element from the end of LIST instead of from the front of LIST.

## shuffle (SEQ)
Return a sequence with the same elements as the given sequence S, but in random order (shuffled).

## slurp (FILENAME)
Reads the entire file FILENAME into a string and returns the string.

## slurp-binary (FILENAME)
Reads the entire binary file given by FILENAME and returns an array with the bytes of the file.

## slurp-n-thaw (FILENAME)
Reads and brings to life serialized objects from the file FILENAME.

## spew (STRING FILENAME &KEY CREATE-DIRECTORIES APPEND)
Writes the contents of STRING to the file specified by FILENAME.  Use the CREATE-DIRECTORIES parameter if any of the directories in the path in FILENAME don't exist and you want to create them.  Use the APPEND parameter if you want to append STRING to an existing file.

## split-n-trim (STRING &KEY (ON-REGEX "\\s+") (FAT "^\\s+|\\s+$"))
Splits STRING into substrings on SPLITTER-REGEX, then trims whitespace from the beginning and end of each substring.  The SPLITTER-REGEX parameter value, which is optional, defaults to \s+, which is to say that the string is split into a list of words at the whitespace boundaries.  Here's an example:

    (split-n-trim "Hello  beautiful      world!")

    => '("Hello" "beautiful" "world!")

## store-delete (ROOT KEY)
Deletes the file specified by ROOT and KEY.  See the store-save and store-path functions for information about how ROOT and KEY are treated.

## store-fetch (ROOT KEY)
Reads the content of the file specified by ROOT and KEY, and deserializes that content into an object.  See the store-save and and store-path functions for information about how ROOT and KEY are treated.

## store-path (ROOT FILENAME)
Computes a path from ROOT (a root folder) and FILENAME, a regular file.  This is useful for when you plan to write many more files than can be held in a single directory.  This function will help you create a tree of directories for the files that you want to store.  FILENAME must have at least 15 characters, and the last 15 characters of FILENAME must be alphanumeric characters.

## store-save (ROOT KEY OBJECT)
Freezes (serializes into a string) the object in OBJECT, then stores the string in the file specified by ROOT and KEY.  This function calls the store-path function to create the path where the object is going to be stored.  Therefore, KEY must have at least 15 characters and the last 15 characters of KEY must be alphanumeric characters.

## thaw (STRING)
Deserializes an object (or data structure) from the string expression in STRING, returning the object.

## thread-pool-progress (POOL-NAME)
Retrieves the count of the records that the thread pool given by POOL-NAME has already processed.

## thread-pool-run-time (POOL-NAME)
Computes the number of seconds that the thread-pool named in POOL-NAME has been running.

## thread-pool-start (POOL-NAME THREAD-COUNT JOB-QUEUE FN-JOB &OPTIONAL FN-FINALLY)
Starts THREAD-COUNT threads using POOL-NAME to name the threads and runs FN-JOB with those threads.  Each thread runs FN-JOB, which takes no parameters, in a loop.  When all the threads are done, this function checks FN-FINALLY.  If the caller provides FN-FINALLY, then this function returns with the result of calling FN-FINALLY.  If the caller doesn't provide FN-FINALLY, then the this function exits with a sum of the return values of all the threads that ran.

## thread-pool-start-time (POOL-NAME)
Retrieves the start-time for thread-pool named in POOL-NAME.

## thread-pool-stop (POOL-NAME)
Stops all the threads in the thread-pool POOL-NAME.

## thread-pool-stop-time (POOL-NAME)
Retrieves the stop-time for the thread-pool named in POOL-NAME.

## thread-pool-time-to-go (POOL-NAME TOTAL-RECORD-COUNT)
Returns the amount of time left for the thread pool given by POOL-NAME to complete processing all the records, the total number of which is given in TOTAL-RECORD-COUNT.

## time-to-go (DC-UTILITIES:CHANGE-PER-SECOND RECORD-COUNT)
Given the number of records per second that are being processed (given in CHANGE-PER-SECOND) and the nuber of records remaining (given in RECORD-COUNT), this function computes the amount of time still left before all the records have been processed.

## timestamp (&KEY (TIME (GET-UNIVERSAL-TIME)) STRING (FORMAT "Y-M-DTh:m:s"))
Returns the given time (or the current time) formatted according to the FORMAT parameter, followed by an optional value for STRING.  If STRING is provided, the function adds a space to the result and then appends the string to that.  The FORMAT string can contain any characters.  This function will replace the format characters Y, M, D, h, m, and s, respectively, with numbers representing the year,month, day, hour, minute, and second.  All the numbers are 2 digits long, except for the year, which is 4 digits long.

## to-ascii (S)
Converts the string S, which may contain non-ascii characters, into a string with nothing but ascii characters.  Non ascii characters are converted into spaces.  If S is a list, this function converts each element of the list into an all-ascii string.

## trim (S &OPTIONAL (FAT "^\\s+|\\s+$"))
Trim FAT from the string in S.  The FAT parameter is optional and defaults to "^\s+|\s+$", which means "Whitespace at the beginning or end of the string".

## uint-to-bytes (I &OPTIONAL (SIZE 4))
Converts the unsigned integer I into a list of bytes.  The SIZE parameter specifies the byte-size of the integer in I.

## unique-name NIL
Returns a fairly unique short string

## verify-string (STRING REGEX &KEY IGNORE-CASE)
Return t if STRING matches the REGEX exactly.  Use the IGNORE-CASE parameter if you want case-insensitve matches.