## alist-values (ALIST &REST KEYS)
Returns the values in an alist.

## bytes-to-uint (BYTE-LIST)
Converts a list of bytes into an unsigned integer.

## change-per-second (FUNCTION-OR-SYMBOL &OPTIONAL (SECONDS 1))
Given a function who's return value changes over time, or a variable who's value changes over time, with the change being unidirectional, this function computes the rate of change by calling the function, sleeping, then calling the function again, then computing the rate of change per second.  You can optionally specify the number of seconds to wait between calls.  If function-or-symbol is a variable, then this function retrieves the value of the variable, sleeps, then retrieves the value of the variable again.

## create-directory (DIR &KEY WITH-PARENTS)
Works just like the mkdir shell command.  Use with-parents if you want the function to create parents as necessary.

## cull-named-params (NAMED-PARAMS CULL-KEYS)
Given a list of named parameters like this one

    '(:one 1 :two 2 :three 3)

and a list of cull-keys like this one

    '(:one :two)

this function returns a list of named parameters that excludes the names (and their values) that match the names in cull-keys.

## directory-exists (PATH)
Returns a boolean value indicating if the file specified by path exists.

## distinct-elements (LIST)
Accepts a list of elements and returns a new list with distinct elements from the first list.  (Copies the original list, removes duplicate elements from the copy, and returns the copy.)

## ds (LIST-OR-ATOM &OPTIONAL TYPE)
Create a dc-utilities nested data structure.  Each node in the data structure can be a scalar value or object, a map (hash table), an array, or a list.  Here's an example:

    (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225)
                 (:map :name "Tracy" :age 45 :height "5'0'" :weight 120)))

When you create a dc-utilities data structure like the one above, you can use other data-structure functions to easily access and manipulate the data.

## ds-clone (DS)
Clone a dc-utilities data structure.

## ds-from-json (JSON)
Creates a dc-utilities data structure from JSON.  This is useful if you want to easily traverse the JSON data structure.

## ds-get (DS &REST KEYS)
Get a node (a leaf or a subtree) of ds, a dc-utilities data structure.  The parameters that follow ds describe the path to the node.  For example, given the following data structure in bogus-ds:

    (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225)
                 (:map :name "Tracy" :age 45 :height "5'0'" :weight 120)))

You can get Tracy's weight like this:

    (ds-get bogus-ds 1 :weight)

or like this:

    (ds-get (elt (remove-if-not (lambda (x) (string= (ds-get x :name) "Tracy"))
                                bogus-ds)
                 0)
            :weight)

## ds-keys (DS &OPTIONAL PARENT-KEYS)
Given a dc-utilities data structure, this function returns the path to every leaf.  If you provide a key or list of keys in parent-keys, those keys are prepended to the path to every leaf.

## ds-list (DS)
Render a dc-utilities data structure in a human-readable way

## ds-merge (DS-BASE &REST DS-SET)
Merges dc-utilities data structures, starting with base and then progressing through the rest of the data structures in order.  Values in later data structures override values in earlier data structures when the paths of the values coincide.

## ds-set (DS LOCATION-KEY-PATH VALUE)
In the given dc-utilities data structure, this function sets the value of the node at location-key-path, which is a key or a list of keys, to value.

## ds-to-json (DS)
Converts a dc-utilities data structure into JSON.

## ds-type (DS)
Given a dc-utilities data structure, this function returns the type of the data structure.  Valid return values include string, sequence, hash-table, and some Common Lisp types.

## fast-compress (V)
Uses a simple compression mechanism to very quickly compress a vector into a list.

## fast-decompress (L)
Quickly decompresses a list or vector created by fast-compress into the original vector.

## fib (X)
Compute the sum of the first X numbers in the Fibonnacci series.

## file-exists (PATH)
Returns a boolean value indicating if the file specified by path exists.

## file-extension (PATH)
Returns a string consisting of the file extension for the given file name.

## file-line-count (FILENAME)
Obtain a count of the lines in the given file using the wc program.

## flatten (L)
Given a nested list, return a flat list.

## freeze (OBJECT)
Serializes an object (or data structure) into a string, returning the string.

## freeze-n-spew (OBJECT FILENAME)
Serializes an object into a string and writes the string to the file specified by filename.

## hash-keys (HASH)
Returns a list of all the keys in the given hash table.

## hash-string (STRING)
Hash a string and return a hex representation of the hash

## hash-values (HASH)
Returns a list of all the values in the given hash table.

## home-based (PATH)
Prepends the user's home directory to the given path.

## index-values (L)
Accepts a list of values and puts the values in a hash table, keying each value with the index of the value in the list.

## interrupt-sleep (NAME)
Interrupts an active time set with another thread using the interruptible-sleep function.  The name parameter specifies the name of the timer to interrupt.

## interruptible-sleep (SECS NAME)
Sets up a named timer and sleeps for secs seconds or until another thread calls the interrupt-sleep function with the given name.  This function checks once per second to see if the timer has been reached or interrupted.

## join-paths (&REST PATH-PARTS)
Joins elements of path-parts into a file path, inserting slashes where necessary.

## load-settings (&REST FILEPATHS)
Accepts one or more file paths and reads settings from the given files, with settings in later files overriding the same settings in earlier files.  Each settings file is a Lisp file with a dc-utilities data structure.

## lof (FILENAME)
Returns the length of the given file.

## log-entry (&REST MESSAGES)
Accepts one or more strings, concatenates them, precedes the result with a timestamp, and returns a string that looks like a log entry.

## mark-time (TAG)
Marks the current time and stores it with the given tag.  You can later read this time by passing the tag to the read-time function.

## memoize (FUNCTION-SYMBOL)
Incorporate caching into a function, so that when the function is called with the same parameter a second time, it can retrieve the result from the cache instead of having to compute the result again.

## memoize-with-limit (F LIMIT)
Like memoize, but limits the size of the cache.  If more elements than limit are cached when a new element needs to be cached, the oldest element is evicted from the cache to make room for the new one.

## parse-number (STRING)
Converts a string that contains a number into the number.

## path-only (FILENAME)
Retrieves the path (path only, without the filename) of the given filename.

## range (START END &KEY (STEP 1) (FILTER #'IDENTITY) SHUFFLE)
Returns a list of values between start and end (inclusive), skipping values by step, filtering remaining values with the function in filter, and shuffling the remaining values if shuffle is true.  Step defaults to 1, filter defaults to allowing all values through, and shuffle default to nil.

## read-one-line (STREAM &KEY (EOL UNIX) (MAX-LENGTH 500))
Reads a single line from a stream and returns the line as a string.

## read-settings-file (&REST FILEPATHS)
Accepts one or more parameters that are the names of settings files.  Reads the settings files in the order provided, with settings in later files overriding settings in earlier files.  A settings file is a Lisp file with a dc-utilities data structure (see the function ds).  This function returns a settings data structure.  Normally, you wouldn't use this function.  Instead, use the load-settings function at the beginning of your program (or when it needs to reload settings) and then use the setting function to retrieve values.

## read-time (TAG)
Reads the time that was marked with the given tag. See the mark-time function.

## replace-regexs (TEXT LIST-OF-REGEX-REPLACEMENT-PAIRS &KEY IGNORE-CASE)
Searches through text for substrings that match the regexs in list-of-regex-replacements and replaces those substrings with the corresponding replacements in list-of-regex-replacements.  Use the ignore-case parameter if you want case-insensitive matches.  Here's an example:

    (replace-regexs
         "She was beautiful.  She was smart.  She was sexy"
         '(("sh[aeiou]" "Tracy")
           ("wa[a-z]" "is"))
         :ignore-case t)

==> "Tracy is beautiful.  Tracy is smart.  Tracy is sexy"

## scrape-string (REGEX STRING &KEY IGNORE-CASE)
Returns a list of the substrings in string that match regex.  Use the ignore-case parameter if you want case-insensitive matches.

## sequence-bytes-to-uint (SEQUENCE &OPTIONAL (SIZE 4))
Converts a sequence of bytes into a sequence of unsigned integers of byte-size size.

## sequence-uint-to-bytes (SEQUENCE &OPTIONAL (SIZE 4))
Converts a sequence of unsigned integers into a list of bytes.  The size parameter specifies the byte-size of the integers in the list.

## setting (&REST KEYS)
Accepts one or more parameters which are used to traverse the settings data structure to locate the desired value.

## shell-execute (PROGRAM &OPTIONAL (PARAMETERS NIL) (INPUT-PIPE-DATA ))
Run shell program and return the output of the program as a string.  You can pass an atom or a list for parameters (the command-line options for the program). You can also pipe data to the program by passing the input-pipe-data parameter with a string containing the data you want to pipe.

## shift (LIST)
This function is like the Common Lisp pop function, but takes an element from the end of the list instead of from the front of the list.

## shuffle (SEQ)
Return a sequence with the same elements as the given sequence, but in random order (shuffled).

## slurp (FILENAME)
Reads a whole file and returns the data of the file as a string.

## slurp-binary (FILENAME)
Reads a whole binary file and returns an array with the bytes.

## slurp-n-thaw (FILENAME)
Reads and brings to life serialized objects from a file.

## spew (STRING FILENAME &KEY CREATE-DIRECTORIES APPEND)
Writes the contents of string to the file specified by filename.

## split-n-trim (STRING &OPTIONAL (SPLITTER-REGEX s+))
Splits a string into substrings on splitter-regex, then trims whitespace from the beginning and end of each substring.  The splitter-regex parameter value, which is optional, defaults to \s+, which is to say that the string is split into a list of words at the whitespace boundaries.  Here's an example:

    (split-n-trim "Hello  beautiful      world!")

    => '("Hello" "beautiful" "world!")

## store-delete (ROOT KEY)
Deletes the file specified by root and key.  See the store-save and store-path functions for information about how root and key are treated.

## store-fetch (ROOT KEY)
Reads the content of the file specified by root and key, and deserializes that content into an object.  See the store-save and and store-path functions for information about how root and key are treated.

## store-path (ROOT FILENAME)
Computes a path from a root folder and a filename.  This is useful for when you plan to write many more files than can be held in a single directory.  This function will help you create a tree of directories for the files that you want to store.  The filename must have at least 15 characters, and the last 15 characters of the filename must be alphanumeric characters.

## store-save (ROOT KEY OBJECT)
Freezes (serializes into a string) the given object, then stores the string in the file specified by root and key.  This function calls the store-path function to create the path where the object is going to be stored.  Therefore, key must have at least 15 characters and the last 15 characters of key must be alphanumeric characters.

## thaw (STRING)
Deserializes an object (or data structure) from its string representation, returning the object.

## thread-pool-progress (POOL-NAME)
Retrieves the count of the records that the given thread pool has already processed.

## thread-pool-run-time (POOL-NAME)
Computes the number of seconds that the given thread-pool has been running.

## thread-pool-start (POOL-NAME THREAD-COUNT JOB-QUEUE FN-JOB &OPTIONAL
                      FN-FINALLY)
Starts thread-count threads using pool-name to name the threads and runs fn-job with those threads.  Each thread runs fn-job, which takes no parameters, in a loop.  When all the threads are done, this function checks fn-finally.  If the caller provides fn-finally, then this function returns with the result of calling fn-finally.  If the caller doesn't provide fn-finally, then the this function exists with a sum of the return values of all the threads that ran.

## thread-pool-start-time (POOL-NAME)
Retrieves the start-time for the given thread-pool.

## thread-pool-stop (POOL-NAME)
Stops all the threads in the named thread-pool.

## thread-pool-stop-time (POOL-NAME)
Retrieves the stop-time for the given thread-pool.

## thread-pool-time-to-go (POOL-NAME TOTAL-RECORD-COUNT)
Returns the amount of time left for the given pool to complete processing all the records.

## time-to-go (CHANGE-PER-SECOND RECORD-COUNT)
Given the number of records per second that are being processed (given in change-per-second) and the nuber of records remaining (given in record-count), this function computes the amount of time still left before all the records have been processed.

## timestamp (&KEY (TIME (GET-UNIVERSAL-TIME)) STRING (FORMAT Y-M-DTh:m:s))
Returns the give time (or the current time) formatted according to the format parameter, followed by an optional string.  If a string is provided, the function adds a space to the result and then appends the string to that.  The format string can contain any characters.  This function will replace the format characters Y, M,D, h, m, and s, respectively, with numbers representing the year,month, day, hour, minute, and second.  All the numbers are 2 digits long, except for the year, which is 4 digits long.

## to-ascii (S)
Converts the string s, which may contain non-ascii characters, into a string with nothing but ascii characters.  Non ascii characters are converted into spaces.  If s is a list, this function converts each element of the list into an all-ascii string.

## trim (S &OPTIONAL (FAT ^\s+|\s+$))
Trim fat from the string.  The fat parameter is optional and defaults to "^\s+|\s+$", which means "Whitespace at the beginning or end of the string".

## uint-to-bytes (I &OPTIONAL (SIZE 4))
Converts the unsigned integer in i into a list of bytes.  The size parameter specifies the byte-size of the integer in i.

## verify-string (STRING REGEX &KEY IGNORE-CASE)
Return t if the string matches the regex exactly.  Use the ignore-case parameter if you want case-insensitve matches.