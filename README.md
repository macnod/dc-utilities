## alist-values (alist &rest keys)
Returns the values in an alist.

## bytes-to-uint (byte-list)
Converts a list of bytes into an unsigned integer.

## change-per-second (function-or-symbol &optional (seconds 1))
Given a function who's return value changes over time, or a variable who's value changes over time, with the change being unidirectional, this function computes the rate of change by calling the function, sleeping, then calling the function again, then computing the rate of change per second.  You can optionally specify the number of seconds to wait between calls.  If function-or-symbol is a variable, then this function retrieves the value of the variable, sleeps, then retrieves the value of the variable again.

## create-directory (dir &key with-parents)
Works just like the mkdir shell command.  Use with-parents if you want the function to create parents as necessary.

## cull-named-params (named-params cull-keys)
Given a list of named parameters like this one

    '(:one 1 :two 2 :three 3)

and a list of cull-keys like this one

    '(:one :two)

this function returns a list of named parameters that excludes the names (and their values) that match the names in cull-keys.

## directory-exists (path)
Returns a boolean value indicating if the file specified by path exists.

## distinct-elements (list)
Accepts a list of elements and returns a new list with distinct elements from the first list.  (Copies the original list, removes duplicate elements from the copy, and returns the copy.)

## ds (list-or-atom &optional type)
Create a dc-utilities nested data structure.  Each node in the data structure can be a scalar value or object, a map (hash table), an array, or a list.  Here's an example:

    (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225)
                 (:map :name "Tracy" :age 45 :height "5'0'" :weight 120)))

When you create a dc-utilities data structure like the one above, you can use other data-structure functions to easily access and manipulate the data.

## ds-clone (ds)
Clone a dc-utilities data structure.

## ds-from-json (json)
Creates a dc-utilities data structure from JSON.  This is useful if you want to easily traverse the JSON data structure.

## ds-get (ds &rest keys)
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

## ds-keys (ds &optional parent-keys)
Given a dc-utilities data structure, this function returns the path to every leaf.  If you provide a key or list of keys in parent-keys, those keys are prepended to the path to every leaf.

## ds-list (ds)
Render a dc-utilities data structure in a human-readable way

## ds-merge (ds-base &rest ds-set)
Merges dc-utilities data structures, starting with base and then progressing through the rest of the data structures in order.  Values in later data structures override values in earlier data structures when the paths of the values coincide.

## ds-set (ds location-key-path value)
In the given dc-utilities data structure, this function sets the value of the node at location-key-path, which is a key or a list of keys, to value.

## ds-to-json (ds)
Converts a dc-utilities data structure into JSON.

## ds-type (ds)
Given a dc-utilities data structure, this function returns the type of the data structure.  Valid return values include string, sequence, hash-table, and some Common Lisp types.

## fast-compress (v)
Uses a simple compression mechanism to very quickly compress a vector into a list.

## fast-decompress (l)
Quickly decompresses a list or vector created by fast-compress into the original vector.

## fib (x)
Compute the sum of the first X numbers in the Fibonnacci series.

## file-exists (path)
Returns a boolean value indicating if the file specified by path exists.

## file-extension (path)
Returns a string consisting of the file extension for the given file name.

## file-line-count (filename)
Obtain a count of the lines in the given file using the wc program.

## flatten (l)
Given a nested list, return a flat list.

## freeze (object)
Serializes an object (or data structure) into a string, returning the string.

## freeze-n-spew (object filename)
Serializes an object into a string and writes the string to the file specified by filename.

## hash-keys (hash)
Returns a list of all the keys in the given hash table.

## hash-string (string)
Hash a string and return a hex representation of the hash

## hash-values (hash)
Returns a list of all the values in the given hash table.

## home-based (path)
Prepends the user's home directory to the given path.

## index-values (l)
Accepts a list of values and puts the values in a hash table, keying each value with the index of the value in the list.

## interrupt-sleep (name)
Interrupts an active time set with another thread using the interruptible-sleep function.  The name parameter specifies the name of the timer to interrupt.

## interruptible-sleep (secs name)
Sets up a named timer and sleeps for secs seconds or until another thread calls the interrupt-sleep function with the given name.  This function checks once per second to see if the timer has been reached or interrupted.

## join-paths (&rest path-parts)
Joins elements of path-parts into a file path, inserting slashes where necessary.

## load-settings (&rest filepaths)
Accepts one or more file paths and reads settings from the given files, with settings in later files overriding the same settings in earlier files.  Each settings file is a Lisp file with a dc-utilities data structure.

## lof (filename)
Returns the length of the given file.

## log-entry (&rest messages)
Accepts one or more strings, concatenates them, precedes the result with a timestamp, and returns a string that looks like a log entry.

## mark-time (tag)
Marks the current time and stores it with the given tag.  You can later read this time by passing the tag to the read-time function.

## memoize (function-symbol)
Incorporate caching into a function, so that when the function is called with the same parameter a second time, it can retrieve the result from the cache instead of having to compute the result again.

## memoize-with-limit (f limit)
Like memoize, but limits the size of the cache.  If more elements than limit are cached when a new element needs to be cached, the oldest element is evicted from the cache to make room for the new one.

## parse-number (string)
Converts a string that contains a number into the number.

## path-only (filename)
Retrieves the path (path only, without the filename) of the given filename.

## range (start end &key (step 1) (filter #'identity) shuffle)
Returns a list of values between start and end (inclusive), skipping values by step, filtering remaining values with the function in filter, and shuffling the remaining values if shuffle is true.  Step defaults to 1, filter defaults to allowing all values through, and shuffle default to nil.

## read-one-line (stream &key (eol unix) (max-length 500))
Reads a single line from a stream and returns the line as a string.

## read-settings-file (&rest filepaths)
Accepts one or more parameters that are the names of settings files.  Reads the settings files in the order provided, with settings in later files overriding settings in earlier files.  A settings file is a Lisp file with a dc-utilities data structure (see the function ds).  This function returns a settings data structure.  Normally, you wouldn't use this function.  Instead, use the load-settings function at the beginning of your program (or when it needs to reload settings) and then use the setting function to retrieve values.

## read-time (tag)
Reads the time that was marked with the given tag. See the mark-time function.

## replace-regexs (text list-of-regex-replacement-pairs &key ignore-case)
Searches through text for substrings that match the regexs in list-of-regex-replacements and replaces those substrings with the corresponding replacements in list-of-regex-replacements.  Use the ignore-case parameter if you want case-insensitive matches.  Here's an example:

    (replace-regexs
         "She was beautiful.  She was smart.  She was sexy"
         '(("sh[aeiou]" "Tracy")
           ("wa[a-z]" "is"))
         :ignore-case t)

==> "Tracy is beautiful.  Tracy is smart.  Tracy is sexy"

## scrape-string (regex string &key ignore-case)
Returns a list of the substrings in string that match regex.  Use the ignore-case parameter if you want case-insensitive matches.

## sequence-bytes-to-uint (sequence &optional (size 4))
Converts a sequence of bytes into a sequence of unsigned integers of byte-size size.

## sequence-uint-to-bytes (sequence &optional (size 4))
Converts a sequence of unsigned integers into a list of bytes.  The size parameter specifies the byte-size of the integers in the list.

## setting (&rest keys)
Accepts one or more parameters which are used to traverse the settings data structure to locate the desired value.

## shell-execute (program &optional (parameters nil) (input-pipe-data ))
Run shell program and return the output of the program as a string.  You can pass an atom or a list for parameters (the command-line options for the program). You can also pipe data to the program by passing the input-pipe-data parameter with a string containing the data you want to pipe.

## shift (list)
This function is like the Common Lisp pop function, but takes an element from the end of the list instead of from the front of the list.

## shuffle (seq)
Return a sequence with the same elements as the given sequence, but in random order (shuffled).

## slurp (filename)
Reads a whole file and returns the data of the file as a string.

## slurp-binary (filename)
Reads a whole binary file and returns an array with the bytes.

## slurp-n-thaw (filename)
Reads and brings to life serialized objects from a file.

## spew (string filename &key create-directories append)
Writes the contents of string to the file specified by filename.

## split-n-trim (string &optional (splitter-regex s+))
Splits a string into substrings on splitter-regex, then trims whitespace from the beginning and end of each substring.  The splitter-regex parameter value, which is optional, defaults to \s+, which is to say that the string is split into a list of words at the whitespace boundaries.  Here's an example:

    (split-n-trim "Hello  beautiful      world!")

    => '("Hello" "beautiful" "world!")

## store-delete (root key)
Deletes the file specified by root and key.  See the store-save and store-path functions for information about how root and key are treated.

## store-fetch (root key)
Reads the content of the file specified by root and key, and deserializes that content into an object.  See the store-save and and store-path functions for information about how root and key are treated.

## store-path (root filename)
Computes a path from a root folder and a filename.  This is useful for when you plan to write many more files than can be held in a single directory.  This function will help you create a tree of directories for the files that you want to store.  The filename must have at least 15 characters, and the last 15 characters of the filename must be alphanumeric characters.

## store-save (root key object)
Freezes (serializes into a string) the given object, then stores the string in the file specified by root and key.  This function calls the store-path function to create the path where the object is going to be stored.  Therefore, key must have at least 15 characters and the last 15 characters of key must be alphanumeric characters.

## thaw (string)
Deserializes an object (or data structure) from its string representation, returning the object.

## thread-pool-progress (pool-name)
Retrieves the count of the records that the given thread pool has already processed.

## thread-pool-run-time (pool-name)
Computes the number of seconds that the given thread-pool has been running.

## thread-pool-start (pool-name thread-count job-queue fn-job &optional fn-finally)
Starts thread-count threads using pool-name to name the threads and runs fn-job with those threads.  Each thread runs fn-job, which takes no parameters, in a loop.  When all the threads are done, this function checks fn-finally.  If the caller provides fn-finally, then this function returns with the result of calling fn-finally.  If the caller doesn't provide fn-finally, then the this function exists with a sum of the return values of all the threads that ran.

## thread-pool-start-time (pool-name)
Retrieves the start-time for the given thread-pool.

## thread-pool-stop (pool-name)
Stops all the threads in the named thread-pool.

## thread-pool-stop-time (pool-name)
Retrieves the stop-time for the given thread-pool.

## thread-pool-time-to-go (pool-name total-record-count)
Returns the amount of time left for the given pool to complete processing all the records.

## time-to-go (change-per-second record-count)
Given the number of records per second that are being processed (given in change-per-second) and the nuber of records remaining (given in record-count), this function computes the amount of time still left before all the records have been processed.

## timestamp (&key (time (get-universal-time)) string (format y-m-dth:m:s))
Returns the give time (or the current time) formatted according to the format parameter, followed by an optional string.  If a string is provided, the function adds a space to the result and then appends the string to that.  The format string can contain any characters.  This function will replace the format characters Y, M,D, h, m, and s, respectively, with numbers representing the year,month, day, hour, minute, and second.  All the numbers are 2 digits long, except for the year, which is 4 digits long.

## to-ascii (s)
Converts the string s, which may contain non-ascii characters, into a string with nothing but ascii characters.  Non ascii characters are converted into spaces.  If s is a list, this function converts each element of the list into an all-ascii string.

## trim (s &optional (fat ^\s+|\s+$))
Trim fat from the string.  The fat parameter is optional and defaults to "^\s+|\s+$", which means "Whitespace at the beginning or end of the string".

## uint-to-bytes (i &optional (size 4))
Converts the unsigned integer in i into a list of bytes.  The size parameter specifies the byte-size of the integer in i.

## verify-string (string regex &key ignore-case)
Return t if the string matches the regex exactly.  Use the ignore-case parameter if you want case-insensitve matches.