## alist-values (dc-utilities::alist &rest dc-utilities::keys)
returns the values in an alist.

## bytes-to-uint (dc-utilities::byte-list)
converts a list of bytes into an unsigned integer.

## change-per-second (dc-utilities::function-or-symbol &optional (dc-utilities::seconds 1))
given a function who's return value changes over time, or a variable who's value changes over time, with the change being unidirectional, this function computes the rate of change by calling the function, sleeping, then calling the function again, then computing the rate of change per second.  you can optionally specify the number of seconds to wait between calls.  if function-or-symbol is a variable, then this function retrieves the value of the variable, sleeps, then retrieves the value of the variable again.

## create-directory (dc-utilities::dir &key dc-utilities::with-parents)
works just like the mkdir shell command. use with-parents if you want the function to create parents as necessary.

## cull-named-params (dc-utilities::named-params dc-utilities::cull-keys)
given a list of named parameters like this one '(:one 1 :two 2 :three 3)

and a list of cull-keys like this one

    '(:one :two)

this function returns a list of named parameters that excludes the names (and their values) that match the names in cull-keys.

## directory-exists (dc-utilities::path)
returns a boolean value indicating if the file specified by path exists.

## distinct-elements (list)
accepts a list of elements and returns a new list with distinct elements from the first list. (copies the original list, removes duplicate elements from the copy, and returns the copy.)

## ds (dc-utilities::list-or-atom &optional type)
create a dc-utilities nested data structure. each node in the data structure can be a scalar value or object, a map (hash table), an array, or a list.  here's an example:

    (ds '(:array (:map :name "donnie" :age 50 :height "6'4" :weight 225)
                 (:map :name "tracy" :age 45 :height "5'0'" :weight 120)))

when you create a dc-utilities data structure like the one above, you can use other data-structure functions to easily access and manipulate the data.

## ds-clone (ds)
clone a dc-utilities data structure.

## ds-from-json (dc-utilities::json)
creates a dc-utilities data structure from json. this is useful if you want to easily traverse the json data structure.

## ds-get (ds &rest dc-utilities::keys)
get a node (a leaf or a subtree) of ds, a dc-utilities data structure. the parameters that follow ds describe the path to the node.  for example, given the following data structure in bogus-ds:

    (ds '(:array (:map :name "donnie" :age 50 :height "6'4" :weight 225)
                 (:map :name "tracy" :age 45 :height "5'0'" :weight 120)))

you can get tracy's weight like this:

    (ds-get bogus-ds 1 :weight)

or like this:

    (ds-get (elt (remove-if-not (lambda (x) (string= (ds-get x :name) "tracy"))
                                bogus-ds)
                 0)
            :weight)

## ds-keys (ds &optional dc-utilities::parent-keys)
given a dc-utilities data structure, this function returns the path to every leaf. if you provide a key or list of keys in parent-keys, those keys are prepended to the path to every leaf.

## ds-list (ds)
render a dc-utilities data structure in a human-readable way

## ds-merge (dc-utilities::ds-base &rest ds-set)
merges dc-utilities data structures, starting with base and then progressing through the rest of the data structures in order. values in later data structures override values in earlier data structures when the paths of the values coincide.

## ds-set (ds dc-utilities::location-key-path dc-utilities::value)
in the given dc-utilities data structure, this function sets the value of the node at location-key-path, which is a key or a list of keys, to value.

## ds-to-json (ds)
converts a dc-utilities data structure into json.

## ds-type (ds)
given a dc-utilities data structure, this function returns the type of the data structure. valid return values include string, sequence, hash-table, and some common lisp types.

## fast-compress (dc-utilities::v)
uses a simple compression mechanism to very quickly compress a vector into a list.

## fast-decompress (dc-utilities::l)
quickly decompresses a list or vector created by fast-compress into the original vector.

## fib (dc-utilities::x)
compute the sum of the first x numbers in the fibonnacci series.

## file-exists (dc-utilities::path)
returns a boolean value indicating if the file specified by path exists.

## file-extension (dc-utilities::path)
returns a string consisting of the file extension for the given file name.

## file-line-count (dc-utilities::filename)
obtain a count of the lines in the given file using the wc program.

## flatten (dc-utilities::l)
given a nested list, return a flat list.

## freeze (dc-utilities::object)
serializes an object (or data structure) into a string, returning the string.

## freeze-n-spew (dc-utilities::object dc-utilities::filename)
serializes an object into a string and writes the string to the file specified by filename.

## hash-keys (dc-utilities::hash)
returns a list of all the keys in the given hash table.

## hash-string (string)
hash a string and return a hex representation of the hash

## hash-values (dc-utilities::hash)
returns a list of all the values in the given hash table.

## home-based (dc-utilities::path)
prepends the user's home directory to the given path.

## index-values (dc-utilities::l)
accepts a list of values and puts the values in a hash table, keying each value with the index of the value in the list.

## interrupt-sleep (dc-utilities::name)
interrupts an active time set with another thread using the interruptible-sleep function. the name parameter specifies the name of the timer to interrupt.

## interruptible-sleep (dc-utilities::secs dc-utilities::name)
sets up a named timer and sleeps for secs seconds or until another thread calls the interrupt-sleep function with the given name. this function checks once per second to see if the timer has been reached or interrupted.

## join-paths (&rest dc-utilities::path-parts)
joins elements of path-parts into a file path, inserting slashes where necessary.

## load-settings (&rest dc-utilities::filepaths)
accepts one or more file paths and reads settings from the given files, with settings in later files overriding the same settings in earlier files. each settings file is a lisp file with a dc-utilities data structure.

## lof (dc-utilities::filename)
returns the length of the given file.

## log-entry (&rest dc-utilities::messages)
accepts one or more strings, concatenates them, precedes the result with a timestamp, and returns a string that looks like a log entry.

## mark-time (dc-utilities::tag)
marks the current time and stores it with the given tag. you can later read this time by passing the tag to the read-time function.

## memoize (dc-utilities::function-symbol)
incorporate caching into a function, so that when the function is called with the same parameter a second time, it can retrieve the result from the cache instead of having to compute the result again.

## memoize-with-limit (dc-utilities::f dc-utilities::limit)
like memoize, but limits the size of the cache. if more elements than limit are cached when a new element needs to be cached, the oldest element is evicted from the cache to make room for the new one.

## parse-number (string)
converts a string that contains a number into the number.

## path-only (dc-utilities::filename)
retrieves the path (path only, without the filename) of the given filename.

## range (dc-utilities::start dc-utilities::end &key (step 1) (dc-utilities::filter #'identity) shuffle)
returns a list of values between start and end (inclusive), skipping values by step, filtering remaining values with the function in filter, and shuffling the remaining values if shuffle is true.  step defaults to 1, filter defaults to allowing all values through, and shuffle default to nil.

## read-one-line (stream &key (dc-utilities::eol :unix) (dc-utilities::max-length 500))
reads a single line from a stream and returns the line as a string.

## read-settings-file (&rest dc-utilities::filepaths)
accepts one or more parameters that are the names of settings files. reads the settings files in the order provided, with settings in later files overriding settings in earlier files.  a settings file is a lisp file with a dc-utilities data structure (see the function ds).  this function returns a settings data structure.  normally, you wouldn't use this function.  instead, use the load-settings function at the beginning of your program (or when it needs to reload settings) and then use the setting function to retrieve values.

## read-time (dc-utilities::tag)
reads the time that was marked with the given tag. see the mark-time function.

## replace-regexs (dc-utilities::text dc-utilities::list-of-regex-replacement-pairs &key
                   dc-utilities::ignore-case)
searches through text for substrings that match the regexs in list-of-regex-replacements and replaces those substrings with the corresponding replacements in list-of-regex-replacements.  use the ignore-case parameter if you want case-insensitive matches.  here's an example:

    (replace-regexs
         "she was beautiful.  she was smart.  she was sexy"
         '(("sh[aeiou]" "tracy")
           ("wa[a-z]" "is"))
         :ignore-case t)

==> "tracy is beautiful.  tracy is smart.  tracy is sexy"

## scrape-string (dc-utilities::regex string &key dc-utilities::ignore-case)
returns a list of the substrings in string that match regex. use the ignore-case parameter if you want case-insensitive matches.

## sequence-bytes-to-uint (sequence &optional (dc-utilities::size 4))
converts a sequence of bytes into a sequence of unsigned integers of byte-size size.

## sequence-uint-to-bytes (sequence &optional (dc-utilities::size 4))
converts a sequence of unsigned integers into a list of bytes. the size parameter specifies the byte-size of the integers in the list.

## setting (&rest dc-utilities::keys)
accepts one or more parameters which are used to traverse the settings data structure to locate the desired value.

## shell-execute (dc-utilities::program &optional (dc-utilities::parameters nil)
                  (dc-utilities::input-pipe-data ""))
run shell program and return the output of the program as a string.  you can pass an atom or a list for parameters (the command-line options for the program). you can also pipe data to the program by passing the input-pipe-data parameter with a string containing the data you want to pipe.

## shift (list)
this function is like the common lisp pop function, but takes an element from the end of the list instead of from the front of the list.

## shuffle (dc-utilities::seq)
return a sequence with the same elements as the given sequence, but in random order (shuffled).

## slurp (dc-utilities::filename)
reads a whole file and returns the data of the file as a string.

## slurp-binary (dc-utilities::filename)
reads a whole binary file and returns an array with the bytes.

## slurp-n-thaw (dc-utilities::filename)
reads and brings to life serialized objects from a file.

## spew (string dc-utilities::filename &key dc-utilities::create-directories append)
writes the contents of string to the file specified by filename.

## split-n-trim (string &optional (dc-utilities::splitter-regex "s+"))
splits a string into substrings on splitter-regex, then trims whitespace from the beginning and end of each substring. the splitter-regex parameter value, which is optional, defaults to \s+, which is to say that the string is split into a list of words at the whitespace boundaries.  here's an example:

    (split-n-trim "hello  beautiful      world!")

    => '("hello" "beautiful" "world!")

## store-delete (dc-utilities::root dc-utilities::key)
deletes the file specified by root and key. see the store-save and store-path functions for information about how root and key are treated.

## store-fetch (dc-utilities::root dc-utilities::key)
reads the content of the file specified by root and key, and deserializes that content into an object. see the store-save and and store-path functions for information about how root and key are treated.

## store-path (dc-utilities::root dc-utilities::filename)
computes a path from a root folder and a filename. this is useful for when you plan to write many more files than can be held in a single directory.  this function will help you create a tree of directories for the files that you want to store.  the filename must have at least 15 characters, and the last 15 characters of the filename must be alphanumeric characters.

## store-save (dc-utilities::root dc-utilities::key dc-utilities::object)
freezes (serializes into a string) the given object, then stores the string in the file specified by root and key. this function calls the store-path function to create the path where the object is going to be stored.  therefore, key must have at least 15 characters and the last 15 characters of key must be alphanumeric characters.

## thaw (string)
deserializes an object (or data structure) from its string representation, returning the object.

## thread-pool-progress (dc-utilities::pool-name)
retrieves the count of the records that the given thread pool has already processed.

## thread-pool-run-time (dc-utilities::pool-name)
computes the number of seconds that the given thread-pool has been running.

## thread-pool-start (dc-utilities::pool-name dc-utilities::thread-count dc-utilities::job-queue dc-utilities::fn-job &optional
                      dc-utilities::fn-finally)
starts thread-count threads using pool-name to name the threads and runs fn-job with those threads.  each thread runs fn-job, which takes no parameters, in a loop.  when all the threads are done, this function checks fn-finally.  if the caller provides fn-finally, then this function returns with the result of calling fn-finally.  if the caller doesn't provide fn-finally, then the this function exists with a sum of the return values of all the threads that ran.

## thread-pool-start-time (dc-utilities::pool-name)
retrieves the start-time for the given thread-pool.

## thread-pool-stop (dc-utilities::pool-name)
stops all the threads in the named thread-pool.

## thread-pool-stop-time (dc-utilities::pool-name)
retrieves the stop-time for the given thread-pool.

## thread-pool-time-to-go (dc-utilities::pool-name dc-utilities::total-record-count)
returns the amount of time left for the given pool to complete processing all the records.

## time-to-go (change-per-second dc-utilities::record-count)
given the number of records per second that are being processed (given in change-per-second) and the nuber of records remaining (given in record-count), this function computes the amount of time still left before all the records have been processed.

## timestamp (&key (time (get-universal-time)) string (format "y-m-dth:m:s"))
returns the give time (or the current time) formatted according to the format parameter, followed by an optional string. if a string is provided, the function adds a space to the result and then appends the string to that.  the format string can contain any characters.  this function will replace the format characters y, m,d, h, m, and s, respectively, with numbers representing the year,month, day, hour, minute, and second.  all the numbers are 2 digits long, except for the year, which is 4 digits long.

## to-ascii (dc-utilities::s)
converts the string s, which may contain non-ascii characters, into a string with nothing but ascii characters. non ascii characters are converted into spaces.  if s is a list, this function converts each element of the list into an all-ascii string.

## trim (dc-utilities::s &optional (dc-utilities::fat "^\\s+|\\s+$"))
trim fat from the string. the fat parameter is optional and defaults to "^\s+|\s+$", which means "whitespace at the beginning or end of the string".

## uint-to-bytes (dc-utilities::i &optional (dc-utilities::size 4))
converts the unsigned integer in i into a list of bytes. the size parameter specifies the byte-size of the integer in i.

## verify-string (string dc-utilities::regex &key dc-utilities::ignore-case)
return t if the string matches the regex exactly. use the ignore-case parameter if you want case-insensitve matches.