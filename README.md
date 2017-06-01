# DC-UTILITIES
## License
MIT License

Copyright (c) 2017 Donnie Cameron

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## API
### function  alist-values 
(

&nbsp;&nbsp;&nbsp;&nbsp;**ALIST**

&nbsp;&nbsp;&nbsp;&nbsp;**&REST**

&nbsp;&nbsp;&nbsp;&nbsp;**KEYS**


)

Returns the values associated with KEYS in ALIST. ALIST is an associative list.

### function  bytes-to-uint 
(**BYTE-LIST**)

Converts the list of bytes BYTE-LIST into an unsigned integer.

### function  change-per-second 
(

&nbsp;&nbsp;&nbsp;&nbsp;**FUNCTION-OR-SYMBOL**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**(SECONDS 1)**


)

Given the function FUNCTION-OR-SYMBOL, who's return value changes over time, or a variable who's value changes over time, with the change being unidirectional, this function computes the rate of change by calling the function, sleeping SECONDS seconds, calling the function again, then computing the rate of change per second. You can optionally specify the number of seconds to wait between calls with the SECONDS parameter, which defaults to 1. If FUNCTION-OR-SYMBOL is a variable, then this function retrieves the value of the variable, sleeps, then retrieves the value of the variable again.

### function  create-directory 
(

&nbsp;&nbsp;&nbsp;&nbsp;**DIR**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**WITH-PARENTS**


)

Works just like the mkdir shell command. DIR is the directory you want to create. Use WITH-PARENTS if you want the function to create parent directories as necessary.

### function  cull-named-params 
(**NAMED-PARAMS** **CULL-KEYS**)

Given a value for NAMED-PARAMS like this one '(:one 1 :two 2 :three 3) and a list of CULL-KEYS like this one '(:one :two) this function returns a list of named parameters that excludes the names (and their values) that match the names in CULL-KEYS. In the above example, the result is '(:three 3)

### function  directory-exists 
(**PATH**)

Returns a boolean value indicating if the directory specified by PATH exists.

### function  distinct-elements 
(**LIST**)

Accepts a list of elements LIST and returns a new list with distinct elements from the first list. (Copies the original list, removes duplicate elements from the copy, and returns the copy.)

### function  document-package 
(

&nbsp;&nbsp;&nbsp;&nbsp;**PACKAGE**

&nbsp;&nbsp;&nbsp;&nbsp;**OUTPUT-FILENAME**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**OVERVIEW-FILE**

&nbsp;&nbsp;&nbsp;&nbsp;**LICENSE-FILE**


)

Documents the Common Lisp package PACKAGE and writes that documentation to the file given by OUTPUT-FILENAME. If you provide file name for overview-file or license-file, document-package includes the contents of those files in the documentation it creates.

### function  ds 
(

&nbsp;&nbsp;&nbsp;&nbsp;**LIST-OR-ATOM**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**TYPE**


)

Create a dc-utilities nested data structure. Each node in LIST-OR-ATOM can be a scalar value or object, a map (hash table), an array, or a list. Here's an example: (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225) (:map :name "Tracy" :age 45 :height "5'0'" :weight 120))) When you create a dc-utilities data structure like the one above, you can use other data-structure functions to easily access and manipulate the data.

### function  ds-clone 
(**DS**)

Clone the dc-utilities data structure DS.

### function  ds-from-json 
(**JSON**)

Creates a dc-utilities data structure from JSON. This is useful if you want to easily traverse the JSON data structure.

### function  ds-get 
(

&nbsp;&nbsp;&nbsp;&nbsp;**DS**

&nbsp;&nbsp;&nbsp;&nbsp;**&REST**

&nbsp;&nbsp;&nbsp;&nbsp;**KEYS**


)

Get a node (a leaf or a subtree) of DS, a dc-utilities data structure. The parameters that follow ds, collected in KEYS, describe the path to the node. For example, given the following data structure in bogus-ds: (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225) (:map :name "Tracy" :age 45 :height "5'0'" :weight 120))) You can get Tracy's weight like this: (ds-get bogus-ds 1 :weight) or like this: (ds-get (elt (remove-if-not (lambda (x) (string= (ds-get x :name) "Tracy")) bogus-ds) 0) :weight)

### function  ds-keys 
(

&nbsp;&nbsp;&nbsp;&nbsp;**DS**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**PARENT-KEYS**


)

Given a dc-utilities data structure DS, this function returns the path to every leaf. If you provide a key or list of keys in PARENT-KEYS, those keys are prepended to the path to every leaf.

### function  ds-list 
(**DS**)

Render the dc-utilities data structure DS in a human-readable way

### function  ds-merge 
(

&nbsp;&nbsp;&nbsp;&nbsp;**DS-BASE**

&nbsp;&nbsp;&nbsp;&nbsp;**&REST**

&nbsp;&nbsp;&nbsp;&nbsp;**DS-SET**


)

Merges dc-utilities data structures, starting with DS-BASE and then progressing through the rest of the data structures, collected in ds-set, in order. Values in later data structures override values in earlier data structures when the paths of the values coincide.

### function  ds-set 
(

&nbsp;&nbsp;&nbsp;&nbsp;**DS**

&nbsp;&nbsp;&nbsp;&nbsp;**LOCATION-KEY-PATH**

&nbsp;&nbsp;&nbsp;&nbsp;**VALUE**


)

In the given dc-utilities data structure DS, this function sets the value of the node at LOCATION-KEY-PATH, which is a key or a list of keys, to VALUE.

### function  ds-to-json 
(**DS**)

Converts the dc-utilities data structure DS into JSON.

### function  ds-type 
(**DS**)

Given a dc-utilities data structure DS, this function returns the type of the data structure. Valid return values include 'string, 'sequence, 'hash-table, and some Common Lisp types.

### function  elapsed-time 
(

&nbsp;&nbsp;&nbsp;&nbsp;**TIME-TRACKER**

&nbsp;&nbsp;&nbsp;&nbsp;**TAG**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**ANY-THREAD**


)

Computes time elapsed since calling mark-time with TAG. You must pass in a TIME-TRACKER object, which you can create with the make-time-tracker function. When you call elapsed-time from multiple threads, elapsed-time associates TAG with the calling thread. If two threads use the same TAG value to fetch elapsed time, the mark-time and elapsed time functions behave as if the TAG values were different. You can change this behavior by passing T for ANY-THREAD, which causes TAG to be global across threads. See the mark-time function.

### function  factorial 
(**N**)

Computes the factorial for N.

### function  fast-compress 
(**V**)

Uses a simple compression mechanism to very quickly compress the vector in V into a list.

### function  fast-decompress 
(**L**)

Quickly decompresses the list or vector L, created by fast-compress, into the original vector.

### function  fib 
(**X**)

Compute the sum of the first X numbers in the Fibonnacci series.

### function  file-exists 
(**PATH**)

Returns a boolean value indicating if the file specified by PATH exists.

### function  file-extension 
(**PATH**)

Returns a string consisting of the file extension for the file name given in PATH.

### function  file-line-count 
(**FILENAME**)

Obtain a count of the lines in the file FILENAME using the Linux wc program.

### macro  filter-file 
(**&REST** **ARGS**)

Copies lines from the file INPUT-FILENAME to the file OUTPUT-FILENAME, omitting lines for which BODY returns nil.

### function  flatten 
(**L**)

Given a nested list L, return a flat list.

### function  freeze 
(**OBJECT**)

Serializes OBJECT into a string, returning the string.

### function  freeze-n-spew 
(**OBJECT** **FILENAME**)

Serializes OBJECT into a string and writes the string to the file specified by FILENAME.

### function  hash-keys 
(**HASH**)

Returns a list of all the keys in HASH, which is a hash table.

### function  hash-string 
(**STRING**)

Hash STRING and return a hex representation of the hash

### function  hash-values 
(**HASH**)

Returns a list of all the values in HASH, which is a hash table.

### function  home-based 
(**PATH**)

Prepends the user's home directory to PATH.

### function  index-values 
(**L**)

Accepts a list of values L and puts the values in a hash table, keying each value with the index of the value in the list.

### function  interrupt-sleep 
(**NAME**)

Interrupts an active timer set with another thread using the interruptible-sleep function. The NAME parameter specifies the name of the timer to interrupt.

### function  interruptible-sleep 
(**SECS** **NAME**)

Sets up a named timer and sleeps for SECS seconds or until another thread calls the interrupt-sleep function with NAME. This function checks once per second to see if the timer has been reached or interrupted.

### function  join-paths 
(**&REST** **PATH-PARTS**)

Joins elements of PATH-PARTS into a file path, inserting slashes where necessary.

### function  k-combination 
(**K** **N**)

Computes the k-combination value for K and N.

### function  list-keys 
(**PLIST**)

Returns the keys (properties) of the property list PLIST

### function  list-values 
(**PLIST**)

Returns the values of the property list PLIST

### function  load-settings 
(**&REST** **FILEPATHS**)

Accepts one or more file paths, collected in FILEPATHS, and reads settings from the given files, with settings in later files overriding the same settings in earlier files. Each settings file is a Lisp file with a dc-utilities data structure.

### function  lof 
(**FILENAME**)

Returns the length of the file FILENAME.

### function  log-entry 
(**&REST** **MESSAGES**)

Accepts one or more strings, concatenates them, precedes the result with a timestamp, and returns a string that looks like a log entry.

### function  make-time-tracker 
()

Creates a time-tracker object that you can use later in calls to mark-time and elapsed-time.

### function  mark-time 
(

&nbsp;&nbsp;&nbsp;&nbsp;**TIME-TRACKER**

&nbsp;&nbsp;&nbsp;&nbsp;**TAG**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**ANY-THREAD**


)

Marks the current time with TAG, for the purpose of later retrieving elapsed time. You must pass in a TIME-TRACKER object, which you can create with the make-time-tracker function. When you call mark-time from multiple threads, mark-time makes TAG visible only to the calling thread. If two threads use the same TAG value to mark the time, the mark-time and elapsed time functions behave as if the TAG values were different. You can change this behavior by passing T for ANY-THREAD, which causes a TAG to be global across threads. See the elapsed-time function.

### function  memoize 
(**FUNCTION-SYMBOL**)

Incorporate caching into a function, specified by the symbol FUNCTION-SYMBOL, so that when the function is called with the same parameter a second time, it can retrieve the result from the cache instead of having to compute the result again.

### function  memoize-with-limit 
(**FUNCTION-SYMBOL** **LIMIT**)

Like memoize, but limits the size of the cache. If more elements than LIMIT are cached when a new element needs to be cached, the oldest element is evicted from the cache to make room for the new one. This is an excellent memoizing function to use when the function frequently returns a limited set of values, but has an infinite range.

### function  parse-number 
(**STRING**)

Converts STRING, which contains a number, into the number.

### function  path-only 
(**FILENAME**)

Retrieves the path (path only, without the filename) of FILENAME.

### function  range 
(

&nbsp;&nbsp;&nbsp;&nbsp;**START**

&nbsp;&nbsp;&nbsp;&nbsp;**END**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**(STEP 1)**

&nbsp;&nbsp;&nbsp;&nbsp;**(FILTER #'IDENTITY)**

&nbsp;&nbsp;&nbsp;&nbsp;**SHUFFLE**


)

Returns a list of values between START and END (inclusive), skipping values by STEP, filtering remaining values with the function in FILTER, and shuffling the remaining values if SHUFFLE is true. STEP defaults to 1, FILTER defaults to allowing all values through, and SHUFFLE default to nil.

### function  read-one-line 
(

&nbsp;&nbsp;&nbsp;&nbsp;**STREAM**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**(EOL UNIX)**

&nbsp;&nbsp;&nbsp;&nbsp;**(MAX-LENGTH 500)**


)

Reads a single line from STREAM and returns the line as a string. You can specify the end-of-line character with EOL, which defaults to :unix. The other option is :dos. If no end-of-line character is found before the line reaches a length of MAX-LENGTH, a line of length MAX-LENGTH is returned.

### function  read-settings-file 
(**&REST** **FILEPATHS**)

Accepts one or more parameters, collected in FILEPATHS, that are the names of settings files. Reads the settings files in the order provided, with settings in later files overriding settings in earlier files. A settings file is a Lisp file with a dc-utilities data structure (see the function ds). This function returns a settings data structure. Normally, you wouldn't use this function. Instead, use the load-settings function at the beginning of your program (or when it needs to reload settings) and then use the setting function to retrieve values.

### function  replace-extension 
(**FILENAME** **NEW-EXTENSION**)

This function replaces the file extension in FILENAME with the file extension provided in NEW-EXTENSION.

### function  replace-regexs 
(

&nbsp;&nbsp;&nbsp;&nbsp;**TEXT**

&nbsp;&nbsp;&nbsp;&nbsp;**LIST-OF-REGEX-REPLACEMENT-PAIRS**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**IGNORE-CASE**


)

Searches through TEXT for substrings that match the regexs in LIST-OF-REGEX-REPLACEMENTS and replaces those substrings with the corresponding replacements in LIST-OF-REGEX-REPLACEMENTS. Use the IGNORE-CASE parameter if you want case-insensitive matches. Here's an example: (replace-regexs "She was beautiful. She was smart. She was sexy" '(("sh[aeiou]" "Tracy") ("wa[a-z]" "is")) :ignore-case t) ==> "Tracy is beautiful. Tracy is smart. Tracy is sexy"

### function  scrape-string 
(

&nbsp;&nbsp;&nbsp;&nbsp;**REGEX**

&nbsp;&nbsp;&nbsp;&nbsp;**STRING**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**IGNORE-CASE**


)

Returns a list of the substrings in STRING that match REGEX. Use the IGNORE-CASE parameter if you want case-insensitive matches.

### function  sequence-bytes-to-uint 
(

&nbsp;&nbsp;&nbsp;&nbsp;**SEQUENCE**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**(SIZE 4)**


)

Converts SEQUENCE, a sequence of bytes, into a sequence of unsigned integers of byte-size SIZE, which defaults to 4.

### function  sequence-uint-to-bytes 
(

&nbsp;&nbsp;&nbsp;&nbsp;**SEQUENCE**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**(SIZE 4)**


)

Converts SEQUENCE, a sequence of unsigned integers, into a list of bytes. The SIZE parameter specifies the byte-size of the integers in SEQUENCE, and defaults to 4.

### function  setting 
(**&REST** **KEYS**)

Accepts one or more parameters, collected in KEYS, which are used to traverse the settings data structure to locate the desired value.

### function  shell-execute 
(

&nbsp;&nbsp;&nbsp;&nbsp;**PROGRAM**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**PARAMETERS**

&nbsp;&nbsp;&nbsp;&nbsp;**(INPUT-PIPE-DATA "")**


)

Run PROGRAM and return the output of the program as a string. You can pass an atom or a list for PARAMETERS (the command-line options for the program). You can also pipe data to the program by passing the INPUT-PIPE-DATA parameter with a string containing the data you want to pipe. The INPUT-PIPE-DATA parameter defaults to the empty string.

### function  shift 
(**LIST**)

This function is like the Common Lisp pop function, but takes an element from the end of LIST instead of from the front of LIST.

### function  shuffle 
(**SEQ**)

Return a sequence with the same elements as the given sequence S, but in random order (shuffled).

### function  slurp 
(**FILENAME**)

Reads the entire file FILENAME into a string and returns the string.

### function  slurp-binary 
(**FILENAME**)

Reads the entire binary file given by FILENAME and returns an array with the bytes of the file.

### function  slurp-n-thaw 
(**FILENAME**)

Reads and brings to life serialized objects from the file FILENAME.

### function  spew 
(

&nbsp;&nbsp;&nbsp;&nbsp;**STRING**

&nbsp;&nbsp;&nbsp;&nbsp;**FILENAME**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**CREATE-DIRECTORIES**

&nbsp;&nbsp;&nbsp;&nbsp;**APPEND**


)

Writes the contents of STRING to the file specified by FILENAME. Use the CREATE-DIRECTORIES parameter if any of the directories in the path in FILENAME don't exist and you want to create them. Use the APPEND parameter if you want to append STRING to an existing file.

### function  split-n-trim 
(

&nbsp;&nbsp;&nbsp;&nbsp;**STRING**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**(ON-REGEX "\\s+")**

&nbsp;&nbsp;&nbsp;&nbsp;**(FAT "^\\s+|\\s+$")**


)

Splits STRING into substrings on ON-REGEX, then trims FAT from each substring. The ON-REGEX parameter value, which is optional, defaults to "\s+", which is to say that the string is split into a list of words at the whitespace boundaries. The default value for FAT, which is also optional, "\s+|\s+$", causes this function to trim whitespace from the beggining and end of each substring. Here's an example: (split-n-trim "Hello beautiful world!") => '("Hello" "beautiful" "world!")

### function  store-delete 
(**ROOT** **KEY**)

Deletes the file specified by ROOT and KEY. See the store-save and store-path functions for information about how ROOT and KEY are treated.

### function  store-fetch 
(**ROOT** **KEY**)

Reads the content of the file specified by ROOT and KEY, and deserializes that content into an object. See the store-save and and store-path functions for information about how ROOT and KEY are treated.

### function  store-path 
(**ROOT** **FILENAME**)

Computes a path from ROOT (a root folder) and FILENAME, a regular file. This is useful for when you plan to write many more files than can be held in a single directory. This function will help you create a tree of directories for the files that you want to store. FILENAME must have at least 15 characters, and the last 15 characters of FILENAME must be alphanumeric characters.

### function  store-save 
(

&nbsp;&nbsp;&nbsp;&nbsp;**ROOT**

&nbsp;&nbsp;&nbsp;&nbsp;**KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**OBJECT**


)

Freezes (serializes into a string) the object in OBJECT, then stores the string in the file specified by ROOT and KEY. This function calls the store-path function to create the path where the object is going to be stored. Therefore, KEY must have at least 15 characters and the last 15 characters of KEY must be alphanumeric characters.

### function  thaw 
(**STRING**)

Deserializes an object (or data structure) from the string expression in STRING, returning the object.

### function  thread-pool-progress 
(**POOL-NAME**)

Retrieves the count of the records that the thread pool given by POOL-NAME has already processed.

### function  thread-pool-run-time 
(**POOL-NAME**)

Computes the number of seconds that the thread-pool named in POOL-NAME has been running.

### function  thread-pool-start 
(

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**(POOL-NAME (ERROR pool-name parameter is required))**

&nbsp;&nbsp;&nbsp;&nbsp;**(THREAD-COUNT (ERROR thread-count parameter is required))**

&nbsp;&nbsp;&nbsp;&nbsp;**(JOB-QUEUE (ERROR job-queue paramater is required))**

&nbsp;&nbsp;&nbsp;&nbsp;**(FN-JOB (ERROR fn-job parameter is required))**

&nbsp;&nbsp;&nbsp;&nbsp;**(STANDARD-OUTPUT *STANDARD-OUTPUT*)**

&nbsp;&nbsp;&nbsp;&nbsp;**FN-FINALLY**


)

Starts THREAD-COUNT threads using POOL-NAME (a keyword symbol) to name the threads and runs FN-JOB with those threads. Each thread runs FN-JOB, which takes no parameters, in a loop. When all the threads are done, this function checks FN-FINALLY. If the caller provides FN-FINALLY, then this function returns with the result of calling FN-FINALLY. If the caller doesn't provide FN-FINALLY, then the this function exits with a sum of the return values of all the threads that ran.

### function  thread-pool-start-time 
(**POOL-NAME**)

Retrieves the start-time for thread-pool named in POOL-NAME.

### function  thread-pool-stop 
(**POOL-NAME**)

Stops all the threads in the thread-pool POOL-NAME.

### function  thread-pool-stop-time 
(**POOL-NAME**)

Retrieves the stop-time for the thread-pool named in POOL-NAME.

### function  thread-pool-time-to-go 
(**POOL-NAME** **TOTAL-RECORD-COUNT**)

Returns the amount of time left for the thread pool given by POOL-NAME to complete processing all the records, the total number of which is given in TOTAL-RECORD-COUNT.

### function  time-to-go 
(**CHANGE-PER-SECOND** **RECORD-COUNT**)

Given the number of records per second that are being processed (given in CHANGE-PER-SECOND) and the nuber of records remaining (given in RECORD-COUNT), this function computes the amount of time still left before all the records have been processed.

### function  timestamp 
(

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**(TIME (GET-UNIVERSAL-TIME))**

&nbsp;&nbsp;&nbsp;&nbsp;**STRING**

&nbsp;&nbsp;&nbsp;&nbsp;**(FORMAT "Y-M-DTh:m:s")**


)

Returns the given time (or the current time) formatted according to the FORMAT parameter, followed by an optional value for STRING. If STRING is provided, the function adds a space to the result and then appends the string to that. The FORMAT string can contain any characters. This function will replace the format characters Y, M, D, h, m, and s, respectively, with numbers representing the year,month, day, hour, minute, and second. All the numbers are 2 digits long, except for the year, which is 4 digits long.

### function  to-ascii 
(**S**)

Converts the string S, which may contain non-ascii characters, into a string with nothing but ascii characters. Non ascii characters are converted into spaces. If S is a list, this function converts each element of the list into an all-ascii string.

### function  trim 
(

&nbsp;&nbsp;&nbsp;&nbsp;**S**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**(FAT "^\\s+|\\s+$")**


)

Trim FAT from the string in S. The FAT parameter is optional and defaults to "^\s+|\s+$", which means "Whitespace at the beginning or end of the string".

### function  uint-to-bytes 
(

&nbsp;&nbsp;&nbsp;&nbsp;**I**

&nbsp;&nbsp;&nbsp;&nbsp;**&OPTIONAL**

&nbsp;&nbsp;&nbsp;&nbsp;**(SIZE 4)**


)

Converts the unsigned integer I into a list of bytes. The SIZE parameter specifies the byte-size of the integer in I and defaults to 4.

### function  unique-name 
()

Returns a fairly unique short string

### function  verify-string 
(

&nbsp;&nbsp;&nbsp;&nbsp;**STRING**

&nbsp;&nbsp;&nbsp;&nbsp;**REGEX**

&nbsp;&nbsp;&nbsp;&nbsp;**&KEY**

&nbsp;&nbsp;&nbsp;&nbsp;**IGNORE-CASE**


)

Return t if STRING matches the REGEX exactly. Use the IGNORE-CASE parameter if you want case-insensitve matches.

### macro  with-lines-in-file 
(**&REST** **ARGS**)

Lambda list is `((LINE FILENAME) &BODY BODY)`. Sequentially assigns each line in the file given by FILENAME to LINE and runs BODY for each line.

