<header>

# dc-utilities

<span class="version">

Functions that I use in most of my programs.

</span></header>

<main>

<article id="documentation">

<div>

# dc-utilities

Some Common Lisp utilities I use all the time

</div>

</article>

<article id="copyright">

## Copyright

<span>dc-utilities</span> is licensed under the <span>[MIT License](https://tldrlegal.com/search?q=MIT License)</span> license. © <span>Donnie Cameron <macnod@gmail.com></span> .</article>

<article id="symbol-index">

## Package Index

*   ### [DC-UTILITIES](#DC-UTILITIES)<span class="nicknames"></span>

    *   <a name="DC-UTILITIES:ALIST-VALUES"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[ALIST-VALUES](#DC-UTILITIES:ALIST-VALUES)`

        `ALIST &REST KEYS``)`</header>

        <pre class="docstring">Returns the values in an alist.</pre>

        </article>

    *   <a name="DC-UTILITIES:BYTES-TO-UINT"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[BYTES-TO-UINT](#DC-UTILITIES:BYTES-TO-UINT)`

        `BYTE-LIST``)`</header>

        </article>

    *   <a name="DC-UTILITIES:CHANGE-PER-SECOND"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[CHANGE-PER-SECOND](#DC-UTILITIES:CHANGE-PER-SECOND)`

        `FUNCTION-OR-SYMBOL &OPTIONAL (SECONDS 1)``)`</header>

        <pre class="docstring">Given a function who's return value changes over time, or a variable who's value changes over time, with the change being unidirectional, this function computes the rate of change by calling the function, sleeping, then calling the function again, then computing the rate of change per second.  You can optionally specify the number of seconds to wait between calls.  If function-or-symbol is a variable, then this function retrieves the value of the variable, sleeps, then retrieves the value of the variable again.</pre>

        </article>

    *   <a name="DC-UTILITIES:COMMAND-LINE-OPTIONS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[COMMAND-LINE-OPTIONS](#DC-UTILITIES:COMMAND-LINE-OPTIONS)`

        `SHORT-LONG-KEYWORD-LIST``)`</header>

        </article>

    *   <a name="DC-UTILITIES:CREATE-DIRECTORY"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[CREATE-DIRECTORY](#DC-UTILITIES:CREATE-DIRECTORY)`

        `DIR &KEY WITH-PARENTS``)`</header>

        <pre class="docstring">Works just like the mkdir shell command.  Use with-parents if you want the function to create parents as necessary.</pre>

        </article>

    *   <a name="DC-UTILITIES:CULL-NAMED-PARAMS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[CULL-NAMED-PARAMS](#DC-UTILITIES:CULL-NAMED-PARAMS)`

        `NAMED-PARAMS CULL-KEYS``)`</header>

        <pre class="docstring">Given a list of named parameters like this one

        '(:one 1 :two 2 :three 3)

        and a list of cull-keys like this one

        '(:one :two)

        this function returns a list of named parameters that excludes
        the names (and their values) that match the names in cull-keys.</pre>

        </article>

    *   <a name="DC-UTILITIES:DIRECTORY-EXISTS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DIRECTORY-EXISTS](#DC-UTILITIES:DIRECTORY-EXISTS)`

        `PATH``)`</header>

        </article>

    *   <a name="DC-UTILITIES:DISTINCT-ELEMENTS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DISTINCT-ELEMENTS](#DC-UTILITIES:DISTINCT-ELEMENTS)`

        `LIST``)`</header>

        <pre class="docstring">Accepts a list of elements and returns a new list with distinct elements from the first list.  (Copies the original list, removes duplicate elements from the copy, and returns the copy.)</pre>

        </article>

    *   <a name="DC-UTILITIES:DS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS](#DC-UTILITIES:DS)`

        `LIST-OR-ATOM &OPTIONAL TYPE``)`</header>

        <pre class="docstring">Create a dc-utilities nested data structure.  Each node in the data structure can be a scalar value or object, a map (hash table), an array, or a list.  Here's an example:

        (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225)
                     (:map :name "Tracy" :age 45 :height "5'0'" :weight 120)))

        When you create a dc-utilities data structure like the one above, you can use other data-structure functions to easily access and manipulate the data.</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-CLONE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-CLONE](#DC-UTILITIES:DS-CLONE)`

        `DS``)`</header>

        <pre class="docstring">Clone a dc-utilities data structure.</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-FROM-JSON"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-FROM-JSON](#DC-UTILITIES:DS-FROM-JSON)`

        `JSON``)`</header>

        <pre class="docstring">Creates a dc-utilities data structure from JSON.  This is useful if you want to easily traverse the JSON data structure.</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-GET"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-GET](#DC-UTILITIES:DS-GET)`

        `DS &REST KEYS``)`</header>

        <pre class="docstring">Get a node (a leaf or a subtree) of ds, a dc-utilities data structure.  The parameters that follow ds describe the path to the node.  For example, given the following data structure in bogus-ds:

        (ds '(:array (:map :name "Donnie" :age 50 :height "6'4" :weight 225)
                     (:map :name "Tracy" :age 45 :height "5'0'" :weight 120)))

        You can get Tracy's weight like this:

        (ds-get bogus-ds 1 :weight)

        or like this:

        (ds-get (elt (remove-if-not (lambda (x) (string= (ds-get x :name) "Tracy"))
                                    bogus-ds)
                     0)
                :weight)</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-KEYS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-KEYS](#DC-UTILITIES:DS-KEYS)`

        `DS &OPTIONAL PARENT-KEYS``)`</header>

        <pre class="docstring">Given a dc-utilities data structure, this function returns the path to every leaf.  If you provide a key or list of keys in parent-keys, those keys are prepended to the path to every leaf.</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-LIST"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-LIST](#DC-UTILITIES:DS-LIST)`

        `DS``)`</header>

        <pre class="docstring">Render a dc-utilities data structure in a human-readable way</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-MERGE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-MERGE](#DC-UTILITIES:DS-MERGE)`

        `DS-BASE &REST DS-SET``)`</header>

        <pre class="docstring">Merges dc-utilities data structures, starting with base and then progressing through the rest of the data structures in order.  Values in later data structures override values in earlier data structures when the paths of the values coincide.</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-SET"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-SET](#DC-UTILITIES:DS-SET)`

        `DS LOCATION-KEY-PATH VALUE``)`</header>

        <pre class="docstring">In the given dc-utilities data structure, this function sets the value of the node at location-key-path, which is a key or a list of keys, to value.</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-TO-JSON"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-TO-JSON](#DC-UTILITIES:DS-TO-JSON)`

        `DS``)`</header>

        <pre class="docstring">Converts a dc-utilities data structure into JSON.</pre>

        </article>

    *   <a name="DC-UTILITIES:DS-TYPE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[DS-TYPE](#DC-UTILITIES:DS-TYPE)`

        `DS``)`</header>

        <pre class="docstring">Given a dc-utilities data structure, this function returns the type of the data structure.  Valid return values include string, sequence, hash-table, and some Common Lisp types.</pre>

        </article>

    *   <a name="DC-UTILITIES:FACTORIAL"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FACTORIAL](#DC-UTILITIES:FACTORIAL)`

        `N``)`</header>

        </article>

    *   <a name="DC-UTILITIES:FAST-COMPRESS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FAST-COMPRESS](#DC-UTILITIES:FAST-COMPRESS)`

        `V``)`</header>

        </article>

    *   <a name="DC-UTILITIES:FAST-DECOMPRESS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FAST-DECOMPRESS](#DC-UTILITIES:FAST-DECOMPRESS)`

        `L``)`</header>

        </article>

    *   <a name="DC-UTILITIES:FIB"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FIB](#DC-UTILITIES:FIB)`

        `X``)`</header>

        <pre class="docstring">Compute the sum of the first X numbers in the Fibonnacci series.</pre>

        </article>

    *   <a name="DC-UTILITIES:FILE-EXISTS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FILE-EXISTS](#DC-UTILITIES:FILE-EXISTS)`

        `PATH``)`</header>

        </article>

    *   <a name="DC-UTILITIES:FILE-EXTENSION"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FILE-EXTENSION](#DC-UTILITIES:FILE-EXTENSION)`

        `PATH``)`</header>

        </article>

    *   <a name="DC-UTILITIES:FILE-LINE-COUNT"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FILE-LINE-COUNT](#DC-UTILITIES:FILE-LINE-COUNT)`

        `FILENAME``)`</header>

        <pre class="docstring">Obtain a count of the lines in the given file using the wc program.</pre>

        </article>

    *   <a name="DC-UTILITIES:FLATTEN"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FLATTEN](#DC-UTILITIES:FLATTEN)`

        `L``)`</header>

        <pre class="docstring">Given a nested list, return a flat list.</pre>

        </article>

    *   <a name="DC-UTILITIES:FREEZE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FREEZE](#DC-UTILITIES:FREEZE)`

        `OBJECT``)`</header>

        <pre class="docstring">Serializes an object (or data structure) into a string, returning the string.</pre>

        </article>

    *   <a name="DC-UTILITIES:FREEZE-N-SPEW"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[FREEZE-N-SPEW](#DC-UTILITIES:FREEZE-N-SPEW)`

        `OBJECT FILENAME``)`</header>

        <pre class="docstring">Serializes an object into a string and writes the string to the file specified by filename.</pre>

        </article>

    *   <a name="DC-UTILITIES:HASH-KEYS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[HASH-KEYS](#DC-UTILITIES:HASH-KEYS)`

        `HASH``)`</header>

        <pre class="docstring">Returns a list of all the keys in the given hash table.</pre>

        </article>

    *   <a name="DC-UTILITIES:HASH-STRING"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[HASH-STRING](#DC-UTILITIES:HASH-STRING)`

        `STRING``)`</header>

        <pre class="docstring">Hash a string and return a hex representation of the hash</pre>

        </article>

    *   <a name="DC-UTILITIES:HASH-VALUES"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[HASH-VALUES](#DC-UTILITIES:HASH-VALUES)`

        `HASH``)`</header>

        <pre class="docstring">Returns a list of all the values in the given hash table.</pre>

        </article>

    *   <a name="DC-UTILITIES:HOME-BASED"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[HOME-BASED](#DC-UTILITIES:HOME-BASED)`

        `PATH``)`</header>

        </article>

    *   <a name="DC-UTILITIES:INDEX-VALUES"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[INDEX-VALUES](#DC-UTILITIES:INDEX-VALUES)`

        `L``)`</header>

        <pre class="docstring">Accepts a list of values and puts the values in a hash table, keying each value with the index of the value in the list.</pre>

        </article>

    *   <a name="DC-UTILITIES:INTERRUPT-SLEEP"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[INTERRUPT-SLEEP](#DC-UTILITIES:INTERRUPT-SLEEP)`

        `NAME``)`</header>

        <pre class="docstring">Interrupts an active time set with another thread using the interruptible-sleep function.  The name parameter specifies the name of the timer to interrupt.</pre>

        </article>

    *   <a name="DC-UTILITIES:INTERRUPTIBLE-SLEEP"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[INTERRUPTIBLE-SLEEP](#DC-UTILITIES:INTERRUPTIBLE-SLEEP)`

        `SECS NAME``)`</header>

        <pre class="docstring">Sets up a named timer and sleeps for secs seconds or until another thread calls the interrupt-sleep function with the given name.  This function checks once per second to see if the timer has been reached or interrupted.</pre>

        </article>

    *   <a name="DC-UTILITIES:JOIN-PATHS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[JOIN-PATHS](#DC-UTILITIES:JOIN-PATHS)`

        `&REST PATH-PARTS``)`</header>

        <pre class="docstring">Joins elements of path-parts into a file path, inserting slashes where necessary.</pre>

        </article>

    *   <a name="DC-UTILITIES:K-COMBINATION"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[K-COMBINATION](#DC-UTILITIES:K-COMBINATION)`

        `K N``)`</header>

        </article>

    *   <a name="DC-UTILITIES:LOAD-SETTINGS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[LOAD-SETTINGS](#DC-UTILITIES:LOAD-SETTINGS)`

        `&REST FILEPATHS``)`</header>

        <pre class="docstring">Accepts one or more file paths and reads settings from the given files, with settings in later files overriding the same settings in earlier files.  Each settings file is a Lisp file with a dc-utilities data structure.</pre>

        </article>

    *   <a name="DC-UTILITIES:LOF"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[LOF](#DC-UTILITIES:LOF)`

        `FILENAME``)`</header>

        <pre class="docstring">Returns the length of the given file.</pre>

        </article>

    *   <a name="DC-UTILITIES:LOG-ENTRY"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[LOG-ENTRY](#DC-UTILITIES:LOG-ENTRY)`

        `&REST MESSAGES``)`</header>

        <pre class="docstring">Accepts one or more strings, concatenates them, precedes the result with a timestamp, and returns a string that looks like a log entry.</pre>

        </article>

    *   <a name="DC-UTILITIES:MARK-TIME"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[MARK-TIME](#DC-UTILITIES:MARK-TIME)`

        `TAG``)`</header>

        </article>

    *   <a name="DC-UTILITIES:MEMOIZE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[MEMOIZE](#DC-UTILITIES:MEMOIZE)`

        `F``)`</header>

        <pre class="docstring">Incorporate caching into a function, so that when the function is called with the same parameter a second time, it can retrieve the result from the cache instead of having to compute the result again.</pre>

        </article>

    *   <a name="DC-UTILITIES:MEMOIZE-WITH-LIMIT"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[MEMOIZE-WITH-LIMIT](#DC-UTILITIES:MEMOIZE-WITH-LIMIT)`

        `F LIMIT``)`</header>

        <pre class="docstring">Like memoize, but limits the size of the cache.  If more elements than limit are cached when a new element needs to be cached, the oldest element is evicted from the cache to make room for the new one.</pre>

        </article>

    *   <a name="DC-UTILITIES:PARSE-NUMBER"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[PARSE-NUMBER](#DC-UTILITIES:PARSE-NUMBER)`

        `STRING``)`</header>

        </article>

    *   <a name="DC-UTILITIES:PATH-ONLY"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[PATH-ONLY](#DC-UTILITIES:PATH-ONLY)`

        `FILENAME``)`</header>

        <pre class="docstring">Retrieves the path (path only, without the filename) of the given filename.</pre>

        </article>

    *   <a name="DC-UTILITIES:RANGE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[RANGE](#DC-UTILITIES:RANGE)`

        `START END &KEY (STEP 1) (FILTER (FUNCTION IDENTITY)) SHUFFLE``)`</header>

        <pre class="docstring">Returns a list of values between start and end (inclusive), skipping values by step, filtering remaining values with the function in filter, and shuffling the remaining values if shuffle is true.  Step defaults to 1, filter defaults to allowing all values through, and shuffle default to nil.</pre>

        </article>

    *   <a name="DC-UTILITIES:READ-ONE-LINE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[READ-ONE-LINE](#DC-UTILITIES:READ-ONE-LINE)`

        `STREAM &KEY (EOL :UNIX) (MAX-LENGTH 500)``)`</header>

        <pre class="docstring">Reads a single line from a stream and returns the line as a string.</pre>

        </article>

    *   <a name="DC-UTILITIES:READ-SETTINGS-FILE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[READ-SETTINGS-FILE](#DC-UTILITIES:READ-SETTINGS-FILE)`

        `&REST FILEPATHS``)`</header>

        <pre class="docstring">Accepts one or more parameters that are the names of settings files.  Reads the settings files in the order provided, with settings in later files overriding settings in earlier files.  A settings file is a Lisp file with a dc-utilities data structure (see the function ds).  This function returns a settings data structure.  Normally, you wouldn't use this function.  Instead, use the load-settings function at the beginning of your program (or when it needs to reload settings) and then use the setting function to retrieve values.</pre>

        </article>

    *   <a name="DC-UTILITIES:READ-TIME"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[READ-TIME](#DC-UTILITIES:READ-TIME)`

        `TAG``)`</header>

        </article>

    *   <a name="DC-UTILITIES:REPLACE-REGEXS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[REPLACE-REGEXS](#DC-UTILITIES:REPLACE-REGEXS)`

        `TEXT LIST-OF-REGEX-REPLACEMENT-PAIRS &KEY IGNORE-CASE``)`</header>

        <pre class="docstring">Searches through text for substrings that match the regexs in list-of-regex-replacements and replaces those substrings with the corresponding replacements in list-of-regex-replacements.  Use the ignore-case parameter if you want case-insensitive matches.  Here's an example:

        (replace-regexs
             "She was beautiful.  She was smart.  She was sexy"
             '(("sh[aeiou]" "Tracy")
               ("wa[a-z]" "is"))
             :ignore-case t)

        ==> "Tracy is beautiful.  Tracy is smart.  Tracy is sexy"</pre>

        </article>

    *   <a name="DC-UTILITIES:SCRAPE-STRING"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SCRAPE-STRING](#DC-UTILITIES:SCRAPE-STRING)`

        `REGEX STRING &KEY IGNORE-CASE``)`</header>

        <pre class="docstring">Returns a list of the substrings in string that match regex.  Use the ignore-case parameter if you want case-insensitive matches.</pre>

        </article>

    *   <a name="DC-UTILITIES:SEQUENCE-BYTES-TO-UINT"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SEQUENCE-BYTES-TO-UINT](#DC-UTILITIES:SEQUENCE-BYTES-TO-UINT)`

        `SEQUENCE &OPTIONAL (SIZE 4)``)`</header>

        </article>

    *   <a name="DC-UTILITIES:SEQUENCE-UINT-TO-BYTES"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SEQUENCE-UINT-TO-BYTES](#DC-UTILITIES:SEQUENCE-UINT-TO-BYTES)`

        `SEQUENCE &OPTIONAL (SIZE 4)``)`</header>

        </article>

    *   <a name="DC-UTILITIES:SETTING"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SETTING](#DC-UTILITIES:SETTING)`

        `&REST KEYS``)`</header>

        <pre class="docstring">Accepts one or more parameters which are used to traverse the settings data structure to locate the desired value.</pre>

        </article>

    *   <a name="DC-UTILITIES:SHELL-EXECUTE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SHELL-EXECUTE](#DC-UTILITIES:SHELL-EXECUTE)`

        `PROGRAM &OPTIONAL (PARAMETERS NIL) (INPUT-PIPE-DATA "")``)`</header>

        <pre class="docstring">Run shell program and return the output of the program as a string.  You can pass an atom or a list for parameters (the command-line options for the program). You can also pipe data to the program by passing the input-pipe-data parameter with a string containing the data you want to pipe.</pre>

        </article>

    *   <a name="DC-UTILITIES:SHIFT"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SHIFT](#DC-UTILITIES:SHIFT)`

        `LIST``)`</header>

        <pre class="docstring">This function is like the Common Lisp pop function, but takes an element from the end of the list instead of from the front of the list.</pre>

        </article>

    *   <a name="DC-UTILITIES:SHUFFLE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SHUFFLE](#DC-UTILITIES:SHUFFLE)`

        `SEQ``)`</header>

        <pre class="docstring">Return a sequence with the same elements as the given sequence, but in random order (shuffled).</pre>

        </article>

    *   <a name="DC-UTILITIES:SLURP"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SLURP](#DC-UTILITIES:SLURP)`

        `FILENAME``)`</header>

        <pre class="docstring">Reads a whole file and returns the data of the file as a string.</pre>

        </article>

    *   <a name="DC-UTILITIES:SLURP-BINARY"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SLURP-BINARY](#DC-UTILITIES:SLURP-BINARY)`

        `FILENAME``)`</header>

        <pre class="docstring">Reads a whole binary file and returns an array with the bytes.</pre>

        </article>

    *   <a name="DC-UTILITIES:SLURP-N-THAW"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SLURP-N-THAW](#DC-UTILITIES:SLURP-N-THAW)`

        `FILENAME``)`</header>

        <pre class="docstring">Reads and brings to life serialized objects from a file.</pre>

        </article>

    *   <a name="DC-UTILITIES:SPEW"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SPEW](#DC-UTILITIES:SPEW)`

        `STRING FILENAME &KEY CREATE-DIRECTORIES APPEND``)`</header>

        <pre class="docstring">Writes the contents of string to the file specified by filename.</pre>

        </article>

    *   <a name="DC-UTILITIES:SPLIT-N-TRIM"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[SPLIT-N-TRIM](#DC-UTILITIES:SPLIT-N-TRIM)`

        `STRING &OPTIONAL (SPLITTER-REGEX "s+")``)`</header>

        <pre class="docstring">Splits a string into substrings on splitter-regex, then trims whitespace from the beginning and end of each substring.  The splitter-regex parameter value, which is optional, defaults to \s+, which is to say that the string is split into a list of words at the whitespace boundaries.  Here's an example:

        (split-n-trim "Hello  beautiful      world!")

        => '("Hello" "beautiful" "world!")</pre>

        </article>

    *   <a name="DC-UTILITIES:STORE-DELETE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[STORE-DELETE](#DC-UTILITIES:STORE-DELETE)`

        `ROOT KEY``)`</header>

        </article>

    *   <a name="DC-UTILITIES:STORE-FETCH"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[STORE-FETCH](#DC-UTILITIES:STORE-FETCH)`

        `ROOT KEY``)`</header>

        </article>

    *   <a name="DC-UTILITIES:STORE-PATH"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[STORE-PATH](#DC-UTILITIES:STORE-PATH)`

        `ROOT FILENAME``)`</header>

        <pre class="docstring">Computes a path from a root folder and a filename.  This is useful for when you plan to write many more files than can be held in a single directory.  This function will help you create a tree of directories for the files that you want to store.</pre>

        </article>

    *   <a name="DC-UTILITIES:STORE-SAVE"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[STORE-SAVE](#DC-UTILITIES:STORE-SAVE)`

        `ROOT KEY OBJECT``)`</header>

        </article>

    *   <a name="DC-UTILITIES:THAW"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THAW](#DC-UTILITIES:THAW)`

        `STRING``)`</header>

        <pre class="docstring">Deserializes an object (or data structure) from its string representation, returning the object.</pre>

        </article>

    *   <a name="DC-UTILITIES:THREAD-POOL-PROGRESS"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THREAD-POOL-PROGRESS](#DC-UTILITIES:THREAD-POOL-PROGRESS)`

        `POOL-NAME``)`</header>

        </article>

    *   <a name="DC-UTILITIES:THREAD-POOL-RUN-TIME"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THREAD-POOL-RUN-TIME](#DC-UTILITIES:THREAD-POOL-RUN-TIME)`

        `POOL-NAME``)`</header>

        </article>

    *   <a name="DC-UTILITIES:THREAD-POOL-START"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THREAD-POOL-START](#DC-UTILITIES:THREAD-POOL-START)`

        `POOL-NAME THREAD-COUNT JOB-QUEUE FN-JOB &OPTIONAL FN-FINALLY``)`</header>

        <pre class="docstring">Starts thread-count threads using pool-name to name the threads and
        runs fn-job with those threads.  Each thread runs fn-job, which takes
        no parameters, in a loop.  When all the threads are done, this
        function checks fn-finally.  If the caller provides fn-finally, then
        this function returns with the result of calling fn-finally.  If the
        caller doesn't provide fn-finally, then the this function exists with
        a sum of the return values of all the threads that ran.</pre>

        </article>

    *   <a name="DC-UTILITIES:THREAD-POOL-START-TIME"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THREAD-POOL-START-TIME](#DC-UTILITIES:THREAD-POOL-START-TIME)`

        `POOL-NAME``)`</header>

        </article>

    *   <a name="DC-UTILITIES:THREAD-POOL-STOP"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THREAD-POOL-STOP](#DC-UTILITIES:THREAD-POOL-STOP)`

        `POOL-NAME``)`</header>

        </article>

    *   <a name="DC-UTILITIES:THREAD-POOL-STOP-TIME"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THREAD-POOL-STOP-TIME](#DC-UTILITIES:THREAD-POOL-STOP-TIME)`

        `POOL-NAME``)`</header>

        </article>

    *   <a name="DC-UTILITIES:THREAD-POOL-TIME-TO-GO"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[THREAD-POOL-TIME-TO-GO](#DC-UTILITIES:THREAD-POOL-TIME-TO-GO)`

        `POOL-NAME TOTAL-RECORD-COUNT``)`</header>

        </article>

    *   <a name="DC-UTILITIES:TIME-TO-GO"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[TIME-TO-GO](#DC-UTILITIES:TIME-TO-GO)`

        `CHANGE-PER-SECOND RECORD-COUNT``)`</header>

        <pre class="docstring">Given the number of records per second that are being processed (given in change-per-second) and the nuber of records remaining (given in record-count), this function computes the amount of time still left before all the records have been processed.</pre>

        </article>

    *   <a name="DC-UTILITIES:TIMESTAMP"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[TIMESTAMP](#DC-UTILITIES:TIMESTAMP)`

        `&KEY (TIME (GET-UNIVERSAL-TIME)) STRING (FORMAT "Y-M-DTh:m:s")``)`</header>

        <pre class="docstring">Returns the give time (or the current time) formatted according to
           the format parameter, followed by an optional string.  If a string
           is provided, the function adds a space to the result and then
           appends the string to that.  The format string can contain any
           characters.  This function will replace the format characters Y, M,
           D, h, m, and s, respectively, with numbers representing the year,
           month, day, hour, minute, and second.  All the numbers are 2 digits
           long, except for the year, which is 4 digits long.</pre>

        </article>

    *   <a name="DC-UTILITIES:TO-ASCII"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[TO-ASCII](#DC-UTILITIES:TO-ASCII)`

        `S``)`</header>

        <pre class="docstring">Converts the string s, which may contain non-ascii characters, into
           a string with nothing but ascii characters.  Non ascii characters are
           converted into spaces.  If s is a list, this function converts each
           element of the list into an all-ascii string.</pre>

        </article>

    *   <a name="DC-UTILITIES:TRIM"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[TRIM](#DC-UTILITIES:TRIM)`

        `S &OPTIONAL (FAT "^\\s+|\\s+$")``)`</header>

        <pre class="docstring">Trim fat from the string.  The fat parameter is optional and defaults to "^\s+|\s+$", which means "Whitespace at the beginning or end of the string".</pre>

        </article>

    *   <a name="DC-UTILITIES:UINT-TO-BYTES"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[UINT-TO-BYTES](#DC-UTILITIES:UINT-TO-BYTES)`

        `I &OPTIONAL (SIZE 4)``)`</header>

        </article>

    *   <a name="DC-UTILITIES:VERIFY-STRING"></a>

        <article>

        <header class="function"><span class="type">function</span> `(`

        #### `[VERIFY-STRING](#DC-UTILITIES:VERIFY-STRING)`

        `STRING REGEX &KEY IGNORE-CASE``)`</header>

        <pre class="docstring">Return t if the string matches the regex exactly.  Use the ignore-case parameter if you want case-insensitve matches.</pre>

        </article>

    *   <a name="DC-UTILITIES:FILTER-FILE"></a>

        <article>

        <header class="macro"><span class="type">macro</span> `(`

        #### `[FILTER-FILE](#DC-UTILITIES:FILTER-FILE)`

        `&REST ARGS``)`</header>

        <pre class="docstring">Copies lines from input to output, omitting lines for which body returns nil.</pre>

        </article>

    *   <a name="DC-UTILITIES:WITH-LINES-IN-FILE"></a>

        <article>

        <header class="macro"><span class="type">macro</span> `(`

        #### `[WITH-LINES-IN-FILE](#DC-UTILITIES:WITH-LINES-IN-FILE)`

        `&REST ARGS``)`</header>

        <pre class="docstring">Runs body for each line in the file specified by filename.</pre>

        </article>

</article>

</main>
