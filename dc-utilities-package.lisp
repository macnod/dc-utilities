;;;; package.lisp

(defpackage :dc-utilities
  (:use :cl :cl-ppcre :trivial-utf-8 :sb-thread :sb-ext :cl-csv)
  (:import-from :ironclad
                :ascii-string-to-byte-array
                :byte-array-to-hex-string
                :digest-sequence
                :sha512)
  (:export

   *ds-timings*
   alist-to-plist
   alist-values
   aws-load-settings
   aws-setting
   bytes-to-uint
   change-per-second
   choose-from-list
   command-line-options
   create-directory
   cull-named-params
   directory-exists-p
   distinct-elements
   distinct-values
   document-package
   ds
   ds-clone
   ds-from-json
   ds-get
   ds-keys
   ds-list
   ds-merge
   ds-set
   ds-to-json
   ds-type
   elapsed-time
   factorial
   fast-compress
   fast-decompress
   fib
   file-exists-p
   file-extension
   file-line-count
   filter-file
   flatten
   freeze
   freeze-n-spew
   hash-hmac-256
   hash-keys
   hash-list-add-columns
   hash-list-column
   hash-list-columns
   hash-list-filter
   hash-list-first-index-of
   hash-list-first-record-of
   hash-list-from-csv
   hash-list-from-list
   hash-list-from-plists
   hash-list-keys
   hash-list-remove-columns
   hash-list-rename-columns
   hash-list-set
   hash-list-to-csv
   hash-list-to-plists
   hash-list-to-table
   hash-string
   hash-to-list
   hash-to-plist
   hash-values
   hashify-list
   home-based
   home-settings-file
   index-of-max
   index-values
   initialize-progress-report
   interrupt-sleep
   interruptible-sleep
   join-paths
   k-combination
   list-keys
   list-values
   load-settings
   lof
   log-entry
   make-keyword
   make-time-tracker
   mark-time
   memoize
   memoize-with-limit
   month-string-to-int
   parse-date-1
   parse-number
   partition-by-lambdas
   partition-by-size
   path-only
   plist-clean-keys
   plist-keys
   plist-list-to-csv
   plist-values
   qsort
   range
   read-one-line
   read-settings-file
   replace-extension
   replace-regexs
   report-progress
   scrape-string
   sequence-bytes-to-uint
   sequence-uint-to-bytes
   setting
   setting
   shell-execute
   shift
   shuffle
   slurp
   slurp-binary
   slurp-n-thaw
   sort-keywords
   spew
   split-n-trim
   store-delete
   store-fetch
   store-path
   store-save
   string-to-keyword
   subtract-lists
   temp-file-name
   thaw
   thread-pool-job-queue
   thread-pool-progress
   thread-pool-result
   thread-pool-run-time
   thread-pool-start
   thread-pool-start-time
   thread-pool-stop
   thread-pool-stop-time
   thread-pool-time-to-go
   time-to-go
   time-zone-to-int
   timestamp
   to-ascii
   trim
   uint-to-bytes
   unique-file-name
   unique-name
   universal-to-unix-time
   unix-time
   unix-to-universal-time
   unshift
   verify-string
   with-lines-in-file
   ))
