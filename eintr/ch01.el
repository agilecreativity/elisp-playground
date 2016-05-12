;; List Lists
'(rose
  violet
  daisy
  buttercup)
;; (rose violet daisy buttercup)

'(this list has (a list inside of it))
;; (this list has (a list inside of it))

'(this list includes "text between quotation marks.")
;; (this list includes "text between quotation marks.")

;; whitespaces in lisp
'(this list
  looks like this)
;; (this list looks like this)

'(this list looks like this)
;; (this list looks like this)

(+ 2 2)
;; 4

'(this is a quoted list)
;; (this is a quoted list)

;; (this is unquoted list) ;; will throw an error

(+ 2 (+ 3 3))
;; 8

fill-column
;; 80

;; (fill-column) ;; will raises error

(concat "abc" "def")
;; "abcdef"

(substring "The quick brown fox jumped." 16 19)
;; "fox"

(+ 2 fill-column)
;; 82

(concat "The " (number-to-string (+ 2 fill-column)) " red foxes.")
;; "The 82 red foxes."

;; Page 13: TBC
