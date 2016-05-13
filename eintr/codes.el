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

(+) ;; 0
(*) ;; 1
(+ 3) ;; 3
(* 3) ;; 3
(+ 3 4 5) ;; 12
(* 3 4 5) ;; 60

;; Wrong type of argument
;(+ 2 'hello) ;; throw error
(message "This message appears in the echo area!")

(message "The name of this buffer is: %s" (buffer-name))
;; "The name of this buffer is: ch01.el"

(message "The value of fill-column is %d." fill-column)
;; "The value of fill-column is 80."

(message "There are %d %s in the office" (- fill-column 14) "pink elephants")
;; "There are 66 pink elephants in the office"

(message "He saw %d %s"
         (- fill-column 32)
         (concat "red "
                 (substring "The quick brown foxes jumped." 16 21)
                 " leaping."))
;; "He saw 48 red foxes leaping."

(set 'flowers '(rose violet daisy buttercup))
;; (rose violet daisy buttercup)

(setq flowers '(rose violet daisy buttercup))
;; (rose violet daisy buttercup)

(setq carnivores '(lion tiger leopard))
;; (lion tiger leopard)

(set 'carnivores '(lion tiger leopard))
;; (lion tiger leopard)

(setq trees '(pine fir oak maple)
      herbivores '(gazelle antelope zebra))
;; (gazelle antelope zebra) ;; note: the last result is returned
trees      ;; (pine fir oak maple)
herbivores ;; (gazelle antelope zebra)

(setq counter 0)
(setq counter (+ counter 1))
counter ;; 1

(buffer-file-name)

(current-buffer)
;; #<buffer ch02.el>

(other-buffer)
;; #<buffer *Ibuffer*>

;; switch to other buffer
;(switch-to-buffer (other-buffer))

(buffer-size)
;; 267

(point)
;; 284

;; See also: point-min, point-max

;; Chapter 3: functions
(defun multiply-by-seven (number)
  "Multiply NUMBER by seven"
  (* 7 number))

(multiply-by-seven 3) 
;; 21

(defn multiply-by-seven (number) ;; second version
  "Multiply NUMBER by seven."
  (+ number number number number number number number))

(multiply-by-seven 4)
;; 28

;; Make function interactive
(defun multiply-by-seven (number) ;; second version
  "Multiply NUMBER by seven."
  (interactive "p")
  (message "The result is %d" (* 7 number)))
;; To run this type C-u 3 M-x multiply-by-seven

;; 3.4: different options for `interactive' see: Code Character for `interactive'

;; Sample `let' expression
(let ((zebra 'stripes)
      (tiger 'fierce))
  (message "One kind of animal has %s and another is %s."
           zebra
           tiger))
;; "One kind of animal has stripes and another is fierce."

;; Uninitialized variable in a `let' statement
(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
   "Here are %d variables with %s, %s, and %s value"
   birch pine fir oak))
;; "Here are 3 variables with nil, nil, and some value"

(if (> 5 4)
    (message "5 is greater than 4!"))
;; "5 is greater than 4!"

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
If the CHARACTERISTIC is the symbol 'fierce then warn of a tiger."
  (if (equal characteristic 'fierce)
      (message "It's a tiger!")))

(type-of-animal 'fierce)
;; "It’s a tiger!"

(type-of-animal 'zebra)
;; nil

;; if-then-else form
(if (> 4 5)
    (message "4 falsely greater than 5!")
  (message "4 is not greater than 5!"))
;; "4 is not greater than 5!"

(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
If the CHARACTERISTIC is the symbol 'fierce then warn of a tiger;
else say it's not fierce"
  (if (equal characteristic 'fierce)
      (message "It's a tiger!")
    (message "It's not fierce!")))

(type-of-animal 'fierce)
;; "It’s a tiger!"

(type-of-animal 'zebra)
;; "It’s not fierce!"

(if 4
    'true
  'false)
;; true

(if nil
    'true
  'false)
;; false

;; save-excursion
(message "We are %d characters into this buffer."
         (- (point)
            (save-excursion
             (goto-char (point-min)) (point))))
;; "We are 4585 characters into this buffer."

emacs-version 
;; "25.1.50.1"

emacs-major-version 
;; 25

emacs-minor-version
;; 1

(if (= 25 emacs-major-version)
    (message "This is version 25 Emacs")
  (message "This is not version 25 Emacs"))
;; "This is version 25 Emacs"

;; switch back to the last location with `C-x Cx`
(defun simplified-beginning-of-buffer ()
  "Move point to the beginning of the buffer;
leave mark at previous point"
  (interactive)
  (push-mark) ;;
  (goto-char (point-min)))
