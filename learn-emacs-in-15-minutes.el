;; Examples from 'Learn X in Y minutes' where X = Elisp
(+ 1 2) ;; 3

(+ 2 (+ 1 1)) ;; 4

(setq my-name "Burin Choomnuan") ;; see mini-buffer

(insert "Hello") ;; Hello

;; Let's try it in Thai
(insert "สวัสดี") ;; สวัสดี

(insert "Hello" " world!") ;; Hello world!

(insert "Hello, I am " my-name) ;; Hello, I am Burin Choomnuan

(defun hello () (insert "Hello, I am " my-name))

(hello) ;; Hello, I am Burin Choomnuan

(defun hello (name) (insert "Hello " name))

(hello "you") ;; Hello you

(switch-to-buffer-other-window "*test*")

;; combine several sexp with `progn`
(progn
  (switch-to-buffer-other-window "*test*")
  (hello "you"))

;; Often useful to erase the buffer:
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "there"))

;; Or to go back to other window
(progn
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello "you")
  (other-window 1))

;; You can bind a value to a local variable with `let ':
(let ((local-name "again!"))
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (hello local-name)
  (other-window 1))

;; let's format string
(format "Hello %s!\n" "visitor") ;;
;; "Hello visitor!
;; "

(defun hello (name)
  (insert (format "Hello %s!\n" name)))

(hello "you")

;; Let's create another fuction which uses `let' :
(defun greeting (name)
  (let ((your-name "Burin"))
    (insert (format "Hello %s!\n\nI am %s."
                    name
                    your-name
                    ))))

(greeting "you")
;; Hello you!
;;
;; I am Burin.

;; Some function are interactive
(read-from-minibuffer "Enter your name: ")

;; Let's use this in our greeting
(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name :")))
    (insert (format "Hello!\n\nI am %s and you are %s."
                    from-name
                    your-name))))

(greeting "Burin")

;; Let display the result in the other window:

(defun greeting (from-name)
  (let ((your-name (read-from-minibuffer "Enter your name :")))
    (switch-to-buffer-other-window "*test*")
    (erase-buffer)
    (insert (format "Hello!\n\nI am %s and you are %s." from-name your-name))
    (other-window 1)))

;; Now let's test it
(greeting "Johnny")

;; Let's store a list of names:
(setq list-of-names '("Sarah" "Chloe" "Mathilde"))

;; Get the first element of this list with `car`
(car list-of-names) ;; "Sarah"

;; Get the list of all but the first
(cdr list-of-names)
;; ("Chloe" "Mathilde")

;; Add element to the beginning of the list with `push`
(push "Stephanie" list-of-names)
;; ("Stephanie" "Sarah" "Chloe" "Mathilde")

;; Call the hello function for each name on the list
(mapcar 'hello list-of-names)
;; Hello Stephanie!
;; Hello Sarah!
;; Hello Chloe!
;; Hello Mathilde!

;; redefined `greeting` to say hello to everyone in `list-of-names`
(defun greeting ()
  (switch-to-buffer-other-window "*test*")
  (erase-buffer)
  (mapcar 'hello list-of-names)
  (other-window 1))

;; let call it
(greeting)

;; First attemp
(defun replace-hello-by-bonjour ()
  (switch-to-buffer-other-window "*test*")
  (goto-char (point-min))
  (while (search-forward "Hello")
    (replace-match "Bonjour"))
  (other-windows 1)) 

(replace-hello-by-bonjour)

;; 2nd attempt
(defun replace-hello-by-bonjour ()
  (switch-to-buffer-other-window "*test*")
  (goto-char (point-min))
  (while (search-forward "Hello" nil t)
    (replace-match "Bonjour"))
  (other-windows 1))

(replace-hello-by-bonjour)

;; Let's colorize the names:
(defun boldify-names ()
  (switch-to-buffer-other-window "*test*")
  (goto-char (point-min))
  (while (re-search-forward "Bonjour \\(.+\\)!" nil t)
    (add-text-properties (match-beginning 1)
                         (match-end 1)
                         (list 'face 'bold)))
  (other-window 1))

(boldify-names)
