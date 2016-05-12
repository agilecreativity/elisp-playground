;; chapter 11: Emacs Lisp Programming
(+ 2 4) ;; 6
(>= 4 2) ;; t
;;seq auto-save-interval 800
(defun count-word-buffer ()
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (forward-word 1)
        (setq count (1+ count)))
      (message "buffer contains %d words." count))))

(count-word-buffer) ;; "buffer contains 50 words."

(message "\"%s\" is a string, %d is a number, and %c is a character"
         "hi there"
         142
         ?q)
;; "\"hi there\" is a string, 142 is a number, and q is a character"

(message "This book was printed in %f, also known as %e." 2004 2004)
;; "This book was printed in 2004.000000, also known as 2.004000e+03."

(message "This book was printed in %.3e, also known as %.0f" 2004 2004)
;; "This book was printed in 2.004e+03, also known as 2004"
