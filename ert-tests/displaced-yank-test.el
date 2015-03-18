(require 'displaced-yank)

(ert-deftest test-displaced-yank ()
  (mapc (lambda (function)
          (let ((funcname (car function))
                (data     (cdr function)))
            (eval `(define-displaced-yank ,funcname ,data))))
        '((parens              "()")
          (brackets-with-colon "[:]")
          (little-arrow        "->" 0)
          (doxygen-comment     "/*!  */" 3)))

  (let ((p (point)))
    (yank-displaced-parens)
    (should (eq (point) (+ p 1)))
    (setq p (point))

    (yank-displaced-brackets-with-colon)
    (should (eq (point) (+ p 2)))
    (setq p (point))

    (yank-displaced-little-arrow)
    (should (eq (point) (+ p 2)))
    (setq p (point))

    (yank-displaced-doxygen-comment)
    (should (eq (point) (+ p 4)))))

(provide 'displaced-yank-test)
;;; displaced-yank-test.el ends here
