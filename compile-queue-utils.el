;;; -*- lexical-binding: t -*-

(defmacro compile-queue--save-var-excursion (var-names &rest body)
  "Bind the variables globally in VAR-NAMES and reset them after BODY.
Accepts initial values for the var-names as well similar to `let' bindings."
  (declare (indent 1))
  (let* ((var-names (if (symbolp var-names) (list var-names) var-names))
         (vars
          (->> var-names
               (--map (append
                       (list (make-symbol (concat
                                           "old-"
                                           (-> (pcase it
                                                 ((pred listp) (car it))
                                                 ((pred symbolp) it)
                                                 (_ (user-error "Unexpected type: %s" it)))
                                               symbol-name))))
                       (if (listp it) (list (car it) (cadr it))
                         (list it)))))))
    `(let ,(append (->> vars (--map (list (car it) (cadr it)))))
       ,@(->> vars (--map (when (caddr it) (list (cadr it) (caddr it))))
              (--filter it)
              (--map (cons (intern "setq") it)))
       (unwind-protect
           (progn ,@body)
         (progn ,@(->> vars (--map `(setq ,(cadr it) ,(car it)))))))))

(provide 'compile-queue-utils)
