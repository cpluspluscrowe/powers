(setq total (read))
(setq power (read))
(setq max-digit (expt total (/ 1 power)))

(defun get-numbers (current-number)
                                        ; base case is if current-number > max-digit
  (if (not (> current-number max-digit))
      (append (list (expt current-number power)) (get-numbers (+ current-number 1))
              )))

(setq numbers (get-numbers 0))

(defun flatten (nested)
  (if (and (listp nested) nested)
      (remove nil (append (flatten (first nested)) (flatten (cdr nested))))
    (list nested)))

(defun add-values-to-list (given values-to-add) ; base case is when there is nothing
  (if (not values-to-add) given
    (let ((to-add (first values-to-add))
          (rest (cdr values-to-add)))
      (cons (add-values-to-list given (list to-add)) (add-values-to-list given (rest))
            ))))

(defun add-values (to-build values-to-add)
  (if (eq (list-length to-build) (list-length values-to-add))
      to-build
    (cons to-build (add-values (values-to-add-to-list to-build values-to-add) values-to-add))
    ))

(princ (add-values (list) numbers))



(defun add-values (to-build values-to-add)
  (if (eq (list-length to-build) (list-length values-to-add))
      to-build
    (cons to-build (add-values (add-values-to-list to-build values-to-add) values-to-add))
    ))
