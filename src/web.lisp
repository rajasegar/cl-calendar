(in-package :cl-user)
(defpackage cl-calendar.web
  (:use :cl
        :caveman2
        :cl-calendar.config
        :cl-calendar.view
        :cl-calendar.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :cl-calendar.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defvar *day-names* '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" ))
(defvar *month-names* '("January" "February" "March" "April" "May" "June" "July" "August" "September"  "October" "November" "December"))

(defun get-date (year month date)
  )

(defun get-day (year month date)
  )
(defun get-calendar-rows (month year)
  (let* ((days-in-month (- 32 ))))
  )

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html" (list :day-names *day-names*)))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
