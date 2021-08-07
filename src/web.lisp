(in-package :cl-user)
(defpackage cl-calendar.web
  (:use :cl
        :caveman2
        :cl-calendar.config
        :cl-calendar.view
        :cl-calendar.db)
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


(defun get-calendar-rows (month year)
  (let* ((days-in-month (local-time:days-in-month month year))
	 (first-day (parse-integer (local-time:format-timestring nil (local-time:encode-timestamp 0 0 0 0 1 month year) :format (list :weekday))))
	 (first-week-days (- 7 first-day))
	 (remaining-days (- days-in-month first-week-days))
	 (rows-needed (ceiling (/ remaining-days 7)))
	 (calendar (make-array 5 :initial-element (make-array 7 :initial-element ""))))

    ;; populate first week
    (let ((first-row (make-array 7 :initial-element "")))
      (loop for j from first-day to 6
	    for x from 1 to 7
	    do (setf (aref first-row j) x))
      (setf (aref calendar 0) first-row))

      ;; populate the remaining rows
      (loop for i from 1 to (1- rows-needed)
	    do(let ((temp (make-array 7 :initial-element "")))
		(loop for j from 0 to 6
		      do(let ((dt (+ first-week-days (* 7 (- i 1)) (+ j 1))))
			  (if (<= dt days-in-month)
			      (setf (aref temp j) dt)
			      (setf (aref temp j) ""))
			  ))
		(setf (aref calendar i) temp)))
      calendar))

;;
;; Routing rules

(defroute "/" ()
  (let* ((month (parse-integer (local-time:format-timestring nil (local-time:now) :format (list :month))))
	 (year (parse-integer (local-time:format-timestring nil (local-time:now) :format (list :year))))
	 (next-month (1+ month))
	 (prev-month (1- month)))
  (render #P"index.html" (list
			  :day-names *day-names*
			  :rows (get-calendar-rows month year)
			  :next-month next-month
			  :prev-month prev-month
			  :year year
			  :month-name (nth (1- month) *month-names*)))))


(defroute "/next" (&key _parsed)
  (let* ((month (parse-integer (cdr (assoc "month" _parsed :test #'string=))))
	(year (parse-integer (cdr (assoc "year" _parsed :test #'string=))))
	(next-month (1+ month))
	(prev-month (1- month)))
  (render #P"_calendar.html" (list
			  :day-names *day-names*
			  :rows (get-calendar-rows month year)
			  :next-month next-month
			  :prev-month prev-month
			  :year year
			  :month-name (nth (1- month) *month-names*)))))

(defroute "/previous" (&key _parsed)
  (print _parsed)
  (let* ((month (parse-integer (cdr (assoc "month" _parsed :test #'string=))))
	(year (parse-integer (cdr (assoc "year" _parsed :test #'string=))))
	(next-month (1+ month))
	(prev-month (1- month)))
  (render #P"_calendar.html" (list
			  :day-names *day-names*
			  :rows (get-calendar-rows month year)
			  :next-month next-month
			  :prev-month prev-month
			  :year year
			  :month-name (nth (1- month) *month-names*)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
		   *template-directory*))
