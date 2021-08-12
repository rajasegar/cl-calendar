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

(defun get-param (name parsed)
  "Get parameter values from _parsed"
  (cdr (assoc name parsed :test #'string=)))

(defun get-calendar-rows (month year)
  (let* ((days-in-month (local-time:days-in-month month year))
	 (first-day (parse-integer (local-time:format-timestring nil (local-time:encode-timestamp 0 0 0 0 1 month year) :format (list :weekday))))
	 (first-week-days (- 7 first-day))
	 (remaining-days (- days-in-month first-week-days))
	 (rows-needed (ceiling (/ remaining-days 7)))
	 (calendar (make-array (1+ rows-needed) :initial-element (make-array 7 :initial-element ""))))

    ;; populate first week
    (let ((first-row (make-array 7 :initial-element "")))
      (loop for j from first-day to 6
	    for x from 1 to 7
	    do (setf (aref first-row j) x))
      (setf (aref calendar 0) first-row))

      ;; populate the remaining rows
      (loop for i from 1 to rows-needed 
	    do(let ((temp (make-array 7 :initial-element "")))
		(loop for j from 0 to 6
		      do(let ((dt (+ first-week-days (* 7 (- i 1)) (+ j 1))))
			  (if (<= dt days-in-month)
			      (setf (aref temp j) dt)
			      (setf (aref temp j) ""))))
		(setf (aref calendar i) temp)))
      calendar))

(defun get-current-month ()
  (parse-integer (local-time:format-timestring nil (local-time:now) :format (list :month))))

(defun get-current-year ()
  (parse-integer (local-time:format-timestring nil (local-time:now) :format (list :year))))

(defun get-current-date ()
  (parse-integer (local-time:format-timestring nil (local-time:now) :format (list :day))))

(defun get-template-data (month year)
  "Format the data to be sent to Djula templates for calendar routes"
  (list
   :day-names local-time:+day-names+
   :rows (get-calendar-rows month year)
   :next-month (1+ month)
   :prev-month (1- month)
   :year year
   :month-name (aref local-time:+month-names+ month)
   :month month
   :current-date (get-current-date)
   :current-month (get-current-month)
   :current-year (get-current-year)))
;;
;; Routing rules

(defroute "/" ()
  (let* ((month (get-current-month))
	 (year (get-current-year)))
  (render #P"index.html" (get-template-data month year))))


(defroute "/next" (&key _parsed)
  (let* ((month (parse-integer (get-param "month" _parsed)))
	(year (parse-integer (get-param "year" _parsed))))
    (if (> month 12)
	(progn 
	  (setf month 1)
	  (incf year))
	nil)
  (render #P"_calendar.html" (get-template-data month year))))

(defroute "/previous" (&key _parsed)
  (print _parsed)
  (let* ((month (parse-integer (get-param "month" _parsed)))
	(year (parse-integer (get-param "year" _parsed))))
    (if (< month 1)
	(progn
	  (setf month 12)
	  (decf year))
	nil)
  (render #P"_calendar.html" (get-template-data month year))))

;; Today
(defroute "/today" ()
  (let* ((month (get-current-month))
	 (year (get-current-year)))
  (render #P"_calendar.html" (get-template-data month year))))

;; Modal
(defroute "/modal" (&key _parsed)
  (let ((date (parse-integer (get-param "date" _parsed)))
	(month (parse-integer (get-param "month" _parsed)))
	(year (parse-integer (get-param "year" _parsed))))
    (render #P"_modal.html" (list :date date
				  :month month
				  :year year
				  :month-name (aref local-time:+month-names+ month)))))

;; Events
(defroute ("/events" :method :POST) (&key _parsed)
  (let ((date (parse-integer (get-param "date" _parsed)))
	(month (parse-integer (get-param "month" _parsed)))
	(year (parse-integer (get-param "year" _parsed)))
	(time  (get-param "time" _parsed))
	(ampm  (get-param "ampm" _parsed))
	(description (get-param "description" _parsed)))
  (render #P"_event.html" (list :date date
				:month month
				:year year
				:time time
				:ampm ampm
				:description description))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
		   *template-directory*))
