;; THEME ROTATION
;; automatically changes theme according to chosen times

(defgroup theme-rotation ()
  "Automatically change theme according to time of day."
  :group 'external)

(defcustom theme-rotation-config
	'(("08:00" . tsdh-light)
		("20:00" . tsdh-dark))
	"Configuration of theme rotation, composed of starting times for specific themes."
	:type '(set (cons string function))
  :group 'theme-rotation)
	
;; AUXILIARY FUNCTIONS AND VARIABLES

; set variable with current theme
(setq current-theme nil)

(defun convert-time-pair-to-string (date)
	"convert date pair format to string"
	(concat (number-to-string (car date)) ":" (number-to-string (cdr date))))

(defun convert-time-string-to-pair (string)
	"convert date string format to pair"
	(cons (string-to-number (substring string 0 2)) (string-to-number (substring string 3 5))))

(defun get-current-hour ()
	"get the current hour as a number"
	(string-to-number 
	 (substring (current-time-string) 11 13)))

(defun get-current-minute ()
	"get the current minute as a number"
	(string-to-number 
	 (substring (current-time-string) 14 16)))

(defun get-current-time ()
	"get current time as a pair of numbers"
	(cons (get-current-hour) (get-current-minute)))

(defun get-list-starting-times-string ()
	"get a list of starting times in the theme rotation config, as strings"
	(mapcar 'car theme-rotation-config))

(defun get-list-starting-times ()
	"get a list of starting times in the theme rotation config"
	(mapcar 'convert-time-string-to-pair (mapcar 'car theme-rotation-config)))

(defun get-list-themes ()
	"get a list of themes in the theme rotation config"
	(mapcar 'cdr theme-rotation-config))

(defun get-list-time-intervals ()
	"get list of time intervals from theme rotation"
	; build list of time intervals except the last that loops around
	(setq time-intervals
				(cl-mapcar #'cons
									 (get-list-starting-times) 
									 (cdr (get-list-starting-times))))
	; build last element of list of time intervals, the one that loops around
	(setq final-time-interval
				; join last starting time with first starting time
				(cons (nth (- (length (get-list-starting-times)) 1) (get-list-starting-times))
							(nth 0 (get-list-starting-times))))
	; return complete list
	(append time-intervals (cons final-time-interval nil)))

(defun get-list-time-intervals-themes ()
	"get list of time intervals with list of themes, both from the theme rotation"
	; zip list of time intervals with list of themes
	(cl-mapcar #'cons (get-list-time-intervals) (get-list-themes)))

(defun is-time-before-inclusive-p (time1 time2)
	"return t if time in first argument happens before time in second argument, or if both are equal"
	(if (or (< (car time1) (car time2)) 
					(and (= (car time1) (car time2)) 
							 (<= (cdr time1) (cdr time2)))) t nil))

(defun is-time-before-exclusive-p (time1 time2)
	"return t if time in first argument strictly happens before time in second argument"
	(if (or (< (car time1) (car time2)) 
					(and (= (car time1) (car time2)) 
							 (< (cdr time1) (cdr time2)))) t nil))

(defun current-time-interval-p (time-interval)
	"return t if current time is within argument time interval"
	(setq current-time (get-current-time))
	(setq starting-time (car time-interval))
	(setq ending-time (cdr time-interval))
	; check if midnight is within time interval or not
	(if (is-time-before-inclusive-p starting-time ending-time)
			; if midnight is not within time interval
			(and
			 ; check if starting time comes before current time and
			 (is-time-before-inclusive-p starting-time current-time)
			 ; check if current time comes before ending time
			 (is-time-before-exclusive-p current-time ending-time))
		  ; if midnight is with time interval
		  (or 
			 ; check if starting time comes before current time or
			 (is-time-before-inclusive-p starting-time current-time)
			 ; check if current time comes before ending time
			 (is-time-before-exclusive-p current-time ending-time))))

;; MAIN FUNCTIONALITY

(defun get-new-theme ()
	"get appropriate theme from theme rotation according to current time of day"
	; set counter to 0
	(setq i 0)
	(setq chosen-theme nil)
	; loop on all themes and respective intervals
	(while (/= i (length theme-rotation-config))
				 (setq theme (nth i (get-list-time-intervals-themes)))
				 ; if theme's interval contains the current time
				 (if (current-time-interval-p (car theme))
						 (progn
							; change chosen theme
							(setq chosen-theme (cdr theme))
							; update counter to break out of loop
							(setq i (length theme-rotation-config)))
					   (setq i (+ i 1))))
	chosen-theme)

(defun set-theme-from-rotation ()
	"change theme automatically according to time of day"
	(setq new-theme (get-new-theme))
	(if (equal new-theme current-theme)
			nil
		  (setq current-theme new-theme)
			(load-theme current-theme t)))

;; TIMERS

(defun set-theme-timer (time)
	"set a timer to call theme changer function every day"
	(run-at-time time 86400 'set-theme-from-rotation))

(defun set-all-theme-timers ()
	"set timers for each theme's starting time"
	(mapcar 'set-theme-timer (get-list-starting-times-string)))

(defun theme-rotation-mode ()
  "change theme according to time of day while also setting timers"
  (progn
    (set-theme-from-rotation)
    (set-all-theme-timers)))

(provide 'theme-rotation) 
;;; theme-rotation.el ends here
