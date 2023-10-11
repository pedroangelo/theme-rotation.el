;; THEME ROTATION
;; automatically changes theme according to chosen times

(defgroup theme-rotation ()
  "Automatically change theme according to time of day."
  :group 'external)

(defcustom theme-rotation-config
	'(("08:00" . tsdh-light)
		("20:00" . tsdh-dark))
	"Theme rotation configuration, composed of starting times for specific themes."
	:type '(set (cons string function))
  :group 'theme-rotation)
	
;; AUXILIARY FUNCTIONS AND VARIABLES

; set variable with current theme
(setq theme-rotation-current-theme nil)

(defun theme-rotation-convert-time-pair-to-string (date)
	"convert date pair format to string"
	(concat (number-to-string (car date)) ":" (number-to-string (cdr date))))

(defun theme-rotation-convert-time-string-to-pair (string)
	"convert date string format to pair"
	(cons (string-to-number (substring string 0 2)) (string-to-number (substring string 3 5))))

(defun theme-rotation-get-current-time ()
	"get current time as a pair: hour number as car and minute number as cdr"
  (let ((current-hour (string-to-number (substring (current-time-string) 11 13))) 
        (current-minute (string-to-number (substring (current-time-string) 14 16))))
    (cons current-hour current-minute)))

(defun theme-rotation-list-starting-times-string ()
	"list starting times in the theme rotation config, as strings"
	(mapcar 'car theme-rotation-config))

(defun theme-rotation-list-starting-times ()
	"list starting times in the theme rotation config"
	(mapcar 'theme-rotation-convert-time-string-to-pair (mapcar 'car theme-rotation-config)))

(defun theme-rotation-list-themes ()
	"list the themes in the theme rotation config"
	(mapcar 'cdr theme-rotation-config))

(defun theme-rotation-list-time-intervals ()
	"list time intervals, built from theme rotation config"
	; build list of time intervals except the last that loops around
	(let ((time-intervals (cl-mapcar #'cons
									                 (theme-rotation-list-starting-times) 
									                 (cdr (theme-rotation-list-starting-times))))
        ; build last element of list of time intervals, the one that loops around
				; join last starting time with first starting time
	      (final-time-interval (cons (nth (- (length (theme-rotation-list-starting-times)) 1) (theme-rotation-list-starting-times))
							                     (nth 0 (theme-rotation-list-starting-times)))))
	; return complete list
	  (append time-intervals (cons final-time-interval nil))))

(defun theme-rotation-list-time-intervals-themes ()
	"list time intervals along with respective theme, built from theme rotation config"
	; zip list of time intervals with list of themes
	(cl-mapcar #'cons (theme-rotation-list-time-intervals) (theme-rotation-list-themes)))

(defun theme-rotation-is-time-before-inclusive-p (time1 time2)
	"return t if time in first argument happens before time in second argument, or if both are equal"
	(if (or (< (car time1) (car time2)) 
					(and (= (car time1) (car time2)) 
							 (<= (cdr time1) (cdr time2)))) t nil))

(defun theme-rotation-is-time-before-exclusive-p (time1 time2)
	"return t if time in first argument strictly happens before time in second argument"
	(if (or (< (car time1) (car time2)) 
					(and (= (car time1) (car time2)) 
							 (< (cdr time1) (cdr time2)))) t nil))

(defun theme-rotation-current-time-interval-p (time-interval)
	"return t if current time is within argument time interval"
  (let ((current-time (theme-rotation-get-current-time))
        (starting-time (car time-interval))
        (ending-time (cdr time-interval)))
	; check if midnight is within time interval or not
	(if (theme-rotation-is-time-before-inclusive-p starting-time ending-time)
			; if midnight is not within time interval
			(and
			 ; check if starting time comes before current time and
			 (theme-rotation-is-time-before-inclusive-p starting-time current-time)
			 ; check if current time comes before ending time
			 (theme-rotation-is-time-before-exclusive-p current-time ending-time))
		  ; if midnight is with time interval
		  (or 
			 ; check if starting time comes before current time or
			 (theme-rotation-is-time-before-inclusive-p starting-time current-time)
			 ; check if current time comes before ending time
			 (theme-rotation-is-time-before-exclusive-p current-time ending-time)))))

;; MAIN FUNCTIONALITY

(defun theme-rotation-get-next-theme ()
	"get appropriate theme from theme rotation according to current time of day"
	; set counter to 0
	(let ((i 0)
        (next-theme nil))
    
	; loop on all themes and respective intervals
	  (progn
      (while (/= i (length theme-rotation-config))
				(let ((theme (nth i (theme-rotation-list-time-intervals-themes))))
				 ; if theme's interval contains the current time
				  (if (theme-rotation-current-time-interval-p (car theme))
						  (progn
							; change chosen theme
							  (setq next-theme (cdr theme))
							; update counter to break out of loop
							  (setq i (length theme-rotation-config)))
					  (setq i (+ i 1)))))
      next-theme)))

(defun theme-rotation-update-theme (&optional provided-theme)
  "update theme according to time of day, first disabling the previous theme"
  (cond
   ;; provided-theme is nil, so get next-theme according to current time
   ((equal provided-theme nil)
    (let ((next-theme (theme-rotation-get-next-theme)))
      (if (equal next-theme theme-rotation-current-theme)
          nil
        (progn
          (when (not (equal theme-rotation-current-theme nil))
            (disable-theme theme-rotation-current-theme t))
          (load-theme next-theme t)
          (setq theme-rotation-current-theme next-theme)))))
   ;; if we have a provided-theme, apply that theme
   (t
    (if (equal provided-theme theme-rotation-current-theme)
        nil
      (progn
        (when (not (equal theme-rotation-current-theme nil))
          (disable-theme theme-rotation-current-theme))
        (load-theme provided-theme t)
        (setq theme-rotation-current-theme provided-theme))))))

(defun theme-rotation-set-timer (time)
	"set a timer to call theme changer function every day"
  (run-at-time time 86400 'theme-rotation-update-theme))

(defun theme-rotation-set-all-timers ()
  "set timers for each theme's starting time"
  (mapcar 'theme-rotation-set-timer (theme-rotation-list-starting-times-string)))

(defun theme-rotation-mode ()
  "change theme according to time of day while also setting timers"
  (progn
    (theme-rotation-update-theme)
    (theme-rotation-set-all-timers)))

;; (defun theme-rotation-set-timer (pair)
;; 	"set a timer to call theme changer function every day"
;;   (let ((starting-time (car pair))
;;         (theme (cdr pair)))
;;     (run-at-time starting-time (* 60 60 24) (lambda () (theme-rotation-update-theme theme)))))

;; (defun theme-rotation-set-all-timers ()
;;   "set timers for each theme's starting time"
;;   (mapcar 'theme-rotation-set-timer theme-rotation-config))

(provide 'theme-rotation) 
;;; theme-rotation.el ends here
