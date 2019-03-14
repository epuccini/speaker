; ------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Speaker library make and loader
; -------------------------------------------------------------
; file: main.lisp 
; -------------------------------------------------------------
; make - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------
; Requirements: cffi
; -------------------------------------------------------------

(eval-when (:load-toplevel :compile-toplevel :execute)
  (use-package :speaker))

(require 'bordeaux-threads)
(require 'trivial-main-thread)

;;
;; Global objects
;;
(defvar *speaker* nil)
(defvar *listener* nil)
(defvar *stop-flag* nil)

;;
;; Utility macro
;;
(defmacro spoken (imperative response voice)
  "Check if commando is spoken and respond
with a response."
  `(cond ((equal text ,imperative)
		  (progn
			(stop-listening *listener*)
			(set-voice-with *speaker* ,voice)
			(speak-with *speaker* ',response)
		    (start-listening *listener*)))))

(defmacro spoken-action (imperative response voice)
  "Check if commando is spoken and respond
with a response."
  `(cond ((equal text ,imperative)
		  (progn
			(stop-listening *listener*)
			(set-voice-with *speaker* ,voice)
			(speak-with *speaker* ',response)
			',response
		    (start-listening *listener*)))))

;;
;; Callbacks
;;
(cffi:defcallback wsw-callback :void ((text :string))
  (format t "Called back and spoke: ~A!~%" text))

(cffi:defcallback wsp-callback :void ((op-code :short))
  (format t  "Called back and spoke phoneme (op-code: ~D)!~%" op-code))

(cffi:defcallback dfs-callback :void ()
  (format t "Called back and did finish word!~%"))

(cffi:defcallback drc-callback :void ((text :string))
  (format t "Called back and recognize: ~A.~%" text)		  
  (spoken "test" "this is a test" 7)
  (spoken "you" "I am master blaster" 7)
  (spoken "speak" "What should i say" 7)
  ;; say exit and it will exit
  (spoken-action "exit" (setq *stop-flag* t) 7))

(defun load-data (path)
  "Load ascii file from path."
  (let ((store '()))
	(with-open-file (stream path)
      (do ((line (read-line stream nil)
				 (read-line stream nil)))
          ((null line))
		(setf store (append store (list line)))))
	store))

(defun listener-setup (listener)
  (add-commands listener "test" "speak" "exit" "you")
  (speaker:start-listening listener))

;;
;; Main
;;
(defun main ()
  "Main test program."
  (trivial-main-thread:with-body-in-main-thread () ;; main loop
	(terpri)
	(princ "Start speaker")
	(init-speaker)
	(set-voice 7)
	(speak "Speech recognition is only supported on Mac OSX platforms")
	
	;; now with object
#+darwin
	(progn
	  (setf *speaker* (make-speaker))
	  (setf *listener* (make-listener))
	  ;; setup callbacks
	  (register-did-recognize-command-callback
	   *listener* (cffi:callback drc-callback))
	  (register-will-speak-word-callback
	   *speaker* (cffi:callback wsw-callback))
	  
	  ;; setup voice, speak and listen
	  (set-voice-with *speaker* 7)
	  (listener-setup *listener*)
	  
	  ;; setup loop
	  (setf *stop-flag* nil)
	  (loop while (not *stop-flag*) do 
		   (mainloop-speaker *speaker*)
		   (mainloop-listener *listener*))
	  ;; exit
	  (setf *stop-flag* nil)
	  (stop-listening *listener*)
	  (speak-with *speaker* "exit")
	  (cleanup-with *speaker*))
  ))



