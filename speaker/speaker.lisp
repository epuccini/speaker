; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; CL-Speak cffi
; -------------------------------------------------------------
; file: speker.lisp
; -------------------------------------------------------------
; Define library exampple callbacks and c-function-wrapper
; -------------------------------------------------------------

(in-package :speaker)

(require 'cffi)
(require 'bordeaux-threads)

(defvar *previous-readtables* nil)

;; ---------------------------------
;; Setup library (platform specific)
;;----------------------------------

#+darwin
(define-foreign-library libspeak
  (darwin  (:or "libspeak.dylib" "/usr/local/lib/libspeak.dylib")))

#+windows
(define-foreign-library libspeak
  (:windows (:or #P"libspeak.dll" #P"d:\\Code\\Common Lisp\\projects\\Speaker\\speaker\\libspeak.dll")))

#+linux
(define-foreign-library libspeak
  (:linux (:or "libspeak.so" "/usr/local/lib/libspeak.so")))

#+darwin 
(load-foreign-library "libspeak.dylib")

#+windows
(load-foreign-library #P"d:\\Code\\Common Lisp\\projects\\Speaker\\speaker\\libspeak.dll")

#+linux
(load-foreign-library "libspeak.so")

;; -----------------------
;; Error handling
;; -----------------------

(defun speaker-error-handler (condition)
  (format *error-output* "~&~A~&" condition)
  (throw 'common-parse niL))

; -------------------------------------------------------------
; These function wrap basic c-functions working with
; single speakers. No objects. Designed to fast use.
; Disadvantage: no callback - only single speakers
; -------------------------------------------------------------

(defun init-speaker ()
  "Initialize synth with given speech."
  (handler-case 
	  (foreign-funcall "init_speaker" :void)
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'init-speaker': ~A~%" condition))))


(defun format-with-list (fmt-msg args)
  "Format with argument-list."
  (eval
   `(format nil ,fmt-msg ,@args)))

(defun collect-args (args)
  "Create a string out of args."
  (let ((message "")
		(arg-list '()))
	(mapc (lambda (arg)
			(cond ((stringp arg)
				   (setq message (concatenate 'string
											  message arg)))
				  ((not (stringp arg))
				   (progn
					 (setq message (concatenate 'string 
												message "~A"))
					 (push arg arg-list))))) args)
	(format-with-list message arg-list)))

(defun speak(&rest args)
  "Speaks a lisp-string text. Initialization is needed."
  (handler-case
   (let ((text (collect-args args))) 
	 (with-foreign-strings ((foreign-text text)
							(foreign-speech "com.apple.speech.synthesis.voice.Alex"))
						   (foreign-funcall "speak" :string foreign-text :void)))
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'speak': ~A~%" condition))))


(defun available-voices-count ()
  "Get the amount of all available voices."
  (handler-case
	  (let ((retval (foreign-funcall "available_voices_count" :uint)))
		retval)
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'available-voices-count': ~A~%" condition))))


(defun set-voice (idx)
  "Set voice with id 'idx'."
  (handler-case
	  (foreign-funcall "set_voice" :uint idx :void)
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'set-voice': ~A~%" condition))))
  

(defun get-voice-name (idx)
  "Get name of a voice with index 'idx'."
  (handler-case
	  (with-foreign-pointer-as-string (voice-c-str 255)
		(setf (mem-ref voice-c-str :char) 0)
		(foreign-funcall "get_voice_name" :uint idx :pointer voice-c-str :void)
		(foreign-string-to-lisp voice-c-str))
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'get-voice-name': ~A~%" condition))))

(defun cleanup-speaker ()
  "Cleanup. Expecially useful for com-connection in windows."
  (handler-case
	  (foreign-funcall "cleanup_speaker" :void)
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'cleanup-speaker': ~A~%" condition))))
  


; -------------------------------------------------------------
; These function wrap objective-c class methods
; First call have to be make-speaker to create
; a Speaker-class instance
; Avantage: callbacks - multiple speakers
; -------------------------------------------------------------

(defun make-speaker ()
  "Create speaker instance and initialize
created synth instance with given speech."
  (handler-case 
	  (let ((speaker (foreign-funcall "make_speaker" :pointer)))
		speaker)
  (error (condition)
		 (format *error-output* "Speaker: error in 'make-speaker': ~A~%" condition))))

(defun speak-with (speaker &rest args)
  "Speak text with synth container 'Speaker'."
  (handler-case
   (let ((text (collect-args args)))
	 (with-foreign-string (foreign-text text)
						  (foreign-funcall "speak_with" :pointer 
										   speaker :string 
										   foreign-text :void)))
	(error (condition) 
	  (format *error-output* "Speaker: error in 'speak-with': ~A~%" condition))))

(defun set-voice-with (speaker voiceid)
  "Set voiceid in synth container 'Speaker'."
  (handler-case 
	  (foreign-funcall "set_voice_with" :pointer speaker :uint voiceid :void)
	(error (condition) 
	  (format *error-output* "Speaker: error in 'set-voice-with': ~A~%" condition))))

(defun cleanup-with (speaker)
  "Cleanup. Expecially useful for com-connection in windows."
  (handler-case
	  (foreign-funcall "cleanup_with" :pointer speaker :void)
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'cleanup-with': ~A~%" condition))))

(defun runloop-thread-speaker (speaker)
  "Mainthread runloop-loop for speaker."
  (handler-case 
	  (foreign-funcall "runloop_thread_speaker" :pointer speaker :void)
  (error (condition)
		 (format *error-output* "Speaker: error in 'runloop_thread_speaker': ~A~%" 
				 condition))))

(defun runloop-call-thread-speaker (speaker)
  "Mainthread single runloop-call for speaker."
  (handler-case 
	  (foreign-funcall "runloop_call_thread_speaker" :pointer speaker :void)
  (error (condition)
		 (format *error-output* "Speaker: error in 'runloop_call_thread_speaker': ~A~%" 
				 condition))))
        
;; ------------------------------------------------------
;; Lisp callbacks are called within objective-c delegates
;; ------------------------------------------------------

(defun register-will-speak-word-callback (speaker callback)
  "Register callback for 'willSpeakWord' delegate."
  (handler-case
	  (foreign-funcall "register_will_speak_word_callback" 
					   :pointer speaker :pointer callback :void)
	(error (condition) 
	  (format *error-output* 
			  "Speaker: error in 'register-will-speak-word-callback': ~A~%" condition))))


(defun register-will-speak-phoneme-callback (speaker callback)
  "Register callback for 'willSpeakPhoneme' delegate."
  (handler-case
	  (foreign-funcall "register_will_speak_phoneme_callback" 
					   :pointer speaker :pointer callback :void)
	(error (condition) 
	  (format *error-output* 
			  "Speaker: error in 'register-will-speak-phoneme-callback': ~A~%" condition))))


(defun register-did-finish-speaking-callback (speaker callback)
  "Register callback for 'didFinishSpeaking' delegate."
  (handler-case
	  (foreign-funcall "register_did_finish_speaking_callback" 
					   :pointer speaker :pointer callback :void)
	(error (condition) 
	  (format *error-output* 
			  "Speaker: error in 'register-did-finish-speaking-cakkback': ~A~%" condition))))

; -------------------------------------------------------------
; These function wrap objective-c class methods
; First call have to be make-listener to create
; a Listener-class instance
; -------------------------------------------------------------

(defun make-listener ()
  "Create listener instance and initialize
runloop."
  (handler-case 
	  (let ((speaker (foreign-funcall "make_listener" :pointer)))
		speaker)
  (error (condition)
		 (format *error-output* "Speaker: error in 'make-listener': ~A~%" condition))))


(defun start-listening (listener)
  "Start speech recognizing."
  (handler-case 
	  (let ((listener (foreign-funcall "start_listening" :pointer listener :void)))
		listener)
  (error (condition)
		 (format *error-output* "Speaker: error in 'start_listening': ~A~%" condition))))

(defun stop-listening (listener)
  "Stop speech recognizing."
  (handler-case 
	  (let ((listener (foreign-funcall "stop_listening" :pointer listener :void)))
		listener)
  (error (condition)
		 (format *error-output* "Speaker: error in 'stop_listening': ~A~%" condition))))

(defun add-command (listener command)
  "Add command for recoginition."
  (handler-case
	  (with-foreign-strings ((foreign-command command))
		(foreign-funcall "add_command" :pointer listener :string foreign-command :void))
	(error (condition)
	  (format *error-output* 
			  "Speaker: error in 'add-command': ~A~%" condition))))

(defun runloop-listener (listener)
  "Listener runloop caller and autoreleasepool."
  (handler-case 
	  (foreign-funcall "runloop_listener" :pointer listener :void)
  (error (condition)
		 (format *error-output* "Speaker: error in 'runloop_listener': ~A~%" condition))))
  

(defun runloop-thread-listener (listener)
  "Listener runloop-thread."
  (handler-case 
	  (foreign-funcall "runloop_thread_listener" :pointer listener :void)
  (error (condition)
		 (format *error-output* "Speaker: error in 'runloop_thread_listener': ~A~%" 
				 condition))))

(defun runloop-call-thread-listener (listener)
  "Listener runloop thread as single call which returns after one loop."
  (handler-case 
	  (foreign-funcall "runloop_call_thread_listener" :pointer listener :void)
  (error (condition)
		 (format *error-output* "Speaker: error in 'runloop_call_thread_listener': ~A~%" 
				 condition))))

(defun stop-runloop-thread-listener (listener)
  "Stop speech recognizing."
  (handler-case 
	  (let ((listener (foreign-funcall "stop_runloop_thread_listener" 
									   :pointer listener :void)))
		listener)
  (error (condition)
	(format *error-output* "Speaker: error in 'stop_runloop_thread_listener': ~A~%" 
			condition))))
  
;; ------------------------------------------------------
;; Lisp callbacks are called within objective-c delegates
;; ------------------------------------------------------

(defun register-did-recognize-command-callback (listener callback)
  "Register callback for 'did recognize command' delegate."
  (handler-case
	  (foreign-funcall "register_did_recognize_command_callback" 
					   :pointer listener :pointer callback :void)
	(error (condition) 
	  (format *error-output* 
			  "Speaker: error in 'register-did-recognize-command-callback': ~A~%" condition))))

;;
;; Async operations
;;
(defun async-prefix (stream char)
  "Reader-macro function for 'async-' substitution."
  (declare (ignore char))
  `(bordeaux-threads:make-thread (lambda () ,(read stream t nil t)) 
								 :name "async-thread"))

(defmacro enable-async-syntax ()
  "Enable special-character '°' syntax."
  `(eval-when (:load-toplevel :compile-toplevel :execute)
      (push common-lisp:*readtable* *previous-readtables*)
      (setq common-lisp:*readtable* (copy-readtable))
      (set-macro-character #\° 'async-prefix)))
  
(defmacro disable-async-syntax ()
  "Disable special-character '°' syntax."
  `(eval-when (:load-toplevel :compile-slitoplevel :execute)
     (setq *readtable* (pop speaker:*previous-readtables*))))



 
  
