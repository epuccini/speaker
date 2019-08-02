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

(require 'speaker)
(require 'bordeaux-threads)
(require 'trivial-main-thread)
(require 'cl-string-match)
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
            (speaker:stop-listening *listener*)
            (speaker:set-voice-with *speaker* ,voice)
            (speaker:speak-with *speaker* ',response)
            (speaker:start-listening *listener*)))))

(defmacro spoken-action (imperative response voice)
  "Check if commando is spoken and respond
with a response."
  `(cond ((equal text ,imperative)
          (progn
            (speaker:stop-listening *listener*)
            (speaker:set-voice-with *speaker* ,voice)
            (speaker:speak-with *speaker* ',response)
            ',response
            (speaker:start-listening *listener*)))))

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
  (speaker:speak (format nil "recognize: ~A.~%" text))
  (spoken "test" "this is a test" 7)
  (spoken "you" "I am master blaster" 7)
  (spoken "speak" "What should i say" 7)
  ;; say leave and it will exit
  (spoken-action "leave" (setq *stop-flag* t) 7))


(defun load-data-old (path)
   "Load ascii file from path."
  (let ((store '()))
    (with-open-file (stream path)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (setf store (append store (list line)))))
    store))

(defun load-data (path)
   "Load ascii file from path. Faster than read-sequence."
   (with-open-file (stream path :direction :input :element-type 'ascii:ub-char)
     (loop with reader = (ascii:make-ub-line-reader :stream stream)
	    for line = (ascii:ub-read-line-string reader)
	    while line
	    collect line)))

(defun load-data-alt (path)
  "Load ascii file from path. Faster than read-line or append."
  (with-open-file (stream path :direction :input)
    (let* ((len (file-length stream))
           (data (make-array len)))
      (read-sequence data stream)
      (setf data (map 'string #'append data))
      (cl-ppcre:split #\newline data))))

(defun listener-setup (listener)
  (let ((verbs (load-data "../src/verbs.txt")))
;  (speaker:add-commands listener "test" "speak" "exit" "you")
  (speaker:add-commands-list listener verbs)
  (speaker:start-listening listener)))

;;
;; Main
;;
(defun main ()
  "Main test program."
  (trivial-main-thread:with-body-in-main-thread () ;; main loop
    (terpri)
    (princ "Start speaker")
    (speaker:init-speaker)
    (speaker:set-voice 7)
    (speaker:speak "Speech recognition is only supported on Mac OSX platforms")
   
    ;; now with object
#+darwin (progn
		   (setf *speaker* (speaker:make-speaker))
		   (setf *listener* (speaker:make-listener))
		   ;; setup callbacks
		   (speaker:register-did-recognize-command-callback
			*listener* (cffi:callback drc-callback))
		   (speaker:register-will-speak-word-callback
			*speaker* (cffi:callback wsw-callback))
		   
		   ;; setup voice, speak and listen
		   (speaker:set-voice-with *speaker* 7)
		   (listener-setup *listener*)
		   
		   ;; setup loop
		   (setf *stop-flag* nil)
		   (loop while (not *stop-flag*) do 
				(speaker:mainloop-speaker *speaker*)
				(speaker:mainloop-listener *listener*))
		   ;; exit
		   (setf *stop-flag* nil)
		   (speaker:stop-listening *listener*)
		   (speaker:speak-with *speaker* "exit")
		   (speaker:cleanup-with *speaker*))
))



