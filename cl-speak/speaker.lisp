; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; CL-Speak cffi
; -------------------------------------------------------------
; file: speker.lisp
; -------------------------------------------------------------
; Define library exampple callbacks and c-function-wrapper
; -------------------------------------------------------------

(in-package :cl-sp)

(require 'cffi)

;; ---------------------------------
;; Setup library (platform specific)
;;----------------------------------

(define-foreign-library libspeak
  (:darwin  "/usr/local/lib/libspeak.dylib")
  (:windows "libspeak.dll")
  (:unix (:or "libspeak.so" "/usr/local/lib/libspeak.so"))
  (t (:default "libspeak")))

(load-foreign-library "libspeak.dylib")

; -------------------------------------------------------------
; These function wrap basic c-functions working with
; single speakers. No objects. Designed to fast use.
; Disadvantage: no callback - only single speakers
; -------------------------------------------------------------


(defun init-with-speech (speech)
  "Initialize synth with given speech."
   (with-foreign-string (foreign-speech speech)
     (foreign-funcall "init_with_speech" :string foreign-speech :void)))

(defun speak(text)
  (with-foreign-strings ((foreign-text text)
			 (foreign-speech "com.apple.speech.synthesis.voice.Alex"))
    (foreign-funcall "speak" :string foreign-text :void)))


(defun start-speaking-string(text)
  (with-foreign-strings ((foreign-text text)
			 (foreign-speech "com.apple.speech.synthesis.voice.Alex"))
 
    (foreign-funcall "start_speaking_string" :string foreign-text :void)))


(defun available-voices-count ()
  (let ((retval (foreign-funcall "available_voices_count" :uint)))
    retval))

(defun set-voice (idx)
  (foreign-funcall "set_voice" :uint idx :void))


(defun get-voice-name (idx)
  (with-foreign-pointer-as-string (voice-c-str 255)
    (setf (mem-ref voice-c-str :char) 0)
    (foreign-funcall "get_voice_name" :uint idx :pointer voice-c-str :void)
    (foreign-string-to-lisp voice-c-str)))

; -------------------------------------------------------------
; These function wrap objective-c class methods
; First call have to be make-speaker to create
; a Speaker-class instance
; Avantage: callbacks - multiple speakers
; -------------------------------------------------------------

(defun make-speaker (speech)
  "Create speaker instance and initialize
created synth instance with given speech."
   (with-foreign-string (foreign-speech speech)
     (let ((speaker (foreign-funcall "make_speaker" :string foreign-speech :pointer)))
	   speaker)))

(defun speak-with (speaker text)
  (with-foreign-string (foreign-text text)
	(foreign-funcall "speak_with" :pointer speaker :string foreign-text :void)))

(defun set-voice-with (speaker voiceid)
  (foreign-funcall "set_voice_with" :pointer speaker :uint voiceid :void))

(defun register-did-finish-speaking-callback (speaker callback)
  (foreign-funcall "register_did_finish_speaking_callback" 
				   :pointer speaker :pointer callback :void))
