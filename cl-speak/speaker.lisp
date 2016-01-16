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

(define-foreign-library libspeak
  (:darwin  "/usr/local/lib/libspeak.dylib")
  (:windows "libspeak.dll")
  (:unix (:or "libspeak.so" "/usr/local/lib/libspeak.so"))
  (t (:default "libspeak")))

(load-foreign-library "libspeak.dylib")

(defun init-with-speech (speech)
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

