; -------------------------------------------------------------
; Edward Alan Puccini 07.02.2016
; -------------------------------------------------------------
; Speaker - language synthesis and recognition
; -------------------------------------------------------------
; file: speaker.asd
; -------------------------------------------------------------

(require 'asdf)

(defsystem "speaker-example"
  :description "Speaker - a language synthesis and recognition application"
  :version "0.1"
  :author "Edward Puccini epuccini@gmx.de"
  :license "LGPL"
  :depends-on ( "cffi" "trivial-main-thread")
  :components (( :file "package" )
			   ( :file "speaker" :depends-on ( "package" ))
               ( :file "example" :depends-on ( "speaker" ))))
