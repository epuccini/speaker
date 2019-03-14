; -------------------------------------------------------------
; Edward Alan Puccini 07.02.2016
; -------------------------------------------------------------
; Speaker - language synthesis and recognition
; -------------------------------------------------------------
; file: speaker.asd
; -------------------------------------------------------------

(require 'asdf)

(defsystem "speaker"
  :description "Speaker - a language synthesis and recognition library"
  :version "0.1"
  :author "Edward Puccini epuccini@gmx.de"
  :license "LGPL"
  :depends-on ( "cffi" )
  :components (( :file "package" )
			   ( :file "speaker" :depends-on ( "package" ))))
