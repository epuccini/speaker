# speaker
speaker - Multiplatform Speech synthesis library wrapper for Common Lisp

This speech synthesis library is a cffi interface to an adapter library which interfaces to a speech library of the platform. It enables text-to-speech and -recognition (recognition only on Mac OSX). Dependency is only cffi. The MacOSX version uses Cocoa NSSpeechSynthesizer, Windows uses Speech API, Linux uses QtTextToSpeech. For the exaxmples you need "bordeaux-threads" and "trivial-main-thread". Tested on sbcl, ecl and ccl on Linux, Mac OSX and Windows.

First open your IDE XCode/Visual Studio/QtCreator project "libspeak" and compile the libspeak.dylib/.so/.dll and copy the lib to /usr/local/lib. Windows users copy to Windows/System32. Then cd into the speaker/src directory and load your favouorite lisp and type:

    (asdf:load-system :speaker-examples)
    (main)

If you just use the library then just type

    (asdf:load-system :speaker)
    (init-speaker)
    (speak "Test")
    
License is based on GNU LESSER GENERAL PUBLIC LICENSE.
