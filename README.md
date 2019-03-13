# speaker
speaker - Multiplatform Speech synthesis library wrapper for Common Lisp

This speech synthesis library is a cffi interface to the speech library of the platform. It enables speech-output and -recognition (only Mac OSX). Dependency is only cffi. MacOSX uses Cocoa NSSpeechSynthesizer, Windows uses SAPI, Linux uses QtTextToSpeech. For exaxmples you need "bordeaux-threads" and "trivial-main-thread". Tested on sbcl, ecl and ccl.

First open the XCode/Visual Studio/QtCreator project libspeak and compile the libspeak.dylib/.so/.dll and copy the lib to /usr/local/lib. Windows users copy to Windows/System32.
Then cd into the speaker/src directory and load your favouorite lisp and type:

    (asdf:load-system :speaker-examples)
    (main)

If you just use the library then just type

    (asdf:load-system :speaker)
    
There is also a windows project you can compile to a dll.
But it only supports speech output and the dll is deprecated beacuse .NET assemly's are used now.    
    
License is based on GNU LESSER GENERAL PUBLIC LICENSE.
