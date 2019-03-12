# speaker
speaker - Speech synthesis library for Common Lisp under Mac OSX

This speech synthesis library is a cffi interface to the Mac OSX Cocoa library. It enables speech-output and -recognition. 
Dependency is only cffi. For exaxmples you need "bordeaux-threads" and "trivial-main-thread".

First open the XCode project libspeak and compile the libspeak.dylib and copy the lib to /usr/local/lib.
Then cd into the speaker/src directory and load your favouorite lisp and type:

    (asdf:load-system :speaker-examples)
    (main)

If you just use the library then just type

    (asdf:load-system :speaker)
    
There is also a windows project you can compile to a dll.
But it only supports speech output and the dll is deprecated beacuse .NET assemly's are used now.    
    
License is based on GNU LESSER GENERAL PUBLIC LICENSE.
