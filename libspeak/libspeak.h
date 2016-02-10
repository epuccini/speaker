////////////////////////////////////////////////
//
//  speak.h
//  speak - c-interface for cffi to lisp
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////
#pragma once 

////////////////////////////////////////////////
// Callback types
//
typedef void(*wsw_callback)(char*);
typedef void(*wsp_callback)(short);
typedef void(*dfs_callback)(void);

typedef void(*drc_callback)(char*);

////////////////////////////////////////////////
// Basic c interface to wrap objective-c 
// for direct cffi access
//
#ifdef _WINDLL
	#define DLL_MODE_EXPORT
	#ifdef DLL_MODE_EXPORT	
		#define LIBRARY_EXPORT   __declspec( dllexport ) 
	#else
		#define LIBRARY_EXPORT   __declspec( dllimport ) 
	#endif
#else
    #define LIBRARY_EXPORT
#endif

#ifdef _WINDLL
extern "C"
{
#endif
    ///////////
    // Speaker
    //
    LIBRARY_EXPORT void init_speaker();
    LIBRARY_EXPORT void speak(char* text);
    LIBRARY_EXPORT void set_voice(int index);
    LIBRARY_EXPORT unsigned int available_voices_count(void);
    LIBRARY_EXPORT void get_voice_name(unsigned int idx, char* pszOut);
    LIBRARY_EXPORT void cleanup_speaker(void);
    LIBRARY_EXPORT void mainloop_speaker(void* speaker);
    LIBRARY_EXPORT bool is_speaking(void* speaker);
    ///////////////
    // Speaker OO
    //
    LIBRARY_EXPORT void* make_speaker();
    LIBRARY_EXPORT void speak_with(void* speaker, char* text);
    LIBRARY_EXPORT void set_voice_with(void* speaker, int index);
    LIBRARY_EXPORT void cleanup_with(void* speaker);
    
    LIBRARY_EXPORT void register_will_speak_word_callback(void* speaker, wsw_callback cb);
    LIBRARY_EXPORT void register_will_speak_phoneme_callback(void* speaker, wsp_callback cb);
    LIBRARY_EXPORT void register_did_finish_speaking_callback(void* speaker, dfs_callback cb);
    
    /////////////////
    // Recognizer OO
    //
    LIBRARY_EXPORT void* make_listener();
    LIBRARY_EXPORT void start_listening(void* listener);
    LIBRARY_EXPORT void stop_listening(void* listener);
    LIBRARY_EXPORT void add_command(void* listener, char*);
    LIBRARY_EXPORT void cleanup_listener(void* listener);
    LIBRARY_EXPORT void mainloop_listener(void* listener);
    LIBRARY_EXPORT bool is_listening(void* listener);
    
    LIBRARY_EXPORT void register_did_recognize_command_callback(void* listener, drc_callback cb);
    
    
    
#ifdef _WINDLL
}
#endif
