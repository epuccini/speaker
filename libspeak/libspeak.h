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
    LIBRARY_EXPORT void* make_speaker();
    LIBRARY_EXPORT void speak_with(void* speaker, char* text);
    LIBRARY_EXPORT void set_voice_with(void* speaker, int index);
    LIBRARY_EXPORT void cleanup_with(void* speaker);
    
    LIBRARY_EXPORT void register_will_speak_word_callback(void* speaker, wsw_callback cb);
    LIBRARY_EXPORT void register_will_speak_phoneme_callback(void* speaker, wsp_callback cb);
    LIBRARY_EXPORT void register_did_finish_speaking_callback(void* speaker, dfs_callback cb);
    
    LIBRARY_EXPORT void init_speaker();
    LIBRARY_EXPORT void speak(char* text);
    LIBRARY_EXPORT void set_voice(int index);
    LIBRARY_EXPORT unsigned int available_voices_count(void);
    LIBRARY_EXPORT void get_voice_name(unsigned int idx, char* pszOut);
    LIBRARY_EXPORT void cleanup(void);
#ifdef _WINDLL
}
#endif
