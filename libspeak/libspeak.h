////////////////////////////////////////////////
//
//  speak.h
//  speak - c-interface for cffi to lisp
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////

////////////////////////////////////////////////

const char* pszVoiceAlex = "com.apple.speech.synthesis.voice.Alex";

// Basic objective-c interface

void* make_speaker(char* speech);
void speak_with(void* speaker, char* text);
void set_voice_with(void* speaker, int index);

void register_will_speak_word_callback(void* speaker, wsw_callback cb);
void register_will_speak_phoneme_callback(void* speaker, wsp_callback cb);
void register_did_finish_speaking_callback(void* speaker, dfs_callback cb);

// Basic c interface

void init_with_speech(char* speech);
void loop_start();
void speak(char* text);
void start_speaking_string(char* text);
void set_voice(int index);
unsigned int available_voices_count(void);
void get_voice_name(unsigned int idx, char* pszOut);
