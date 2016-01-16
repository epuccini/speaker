//
//  speak.h
//  speak - c-interface for cffi to lisp
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>

#include "Speaker.h"

static NSSpeechSynthesizer *synth = NULL;
static char* pszVoiceAlex = "com.apple.speech.synthesis.voice.Alex";

// Basic objective-c interface

void* make_speaker(char* speech);
void speak_with(void* speaker, char* text);
void set_voice_with(void* speaker, int index);

// Basic c interface

void init_with_speech(char* speech);
void loop_start();
void speak(char* text);
void start_speaking_string(char* text);
void set_voice(int index);
unsigned int available_voices_count(void);
void get_voice_name(unsigned int idx, char* pszOut);
