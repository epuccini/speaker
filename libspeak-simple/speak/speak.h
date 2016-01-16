//
//  libspeak.h
//  libspeak
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

static NSSpeechSynthesizer *synth = NULL;
static char* pszVoiceAlex = "com.apple.speech.synthesis.voice.Alex";

void init_with_speech(char* speech);
void speak(char* text);
void start_speaking_string(char* text);
void set_voice(int index);
unsigned int available_voices_count(void);
void get_voice_name(unsigned int idx, char* pszOut);

@interface libspeak : NSObject

@end
