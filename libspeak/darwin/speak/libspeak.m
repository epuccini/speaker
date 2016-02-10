////////////////////////////////////////////////
//
//  speak.c
//  speak - c-interface for cffi to lisp
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////

#import <Foundation/Foundation.h>

#include "Speaker.h"
#include "Listener.h"
#include "libspeak.h"

////////////////////////////////////////////////

// static object pointer
static NSSpeechSynthesizer *synth = NULL;
const char* pszVoiceAlex = "com.apple.speech.synthesis.voice.Alex";

////////////////////////////////////////////////
//
// Function for fast setup speaking
// Only single speaker possible, but
// offering a simple c-interface
//
void init_speaker()
{
    synth =[[NSSpeechSynthesizer alloc] initWithVoice:
            [[NSString alloc] initWithCString:"com.apple.speech.synthesis.voice.anna"
                                     encoding: NSASCIIStringEncoding]];
}

void speak(char* text)
{
    [synth startSpeakingString:[[NSString alloc] initWithCString:text encoding: NSASCIIStringEncoding]];
}

void set_voice(int index)
{
    NSString *voiceID =[[NSSpeechSynthesizer availableVoices] objectAtIndex:index];
    [synth setVoice:voiceID];
}

unsigned int available_voices_count(void)
{
    NSArray *a = [NSSpeechSynthesizer availableVoices];
    return (unsigned int)[a count];
}

void get_voice_name(unsigned int i, char* pszOut)
{
    NSArray *a = [NSSpeechSynthesizer availableVoices];
    const char* pszConst = [[a objectAtIndex:i] cStringUsingEncoding:NSASCIIStringEncoding];
    strcpy(pszOut, pszConst);
}

void cleanup_speaker(void)
{
    synth = NULL;
}

////////////////////////////////////////////////
//
// Function to create Speaker instances
// This is useful for handling different speakers
// at once
//
void* make_speaker()
{
    Speaker* speaker = [[Speaker alloc] init];
    return (__bridge void*)speaker;
}

void speak_with(void* speaker, char* text)
{
    [((__bridge Speaker*)speaker) speakWithText:[[NSString alloc] initWithCString:text
                                                                         encoding: NSASCIIStringEncoding]];
}

void set_voice_with(void* speaker, int index)
{
    NSString *voiceID =[[NSSpeechSynthesizer availableVoices] objectAtIndex:index];
    [[((__bridge Speaker*)speaker) synth] setVoice:voiceID];
}

bool is_speaking(void* speaker)
{
    return [((__bridge Speaker*)speaker) isSpeaking];
}

void cleanup_with(void* speaker)
{
    speaker = NULL;
}

void mainloop_speaker(void* speaker)
{
    [(__bridge Speaker*)speaker runLoop];
}

////////////////////////////////////////////////
//
// Lisp callbacks can be registered here and
// will be called in Speaker-instance*previous-readtables*
//
void register_will_speak_word_callback(void* speaker, wsw_callback cb)
{
    [((__bridge Speaker*)speaker) registerWillSpeakWordCallback:cb];
}

void register_will_speak_phoneme_callback(void* speaker, wsp_callback cb)
{
    [((__bridge Speaker*)speaker) registerWillSpeakPhonemeCallback:cb];
}

void register_did_finish_speaking_callback(void* speaker, dfs_callback cb)
{
    [((__bridge Speaker*)speaker) registerDidFinishSpeakingCallback:cb];
}

////////////////////////////////////////////////
//
// Function for fast setup listening
// Only single listener possible, but
// offering a simple c-interface
//
void* make_listener()
{
    Listener *listener = [[Listener alloc] init];
    return (__bridge void *)(listener);
}

void add_command(void* listener, char* command)
{
    NSString* command_string = [[NSString alloc] initWithCString:command encoding: NSASCIIStringEncoding];
    [(__bridge Listener*)listener addCommand:command_string];
}

void start_listening(void* listener)
{
    [(__bridge Listener*)listener startListening];
}

void stop_listening(void* listener)
{
    [(__bridge Listener*)listener stopListening];
}

void cleanup_listener(void* listener)
{
   listener = NULL;
}

void mainloop_listener(void* listener)
{
    [(__bridge Listener*)listener runLoop];
}

bool is_listening(void* listener)
{
    return [((__bridge Listener*)listener) isListening];
}

////////////////////////////////////////////////
//
// Lisp callbacks can be registered here and
// will be called in Listener-instance
//
void register_did_recognize_command_callback(void* listener, drc_callback cb)
{
    [((__bridge Listener*)listener)
        registerDidRecognizeCommand:cb];
}
