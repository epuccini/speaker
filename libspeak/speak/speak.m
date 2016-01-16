//
//  speak.m
//  speak - c-interface for cffi to lisp
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import "speak.h"

////////////////////////////////////////////////
//
// Function to create Speaker instances
// This is useful for handling different speakers
// at once
//
void* make_speaker(char* speech)
{
    Speaker* speaker = [[Speaker alloc] initWithSpeach:speech];
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

////////////////////////////////////////////////
//
// Lisp callbacks can be registered here and
// will be called in Speaker-instance
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
// Function for fast setup speaking
// Only single speaker possible, but
// offering a simple c-interface
//
void init_with_speech(char* speech)
{
    synth =[[NSSpeechSynthesizer alloc] initWithVoice:
            [[NSString alloc] initWithCString:speech encoding: NSASCIIStringEncoding]];
}

void speak(char* text)
{
    [synth startSpeakingString:[[NSString alloc] initWithCString:text encoding: NSASCIIStringEncoding]];
}

void start_speaking_string(char* text)
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
