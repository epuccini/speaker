//
//  speak.m
//  speak
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import "speak.h"

@implementation libspeak

void init_with_speech(char* speech)
{
    synth = [[NSSpeechSynthesizer alloc] init];
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
    const char* pszConst = [[[a objectAtIndex:i] name] cStringUsingEncoding:NSASCIIStringEncoding];
    strcpy(pszOut, pszConst);
}

@end



