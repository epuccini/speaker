//
//  speak.m
//  testspeak
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "speak.h"

@implementation libspeak

@synthesize synth;

- (id)init {
    self = [super init];
    if (self) {
        
        synth =[[NSSpeechSynthesizer alloc] init]; //start with default voice
        
        //synth is an ivar
        [synth setDelegate:self];
        [synth.delegate speechSynthesizer:synth didFinishSpeaking:true];
    }
    return self;
}

- (void)awakeFromNib
{
    [synth setDelegate:self];
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
            willSpeakWord:(NSRange)wordToSpeak
                 ofString:(NSString *)text
{
    
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
         willSpeakPhoneme:(short)phonemeOpcode
{
    
}
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
        didFinishSpeaking:(BOOL)success
{
    
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
 didEncounterErrorAtIndex:(NSUInteger)characterIndex ofString:(NSString *)string
                  message:(NSString *)message NS_AVAILABLE_MAC(10_5)
{
    
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
  didEncounterSyncMessage:(NSString *)message NS_AVAILABLE_MAC(10_5)
{
    
}

- (IBAction)speak:(NSString*)text
{
    NSString *voiceID = [[NSSpeechSynthesizer availableVoices] objectAtIndex:0];
    
    [synth setVoice:voiceID];
    [synth startSpeakingString:text];
}

// ---------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------

void init_with_speech(char* speech)
{
    obj = [[libspeak alloc] init];
}

void speak(char* text)
{
    [[obj synth] startSpeakingString:[[NSString alloc] initWithCString:text encoding: NSASCIIStringEncoding]];
}

void start_speaking_string(char* text)
{
    [[obj synth] startSpeakingString:[[NSString alloc] initWithCString:text encoding: NSASCIIStringEncoding]];
}

void set_voice(int index)
{
    NSString *voiceID =[[NSSpeechSynthesizer availableVoices] objectAtIndex:index];
    [[obj synth] setVoice:voiceID];
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


@end
