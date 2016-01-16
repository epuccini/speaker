//
//  Speaker.h
//  Speaker - container for NSSpeechSynthesizer
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//

#import "Speaker.h"

@implementation Speaker

@synthesize synth;
@synthesize voiceid;

- (id)initWithSpeach:(char*)speech {
    self = [super init];
    if (self) {
        // create speech-synth
        synth = [[NSSpeechSynthesizer alloc] initWithVoice:
                 [[NSString alloc] initWithCString:speech encoding: NSASCIIStringEncoding]];
            
        //synth is an ivar
        [synth setDelegate:self];

        voiceid = 6;
    }
    return self;
}

- (IBAction)speakWithText:(NSString*)text
{
    NSString *voiceID = [[NSSpeechSynthesizer availableVoices] objectAtIndex:voiceid];
    
    [synth setVoice:voiceID];
    [synth startSpeakingString:text];
    [synth.delegate speechSynthesizer:synth didFinishSpeaking:true];
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
            willSpeakWord:(NSRange)wordToSpeak
                 ofString:(NSString *)text
{
    NSLog(@"willSpeakWord %@", text);
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
         willSpeakPhoneme:(short)phonemeOpcode
{
    NSLog(@"willSpeakPhoneme %d", phonemeOpcode);
}
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
        didFinishSpeaking:(BOOL)success
{
    NSLog(@"didFinishSpeaking %d", success);
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

@end
