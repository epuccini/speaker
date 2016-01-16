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

- (void)registerWillSpeakWordCallback:(wsw_callback)cb
{
    will_speak_word_callback = cb;
}

- (void)registerWillSpeakPhonemeCallback:(wsp_callback)cb
{
    will_speak_phoneme_callback = cb;
}

- (void)registerDidFinishSpeakingCallback:(dfs_callback)cb
{
    did_finish_speaking_callback = cb;
}

- (IBAction)speakWithText:(NSString*)text
{
    // speak with speaker instance
    [synth startSpeakingString:text];
    // call when finished speaking
    //[synth.delegate speechSynthesizer:synth didFinishSpeaking:true];
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
            willSpeakWord:(NSRange)wordToSpeak
                 ofString:(NSString *)text
{
    NSLog(@"willSpeakWord %@", text);
    if (will_speak_word_callback != NULL)
    {
        char pszForeignText[1024];
        strcpy(pszForeignText, [text cStringUsingEncoding:[NSString defaultCStringEncoding]]);
        (*will_speak_word_callback)(pszForeignText);
    }
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
         willSpeakPhoneme:(short)phonemeOpcode
{
    NSLog(@"willSpeakPhoneme %d", phonemeOpcode);
    if (will_speak_phoneme_callback != NULL)
    {
        (*will_speak_phoneme_callback)(phonemeOpcode);
    }
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
        didFinishSpeaking:(BOOL)success
{
    NSLog(@"didFinishSpeaking %d", success);
    if (did_finish_speaking_callback != NULL)
    {
        (*did_finish_speaking_callback)();
    }
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
