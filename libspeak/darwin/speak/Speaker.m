////////////////////////////////////////////////
//
//  Speaker.h
//  Speaker - container for NSSpeechSynthesizer
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////

#import "Speaker.h"

////////////////////////////////////////////////

@implementation Speaker

@synthesize synth=_synth;
@synthesize voiceid=_voiceid;
@synthesize timer=_timer;

- (id)init {
    self = [super init];
    if (self) {
        // create speech-synth
        _synth = [[NSSpeechSynthesizer alloc] initWithVoice:
                    [[NSString alloc] initWithCString:"com.apple.speech.synthesis.voice.anna"
                                             encoding: NSASCIIStringEncoding]];
            
        //synth is an ivar
        [_synth setDelegate:self];
        
        _voiceid = 6;
    }
    return self;
}

- (bool)isSpeaking
{
    if (_synth) return [_synth isSpeaking];
    return false;
}

- (void)runLoop;
{
    CFRunLoopRunInMode(kCFRunLoopDefaultMode, 0, YES);
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

- (void)speakWithText:(NSString*)text
{
    // speak with speaker instance
    [_synth startSpeakingString:text];
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
            willSpeakWord:(NSRange)wordToSpeak
                 ofString:(NSString *)text
{
#ifdef __DEBUG
    NSLog(@"willSpeakWord %@", text);
#endif
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
#ifdef __DEBUG
    NSLog(@"willSpeakPhoneme %d", phonemeOpcode);
#endif
    if (will_speak_phoneme_callback != NULL)
    {
        (*will_speak_phoneme_callback)(phonemeOpcode);
    }
}

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
        didFinishSpeaking:(BOOL)success
{
#ifdef __DEBUG
    NSLog(@"didFinishSpeaking %d", success);
#endif
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
