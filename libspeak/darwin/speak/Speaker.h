////////////////////////////////////////////////
//
//  Speaker.h
//  Container for NSSpeechSynthesizer
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////

#import <Foundation/Foundation.h>

#import <AppKit/AppKit.h>

#include "libspeak.h"

////////////////////////////////////////////////

@interface Speaker : NSObject<NSSpeechSynthesizerDelegate> {
    NSSpeechSynthesizer* _synth;
    unsigned int _voiceid;
    NSTimer *_timer;
    
    wsw_callback will_speak_word_callback;
    wsp_callback will_speak_phoneme_callback;
    dfs_callback did_finish_speaking_callback;
}

@property (nonatomic, retain)NSSpeechSynthesizer* synth;
@property (nonatomic)unsigned int voiceid;
@property (nonatomic, retain)NSTimer* timer;

- (id)init;
- (void)runLoop;
- (void)speakWithText:(NSString*)text;
- (bool)isSpeaking;

- (void)registerWillSpeakWordCallback:(wsw_callback)cb;
- (void)registerWillSpeakPhonemeCallback:(wsp_callback)cb;
- (void)registerDidFinishSpeakingCallback:(dfs_callback)cb;

- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
            willSpeakWord:(NSRange)wordToSpeak
                 ofString:(NSString *)text;
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
         willSpeakPhoneme:(short)phonemeOpcode;
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
        didFinishSpeaking:(BOOL)success;
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
 didEncounterErrorAtIndex:(NSUInteger)characterIndex
                 ofString:(NSString *)string
                  message:(NSString *)message NS_AVAILABLE_MAC(10_5);
- (void)speechSynthesizer:(NSSpeechSynthesizer *)sender
  didEncounterSyncMessage:(NSString *)message NS_AVAILABLE_MAC(10_5);

@end

