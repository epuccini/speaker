//
//  Speaker.h
//  Container for NSSpeechSynthesizer
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>

#import <AppKit/AppKit.h>

typedef void(*wsw_callback)(char*);
typedef void(*wsp_callback)(short);
typedef void(*dfs_callback)(void);

@interface Speaker : NSObject<NSSpeechSynthesizerDelegate> {
    NSSpeechSynthesizer* synth;
    unsigned int voiceid;
    wsw_callback will_speak_word_callback;
    wsp_callback will_speak_phoneme_callback;
    dfs_callback did_finish_speaking_callback;
}

@property (nonatomic, retain)NSSpeechSynthesizer* synth;
@property (nonatomic)unsigned int voiceid;

- (id)initWithSpeach:(char*)speech;
- (IBAction)speakWithText:(NSString*)text;

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

