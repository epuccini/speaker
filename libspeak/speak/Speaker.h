//
//  Speaker.h
//  Container for NSSpeechSynthesizer
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>

#import <AppKit/AppKit.h>

@interface Speaker : NSObject<NSSpeechSynthesizerDelegate> {
    NSSpeechSynthesizer* synth;
    unsigned int voiceid;
}

@property (nonatomic, retain)NSSpeechSynthesizer* synth;
@property (nonatomic)unsigned int voiceid;

- (id)initWithSpeach:(char*)speech;
- (IBAction)speakWithText:(NSString*)text;

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

