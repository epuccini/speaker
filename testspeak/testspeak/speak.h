//
//  libspeak.h
//  libspeak
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

@interface libspeak : NSObject<NSSpeechSynthesizerDelegate> {
    NSSpeechSynthesizer* synth;
}

@property (nonatomic, retain)NSSpeechSynthesizer* synth;
//@property (nonatomic, strong)id<NSSpeechSynthesizerDelegate> delegate;

- (id)init;
- (void)awakeFromNib;
- (IBAction)speak:(id)sender;
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


static libspeak *obj = NULL;
static char* pszVoiceAlex = "com.apple.speech.synthesis.voice.Alex";

void init_with_speech(char* speech);
void speak(char* text);
void start_speaking_string(char* text);
void set_voice(int index);
unsigned int available_voices_count(void);
void get_voice_name(unsigned int idx, char* pszOut);
