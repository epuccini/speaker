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
@synthesize done=_done;
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
        _done = NO;
        
        // start runloop
        //[self performSelectorInBackground:@selector(mainLoop) withObject:self];
    }
    return self;
}

- (void)mainLoop
{
    // Set up an autorelease pool here if not using garbage collection.
     // Add your sources or timers to the run loop and do any other setup.
    do
    {
        // Start the run loop but return after each source is handled.
        SInt32 result = CFRunLoopRunInMode(kCFRunLoopDefaultMode, 10, YES);
        
        // If a source explicitly stopped the run loop, or if there are no
        // sources or timers, go ahead and exit.
        if (result == kCFRunLoopRunStopped)// || (result == kCFRunLoopRunFinished))
            _done = YES;
        // Check for any other exit conditions here and set the
        // done variable as needed.
    }
    while (!_done);
    
    // Clean up code here. Be sure to release any allocated autorelease pools.
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
    [_synth startSpeakingString:text];
    // call when finished speaking
    //[_synth.delegate speechSynthesizer:_synth didFinishSpeaking:true];
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
