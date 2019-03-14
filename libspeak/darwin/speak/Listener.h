////////////////////////////////////////////////
//
//  Listener.h
//  speak
//
//  Created by Edward Puccini on 08.02.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////


#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>

#include "libspeak.h"

@interface Listener : NSObject<NSSpeechRecognizerDelegate>
{
    NSSpeechRecognizer* _speechRecognizer;
    NSMutableArray * _commands;
    NSMutableDictionary * _command_dispatch;
    NSTimer *_timer;
    NSThread* _mainloopThread;
    
    drc_callback did_recognize_command_callback;
}

- (id)init;
- (void)startListening;
- (void)stopListening;
- (void)addCommand:(NSString*)command_string;
- (void)runLoop;
- (void)dummyThread;
- (bool)isListening;

- (void)registerDidRecognizeCommand:(drc_callback)cb;

- (void)speechRecognizer:(NSSpeechRecognizer *)sender
     didRecognizeCommand:(NSString *)command;

@property (nonatomic, retain)NSSpeechRecognizer* speechRecognizer;
@property (nonatomic, retain)NSMutableArray* commands;
@property (nonatomic, retain)NSMutableDictionary* command_dispatch;
@property (atomic, retain)NSThread* mainloopThread;

@end
