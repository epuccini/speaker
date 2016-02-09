////////////////////////////////////////////////
//
//  Listener.m
//  speak
//
//  Created by Edward Puccini on 08.02.16.
//  Copyright © 2016 Edward Puccini. All rights reserved.
//
////////////////////////////////////////////////

#import "Listener.h"

////////////////////////////////////////////////

@implementation Listener

@synthesize commands=_commands;
@synthesize command_dispatch=_command_dispatch;
@synthesize speechRecognizer=_speechRecognizer;
@synthesize done=_done;
@synthesize mainloopThread=_mainloopThread;

- (id)init
{
    self = [super init];
    _speechRecognizer = [[NSSpeechRecognizer alloc] init];
    _commands = [[NSMutableArray alloc] init];
    
    [_speechRecognizer setCommands:[_commands copy]];
    [_speechRecognizer setDelegate:self];
    [_speechRecognizer setListensInForegroundOnly:NO];
    [_speechRecognizer setBlocksOtherRecognizers:YES];
    _done = NO;
    
    // set delegate for callbacks
    [_speechRecognizer setDelegate:self];
    
    // enable mutlithreading
    @autoreleasepool {
        [NSThread detachNewThreadSelector:@selector(dummyThread) toTarget:self withObject:nil];
    }
    return self;
}

- (void)runLoop
{
    // start runloop
    @autoreleasepool {
//        [self performSelectorOnMainThread:@selector(runLoopThread) withObject:nil waitUntilDone:YES];
        [self performSelectorInBackground:@selector(runLoopCallThread) withObject:self];
//        [NSThread detachNewThreadSelector:@selector(runLoopThread) toTarget:self withObject:nil];
//        _mainloopThread = [[NSThread alloc] initWithTarget:self
//                                                  selector:@selector(mainLoopThread)
//                                                    object:nil];
//        
//        [_mainloopThread start];  // Actually create the thread
    }
}

- (void)runLoopThread
{
    // Add your sources or timers to the run loop and do any other setup.
    do
    {
        // Start the run loop but return after each source is handled.
        SInt32 result = CFRunLoopRunInMode(kCFRunLoopDefaultMode, 1, YES);
        
        // If a source explicitly stopped the run loop, or if there are no
        // sources or timers, go ahead and exit.
        if ((result == kCFRunLoopRunStopped) || (result == kCFRunLoopRunFinished))
            _done = YES;
        // Check for any other exit conditions here and set the
        // done variable as needed.
    }
    while (!_done);
    
    // Clean up code here. Be sure to release any allocated autorelease pools.
    _mainloopThread = NULL;
}

- (void)runLoopCallThread
{
    // Start the run loop but return after each source is handled.
    SInt32 result = CFRunLoopRunInMode(kCFRunLoopDefaultMode, 1, YES);
}

- (void)stopMainLoopThread
{
    _done = YES;
}

- (void)dummyThread
{
}

- (void)speechRecognizer:(NSSpeechRecognizer *)sender
     didRecognizeCommand:(NSString *)command
{
#ifdef __DEBUG
    NSLog(@"Speech recognized...");
#endif
    char pszForeignText[1024];
    strcpy(pszForeignText, [command cStringUsingEncoding:[NSString defaultCStringEncoding]]);
    (*did_recognize_command_callback)(pszForeignText);
}

- (void)registerDidRecognizeCommand:(drc_callback)cb
{
    did_recognize_command_callback = cb;
}

- (void)addCommand:(NSString*)command_string
{
    [_commands addObject:command_string];
    [_speechRecognizer setCommands:[_commands copy]];
}

- (void)startListening
{
    [_speechRecognizer startListening];
}

- (void)stopListening
{
    [_speechRecognizer stopListening];
}

@end
