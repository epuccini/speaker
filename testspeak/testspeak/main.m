//
//  main.m
//  testspeak
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#include "speak.h"

void loop_thread();


@interface MyClass : NSObject
@property NSTimer *timer;
-(id)init;
-(void)loop_thread;
-(void)onTick:(NSTimer *)aTimer;
@end

@implementation MyClass
-(id)init {
    id newInstance = [super init];
    if (newInstance) {
//        NSLog(@"Creating timer...");
//        _timer = [NSTimer scheduledTimerWithTimeInterval:1.0
//                                                  target:self
//                                                selector:@selector(onTick:)
//                                                userInfo:nil
//                                                 repeats:YES];
        NSThread* thread = [[NSThread alloc]initWithTarget:self selector:@selector(loop_thread) object:nil];
        [thread start];

    }
    return newInstance;
}

static libspeak *ls;

-(void)loop_thread
{
    // start mainloop for delegates
    [[NSRunLoop currentRunLoop] run];
}

-(void)onTick:(NSTimer *)aTimer {
            [ls speak:ls];
            sleep(2);
}
@end


int main() {
     @autoreleasepool {
        libspeak* ls = [[libspeak alloc] init];
        //init_with_speech(pszVoiceAlex);
        // speak("Hello working");
         [ls speak:@"TEST"];
         //sleep(5);
        //[[NSRunLoop currentRunLoop] run];
    }
    return 0;
}
//
//int main(int argc, const char * argv[]) {
//    @autoreleasepool {
//        // insert code here...
//        NSLog(@"Hello, World!");
//        
//        libspeak *ls = [[libspeak alloc] init];
//        [ls speak:ls];
//        sleep(2);
//    }
//    return 0;
//}
