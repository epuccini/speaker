//
//  mainloop.m
//  speak
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#import <Foundation/Foundation.h>

#include "custommainloop.h"
#include "speak.h"

@implementation CustomRunloop
-(id)init {
    id newInstance = [super init];
    if (newInstance) {
        NSLog(@"Creating timer...");
        _timer = [NSTimer scheduledTimerWithTimeInterval:1.0
                                                  target:self
                                                selector:@selector(onTick:)
                                                userInfo:nil
                                                 repeats:YES];
    }
    return newInstance;
}

-(void)onTick:(NSTimer *)aTimer {
    speak("Hello ticker, eins zwei drei vier fünf sechs sieben acht neun zehn Edward Puccini!");
    sleep(2);
}

@end