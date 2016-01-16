//
//  custommainloop.h
//  speak
//
//  Created by Edward Puccini on 15.01.16.
//  Copyright (c) 2016 Edward Puccini. All rights reserved.
//

#ifndef speak_custommainloop_h
#define speak_custommainloop_h

@interface CustomRunloop : NSObject

@property NSTimer *timer;

-(id)init;
-(void)onTick:(NSTimer *)aTimer;

@end


#endif
