//
//  AccelDelegate.h
//  ioshello
//
//  Created by Sorel Mitra on 4/17/14.
//  Copyright (c) 2014 Sorel Mitra. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <CoreMotion/CoreMotion.h>

@interface Mover : NSObject

@property (weak) UIImageView *img;
@property (weak) UILabel *label;

@property (readonly) CMMotionManager *motionMgr;
@property double accelFactor;
@property double wallHitFactor;

- (void)start;
- (void)stop;

@end
