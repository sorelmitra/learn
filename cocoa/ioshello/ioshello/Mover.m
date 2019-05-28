//
//  Accel.m
//  ioshello
//
//  Created by Sorel Mitra on 4/17/14.
//  Copyright (c) 2014 Sorel Mitra. All rights reserved.
//

#import <CoreMotion/CoreMotion.h>
#import <CoreGraphics/CoreGraphics.h>

#import "Mover.h"


////////////////////////////////////////////////////////////////////////

@interface AxisMovementComputator : NSObject
{
    double secsLastCalled;
    double accelLastCalled;
    double velocityInitial;
}

- (void)setAccelSign:(int)sign;
- (double)computeDistanceWithAccel:(double)acc;
- (void)wallHit;

@property (readonly) int accelSign;
@property double accelFactor;
@property double wallHitFactor;

@end

@implementation AxisMovementComputator

- (id)init {
    self = [super init];
    if (self) {
        secsLastCalled = 0;
        accelLastCalled = 0;
        velocityInitial = 0;
        _accelSign = 1;
    }
    return self;
}

- (void)setAccelSign:(int)sign
{
    if (sign < 0)
    {
        _accelSign = -1;
    }
    else
    {
        _accelSign = 1;
    }
}

- (double)computeDistanceWithAccel:(double)acc
{
    // Constraint: acc is expressed in m / s^2
    
    double secsNow = [[NSDate date] timeIntervalSince1970];
    if (secsLastCalled == 0)
    {
        secsLastCalled = secsNow;
        return 0;
    }
    
    double secsDiff = secsNow - secsLastCalled;
    if (secsDiff <= 0)
    {
        return 0;
    }
    
    double velocityFinal = velocityInitial + [self accelSign] * acc * [self accelFactor] * secsDiff;
    double distance = (velocityInitial + velocityFinal) * secsDiff / 2;
    
    secsLastCalled = secsNow;
    accelLastCalled = acc;
    velocityInitial = velocityFinal;
    
    return distance;
}

- (void)wallHit
{
    velocityInitial *= -1 * [self wallHitFactor];
}

@end


////////////////////////////////////////////////////////////////////////

@interface Mover ()
{
    CMMotionManager *_motionMgr;
}

@property AxisMovementComputator *xMover;
@property AxisMovementComputator *yMover;

- (void)moveImgWithAccel:(CMAcceleration)acc;
- (void)moveRect:(CGRect *)imgFrame withAccel:(CMAcceleration *)acc;

@end

@implementation Mover

- (id)init {
    self = [super init];
    if (self) {
        _accelFactor = 1000;
        _wallHitFactor = 0.5;
        _xMover = [[AxisMovementComputator alloc] init];
        _yMover = [[AxisMovementComputator alloc] init];
        [[self xMover] setAccelFactor:[self accelFactor]];
        [[self xMover] setWallHitFactor:[self wallHitFactor]];
        [[self yMover] setAccelFactor:[self accelFactor]];
        [[self yMover] setWallHitFactor:[self wallHitFactor]];
        [[self yMover] setAccelSign:-1];
    }
    return self;
}

- (CMMotionManager *)motionMgr
{
    if (!_motionMgr)
    {
        _motionMgr = [[CMMotionManager alloc] init];
    }
    return _motionMgr;
}

- (void)moveRect:(CGRect *)imgFrame withAccel:(CMAcceleration *)acc
{
    CGRect screenBounds = [[UIScreen mainScreen] bounds];
    
    double newX = imgFrame->origin.x + [[self xMover] computeDistanceWithAccel:acc->x];
    double newY = imgFrame->origin.y + [[self yMover] computeDistanceWithAccel:acc->y];
    
    if (newX + imgFrame->size.width > screenBounds.origin.x + screenBounds.size.width)
    {
        newX = screenBounds.origin.x + screenBounds.size.width - imgFrame->size.width;
        [[self xMover] wallHit];
    }
    if (newY + imgFrame->size.height > screenBounds.origin.y + screenBounds.size.height)
    {
        newY = screenBounds.origin.y + screenBounds.size.height - imgFrame->size.height;
        [[self yMover] wallHit];
    }
    if (newX < screenBounds.origin.x)
    {
        newX = screenBounds.origin.x;
        [[self xMover] wallHit];
    }
    if (newY < screenBounds.origin.y)
    {
        newY = screenBounds.origin.y;
        [[self yMover] wallHit];
    }
    
    imgFrame->origin.x = newX;
    imgFrame->origin.y = newY;
}

- (void)moveImgWithAccel:(CMAcceleration)acc
{
    CGRect imgFrame = [self.img frame];
    [self moveRect:&imgFrame withAccel:&acc];
    
    [self.img setFrame:imgFrame];
}

- (void)start
{
    NSOperationQueue *queue = [[NSOperationQueue alloc] init];
    [self.motionMgr setAccelerometerUpdateInterval:0.03];
    [self.label setText:@"Starting accel updates"];
    [self.motionMgr
     startAccelerometerUpdatesToQueue:queue
     withHandler:^(CMAccelerometerData *accelerometerData, NSError *error) {
        CMAcceleration acc = [accelerometerData acceleration];
        NSString *accelValues = [NSString stringWithFormat:@"accel x=%0.2f y=%0.2f z=%0.2f", acc.x, acc.y, acc.z];
        [[NSOperationQueue mainQueue] addOperationWithBlock:^{
            // Any updates to the GUI must be done on the main queue
            // You could also do it with dispatch_async(dispatch_get_main_queue(), ^{...});
            [self.label setText:accelValues];
            [self moveImgWithAccel:acc];
         }];
    }];
}

- (void)stop
{
    [self.motionMgr stopAccelerometerUpdates];
}

@end
