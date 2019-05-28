//
//  AppDelegate.m
//  HelloWorld
//
//  Created by Sorel Mitra on 3/24/14.
//
//

#import "AppDelegate.h"

#import "Track.h"

@implementation AppDelegate

@synthesize textField;
@synthesize slider;

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    Track *aTrack = [[Track alloc] init];
    [self setTrack:aTrack];
    [self updateUserInterface];    
    [[self directionCombo] addItemWithObjectValue:@"hello"];
}

- (IBAction)mute:(id)sender {
    [self.track setVolume:0.0];
    [self updateUserInterface];
}

- (IBAction)takeFloatValueForVolumeFrom:(id)sender {
    float newValue = [sender floatValue];
    [self.track setVolume:newValue];
    [self updateUserInterface];
}

- (void)updateUserInterface {
    float volume = [self.track volume];
    [self.textField setFloatValue:volume];
    [self.slider setFloatValue:volume];
}

@end
