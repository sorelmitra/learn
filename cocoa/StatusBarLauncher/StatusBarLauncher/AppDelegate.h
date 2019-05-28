//
//  AppDelegate.h
//  StatusBarLauncher
//
//  Created by Sorel Mitra on 02/05/2014.
//  Copyright (c) 2014 Sorel Mitra. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (weak) IBOutlet NSMenu *statusMenu;
@property (weak) IBOutlet NSMenuItem *statusMenuLaunch;
@property (weak) IBOutlet NSMenuItem *statusMenuQuit;
@property (weak) IBOutlet NSTextFieldCell *uiNotifLabel;
@property (unsafe_unretained) IBOutlet NSPanel *uiNotifPanel;

@property (assign) IBOutlet NSWindow *window;

@property (strong) NSString *appToLaunch;
@property (strong) NSString *agentName;

- (IBAction)onStatusMenuLaunchClicked:(id)sender;
- (IBAction)onStatusMenuQuitClicked:(id)sender;

@end
