//
//  AppDelegate.m
//  StatusBarLauncher
//
//  Created by Sorel Mitra on 02/05/2014.
//  Copyright (c) 2014 Sorel Mitra. All rights reserved.
//

#import "AppDelegate.h"

@interface AppDelegate ()
{
    NSStatusItem *_statusItem;
    NSRunningApplication *_runningApp;
    NSURL *libDir;
    NSURL *appSupportdir;
    NSURL *recentsFile;
    dispatch_queue_t queue;
    dispatch_source_t source;
}

@property (strong, readonly) NSStatusItem *statusItem;

@end

@implementation AppDelegate

- (void)createStatusBarItemWithTitle:(NSString *)title andMenu:(NSMenu *)menu
{
    [[self statusItem] setTitle:title];
    [[self statusItem] setMenu:menu];
    [[self statusItem] setHighlightMode:true];
}

- (void)displayErrorMessage:(NSString *)msg
{
    NSAlert *alert = [[NSAlert alloc] init];
    [alert addButtonWithTitle:@"OK"];
    [alert setMessageText:msg];
    [alert runModal];
}

- (NSRunningApplication *)getOurRunningApp:(NSNotification *)notification
{
    NSRunningApplication *app = [[notification userInfo] objectForKey:NSWorkspaceApplicationKey];
    NSString *appName = [app localizedName];
    
    NSError *error = NULL;
    NSRegularExpression *regex =
        [NSRegularExpression regularExpressionWithPattern:@"one-x\\s*comm"
                                                  options:NSRegularExpressionCaseInsensitive
                                                    error:&error];
    NSRange rng = {0, [appName length]};
    NSTextCheckingResult *res = [regex firstMatchInString:appName
                                                  options:0
                                                    range:rng];
    unsigned long matchLen = [res range].length;
    NSLog(@"appName %s, match len %lu", [appName UTF8String], matchLen);
    
    // NSLog(@"app name regex range: %d, %d", (int)[res range].location, (int)[res range].length);
    if (matchLen < 9)
    {
        return nil;
    }
    
    return app;
}

- (void)observeNotifWillLaunchApp
{
    NSWorkspace *sharedWs = [NSWorkspace sharedWorkspace];
    NSNotificationCenter *wsNotif = [sharedWs notificationCenter];
    [wsNotif addObserverForName:@"NSWorkspaceWillLaunchApplicationNotification"
                         object:sharedWs
                          queue:[[NSOperationQueue alloc] init]
                     usingBlock:^(NSNotification *notification) {
                         if ([self getOurRunningApp:notification] == nil) {
                             return;
                         }
                         NSUserNotification *userNotif = [[NSUserNotification alloc] init];
                         userNotif.title = [self agentName];
                         userNotif.informativeText = [NSString stringWithFormat:@"Starting %s", [[self appToLaunch] UTF8String]];
                         //userNotif.soundName = NSUserNotificationDefaultSoundName;
                         [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:userNotif];
                     }];
}

- (void)observeNotifDidLaunchApp
{
    NSWorkspace *sharedWs = [NSWorkspace sharedWorkspace];
    NSNotificationCenter *wsNotif = [sharedWs notificationCenter];
    [wsNotif addObserverForName:@"NSWorkspaceDidLaunchApplicationNotification"
                         object:sharedWs
                          queue:[[NSOperationQueue alloc] init]
                     usingBlock:^(NSNotification *notification) {
                         _runningApp = [self getOurRunningApp:notification];
                         if (_runningApp == nil)
                         {
                             return;
                         }
                         NSUserNotification *userNotif = [[NSUserNotification alloc] init];
                         userNotif.title = [self agentName];
                         userNotif.informativeText = [NSString stringWithFormat:@"Started %s", [[self appToLaunch] UTF8String]];
                         [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:userNotif];
                     }];
}

- (void)registerForAppLifeCycleEvents
{
    [self observeNotifWillLaunchApp];
    [self observeNotifDidLaunchApp];
}

- (void)quitAgent
{
    NSUserNotification *userNotif = [[NSUserNotification alloc] init];
    userNotif.title = [self agentName];
    userNotif.informativeText = [NSString stringWithFormat:@"Stopped %s", [[self agentName] UTF8String]];
    [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:userNotif];
    
    [[NSApplication sharedApplication] terminate:nil];
}

- (void)monitorURLChanges:(NSURL *)url withEventHandlerBlock:(void(^)(void))eventHandlerBlock withCancelHandlerBlock:(void(^)(void))cancelHandlerBlock
{
    NSString *path = [url path];
    NSLog(@"Starting monitoring  path: %s", [path UTF8String]);
    
    int fildes = open([path UTF8String], O_RDONLY);
    if (fildes == -1)
    {
        NSLog(@"Could not open %s for monitoring: %s", [path UTF8String], strerror(errno));
        return;
    }
    
    queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    source = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE, fildes,
                                                      DISPATCH_VNODE_DELETE | DISPATCH_VNODE_WRITE | DISPATCH_VNODE_EXTEND | DISPATCH_VNODE_ATTRIB | DISPATCH_VNODE_LINK | DISPATCH_VNODE_RENAME | DISPATCH_VNODE_REVOKE,
                                                      queue);
    if (source == NULL)
    {
        NSLog(@"Could not create dispatch source for fd %d", fildes);
        return;
    }
    
    dispatch_source_set_event_handler(source, eventHandlerBlock);
    dispatch_source_set_cancel_handler(source, cancelHandlerBlock);
    dispatch_resume(source);
    
    // sometime later
    // dispatch_source_cancel(source);
}

- (void)handleRecentsFileChanges
{
    [self monitorURLChanges:recentsFile
      withEventHandlerBlock:
     ^
     {
         unsigned long srcData = dispatch_source_get_data(source);
         NSLog(@"Recents file changed, flags are 0x%08lx", srcData);
     }
     withCancelHandlerBlock:
     ^
     {
         NSLog(@"Canceled recents file monitoring TODO");
     }];
}

- (NSStatusItem *)statusItem
{
    if (_statusItem == nil)
    {
        _statusItem = [[NSStatusBar systemStatusBar]
                                    statusItemWithLength:NSVariableStatusItemLength];
    }
    return _statusItem;
}

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
    _statusItem = nil;
    [self setAppToLaunch:@"/Users/smitra/w/asc/MacOSXClient/build/Debug/Avaya one-X Communicator.app"];
    [self setAgentName:@"Avaya SIP Communicator Agent"];
    [[self statusMenuLaunch] setTitle:[self appToLaunch]];
    [self createStatusBarItemWithTitle:@"1XC" andMenu:[self statusMenu]];
    [self registerForAppLifeCycleEvents];
    
    libDir = [[[NSFileManager defaultManager] URLsForDirectory:NSAllLibrariesDirectory inDomains:NSUserDomainMask] objectAtIndex:0];
    appSupportdir = [[libDir URLByAppendingPathComponent:@"Application Support"] URLByAppendingPathComponent:@"one-X Communicator"];
    recentsFile = [appSupportdir URLByAppendingPathComponent:@"recents.sqlite"];
    
    [self handleRecentsFileChanges];
    
    NSUserNotification *userNotif = [[NSUserNotification alloc] init];
    userNotif.title = [self agentName];
    userNotif.informativeText = [NSString stringWithFormat:@"Started %s", [[self agentName] UTF8String]];
    [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:userNotif];
}

- (IBAction)onStatusMenuLaunchClicked:(id)sender
{
    BOOL result = [[NSWorkspace sharedWorkspace] launchApplication:[self appToLaunch]];
    if (!result)
    {
        [self displayErrorMessage:[NSString stringWithFormat:@"Could not launch application with name %s", [[self appToLaunch] UTF8String]]];
    }
}

- (void)waitForAppToTerminateInSecs:(double)secsTimeout
{
    NSOperationQueue *queue = [[NSOperationQueue alloc] init];
    [queue addOperationWithBlock:^(void) {
        NSUserNotification *userNotif = [[NSUserNotification alloc] init];
        userNotif.title = [self agentName];
        userNotif.informativeText = [NSString stringWithFormat:@"Stopping %s", [[self appToLaunch] UTF8String]];
        [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:userNotif];
        
        BOOL term = NO;
        for (double secsOld = [[NSDate date] timeIntervalSince1970], secsNow = secsOld;
             secsNow - secsOld < secsTimeout;
             secsNow = [[NSDate date] timeIntervalSince1970])
        {
            if ([_runningApp isTerminated])
            {
                term = YES;
                break;
            }
            [NSThread sleepForTimeInterval:1];
        }
        
        userNotif = [[NSUserNotification alloc] init];
        userNotif.title = [self agentName];
        if (term == YES)
        {
            userNotif.informativeText = [NSString stringWithFormat:@"Stopped %s", [[self appToLaunch] UTF8String]];
        }
        else
        {
            userNotif.informativeText = [NSString stringWithFormat:@"Could not stop %s in %0.02f seconds", [[self appToLaunch] UTF8String], secsTimeout];
        }
        [[NSUserNotificationCenter defaultUserNotificationCenter] deliverNotification:userNotif];
    }];
}

- (IBAction)onStatusMenuQuitClicked:(id)sender
{
    BOOL result = [_runningApp terminate];
    
    if (result)
    {
        [self waitForAppToTerminateInSecs:5];
        [NSTimer scheduledTimerWithTimeInterval:6
                                         target:self
                                       selector:@selector(quitAgent)
                                       userInfo:nil
                                        repeats:NO];
        
    }
    else
    {
        [self quitAgent];
    }
}

@end
