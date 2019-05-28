//
//  PlaceCallCommand.m
//  ASC
//
//  Created by Sorel Mitra on 07/05/2014.
//
//

#import "OCDefinitions.h"
#import "AddressOfRecord+LocalContact.h"
#import "ServiceLocator.h"

#import "PlaceCallCommand.h"

@implementation PlaceCallCommand

@synthesize fileWatcher;

#pragma mark Private Methods

- (void)restartMonitor
{
    dispatch_source_cancel([fileWatcher source]);
    [fileWatcher closeMonitoredFd];
    [fileWatcher waitForMonitoredPathToAppear];
    [self monitor];
}

- (void)run
{
    NSDictionary *cmdAsDict = [NSDictionary dictionaryWithContentsOfFile:placeCallCmdFile];
	AddressOfRecord *aor = [[[AddressOfRecord alloc] init] autorelease];
    [aor setAddress:[cmdAsDict valueForKey:@"address"]];
    [aor setExtension:[cmdAsDict valueForKey:@"extension"]];
    [aor setDialedNumber:[cmdAsDict valueForKey:@"dialedNumber"]];
    [aor setDisplayName:[cmdAsDict valueForKey:@"displayName"]];
    [aor setDisplayNameSetByServer:[cmdAsDict valueForKey:@"displayNameSetByServer"]];
    
    id<UserAgent> userAgent = [[ServiceLocator sharedLocator] userAgent];
    [userAgent placeAudioCallWithAddress:aor andApplyDialingRules:YES];
}

#pragma mark Public Methods

- (id)init {
    self = [super init];
    if (self)
    {
        placeCallCmdFile = [APP_SUPPORT_DIR stringByAppendingString:@"place_call_cmd.plist"];
        /*
          OK, so I called retain on PlaceCallCommand::placeCallCmdFile, but when do I release it? I suspect this is a code smell suggesting a different place for initialization and lifetime of placeCallCmdFile
         */
        [placeCallCmdFile retain];
        fileWatcher = [[FileWatcher alloc] init];
    }
    return self;
    
}

- (void)monitor
{
    eventHandlerBlock = ^
    {
        unsigned long srcData = dispatch_source_get_data([fileWatcher source]);
        NSLog(@"Place call command file changed, flags are 0x%08lx", srcData);
        if (srcData & DISPATCH_VNODE_DELETE)
        {
            NSLog(@"Place call command file deleted, resuming monitoring on the new file");
            [self restartMonitor];
        }
        [self run];
    };
    
    cancelHandlerBlock = ^
    {
        NSLog(@"Canceled Place call command file monitoring");
    };
    
    [fileWatcher setFileToMonitor:placeCallCmdFile];
    [fileWatcher startMonitoringWithEventHandlerBlock:eventHandlerBlock
                               withCancelHandlerBlock:cancelHandlerBlock];
}

@end
