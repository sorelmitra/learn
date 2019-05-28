//
//  FileWatcher.m
//  ASC
//
//  Created by Sorel Mitra on 07/05/2014.
//
//

#import "FileWatcher.h"

@implementation FileWatcher

@synthesize queue = _queue;
@synthesize source = _source;

@dynamic fileToMonitor;

- (void)setFileToMonitor:(NSString *)path
{
    strncpy(cstrPath, [path UTF8String], 254);
}

- (NSString *)fileToMonitor
{
    return [NSString stringWithUTF8String:cstrPath];
}

- (void)startMonitoringWithEventHandlerBlock:(void(^)(void))eventHandlerBlock withCancelHandlerBlock:(void(^)(void))cancelHandlerBlock
{
    
    NSLog(@"Starting monitoring  path: %s", cstrPath);
    
    int fildes = open(cstrPath, O_RDONLY);
    if (fildes == -1)
    {
        NSLog(@"Could not open %s for monitoring: %s", cstrPath, strerror(errno));
        return;
    }
    
    _queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    _source = dispatch_source_create(DISPATCH_SOURCE_TYPE_VNODE, fildes,
                                    DISPATCH_VNODE_DELETE | DISPATCH_VNODE_WRITE | DISPATCH_VNODE_EXTEND | DISPATCH_VNODE_ATTRIB | DISPATCH_VNODE_LINK | DISPATCH_VNODE_RENAME | DISPATCH_VNODE_REVOKE,
                                    _queue);
    if (_source == NULL)
    {
        NSLog(@"Could not create dispatch source for fd %d", fildes);
        return;
    }
    
    dispatch_source_set_event_handler(_source, eventHandlerBlock);
    dispatch_source_set_cancel_handler(_source, cancelHandlerBlock);
    dispatch_resume(_source);
    
    // sometime later
    // dispatch_source_cancel(source);
}

- (void)closeMonitoredFd
{
    int fdes = dispatch_source_get_handle(_source);
    close(fdes);
}

- (void)waitForMonitoredPathToAppear
{
    int fdes = dispatch_source_get_handle(_source);
    // Wait for new file to exist.
    while ((fdes = open(cstrPath, O_RDONLY)) == -1)
    {
        sleep(1);
    }
}

@end
