//
//  FileWatcher.h
//  ASC
//
//  Created by Sorel Mitra on 07/05/2014.
//
//

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>

@interface FileWatcher : NSObject
{
    @private
    dispatch_queue_t _queue;
    dispatch_source_t _source;
    char cstrPath[255];
}

@property (readonly) dispatch_queue_t queue;
@property (readonly) dispatch_source_t source;

@property (nonatomic, copy) NSString *fileToMonitor;

- (void)startMonitoringWithEventHandlerBlock:(void(^)(void))eventHandlerBlock withCancelHandlerBlock:(void(^)(void))cancelHandlerBlock;
- (void)closeMonitoredFd;
- (void)waitForMonitoredPathToAppear;

@end
