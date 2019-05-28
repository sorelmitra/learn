//
//  PlaceCallCommand.h
//  ASC
//
//  Created by Sorel Mitra on 07/05/2014.
//
//

#import <Foundation/Foundation.h>
#import "FileWatcher.h"

@interface PlaceCallCommand : NSObject
{
    @private
    NSString *placeCallCmdFile;
    FileWatcher *fileWatcher;
    void(^eventHandlerBlock)(void);
    void(^cancelHandlerBlock)(void);
}

@property (strong) FileWatcher *fileWatcher;

- (void)monitor;

@end
