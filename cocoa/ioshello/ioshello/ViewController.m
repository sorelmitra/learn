//
//  ViewController.m
//  ioshello
//
//  Created by Sorel Mitra on 4/15/14.
//  Copyright (c) 2014 Sorel Mitra. All rights reserved.
//

#import <UIKit/UIAccelerometer.h>

#import "ViewController.h"

#import "Mover.h"

@interface ViewController ()
{
    Mover *mover;
}

@property (weak, nonatomic) IBOutlet UILabel *helloLabel;
@property (weak, nonatomic) IBOutlet UIImageView *thingImg;

@end

@implementation ViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
	// Do any additional setup after loading the view, typically from a nib.
    [self.helloLabel setText:@"Hello, worldie!"];
    self->mover = [[Mover alloc] init];
    [self->mover setImg:_thingImg];
    [self->mover setLabel:_helloLabel];
}

- (void)viewDidAppear:(BOOL)animated
{
    [super viewDidAppear:animated];
    [self->mover start];    
}

- (void)viewDidDisappear:(BOOL)animated
{
    [super viewDidDisappear:animated];
    [self->mover stop];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (BOOL)shouldAutorotate
{
    return FALSE;
}

@end
