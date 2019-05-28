//
//  TestViewController.swift
//  BullsEye
//
//  Created by Sorel Mitra on 30/07/15.
//  Copyright (c) 2015 Razeware. All rights reserved.
//

import UIKit

class TestViewController: UIViewController {

    var conversionLabelText: String?
  
    override func viewDidLoad() {
        super.viewDidLoad()

        conversionLabel.text = conversionLabelText
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    

    @IBOutlet weak var celsiusLabel: UILabel!
    @IBOutlet weak var fahrenheitTextField: UITextField!
    @IBOutlet weak var conversionLabel: UILabel!
    
    @IBAction func closeMe(sender: AnyObject) {
        dismissViewControllerAnimated(true, completion: nil);
    }
    
    @IBAction func fahrenheitChanged(sender: AnyObject) {
        let f = (fahrenheitTextField.text as NSString).doubleValue
        let c = (f - 32) / 1.8
        let s: NSString = NSString(format: "%0.02f", c)
        celsiusLabel.text = s as String
    }
    
    override func touchesBegan(touches: Set<NSObject>, withEvent event: UIEvent) {
        fahrenheitTextField.endEditing(true)
    }
  
    /*
    // MARK: - Navigation

    // In a storyboard-based application, you will often want to do a little preparation before navigation
    override func prepareForSegue(segue: UIStoryboardSegue, sender: AnyObject?) {
        // Get the new view controller using segue.destinationViewController.
        // Pass the selected object to the new view controller.
    }
    */

}
