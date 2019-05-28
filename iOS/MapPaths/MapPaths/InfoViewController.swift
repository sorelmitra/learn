//
//  InfoViewController.swift
//  MapPaths
//
//  Created by Sorel Mitra on 08/08/15.
//  Copyright (c) 2015 Sorel Mitra. All rights reserved.
//

import UIKit
import CoreLocation
import AddressBook

class InfoViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()

        // Do any additional setup after loading the view.
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }
    
    @IBOutlet weak var addressTextField: UITextField!
    @IBOutlet weak var latitudeTextField: UITextField!
    @IBOutlet weak var longitudeTextField: UITextField!
    @IBOutlet weak var messageLabel: UILabel!

    @IBAction func addressTextFieldDidEndOnExit(sender: AnyObject) {
        sender.resignFirstResponder()
        computeLatitudeAndLongitude(sender)
    }

    @IBAction func latitudeTextFieldDidEndOnExit(sender: AnyObject) {
        sender.resignFirstResponder()
        computeAddress(sender)
    }

    @IBAction func longitudeTextFieldDidEndOnExit(sender: AnyObject) {
        sender.resignFirstResponder()
        computeAddress(sender)
    }

    @IBAction func computeLatitudeAndLongitude(sender: AnyObject) {
        if (count(addressTextField.text) < 1) {
            showMessage("Please enter a street address")
            return
        }

        let geoCoder = CLGeocoder()
        geoCoder.geocodeAddressString(
            addressTextField.text,
            completionHandler:
            {
                (placemarks: [AnyObject]!, error: NSError!) in

                if error != nil {
                    self.showMessage("Geocode failed with error: \(error.localizedDescription)")
                    return
                }

                if placemarks.count > 0 {
                    let placemark = placemarks[0] as! CLPlacemark
                    let location = placemark.location
                    let coords: CLLocationCoordinate2D = location.coordinate

                    self.hideMessage()
                    self.latitudeTextField.text = "\(coords.latitude)"
                    self.longitudeTextField.text = "\(coords.longitude)"
                } else {
                    self.showMessage("Geocode returned no placemarks for the address");
                }
            }
        )
    }

    @IBAction func computeAddress(sender: AnyObject) {
        if (count(latitudeTextField.text) < 1) {
            showMessage("Please enter latitude")
            return
        }
        if (count(longitudeTextField.text) < 1) {
            showMessage("Please enter longitude")
            return
        }

        let latitudeString = latitudeTextField.text as NSString
        let longitudeString = longitudeTextField.text as NSString
        let location = CLLocation(
            latitude: latitudeString.doubleValue,
            longitude: longitudeString.doubleValue)

        let geoCoder = CLGeocoder()
        geoCoder.reverseGeocodeLocation(
            location,
            completionHandler: {
                (placemarks: [AnyObject]!, error: NSError!) in

                if error != nil {
                    self.showMessage("Geocode failed with error: \(error.localizedDescription)")
                    return
                }

                if placemarks.count > 0 {
                    let placemark = placemarks[0] as! CLPlacemark
                    let addressDictionary = placemark.addressDictionary

                    let address = self.optionalToHumanReadableString(addressDictionary[kABPersonAddressStreetKey])
                    let city = self.optionalToHumanReadableString(addressDictionary[kABPersonAddressCityKey])
                    let state = self.optionalToHumanReadableString(addressDictionary[kABPersonAddressStateKey])
                    let zip = self.optionalToHumanReadableString(addressDictionary[kABPersonAddressZIPKey])
                    let countryCode = self.optionalToHumanReadableString(addressDictionary[kABPersonAddressCountryCodeKey])

                    self.hideMessage()
                    self.addressTextField.text = "\(address) \(city) \(state) \(zip) \(countryCode)"
                } else {
                    self.showMessage("Geocode returned no placemarks for the address");
                }
            }
        )
    }

    // MARK: - Helper methods

    func showMessage(message: String) {
        messageLabel.text = message
        messageLabel.hidden = false
    }

    func hideMessage() {
        messageLabel.hidden = true
    }

    func optionalToHumanReadableString(object: AnyObject?) -> String {
        let optionalString = object as! String?
        let humanReadableString = optionalString == nil ? "" : optionalString!
        return humanReadableString
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
