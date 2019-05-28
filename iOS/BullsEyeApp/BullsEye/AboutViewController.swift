import UIKit

// This is the About screen. It shows the gameplay instructions in a web view.
// It also has a Close button that closes the screen and returns the player to 
// the main game screen.
class AboutViewController: UIViewController {
  @IBOutlet weak var webView: UIWebView!

  override func viewDidLoad() {
    super.viewDidLoad()
    
    // Load the BullsEye.html file into the web view.
    if let htmlFile = NSBundle.mainBundle().pathForResource("BullsEye", ofType: "html") {
      let htmlData = NSData(contentsOfFile: htmlFile)
      let baseURL = NSURL.fileURLWithPath(NSBundle.mainBundle().bundlePath)
      webView.loadData(htmlData, MIMEType: "text/html", textEncodingName: "UTF-8", baseURL: baseURL)
    }
  }

  // This action is called when the user taps the Close button. In response, we
  // dismiss the About screen and automatically return to the main game screen
  // (BullsEyeViewController).
  @IBAction func close() {
    dismissViewControllerAnimated(true, completion: nil)
  }
}
