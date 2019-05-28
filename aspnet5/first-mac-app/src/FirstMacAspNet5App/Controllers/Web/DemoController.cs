using Microsoft.AspNet.Mvc;

namespace FirstMacAspNet5App
{
	public class DemoController : Controller
	{
		public IActionResult Stuff()
		{
			ViewData["stuffed"] = "This is stuffed";
			// Return the view named as this method from the Views/ folder (it can reside in subfolders)
			return View();
		}
	}
}
