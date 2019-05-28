@RestController
class HelloWorldController {
	@RequestMapping("/")
	String sayHello() {
		"Hello World!"
	}
}

