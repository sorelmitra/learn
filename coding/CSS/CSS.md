## Utility & Component Based Design

https://adamwathan.me/css-utility-classes-and-separation-of-concerns/

In a nutshell:

Have a CSS utility library, such as [Tailwind CSS](), and compose your HTML with different available classes, such as `.ph3` = `use the third size in the padding scale for horizontal padding`.

Compose your style by adding multiple classes to an HTML tag, and by wrapping your content in extra `div` or `span` tags with specific classes, based on your need.

Create reusable components wherever the amount of classes used in a row becomes overwhelming, such as `<button class="f6 br3 ph3 pv2 white bg-purple hover-bg-light-purple">`.  If your CSS preprocessor is [Less CSS](https://lesscss.org), you could use multi-cursor in your editor to end up with:

	.btn-purple {
		.f6;
		.br3;
		.ph3;
		.pv2;
		.white;
		.bg-purple;
		.hover-bg-light-purple;
	}

And then just say `<button class="btn-purple>`.  Use your common sense to decide when to enumerate some classes on your tag, or create a new component.
