
Last update: 2021-12-03
(Minor corrections: 2025-07-17)

# Overview

React is a declarative, efficient, and flexible JavaScript library for building user interfaces.  It lets you compose complex UIs from small and isolated pieces of code called “components”.

- Libraries:
	- React: Contains basic functionality including creating the Object Tree.
	- React DOM: Renders the Object Tree for browsers.
	- React Native: Renders (and more?) for mobile platforms.
- Creating Tree elements can be done in one of two ways:
	 - In JS code with `React.createElement()`.
	 - JSX (usually preferred for its terseness): HTML-like syntax (but NOT HTML) for writing React code.  Converted to JS using transpilers such as Babel.
- Components:
	- A component has properties such as: `Name`, `props` (input data), `State`, `render()`.
	- An app is created by using composition of components.
		- E.g. `App {Header Search FeaturedHouse {HouseDetails}}`
	- A component renders itself by calling to its sub-components.
- State drives rendering.  `React` reacts to state changes and triggers re-renders.
- Rendering is done in memory, to "Virtual views", using tree Reconciliation, or "diffing" algorithm.  This creates a new tree, then figures out differences from the old one.  Then only renders the different parts.




# Basics

## Code Structure

### Entry Points

Files `index.html` and `src/index.js` have to stay where they are, the rest of the files can be organized as you like.

### Subfolders

If your component has more than one file, it helps creating a folder like this:

	main-page/
		App.css
		index.js
		logo.svg

Then import it like this: `import App from ./main-page`.  Webpack understands that it has to look for `main-page/index.js` in this case.

## Bootstrap - Optional but Useful

Import it in `index.js` and it becomes available in all the app.

## Render


	ReactDOM.render(
		<React.StrictMode>
			<MyComponent />
		</React.StrictMode>,
	  document.getElementById('htmlParentElem')
	);

Component `MyComponent` must be available in this file, `React.StrictMode` is a predefined React component that warns us of errors, and `htmlParentElem` is ID of an existing HTML file in `index.html`.

## Component

	const App = (props) => (
		<div className="App">
			<img src={logo} className="App-logo" alt="logo" />
			<b>{props.someMessage}</b> //--> (1)
		</div>
	);

Although JSX looks like HTML, it is not.  Each HTML-like tag is a component that usually produces it's HTML correspondent, but `src` is actually a React prop, not an HTML attribute.

## Props

A prop is essentially a parameter sent to a component.

	<img src={logo} className="App-logo" alt="app logo" />

The React standard `img` component has the `src` argument, which in the above case is sent an expression.  `logo` is a JS variable available in this file.

The above `App` component also defines the `someMessage` prop (1) that it can be passed to it like this:

	<App someMessage='Hi, guy!' />

### Component Types

1) Function (preferred, combined with hooks keeps app structure simple and clean):

	const Header = () => (
		<div className = 'col-md-5'>
			// ...
		</div>
	);

It has to return a single root element.
Here, `col-md-5` is part of Bootstrap and helps with arranging files.

2) Class (discouraged due to added complexities with `this`, `bind`, and others):

	import { Component } from 'react';
	class Header extends Component {
		render() {
			return (
				<div className = 'col-md-5'>
					// ...
					<b>{this.props.xyz}</b>
				</div>
			);
		}
	);

The props are made available via the `this.props` member of the class.

## State

State is internal, private, data for the component, that can survive re-renders.

## Hooks

Suppose I have this component that wants to load some data the first time it is rendered:

	const ParentBox = ({ title, name }) => (
		loadData();
		<div>{title}, {name}</div>
	);

As the props change during the lifetime of the app, `loadData()` will get called multiple times.

Hooks solve this problem.  A hook is a function that hooks into (attaches to) some complexity and provides an interface to it.  Hooks can be predefined in React, such as `useEffect` or be created by you.

### `useEffect`, `useState` - Side Effects and Statefulness

This internal React hook lets us create side effects when the state of a component changes:

	import { useEffect, useState } from 'react';
	const ParentBox = ({ title, name }) => { //--> (1)
		let data = 0; // Don't rely on 'data' as it will be 0 at the next render
		
		// this survives re-renders
		const [allHouses, setAllHouses] = useState([]);  //--> (2)
		
		useEffect( /* Don't use async here! */ () => {
			// An IIFE will get us moving correctly with async
			(async () => {  //--> (3)
				const response = await fetch('https://houses.api');
				data = await response.json();
				subscribeToHouseUpdates(data.foo);
				setAllHouses(data);
			})();
			
			// Cleanup function
			// Because the IIFE returns not a promise, React has a chance to run the cleanup function
			return () => { //--> (4)
				if (allHouses) { // (5)
					unsubscribeFromHouseUpdates(allHouses.foo); //--> (6)
				}
			};
		}, []);  //--> (7)
		
		let featuredHouse = {}; //--> (8)
		if (allHouses.length) {
			const randomIndex = Math.floor(Math.random() * allHouses.length);
			featuredHouse = allHouses[randomIndex];
		}
		
		return (
			<div>{title}, {name}, {allHouses}</div>
		);
	};

(1) First param of `useEffect` is the function to execute, that is the one that creates the side effect.

(2) To store data created by the side effect, we need to use component's *state*, like with `allHouses` above.  If we did it like with `data` above, it will be `0` at the next render.
All state for this component gets declared somewhere inside it.  Each time you call `useState()` it will create a new state that you can use throughout your component's code.

(3) To fetch data asynchronously in a side effect:

- The first rule is to NOT pass an async func directly to `useEffect`, because that returns a promise and would prevent React from executing its cleanup.
- Instead, you can use an Immediately Invoked Function Expression, like above.  The Function Expression can then be async.  React will fire the function which will asynchronously fetch, and set state once the data becomes available.  This will trigger any component updates that depend on the newly changed data.  React can also execute the cleanup function.

(4) The return value of the function to execute in `useEffect` is the cleanup function, which React schedules to execute before the next render happens.  If no cleanup function is provided, nothing happens.

(5) I suppose that if data is not fetched by the time the next render happens, the cleanup code would blow up without this check.
[Some](https://dev.to/raibtoffoletto/comment/1f63o) even suggest "that any async logic state goes to [Redux](https://react-redux.js.org/)."

(6) We need to use `allHouses` here as, although we have a closure on `data`, it will be reset to 0 at the next render so I'm not sure it's reliable.

(7) The second param of `useEffect` is an array of values to be watched.  Based on what's in this second param, the side effect is triggered:

- If no array is passed in, then the side effect is executed after each completed render.
- If an empty array `[]` is passed in, then the side effect is executed only the first time the component gets rendered.
- Otherwise the side effect is executed when any of those values change.

(8) We're randomly choosing a featured house here.  Let's see how this works step by step:

- Based on (7), the `allHouses` state is being fetched via `useEffect` *the first time* the component is rendered.
- The fetch is async so `useEffect` returns right away so now the code from (8) is being executed.
- Thanks to the check for `allHouses.length`, it does nothing so no featured house so far!
- At some point in time, the async fetch returns so the code from inside `useEffect` fills in `allHouses`.
- This makes `allHouses` change so React re-renders.
- This time `useEffect` is not doing anything since we told it to only execute on the first render.
- Now `allHouses.length` has a value so the code to determine the `featuredHouse` is executed.

### `useMemo` - Optimize Results of Expensive Computations

Implementation of memoization - storing the results of expensive function calls and returning the cached result when the same inputs occur again.

	const memoizedValue = useMemo(() => computeExpensiveValue(a, b), [a, b]);

Write your code so that it still works without `useMemo` — and then add it to optimize performance.  Do NOT rely on memoization for semantic guarantees.

## Router

A 'page' in a single-page application isn't a page that the browser can understand and redirect.  But at the same time we want the user to be able to use the `<< Back` and `Forward >>` buttons to navigate, as well as create links that can be edited or pasted.
This is where `react-router` comes into place.

	npm install react-router-dom

Then:

	import { BrowserRouter as Router, Switch, Route } from 'react-router-dom';
	const MainPage = () => { //--> (1)
		const allHouses = ...; // get it as shown above with `useEffect`
		return (
			<Router> //--> (2)
				<div className="container"> //--> (3)
					<Header subtitle="Hi there" />
				</div>
				<Switch> //--> (4)
					<Route exact path='/'> //--> (5)
						<Welcome />
					</Route>
					<Route path='/about'> //--> (6)
						<About />
					</Route>
					<Route path='/countries/:country'> //--> (7)
						<HouseList values={allHouses} /> //--> (8)
					</Route>
					<Route path='/houses/:id'>
						<HouseDetails values={allHouses} />
					</Route>
				</Switch> //--> (9)
			</Router>
		);
	}

(1) This is our main component, that embeds the entire app.

(2) We now wrap everything in the `Router` component.  This enables it to drive our application.

(3) Everything that is a *direct child* of `Router` is *always* displayed.

(4) Inside `Switch` we place one `Route` component for each route we want to handle.

(5) Suppose the app is hosted on `react.com`, the `Welcome` component is displayed on `react.com/`.

- Order matters here: `react-router` evaluates the routes in the order you define them.
- That means in this case that the `/` route is evaluated first.  If it matches, it is used and no other routes are evaluated.
- The Router does *partial matching* by default.  This means that if you put `/` before `/about` and do nothing else, when you go to `react.com/about` it will actually match `react.com/` thus going to the homepage instead of About.
- The `exact` parameter we added to the `/` route makes sure that the Router does *not* do partial matching for this particular route, so we can leave it the first here.  The other option would've been to move `/` at the end so that it is evaluated last.

(6) The `About` component is displayed on `react.com/about`.

(7) Route for the houses per country.  We call it in the next section.  Here `country` is the name of the parameter as seen by React.

(8) We call to the `HouseList` component passing in a list of houses that we get in a way not pictured here.
Note that the name of the param, in this case `values`, doesn't matter as the target component de-structures it anyway.

(9) Suppose the user goes to `react.com/`, then to `react.com/about`.  Now if they hit `<< Back` in the browser, they'll be directed back to `react.com/`.  

### Implementing a Route & Directing to Other Routes

The `HouseList` and `HouseListItem` components:

	import { useHistory } from 'react-router-dom';
	const HouseListItem = ({ house }) => { //--> (1)
		const history = useHistory();
		const setActiveHouse = 
			() => history.push(`/houses/${house.id}`); //--> (2)
		return (
			<tr onClick={setActiveHouse}> //--> (3)
				<td>{house.address}</td>
				<td>{house.price}</td>
			</tr>
		);
	};
	
	import { useParams } from 'react-router-dom';
	const HouseList = ({ houses }) => {
		const { country } = useParams(); //--> (4)
		const housesPerCountry = 
			houses.filter(h => h.country === country); //--> (5)
		return (
			<div className="mt-2">
				<h4>Houses in {country}</h4>
				<table className="table table-hover">
					<tbody>
						{housesPerCountry.map(h => (
							<HouseListItem key={h.id} house={h} /> //--> (6)
						))}
					</tbody>
				</table>
			</div>
		);
	};


(1) We use this component to render an individual house - here, as a table row.

(2) Function to navigate to the house that's being rendered in this row.  That route exists in our `MainPage`.

(3) Call the function that navigates when the user clicks anywhere in the row.

(4) We de-structure the `country` param that we defined in the route URL by using the `useParams` hook.

(5) Now filter the house list (which has all of them!) by the country we received - which comes from the `select` below.

(6) We render the component using JSX as explained below.  Note the usage of `key` to allow React to update individual elements in the list (more of this JSX feature below).

### More JSX and Filtering

	import { useHistory } from 'react-router-dom'; //--> (1)
	const HouseFilter = ({ houses }) => {
		const history = useHistory();
		const countries = houses ? Array.from(new Set(houses.map((h) => h.country))) : []; //--> (2)
		countries.unshift(null); //--> (3)
		
		const onSearchChange = (e) => { //--> (4)
			const country = e.target.value;
			history.push(`/countries/${country}`); //--> (5)
		};
		
		return (
			<div className = "offset-md-2 col-md-4">
				Find house in country:
			</div>
			<div className = "col-md-4 mb-3">
				<select className="form-select" onChange={onSearchChange}> //--> (6)
					{countries.map((c) => ( //--> (7)
						<option key={c} value={c}> //--> (8)
							{c}
						</option>
					))}
				</select>
			</div>
		);
	};

(1) The `useHistory` hook allows us to acces the Router's side effect of viewing and altering the browser history.

(2) Create array of unique countries from all the houses.

(3) Put a `null` entry in the beginning to make the `select` have no value initially.

(4) The function we specified for `onChange` on the `select` receives an event and here we use that event's target value, which is the value of the `select` element.  That gives us the country and then we instruct the React router to navigate to the specified path by pushing a new route into the browser history.

(5) The route we're navigating to must exist defined in the main page.  In our case (see above `MainPage`) it calls out to a HouseList component that we defined along with the route in the previous section.  It passes a value for the `country` parameter that the route is expecting, based on the event's target value we took previously.

(6) Inform React we want to run the specified function on value change for the `select` element.

(7) This is how you mix React components with JS code, aka JSX.  Here we map each country to an option with that value and key and text.

(8) The `key` param tells React how to compare individual array items when rendering.  Without it it would re-render the whole array if just one element changed.

## Forms & State Monitoring

A simple form:

	const Inquiry = () => {
	  const [contactInfo, setContactInfo] = useState({
	    name: "",
	    email: "",
	    remarks: "",
	  }); //--> (1)
	  
	  const onChange = (e) => { //--> (2)
	    setContactInfo({ ...contactInfo, [e.target.id]: e.target.value });
	  };
	  
	  const onSubmit = (e) => {
	    e.preventDefault(); //--> (3)
	    console.log(contactInfo);
	 	// send...
	  };
	  
	  return ( //--> (4)
	    <form className="mt-2">
	      <div className="form-group">
	        <label htmlFor="name">Name</label>
	        <input
	          type="text"
	          className="form-control"
	          placeholder="Name"
	          id="name" //--> (5)
	          value={contactInfo.name} //--> (6)
	          onChange={onChange} //--> (7)
	        />
	      </div>
	      <div className="form-group">
	        <label htmlFor="email">Email address</label>
	        <input
	          type="text"
	          id="email"
	          className="form-control"
	          placeholder="Email"
	          value={contactInfo.email}
	          onChange={onChange}
	        />
	      </div>
	      <div className="form-group">
	        <label htmlFor="remarks">Remarks</label>
	        <input
	          type="text"
	          id="remarks"
	          className="form-control"
	          placeholder="Remarks"
	          value={contactInfo.remarks}
	          onChange={onChange}
	        />
	      </div>
	      <button
	        className="btn btn-primary mt-2"
	        disabled={!contactInfo.name || !contactInfo.email} //--> (8)
	        onClick={onSubmit}
	      >
	        Submit
	      </button>
	    </form>
	  );
	};

(1) The state is initialized with an object with some values.

(2) Function to update state when the form field is changed:

- Note the use of the spread operator `...` to include all properties from the original state object.
- The event's target gives us the HTML element that was clicked.
- We use the `id` of that element to identify a property on the state object that we need to change.  This works because we took care to set that `id` to match one of the state object's properties.
- Finally we use the `value` of the same HTML element to set our state object's property value.
- This technique saves you from writing multiple `onChange` handlers.

(3) Prevent the default behavior of the `submit` button in the browser when the button is clicked.  Then we go on "sending" the form.

(4) We compose the form here using React JSX.  Remember, this is not pure HTML.

(5) The value of the `id` prop is passed on by React to the actual HTML element that's being used for rendering.  Here we set it to match the name of one of the state object's properties.

(6) The value of the `input` is one of the state object's properties.

(7) Tell React to call our function to update state when the form field is changed.

(8) Here we see how React monitors state:  We have a single state, `contactInfo` but we're using different fields from inside it to form a logical condition that React is able to evaluate.  Only when the condition evaluates to `true` does the `button` get disabled.




# Intermediate

## Common Practices on Components & Hooks

1. Always call hooks at the top level of a Components, i.e. not in loops, ifs, etc.  The reason is that for each hook call, React adds a new element into a list.  So it would not track state correctly in this case.

2. Pass functions as props to Components.  This allows the Component to call that function, effectively notifying the parent Component that something has happened.

3. Extract JSX parts into variables, e.g.

		const header = <Header subtitle="Howdy!" />
		return <div>{header}</div>
	
	This helps organize the code better and also adding conditionals.

4. Utilize `useCallback` to define a memoized callback, i.e. one that doesn't change until its listed dependencies are changed.  E.g. below, where `doSomething` is memoized but updated each time the value of `a` or `b` changes:

		const memoizedCallback = useCallback(
			() => { doSomething(a, b); },
			[a, b],
		);

5. Get a hold on an HTML element with `useRef`.  The same can be used to define an object that survives re-renders but is not state that React monitors.  Example with HTML element:

		function TextInputWithFocusButton() {
			const inputEl = useRef(null); //--> (1)
			const onButtonClick = () => {
				// `current` points to the mounted text input element
				inputEl.current.focus(); //--> (3)
			};
			return (
				<> //--> (2)
					<input ref={inputEl} type="text" /> //--> (4)
					<button onClick={onButtonClick}>Focus the input</button>
				</>
			);
		}

(1) `useRef` returns a ref object with a single `current` property initially set to the initial value you provided, in this case `null`.

(2) Note the use of the empty tag `<>` which is a shortcut for `<React.Fragment>`, which in turn is a way of rendering multiple components without actually creating a parent DOM element like would've happened if you had used `<div>`.

(3) We use the ref that react filled in for us (see below) to focus that particular element.

(4) When React creates a DOM node for this `<input>`, React will put a reference to this node into `inputEl.current`.  You can then access this DOM node from your event handlers (like above with `onButtonClick`) and use the built-in browser APIs defined on it.

## Custom Hooks

Make the code cleaner by extracting the fetch for houses in a hook defined in a separate file.  A custom hook is just an exported function that makes use of `useEffect` and `useState` and returns the processed object:

	import { useState, useEffect } from "react";
	
	const useHouses = () => {
		const [allHouses, setAllHouses] = useState([]);
		
		useEffect(() => {
			const fetchHouses = async () => {
				const rsp = await fetch("/houses.json");
				const houses = await rsp.json();
				setAllHouses(houses);
			};
			fetchHouses();
		}, []);
		
		return allHouses;
	};
	
	export default useHouses;

We add the above code to a file inside a `src/hooks` directory.
Then we use it in our component like this:

	import useHouses from '../hooks/useHouses';
	const MainPage = () => { //--> (1)
		const allHouses = useHouses();
		// ...
	}


## Context

Rather than passing `allHouses` all over the place, we can create a `context`.  In `src/context/housesContext.js`, do:

	import { createContext } from "react";
	const HousesContext = createContext([]);
	export default HousesContext;

Then in `index.js:`

	import HousesContext from "../context/housesContext";
	<Router>
		<HousesContext.Provider value={allHouses}>
			<div>
				<Route path='/countries/:country'>
					<HouseList />
				</Route>
				<Route path='/houses/:id'>
					<HouseDetails />
				</Route>
			</div
		</HousesContext>
	</Router>

Note how we wrap everything in `HousesContext`, which is in turn wrapped in `Router`.
Now your component that needs `allHouses`, say `HouseList`, will do like this:

	import { useParams } from 'react-router-dom';
	import HousesContext from "../context/housesContext";
	const HouseList = () => {
		const { country } = useParams();
		const allHouses = useContext(HousesContext);
		const housesPerCountry = 
			allHouses.filter(h => h.country === country);
		return (
			<div className="mt-2"> ... </div>
		);
	};

So we no longer receive the `allHouses` de-structured parameter and instead take it from the `HousesContext`.  This context is available to all all child components of `HouseList`.

## Type Checking Components

Use **TypeScript**, or:

Say we have a component `House`:

	const House = ({ house }) => {
		return ( <div> {house.country} </div> );
	}

And we call it like that, forgetting to pass in the `house` parameter with `<House house={house} />`, as we should've:

	const FeaturedHouse = ({ house }) => {
		return (<div> <House /> </div>);
	};

Now React will give you this error along with a message like `TypeError: undefined is not an object (evaluating 'house.country')` and some context lines.

We can add more debug info to that if we install `prop-types` and then change `House` like this:

	import PropTypes from 'prop-types';
	const House = ({ house }) => {
		return ( <div> {house.country} </div> );
	}
	House.propTypes = {
		house: PropTypes.object.isRequired,
	}

This will log to the `console` a message like:

	Warning: Failed prop type: The prop `house` is marked as required in `House`, but its value is `undefined`.

These checks only function on development builds so they do not affect production.

## Eject

Using `create-react-app` configures your app using `react-scripts` which bundles together all needed tools and configuration for developing and building React apps.

If we run `npm run eject`, it will "eject" us from using these and create a `config` directory where we can fine tune all settings and dependencies.  This adds flexibility at the cost of increased complexity.

## Unit Testing

There's out of the box support for `jest` where you can render your components and expect results.

There are also other options, such as [Enzyme](https://enzymejs.github.io/enzyme/).

## End-to-End Testing

Not tied to React, but there's [Cypress](https://www.cypress.io).

## PWA

Progressive Web Applications can function while offline as well, and can also be installed on the device.  The basis for that is the Service Worker provided by browsers' `navigator.serviceWorker` property, which in this case is populated by a service that caches data in the local storage and periodically checks for updates from the server.  `create-react-app` provides a starter point for this, but it must be maintained by the developer to also catch required data.

The workhorse for the PWA service is Google's [Workbox](https://developers.google.com/web/tools/workbox/).

To get started, you can have `create-react-app` generate the starter service for you by choosing one of the available templates:

	npx create-react-app my-app --template cra-template-pwa-typescript




# Advanced

## JSX vs JS

Hello world component with JSX:

	const Hello = () => {
		return (
			<div className="container"> <h1>Hello, world!</h1> </div>
		);
	}
	ReactDOM.render(<Hello />, mountNode);

The same with pur JS code:

	const Hello = () => {
		return (
			React.createElement("div", { className: "container"}, React.createElement("h1", null, "Getting Started")
		);
	}
	ReactDOM.render(<Hello />, mountNode);

The Babel transpiler more or less translates the first into the second.

## Elements

Elements are the smallest building blocks of React apps.
An element describes what you want to see on the screen:

	const element = <h1>Hello, world</h1>;

Unlike browser DOM elements, React elements are plain objects, and are cheap to create. React DOM takes care of updating the DOM to match the React elements.

## Root Node

Put this somewhere in your HTML:

	<div id="root"></div>

Then this in your `index.jsx`:

	const element = <h1>Hello, world</h1>;
	ReactDOM.render(element, document.getElementById('root'));

React's hello world: the above will render Hello, world in that `root` HTML element.

## Updating Elements

A React element is immutable, like a single frame in a movie: it represents the UI at a certain point in time.  To change what's being shown, you have to pass it a new element with a changed value.  React will then generate the new DOM, compare it to the old one, and only update the changed values.

This allows thinking about how the UI should look at any given moment, rather than how to change it over time.

## Declarative vs Imperative & Tree Reconciliation

Consider these two pieces of code:

HTML version:

	const render = () => {
		document.getElementById('mountNode').innerHTML = `
			<div>
				Hello HTML
				<input />
				<pre>${(new Date).toLocaleTimeString()}</pre>
			</div>
		`;
	}
	
	setInterval(render, 1000);

React version:

	const render = () => {
		ReactDOM.render(
			<div>
				Hello React
				<input />
				<pre>{new Date().toLocaleTimeString()}</pre>
			</div>,
			document.getElementById('mountNode2')
	  );
	};
	setInterval(render, 1000);

We render an entire DOM fragment every second, first time with pure HTML and some JS code, and the second time with React.  There are a few things to observe:

1. Obviously, tree reconciliation makes React only update the `pre` element, which is visible by looking at the DOM with Developer Tools.
2. The same tree reconciliation makes it possible to type in the `input`, while in the HTML version we can't.
3. We could make the HTML version update only the `pre`, but we'd have to write some code for that.
4. That code would be *imperative*, i.e., hunt for the right element and update it.
5. In contrast, React allows us to write *declarative* code - we just want the current date in the `pre` - and takes care of the update behind the scenes.




# Resources

Playground: https://jscomplete.com/playground

Courses & Documentation:

[1] A Practical Start with React by Roland Guijt https://app.pluralsight.com/courses/dbcc56fe-7657-4e41-bb61-278bd45e7edb/table-of-contents
[2] React JS Official Tutorial https://reactjs.org/tutorial/tutorial.html
[3] React & React Native Comparison https://www.simform.com/blog/reactjs-vs-reactnative/
[4] React Effect Hook https://reactjs.org/docs/hooks-effect.html
[5] React Hooks API Reference https://reactjs.org/docs/hooks-reference.html
[6] Imperative vs Declarative (Functional) Programming https://stackoverflow.com/q/17826380/6239668
[7] NextJS Framework https://nextjs.org/learn/basics/create-nextjs-app
[8] React with Typescript https://create-react-app.dev/docs/adding-typescript/
[9] React, List, and Keys https://reactjs.org/docs/lists-and-keys.html
[10] React Elements https://reactjs.org/docs/rendering-elements.html
[11] React Main Concepts https://reactjs.org/docs/hello-world.html
[12] PWA in React https://create-react-app.dev/docs/making-a-progressive-web-app/
