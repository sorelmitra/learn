/**
 * Paste this as well as the associated CSS into https://jscomplete.com/playground
 */

const utils = {
	range: (start, end) => Array.from({length: end - start + 1}, (_, i) => start + i)
};

const AVAILABLE = 1;
const CANDIDATE = 2;
const WRONG = 3;
const USED = 4;
const INITIAL = 5;

const BUTTONS_COUNT = 9;

const Button = ( {value, starData, sumFunc, incrementUsedCount} ) => {
	const [states] = useState(new Map([
		[AVAILABLE, 'available'], [CANDIDATE, 'candidate'], [WRONG, 'wrong'], [USED, 'used'], [INITIAL, 'used']]));
	const [state, setState] = useState(INITIAL);
	const changeStateOnClick = () => {
		switch (state) {
		case AVAILABLE:
			setState(CANDIDATE);
			sumFunc(value);
			break;
		case CANDIDATE:
			// fall through
		case WRONG:
			setState(AVAILABLE);
			sumFunc(-value);
			break;
		case USED:
			break;
		}
	};
	const determineNewState = () => {
		if (starData.count === 0) return INITIAL;
		if (state === USED) return USED;
		if ([CANDIDATE, WRONG].find(element => element === state)) return starData.sumState;
		return AVAILABLE;
	};
	const newState = determineNewState();
	if (state !== newState) {
		console.log('button', value, 'state from', state, 'to', newState);
		if (newState === USED) {
			incrementUsedCount();
		}
		setState(newState);
	}
	console.log('render button', value, 'state', state);
	return (
		<button key={value} className={`number button-${states.get(state)}`}
			onClick={changeStateOnClick}>
			{value}
		</button>
	);
}

const Star = ( {value} ) => {
	return ( <div key={value} className="star" /> );
}

/*
JSComplete Playground does not support Context
so I had to pass everything through this component...
*/
const BoxedPad = ( {type, count, starData, sumFunc, incrementUsedCount} ) => {
	const [Component, className] = type === "button" ? [Button, "left"] : [Star, "right"];
	return (
		<div className={className}>
			{utils.range(1, count).map(i => 
				<Component key={i} value={i} 
					starData={starData} sumFunc={sumFunc}
					incrementUsedCount={incrementUsedCount} />
			)}
		</div>
	);
};

const ResultBox = ( {value, message} ) => {
	return (
		<p className={`result result-${value}`} >
			{message}
		</p>
	);
};

const Game = () => {
	const [starData, setStarData] = useState({
		count: 0,
		sum: 0,
		sumState: CANDIDATE,
		autoInit: false
	});
	const usedCount = useRef();
	const [gameOver, setGameOver] = useState(false);
	useEffect(() => {
		usedCount.current = 0;
	}, []);
	const [results, setResults] = useState([]);
	const updateResult = (value, number) => {
		const currentResult = results.shift();
		results.unshift({...currentResult, value: value, message: `Take ${currentResult.count}: ${value}, number: ${number}`});
		setResults(results);
	};
	const init = () => {
		const newCount = results.length ? results[0].count + 1 : 1;
		if (gameOver) {
			results.shift();
			const key = 'SUCCESS';
			const successCount = results.reduce((acc, element) => {
				if (element.value === key) {
					acc++;
				}
				return acc;
			}, 0);
			const score = Math.floor(successCount * 100 / results.length);
			results.unshift({count: newCount, value: 'GAME-OVER', message: `Game Over! Score ${score}%`});
			setStarData({...starData, 
				count: 0,
				sum: 0,
				autoInit: false
			});
		} else {
			if (results.length && starData.sum !== starData.count) {
				updateResult('FAILED', 'N/A');
			}
			setStarData({...starData, 
				count: Math.floor(1 + Math.random() * 9),
				sum: 0,
				autoInit: false
			});
			results.unshift({count: newCount, value: 'PENDING', message: `Take ${newCount}: 'IN PROGRESS'`});
		}

		setResults(results);
	};
	const reset = () => {
		setStarData({
			count: 0, 
			sum: 0,
			sumState: CANDIDATE,
			autoInit: false
		});
		usedCount.current = 0;
		setGameOver(false);
		setResults([]);
	};
	const sumFunc = (value) => {
		const newSum = starData.sum + value;
		const computeNewState = () => {
			if (newSum === starData.count) {
				updateResult('SUCCESS', newSum);
				return USED;
			}
			if (newSum > starData.count) return WRONG;
			return CANDIDATE;
		};
		const computeAutoInit = () => newSum === starData.count ? true : false;
		const newState = computeNewState();
		const autoInit = computeAutoInit();
		setStarData({...starData, 
			sum: newSum,
			sumState: newState,
			autoInit: autoInit
		});
		console.log('count', starData.count, 'sum', newSum, 'sum state', newState);
	};
	if (starData.autoInit) {
		init();
	}
	const incrementUsedCount = () => {
		usedCount.current++;
		if (usedCount.current >= BUTTONS_COUNT) {
			setGameOver(true);
			setStarData({...starData, autoInit: true});
			console.log('Game over');
		}
	};
	console.log('rendering', starData, 'game over', gameOver, 'results', results);
	return (
		<div className="game">
			<div className='control-buttons'>
				<button onClick={init}>Play</button>
				<button onClick={reset}>Reset</button>
			</div>
			<div className="body">
				<BoxedPad type="button" count={BUTTONS_COUNT} 
					starData={starData} sumFunc={sumFunc}
					incrementUsedCount={incrementUsedCount} />
				<BoxedPad type="star" count={starData.count} starData={starData}/>
			</div>
			{results.map(r => 
				<ResultBox key={r.count} value={r.value} message={r.message} />
			)}
		</div>
	);
};

ReactDOM.render(<Game />, document.getElementById('mountNode'));
