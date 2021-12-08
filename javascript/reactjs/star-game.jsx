/**
 * This is functional and more React-like written than the 'poor-code' variant.
 * 
 * Paste this as well as the associated CSS into https://jscomplete.com/playground
 */

const utils = {
	range: (start, end) => Array.from({length: end - start + 1}, (_, i) => start + i)
};

const AVAILABLE = 'available';
const CANDIDATE = 'candidate';
const WRONG = 'wrong';
const USED = 'used';
const INITIAL = 'initial';

const GAME_OVER = 'game-over';
const PENDING = 'pending';
const SUCCESS = 'success';
const FAILED = 'failed';

const Buttons = ( {states, changeStateOnClick} ) => {
	return (
		<div className='left'>
			{utils.range(0, states.length - 1).map(value => 
				<button key={value} id={value} className={`number button-${states[value]}`}
					onClick={changeStateOnClick}>
					{value + 1}
				</button>
			)}
		</div>
	);
};

const Stars = ( {count} ) => {
	return (
		<div className='right'>
			{utils.range(1, count).map(value => 
				<div key={value} className='star' />
			)}
		</div>
	);
};

const Results = ( {items} ) => {
	return (
		<>
			{items.map(r => 
				<p key={`result-${r.number}`} className={`result result-${r.value}`}>
					{r.message}
				</p>
			)}
		</>
	);
};

const useGameEngine = () => {
	const [starsCount, setStarsCount] = useState(0);
	const [buttonStates, setButtonStates] = useState(utils.range(1, 9).map(() => INITIAL));
	const [candidateSum, setCandidateSum] = useState(0);
	const [usedButtonsCount, setUsedButtonsCount] = useState(0);
	const [results, setResults] = useState([]);

	const computeGameScore = () => {
		const successCount = results.reduce((acc, element) => {
			if (element.value === SUCCESS) {
				acc++;
			}
			return acc;
		}, 0);
		return Math.floor(successCount * 100 / results.length);
	};

	const createResultDetails = (value, number) => {
		const message = value === GAME_OVER ? `Game Over!  Result ${computeGameScore()}%` : `Take ${number}: ${value.toUpperCase()}`;
		return { number, message };
	};

	const addResult = value => {
		const number = results.length + 1;
		const { message } = createResultDetails(value, number);
		results.unshift( {number, value, message} );
		setResults(results);
	};

	const updateResult = value => {
		const { number } = results.shift();
		const { message } = createResultDetails(value, number);
		results.unshift( {number, value, message} );
		setResults(results);
	};

	const playNextRound = () => {
		if (starsCount > 0) {
			candidateSum === starsCount ? updateResult(SUCCESS) : updateResult(FAILED);
		}
		setStarsCount(Math.floor(1 + Math.random() * 9));
		setButtonStates(buttonStates.map(state => state === INITIAL ? AVAILABLE : state ));
		setCandidateSum(0);
		addResult(PENDING);
	};

	const getCandidateButtonState = () => {
		if (candidateSum < starsCount) return CANDIDATE;
		if (candidateSum > starsCount) return WRONG;
		return USED;
	}

	const getNewButtonState = oldState => {
		switch (oldState) {
		case USED:
		case INITIAL:
			return oldState;
		case AVAILABLE:
			return getCandidateButtonState();
		}
		return AVAILABLE;
	}

	const updateClickedButtonState = index => {
		const newButtonStates = buttonStates.map(state => state);
		newButtonStates[index] = getNewButtonState(newButtonStates[index]);
		setButtonStates(newButtonStates);
	}

	const updateCandidateSum = index => {
		const indexNum = parseInt(index);
		if (buttonStates[indexNum] == USED) return;
		const increment = buttonStates[indexNum] === AVAILABLE ? indexNum + 1 : -indexNum - 1;
		setCandidateSum(candidateSum + increment);
	}

	const updateAllCandidateButtonStates = () => {
		let newUsedButtonsCount = usedButtonsCount;
		setButtonStates(buttonStates.map(state => {
			if ([CANDIDATE, WRONG].find(s => s === state)) {
				const newState = getCandidateButtonState();
				if (newState === USED) {
					newUsedButtonsCount++;
				}
				return newState;
			}
			return state;
		}));
		setUsedButtonsCount(newUsedButtonsCount);
	};

	const startNewRoundOnCandidateSumMatch = () => {
		if (starsCount > 0 && candidateSum === starsCount) {
			updateResult(SUCCESS);
			playNextRound();
		}
	};

	const gameOverOnAllButtonsUsed = () => {
		if (usedButtonsCount === buttonStates.length) {
			updateResult(GAME_OVER);
			setStarsCount(0);
			setCandidateSum(0);
		}
	};

	useEffect(() => {
		updateAllCandidateButtonStates();
	}, [candidateSum]);

	useEffect(() => {
		startNewRoundOnCandidateSumMatch();
		gameOverOnAllButtonsUsed();
	}, [usedButtonsCount]);
	
	const changeStateOnClick = (e) => {
		updateCandidateSum(e.target.id);
		updateClickedButtonState(e.target.id);
	};
	
	const resetGame = () => {
		setStarsCount(0);
		setButtonStates(buttonStates.map(() => INITIAL));
		setCandidateSum(0);
		setResults([]);
	};

	console.log('render game', starsCount, candidateSum, usedButtonsCount);
	return {
		playNextRound, resetGame, changeStateOnClick,
		buttonStates, starsCount, results
	};
}

const Game = () => {
	const {
		playNextRound, resetGame, changeStateOnClick,
		buttonStates, starsCount, results
	} = useGameEngine();
	
	return (
		<div className='game'>
			<div className='control-buttons'>
				<button onClick={playNextRound}>Play</button>
				<button onClick={resetGame}>Reset</button>
			</div>
			<div className='body'>
				<Buttons states={buttonStates} changeStateOnClick={changeStateOnClick} />
				<Stars count={starsCount} />
			</div>
			<Results items={results} />
		</div>
	);
};

ReactDOM.render(<Game />, document.getElementById('mountNode'));
