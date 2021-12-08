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
	const [featureFlags, setFeatureFlags] = useState({
		showRoundBanner: false,
		hasTimer: false
	});
	const [playButtonDisabled, setPlayButtonDisabled] = useState(false);
	const [roundBannerHidden, setRoundBannerHidden] = useState(true);
	const wasRoundBannerShown = useRef();
	const timerInterval = useRef();
	const [timerHidden, setTimerHidden] = useState(true);
	const [timerSeconds, setTimerSeconds] = useState(3);
	const [timerTick, setTimerTick] = useState(3);
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

	const clearTimer = () => {
		if (!featureFlags.hasTimer) return;
		setTimerHidden(true);
		clearTimeout(timerInterval.current);
		timerInterval.current = null;
	};

	const startTimer = () => {
		if (!featureFlags.hasTimer) return;
		timerInterval.current = setTimeout(() => {
			console.log('tick!', timerTick);
			setTimerTick(timerTick - 1);
		}, 1 * 1000);
		setTimerHidden(false);
	};

	useEffect(() => {
		if (timerTick === 0) {
			playNextRound();
		} else if (starsCount > 0) {
			clearTimer();
			startTimer();
		}
	}, [timerTick]);

	const startRound = () => {
		if (starsCount > 0) {
			candidateSum === starsCount ? updateResult(SUCCESS) : updateResult(FAILED);
		}
		setStarsCount(Math.floor(1 + Math.random() * 9));
		setButtonStates(buttonStates.map(state => state === INITIAL ? AVAILABLE : state ));
		setCandidateSum(0);
		addResult(PENDING);
		setTimerTick(timerSeconds);
		clearTimer();
		startTimer();
	};

	const playNextRound = () => {
		if (!featureFlags.showRoundBanner) {
			startRound();
			return;
		}
		setTimeout(() => {
			setRoundBannerHidden(true);
		}, 0.5 * 1000);
		setRoundBannerHidden(false);
		wasRoundBannerShown.current = true;
	};

	useEffect(() => {
		wasRoundBannerShown.current = false;
		timerInterval.current = null;
	}, []);

	useEffect(() => {
		if (wasRoundBannerShown.current && roundBannerHidden) {
			startRound();
		}
	}, [roundBannerHidden]);

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
		if ([USED, INITIAL].find(state => state === buttonStates[indexNum])) return;
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

	const newRoundOrGameOver = () => {
		if (starsCount > 0 && candidateSum === starsCount) {
			updateResult(SUCCESS);
			if (usedButtonsCount === buttonStates.length) {
				clearTimer();
				setPlayButtonDisabled(true);
				addResult(GAME_OVER);
				setStarsCount(0);
				setCandidateSum(0);
				return;
			} else {
				playNextRound();
			}
		}
	};

	useEffect(() => {
		updateAllCandidateButtonStates();
	}, [candidateSum]);

	useEffect(() => {
		newRoundOrGameOver();
	}, [usedButtonsCount]);
	
	const changeStateOnClick = (e) => {
		updateCandidateSum(e.target.id);
		updateClickedButtonState(e.target.id);
	};
	
	const resetGame = () => {
		clearTimer();
		setStarsCount(0);
		setButtonStates(buttonStates.map(() => INITIAL));
		setCandidateSum(0);
		setUsedButtonsCount(0);
		setResults([]);
		setPlayButtonDisabled(false);
	};

	console.log('render game', starsCount, candidateSum, usedButtonsCount, timerTick);
	return {
		featureFlags, setFeatureFlags,
		playNextRound, resetGame, changeStateOnClick,
		roundBannerHidden,
		timerHidden, timerTick, timerSeconds,
		playButtonDisabled, buttonStates, starsCount, results
	};
}

const Game = () => {
	const {
		featureFlags, setFeatureFlags,
		playNextRound, resetGame, changeStateOnClick,
		roundBannerHidden,
		timerHidden, timerTick, timerSeconds,
		playButtonDisabled, buttonStates, starsCount, results
	} = useGameEngine();
	
	return (
		<div className="game">
			<div className="round-banner" hidden={roundBannerHidden}>
				New Round!
			</div>
			<div className="control-buttons">
				<button onClick={playNextRound} disabled={playButtonDisabled}>Play</button>
				<label>
					<input
						name="showRoundBanner"
						type="checkbox"
						defaultChecked={featureFlags.showRoundBanner}
						onChange={() => setFeatureFlags({...featureFlags, showRoundBanner: !featureFlags.showRoundBanner})} />
					Banner?
				</label>
				<button onClick={resetGame}>Reset</button>
			</div>
			<div className="body">
				<Buttons states={buttonStates} changeStateOnClick={changeStateOnClick} />
				<Stars count={starsCount} />
			</div>
			<div className="timer" hidden={timerHidden}>
				{timerTick}
			</div>
			<Results items={results} />
		</div>
	);
};

ReactDOM.render(<Game />, document.getElementById('mountNode'));
