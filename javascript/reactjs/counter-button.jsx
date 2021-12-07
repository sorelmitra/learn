const CounterButton = ( {updateCount, increment} ) => {
  const click = (e) => {
    e.preventDefault();
    updateCount(increment);
  }
  return (
    <div class="buttonDiv">
      <button
        id="count"
        onClick={click}>
        +{increment}
      </button>
    </div>
  );
};

const DisplayCounter = ( {counter} ) => (
  <div>{counter}</div>
);

const WrapperForm = () => {
  const [count, setCount] = useState(0);
  const updateCount = (increment) => setCount(count + increment);
  return (
    <div class="formDiv">
      <form>
        <label>Hello, world!</label>
        <CounterButton updateCount={updateCount} increment={1} />
        <CounterButton updateCount={updateCount} increment={5} />
        <CounterButton updateCount={updateCount} increment={20} />
        <CounterButton updateCount={updateCount} increment={100} />
        <DisplayCounter counter={count} />
      </form>
    </div>
  );
};

const root = document.getElementById('mountNode');
ReactDOM.render(<WrapperForm /> , root);

