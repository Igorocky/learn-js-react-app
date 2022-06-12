import logo from './logo.svg';
import './App.css';
import Cmp1 from "./Cmp1";
import {make as Cmp2} from "./Cmp2.bs";

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
        <Cmp1/>
        <Cmp2/>
      </header>
    </div>
  );
}

export default App;
