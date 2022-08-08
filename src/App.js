import logo from './logo.svg';
import './App.css';
import Cmp1 from "./Cmp1";
import {make as Cmp2} from "./Cmp2.bs";
import {make as ViewSelector} from '@expln/utils/src/main/Expln_React_ViewSelector.bs';
import {make as JsonParseView} from "./JsonParseView.bs";

function App() {
  return <ViewSelector allViews = {[
    {id: 1, title: "JsonParseView", render: _ => <JsonParseView/>}
  ]}/>
}

export default App;
