import './main.css';
import { Elm } from './Remake.elm';
import registerServiceWorker from './registerServiceWorker';

const app = Elm.Remake.init({
  node: document.getElementById('root'),
  flags: JSON.parse(localStorage.getItem('Quran-progress'))
});

//app.ports.saveProgress.subscribe(message => {
//   localStorage.setItem('Quran-progress', JSON.stringify(message))
//});


registerServiceWorker();
