import { Elm } from '../src/Main.elm'

const Storage = {
  save: (saveData) => localStorage.setItem('saveData', JSON.stringify(saveData)),
  load: () => JSON.parse(localStorage.getItem('saveData')) || null
}

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: {
    window: {
      width: window.innerWidth,
      height: window.innerHeight
    },
    currentTime: Date.now(),
    saveData: Storage.load()
  }
})

if (app.ports && app.ports.save) {
  app.ports.save.subscribe(saveData => Storage.save(saveData))
}