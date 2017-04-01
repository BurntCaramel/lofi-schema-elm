require('./index.css')

const Elm = require('./Main.elm')
const app = Elm.Main.embed(document.querySelector('#app'));