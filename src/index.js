import Elm from './Main.elm'
import Bright from '../../brightdb/src/Bright.js'
import RandomName from 'random-name'

let bright = new Bright(WebSocket)
let dataspace = 'local.pisys.eu'
let instanceUri = dataspace + '/' + RandomName.first().toLowerCase()
let app = Elm.Main.fullscreen(instanceUri)

app.ports.outPort.subscribe(message => {
  bright.message('app', message)
})

bright.message('app', {type:'register', uri : instanceUri})
bright.on('message', (target, message) => {
  switch(message.type) {
    case 'registered':
      bright.message('app', {type:'connect', dataspace : dataspace})
      break
    default:
      app.ports.inPort.send(message)
  }
})
