import { startConversion, subscribeToConversion } from './worker'

export default {
  startConversion,
  onConverted: subscribeToConversion
}
