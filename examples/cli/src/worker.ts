declare var require: any

const Elm = require('../main')

const app = Elm.Main.worker({});

export type Request = {
  inputFormat: string,
  input: string,
  outputFormat: string
}

export function startConversion({
  inputFormat,
  input,
  outputFormat
}: Request) {
  app.ports.beginConversion.send({
    collectionName: 'items',
    individualName: 'item',
    inputFormat,
    input,
    outputFormat
  })
}

export function subscribeToConversion(callback: (error: Error | undefined, result: string | undefined, format: string) => any) {
  app.ports.conversionComplete.subscribe(({ format, errorMessage, result }) => {
    callback(errorMessage ? new Error(errorMessage) : undefined, result, format)
  })
}
