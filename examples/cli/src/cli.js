const Elm = require('../main')

const format = process.argv[2]
if (!format) {
  throw 'You must provide a format'
}

const app = Elm.Main.worker({
  collectionName: 'things',
  individualName: 'thing',
  input: [
    "First name",
    "Middle name #optional",
    "Last name #text #max: 255",
    "Email #email",
    "Date of birth #date"
  ].join("\n")
});

app.ports.conversionComplete.subscribe(([format, content]) => {
  process.stdout.write(content)
});

app.ports.beginConversion.send(format);
