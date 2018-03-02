var Elm = require('../main');
var format = process.argv[2];
if (!format) {
    throw 'You must provide a format';
}
var app = Elm.Main.worker({
    collectionName: 'things',
    individualName: 'thing',
    input: ["First name","Middle name #optional","Last name #text #max: 255","Email #email",
        "Date of birth #date"].join("\n")
});
app.ports.conversionComplete.subscribe(function (ref) {
    var format = ref[0];
    var content = ref[1];

    process.stdout.write(content);
});
app.ports.beginConversion.send(format);
//# sourceMappingURL=lofi-schema.js.map
