module.exports = {
  type: 'web-app',
  webpack: {
    extra: {
      module: {
        //noParse: [/.elm$/],
        rules: [{
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-webpack-loader?verbose=true&warn=true',
        }]
      }
    }
  }
}
