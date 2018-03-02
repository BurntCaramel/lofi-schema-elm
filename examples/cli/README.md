# Lofi Schema

## CLI

```
npm i lofi-schema -g
# or yarn
yarn global add lofi-schema
```

```
lofi-schema graphql -s path/to/file.lofi
lofi-schema go -s path/to/file.lofi
lofi-schema mongoose -s path/to/file.lofi
lofi-schema swift -s path/to/file.lofi
```

## Module

```
npm i lofi-schema -S
# or yarn
yarn add lofi-schema
```

```js
import lofiSchema from 'lofi-schema';

const input = `
title
yearReleased #number
`;

lofiSchema.onConverted((error, content, format) => {
  if (error) {
    // Handle error
  }
  else {
    // Use content
  }
});

lofiSchema.startConversion({
  inputFormat: 'lofi',
  input,
  outputFormat: 'graphql'
});

lofiSchema.startConversion({
  inputFormat: 'lofi',
  input,
  outputFormat: 'go'
});

lofiSchema.startConversion({
  inputFormat: 'lofi',
  input,
  outputFormat: 'swift'
});
```
