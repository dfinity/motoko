---
sidebar_position: 4
---

# Motoko.js



The Motoko.js package can be used to compile and run Motoko smart contracts in a web browser or Node.js.

## Installation

To install the Motoko.js package, use `npm`:

```
npm i --save motoko
```

## Usage

First, create a new Node.js project with the commands:

```
mkdir motoko-js-test
cd motoko-js-test
npm init
```

In the `package.json` file created by `npm init`, insert the following line:

```
  "type": "module",
```

Then, create and open an `index.js` file. In this new file, import the Motoko.js package into your source code file:

```
import mo from 'motoko';
```

Create a Motoko script using the Node.js virtual file system:

```
mo.write('Main.mo', `
  actor Main {
    public query func hello() : async Text {
      "Hello, world!"
    };
  };
`)
console.log(mo.run('Main.mo'));
```

Next, add a final line to generate the corresponding Candid interface for the Motoko script:

```
console.log(mo.candid('Main.mo'));
```

Run this code with the command:

```
node index.js
```

The console will return the following output:

```
{
  stdout: '`ys6dh-5cjiq-5dc` : actor {hello : shared query () -> async Text}\n',
  stderr: '',
  result: { error: null }
}
service : {
  hello: () -> (text) query;
}
```


## References

- [npm documentation](https://www.npmjs.com/package/motoko)

- [Load dependencies from GitHub](https://github.com/dfinity/node-motoko?tab=readme-ov-file#load-dependencies-from-github)

- [Optimize for browsers](https://github.com/dfinity/node-motoko?tab=readme-ov-file#optimize-for-browsers)

- [Top level API](https://github.com/dfinity/node-motoko?tab=readme-ov-file#top-level-api)

- [File API](https://github.com/dfinity/node-motoko?tab=readme-ov-file#file-api)


<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />
