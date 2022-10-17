# purescript-js-bigints

ffi bindings for JavaScript's [`BigInt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt).

**Note:** There is also [`purescript-bigints`](https://github.com/purescript-contrib/purescript-bigints), which wraps the [BigInteger.js](https://github.com/peterolson/BigInteger.js) js library that emulated big integers before the arrival of `BigInt` and is now basically just a polyfill.
`purescript-js-bigints` on the other hand provides close and direct ffi bindings to the new native `BigInt` and therefore doesn't require any external npm dependency. This library has been adapted from the original `purescript-bigints`.

## Installation

```bash
spago install js-bigints
```
