# purescript-js-bigints

ffi bindings for JavaScript's [`BigInt`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt).

**Note:** There is also [`purescript-bigints`](https://github.com/purescript-contrib/purescript-bigints), which wraps the [BigInteger.js](https://github.com/peterolson/BigInteger.js) js library that emulated big integers before the arrival of `BigInt`.
`purescript-js-bigints` on the other hand provides close ffi bindings to the new native `BigInt` and therefore doesn't require an external npm dependency. This library is basically an adoption of `purescript-bigints`.

## Installation

```bash
spago install js-bigints
```
