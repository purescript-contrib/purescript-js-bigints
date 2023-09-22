export const fromStringImpl = (just) => (nothing) => (s) => {
  try {
    var x = BigInt(s);
    return just(x);
  } catch (err) {
    return nothing;
  }
};

// Pretty much copy/paste from https://stackoverflow.com/questions/55646698/base-36-to-bigint
export const fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {
      return function (_value) {

        // Preprocess for potentially negative numbers. Since the toString 
        // function simply prepends a '-' character for negative numbers, 
        // we need this to make fromStringAs an inverse function.
        var value, op;
        if (_value[0] === "-") {
          op = function (v) {return BigInt(-1) * v};
          value = _value.slice(1);
        } else {
          op = function (v) {return v};
          value = _value;
        }

        value.replace(/^0+/, '')

        var size = 6,
            factor = BigInt(radix ** size),
            i = value.length % size || size,
            parts = [value.slice(0, i)];

        while (i < value.length) parts.push(value.slice(i, i += size));

        function f(acc, chunk) {
          let n = parseInt(chunk, radix);
          if (isNaN(n)) {
            throw new Error("Invalid number");
          } else {
            return acc * factor + BigInt(n);
          }
        };
        
        try {
          return just(op(parts.reduce(f, 0n)));
        } catch (err) {
          return nothing;
        }
      };
    };
  };
};

export const fromNumberImpl = (just) => (nothing) => (n) => {
  try {
    var x = BigInt(n);
    return just(x);
  } catch (err) {
    return nothing;
  }
};

export const fromInt = (n) => BigInt(n);

export const fromTypeLevelInt = (str) => BigInt(str);

export const toNumber = (n) => Number(n);

export const biAdd = (x) => (y) => x + y;

export const biMul = (x) => (y) => x * y;

export const biSub = (x) => (y) => x - y;

export const biMod = (x) => (y) => {
  if (y === 0n) return 0n;
  const yy = y < 0n ? -y : y;
  return ((x % yy) + yy) % yy;
}

export const biDiv = (x) => (y) => {
  if (y === 0n) return 0n;
  return (x - biMod(x)(y)) / y;
}

export const biDegree = (x) => {
  return x < 0n ? -x : x;
  
}

export const biZero = 0n;

export const biOne = 1n;

export const pow = (x) => (y) => y >= 0n ?  x ** y : 0n;

export const not = (x) => ~x;

export const or = (x) => (y) => x | y;

export const xor = (x) => (y) => x ^ y;

export const and = (x) => (y) => x & y;

export const shl = (x) => (n) => x << n;

export const shr = (x) => (n) => x >> n;

export const biEquals = (x) => (y) => x == y;

export const biCompare = (x) => (y) => {
  if (x === y) return 0;
  else if (x > y) return 1;
  else return -1;
};

export const toString = (x) => x.toString();

export const asIntN = (bits) => (n) => BigInt.asIntN(bits, n);
export const asUintN = (bits) => (n) => BigInt.asUintN(bits, n);

export const toStringAs = (radix) => (i) => i.toString(radix);
