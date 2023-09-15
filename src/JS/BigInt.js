export const fromStringImpl = (just) => (nothing) => (s) => {
  try {
    var x = BigInt(s);
    return just(x);
  } catch (err) {
    return nothing;
  }
};

export const fromStringAsImpl = function (just) {
  return function (nothing) {
    return function (radix) {

      var digits;
      if (radix < 11) {
        digits = "[0-" + (radix - 1).toString() + "]";
      } else if (radix === 11) {
        digits = "[0-9a]";
      } else {
        digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
      }
      var pattern = new RegExp("^[\\+\\-]?" + digits + "+$", "i");

      return function (s) {
        // Not yet in the standard: https://github.com/tc39/proposal-number-fromstring
        // Code converted from https://stackoverflow.com/a/55646905/1833322

        /* jshint bitwise: false */
        if (pattern.test(s)) {
          var size = 10;
          var factor = BigInt(radix ** size);
          var r = 0n

          for (var i = 0; i < s.length; i += size) {
            var n = parseInt(s.slice(i, i + size), radix);

            // check for NaN
            if ((n | 0) !== n) {
              return nothing;
            }

            r = r * factor + BigInt(n);
          }

          return just(r);
        } else {
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
  return x = x < 0n ? -x : x;
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
