"use-strict";

exports.exampleA = { x: 2 };

exports.exampleB = { a: "string", b: "c", c: true, d: 200.0, e: 200 };

exports.exampleC = { strings: ["foo", "bar"] };

exports.exampleD = { nested: { x: 24.0, y: 42.0 } };

exports.exampleE = {
  callback: function(x) {
    return x;
  }
};
