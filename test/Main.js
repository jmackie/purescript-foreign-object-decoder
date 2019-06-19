"use-strict";

exports.good = {
  x: 2,
  y: function(foo) {
    return foo;
  },
  z: { zz: true }
};

exports.bad = { x: 2, y: "foo", z: { zz: true } };
