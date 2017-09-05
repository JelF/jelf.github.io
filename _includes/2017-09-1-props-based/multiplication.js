#!/usr/bin/env node

function* multiplication(x, y) {
  let xs = [], ys = []

  for(;;) {
    const x1 = x.next()
    if (!x1.done) {
      xs.push(x1.value)
      for(let i in ys) { yield [x1.value, ys[i]] }
    }

    const y1 = y.next()

    if (!y1.done) {
      ys.push(y1.value)
      for(let i in xs) { yield [xs[i], y1.value] }
    }

    if (x1.done && y1.done) { return }
  }
}

module.exports = multiplication
