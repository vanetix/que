# Que

![stack test](https://github.com/vanetix/que/workflows/stack%20test/badge.svg)
[![CircleCI](https://circleci.com/gh/vanetix/que.svg?style=svg)](https://circleci.com/gh/vanetix/que)

A Todo tracking CLI tool written in Haskell for learning purposes.

## Building

Project uses [stack](https://docs.haskellstack.org/en/stable/README/) as it's build tool.

- Ensure you have the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) installed.
  - For OSX `brew install ghc haskell-stack`
- `git clone git@github.com:vanetix/que.git && cd que`
- `stack build`

## Testing

Unit tests can be ran with `stack test`.

## Install

Que can be installed to the stack path with `stack install`.

## License

Copyright 2020 Matt McFarland

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
