#!/bin/bash
beefy three.js:bundle.js --live -- three.js -o bundle.js  -t [ babelify --presets [ es2015 react ] ]
