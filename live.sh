#!/bin/bash
watchify schindler.jsx -v -o build/bundle.js -t [ babelify --presets [ es2015 react ] ]
