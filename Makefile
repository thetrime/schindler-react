all:	bundle.js



bundle.js: three.js
	browserify three.js -o bundle.js  -t [ babelify --presets [ es2015 react ] ]


# watchify three.js -v -o build/bundle.js  -t [babelify [--presets es2015,react]]
