var React = require('react');
var ReactDOM = require('react-dom');

var ServerConnection = require('./ServerConnection');
var SchindlerApp = require('./SchindlerApp');

ServerConnection.initialize();
ReactDOM.render(<div className="root vertical_layout">
                <SchindlerApp mode="shop"/>
                </div>,
                document.getElementById("container"));



/* To do list:
   * Restructure to use Flux, now that I understand data flow
   * Geolocation for store (and ability to change store)
   * Undo
   * Details screen for items
   * Defer mode and relocate mode.
   * Multiple users
   * Gzip large items
   * Native version :P

Bugs:
   * I think an aisle with no items will never be represented at a store

*/
