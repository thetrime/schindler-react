var React = require('react');
var ReactDOM = require('react-dom');

var ServerConnection = require('./ServerConnection');
var SchindlerApp = require('./SchindlerApp');
var GPSTracker = require('./GPSTracker');

ServerConnection.initialize();
GPSTracker.initialize();
ReactDOM.render(<div className="root vertical_layout">
                <SchindlerApp mode="login"/>
                </div>,
                document.getElementById("container"));



/* To do list:
   * Geolocation to move between stores automatically
   * Undo
   * Details screen for items
   * Defer mode and relocate mode. (Maybe put this inside the details screen?)
   * Secure password management (or at least not totally insecure management!)
   * Gzip large items
   * Native version :P

Bugs:

*/
