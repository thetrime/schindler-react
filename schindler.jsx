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
      * Need an 'unknown location' then. Get rid of the 'home' location, and start here
      * Also if all locations are deleted we would end up here
   * Undo
   * Details screen for items
   * Secure password management (or at least not totally insecure management!)
   * Gzip large items
   * Native version :P

Bugs:
   * Setting rotating the web-app version on my phone gives me the wrong viewport. This persists when I rotate back, too :(
*/
