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
      * Need an 'unknown store' then. Get rid of the 'home' store, and start here
      * If you tick an item off the list and you are at an unknown store, we need to prompt the user to identify the store. Allow them to say 'I forgot', and in that case, do not prompt them for an aisle as well
      * Also if all locations are deleted we would end up here
   * Undo
   * Details screen for items
   * Secure password management (or at least not totally insecure management!)
   * Gzip large items
   * Native version :P

Bugs:
   * Setting rotating the web-app version on my phone gives me the wrong viewport. This persists when I rotate back, too :(
*/
