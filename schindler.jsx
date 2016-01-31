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
   * Prolog backdoor listening for connections from localhost only, for talking to the server in headless mode
   * Native version :P

I *think* that state is something that must be preserved between renders. Things like: The value in an input which a user has started typing?

I think I need a more complicated thing for my websocket; something you can subscribe to events on (like onClose), and subscribe to messages with a particular tag.
Then I can pass this thing around as a property. Components can listen for events on it and call setState() as needed.

I should then add on componentDidMount, and remove it on componentDidUmount


*/
