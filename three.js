var SchindlerApp = React.createClass(
    {
        getInitialState: function()
        {
            return {currentView: "shop",
                    connectionStatus: "disconnected"};
        },

        switchMode: function(newMode)
        {
            setState({currentMode: newMode});
        },
        onConnect: function()
        {
            this.setState({connectionStatus:"connected"});
        },
        onDisconnect: function()
        {
            this.setState({connectionStatus:"disconnected"});
        },
        
        componentWillMount: function()
        {
            server_connection.registerFor("connect", this.onConnect);
            server_connection.registerFor("disconnect", this.onDisconnect);
            
        },
        
        render: function()
        {
            if (this.state.currentView == "shop")
                return (<div>
                        <ShoppingView model={model}/>
                        <ConnectionInfo status={this.state.connectionStatus}/>
                        </div>);
            else if (this.state.currentView == "select_aisle")
                return (<div>
                        Select your aisle panel goes here
                        <ConnectionInfo status={this.state.connectionStatus}/>
                        </div>);
        }
        
    });

var ShoppingView = React.createClass(
    {
        getInitialState: function()
        {
            return {filterText: '',
                    items: []};
        },

        redoSearch: function(value)
        {
            this.setState({filterText: value});
        },

        onListChange: function(data)
        {
            this.setState({items: data.items});
        },

        gotItem: function(item)
        {
            this.setState({items: this.state.items.filter(function(existing_item)
                                                          {
                                                              return existing_item.name != item.name;
                                                          })});
        },

        addItem: function(item)
        {
            this.setState({items: this.state.items.concat([item])});
        },
        
        componentWillMount: function()
        {
            server_connection.registerFor("list", this.onListChange);
            server_connection.registerFor("got_item_ack", this.gotItem);
            server_connection.registerFor("add_item_ack", this.addItem);
        },
        
        render: function()
        {
            return (<div>
                    <SearchBox filterText={this.state.filterText} redoSearch={this.redoSearch}/>
                    <ItemTable model={this.state.items} filterText={this.state.filterText} redoSearch={this.redoSearch}/>
                    </div>);
        }
    });

var ConnectionInfo = React.createClass(
    {
        render: function()
        {
            if (this.props.status == "connected")
                return (<div/>);
            else
                return (<div className="connection_bar">Reestablishing connection...</div>);
        }
    });

var SearchBox = React.createClass(
    {
        searchChanged : function()
        {
            this.props.redoSearch(this.refs.searchField.value);
        },
        
        render: function()
        {
            return (<form className="search_bar">
                    <input type="search" placeholder="Search..." className="search_field" autoCapitalize="off" onChange={this.searchChanged} value={this.props.filterText} ref="searchField"/>
                    </form>);
        }
    });

var ItemTable = React.createClass(
    {
        gotItem: function(name)
        {
            // Here we need to change the entire app to aisle-select (and possibly store-select!) view.
            // This is why 2-way binding between views ends up as a total mess
            server_connection.sendMessage({operation:"got_item", name:name});
        },
        addItem: function(name)
        {
            server_connection.sendMessage({operation:"add_item", name:name});
            this.props.redoSearch('');
        },
        render: function()
        {
            var rows = [];
            var groups = {};
            var filter = this.props.filterText;
            var exactMatch = false;
            this.props.model.forEach(function(item)
                                     {
                                         if (filter != '' && item.name.indexOf(filter) != 0)
                                             return;
                                         if (filter != '' && item.name == filter)
                                             exactMatch = true;
                                         if (groups[item.location] === undefined)
                                             groups[item.location] = {location: item.location,
                                                                      items: [item]};
                                         else
                                             groups[item.location].items.push(item);
                                     });
            var table = this;
            if (filter != '' && !exactMatch)
            {
                rows.push(<NewItem name={filter} key={filter} addItem={this.addItem}/>);
            }            
            Object.keys(groups).sort().forEach(function(group)
                           {
                               rows.push(<Location key={groups[group].location} location={groups[group].location}/>);
                               groups[group].items.forEach(function(item)
                                                           {
                                                               rows.push(<Item name={item.name} key={item.name} gotItem={table.gotItem}/>);
                                                           });
                           });
            return (<div className="table_container">
                    <table className="item_table">
                    <thead>
                    </thead>
                    <tbody>
                    {rows}
                    </tbody>
                    </table>
                    </div>);
        }
    });

var Location = React.createClass(
    {
        render: function()
        {
            return (<tr>
                    <th colSpan="2">
                    {this.props.location}
                    </th>
                    </tr>);
                    
        }
    });

var Item = React.createClass(
    {
        gotIt: function()
        {
            this.props.gotItem(this.props.name);
        },        
        render: function()
        {
            return (<tr>
                    <td>{this.props.name}</td>
                    <td className="button_column"><button onClick={this.gotIt}>Got it!</button></td>
                    </tr>);
        }
    });

var NewItem = React.createClass(
    {
        addItem: function()
        {
            this.props.addItem(this.props.name);
        },
        render: function()
        {
            return (<tr>
                    <td>{this.props.name}</td>
                    <td className="button_column"><button onClick={this.addItem}>Add New Item</button></td>
                    </tr>);
            
        }
    });

var model = [];
var server_connection =
    {
        websocket: null,
        listeners: {},        
        handle_server_connect: function()
        {
            this.sendMessage({operation:"hello",version:1});
            this.dispatchEvent("connect");
        },
        handle_server_disconnect: function()
        {
            console.log("Close detected. Reopening connection in 3 seconds...");
            this.dispatchEvent("disconnect");
            var that = this;
            setTimeout(function() {that.sendMessage({operation:"ping"})}, 3000);
        },
        dispatchEvent: function(key, data)
        {
            console.log("Dispatching " + key);
            if (this.listeners[key] !== undefined)
            {
                this.listeners[key].forEach(function(fn)  { fn(data); });
            }
        },
        registerFor: function(event, callback)
        {
            if (this.listeners[event] === undefined)
                this.listeners[event] = [callback];
            else
                this.listeners[event].push(callback);
        },
        sendMessage: function(message)
        {
            if (this.websocket.readyState == this.websocket.OPEN)
            {
                console.log("Connection OK");
                this.websocket.send(JSON.stringify(message));
            }
            else
            {
                // FIXME: The message actually doesn't get sent here?
                console.log("Connection lost....");
                dispatchEvent("disconnect");
                var new_websocket = new WebSocket(uri);
                new_websocket.onmessage = this.websocket.onmessage;
                new_websocket.onopen = this.websocket.onopen;
                new_websocket.onclose = this.websocket.onclose;
                new_websocket.onerror = this.websocket.onerror;
                this.websocket = new_websocket;
            }
        },
        handle_server_message: function(event)
        {
            var msg = JSON.parse(event.data);
            this.dispatchEvent(msg.operation, msg.data);
        },
        initialize: function()
        {
            var loc = window.location;
            var uri = "ws:";
            if (loc.protocol === "https:") 
                uri = "wss:";
            uri += "//" + loc.host;
            uri += loc.pathname + "ws";
            console.log(uri);
            this.websocket = new WebSocket(uri);
            this.websocket.onmessage = this.handle_server_message.bind(this);
            this.websocket.onclose = this.handle_server_disconnect.bind(this);
            this.websocket.onopen = this.handle_server_connect.bind(this);
        }
    };

server_connection.initialize();
ReactDOM.render(<SchindlerApp mode="shop"/>,
                document.getElementById("container"));


initialize();


/* To do list:
   * Restructure to use Flux, now that I understand data flow
   * Store list in database
   * Geolocation for store
   * Configure location if unknown when purchased
   * Maintenance screen for items
   * Maintenance screen for stores
   * Undo
   * Details screen for items
   * Defer mode and relocate mode.
   * Multiple users
   * Offline mode?
      * Queue messages for delivery to server
      * All messages must therefore be relative; applied in different orders?
      * Need to keep some state on the client then (the queued messages plus the current state)
   * Native version :P

I *think* that state is something that must be preserved between renders. Things like: The value in an input which a user has started typing?

I think I need a more complicated thing for my websocket; something you can subscribe to events on (like onClose), and subscribe to messages with a particular tag.
Then I can pass this thing around as a property. Components can listen for events on it and call setState() as needed.

I should then add on componentDidMount, and remove it on componentDidUmount


*/
