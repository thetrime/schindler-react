var ShoppingList = React.createClass(
    {
        getInitialState: function()
        {
            return {filterText: ''};
        },

        redoSearch: function(value)
        {
            this.setState({filterText: value});
        },
        
        render: function()
        {
            return (<div>
                    <SearchBox filterText={this.state.filterText} redoSearch={this.redoSearch}/>
                    <ItemTable items={this.props.items} filterText={this.state.filterText} redoSearch={this.redoSearch}/>
                    </div>);
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
            return (<form>
                    <input type="text" placeholder="Search..." className="search_field" onChange={this.searchChanged} value={this.props.filterText} ref="searchField"/>
                    </form>);
        }
    });

var ItemTable = React.createClass(
    {
        getInitialState : function()
        {
            return {items:this.props.items};
        },        
        gotItem: function(name)
        {
            var new_items = [];
            this.state.items.forEach(function(item)
                                     {
                                         if (item.name != name)
                                             new_items.push(item);
                                     });
            this.setState({items:new_items});
        },
        addItem: function(name)
        {
            var new_items = this.state.items;
            new_items.push({name: name,
                            location: 'Unknown'});
            this.setState({items:new_items});
            this.props.redoSearch('');
        },
        render: function()
        {
            var rows = [];
            var groups = {};
            var filter = this.props.filterText;
            var exactMatch = false;
            this.state.items.forEach(function(item)
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
            return (<table className="item_table">
                      <thead>
                      </thead>
                      <tbody>
                        {rows}
                      </tbody>
                    </table>);
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


var websocket = null;
function initialize()
{
    var items = [];
    websocket = new WebSocket("ws://localhost:9999/ws");
    websocket.onmessage = handle_server_message;
    websocket.onopen = function()
    {
        websocket.send(JSON.stringify({operation:"hello",
                                       version:1}));
        
    };

}

function handle_server_message(event)
{
    var msg = JSON.parse(event.data);
    if (msg.operation == "list")
    {
        ReactDOM.render(<ShoppingList items={msg.items}/>,                        
                        document.getElementById("container"));
    }
}

initialize();
