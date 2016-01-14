var ShoppingList = React.createClass(
    {
        render: function()
        {
            return (<div>
                      <SearchBox/>
                      <ItemTable items={this.props.items}/>
                    </div>);
        }
    });

var SearchBox = React.createClass(
    {
        render: function()
        {
            return (<form>
                    <input type="text" placeholder="Search..." className="search_field"/>
                    </form>);
        }
    });

var ItemTable = React.createClass(
    {
        getInitialState : function()
        {
            return {items:ITEMS};
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
        render: function()
        {
            var rows = [];
            var groups = {};
            this.state.items.forEach(function(item)
                                     {
                                         if (groups[item.location] === undefined)
                                             groups[item.location] = {location: item.location,
                                                                      items: [item]};
                                         else
                                             groups[item.location].items.push(item);
                                     });
            var table = this;
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

var ITEMS = [{name:'apple', location:'lounge'},
             {name:'orange', location:'lounge'},
             {name:'guitar', location:'kitchen'},
             {name:'banjo', location:'kitchen'},
             {name:'soap', location:'bathroom'},
             {name:'chutney', location:'bathroom'},
             {name:'fish paste', location:'bathroom'},
             {name:'gruel', location:'bathroom'},
             {name:'pillow', location:'bedroom'},
             {name:'boxes', location:'bedroom'},
             {name:'apes', location:'bedroom'},
             {name:'trampoline', location:'bedroom'},
             {name:'curtains', location:'bedroom'},
             {name:'floor', location:'bathroom'},
             {name:'old timey jig', location:'kitchen'},
             {name:'clocks', location:'lounge'},
             {name:'banana', location:'lounge'},
             {name:'guava', location:'lounge'},
             {name:'peach', location:'lounge'},
             {name:'durian', location:'lounge'},
             {name:'mango', location:'lounge'}];

ReactDOM.render(<ShoppingList items={ITEMS}/>,
                document.getElementById("container"));
