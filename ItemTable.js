var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');
var Location = require('./Location');
var Item = require('./Item');
var NewItem = require('./NewItem');
var StoreStore = require('./StoreStore');

module.exports = React.createClass(
    {     
        addItem: function(item)
        {
            AppDispatcher.dispatch({operation:"new_item",
                                    data:{name:item.name}});
            this.props.redoSearch('');
        },
        wantItem: function(item)
        {
            AppDispatcher.dispatch({operation:"want_item",
                                    data:{name:item.name}});
            this.props.redoSearch('');
        },
        gotItem: function(item)
        {
            AppDispatcher.dispatch({operation:"got_item",
                                    data:{name:item.name,
                                          location:item.location}});
        },       
        render: function()
        {
            var rows = [];
            var groups = {};
            var filter = this.props.filterText;
            var exactMatch = false;
            var allItems = this.props.model;
            if (filter != '')
            {
                // Need to determine which of these items are in this.props.model, and mark them appropriately so that we can
                // distinguish 'Add Item' from 'Got It!' and call the appropriate function when clicked
                // FIXME: This is really inefficient. We must be able to do better...
                allItems = StoreStore.getItemsForCurrentStore();
                this.props.model.forEach(function(list_item)
                                         {
                                             for (var i = 0; i < allItems.length; i++)
                                             {
                                                 if (allItems[i].name == list_item.name)
                                                     allItems[i].on_list = true;
                                             }
                                         });
            }
            allItems.forEach(function(item)
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
                rows.push(<NewItem name={filter} key={filter} addItem={this.addItem} label="Add New Item"/>);
            }
            Object.keys(groups).sort().forEach(function(group)
                           {
                               rows.push(<Location key={groups[group].location} location={groups[group].location}/>);
                               groups[group].items.forEach(function(item)
                                                           {
                                                               rows.push(<Item item={item}
                                                                         key={item.name}
                                                                         onClick={item.on_list?table.gotItem:table.wantItem}
                                                                         label={item.on_list?"Got It!":"Add Item"}/>);
                                                           });
                           });
            return (<div className="table_container vertical_fill">
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
