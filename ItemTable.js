var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');
var Location = require('./Location');
var Item = require('./Item');
var NewItem = require('./NewItem');

module.exports = React.createClass(
    {     
        addItem: function(name)
        {
            AppDispatcher.dispatch({operation:"new_item",
                                    data:{name:name}});
            this.props.redoSearch('');
        },
        gotItem: function(item)
        {
            AppDispatcher.dispatch({operation:"got_item",
                                    data:item});
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
                rows.push(<NewItem name={filter} key={filter} addItem={this.addItem} label="Add New Item"/>);
            }            
            Object.keys(groups).sort().forEach(function(group)
                           {
                               rows.push(<Location key={groups[group].location} location={groups[group].location}/>);
                               groups[group].items.forEach(function(item)
                                                           {
                                                               rows.push(<Item item={item} key={item.name} onClick={table.gotItem} label="Got It!"/>);
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
