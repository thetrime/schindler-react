var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');
var NewItem = require('./NewItem');
var Item = require('./Item');

module.exports = React.createClass(
    {       
        selectAisle: function(aisle)
        {
            AppDispatcher.dispatch({operation:"set_item_location",
                                    origin:"client",
                                    data:{location:aisle.name,
                                          item:this.props.item,
                                          store:this.props.store}});
            // Also delete the item from the list
            AppDispatcher.dispatch({operation:"got_item",
                                    data:{location:aisle,
                                          name:this.props.item}});

        },
        render: function()
        {
            var rows = [];
            var filter = this.props.filterText;
            var exactMatch = false;
            if (filter != '' && !exactMatch)
            {
                rows.push(<NewItem name={filter} key={filter} addItem={this.selectAisle} label="Add New Aisle"/>);
            }
            var table = this;
            this.props.aisles.sort().forEach(function(aisle)
                                             {
                                                 rows.push(<Item item={aisle} key={aisle.name} onClick={table.selectAisle} label='Select Aisle'/>);
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
