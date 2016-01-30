var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');
var NewItem = require('./NewItem');
var Item = require('./Item');

module.exports = React.createClass(
    {       
        addAisle: function(name)
        {
            AppDispatcher.dispatch({operation:"new_aisle",
                                    data:{name:name,
                                          store:this.props.store}});

            this.props.redoSearch('');
        },
        setAisle: function(name)
        {
        },
        render: function()
        {
            var rows = [];
            var filter = this.props.filterText;
            var exactMatch = false;
            if (filter != '' && !exactMatch)
            {
                rows.push(<NewItem name={filter} key={filter} addItem={this.addAisle}/>);
            }
            var table = this;
            this.props.aisles.sort().forEach(function(aisle)
                                             {
                                                 rows.push(<Item item={aisle} key={aisle.name} gotItem={table.setAisle} label='Select Aisle'/>);
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
