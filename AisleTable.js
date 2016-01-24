var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');

module.exports = React.createClass(
    {       
        addAisle: function(name)
        {
            ServerConnection.sendMessage({operation:"add_aisle", name:name});
            this.props.redoSearch('');
        },
        render: function()
        {
            var rows = [];
            var filter = this.props.filterText;
            var exactMatch = false;
            Object.aisles.sort().forEach(function(aisle)
                                         {
                                             rows.push(<tr><td>{aisle.name}</td></tr>);
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
