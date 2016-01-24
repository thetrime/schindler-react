var React = require('react');
var AppDispatcher = require('./AppDispatcher');

module.exports = React.createClass(
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