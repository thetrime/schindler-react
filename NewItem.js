var React = require('react');
var AppDispatcher = require('./AppDispatcher');

module.exports = React.createClass(
    {
        addItem: function()
        {
            this.props.addItem({name:this.props.name});
        },
        render: function()
        {
            return (<tr>
                    <td>{this.props.name}</td>
                    <td className="button_column" colSpan="2"><button className="app_button add_button" onClick={this.addItem}></button></td>
                    </tr>);
            
        }
    });
