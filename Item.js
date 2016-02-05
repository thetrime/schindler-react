var React = require('react');
var AppDispatcher = require('./AppDispatcher');


module.exports = React.createClass(
    {
        onClick: function()
        {
            this.props.onClick(this.props.item);
        },        
        render: function()
        {
            var label = this.props.label;
            return (<tr>
                    <td>{this.props.item.name}</td>
                    <td className="button_column"><button className="app_button" onClick={this.onClick}>{label}</button></td>
                    </tr>);
        }
    });
