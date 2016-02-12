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
            var className = "app_button " + this.props.label + "_button";
            if (this.props.settings === undefined)
            {
                return (<tr>
                        <td>{this.props.item.name}</td>
                        <td className = "button_column" colSpan="2"><button className={className} onClick={this.onClick}></button></td>
                        </tr>);
            }
            else
            {
                return (<tr>
                        <td>{this.props.item.name}</td>
                        <td className = "settings_column"><button className="app_button settings_button" onClick={this.settings}></button></td>
                        <td className = "button_column"><button className={className} onClick={this.onClick}></button></td>
                        </tr>);
            }
        }
    });
