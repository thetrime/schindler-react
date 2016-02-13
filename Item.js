var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ItemSettings = require('./ItemSettings');

module.exports = React.createClass(
    {
        onClick: function()
        {
            this.props.onClick(this.props.item);
        },
        settings: function()
        {
            this.props.settings(this.props.item);
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
                        <td className = "settings_column"><ItemSettings/></td>
                        <td className = "button_column"><button className={className} onClick={this.onClick}></button></td>
                        </tr>);
            }
        }
    });
