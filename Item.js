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
                return (<div className="horizontal_layout horizontal_fill">
                        <div className="horizontal_fill">{this.props.item.name}</div>
                        <div className="button_column"><button className={className} onClick={this.onClick}></button></div>
                        </div>);
            }
            else
            {
                return (<div className="horizontal_layout horizontal_fill">
                        <div className="horizontal_fill">{this.props.item.name}</div>
                        <div className="settings_column"><ItemSettings item={this.props.item}/></div>
                        <div className="button_column"><button className={className} onClick={this.onClick}></button></div>
                        </div>);
            }
        }
    });
