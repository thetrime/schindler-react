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
            return (<div className="horizontal_layout horizontal_fill">
                    <div className="horizontal_fill">{this.props.name}</div>
                    <div className="button_column" colSpan="2"><button className="app_button add_button" onClick={this.addItem}></button></div>
                    </div>);
            
        }
    });
