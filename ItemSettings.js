var React = require('react');
var ServerConnection = require('./ServerConnection');
var StoreStore = require('./StoreStore');

module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {state:"collapsed"};
        },
        expand: function()
        {
            this.setState({state:"expanded"});
        },
        render: function()
        {
            if (this.state.state == "collapsed")
                return (<button className="app_button settings_button" onClick={this.expand}></button>);
            else
                return (<div className="vertical_layout">
                        <button className="setting_button">Hide for this store</button>
                        <button className="setting_button">Remove from current location</button>
                        <button className="setting_button">Get it next time</button>
                        </div>);
        }
    });
