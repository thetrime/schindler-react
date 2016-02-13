var React = require('react');
var ServerConnection = require('./ServerConnection');
var StoreStore = require('./StoreStore');
var AppDispatcher = require('./AppDispatcher');

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
        collapse: function()
        {
            this.setState({state:"collapsed"});
        },
        beyond: function()
        {
            AppDispatcher.dispatch({operation:"set_item_location",
                                    origin:"client",
                                    data:{location:"$beyond",
                                          item:this.props.item.name,
                                          store:StoreStore.getCurrentStore()}});
            this.setState({state:"collapsed"});
        },
        rehome: function()
        {
            AppDispatcher.dispatch({operation:"set_item_location",
                                    origin:"client",
                                    data:{location:"unknown",
                                          item:this.props.item.name,
                                          store:StoreStore.getCurrentStore()}});
            this.setState({state:"collapsed"});
        },
        defer: function()
        {
            AppDispatcher.dispatch({operation:"defer",
                                    data:{item:this.props.item.name,
                                          store:StoreStore.getCurrentStore()}});
            this.setState({state:"collapsed"});

        },        
        render: function()
        {
            if (this.state.state == "collapsed")
                return (<button className="app_button settings_button" onClick={this.expand}></button>);
            else
                return (<div className="vertical_layout">
                        <button className="setting_button" onclick={this.beyond}>Hide for this store</button>
                        <button className="setting_button" onClick={this.rehome}>Remove from current location</button>
                        <button className="setting_button" onClick={this.defer}>Get it next time</button>
                        <button className="setting_button" onClick={this.collapse}>^</button>
                        </div>);
        }
    });
