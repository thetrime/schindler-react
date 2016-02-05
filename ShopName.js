var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var SchindlerStore = require('./SchindlerStore');
var LoginInfo = require('./LoginInfo');

module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {currentStore: SchindlerStore.getCurrentStore()};
        },

        render: function()
        {
            return (<div className="title_bar horizontal_fill horizontal_layout">
                    <div className="horizontal_fill">
                    <div className="shop_name">Shopping at <a href="#" onClick={this.changeStore}>{this.state.currentStore}</a></div>
                    </div>
                    <LoginInfo/>
                    </div>);
        },

        componentWillMount: function()
        {
            SchindlerStore.addChangeListener(this.onChange);
        },

        onChange: function()
        {
            this.setState({currentStore: SchindlerStore.getCurrentStore()});
        }
        
    });
