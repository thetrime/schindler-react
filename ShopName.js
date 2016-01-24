var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var SchindlerStore = require('./SchindlerStore');

module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {currentStore: SchindlerStore.getCurrentStore()};
        },

        render: function()
        {
            return (<div className="shopName horizontal_fill">
                    Shopping at <a href="#" onClick={this.changeStore}>{this.state.currentStore}</a>
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
