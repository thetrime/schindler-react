var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var StoreStore = require('./StoreStore');
var LoginInfo = require('./LoginInfo');

module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {currentStore: StoreStore.getCurrentStore()};
        },

        changeStore: function()
        {
            AppDispatcher.dispatch({operation:"select_store",
                                    data:{}});
        },

        render: function()
        {
            return (<div className="title_bar horizontal_fill horizontal_layout">
                    <div className="horizontal_fill horizontal_layout">
                    <div className="shop_name horizontal_fill horizontal_layout">
                    <a href="#" className="shop_name_label horizontal_fill" onClick={this.changeStore}>{this.state.currentStore}</a></div>
                    </div>
                    <LoginInfo/>
                    </div>);
        },

        componentWillMount: function()
        {
            StoreStore.addChangeListener(this.onChange);
        },

        componentWillUnmount: function()
        {
            StoreStore.removeChangeListener(this.onChange);
        },

        
        onChange: function()
        {
            this.setState({currentStore: StoreStore.getCurrentStore()});
        }
        
    });
