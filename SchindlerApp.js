var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');
var ShoppingView = require('./ShoppingView');
var AisleView = require('./AisleView');
var ShopName = require('./ShopName');
var ConnectionInfo = require('./ConnectionInfo');
var SchindlerStore = require('./SchindlerStore');
var Login = require('./Login');
module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {currentView: SchindlerStore.getTopLevelView(),
                    pending_item: {}};
        },

        render: function()
        {
            if (this.state.currentView == "login")                
                return (<div className="vertical_layout vertical_fill">
                        <Login/>
                        </div>);
            else if (this.state.currentView == "shop")
                return (<div className="vertical_layout vertical_fill">
                        <ShopName/>
                        <ShoppingView/>
                        <ConnectionInfo/>
                        </div>);
            else if (this.state.currentView == "select_aisle")
                return (<div className="vertical_layout vertical_fill">
                        <ShopName/>
                        <AisleView item={this.state.pending_item}/>
                        <ConnectionInfo/>
                        </div>);
        },

        componentWillMount: function()
        {
            SchindlerStore.addChangeListener(this.onChange);
        },

        onChange: function()
        {
            this.setState({currentView: SchindlerStore.getTopLevelView(),
                           pending_item: SchindlerStore.getPendingItem()});
        }
        
    });
