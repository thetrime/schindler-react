var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');
var ShoppingView = require('./ShoppingView');
var AisleView = require('./AisleView');
var TitleBar = require('./TitleBar');
var ConnectionInfo = require('./ConnectionInfo');
var SchindlerStore = require('./SchindlerStore');
var LoginView = require('./LoginView');
var LoginInfo = require('./LoginInfo');
var StoreView = require('./StoreView');
var ItemSettingsView = require('./ItemSettingsView');
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
                        <LoginView/>
                        <ConnectionInfo/>                        
                        </div>);
            else if (this.state.currentView == "shop")
                return (<div className="vertical_layout vertical_fill">
                        <TitleBar/>
                        <ShoppingView/>
                        <ConnectionInfo/>
                        </div>);
            else if (this.state.currentView == "select_aisle")
                return (<div className="vertical_layout vertical_fill">
                        <TitleBar/>
                        <AisleView item={this.state.pending_item}/>
                        <ConnectionInfo/>
                        </div>);
            else if (this.state.currentView == "select_store")
                return (<div className="vertical_layout vertical_fill">
                        <TitleBar/>
                        <StoreView/>
                        <ConnectionInfo/>
                        </div>);
        },

        componentWillMount: function()
        {
            SchindlerStore.addChangeListener(this.onChange);
        },

        onChange: function()
        {
            console.log("SchindlerStore emitted a change. Top level view is now: " + SchindlerStore.getTopLevelView());
            this.setState({currentView: SchindlerStore.getTopLevelView(),
                           pending_item: SchindlerStore.getPendingItem()});
        }
        
    });
