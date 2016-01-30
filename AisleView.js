var React = require('react');
var ShoppingItemStore = require('./ShoppingItemStore');
var SearchBox = require('./SearchBox');
var AisleTable = require('./AisleTable');
var ServerConnection = require('./ServerConnection');
var StoreStore = require('./StoreStore');

function getStateFromStore()
{
    return {aisles: StoreStore.getAislesForCurrentStore()};
}


module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {filterText: '',
                    aisles: StoreStore.getAislesForCurrentStore()};
        },

        onChange: function()
        {
            this.setState(getStateFromStore());
        },
        
        redoSearch: function(value)
        {
            this.setState({filterText: value});
        },
       
        addAisle: function(item)
        {
            this.setState({items: this.state.items.concat([item])});
        },
        
        componentWillMount: function()
        {
            StoreStore.addChangeListener(this.onChange);
        },

        componentWillUnmount: function()
        {
            StoreStore.removeChangeListener(this.onChange);
        },
        
        render: function()
        {
            // FIXME: store is not always home!
            return (<div className="vertical_layout vertical_fill">
                    <SearchBox filterText={this.state.filterText} redoSearch={this.redoSearch}/>
                    <AisleTable aisles={this.state.aisles} filterText={this.state.filterText} redoSearch={this.redoSearch} store="home" className="horizontal_fill vertical_fill"/> 
                    </div>);
        }
    });
