var React = require('react');
var ShoppingItemStore = require('./ShoppingItemStore');
var SearchBox = require('./SearchBox');
var ItemTable = require('./AisleTable');
var ServerConnection = require('./ServerConnection');

function getStateFromStore()
{
    return {aisles: ShoppingItemStore.getItems()};
}


module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {filterText: '',
                    aisles: []};
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
            //AisleStore.addChangeListener(this.onChange);
        },
        
        render: function()
        {
            return (<div>
                    <SearchBox filterText={this.state.filterText} redoSearch={this.redoSearch}/>
                    <AisleTable model={this.state.items} filterText={this.state.filterText} redoSearch={this.redoSearch}/>
                    </div>);
        }
    });
