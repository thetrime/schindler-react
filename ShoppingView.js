var React = require('react');
var ShoppingItemStore = require('./ShoppingItemStore');
var SearchBox = require('./SearchBox');
var ItemTable = require('./ItemTable');
var ServerConnection = require('./ServerConnection');

function getStateFromStore()
{
    return {items: ShoppingItemStore.getItems()};
}


module.exports = React.createClass(
    {
        getInitialState: function()
        {
            return {filterText: '',
                    items: []};
        },

        onChange: function()
        {
            this.setState(getStateFromStore());
        },
        
        redoSearch: function(value)
        {
            this.setState({filterText: value});
        },

        gotItem: function(item)
        {
            this.setState({items: this.state.items.filter(function(existing_item)
                                                          {
                                                              return existing_item.name != item.name;
                                                          })});
        },
        
        addItem: function(item)
        {
            this.setState({items: this.state.items.concat([item])});
        },
        
        componentWillMount: function()
        {
            ShoppingItemStore.addChangeListener(this.onChange);
        },

        componentWillUnmount: function()
        {
            ShoppingItemStore.removeChangeListener(this.onChange);
        },
        
        render: function()
        {
            return (<div className="vertical_layout vertical_fill">
                    <SearchBox filterText={this.state.filterText} redoSearch={this.redoSearch} className="horizontal_fill"/>
                    <ItemTable model={this.state.items} filterText={this.state.filterText} redoSearch={this.redoSearch} className="horizontal_fill vertical_fill"/>
                    </div>);
        }
    });
