var React = require('react');
var AppDispatcher = require('./AppDispatcher');
var ServerConnection = require('./ServerConnection');
var NewItem = require('./NewItem');
var Item = require('./Item');
var GPSTracker = require('./GPSTracker');

module.exports = React.createClass(
    {
        newStore: function(store)
        {
            var location = GPSTracker.getLocation();
            AppDispatcher.dispatch({operation:"new_store",
                                    origin:"client",
                                    data:{name: store.name,
                                          latitude:location.latitude,
                                          longitude:location.latitude}});
            AppDispatcher.dispatch({operation:"set_store_location",
                                    origin:"client",
                                    data:{name:store.name,
                                          latitude:location.latitude,
                                          longitude:location.longitude}});
            // Also change the current store
            AppDispatcher.dispatch({operation:"set_store",
                                    data:{name:store.name}});
        },
        
        selectStore: function(store)
        {
            var location = GPSTracker.getLocation();
            console.log("Location is: ");
            console.log(location);
            AppDispatcher.dispatch({operation:"set_store_location",
                                    origin:"client",
                                    data:{name:store.name,
                                          latitude:location.latitude,
                                          longitude:location.longitude}});
            // Also change the current store
            AppDispatcher.dispatch({operation:"set_store",
                                    data:{name:store.name}});

        },
        render: function()
        {
            var rows = [];
            var filter = this.props.filterText;
            var exactMatch = false;
            if (filter != '' && !exactMatch)
            {
                rows.push(<NewItem name={filter} key={filter} addItem={this.newStore} label="add"/>);
            }
            var table = this;
            this.props.stores.sort().forEach(function(store)
                                             {
                                                 var settings = [{label:'Delete this store',
                                                                  handler: function()
                                                                  {
                                                                      AppDispatcher.dispatch({operation:"delete_store",
                                                                                              origin:"client",
                                                                                              data:{store:store.name}});
                                                                  }} /*,
                                                                 {label:'Configure locations in this store',
                                                                  handler:function()
                                                                  {
                                                                      AppDispatcher.dispatch({operation:"manage_store",
                                                                                              data:{store:store.name}});
                                                                  }}*/];
                                                 rows.push(<Item item={store} key={store.name} onClick={table.selectStore} label='select' settings={settings}/>);
                                            });
            return (<div className="table_container vertical_fill">
                    {rows}
                    </div>);
        }
    });
