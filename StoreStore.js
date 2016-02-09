var AppDispatcher = require('./AppDispatcher');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;
var ServerConnection = require('./ServerConnection');
var GPSTracker = require('./GPSTracker');
var stores = [];
var all_items = [];
var current_list = [];

var current_store = 'home';

function setCurrentList(i)
{
    current_list = [];
    i.forEach(function(item)              
              {
                  addItemToCurrentList(item);
              });
}

function addItemToCurrentList(item)
{
    var new_item = {name:item.name,
                    on_list:true,
                    location:StoreStore.getAisleFor(item.name)};
    current_list.push(new_item);
}

function relocateItems()
{
    var i = current_list;
    current_list = [];
    i.forEach(function(item)
              {
                  addItemToCurrentList(item);
              });
}

function getNearestStoreTo(position)
{
    var distance = -1;
    var store = 'home';
    console.log("Getting nearest store:");    
    Object.keys(stores).forEach(function(storeName)
                                {
                                    var d = GPSTracker.haversine(stores[storeName].location, position);
                                    console.log('Distance to ' + storeName + ' is ' + d + ' metres');
                                    if (distance == -1 || d < distance)
                                    {
                                        distance = d;
                                        store = storeName;
                                    }
                                });
    console.log('The closest store is ' + store);
    return store;
}

var StoreStore = assign({},
                        EventEmitter.prototype,
                        {
                            emitChange: function()
                            {
                                this.emit('change');
                            },
                            addChangeListener: function(callback)
                            {
                                this.on('change', callback);
                            },
                            removeChangeListener: function(callback)
                            {
                                this.removeListener('change', callback);
                            },

                            /* Actual logic */
                            getCurrentList: function()
                            {
                                return current_list;
                            },
                            
                            getStoreNames: function()
                            {
                                var store_names = [];
                                Object.keys(stores).forEach(function(s) { store_names.push({name: s});});
                                return store_names;
                            },

                            getCurrentStore: function()
                            {
                                return current_store;
                            },

                            getAisleFor: function(item)
                            {
                                if (stores[current_store].item_locations[item] === undefined)
                                    return "unknown";
                                else
                                    return stores[current_store].item_locations[item];
                            },

                            getAislesForCurrentStore: function()
                            {
                                var aisles = [];
                                stores[current_store].aisles.forEach(function(aisle)
                                                                     {
                                                                         aisles.push({name:aisle.name});
                                                                     });
                                return aisles;
                            },                            

                            getItemsForCurrentStore: function()
                            {
                                var located_items = [];
                                all_items.forEach(function(item)
                                                  {
                                                      if (stores[current_store].item_locations[item.name] === undefined)
                                                          located_items.push({name:item.name,
                                                                              location:"unknown"});
                                                      else
                                                          located_items.push({name:item.name,
                                                                              location:stores[current_store].item_locations[item.name]});                                                  
                                              });
                               
                                return located_items;
                            }
                        });

StoreStore.dispatchToken = AppDispatcher.register(function(event)
                                                  {
                                                      if (event.operation == "ohai" || event.operation == "ohai_again")
                                                      {
                                                          if (event.operation == "ohai_again")
                                                          {
                                                              event.data = JSON.parse(localStorage.getItem("checkpoint_data"));
                                                          }
                                                          else
                                                          {
                                                              localStorage.setItem("checkpoint", event.data.checkpoint);
                                                              localStorage.setItem("checkpoint_data", JSON.stringify(event.data));
                                                          }
                                                          stores = {};
                                                          console.log(event.data.stores);
                                                          // First construct the stores. Each store starts out with an empty aisle list and no item locations
                                                          event.data.stores.forEach(function(store)
                                                                                    {
                                                                                        stores[store.name] = {};
                                                                                        stores[store.name].location = {latitude:store.latitude,
                                                                                                                       longitude:store.longitude};
                                                                                        stores[store.name].aisles = [];
                                                                                        stores[store.name].item_locations = {};
                                                                                    });
                                                          // Just copy the list of all known items
                                                          all_items = event.data.items;
                                                          // For every aisle, create a reference in the appropriate store
                                                          event.data.aisles.forEach(function(aisle)
                                                                                    {
                                                                                        stores[aisle.store].aisles.push({name:aisle.name});
                                                                                    });
                                                          // Finally, for each item with a known location, put it in the right htable
                                                          event.data.item_locations.forEach(function(store)
                                                                                            {
                                                                                                store.aisles.forEach(function(aisle)
                                                                                                                     {
                                                                                                                         aisle.items.forEach(function(item)
                                                                                                                                             {
                                                                                                                                                 stores[store.store_name].item_locations[item] = aisle.aisle_name;
                                                                                                                                             });
                                                                                                                     });
                                                                                            });
                                                          setCurrentList(event.data.list);                                                          
                                                          StoreStore.emitChange();
                                                      }
                                                      if (event.operation == "set_item_location")
                                                      {
                                                          stores[event.data.store].item_locations[event.data.item] = event.data.location;
                                                          StoreStore.emitChange();
                                                          // Also advise the server of this realization
                                                          if (event.origin == 'client')
                                                              ServerConnection.sendMessage(event);
                                                      }
                                                      if (event.operation == "got_item" && event.data.location != "unknown")
                                                      {
                                                          // Delete the item from the list in any case - if the server is responding, then we will
                                                          // waste some time processing a meaningless delete_item, but it wont really matter
                                                          current_list = current_list.filter(function(a) {return a.name != event.data.name});
                                                          StoreStore.emitChange();
                                                          // And also tell the server
                                                          ServerConnection.sendMessage(event);
                                                      }
                                                      if (event.operation == "delete_item")
                                                      {
                                                          // The server wants us to remove an item
                                                          current_list = current_list.filter(function(a) {return a.name != event.data.name});
                                                          StoreStore.emitChange();
                                                      }
                                                      if (event.operation == "add_list_item")
                                                      {
                                                          // The server wants us to add an item
                                                          var found = false;
                                                          for (var i = 0; i < current_list.length; i++)
                                                          {
                                                              if (current_list[i].name == event.data.name)
                                                              {
                                                                  found = true;
                                                                  break;
                                                              }
                                                          }
                                                          if (!found)
                                                          {
                                                              console.log("Adding item " + event.data.name);
                                                              addItemToCurrentList(event.data);
                                                              StoreStore.emitChange();                                                                     
                                                          }
                                                          else
                                                          {
                                                              console.log("Already have " + event.data);
                                                          }
                                                      }
                                                      if (event.operation == "new_item")
                                                      {
                                                          // The user wants to add an item
                                                          // First, add it locally
                                                          console.log("Adding " + event.data);
                                                          addItemToCurrentList(event.data);
                                                          StoreStore.emitChange();
                                                          // then tell the server
                                                          ServerConnection.sendMessage(event);
                                                      }
                                                      if (event.operation == "want_item")
                                                      {
                                                          // The user wants to add an existing item
                                                          // First, add it locally
                                                          addItemToCurrentList(event.data);
                                                          StoreStore.emitChange();
                                                          // then tell the server
                                                          ServerConnection.sendMessage(event);
                                                      }
                                                      if (event.operation == "login")
                                                      {
                                                          ServerConnection.sendMessage(event);
                                                      }
                                                      if (event.operation == "set_store_location")
                                                      {
                                                          stores[event.data.name].location = {latitude:event.data.latitude,
                                                                                              longitude:event.data.longitude};
                                                          if (event.origin == 'client')
                                                              ServerConnection.sendMessage(event);
                                                      }
                                                      if (event.operation == "new_store")
                                                      {
                                                          if (stores[event.data.name] === undefined)
                                                          {
                                                              stores[event.data.name] = {};
                                                              stores[event.data.name].aisles = [];
                                                              stores[event.data.name].item_locations = {};
                                                          }
                                                          if (event.origin == 'client')
                                                              ServerConnection.sendMessage(event);
                                                      }
                                                      if (event.operation == "new_aisle")
                                                      {
                                                          var found = false;
                                                          for (var i = 0; i < stores[event.data.store].aisles.length; i++)
                                                          {
                                                              if (stores[event.data.store].aisles[i].name == event.data.name)
                                                              {
                                                                  found = true;
                                                                  break;
                                                              }
                                                          }
                                                          if (!found)
                                                              stores[event.data.store].aisles.push({name:event.data.name});
                                                          if (event.origin == "client")
                                                              ServerConnection.sendMessage(event);
                                                          
                                                      }
                                                      if (event.operation == "set_store")
                                                      {
                                                          console.log('Store is now ' + event.data.name);
                                                          current_store = event.data.name;
                                                          relocateItems();
                                                          StoreStore.emitChange();
                                                      }
                                                      if (event.operation == "moved")
                                                      {
                                                          var new_store = getNearestStoreTo(event.data.position);
                                                          if (new_store != current_store)
                                                          {
                                                              current_store = new_store
                                                              StoreStore.emitChange();
                                                          }
                                                      }

                                                  });

module.exports = StoreStore;
