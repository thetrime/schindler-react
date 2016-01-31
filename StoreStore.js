var AppDispatcher = require('./AppDispatcher');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;
var ServerConnection = require('./ServerConnection');
var stores = [];
var items = [];

var current_store = 'home';

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
                            getStores: function()
                            {
                                var store_names = [];
                                stores.forEach(function(s) { store_names.push(s.name);});
                                return store_names;
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
                                console.log(aisles);
                                return aisles;
                            },

                            getItemsForCurrentStore: function()
                            {
                                var located_items = [];
                                items.forEach(function(item)
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
                                                      if (event.operation == "ohai")
                                                      {
                                                          console.log(event.data);
                                                          // Initialization message. We are only interested in the .locations part
                                                          stores = {};
                                                          event.data.stores.forEach(function(store)
                                                                                    {
                                                                                        stores[store.name] = {};
                                                                                        stores[store.name].aisles = [];
                                                                                        stores[store.name].item_locations = {};
                                                                                    });
                                                          items = event.data.items;
                                                          event.data.item_locations.forEach(function(store)
                                                                                            {
                                                                                                store.aisles.forEach(function(aisle)
                                                                                                                     {
                                                                                                                         stores[store.store_name].aisles.push({name:aisle.aisle_name});
                                                                                                                         aisle.items.forEach(function(item)
                                                                                                                                             {
                                                                                                                                                 stores[store.store_name].item_locations[item] = aisle.aisle_name;
                                                                                                                                             });
                                                                                                                     });
                                                                                            });
                                                          StoreStore.emitChange();
                                                      }
                                                      if (event.operation == "set_item_location")
                                                      {
                                                          console.log("Setting " + event.data.item + " location to " + event.data.location);
                                                          stores[event.data.store].item_locations[event.data.item] = event.data.location;
                                                          StoreStore.emitChange();
                                                          // Also advise the server of this realization
                                                          if (event.origin == 'client')
                                                              ServerConnection.sendMessage(event);
                                                      }
                                                  });

module.exports = StoreStore;
