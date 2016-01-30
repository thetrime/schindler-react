var AppDispatcher = require('./AppDispatcher');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;
var ServerConnection = require('./ServerConnection');
var world = [];

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
                                var stores = [];
                                world.forEach(function(s) { stores.push(s.name);});
                                return stores;
                            },

                            getAisleFor: function(item)
                            {
                                for (var i = 0; i < world.length; i++)
                                {
                                    if (world[i].store_name == current_store)
                                    {
                                        if (world[i].item_locations[item] === undefined)
                                            return "unknown";
                                        else
                                            return world[i].item_locations[item];
                                    }
                                }
                            },

                            getAislesForCurrentStore: function()
                            {
                                var aisles = [];
                                for (var i = 0; i < world.length; i++)
                                {
                                    if (world[i].store_name == current_store)
                                    {
                                        world[i].aisles.forEach(function(aisle)
                                                                {
                                                                    aisles.push({name:aisle.aisle_name});
                                                                });
                                        break;
                                    }
                                }
                                return aisles;
                            }
                            
                        });

StoreStore.dispatchToken = AppDispatcher.register(function(event)
                                                  {
                                                      if (event.operation == "hello")
                                                      {
                                                          // Initialization message. We are only interested in the .world part
                                                          world = event.data.world;
                                                          world.forEach(function(store)
                                                                        {
                                                                            store.item_locations = {};
                                                                            store.aisles.forEach(function(aisle)
                                                                                                 {
                                                                                                     aisle.items.forEach(function(item)
                                                                                                                         {
                                                                                                                             store.item_locations[item] = aisle.aisle_name;
                                                                                                                         });
                                                                                                 });
                                                                        });
                                                          StoreStore.emitChange();
                                                      }
                                                      if (event.operation == "set_item_location")
                                                      {
                                                          console.log("Setting " + event.data.item + " location to " + event.data.location);
                                                          for (var i = 0; i < world.length; i++)
                                                          {
                                                              if (world[i].store_name == event.data.store)
                                                              {
                                                                  world[i].item_locations[event.data.item] = event.data.location;
                                                              }
                                                          }                                                          
                                                          StoreStore.emitChange();
                                                          // Also advise the server of this realization
                                                          if (event.origin == 'client')
                                                              ServerConnection.sendMessage(event);
                                                      }
                                                  });

module.exports = StoreStore;
