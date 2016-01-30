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

                                                      /*
                                                      if (event.operation == "add_store")
                                                      {
                                                          // The server wants us to add a new store
                                                          var found = false;
                                                          for (var i = 0; i < stores.length; i++)
                                                          {
                                                              if ((stores[i].name == event.data.name))
                                                              {
                                                                  // FIXME: Should also try to reconcile aisles in the store, I guess?
                                                                  found = true;
                                                                  break;
                                                              }
                                                          }
                                                          if (!found)
                                                          {
                                                              console.log("Adding item " + event.data);
                                                              stores = stores.concat([event.data]);
                                                              StoreStore.emitChange();                                                                     
                                                          }
                                                          else
                                                          {
                                                              console.log("Already have " + event.data);
                                                          }
                                                      }
                                                      if (event.operation == "new_store")
                                                      {
                                                          // We want to add an store
                                                          // First, add it locally
                                                          console.log("Adding " + event.data);
                                                          stores = stores.concat([event.data]);
                                                          StoreStore.emitChange();
                                                          // then tell the server
                                                          ServerConnection.sendMessage(event);
                                                      }                                                      
                                                      */
                                                      
                                                  });

module.exports = StoreStore;
