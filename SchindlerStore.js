var AppDispatcher = require('./AppDispatcher');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;

var current_view = "shop";
var current_store = "home";
var pending_item = {};

var SchindlerStore = assign({},
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
                                getTopLevelView: function()
                                {
                                    return current_view;
                                },
                                getCurrentStore: function()
                                {
                                    return current_store;
                                },
                                getPendingItem: function()
                                {
                                    return pending_item;
                                }
                                
                            });

SchindlerStore.dispatchToken = AppDispatcher.register(function(event)
                                                      {
                                                          if (event.operation == "got_item" && event.data.location == "unknown")
                                                          {
                                                              current_view = "select_aisle";
                                                              pending_item = event.data.name;
                                                              SchindlerStore.emitChange();
                                                          }
                                                          if (event.operation == "set_item_location")
                                                          {
                                                              current_view = "shop";
                                                              pending_item = {};
                                                              SchindlerStore.emitChange();
                                                          }
                                                          
                                                      });

module.exports = SchindlerStore;
