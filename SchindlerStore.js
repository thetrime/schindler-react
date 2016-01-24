var AppDispatcher = require('./AppDispatcher');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;

var current_view = "shop";
var current_store = "home";

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
                                }
                                
                            });

SchindlerStore.dispatchToken = AppDispatcher.register(function(event)
                                                      {
                                                          if (event.operation == "got_item" && event.data.location == "unknown")
                                                          {
                                                              current_view = "select_aisle";
                                                              SchindlerStore.emitChange();
                                                          }
                                                      });

module.exports = SchindlerStore;
