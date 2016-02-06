var AppDispatcher = require('./AppDispatcher');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;
var ServerConnection = require('./ServerConnection');
var StoreStore = require('./StoreStore');

var current_view = "login";
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
                                                          if (event.operation == "login_ok")
                                                          {
                                                              current_view = "shop";
                                                              localStorage.setItem("credentials", JSON.stringify({username:event.data.username,
                                                                                                                  password:event.data.password}));
                                                              SchindlerStore.emitChange();
                                                              (ServerConnection.reloadList.bind(ServerConnection))()
                                                          }
                                                          if (event.operation == "login_failed")
                                                          {
                                                              // FIXME: Do /something/! This should probably be listened to by the LoginView
                                                          }
                                                          if (event.operation == "select_store")
                                                          {
                                                              current_view = "select_store";
                                                              SchindlerStore.emitChange();
                                                          }
                                                          if (event.operation == "logout")
                                                          {
                                                              localStorage.setItem("credentials", null);
                                                              current_view = "login";
                                                              SchindlerStore.emitChange();
                                                          }
                                                          if (event.operation == "set_store")
                                                          {
                                                              // Wait for the store to be changed before we swap the view back
                                                              AppDispatcher.waitFor([StoreStore.dispatchToken]);
                                                              current_view = "shop";
                                                              SchindlerStore.emitChange();
                                                          }
                                                      });

module.exports = SchindlerStore;
