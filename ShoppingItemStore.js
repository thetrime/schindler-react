var AppDispatcher = require('./AppDispatcher');
var assign = require('object-assign');
var EventEmitter = require('events').EventEmitter;
var ServerConnection = require('./ServerConnection');
var items = [];

var ShoppingItemStore = assign({},
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
                                   getItems: function()
                                   {
                                       return items;
                                   }
                                   
                               });

ShoppingItemStore.dispatchToken = AppDispatcher.register(function(event)
                                                         {
                                                             if (event.operation == "list")
                                                             {
                                                                 items = event.data.items;
                                                                 ShoppingItemStore.emitChange();
                                                             }
                                                             if (event.operation == "got_item" && event.data.location != "unknown")
                                                             {
                                                                 console.log("event");
                                                                 console.log(event);
                                                                 ServerConnection.sendMessage(event);
                                                             }
                                                             if (event.operation == "delete_item")
                                                             {
                                                                 items = items.filter(function(a) { return a.name != event.data.name });
                                                                 ShoppingItemStore.emitChange();
                                                             }
                                                             if (event.operation == "add_item")
                                                             {
                                                                 items = items.concat([event.data]);
                                                                 ShoppingItemStore.emitChange();
                                                             }
                                                         });

module.exports = ShoppingItemStore;
