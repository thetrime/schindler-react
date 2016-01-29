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
                                                                 // This is a complete update from the server. We only get this when we send a "hello" message
                                                                 items = event.data.items;
                                                                 ShoppingItemStore.emitChange();
                                                             }
                                                             if (event.operation == "got_item" && event.data.location != "unknown")
                                                             {
                                                                 // Delete the item from the list in any case - if the server is responding, then we will
                                                                 // waste some time processing a meaningless delete_item, but it wont really matter
                                                                 items = items.filter(function(a) { return a.name != event.data.name });
                                                                 ShoppingItemStore.emitChange();
                                                                 // Though also actually send the message!
                                                                 ServerConnection.sendMessage(event);
                                                             }
                                                             if (event.operation == "delete_item")
                                                             {
                                                                 // The server wants us to remove an item
                                                                 items = items.filter(function(a) { return a.name != event.data.name });
                                                                 ShoppingItemStore.emitChange();
                                                             }
                                                             if (event.operation == "add_item")
                                                             {
                                                                 // The server wants us to add an item
                                                                 items = items.concat([event.data]);
                                                                 ShoppingItemStore.emitChange();
                                                             }
                                                         });

module.exports = ShoppingItemStore;
