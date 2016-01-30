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
                                                                 var found = false;
                                                                 for (var i = 0; i < items.length; i++)
                                                                 {
                                                                     if ((items[i].name == event.data.name) && (items[i].location == event.data.location))
                                                                     {
                                                                         found = true;
                                                                         break;
                                                                     }
                                                                 }
                                                                 if (!found)
                                                                 {
                                                                     console.log("Adding item " + event.data);
                                                                     items = items.concat([event.data]);
                                                                     ShoppingItemStore.emitChange();                                                                     
                                                                 }
                                                                 else
                                                                 {
                                                                     console.log("Already have " + event.data);
                                                                 }
                                                             }
                                                             if (event.operation == "new_item")
                                                             {
                                                                 // We want to add an item
                                                                 // First, add it locally
                                                                 console.log("Adding " + event.data);
                                                                 items = items.concat([event.data]);
                                                                 ShoppingItemStore.emitChange();
                                                                 // then tell the server
                                                                 ServerConnection.sendMessage(event);
                                                             }
                                                         });

module.exports = ShoppingItemStore;
