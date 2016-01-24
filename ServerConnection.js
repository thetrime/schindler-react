var AppDispatcher = require('./AppDispatcher');

module.exports =
    {
        websocket: null,
        handle_server_connect: function()
        {
            this.sendMessage({operation:"hello",data:{version:1}});
            this.dispatchEvent("connection", "connected");
        },
        handle_server_disconnect: function()
        {
            console.log("Close detected. Reopening connection in 3 seconds...");
            this.dispatchEvent("connection", "disconnected");
            var that = this;
            setTimeout(function() {that.sendMessage({operation:"ping"})}, 3000);
        },
        dispatchEvent: function(key, data)
        {
            AppDispatcher.dispatch({operation:key,
                                    data:data});
        },
        sendMessage: function(message)
        {
            if (this.websocket.readyState == this.websocket.OPEN)
            {
                this.websocket.send(JSON.stringify(message));
            }
            else
            {
                // FIXME: The (user) message actually doesn't get re-queued for sending here?
                console.log("Connection lost....");
                dispatchEvent("connection", "disconnected");
                var new_websocket = new WebSocket(uri);
                new_websocket.onmessage = this.websocket.onmessage;
                new_websocket.onopen = this.websocket.onopen;
                new_websocket.onclose = this.websocket.onclose;
                new_websocket.onerror = this.websocket.onerror;
                this.websocket = new_websocket;
            }
        },
        handle_server_message: function(event)
        {
            var msg = JSON.parse(event.data);
            this.dispatchEvent(msg.operation, msg.data);
        },
        initialize: function()
        {
            var loc = window.location;
            var uri = "ws:";
            if (loc.protocol === "https:") 
                uri = "wss:";
            uri += "//" + loc.host;
            uri += loc.pathname + "ws";
            this.websocket = new WebSocket(uri);
            this.websocket.onmessage = this.handle_server_message.bind(this);
            this.websocket.onclose = this.handle_server_disconnect.bind(this);
            this.websocket.onopen = this.handle_server_connect.bind(this);
        }
    };
