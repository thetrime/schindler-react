:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).

:-ensure_loaded(testing).
:-ensure_loaded(database).


:- http_handler(root(ws), http_upgrade_to_websocket(ws, []), [spawn([])]).
:- http_handler(root(.), http_reply_from_files('.', [indexes(['schindler.html'])]), [prefix]).


:-dynamic(listener/2).

ws(Websocket):-
        set_stream(Websocket, encoding(utf8)),
        Class = foo,
        thread_create(dispatch(Websocket), ClientId, [detached(true)]),
        assert(listener(Class, ClientId)),
        client(ClientId, Class, Websocket).

client(ClientId, Class, WebSocket) :-
        format(user_error, 'Waiting for message...~n', []),
        ws_receive(WebSocket, Message, [format(json), value_string_as(atom)]),
        format(user_error, 'Message: ~q~n', [Message]),
        ( Message.opcode == close ->
            thread_send_message(ClientId, close)
        ; Message.opcode == text ->
            Data = Message.data,
            Operation = Data.operation,
            Fields = Data.data,
            Key = matt, % FIXME
            ( catch(handle_message(Key, Class, Operation, Fields),
                    Exception,
                    format(user_error, 'Error: ~p~n', [Exception]))->
                true
            ; otherwise->
                format(user_error, 'Error: ~p~n', [fail])
            ),
            client(ClientId, Class, WebSocket)
        ; otherwise->
            client(ClientId, Class, WebSocket)
        ).

ws_send_message(Class, Key, Data):-
        with_output_to(atom(Atom),
                       json_write(current_output, _{operation:Key, data:Data})),
        forall(listener(Class, ClientId),
               thread_send_message(ClientId, send(Atom))).

dispatch(WebSocket):-
        thread_get_message(Message),
        ( Message == close->
            !,
            thread_self(Self),
            retractall(listener(_, Self))
        ; Message = send(Atom)->
            ws_send(WebSocket, text(Atom)),
            format(user_error, 'Sent message ~w~n', [Atom]),
            dispatch(WebSocket)
        ; otherwise->
            format(user_error, 'Unexpected message ~q~n', [Message]),
            dispatch(WebSocket)
        ).

run:-
        prepare_database,
        prolog_server(9998, []),
        http_server(http_dispatch, [port(9999)]).

%-------------------------------------

handle_message(Key, Class, hello, Message):-
        Checkpoint = Message.checkpoint,
        location_information(Key, Checkpoint, Locations),
        store_information(Key, Checkpoint, Stores),
        item_information(Key, Checkpoint, Items),
        list_information(Key, Checkpoint, List),
        ws_send_message(Class, ohai, _{stores:Stores,
                                       item_locations:Locations,
                                       items:Items,
                                       list:List}).

handle_message(Key, Class, got_item, Message):-
        Name = Message.name,
        transaction(Key, Connection, delete(Connection, list_item(Key, Name))),
        ws_send_message(Class, delete_item, _{name:Name}).

handle_message(Key, Class, new_item, Message):-
        Name = Message.name,
        transaction(Key,
                    Connection,
                    ( insert(Connection, item(Key, Name)),
                      insert(Connection, list_item(Key, Name)))),
        ws_send_message(Class, add_list_item, _{name:Name}).


handle_message(Key, Class, want_item, Message):-
        Name = Message.name,
        transaction(Key, Connection, insert(Connection, list_item(Key, Name))),
        ws_send_message(Class, add_list_item, _{name:Name}).

handle_message(Key, Class, set_item_location, Message):-
        Item = Message.item,
        Location = Message.location,
        Store = Message.store,
        transaction(Key,
                    Connection,
                    ( delete(Connection, known_item_location(Key, Item, Store, Location)),
                      insert(Connection, known_item_location(Key, Item, Store, Location)))),
        ws_send_message(Class, set_item_location, Message).


item_information(Key, _, Items):-
        ( bagof(x{name:Name},
                item(Key, Name),
                Items)->
            true
        ; otherwise->
            Items = []
        ).

location_information(Key, _, Locations):-
        ( bagof(x{store_name:Store,
                  aisles:Aisles},
                bagof(y{aisle_name:Aisle,
                        items:Items},
                      bagof(Item,
                            item_location(Key, Item, Store, Aisle),
                            Items),
                      Aisles),
                Locations)
        ; otherwise->
            Locations = []
        ).


list_information(Key, _, Data):-
        ( bagof(x{name:Name},
                list_item(Key, Name),
                Data)->
            true
        ; otherwise->
            Data = []
        ).

store_information(Key, _, Data):-
        ( bagof(x{name:Name},
                store(Key, Name),
                Data)->
            true
        ; otherwise->
            Data = []
        ).


% This is quite inefficient. We report that every item we know is in the unknown aisle for every store. It might be better to optimise that out
% and just return a single list of all items *once*, and a list of items in a given store where known