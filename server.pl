:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/json)).

:-ensure_loaded(testing).

:- multifile http:location/3.
:- dynamic   http:location/3.
http:location(schindler, '/schindler', []).


:- http_handler(schindler(ws), http_upgrade_to_websocket(ws, []), [spawn([])]).
:- http_handler(schindler(.), http_reply_from_files('.', [indexes(['schindler.html'])]), [prefix]).


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
            ( catch(handle_message(Class, Operation, Fields),
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
        http_server(http_dispatch, [port(9999)]).

%-------------------------------------

handle_message(Class, hello, _):-
        Key = fixme,
        Checkpoint = fixme,
        world_information(Key, Checkpoint, World),
        list_information(Key, Checkpoint, List),
        ws_send_message(Class, hello, _{world:World,
                                        list:List}).

handle_message(Class, got_item, Message):-
        Name = Message.name,
        retractall(list_item(Name)),
        ws_send_message(Class, delete_item, _{name:Name}).

handle_message(Class, new_item, Message):-
        Name = Message.name,
        assert(item(Name)),
        assert(list_item(Name)),
        ws_send_message(Class, add_list_item, _{name:Name}).


handle_message(Class, want_item, Message):-
        Name = Message.name,
        assert(list_item(Name)),
        ws_send_message(Class, add_list_item, _{name:Name}).

handle_message(Class, set_item_location, Message):-
        Item = Message.item,
        Location = Message.location,
        Store = Message.store,
        retractall(known_item_location(Item, Store, Location)),
        assert(known_item_location(Item, Store, Location)),
        ws_send_message(Class, set_item_location, Message).




:-dynamic(list_item/1).
:-dynamic(store/1).
:-dynamic(aisle/2).
:-dynamic(item/1).
:-dynamic(known_item_location/3).


list_item(apple).
list_item(orange).
list_item(guitar).
list_item(banjo).
list_item(soap).
list_item(chutney).
list_item('fish paste').
list_item(gruel).
list_item(boxes).
list_item(apes).
list_item(trampoline).
list_item(floor).
list_item('old timey jig').
list_item(banana).
list_item(guava).
list_item(peach).
list_item(durian).
list_item(mango).
list_item(worms).


store(home).
store(tesco).
store(qfc).

aisle(home, lounge).
aisle(home, kitchen).
aisle(home, bathroom).
aisle(home, bedroom).

aisle(qfc, produce).
aisle(tesco, produce).

item(apple).
item(orange).
item(guitar).
item(banjo).
item(soap).
item(chutney).
item('fish paste').
item(gruel).
item(pillow).
item(boxes).
item(apes).
item(trampoline).
item(curtains).
item(floor).
item('old timey jig').
item(clocks).
item(banana).
item(guava).
item(peach).
item(durian).
item(mango).
item(worms).

known_item_location(apple, qfc, produce).
known_item_location(apple, tesco, produce).
known_item_location(apple, home, lounge).

known_item_location(orange, qfc, produce).
known_item_location(orange, tesco, produce).

known_item_location(pillow, home, bathroom).

item_location(Item, Store, Aisle):-
        known_item_location(Item, Store, Aisle).

item_location(Item, Store, unknown):-
        item(Item),
        store(Store),
        \+known_item_location(Item, Store, _).

world_information(_, _, Data):-
        bagof(x{store_name:Store,
                aisles:Aisles},
              bagof(y{aisle_name:Aisle,
                      items:Items},
                    bagof(Item,
                          item_location(Item, Store, Aisle),
                          Items),
                    Aisles),
              Data).

list_information(_, _, Data):-
        bagof(x{name:Name},
              list_item(Name),
              Data).


% This is quite inefficient. We report that every item we know is in the unknown aisle for every store. It might be better to optimise that out
% and just return a single list of all items *once*, and a list of items in a given store where known