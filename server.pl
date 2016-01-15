:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).

:-ensure_loaded(testing).

:- http_handler(root(ws), http_upgrade_to_websocket(ws, []), [spawn([])]).

:-dynamic(pending_item/2).


ws(Websocket):-
        set_stream(Websocket, encoding(utf8)),        
        thread_create(dispatch(Websocket), ClientId, [detached(true)]),
        client(ClientId, Websocket).

client(ClientId, WebSocket) :-
        format(user_error, 'Waiting for message...~n', []),
        ws_receive(WebSocket, Message, [format(json), value_string_as(atom)]),
        format(user_error, 'Message: ~q~n', [Message]),
        ( Message.opcode == close ->
            thread_send_message(ClientId, close)
        ; Message.opcode == text ->
            Data = Message.data,
            Operation = Data.operation,
            ( catch(handle_message(ClientId, Operation, Data),
                    Exception,
                    format(user_error, 'Error: ~p~n', [Exception]))->
                true
            ; otherwise->
                format(user_error, 'Error: ~p~n', [fail])
            ),
            client(ClientId, WebSocket)
        ; otherwise->
            client(ClientId, WebSocket)
        ).

ws_send_message(ClientId, Json):-
        with_output_to(atom(Atom),
                       json_write(current_output, Json)),
        thread_send_message(ClientId, send(Atom)).

dispatch(WebSocket):-
        thread_get_message(Message),
        ( Message == close->
            !
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

handle_message(ClientId, hello, _):-
        findall(_{name:Name, location:Location},
                pending_item(Name, Location),
                Items),        
        ws_send_message(ClientId, _{operation:list, items:Items}).

handle_message(ClientId, got_item, Message):-
        Name = Message.name,
        retractall(pending_item(Name, _)),
        ws_send_message(ClientId, _{operation:got_item_ack, name:Name}).

handle_message(ClientId, add_item, Message):-
        Name = Message.name,
        Location = unknown,
        assert(pending_item(Name, Location)),
        ws_send_message(ClientId, _{operation:add_item_ack, name:Name, location:Location}).






pending_item(apple, lounge).
pending_item(orange, lounge).
pending_item(guitar, kitchen).
pending_item(banjo, kitchen).
pending_item(soap, bathroom).
pending_item(chutney, bathroom).
pending_item('fish paste', bathroom).
pending_item(gruel, bathroom).
pending_item(pillow, bedroom).
pending_item(boxes, bedroom).
pending_item(apes, bedroom).
pending_item(trampoline, bedroom).
pending_item(curtains, bedroom).
pending_item(floor, bathroom).
pending_item('old timey jig', kitchen).
pending_item(clocks, lounge).
pending_item(banana, lounge).
pending_item(guava, lounge).
pending_item(peach, lounge).
pending_item(durian, lounge).
pending_item(mango, lounge).

