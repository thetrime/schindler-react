:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).

:-ensure_loaded(testing).

:- http_handler(root(ws), http_upgrade_to_websocket(ws, []), [spawn([])]).

ws(Websocket):-
        set_stream(Websocket, encoding(utf8)),
        client(Websocket).

client(WebSocket) :-
        format(user_error, 'Waiting for message...~n', []),
        ws_receive(WebSocket, Message, [format(json)]),
        format(user_error, 'Message: ~q~n', [Message]),
        ( Message.opcode == close ->
            true
        ; Message.opcode == text ->
            Data = Message.data,
            Operation = Data.operation,
            ( catch(handle_message(WebSocket, Operation, Data),
                    Exception,
                    format(user_error, 'Error: ~p~n', [Exception]))->
                true
            ; otherwise->
                format(user_error, 'Error: ~p~n', [fail])
            ),
            client(WebSocket)
        ; otherwise->
            ws_send(WebSocket, Message),
            client(WebSocket)
        ).

handle_message(WebSocket, "hello", _):-
        findall(_{name:Name, location:Location},
                pending_item(Name, Location),
                Items),
        ws_send_json(WebSocket, _{operation:list, items:Items}).

handle_message(WebSocket, "got_item", Message):-
        Name = Message.name,
        retractall(pending_item(Name, _)),
        ws_send_json(WebSocket, _{operation:got_item_ack, name:Name}),
        format(user_error, 'Sent reply~n', []).

ws_send_json(WebSocket, Json):-
        with_output_to(atom(Atom),
                       json_write(current_output, Json)),
        ws_send(WebSocket, text(Atom)).

run:-
        http_server(http_dispatch, [port(9999)]).

%:-dynamic(pending_item/2).

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

