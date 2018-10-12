:-module(api, []).

:-use_module(library(http/http_client)).
:- http_handler(root('api/add_item'), add_item, []).

add_item(Request):-
        memberchk(search(Search), Request),
        memberchk(key=Key, Search),
        http_read_data(Request, Item, []),
        ( list_item(Key, Item)->
            format('Content-Type: text/plain~n~n~w is already on the list', [Item])
        ; otherwise->
            handle_message(Key, want_item, _{name:Item}),
            format('Content-Type: text/plain~n~nOK, added', [])
        ).