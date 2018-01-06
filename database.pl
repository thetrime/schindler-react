:-module(database,
         [prepare_database/0,
          insert/2,
          delete/2,
          update/2,
          check_login/2,
          item_location/4,
          store/4,
          item/2,
          aisle/3,
          checkpoint/2,
          list_item/2,
          transaction/3]).

:-use_module(library(odbc)).

:-meta_predicate(transaction(+, -, 0)).

heroku:-
        % True if in a Heroku environment
        current_predicate(heroku_db_connect/1).

transaction(Key, Connection, Goal):-
        with_connection(Connection,
                        setup_call_catcher_cleanup(true,
                                                   once(Goal),
                                                   Catcher,
                                                   end_transaction(Key, Connection, Catcher, Goal))).

end_transaction(_, Connection, fail, Goal):-
        log('Goal ~q failed', [Goal]),
        odbc_end_transaction(Connection, rollback).

end_transaction(_, Connection, error(Error), Goal):-
        log('Goal ~q raised ~w', [Goal, Error]),
        odbc_end_transaction(Connection, rollback).

end_transaction(Key, Connection, exit, _):-
        update_checkpoint(Connection, Key),
        odbc_end_transaction(Connection, commit).

end_transaction(Key, Connection, !, _):-
        update_checkpoint(Connection, Key),
        odbc_end_transaction(Connection, commit).


% The Heroku buildpack does not have library(uuid).
% Instead, we will just use the SERIAL type to autogenerate unique values on insert
:-if(heroku).
update_checkpoint(Connection, Key):-
        delete_checkpoint(Connection, Key),
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO checkpoint(key) VALUES (?)', [varchar], Statement, []),
                           odbc_execute(Statement, [Key], _),
                           odbc_free_statement(Statement)).
:-else.
:-use_module(library(uuid)).
update_checkpoint(Connection, Key):-
        delete_checkpoint(Connection, Key),
        uuid(UUID),
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO checkpoint(key, checkpoint) VALUES (?, ?)', [varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, UUID], _),
                           odbc_free_statement(Statement)).
:-endif.



delete_checkpoint(Connection, Key):-
         setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM checkpoint WHERE key = ?', [varchar], Statement, []),
                           odbc_execute(Statement, [Key], _),
                           odbc_free_statement(Statement)).


insert(Connection, item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO item(key, name) VALUES (?, ?)', [varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

insert(Connection, list_item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO list_item(key, name) VALUES (?, ?)', [varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

insert(Connection, known_item_location(Key, Item, Store, Location)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES (?, ?, ?, ?)', [varchar, varchar, varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, Item, Store, Location], _),
                           odbc_free_statement(Statement)).

insert(Connection, user(Username, Password)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO users(username, password) VALUES (?, ?)', [varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Username, Password], _),
                           odbc_free_statement(Statement)).


insert(Connection, store(Key, Name, Latitude, Longitude)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO store(key, name, latitude, longitude) VALUES (?, ?, ?, ?)', [varchar, varchar, float, float], Statement, []),
                           odbc_execute(Statement, [Key, Name, Latitude, Longitude], _),
                           odbc_free_statement(Statement)).

insert(Connection, aisle(Key, Name, Store)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO aisle(key, name, store) VALUES (?, ?, ?)', [varchar, varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, Name, Store], _),
                           odbc_free_statement(Statement)).


update(Connection, store(Key, Name, Latitude, Longitude)):-
        setup_call_cleanup(odbc_prepare(Connection, 'UPDATE store SET latitude=?, longitude=? WHERE name=? AND key=?', [float, float, varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Latitude, Longitude, Name, Key], _),
                           odbc_free_statement(Statement)).


delete(Connection, item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM item WHERE key = ? AND name = ?', [varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

delete(Connection, list_item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM list_item WHERE key = ? AND name = ?', [varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

delete(Connection, known_item_location(Key, Item, Store)):-
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM known_item_location WHERE key = ? AND item = ? AND store = ?', [varchar, varchar, varchar], Statement, []),
                           odbc_execute(Statement, [Key, Item, Store], _),
                           odbc_free_statement(Statement)).

delete(Connection, store(Key, Store)):-
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM known_item_location WHERE key = ? AND store = ?', [varchar, varchar], Statement1, []),
                           odbc_execute(Statement1, [Key, Store], _),
                           odbc_free_statement(Statement1)),
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM aisle WHERE key = ? AND store = ?', [varchar, varchar], Statement2, []),
                           odbc_execute(Statement2, [Key, Store], _),
                           odbc_free_statement(Statement2)),
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM store WHERE key = ? AND name = ?', [varchar, varchar], Statement3, []),
                           odbc_execute(Statement3, [Key, Store], _),
                           odbc_free_statement(Statement3)).


item_location(Key, Item, Store, Aisle):-
        select(Connection,
               findall(Row,
                       setup_call_cleanup(odbc_prepare(Connection, 'SELECT item, store, location FROM known_item_location WHERE key = ?', [varchar], Statement, []),
                                          odbc_execute(Statement, [Key], Row),
                                          odbc_free_statement(Statement)),
                       Rows)),
        member(row(Item, Store, Aisle), Rows).

item(Key, Item):-
        select(Connection,
               findall(Row,
                       setup_call_cleanup(odbc_prepare(Connection, 'SELECT name FROM item WHERE key = ?', [varchar], Statement, []),
                                          odbc_execute(Statement, [Key], Row),
                                          odbc_free_statement(Statement)),
                       Rows)),
        member(row(Item), Rows).

aisle(Key, Name, Store):-
        select(Connection,
               findall(Row,
                       setup_call_cleanup(odbc_prepare(Connection, 'SELECT name, store FROM aisle WHERE key = ?', [varchar], Statement, []),
                                          odbc_execute(Statement, [Key], Row),
                                          odbc_free_statement(Statement)),
                       Rows)),
        member(row(Name, Store), Rows).

store(Key, Store, Latitude, Longitude):-
        select(Connection,
               findall(Row,
                       setup_call_cleanup(odbc_prepare(Connection, 'SELECT name, latitude, longitude FROM store WHERE key = ?', [varchar], Statement, []),
                                          odbc_execute(Statement, [Key], Row),
                                          odbc_free_statement(Statement)),
                       Rows)),
        member(row(Store, Latitude, Longitude), Rows).

list_item(Key, Item):-
        select(Connection,
               setup_call_cleanup(odbc_prepare(Connection, 'SELECT name FROM list_item WHERE key = ?', [varchar], Statement, []),
                                  odbc_execute(Statement, [Key], row(Item)),
                                  odbc_free_statement(Statement))).

check_login(Username, Password):-
        format(user_error, 'in check_login/2~n', []),
        ( select(Connection,
                 setup_call_cleanup(odbc_prepare(Connection, 'SELECT password FROM users WHERE username = ?', [varchar], Statement, []),
                                    odbc_execute(Statement, [Username], row(ExpectedPassword)),
                                    odbc_free_statement(Statement)))->
            Password == ExpectedPassword
        ; otherwise->
            % New user. Just check them in
            transaction(Username, Connection, insert(Connection, user(Username, Password)))
        ).

checkpoint(Key, Checkpoint):-
        ( select(Connection,
                 setup_call_cleanup(odbc_prepare(Connection, 'SELECT checkpoint FROM checkpoint WHERE key = ?', [varchar], Statement, []),
                                    odbc_execute(Statement, [Key], row(Checkpoint)),
                                    odbc_free_statement(Statement)))->
            true
        ; otherwise->
            transaction(Key, _, true),
            checkpoint(Key, Checkpoint)
        ).

:-meta_predicate(select(?, 0)).
select(Connection, Goal):-
        with_connection(Connection, Goal).

% In Heroku we cannot cache connections or we just end up accumulating hundreds of them and the database eventually starts rejecting the logins
:-meta_predicate(with_connection(?, 0)).
with_connection(Connection, Goal):-
        current_predicate(heroku_db_connect/1),
        !,
        setup_call_cleanup(heroku_db_connect(Connection),
                           Goal,
                           odbc_disconnect(Connection)).

:-thread_local
        cached_connection/1.

with_connection(Connection, Goal):-
        ( cached_connection(Connection)->
            true
        ; with_mutex(connection_mutex,
                     cache_new_connection(Connection))
        ),
        Goal.

cache_new_connection(Connection):-
        odbc_connect(-,
                     Connection,
                     [driver_string('DRIVER={Sqlite3};Database=schindler.db;FKSupport=True'),
                      silent(true),
                      null({null}),
                      auto_commit(false)]),
        assert(cached_connection(Connection)).



prepare_database:-
        with_connection(Connection,
                        ( catch(odbc_query(Connection, 'SELECT version FROM schema', row(Version)), _, Version = 0),
                          upgrade_schema(Connection, Version)
                        )).

upgrade_schema(Connection, From):-
        upgrade_schema_from(Connection, From),
        NewVersion is From + 1,
        !,
        upgrade_schema(Connection, NewVersion).

upgrade_schema(Connection, LastVersion):-
        format(atom(SQL), 'UPDATE schema SET version = ~w', [LastVersion]),
        odbc_query(Connection, SQL, _),
        odbc_end_transaction(Connection, commit).

upgrade_schema_from(Connection, 0):-
        odbc_query(Connection, 'CREATE TABLE schema(version INTEGER)', _),
        odbc_query(Connection, 'INSERT INTO schema(version) VALUES (1)', _),
        odbc_query(Connection, 'CREATE TABLE item(key VARCHAR, name VARCHAR, PRIMARY KEY(key, name))', _),
        odbc_query(Connection, 'CREATE TABLE list_item(key VARCHAR, name VARCHAR, PRIMARY KEY(key, name), FOREIGN KEY(key, name) REFERENCES item(key, name))', _),
        odbc_query(Connection, 'CREATE TABLE store(key VARCHAR, name VARCHAR, latitude VARCHAR, longitude VARCHAR, PRIMARY KEY(key, name))', _),
        odbc_query(Connection, 'CREATE TABLE aisle(key VARCHAR, store VARCHAR, name VARCHAR, PRIMARY KEY(key, store, name), FOREIGN KEY(key, store) REFERENCES store(key, name))', _),
        odbc_query(Connection, 'CREATE TABLE known_item_location(key VARCHAR, item VARCHAR, store VARCHAR, location VARCHAR, FOREIGN KEY(key, item) REFERENCES item(key, name), FOREIGN KEY(key, store) REFERENCES store(key, name), FOREIGN KEY(key, store, location) REFERENCES aisle(key, store, name))', _),
        ( heroku ->
            odbc_query(Connection, 'CREATE TABLE checkpoint(key VARCHAR, checkpoint SERIAL)', _)
        ; otherwise->
            odbc_query(Connection, 'CREATE TABLE checkpoint(key VARCHAR, checkpoint VARCHAR)', _)
        ),
        % Insert some random data        
        odbc_query(Connection, 'INSERT INTO store(key, name) VALUES (\'matt\', \'home\')', _),
        odbc_query(Connection, 'INSERT INTO store(key, name) VALUES (\'matt\', \'tesco\')', _),
        odbc_query(Connection, 'INSERT INTO store(key, name) VALUES (\'matt\', \'qfc\')', _),

        odbc_query(Connection, 'INSERT INTO aisle(key, store, name) VALUES (\'matt\', \'home\', \'lounge\')', _),
        odbc_query(Connection, 'INSERT INTO aisle(key, store, name) VALUES (\'matt\', \'home\', \'kitchen\')', _),
        odbc_query(Connection, 'INSERT INTO aisle(key, store, name) VALUES (\'matt\', \'home\', \'bathroom\')', _),
        odbc_query(Connection, 'INSERT INTO aisle(key, store, name) VALUES (\'matt\', \'home\', \'bedroom\')', _),

        odbc_query(Connection, 'INSERT INTO aisle(key, store, name) VALUES (\'matt\', \'qfc\', \'produce\')', _),
        odbc_query(Connection, 'INSERT INTO aisle(key, store, name) VALUES (\'matt\', \'tesco\', \'produce\')', _),        

        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'apple\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'orange\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'guitar\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'banjo\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'soap\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'chutney\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'fish paste\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'gruel\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'pillow\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'boxes\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'apes\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'trampoline\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'curtains\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'floor\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'old timey jig\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'clocks\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'banana\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'guava\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'peach\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'durian\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'mango\')', _),
        odbc_query(Connection, 'INSERT INTO item(key, name) VALUES(\'matt\', \'worms\')', _),

        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'apple\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'orange\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'guitar\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'banjo\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'soap\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'chutney\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'fish paste\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'gruel\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'boxes\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'apes\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'trampoline\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'floor\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'old timey jig\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'banana\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'guava\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'peach\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'durian\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'mango\')', _),
        odbc_query(Connection, 'INSERT INTO list_item(key, name) VALUES (\'matt\', \'worms\')', _),
        
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'apple\', \'qfc\', \'produce\')', _),
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'apple\', \'tesco\', \'produce\')', _),
        %odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'apple\', \'home\', \'lounge\')', _),
        
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'orange\', \'qfc\', \'produce\')', _),
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'orange\', \'tesco\', \'produce\')', _).
        
        %odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'pillow\', \'home\', \'bathroom\')', _).

upgrade_schema_from(Connection, 1):-
        odbc_query(Connection, 'CREATE TABLE users(username VARCHAR, password VARCHAR)', _).


set_random_locations:-
        transaction(matt, Connection, set_random_locations(Connection)).

set_random_locations(Connection):-
        % Create random locations for everything.
        findall(Aisle,
                aisle(matt, Aisle, home),
                Aisles),
        forall(item(matt, Item),
               ( random_member(Location, Aisles),
                 insert(Connection, known_item_location(matt, Item, home, Location))
               )),
        odbc_end_transaction(Connection, commit).