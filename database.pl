:-module(database,
         [prepare_database/0,
          insert/2,
          delete/2,
          item_location/4,
          list_item/2,
          transaction/3]).

:-use_module(library(odbc)).
:-use_module(library(uuid)).

:-meta_predicate(transaction(+, -, 0)).

transaction(Key, Connection, Goal):-
        get_connection(Connection),
        setup_call_catcher_cleanup(true,
                                   once(Goal),
                                   Catcher,
                                   end_transaction(Key, Connection, Catcher, Goal)).

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

update_checkpoint(Connection, Key):-
        uuid(UUID),                
        % This is specific to SQLite
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT OR REPLACE INTO checkpoint(key, checkpoint) VALUES (?, ?)', [default, default], Statement, []),
                           odbc_execute(Statement, [Key, UUID], _),
                           odbc_free_statement(Statement)).


insert(Connection, item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO item(key, name) VALUES (?, ?)', [default, default], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

insert(Connection, list_item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO list_item(key, name) VALUES (?, ?)', [default, default], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

insert(Connection, known_item_location(Key, Item, Store, Location)):-
        setup_call_cleanup(odbc_prepare(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES (?, ?, ?, ?)', [default, default, default, default], Statement, []),
                           odbc_execute(Statement, [Key, Item, Store, Location], _),
                           odbc_free_statement(Statement)).

delete(Connection, item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM item WHERE key = ? AND name = ?', [default, default], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

delete(Connection, list_item(Key, Name)):-
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM list_item WHERE key = ? AND name = ?', [default, default], Statement, []),
                           odbc_execute(Statement, [Key, Name], _),
                           odbc_free_statement(Statement)).

delete(Connection, known_item_location(Key, Item, Store, Location)):-
        setup_call_cleanup(odbc_prepare(Connection, 'DELETE FROM known_item_location WHERE key = ? AND item = ? AND store = ? AND location = ?', [default, default, default, default], Statement, []),
                           odbc_execute(Statement, [Key, Item, Store, Location], _),
                           odbc_free_statement(Statement)).        

item_location(Key, Item, Store, Aisle):-
        get_connection(Connection),
        findall(Row,
                setup_call_cleanup(odbc_prepare(Connection, 'SELECT item, store, location FROM known_item_location WHERE key = ?', [default], Statement, []),
                                   odbc_execute(Statement, [Key], Row),
                                   odbc_free_statement(Statement)),
                Rows),
        member(row(Item, Store, Aisle), Rows).

item_location(Key, Item, Store, unknown):-
        % Yuck :(
        get_connection(Connection),
        findall(Row,
                setup_call_cleanup(odbc_prepare(Connection, 'SELECT i.name, s.name FROM item i CROSS JOIN store s WHERE NOT EXISTS (SELECT 1 FROM known_item_location k WHERE k.store = s.name AND k.item = i.name) AND s.key = ? AND i.key = ?', [default, default], Statement, []),
                                   odbc_execute(Statement, [Key, Key], Row),
                                   odbc_free_statement(Statement)),
                Rows),
        memberchk(row(Item, Store), Rows).

list_item(Key, Item):-
        get_connection(Connection),
        setup_call_cleanup(odbc_prepare(Connection, 'SELECT name FROM list_item WHERE key = ?', [default], Statement, []),
                           odbc_execute(Statement, [Key], row(Item)),
                           odbc_free_statement(Statement)).

get_connection(Connection):-
        with_mutex(connection_mutex,
                   get_connection_1(Connection)).

:-thread_local(cached_connection/1).
get_connection_1(Connection):-
        cached_connection(Connection), !.
get_connection_1(Connection):-
        odbc_connect(-,
                     Connection,
                     [driver_string('DRIVER={Sqlite3};Database=schindler.db'),
                      silent(true),
                      null({null}),
                      auto_commit(false)]),
        assert(cached_connection(Connection)).

prepare_database:-
        get_connection(Connection),
        catch(odbc_query(Connection, 'SELECT version FROM schema', row(Version)), _, Version = 0),
        upgrade_schema(Connection, Version).

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
        odbc_query(Connection, 'CREATE TABLE item(key VARCHAR, name VARCHAR)', _),
        odbc_query(Connection, 'CREATE TABLE list_item(key VARCHAR, name VARCHAR)', _),
        odbc_query(Connection, 'CREATE TABLE store(key VARCHAR, name VARCHAR)', _),
        odbc_query(Connection, 'CREATE TABLE aisle(key VARCHAR, name VARCHAR, store VARCHAR)', _),
        odbc_query(Connection, 'CREATE TABLE known_item_location(key VARCHAR, item VARCHAR, store VARCHAR, location VARCHAR)', _),
        odbc_query(Connection, 'CREATE TABLE checkpoint(key VARCHAR, checkpoint VARCHAR)', _),

        % Insert some random data
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

        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'apple\', \'qfc\', \'produce\')', _),
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'apple\', \'tesco\', \'produce\')', _),
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'apple\', \'home\', \'lounge\')', _),
        
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'orange\', \'qfc\', \'produce\')', _),
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'orange\', \'tesco\', \'produce\')', _),
        
        odbc_query(Connection, 'INSERT INTO known_item_location(key, item, store, location) VALUES(\'matt\', \'pillow\', \'home\', \'bathroom\')', _).
