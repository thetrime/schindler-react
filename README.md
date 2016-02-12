---++ Schindler v2

A replacement for Schindler v1, a grocery item organisational tool.

---++ Requirements:
   * npm
   * SWI-Prolog (At least 7.3.16)
   * sqlite3
   * sqliteodbc (in Ubuntu/Raspbian this is in package libsqliteodbc)

---++ Compilation
   npm install

---++ Start the server
   swipl -q -f server.pl -g run

---+++ Start the server in a way that ignores SIGHUP (ie from an ssh session)
   nohup swipl -q -f server.pl -g run -t wait >& /dev/null &

Then go to localhost:9999 to see the app!