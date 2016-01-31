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

Then go to localhost:9999 to see the app