---++ Schindler v2

A replacement for Schindler v1, a grocery item organisational tool.

---++ Requirements:
   * npm
   * SWI-Prolog (At least 7.3.15 - head ref a4b7b093)


---++ Compilation
   npm install

---++ Start the server
   swipl -q -f server.pl -g run

Then go to localhost:9999/schindler to see the app