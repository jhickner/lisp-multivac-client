# multivac client

This is the terminal client for accessing the multivac server. Multivac
is a simple app for storing tagged snippets of information.

This is probably the 10th version of this note-taking app. I rewrite it
probably once a year. This time the server is written in Clojure, and
the command-line client is written in Common Lisp since the JVM startup
time made the Clojure version a little too slow.

The name comes from a short story by Isaac Asimov about a computer that
eventually collects enough information to recreate the universe:
http://www.multivax.com/last_question.html


## Usage

(I usually alias multivac to just "m" and multivac add to just "ma")

```bash
~ ➔ m
Usage: multivac
                [<tag> <tag> <tag>...] - search by tag
                [add <tag,tag...> <body> [-l link]] - add a new item
                [tags] - list your top 20 tags
                [dump] - dump a json stream of all items to stdout
                [-d <item-id>] - delete an item
```

## License

Copyright (C) 2012 Jason Hickner

Distributed under the Eclipse Public License, the same as Clojure.
