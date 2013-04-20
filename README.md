OVERVIEW
--------

With "nerlo" you get an Erlang bridge to the neo4j graph database.


DEPENDENCIES
------------

JDK >= 1.6.0
Erlang >= R13B04
GNU make
Maven 3

log4erl-0.9.0

BUILD
-----

You may try to build from source like this:

$ ./conf.sh
$ make


RUN
---

$ cd erl
$ bin/shell
erl 1> ej_app:start().
erl 2> ej_srv:ping().
erl 3> neo4j_app:start().
erl 4> neo4j:has_db().
erl 5> V1 = neo4j:add_vertex().
erl 6> V2 = neo4j:add_vertex().
erl 7> E1 = neo4j:add_edge(V1,V2).
[...]
erl 998> neo4j_app:stop().
erl 999> ej_app:stop().




