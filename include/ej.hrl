-define(DEBUG, true).

-define(APP, nerlo).

-define(TAG_OK, ok).
-define(TAG_ERROR, error).
-define(TAG_DATA, data).
-define(TAG_CALL, call).
-define(TAG_NODE, node).
-define(TAG_FRAGMENT, fragment).

-define(EJMSGREF(Pid,Ref), {Pid,Ref}).
-define(EJMSG(Ref,Tag,Body), {self(), Ref, {Tag, Body}}).
-define(EJMSGPART(Key, Value), {Key, Value}).

-define(EJCALLBACKSTOP, 'EJCALLBACKSTOP').
-define(EJCALLBACKTIMEOUT, 'EJCALLBACKTIMEOUT').

-define(PROPS_FILE, "nerlo.properties.file").
