ERL ?= erl
APP := nerlo

.PHONY: deps

all: compile

compile: deps
	@./rebar compile
	(cd priv/java; mvn package)

deps:
	@./rebar get-deps

check: compile
	@./rebar eunit skip_deps=true

clean:
	@./rebar clean
	(cd priv/java; mvn clean)

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

rel: compile
	@./rebar generate force=1
