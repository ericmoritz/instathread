build:
	rebar get-deps compile

test:
	rebar eunit skip_deps=true

clean:
	rebar clean

shell:
	erl -pa deps/*/ebin ../*/ebin -s instathread_db
