build:
	rebar get-deps compile

clean:
	rebar clean

test: build
	rebar eunit skip_deps=true

shell:
	erl +A 10 +K true -pa  deps/*/ebin ../*/ebin -s instathread_rest
