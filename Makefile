build: get-deps
	rebar compile

get-deps:
	rebar get-deps

shell:
	PORT=8000 erl -pa apps/*/ebin deps/*/ebin -s instathread

dialyzer: build
	dialyzer --src apps/*/src deps/*/src -I deps/
test:
	rebar eunit skip_deps=true

clean:
	rebar clean

rel:
	./relcool
