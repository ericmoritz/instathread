build: 
	rebar get-deps compile

shell: 
	PORT=8000 erl -pa ../*/ebin deps/*/ebin -s instathread

test:
	rebar eunit skip_deps=true

clean:
	rebar clean
