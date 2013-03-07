build: 
	rebar get-deps compile

shell: 
	erl -pa ../*/ebin deps/*/ebin -s instathread

test:
	rebar eunit skip_deps=true
