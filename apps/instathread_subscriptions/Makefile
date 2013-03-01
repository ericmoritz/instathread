build: get-deps
	rebar compile

get-deps:
	rebar get-deps
test:
	rebar eunit skip_deps=true

shell:
	erl -pa ../*/ebin deps/*/ebin -s instathread_subscriptions
