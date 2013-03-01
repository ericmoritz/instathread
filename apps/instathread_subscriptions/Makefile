build: get-deps
	rebar compile

get-deps:
	rebar get-deps
test:
	rebar eunit

shell:
	erl -pa ../*/ebin deps/*/ebin -s instathread_subscriptions
