all: compile

compile: rebar3
	./rebar3 compile

rebar3:
	wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
