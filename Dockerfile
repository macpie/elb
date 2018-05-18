FROM erlang:20.3

WORKDIR /opt/elb

ADD rebar.config rebar.config
ADD rebar.lock rebar.lock
RUN rebar3 get-deps
RUN rebar3 compile

ADD src/ src/
ADD test/ test/
RUN rebar3 compile

CMD ["rebar3", "shell"]
