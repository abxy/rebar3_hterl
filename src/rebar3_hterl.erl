-module(rebar3_hterl).

-export([init/1]).

init(State0) ->
    State = rebar_state:append_compilers(State0, [rebar3_compiler_hterl]),
    {ok, State}.