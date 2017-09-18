-module(rebar3_hterl).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).

-export([]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        {name, ?PROVIDER},
        {module, ?MODULE},
        {namespace, hterl},
        {bare, true},
        {deps, ?DEPS},
        {example, "rebar3 hterl compile"},
        {short_desc, "Compile hterl modules."},
        {desc, "Compile hterl modules."},
        {opts, []}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Running hterl...", []),
    Apps = case rebar_state:current_app(State) of
        undefined ->
            rebar_state:project_apps(State);
        AppInfo ->
            [AppInfo]
    end,
    [begin
        Opts = rebar_app_info:opts(AppInfo),
        SourceDir = rebar_app_info:dir(AppInfo),
        HterlOpts = rebar_opts:get(Opts, hterl_opts, []),

        FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.hterl\$"),

        CompileFun = fun(Source, HterlOpts1) ->
            hterl:file(Source, HterlOpts1)
        end,

        rebar_base_compiler:run(HterlOpts, [], FoundFiles, CompileFun)
    end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
