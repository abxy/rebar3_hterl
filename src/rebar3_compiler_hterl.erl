-module(rebar3_compiler_hterl).

-behaviour(rebar_compiler).

-export([context/1,
         needed_files/4,
         dependencies/3,
         compile/4,
         clean/2]).

context(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    Mappings = [{".beam", EbinDir}],

    OutDir = rebar_app_info:dir(AppInfo),
    SrcDirs = rebar_dir:src_dirs(rebar_app_info:opts(AppInfo), ["src"]),
    ExistingSrcDirs = lists:filter(fun(D) ->
                                           ec_file:is_dir(filename:join(OutDir, D))
                                   end, SrcDirs),

    RebarOpts = rebar_app_info:opts(AppInfo),
    ErlOpts = rebar_opts:erl_opts(RebarOpts),
    ErlOptIncludes = proplists:get_all_values(i, ErlOpts),
    InclDirs = lists:map(fun(Incl) -> filename:absname(Incl) end, ErlOptIncludes),

    #{src_dirs => ExistingSrcDirs,
      include_dirs => [filename:join([OutDir, "include"]) | InclDirs],
      src_ext => ".hterl",
      out_mappings => Mappings}.

needed_files(_, FoundFiles, Mappings, AppInfo) ->
    FirstFiles = [],

    RestFiles = [Source || Source <- FoundFiles,
                           rebar_compiler:needs_compile(Source, ".beam", Mappings)],

    HterlOpts = rebar_opts:get(rebar_app_info:opts(AppInfo), hterl_opts, []),

    {{FirstFiles, HterlOpts}, {RestFiles, HterlOpts}}.

dependencies(_, _, _) ->
    [].

compile(Source, [{_, OutDir}], AllOpts, HterlOpts) ->
    case hterl:file(Source, [{outdir, OutDir}, HterlOpts]) of
        ok ->
            ok;
        {ok, Ws} ->
            FormattedWs = format_error_sources(Ws, AllOpts),
            rebar_compiler:ok_tuple(Source, FormattedWs);
        {error, Es, Ws} ->
            error_tuple(Source, Es, Ws, AllOpts, HterlOpts)
    end.

target_base(OutDir, Source) ->
    filename:join(OutDir, filename:basename(Source, ".hterl")).

clean(Files, AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    [begin
         Target = target_base(EbinDir, File) ++ ".beam",
         file:delete(Target)
     end || File <- Files].

error_tuple(Module, Es, Ws, AllOpts, Opts) ->
    FormattedEs = format_error_sources(Es, AllOpts),
    FormattedWs = format_error_sources(Ws, AllOpts),
    rebar_compiler:error_tuple(Module, FormattedEs, FormattedWs, Opts).

format_error_sources(Es, Opts) ->
    [{rebar_compiler:format_error_source(Src, Opts), Desc}
     || {Src, Desc} <- Es].
