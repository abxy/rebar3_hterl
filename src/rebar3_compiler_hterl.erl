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

    Dir = rebar_app_info:dir(AppInfo),
    Mappings = [{".erl", filename:join([Dir, "src"])}],
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".hterl",
      out_mappings => Mappings}.

needed_files(_, FoundFiles, Mappings, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles,
                           not lists:member(Source, FirstFiles),
                           rebar_compiler:needs_compile(Source, ".erl", Mappings)],

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

clean(HterlFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.hterl$", ".erl", [unicode]))
       || F <- HterlFiles]).

error_tuple(Module, Es, Ws, AllOpts, Opts) ->
    FormattedEs = format_error_sources(Es, AllOpts),
    FormattedWs = format_error_sources(Ws, AllOpts),
    rebar_compiler:error_tuple(Module, FormattedEs, FormattedWs, Opts).

format_error_sources(Es, Opts) ->
    [{rebar_compiler:format_error_source(Src, Opts), Desc}
     || {Src, Desc} <- Es].
