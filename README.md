rebar3_hterl
=====

A rebar3 plugin for hterl.

Build
-----

```
$ rebar3 compile
```

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_hterl, ".*", {git, "https://github.com/abxy/rebar3_hterl.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:

```
$ rebar3 hterl compile
```

To have it invoked automatically when running rebar3 compile add it as a provider_hooks:

```
{provider_hooks, [
    {pre, [{compile, {hterl, compile}}]}
]}.
```
