rebar3_hterl
=====

A rebar3 plugin for compiling [hterl](https://github.com/abxy/hterl) files.

Build
-----

```
$ rebar3 compile
```

Use
---

To your rebar config, add the rebar3_hterl plugin and the hterl package as a dependency.

```
{deps, [
    {hterl, ".*", {git, "https://github.com/abxy/hterl.git", {branch, "master"}}}
]}.
{plugins, [
    {rebar3_hterl, ".*", {git, "https://github.com/abxy/rebar3_hterl.git", {branch, "master"}}}
]}.
```

Then just call your plugin directly in an existing application:

```
$ rebar3 hterl compile
```

To have it invoked automatically when running rebar3 compile add a hook in rebar.config

```
{provider_hooks, [
    {pre, [{compile, {hterl, compile}}]}
]}.
```

Configure the hterl compiler by adding a hterl_opts entry to rebar.config:

```
{hterl_opts, [
    precompile,
    {encoding, utf8}
]}.
```
