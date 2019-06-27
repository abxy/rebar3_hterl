rebar3_hterl
=====

A rebar3 plugin for compiling [Hypertext Erlang (hterl)](https://github.com/abxy/hterl) files.

Build
-----

```
$ rebar3 compile
```

Use
---

To your rebar config, add the rebar3_hterl plugin and the hterl package as a dependency.


Configure the hterl compiler by adding a hterl_opts entry to rebar.config:

```
{hterl_opts, [
    {encoding, utf8}
]}.
```
