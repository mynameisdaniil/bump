bump
=====

NPM-like semantic versioning for rebar3

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { bump, ".*", {git, "git@host:user/bump.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 bump
    ===> Fetching bump
    ===> Compiling bump
    <Plugin Output>
