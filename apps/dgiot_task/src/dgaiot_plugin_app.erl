-module(dgaiot_plugin_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start shadow hub and worker as supervised children
    io:format("[dgaiot] Plugin starting...~n"),
    Children = [
        %% These are MQTT client processes - start lazily if EMQX is up
    ],
    dgaiot_plugin_sup:start_link(Children).

stop(_State) ->
    io:format("[dgaiot] Plugin stopped~n"),
    ok.
