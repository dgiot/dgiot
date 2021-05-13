%% @doc
%% Module that implements this behaviour can be used
%% as `foramt' parameter for exporters.
%% Built-in formats:
%% - {@link prometheus_text_format}
%% - {@link prometheus_protobuf_format}
%%
%% Callbacks:
%% - `content_type()` - should return content type of the format;
%% - `format()` - should format `default' regsitry;
%% - `format(Registry)` - should format `Registry'.
%% @end
-module(prometheus_format).

%%====================================================================
%% Callbacks
%%====================================================================

-callback content_type() -> binary().

-callback format() -> binary().

-callback format(Registry :: prometheus_registry:registry()) -> binary().
