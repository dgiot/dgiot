%%--------------------------------------------------------------------
%% dgiot_shadow_api - Device Shadow REST API
%%
%% Endpoints:
%%   GET    /api/shadow/:device_id          -> get shadow
%%   PUT    /api/shadow/:device_id/desired  -> update desired
%%   GET    /api/shadow/:device_id/delta    -> get delta
%%   GET    /api/shadow?product_id={id}     -> list shadows
%%   DELETE /api/shadow/:device_id          -> delete shadow
%%   POST   /api/shadow/:device_id/sync     -> push desired to device
%%--------------------------------------------------------------------
-module(dgiot_shadow_api).
-author("dgaiot").

-export([
    get_shadow/1,
    update_desired/2,
    get_delta/1,
    list_shadows/1,
    delete_shadow/1,
    sync_device/1
]).

%% GET /api/shadow/:device_id
get_shadow(#{<<"device_id">> := DeviceId}) ->
    case dgiot_shadow:get_shadow(DeviceId) of
        {ok, Shadow} ->
            {ok, dgiot_shadow:to_map(Shadow)};
        {error, not_found} ->
            {ok, #{
                <<"device_id">> => DeviceId,
                <<"desired">> => #{},
                <<"reported">> => #{},
                <<"delta">> => #{},
                <<"version">> => 0,
                <<"connected">> => dgiot_device:get_online(DeviceId)
            }}
    end.

%% PUT /api/shadow/:device_id/desired
update_desired(#{<<"device_id">> := DeviceId}, #{<<"desired">> := Desired}) ->
    case dgiot_shadow:update_desired(DeviceId, Desired) of
        {ok, Version, Delta} ->
            {ok, #{
                <<"device_id">> => DeviceId,
                <<"version">> => Version,
                <<"delta">> => Delta,
                <<"sync_status">> => case map_size(Delta) of
                    0 -> <<"synced">>;
                    _ -> <<"pending">>
                end
            }};
        {error, Reason} ->
            {error, Reason}
    end;
update_desired(_, _) ->
    {error, <<"missing 'desired' field in body">>}.

%% GET /api/shadow/:device_id/delta
get_delta(#{<<"device_id">> := DeviceId}) ->
    Delta = dgiot_shadow:get_delta(DeviceId),
    {ok, #{
        <<"device_id">> => DeviceId,
        <<"delta">> => Delta,
        <<"has_pending">> => map_size(Delta) > 0
    }}.

%% GET /api/shadow?product_id={id}
list_shadows(#{<<"product_id">> := ProductId}) ->
    Shadows = dgiot_shadow:list_shadows(ProductId),
    List = lists:map(fun dgiot_shadow:to_map/1, Shadows),
    {ok, #{
        <<"product_id">> => ProductId,
        <<"total">> => length(List),
        <<"shadows">> => List
    }};
list_shadows(_) ->
    {error, <<"missing 'product_id' query parameter">>}.

%% DELETE /api/shadow/:device_id
delete_shadow(#{<<"device_id">> := DeviceId}) ->
    ok = dgiot_shadow:delete_shadow(DeviceId),
    {ok, #{<<"deleted">> => DeviceId}}.

%% POST /api/shadow/:device_id/sync
sync_device(#{<<"device_id">> := DeviceId}) ->
    case dgiot_shadow:sync_to_device(DeviceId) of
        ok ->
            {ok, #{<<"device_id">> => DeviceId, <<"sync">> => <<"pushed">>}};
        {error, Reason} ->
            {error, Reason}
    end.
