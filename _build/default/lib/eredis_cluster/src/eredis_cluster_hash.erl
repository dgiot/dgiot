-module(eredis_cluster_hash).

%% API.
-export([hash/1]).

-include("eredis_cluster.hrl").

%% API.
-spec hash(string()) -> integer().
hash(Key) ->
    crc16(Key) rem ?REDIS_CLUSTER_HASH_SLOTS.

-spec crc16(string()) -> integer().
crc16(Key) ->
    crc16(Key,0).

-spec crc16(string(), integer()) -> integer().
crc16([B|T], Crc) ->
    Index = (((Crc bsr 8) bxor B) band 16#ff),
    NewCrc = ((Crc bsl 8) band 16#ffff) bxor crc_index(Index),
    crc16(T,NewCrc);
crc16([],Crc) ->
    Crc.

-spec crc_index(integer()) -> integer().
crc_index(N) ->
    <<Crc:16>> = binary:part(?CRCDEF,N*2,2),
    Crc.
