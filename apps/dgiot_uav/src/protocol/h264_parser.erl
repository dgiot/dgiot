%%%-------------------------------------------------------------------
%%% @doc
%%% h264_parser.erl - H.264视频流解析模块
%%%
%%% 本模块负责解析H.264编码的图像数据，查找NAL单元，识别帧类型。
%%% 协议对应：Payload.docx 中的“压缩数字视频格式”和“图像协议”
%%% H.264编码要求：包含IDR帧、P帧，不含B帧，IDR频率不低于1Hz。
%%% NAL单元起始码为 0x00 0x00 0x00 0x01 或 0x00 0x00 0x01。
%%%
%%% 主要功能：
%%% - parse_stream/1: 解析整个H.264码流
%%% - find_nal_units/1: 查找所有NAL单元
%%% - parse_nal_unit/1: 解析NAL单元头部，获取类型
%%% - detect_frame_type/1: 根据NAL类型检测帧类型（I帧、P帧等）
%%% - identify_image_type/1: 根据数据识别图像类型（可见光/红外）【占位实现】
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(h264_parser).
-export([parse_stream/1, find_nal_units/1, parse_nal_unit/1, detect_frame_type/1]).


-export([identify_image_type/1]).

parse_stream(PayloadBin) ->
    % {ok, PayloadBin} = file:read_file(FilePath),
    
    % NAL
    NALUnits = find_nal_units(PayloadBin),
    
    % NAL
    analyze_nal_units(NALUnits),
    ok.


find_nal_units(PayloadBin) ->
    find_nal_units(PayloadBin, [], 0).

find_nal_units(<<>>, Acc, _) -> lists:reverse(Acc);
find_nal_units(PayloadBin, Acc, Pos) ->
    case find_start_code(PayloadBin) of
        {ok, StartPos, StartCodeLen} ->
            % 
            Rest = binary:part(PayloadBin, StartPos + StartCodeLen, 
                             byte_size(PayloadBin) - StartPos - StartCodeLen),
            case find_start_code(Rest) of
                {ok, NextStartPos, _} ->
                    % NAL
                    NALLength = NextStartPos,
                    NALData = binary:part(Rest, 0, NALLength),
                    NewAcc = [NALData | Acc],
                    NewBinary = binary:part(Rest, NextStartPos, 
                                         byte_size(Rest) - NextStartPos),
                    find_nal_units(NewBinary, NewAcc, Pos + StartPos + StartCodeLen + NextStartPos);
                error ->
                    % NAL
                    NALData = Rest,
                    lists:reverse([NALData | Acc])
            end;
        error ->
            lists:reverse(Acc)
    end.


find_start_code(<<0,0,0,1, _Rest/binary>>) -> {ok, 0, 4};
find_start_code(<<0,0,1, _Rest/binary>>) -> {ok, 0, 3};
find_start_code(<<_, Rest/binary>>) ->
    case find_start_code(Rest) of
        {ok, Pos, Len} -> {ok, Pos + 1, Len};
        error -> error
    end;
find_start_code(_) -> error.


analyze_nal_units([]) -> ok;
analyze_nal_units([NALData | Rest]) ->
    case parse_nal_unit(NALData) of
        {ok, Type, _Description} ->
            detect_frame_type(Type);
        error ->
            io:format("~s ~p Invalid NAL unit, size: ~p bytes~n", [?FILE, ?LINE, byte_size(NALData)])
    end,
    analyze_nal_units(Rest).


parse_nal_unit(<<Header, _/binary>> = NALData) when byte_size(NALData) > 0 ->
    % NAL[7](@ref)
    ForbiddenBit = (Header band 16#80) bsr 7,  % （1bit）
    _NRI = (Header band 16#60) bsr 5,           % （2bit）
    Type = Header band 16#1F,                  % NAL（5bit）
    
    % 
    if 
        ForbiddenBit == 1 ->
            {error, "1，"};
        true ->
            Description = get_nal_type_description(Type),
            {ok, Type, Description}
    end;
parse_nal_unit(_) -> {error, "NAL"}.


get_nal_type_description(1) -> "Non-IDR slice (P)";
get_nal_type_description(5) -> "IDR slice (I)";
get_nal_type_description(6) -> "SEI";
get_nal_type_description(7) -> "SPS";
get_nal_type_description(8) -> "PPS";
get_nal_type_description(_) -> "Other".


detect_frame_type(5) ->
    % I
    ok;
detect_frame_type(1) ->
    % P
    ok;
detect_frame_type(7) ->
    parse_sps_data(),
    ok;
detect_frame_type(8) ->
    ok;
detect_frame_type(_) -> ok.


parse_sps_data() ->
    % ，SPS、[6](@ref)
    ok.


process_visible_image(YUVData) when byte_size(YUVData) =:= 115 ->
    % ，115，YUV 4:2:0
    io:format("Processing visible light YUV image data~n"),
    % YUV 4:2:0
    ok.


process_infrared_image(GrayData) when byte_size(GrayData) =:= 115 ->
    % ，115，8bit
    io:format("Processing infrared thermal image grayscale data~n"),
    % 
    ok.


identify_image_type(NALData) ->
    % ，
    % ：
    case is_infrared_image(NALData) of
        true -> process_infrared_image(NALData);
        false -> process_visible_image(NALData)
    end.


is_infrared_image(_Data) ->
    % 
    % 
    rand:uniform() > 0.5.