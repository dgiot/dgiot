#!/bin/bash
# English test script

echo "========================================"
echo "Start dgiot_task module test"
echo "========================================"

cd "$(dirname "$0")/../../.."

echo "1. Compile module..."
_build/emqx/rel/emqx/bin/emqx eval 'dgiot_plugin:compile(dgiot_task).'

echo "2. Test module loading..."
_build/emqx/rel/emqx/bin/emqx eval 'io:format("Test start~n").'
_build/emqx/rel/emqx/bin/emqx eval 'io:format("Module: ~p~n", [dgiot_task:module_info(name)]).'

echo "3. Test simple functions..."
_build/emqx/rel/emqx/bin/emqx eval 'Result = dgiot_task:string2value("1+2", <<"int">>), io:format("string2value result: ~p~n", [Result]).'
_build/emqx/rel/emqx/bin/emqx eval 'Result = dgiot_task:compare(5, <<"LT">>, 10), io:format("compare result: ~p~n", [Result]).'

echo "4. Test thing model functions..."
_build/emqx/rel/emqx/bin/emqx eval 'Props = dgiot_task:get_props(<<"test">>), io:format("props count: ~p~n", [length(Props)]).'
_build/emqx/rel/emqx/bin/emqx eval 'Control = dgiot_task:get_control(1, #{<<"value">> => 10}, <<"control">>), io:format("control result: ~p~n", [Control]).'

echo "5. Test data saving functions..."
_build/emqx/rel/emqx/bin/emqx eval 'Result = dgiot_task:save_td(<<"test">>, <<"device">>, #{<<"temp">> => 25}, #{<<"interval">> => 3}), io:format("save_td result: ~p~n", [Result]).'

echo "========================================"
echo "Test completed"
echo "========================================"
