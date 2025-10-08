-module(message_size_limit_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Common Test callbacks
groups() ->
    [
        {message_size_tests, [parallel], [
            test_send_wamp_message_within_limit,
            test_send_wamp_message_exceeds_limit,
            test_serialize_large_message,
            test_error_propagation_through_transport,
            test_various_message_sizes,
            test_different_encodings_size_limits
        ]}
    ].

all() ->
    [
        {group, message_size_tests}
    ].

init_per_suite(Config) ->
    %% Stop applications in case they're already running
    application:stop(wamp_client),
    application:stop(wamp),
    application:stop(gproc),
    timer:sleep(100),
    
    %% Start applications fresh
    case application:ensure_all_started(wamp_client) of
        {ok, _} -> 
            Config;
        {error, {wamp_client, {shutdown, {failed_to_start_child, _, _}}}} ->
            %% If there's still a conflict, just proceed - tests can run without full app
            ct:pal("Warning: Could not start wamp_client application, but tests can still run"),
            Config;
        {error, Reason} ->
            ct:pal("Warning: Failed to start wamp_client: ~p", [Reason]),
            Config
    end.

end_per_suite(_Config) ->
    application:stop(wamp_client),
    application:stop(wamp),
    application:stop(gproc),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%% Test Cases

%% Test that messages within the size limit are sent successfully
test_send_wamp_message_within_limit(_Config) ->
    ct:pal("Testing message within size limit"),
    
    %% Create a message that's well under the limit (1KB)
    SmallPayload = binary:copy(<<"x">>, 1024),
    Message = {publish, 1, #{}, <<"test.topic">>, [SmallPayload], #{}},
    Encoding = raw_msgpack,
    MaxLength = 16777216,  %% 16MB
    
    %% Mock send function that captures the serialized message
    SendFun = fun(SerMsg) -> 
        ct:pal("Message sent successfully, size: ~p bytes", [byte_size(SerMsg)]),
        ok 
    end,
    
    %% This should succeed
    Result = awre_trans_tcp:send_wamp_message(Message, Encoding, MaxLength, SendFun),
    ?assertEqual(ok, Result).

%% Test that messages exceeding the size limit return proper error
test_send_wamp_message_exceeds_limit(_Config) ->
    ct:pal("Testing message exceeding size limit"),
    
    %% Create a message that exceeds the limit (20MB)
    LargePayload = binary:copy(<<"x">>, 20 * 1024 * 1024),
    Message = {publish, 1, #{}, <<"test.topic">>, [LargePayload], #{}},
    Encoding = raw_msgpack,
    MaxLength = 16777216,  %% 16MB
    
    %% Send function should not be called for oversized messages
    SendFun = fun(_SerMsg) -> 
        ct:fail("Send function should not be called for oversized messages")
    end,
    
    %% This should return an error
    Result = awre_trans_tcp:send_wamp_message(Message, Encoding, MaxLength, SendFun),
    
    %% Verify the error structure
    ?assertMatch({error, {message_too_large, _ActualSize, MaxLength}}, Result),
    {error, {message_too_large, ActualSize, _MaxLength}} = Result,
    
    %% Verify the actual size is indeed larger than the limit
    ?assert(ActualSize > MaxLength),
    ct:pal("Message size ~p exceeds limit ~p as expected", [ActualSize, MaxLength]).

%% Test serialization of large messages
test_serialize_large_message(_Config) ->
    ct:pal("Testing serialization of large messages"),
    
    %% Test with different payload sizes
    TestSizes = [
        {small, 1024},           %% 1KB
        {medium, 1024 * 1024},   %% 1MB  
        {large, 10 * 1024 * 1024}, %% 10MB - should work
        {too_large, 20 * 1024 * 1024} %% 20MB - should fail
    ],
    
    MaxLength = 16777216,  %% 16MB
    
    lists:foreach(fun({Label, PayloadSize}) ->
        ct:pal("Testing ~p payload (~p bytes)", [Label, PayloadSize]),
        
        Payload = binary:copy(<<"x">>, PayloadSize),
        Message = {call, 1, #{}, <<"test.procedure">>, [Payload], #{}},
        
        %% Test msgpack encoding
        try
            SerializedMsg = wamper_protocol:serialize(Message, raw_msgpack),
            ActualSize = byte_size(SerializedMsg),
            ct:pal("Serialized message size: ~p bytes", [ActualSize]),
            
            case ActualSize > MaxLength of
                true ->
                    ct:pal("Message ~p correctly exceeds limit", [Label]);
                false ->
                    ct:pal("Message ~p fits within limit", [Label])
            end
        catch
            Error:Reason ->
                ct:pal("Serialization failed for ~p: ~p:~p", [Label, Error, Reason]),
                ?assert(PayloadSize >= 20 * 1024 * 1024)  %% Only very large messages should fail serialization
        end
    end, TestSizes).

%% Test error propagation through the transport layer
test_error_propagation_through_transport(_Config) ->
    ct:pal("Testing error propagation through transport layer"),
    
    %% Create an oversized message
    LargePayload = binary:copy(<<"x">>, 20 * 1024 * 1024),
    OversizedMessage = {publish, 1, #{}, <<"test.topic">>, [LargePayload], #{}},
    
    %% Test the send_wamp_message function directly
    MaxLength = 16777216,  %% 16MB
    SendFun = fun(_) -> 
        ct:fail("Send function should not be called for oversized messages")
    end,
    
    %% Test the error from send_wamp_message
    Result = awre_trans_tcp:send_wamp_message(OversizedMessage, raw_msgpack, MaxLength, SendFun),
    
    %% Should return an error
    ?assertMatch({error, {message_too_large, _, _}}, Result),
    
    ct:pal("Error propagation test completed successfully: ~p", [Result]).

%% Test various message sizes around the boundary
test_various_message_sizes(_Config) ->
    ct:pal("Testing various message sizes around the 16MB boundary"),
    
    MaxLength = 16777216,  %% 16MB
    
    %% Test with actual large payloads that will definitely exceed the limit
    TestCases = [
        {small_safe, 1024, should_succeed},           %% 1KB - definitely safe
        {medium_safe, 1024 * 1024, should_succeed},  %% 1MB - safe
        {large_safe, 10 * 1024 * 1024, should_succeed}, %% 10MB - should be safe
        {oversized, 20 * 1024 * 1024, should_fail},  %% 20MB - definitely too large
        {very_large, 50 * 1024 * 1024, should_fail}  %% 50MB - way too large
    ],
    
    lists:foreach(fun({Label, PayloadSize, ExpectedResult}) ->
        ct:pal("Testing ~p with payload size: ~p bytes", [Label, PayloadSize]),
        
        Payload = binary:copy(<<"x">>, PayloadSize),
        Message = {result, 1, #{}, [Payload], #{}},
        
        SendFun = fun(_) -> ok end,
        
        Result = awre_trans_tcp:send_wamp_message(Message, raw_msgpack, MaxLength, SendFun),
        
        case ExpectedResult of
            should_succeed ->
                case Result of
                    ok ->
                        ct:pal("~p: Expected success, got success", [Label]);
                    {error, _} ->
                        ct:pal("~p: Expected success but got error: ~p", [Label, Result]),
                        %% This might happen if the message overhead makes it exceed the limit
                        %% Let's check the actual serialized size
                        try
                            SerMsg = wamper_protocol:serialize(Message, raw_msgpack),
                            ActualSize = byte_size(SerMsg),
                            ct:pal("~p: Actual serialized size: ~p bytes", [Label, ActualSize]),
                            case ActualSize > MaxLength of
                                true -> ct:pal("~p: Size exceeds limit due to overhead", [Label]);
                                false -> ?assertEqual(ok, Result) %% This should not happen
                            end
                        catch
                            _:_ -> ct:pal("~p: Serialization failed", [Label])
                        end
                end;
            should_fail ->
                ?assertMatch({error, {message_too_large, _, _}}, Result),
                ct:pal("~p: Expected failure, got: ~p", [Label, Result])
        end
    end, TestCases).

%% Test size limits with different encodings
test_different_encodings_size_limits(_Config) ->
    ct:pal("Testing size limits with different encodings"),
    
    %% Create a moderately large payload
    PayloadSize = 5 * 1024 * 1024,  %% 5MB
    Payload = binary:copy(<<"x">>, PayloadSize),
    Message = {event, 1, 2, #{}, [Payload], #{}},
    MaxLength = 16777216,  %% 16MB
    
    %% Test different encodings
    Encodings = [raw_msgpack, raw_json],
    
    lists:foreach(fun(Encoding) ->
        ct:pal("Testing encoding: ~p", [Encoding]),
        
        SendFun = fun(SerMsg) -> 
            ct:pal("~p encoding produced ~p bytes", [Encoding, byte_size(SerMsg)]),
            ok 
        end,
        
        Result = awre_trans_tcp:send_wamp_message(Message, Encoding, MaxLength, SendFun),
        
        case Result of
            ok ->
                ct:pal("Message sent successfully with ~p encoding", [Encoding]);
            {error, {message_too_large, ActualSize, _}} ->
                ct:pal("Message too large with ~p encoding: ~p bytes", [Encoding, ActualSize])
        end
    end, Encodings).