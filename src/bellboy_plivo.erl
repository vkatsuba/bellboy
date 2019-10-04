-module(bellboy_plivo).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([message/1]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("bellboy.hrl").

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Plivo handler
%% @end
%% -------------------------------------------------------------------

message(#{type := send_message} = Data) -> send_message(Data);
message(_) -> ?BAD_ARG.

%%% ==================================================================
%%% Internal/Private functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Send simple sms message
%% @end
%% -------------------------------------------------------------------
-spec send_message(Params :: maps:map()) -> {ok, Result :: maps:map()} | {error, Reason :: tuple()} | tuple().

send_message(#{auth_id := AID, auth_token := AT, payload := P}) when is_list(AID), is_list(AT), is_map(P) ->
  RD = #{m => post, u => ?PLIVO_URL(AID), h => #{"Authorization" => ?BASIC_AUTH(AID, AT)}, b => jiffy:encode(P), ct => "application/json"},
  case bellboy_utils:httpc_request(RD) of
    {ok, Resp} -> #{code => bellboy_utils:get_code(Resp), body => gen_resp_body(bellboy_utils:get_body(Resp)), response => Resp};
    Error -> Error
  end;
send_message(_) -> ?BAD_ARG.

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Helper for generate body: if data has JSON format - decode to map, if not - return data as result
%% @end
%% -------------------------------------------------------------------
-spec gen_resp_body(Data :: lists:list() | binary()) -> Result :: maps:map() | any().

gen_resp_body(Data) ->
  case catch jiffy:decode(Data, [return_maps]) of
    Map when is_map(Map) -> Map;
    _ -> Data
  end.
