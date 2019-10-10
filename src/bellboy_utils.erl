-module(bellboy_utils).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([
  get_code/1,
  get_body/1,
  gen_body/1,
  httpc_request/1
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("bellboy.hrl").

%%% ==================================================================
%%% Public functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response code
%% @end
%% -------------------------------------------------------------------
-spec get_code(HttpcResult :: tuple()) -> Code :: pos_integer().

get_code({{_, Code, _}, _, _}) -> Code.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response body
%% @end
%% -------------------------------------------------------------------
-spec get_body(HttpcResult :: tuple()) -> Body :: list().

get_body({_, _, Body}) -> Body.

%% -------------------------------------------------------------------
%% @doc
%% Helper for generate body: if data has JSON format - decode to map, if not - return data as result
%% @end
%% -------------------------------------------------------------------
-spec gen_body(Data :: lists:list() | binary()) -> Result :: maps:map() | any().

gen_body(Data) ->
  case catch jiffy:decode(Data, [return_maps]) of
    Map when is_map(Map) -> Map;
    _ -> Data
  end.

%% -------------------------------------------------------------------
%% @doc
%% HTTPC request with/without headers
%% @end
%% -------------------------------------------------------------------
-spec httpc_request(Params :: maps:map()) -> {ok, Result :: tuple()} | {error, Reason :: tuple()}.

httpc_request(#{m := M, u := URL, h := H, b := B, ct := CT}) ->
  httpc:request(M, {URL, maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], H), CT, B}, [], []);

httpc_request(#{m := M, u := URL, h := H}) ->
  httpc:request(M, {URL, maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], H)}, [], []).
