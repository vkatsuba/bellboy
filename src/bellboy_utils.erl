-module(bellboy_utils).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([
  get_code/1,
  get_body/1,
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
%% HTTPC request with/without headers
%% @end
%% -------------------------------------------------------------------
-spec httpc_request(Params :: maps:map()) -> {ok, Result :: tuple()} | {error, Reason :: tuple()}.

httpc_request(#{m := M, u := URL, h := H, b := B, ct := CT}) ->
  httpc:request(M, {URL, maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], H), CT, B}, [], []);

httpc_request(#{m := M, u := URL, h := H}) ->
  httpc:request(M, {URL, maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], H)}, [], []).
