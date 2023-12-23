%% MIT License

%% Copyright (c) 2021 Viacheslav Katsuba <v.katsuba.dev@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(bellboy_http).

-export([
    get_code/1,
    get_body/1,
    gen_body/1,
    httpc_request/1,
    basic_auth/2
]).

%% @doc
%% Returns HTTP response code
%% @end
-spec get_code(HttpcResult :: tuple()) -> Code :: pos_integer().

get_code({{_, Code, _}, _, _}) -> Code.

%% @doc
%% Returns HTTP response body
%% @end
-spec get_body(HttpcResult :: tuple()) -> Body :: list().

get_body({_, _, Body}) -> Body.

%% @doc
%% Helper for generate body: if data has JSON format - decode to map, if not - return data as result
%% @end
-spec gen_body(Data :: list() | binary()) -> Result :: map() | any().

gen_body(Data) ->
    case catch jsone:decode(Data, [{object_format, map}]) of
        Map when is_map(Map) -> Map;
        _ -> Data
    end.

%% @doc
%% HTTPC request with/without headers
%% @end
-spec httpc_request(Params :: map()) ->
    {ok, Result :: tuple()} |
    {error, Reason :: tuple()}.

httpc_request(#{m := M, u := URL, h := H, b := B, ct := CT}) ->
    httpc:request(M, {URL, maps:to_list(H), CT, B}, [], []);
httpc_request(#{m := M, u := URL, b := B, ct := CT}) ->
    httpc:request(M, {URL, [], CT, B}, [], []);
httpc_request(#{m := M, u := URL, h := H}) ->
    httpc:request(M, {URL, maps:to_list(H)}, [], []);
httpc_request(#{m := M, u := URL}) ->
    httpc:request(M, {URL, []}, [], []).

basic_auth(AuthID, AuthToken) ->
    "Basic " ++ binary_to_list(base64:encode(AuthID ++ ":" ++ AuthToken)).
