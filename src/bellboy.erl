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

-module(bellboy).

-export([
    plivo/1,
    twilio/1,
    nexmo/1
]).

%% @doc
%% Request to Plivo
%% @end
-spec plivo(Data :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

plivo(Data) -> bellboy_plivo:message(Data).

%% @doc
%% Request to Twilio
%% @end
-spec twilio(Data :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

twilio(Data) -> bellboy_twilio:message(Data).

%% @doc
%% Request to Nexmo
%% @end
-spec nexmo(Data :: maps:map()) ->
    {ok, Result :: maps:map()} |
    {error, Reason :: tuple() | bad_arg}.

nexmo(Data) -> bellboy_nexmo:message(Data).
