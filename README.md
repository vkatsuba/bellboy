# bellboy
Bellboy - is Erlang `HTTP` client library for send **[SMS](https://en.wikipedia.org/wiki/SMS)** by different services: **[Plivo](https://www.plivo.com)**, **[Twilio](https://www.twilio.com)**, **[Nexmo](https://developer.nexmo.com)**.

[![Hex.pm Version](https://img.shields.io/hexpm/v/bellboy.svg)](https://hex.pm/packages/bellboy)

[![Build Status](https://github.com/vkatsuba/bellboy/workflows/CI/badge.svg)](https://github.com/vkatsuba/bellboy/actions)

## Contents
* [Goals](#goals)
* [Build & Run](#build--run)
* [Dialyzer](#dialyzer)
* [Clean Project](#clean-project)
* [Install bellboy to project](#install-bellboy-to-project-rebar3)
* [Erlang Plivo](#erlang-plivo)
  * [Send SMS By Erlang Plivo](#send-sms-by-erlang-plivo)
  * [Get Details of a Single Message By Erlang Plivo](#get-details-of-a-single-message-by-erlang-plivo)
  * [Get Details of all Messages By Erlang Plivo](#get-details-of-all-messages-by-erlang-plivo)
* [Erlang Twilio](#erlang-twilio)
  * [Send SMS By Erlang Twilio](#send-sms-by-erlang-twilio)
  * [Fetch a Message Resource By Erlang Twilio](#fetch-a-message-resource-by-erlang-twilio)
  * [Read Multiple Message Resources By Erlang Twilio](#read-multiple-message-resources-by-erlang-twilio)
* [Erlang Nexmo](#erlang-nexmo)
  * [Send SMS By Erlang Nexmo](#send-sms-by-erlang-nexmo)
  * [Send PIN By Erlang Nexmo](#send-pin-by-erlang-nexmo)
  * [Check PIN By Erlang Nexmo](#check-pin-by-erlang-nexmo)
  * [Cancel PIN By Erlang Nexmo](#cancel-pin-by-erlang-nexmo)
* [Support](#support)

# Goals
Bellboy aims to provide a simple way for send **[SMS](https://en.wikipedia.org/wiki/SMS)** by different services by REST API.

# Build & Run
```sh
$ git clone https://github.com/vkatsuba/bellboy.git
$ cd bellboy
$ wget https://s3.amazonaws.com/rebar3/rebar3
$ chmod u+x ./rebar3
$ ./rebar3 shell
```
# Dialyzer
```sh
$ ./rebar3 dialyzer
```
# Clean Project
```sh
$ ./rebar3 clean
```
# Install bellboy to project: [Rebar3](https://www.rebar3.org/)
* Edit file **rebar.config**:
```erlang
{deps, [
    {bellboy, "2.0.0"},
]}.
```

# Erlang Plivo
## Send SMS By Erlang Plivo
```erlang
% https://docs.labs.plivo.com/latest/python/elements/message/send-an-sms
ReqMap = #{
   type => send_message,            % atom()   - bellboy type for send SMS
   auth_id => "PlivoAuthID",        % list()   - Plivo auth ID
   auth_token => "PlivoAuthToken",  % list()   - Plivo auth token
   src => <<"00000000000000000">>,  % binary() - Plivo phone number
   dst => <<"11111111111111111">>,  % binary() - user phone number
   text => <<"Plivo SMS Text">>     % binary() - SMS text
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:plivo(ReqMap).
```

## Get Details of a Single Message By Erlang Plivo
```erlang
% https://www.plivo.com/docs/sms/getting-started/advanced/sms-details-single-message
ReqMap = #{
   type => get_message,             % atom() - bellboy type for send SMS
   auth_id => "PlivoAuthID",        % list() - Plivo auth ID
   auth_token => "PlivoAuthToken",  % list() - Plivo auth token
   message_uuid => "PlivoMsgUUID"   % list() - Plivo message UUID
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:plivo(ReqMap).
```

## Get Details of all Messages By Erlang Plivo
```erlang
% https://www.plivo.com/docs/sms/getting-started/advanced/sms-details-all-messages/
ReqMap = #{
   type => get_messages,            % atom() - bellboy type for send SMS
   auth_id => "PlivoAuthID",        % list() - Plivo auth ID
   auth_token => "PlivoAuthToken"   % list() - Plivo auth token
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:plivo(ReqMap).
```

# Erlang Twilio
## Send SMS By Erlang Twilio
```erlang
% https://www.twilio.com/docs/sms/send-messages
ReqMap = #{
   type        => send_message,         % atom() - bellboy type for send SMS
   account_sid => "TwilioAccountSID",   % list() - Twilio account SID
   auth_token  => "TwilioAuthToken",    % list() - Twilio auth token
   body        => "Twilio SMS Text",    % list() - SMS text
   from        => "00000000000000000",  % list() - Twilio phone number
   to          => "11111111111111111"   % list() - User phone number
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:twilio(ReqMap).
```

## Fetch a Message Resource By Erlang Twilio
```erlang
% https://www.twilio.com/docs/sms/api/message-resource#fetch-a-message-resource
ReqMap = #{
   type        => get_message,          % atom() - bellboy type for send SMS
   account_sid => "TwilioAccountSID",   % list() - Twilio account SID
   auth_token  => "TwilioAuthToken",    % list() - Twilio auth token
   sid         => "MsgSid"              % list() - Twilio SID of SMS message
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:twilio(ReqMap).
```

## Read Multiple Message Resources By Erlang Twilio
```erlang
% https://www.twilio.com/docs/sms/api/message-resource#read-multiple-message-resources
ReqMap = #{
   type        => get_messages,         % atom() - bellboy type for send SMS
   account_sid => "TwilioAccountSID",   % list() - Twilio account SID
   auth_token  => "TwilioAuthToken"     % list() - Twilio auth token
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:twilio(ReqMap).
```

# Erlang Nexmo
## Send SMS By Erlang Nexmo
```erlang
% https://developer.nexmo.com/api/sms
ReqMap = #{
   type        => send_sms,                 % atom()   - bellboy type for send SMS
   from        => <<"00000000000000000">>,  % binary() - Nexmo name or number
   to          => <<"11111111111111111">>,  % binary() - User phone number
   text        => <<"Nexmo SMS text">>,     % binary() - SMS text
   api_key     => <<"ApiKey">>,             % binary() - Nexmo API key
   api_secret  => <<"ApiSecret">>           % binary() - Nexmo API secret
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:nexmo(ReqMap).
```
## Send PIN By Erlang Nexmo
```erlang
% https://developer.nexmo.com/verify/overview 
ReqMap = #{
   type        => send_pin,             % atom() - bellboy type for send SMS
   brand       => "Brand",              % list() - Nexmo brand
   number      => "11111111111111111",  % list() - User phone number
   code_length => "4",                  % list() - Length of PIN
   api_key     => "ApiKey",             % list() - Nexmo API key
   api_secret  => "ApiSecret"           % list() - Nexmo API secret
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:nexmo(ReqMap).
```

## Check PIN By Erlang Nexmo
```erlang
% https://developer.nexmo.com/verify/overview 
ReqMap = #{
   type        => check_pin,   % atom() - bellboy type for send SMS
   request_id  => "ReqID",     % list() - Nexmo `request_id` field from `send_pin` response
   code        => "1111",      % list() - Nexmo PIN code 
   api_key     => "ApiKey",    % list() - Nexmo API key
   api_secret  => "ApiSecret"  % list() - Nexmo API secret
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:nexmo(ReqMap).
```

## Cancel PIN By Erlang Nexmo
```erlang
% https://developer.nexmo.com/verify/overview 
ReqMap = #{
   type        => cancel_pin,  % atom() - bellboy type for send SMS
   request_id  => "ReqID",     % list() - Nexmo `request_id` field from `send_pin` response
   api_key     => "ApiKey",    % list() - Nexmo API key
   api_secret  => "ApiSecret"  % list() - Nexmo API secret
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:nexmo(ReqMap).
```

# Support
v.katsuba.dev@gmail.com
