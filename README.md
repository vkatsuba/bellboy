# bellboy
Bellboy - is Erlang HTTP client library for send SMS by different services: Plivo, Twilio, Nexmo

## Contents
* [Goals](#goals)
* [Build & Run](#build--run)
* [Clean Project](#clean-project)
* [Install bellboy to project](#install-bellboy-to-project-rebar3)
* [Erlang Plivo](#erlang-plivo)
  * [Send SMS By Erlang Plivo](#send-sms-by-erlang-plivo)
  * [Get Details of a Single Message By Erlang Plivo](#get-details-of-a-single-message-by-erlang-plivo)
  * [Get Details of all Messages By Erlang Plivo](#get-details-of-all-messages-by-erlang-plivo)
* [Erlang Twilio](#erlang-twilio)
  * [Send SMS By Erlang Twilio](#send-sms-by-erlang-twilio)
  * [Fetch a Message Resource By Erlang Twilio](#fetch-a-message-resource-by-erlang-twilio)
* [Nexmo](#nexmo)
* [Support](#support)

# Goals
...

# Build & Run
```sh
$ git clone https://github.com/vkatsuba/bellboy.git
$ cd bellboy
$ make
```
# Clean Project
```sh
$ make clean
```
# Install bellboy to project: [Rebar3](https://www.rebar3.org/)
* Edit file **rebar.config**:
```
...
{deps, [
  ...
  {bellboy, {git, "git://github.com/vkatsuba/bellboy.git", {branch, "master"}}},
  ...
]}.
...
```
* Edit file ***.app.src**:
```
...
  {applications,
   [
    ...,
    bellboy,
    ...
   ]},
...
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
  to          => "11111111111111111"   % list() - user phone number
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:twilio(ReqMap).
```

# Fetch a Message Resource By Erlang Twilio
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

# Nexmo


# Support
v.katsuba.dev@gmail.com
