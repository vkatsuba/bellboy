# bellboy
Bellboy - is Erlang HTTP client library for send SMS by different services: Plivo, Twilio, Nexmo

## Contents
* [Goals](#goals)
* [Build & Run](#build--run)
* [Clean Project](#clean-project)
* [Install bellboy to project](#install-bellboy-to-project-rebar3)
* [Erlang Plivo](#erlang-plivo)
  * [Send SMS By Erlang Plivo](#send-sms-by-erlang-plivo)
* [Twilio](#twilio)
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
  type => send_message,            % atom   - bellboy type for send SMS
  auth_id => "PlivoAuthID",        % list   - plivo auth ID
  auth_token => "PlivoAuthToken",  % list   - plivo auth token
  src => <<"00000000000000000">>,  % binary - plivo phone number
  dst => <<"11111111111111111">>,  % binary - user phone number
  text => <<"Plivo SMS Text">>     % binary - SMS text
},

% Code     - integer()
% Body     - map() | list()
% Response - list()
{ok, #{code := Code, body := Body, response := FullResp}} = bellboy:plivo(ReqMap).
```

# Twilio
...

# Nexmo
...

# Support
v.katsuba.dev@gmail.com
