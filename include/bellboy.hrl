-ifndef(BELLBOY_HRL).
-define(BELLBOY_HRL, 1).

%%% ==================================================================
%%% Bellboy
%%% ==================================================================

-define(NO_REDIRECT, {autoredirect, false}).
-define(CIPHERS, [[{ciphers, [{rsa, aes_128_cbc, sha}]}]]).
-define(SSLC, {ssl, ?CIPHERS}).
-define(BAD_ARG, {error, bad_arg}).
-define(GEN_UUID_V4, list_to_binary(uuid:uuid_to_string(uuid:get_v4()))).
-define(PLIVO_URL_MSG(AuthID), "https://api.plivo.com/v1/Account/" ++ AuthID ++ "/Message/").
-define(TWILIO_URL_MSG(AuthID), "https://api.twilio.com/2010-04-01/Accounts/" ++ AuthID ++ "/Messages.json").
-define(TWILIO_URL_SPEC_MSG(AuthID, Sid), "https://api.twilio.com/2010-04-01/Accounts/" ++ AuthID ++ "/Messages/" ++ Sid ++ ".json").
-define(BASIC_AUTH(AuthID, AuthToken), "Basic " ++ binary_to_list(base64:encode(AuthID ++ ":" ++ AuthToken))).
-define(NEXMO_URL_MSG, "https://rest.nexmo.com/sms/json").
-define(NEXMO_URL_VERIFY, "https://api.nexmo.com/verify/json").
-define(NEXMO_URL_CONTROL, "https://api.nexmo.com/verify/control/json").
-define(NEXMO_URL_CHECK, "https://api.nexmo.com/verify/check/json").
-endif.
