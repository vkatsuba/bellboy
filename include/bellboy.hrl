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
-define(PLIVO_URL(AuthID), "https://api.plivo.com/v1/Account/" ++ AuthID ++ "/Message/").
-define(BASIC_AUTH(AuthID, AuthToken), "Basic " ++ binary_to_list(base64:encode(AuthID ++ ":" ++ AuthToken))).
-endif.
