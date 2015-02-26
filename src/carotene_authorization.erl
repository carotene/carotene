-module(carotene_authorization).

-export([check_authorization/3]).

-include_lib("eunit/include/eunit.hrl").

check_authorization(UserId, Channel, Config) ->
    case lists:keyfind(level, 1, Config) of
        {level, anonymous} -> true;
        {level, authenticated} -> check_is_authenticated(UserId);
        {level, ask} -> case check_is_authenticated(UserId) of
                            true -> ask_authentication(UserId, Channel, Config);
                            Error -> Error
                        end;
        _ -> bad_value_for_level_in_configuration
    end.

check_is_authenticated(UserId) when UserId =:= anonymous -> needs_authentication;
check_is_authenticated(_) -> true.


ask_authentication(UserId, Channel, AuthConfig) ->
    case lists:keyfind(authorization_url, 1, AuthConfig) of
        false -> bad_value_for_authorization_url_in_configuration;
        {authorization_url, AuthorizeUrl} ->
            {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(post, {AuthorizeUrl, [], "application/x-www-form-urlencoded", "user_id="++binary_to_list(UserId)++"&channel="++binary_to_list(Channel)}, [], []),
            try jsx:decode(binary:list_to_bin(Body)) of
                Response -> case Response of
                                [{<<"authorized">>, <<"true">>}] -> true;
                                [{<<"authorized">>, <<"false">>}] -> no_authorization;
                                _ -> 
                                    bad_response_from_server_on_authorization
                            end
            catch _:_ ->
                      malformed_json_from_server_on_authorization
            end
    end.
