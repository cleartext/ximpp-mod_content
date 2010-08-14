%%-------------------------------------------------------------------
%%% File    : http_auth.erl
%%% Author  : Boris Okner <b.okner@rogers.com>
%%% Description : Basic authentication
%%%
%%% Created : 14 Jun 2009 by Boris Okner <b.okner@rogers.com>
%%%-------------------------------------------------------------------
-module(http_auth).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([basic_auth/5]).
-export([test/0]).

%%
%% API Functions
%%
basic_auth(URL, Method, User, Password, Data) ->
			AuthorizationHeader = buildAuthHeader(User, Password),
			http:request(Method, {URL, [{"Authorization", AuthorizationHeader}], "application/x-www-form-urlencoded",
																							Data}, [], []).
	

%%
%% Local Functions
%%
test() ->
	inets:start(),
	crypto:start(),	
	http:set_options([{verbose, verbose}]),
	Data = "{\"type\":3,\"ver\":1,\"urls\":[{\"id\":1,\"url\":\"http://yahoo.com\"}, {\"id\":2,\"url\":\"http://www.ibm.com\"}, {\"id\":3,\"url\":\"http://www.youtube.com\"}]}",
	basic_auth("http://72.5.172.35:3128/wwss_url_checker", post, 
						 "test@tagged.com", "wrtest", Data).

buildAuthHeader(User, Password) ->
	UserPasswd = base64:encode_to_string(User ++ ":" ++ Password),
	"Basic " ++ UserPasswd.
