%%-------------------------------------------------------------------
%%% File    : webroot_utils.erl
%%% Author  : Boris Okner <b.okner@rogers.com>
%%% Description : WebRoot service support
%%%
%%% Created : 14 Aug 2010 by Boris Okner <b.okner@rogers.com>
%%%-------------------------------------------------------------------
-module(webroot_utils).

%%
%% Include files
%%
-define(WEBROOT_SERVICE, "http://72.5.172.35:3128/wwss_url_checker").
-define(WEBROOT_USERNAME, "test@tagged.com").
-define(WEBROOT_PASSWORD, "wrtest").
%%
%% Exported Functions
%%
-export([extract_urls/1, get_scores/4]).
-export([test/0]).

%%
%% API Functions
%%
extract_urls(Msg) ->
	Words = string:tokens(Msg, " ,"),
	L = lists:foldl(fun(W, Acc) -> case http_uri:parse(W) of
															 {error, _Reason} -> Acc;
															 _Other -> [W | Acc]
														 end
							end, [], Words),
	lists:reverse(L).

get_scores(URLs, WebRootService, Username, Password) ->
	inets:start(),
	crypto:start(),	
	%%httpc:set_option(verbose, trace),
	%% Format data for POST
	{URLStrs, _N} = lists:foldl(fun(U, {L, N}) -> F = lists:flatten(io_lib:format("{\"id\":~p,\"url\":\"~s\"}", [N, U])), {[F | L], N + 1} end, {[], 1}, URLs),
	Str = string:join(lists:reverse(URLStrs), ","),
	Data = lists:flatten(io_lib:format("{\"type\":3,\"ver\":1,\"urls\":[~s]}", [Str])),
	%% Call webroot
	AuthorizationHeader = buildAuthHeader(Username, Password),
	{ok, Response} = http:request(post, {WebRootService, [{"Authorization", AuthorizationHeader}], "application/x-www-form-urlencoded",
																							Data}, [], []),	
	{ok, URLInfoList} = parse_response(Response),
	lists:map(fun(URLInfo) -> proplists:get_value("reputation", URLInfo) end, URLInfoList).


%%
%% Local Functions
%%
buildAuthHeader(User, Password) ->
	UserPasswd = base64:encode_to_string(User ++ ":" ++ Password),
	"Basic " ++ UserPasswd.

parse_response(Response) ->
	{StatusLine, _Headers, Body} = Response,
	%% Check if status code is 200
	{_HttpVersion, StatusCode, Reason} = StatusLine,
	case StatusCode of
		200 -> {ok, parse_json(Body)};
		_Other -> {error, {StatusCode, Reason}}
	end.

%% Returns list of URL info elements.
%% Each info element structured as [{"id", Id}, {"reputation", R}, {"url_category", C}]
parse_json(Json) ->
	Struct = json_utils:decode(Json),
	{struct, Elements} = Struct,
	{array, URLElems} = proplists:get_value("urls", Elements),
	lists:map(fun(E) -> {struct, Fields} = E, Fields end, URLElems).

test() ->
	Yahoo = "http://yahoo.com",
	Ibm = "http://www.ibm.com",
	YouTube ="http://www.youtube.com",
	URLs = extract_urls(lists:flatten(io_lib:format("Hello, check this out: ~s, also ~s and ~s. Enjoy!", [Yahoo, Ibm, YouTube]))),
	get_scores(URLs, ?WEBROOT_SERVICE, ?WEBROOT_USERNAME, ?WEBROOT_USERNAME).