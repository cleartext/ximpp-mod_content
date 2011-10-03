%%-------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : BrightCloud service support
%%%
%%% Created :  Oct 27, 2010 
%%%-------------------------------------------------------------------
-module(brightcloud_utils).

%%
%% Include files
%%
-define(GET_CATEGORIES_REQUEST(UID, ProductId, OemId), "<?BrightCloud version=bcap/1.1?>
<bcap>
<seqnum>" ++ exmpp_utils:random_id("brightcloud") ++ "</seqnum>
<encrypt-type>none</encrypt-type>
<request>
<method>getcatlist</method>
<uid>" ++ UID ++ "</uid>
<productid>" ++ ProductId ++ "</productid> 
<oemid>"++ OemId ++ "</oemid>
</request>
</bcap>").

-define(GET_URIINFO_REQUEST(URI, UID, ProductId, OemId), "<?BrightCloud version=bcap/1.1?>
<bcap>
<seqnum>" ++ exmpp_utils:random_id("brightcloud") ++ "</seqnum>
<encrypt-type>none</encrypt-type>
<request>
<method>geturiinfoex</method>
<uid>" ++ UID ++ "</uid>
<productid>" ++ ProductId ++ "</productid> 
<oemid>"++ OemId ++ "</oemid>
<uri>" ++ URI ++ "</uri>
</request>
</bcap>").

%% For testing
-define(TEST_BRIGHTCLOUD_SERVICE, "http://service2.brightcloud.com").
-define(TEST_UID, "cleartext_swa123").
-define(TEST_PRODUCTID, "DeviceId_cleartext").
-define(TEST_OEMID, "BrightCloudSdk").
%%
%% Exported Functions
%%
-export([get_scores/5, get_categories/4]).
-export([compile_rule/1, url_check/5]).
-export([test/0]).

%%
%% API Functions
%%

%% the rule has the form:
%% categories:code1,code2, ..codeN;reputation:n
%% Compilation creates a function that returns true if the score (categories + reputation)
%% passes, otherwise false.
compile_rule(Rule) ->
  Fields = string:tokens(Rule, ";"),
  KeyVals = lists:map(fun(F) ->
                            [K, V] = string:tokens(F, ":"),
                             {K,V}
                      end, Fields),
  CatString = proplists:get_value("categories", KeyVals, none),
  RepIndex = proplists:get_value("reputation", KeyVals, none),
  CatFunc = case CatString of
              none ->
                fun(_Categories) ->
                     true
                end;
														_ ->
																CatCodes = string:tokens(CatString, ","),
																fun(Categories) ->
																		 lists:all(fun(C) ->
                              not lists:member(C, CatCodes)
                                end,   
                                 Categories)
                end
            end,   
  RepFunc = case RepIndex of
              none ->
                fun(_Rep) ->
                     true
                end;
              _ ->
                fun(Reputation) ->
                     RepIndex =< Reputation
                end
            end,
  fun(Categories, Reputation) ->
        CatFunc(Categories) andalso RepFunc(Reputation)
  end.              
  
get_categories(ServiceUrl, UID, ProductId, OemId) ->
	inets:start(),
	crypto:start(),
 MIME = "application/x-www-form-urlencoded",
 {ok, R} = http:request(post, {ServiceUrl, [], MIME, ?GET_CATEGORIES_REQUEST(UID, ProductId, OemId)}, [], []),
 parse_response(R, get_categories).

get_scores(URI, ServiceUrl, UID, ProductId, OemId) ->
	inets:start(),
	crypto:start(),	
 MIME = "application/x-www-form-urlencoded",

 {ok, R} = http:request(post, {ServiceUrl, [], MIME, ?GET_URIINFO_REQUEST(content_utils:full_uri(URI), UID, ProductId, OemId)}, [], []),
	{URI, parse_response(R, get_scores)}.


%%
%% Local Functions
%%

parse_response(Response, RequestType) ->
	{StatusLine, _Headers, Body} = Response,
	%% Check if status code is 200
	{_HttpVersion, StatusCode, Reason} = StatusLine,
	case StatusCode of
		200 -> [XML] = exmpp_xml:parse_document(Body),
         process_xml(XML, RequestType);
   
		_Other -> {error, {StatusCode, Reason}}
	end.

process_xml(XML, get_scores) ->
  io:format("~p~n", [XML]),
  Scores = exmpp_xml:get_elements(
  exmpp_xml:get_element(
    exmpp_xml:get_element(XML, "response"),
    "categories"),
  "cat"),
  Reputation =  exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(
    exmpp_xml:get_element(XML, "response"),
    "bcri")),
  Categories = lists:map(fun(C) ->
                 exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(C, "catid"))                
            end, Scores),
  {Reputation, Categories};

process_xml(XML, get_categories) ->
  Categories = exmpp_xml:get_elements(
  exmpp_xml:get_element(
    exmpp_xml:get_element(XML, "response"),
    "categories"),
  "cat"),
  lists:map(fun(C) ->
                 CatId = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(C, "catid")),
                 CatName = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(C, "catname")),
                 CatGroup = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(C, "catgroup")),
                 {CatId, [{catname, CatName}, {catgroup, CatGroup}]}
    								end, Categories);

process_xml(_XML, RequestType) ->
  {error, {unknown_request_type, RequestType}}.


test() ->
  exmpp:start(),
	Yahoo = "http://google.com",
	Ibm = "http://www.ibm.com",
	YouTube ="http://www.youtube.com",
	URLs = content_utils:extract_urls(lists:flatten(io_lib:format("Hello, check this out: ~s, also ~s and ~s . Enjoy!", [Yahoo, Ibm, YouTube]))),
	lists:map(fun(URI) ->
                     get_scores(URI, ?TEST_BRIGHTCLOUD_SERVICE, ?TEST_UID, ?TEST_PRODUCTID, ?TEST_OEMID)
               end, URLs).

url_check(Msg, Rule, Action, _Direction, Host) ->  
  URLs = content_utils:extract_urls(Msg),
  case URLs of
    [] -> {keep, Msg};
    _ ->
      Predicate = brightcloud_utils:compile_rule(Rule),						  
      PassFunc = fun(URL) -> 
                        {URL, Scores} = mod_brightcloud:get_scores(Host, URL), 
                        {Reputation, Categories} = Scores,
                        Predicate(Categories, Reputation)
                   end,
      case Action of 
        "drop" -> 
          case lists:any(fun(S) -> not PassFunc(S) end, URLs) of
            true -> drop;
            false -> {keep, Msg}
          end;	
        "block" -> 	
          NewMsg = lists:foldl(fun(U, M) -> 
                                    case not PassFunc(U) of
                                      true ->
                                        content_utils:block(M, U, "*");
                                      false ->
                                        M
                                    end
                               end, Msg, URLs),
          {keep, NewMsg}
      end
  end.

