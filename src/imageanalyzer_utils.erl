%%-------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : ImageAnalyzer service support
%%%
%%% Created :  March 03, 2011 
%%%-------------------------------------------------------------------
-module(imageanalyzer_utils).

%%
%% Include files
%%
-define(SOAP_ENVELOPE(ImageUrl, Sensitivity, Token), "<?xml version='1.0' encoding='utf-8'?>
<soap:Envelope xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns:xsd='http://www.w3.org/2001/XMLSchema' 
	xmlns:soap='http://schemas.xmlsoap.org/soap/envelope/'>
  <soap:Body>
    <ScanImageURL xmlns='http://test.image-analyzer-dev.com/'>
      <strImageUrl>" ++ ImageUrl ++ "</strImageUrl>
      <nEngineSensitivity>"++ Sensitivity ++ "</nEngineSensitivity>
      <strTocken>" ++ Token ++ "</strTocken>
    </ScanImageURL>
  </soap:Body>
</soap:Envelope>").

-define(SERVICE_URL, "http://test.image-analyzer-dev.com/scan.asmx").
-define(SOAP_ACTION, "http://test.image-analyzer-dev.com/ScanImageURL").

-define(LOW_FALSE_POSITIVE, "65").
-define(LOW_FALSE_NEGATIVE, "75").
-define(MAX_SAFE_SCORE, 75).

-define(TOKEN, "cleartext").
%% For testing
%%
%% Exported Functions
%%
-export([compile_rule/1, image_check/5]).
-export([test/0]).

%% For development only
-compile(export_all).

%%
%% API Functions
%%


%% the rule has the form:
%% filter:number_or_default; sensitivity:number_or_default
%% filter can take either number 0 to 100, "safe", or "all"
%% "safe" means "allow only scores 0-75", "all" means allow all scores
%% sensitivity can take either number 0 to 100, low_false_positive (=65) or low_false_negatives (=75)
compile_rule(Rule) ->
  Fields = string:tokens(Rule, ";"),
  KeyVals = lists:map(fun(F) ->
                              [K, V] = string:tokens(F, ":"),
			                                   {K,V}
							                         end, Fields),  
										 		Filter = proplists:get_value("filter", KeyVals, "safe"),
												  Sensitivity = proplists:get_value("sensitivity", KeyVals, ?LOW_FALSE_POSITIVE),

  {make_score_fun(Filter), calc_sensitivity(Sensitivity)}.


make_score_fun("all") ->
  fun(Score) ->
       true
  end;

make_score_fun("safe") ->
  fun(Score) ->
       Score =< ?MAX_SAFE_SCORE
		end;

make_score_fun(AnotherNumber) ->
  fun(Score) ->
       Score =< list_to_integer(AnotherNumber)
		end.

calc_sensitivity("low_false_negative") ->
  ?LOW_FALSE_NEGATIVE;

calc_sensitivity("low_false_positive") ->
  ?LOW_FALSE_POSITIVE;

calc_sensitivity(Sensitivity) ->
  Sensitivity.

  
              
  
get_score(ImageUrl, ServiceUrl, Sensitivity, Token) ->
	inets:start(),
	crypto:start(),	
      {ok, {{_, ResponseCode, _}, 
            _Fields, 
            ResponseBody}} = http:request(post, {?SERVICE_URL, 
                                                  [{"SOAPAction", ?SOAP_ACTION}], "text/xml; charset=utf-8",
                                                  ?SOAP_ENVELOPE(ImageUrl, Sensitivity, Token)}, [], []),
      [R] = exmpp_xml:parse_document(ResponseBody),
      ResponseXml = exmpp_xml:get_path(R, 
                                       [{element, "Body"}, 
                                        {element, "ScanImageURLResponse"}, 
                                        {element, "ScanImageURLResult"} 
                                        ]),
 
	{ImageUrl, list_to_integer(exmpp_xml:get_cdata_as_list(ResponseXml))}.


%%
%% Local Functions
%%



test() ->
  exmpp:start(),
	Yahoo = "http://google.com",
	Ibm = "http://www.ibm.com",
	YouTube ="http://www.youtube.com",
 PornPhoto = "http://p2.xhamster.com/000/005/538/648_160.jpg",
	URLs = content_utils:extract_urls(lists:flatten(io_lib:format("Hello, check this out: ~s, also ~s and ~s . Enjoy!", [Yahoo, Ibm, PornPhoto]))),
	lists:map(fun(URI) ->
                     get_score(URI, ?SERVICE_URL, ?LOW_FALSE_POSITIVE, ?TOKEN)
               end, URLs).

image_check(Msg, Rule, Action, _Direction, _Host) ->  
  URLs = content_utils:extract_urls(Msg),
  case URLs of
    [] -> {keep, Msg};
    _ ->
      {Predicate, Sensitivity} = imageanalyzer_utils:compile_rule(Rule),						  
      PassFunc = fun(URL) -> 
                        {_Url, Score} = imageanalyzer_utils:get_score(URL, ?SERVICE_URL, Sensitivity, ?TOKEN), 
                        Predicate(Score)
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

parse_uri(Word) ->
  W = case string:str(Word, "www.") of
    1 ->
      "http://" ++ Word;
    _ ->
      Word
      end,
    http_uri:parse(W).

cut_tags(W) ->
    %% Cut out closing tag
    case string:str(W, "</") of
	      0 ->
	              W;
		            P ->
			            string:sub_string(W, 1, P - 1)
				            end.



full_uri(URI) ->
	case string:str(URI, "http://") of
             1 ->
               URI;
             _ ->
               "http://" ++ URI
           end.
