%%-------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : Utility functions for content filtering
%%%
%%% Created : 18 Aug 2010 
%%%-------------------------------------------------------------------

-module(content_utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([block/3, extract_urls/1, full_uri/1, unix_timestamp/1]).

%%
%% API Functions
%%
block(MessageBody, TextToBlock, BlockSymbol) when is_list(BlockSymbol) ->
	block(MessageBody, TextToBlock, hd(BlockSymbol));

block(MessageBody, TextToBlock, BlockSymbol) ->
	BlockStr = string:chars(BlockSymbol, string:len(TextToBlock)),
 re:replace(MessageBody, TextToBlock, BlockStr, [global, caseless, {return, list}]).


extract_urls(Msg) ->
	Words = string:tokens(Msg, " ,"),
	L = lists:foldl(fun(W, Acc) -> 
			W1 = cut_tags(W),
			case parse_uri(W1) of
															 {error, _Reason} -> Acc;												 
     											_Other -> [W1 | Acc]
														 end
							end, [], Words),
	lists:reverse(L).
%%
%% Local Functions
%%
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

unix_timestamp(Time) ->
      calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(Time) ) -
            calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).
  