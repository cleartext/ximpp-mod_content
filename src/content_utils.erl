%%-------------------------------------------------------------------
%%% File    : content_utils.erl
%%% Author  : Boris Okner <b.okner@rogers.com>
%%% Description : Utility functions for content filtering
%%% Created : 18 Aug 2010 by Boris Okner <b.okner@rogers.com>
%%%-------------------------------------------------------------------

-module(content_utils).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([block/3]).

%%
%% API Functions
%%
block(MessageBody, TextToBlock, BlockSymbol) when is_list(BlockSymbol) ->
	block(MessageBody, TextToBlock, hd(BlockSymbol));

block(MessageBody, TextToBlock, BlockSymbol) ->
	BlockStr = string:chars(BlockSymbol, string:len(TextToBlock)),
	{ok, NewBody, _N} = regexp:gsub(MessageBody, TextToBlock, BlockStr),
	NewBody.
%%
%% Local Functions
%%
filters() ->
	[{"contains", fun(Msg, Phrase, "drop", _Host) ->
										 case string:str(Msg, Phrase) of
											 0 -> 
												 {false, Msg};
											 _ ->
												 true
										 end;			
									(Msg, Phrase, "block", _Host) ->                 	
										{false, content_utils:block(Msg, Phrase, "*")}  	
		end
	 },
	 
	 {"equals", fun(Msg, Phrase, "drop", _Host) ->
									 Msg =:= Phrase;
								(Msg, Phrase, "block", _Host) ->                 	
									{false, content_utils:block(Msg, Phrase, "*")}                	
		end
	 },
	 
	 {"contains_all", fun(Msg, Words, "drop", _Host) ->
												 WordList = string:tokens(Words, ";"),
												 lists:all(fun(W) -> string:str(Msg, W) > 0 end, WordList);
											(Msg, Words, "block", _Host) ->
												WordList = string:tokens(Words, ";"),
												NewMsg = lists:foldl(fun(W) -> content_utils:block(Msg, W, "*") end, Msg, WordList),
												{false, NewMsg}                	                  
		end
	 },
	 
	 {"contains_any", fun(Msg, Words, "drop", _Host) ->
												 WordList = string:tokens(Words, ";"),
												 lists:any(fun(W) -> string:str(Msg, W) > 0 end, WordList);
											(Msg, Words, "block", _Host) ->
												WordList = string:tokens(Words, ";"),
												NewMsg = lists:foldl(fun(W) -> content_utils:block(Msg, W, "*") end, Msg, WordList),
												{false, NewMsg} 
		end
	 },
	 
	 {"check_urls", fun(Msg, ScoreThreshold, Action, Host) ->
											 URLs = webroot_utils:extract_urls(Msg),
											 T = list_to_integer(ScoreThreshold), 
											 Scores = webroot_utils:get_scores(Host, URLs),
											 case Action of 
												 "drop" -> lists:any(fun(S) -> S >= T end, Scores);
												 "block" -> 	
													 NewMsg = lists:foldl(fun(U) -> content_utils:block(Msg, U, "*") end, Msg, URLs),
													 {false, NewMsg}
											 end			 
		end
	 }
	].	
