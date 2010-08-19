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
	[{"contains", fun(Msg, Phrase, "drop", _Direction, _Host) ->
										 case string:str(Msg, Phrase) of
											 0 -> 
												 {keep, Msg};
											 _ ->
												 drop
										 end;			
									(Msg, Phrase, "block", _Direction, _Host) ->                 	
										{keep, content_utils:block(Msg, Phrase, "*")}  	
		end
	 },
	 
	 {"equals", fun(Msg, Phrase, "drop", _Direction, _Host) ->
									 case Msg =:= Phrase of
									  true -> drop;
									  false -> {keep, Msg}
									 end;
								(Msg, Phrase, "block", _Direction, _Host) ->                 	
									{keep, content_utils:block(Msg, Phrase, "*")}                	
		end
	 },
	 
	 {"contains_all", fun(Msg, Words, "drop", _Direction, _Host) ->
												 WordList = string:tokens(Words, ";"),
												 ContainsAll = lists:all(fun(W) -> string:str(Msg, W) > 0 end, WordList),
												 case ContainsAll of 
												 	true -> drop;
												 	false -> {keep, Msg}
												 end;	
											(Msg, Words, "block", _Direction, _Host) ->
												WordList = string:tokens(Words, ";"),
												NewMsg = lists:foldl(fun(W, M) -> content_utils:block(M, W, "*") end, Msg, WordList),
												{keep, NewMsg}                	                  
		end
	 },
	 
	 {"contains_any", fun(Msg, Words, "drop", _Direction, _Host) ->
												 WordList = string:tokens(Words, ";"),
												 ContainsAny = lists:any(fun(W) -> string:str(Msg, W) > 0 end, WordList),
												 case ContainsAny of 
												 	true -> drop;
												 	false -> {keep, Msg}
												 end;	
											(Msg, Words, "block", _Direction, _Host) ->
												WordList = string:tokens(Words, ";"),
												NewMsg = lists:foldl(fun(W, M) -> content_utils:block(M, W, "*") end, Msg, WordList),
												{keep, NewMsg} 
		end
	 },
	 
	 {"check_urls", fun(Msg, ScoreThreshold, Action, _Direction, Host) ->
											 URLs = webroot_utils:extract_urls(Msg),
											 T = list_to_integer(ScoreThreshold), 
											 Scores = webroot_utils:get_scores(Host, URLs),
											 case Action of 
												 "drop" -> 
												 	case lists:any(fun(S) -> S >= T end, Scores) of
												 		true -> drop;
												 		false -> {keep, Msg}
												 	end;	
												 "block" -> 	
													 NewMsg = lists:foldl(fun(U, M) -> content_utils:block(M, U, "*") end, Msg, URLs),
													 {keep, NewMsg}
												end	 
											 end			 
	 }
	].	
	
