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
