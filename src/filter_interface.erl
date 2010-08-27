%%-------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : Web interface to content filter
%%%
%%% Created: Aug 23, 2010

-module(filter_interface).

%%
%% Include files
%%
-include("ejabberd_http.hrl").
%%
%% Exported Functions
%%
-export([process/2]).


%%
%% API Functions
%%

%%--------------------------------------------------------------------
%%% Web interface
%%--------------------------------------------------------------------

process(["update_rules"], #request{q = Query}) ->
	Host = proplists:get_value("host", Query, undefined),
	Result = case Host of 
						 undefined ->
							 "host is required";
						 _ ->
							 mod_content_filter:reload_rules(Host)
					 end,
   {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "head", [],
       [{xmlelement, "title", [], []}]},
      {xmlelement, "body", [],
       [{xmlelement, "p", [], [{xmlcdata, io_lib:format("~p", [Result])}]}]}]};

process(_Other, _Request) ->
	{400, [], {xmlelement, "h1", [],
               [{xmlcdata, "400 Bad Request"}]}}.
%%
%% Local Functions
%%

