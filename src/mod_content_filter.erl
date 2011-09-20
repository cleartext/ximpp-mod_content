%%-------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : Criteria-based message filtering
%%%
%%% Created : 14 Jul 2010 
%%%-------------------------------------------------------------------
-module(mod_content_filter).

-behaviour(gen_server).
-behavior(gen_mod).

-export([start/2, stop/1, filter_packet/1, reload_rules/1]).


%% API

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

%% Direction constants
-define(BOTH, "0").
-define(INBOUND, "1").
-define(OUTBOUND, "2").

-record(state, {host, criteria = [], bindings = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Host, Bindings) ->
	ContentFilterName = get_filter_name(Host),
	ContentFilterProc = gen_mod:get_module_proc(Host, ContentFilterName),
	ContentFilterChildSpec =
		{ContentFilterProc,
		 {gen_server, start_link, [{local, ContentFilterName}, ?MODULE, [Host, Bindings], []]},
		 permanent,
		 2000,
		 worker,
		 [gen_server]},
	supervisor:start_child(ejabberd_sup, ContentFilterChildSpec),
	?DEBUG("Main content filter started on ~p~n", [Host]).

reload_rules(Host) ->
	FilterName = get_filter_name(Host),
	gen_server:call(FilterName, reload).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Host, Bindings]) ->
    init_audit_table(Host),
	CList = load_criteria(Host, Bindings),
	{ok, #state{host = Host, criteria = CList, bindings = Bindings}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({get_criteria, Direction}, _From, State) ->
	Criteria = lists:filter(fun({{_P, _Args, _Action, D}, _Func}) -> 
															 D == ?BOTH orelse D == Direction 
													end, State#state.criteria),
	{reply, Criteria, State};

handle_call(reload, _From, #state{host = Host, bindings = Bindings} = State) ->
	Reply = ok,
	{reply, Reply, State#state{criteria = load_criteria(Host, Bindings)}};

handle_call({add_criterion, PredicateName, Args, Action, Direction}, _From, #state{criteria = Criteria, bindings = Bindings, host = Host} = State) ->
	case compile(PredicateName, Args, Action, Direction, Bindings, Host) of
		{ok, CompiledCriterion} ->
			Key = {PredicateName, Args, Action, Direction},
			{reply, ok, State#state{criteria = lists:keystore(Key, 1, Criteria, {Key, CompiledCriterion})}};
		Error ->
			{reply, Error, State}
	end;
handle_call({remove_criterion, PredicateName, Args}, _From, #state{criteria = Criteria} = State) ->
	{reply, ok, State#state{criteria = lists:keydelete({PredicateName, Args}, 1, Criteria)}};

handle_call(cancel_criteria, _From, State) ->
	{reply, ok, State#state{criteria = []}};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%%% gen_mod behavior
%%--------------------------------------------------------------------
start(Host, Opts) ->
	?DEBUG("mod_content_filter starting on ~p...", [Host]),
	ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 50),
	ejabberd_hooks:add(webadmin_page_host, Host, filter_interface, web_page_host, 50),
	BindingFile = proplists:get_value(predicate_bindings, Opts, []),
	{ok, Bindings} = get_bindings(BindingFile),
	start_link(Host, Bindings).

stop(Host) ->
	gen_server:cast(get_filter_name(Host), stop),
	ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 50).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_filter_name(Host) ->
	A = atom_to_list(?MODULE),
	list_to_atom(A ++ "_" ++ Host).

filter_packet({From, To,  {xmlelement, Name, _Attrs, _Els} = Packet}) when Name == "message" ->
	?DEBUG("Packet, From, To:~p~n, ~p~n, ~p~n", [Packet, From, To]),
	
	{jid, _, _, _, _, HostFrom, _} = From,		
	{jid, _, _, _, _, HostTo, _} = To,
	
	case inspect_message(HostFrom, HostTo, Packet)  of
		{drop, Predicate, Text} ->			
			?INFO_MSG("Dropped by content filter:~p", [Packet]),
   spawn(fun() ->
                 MyHost = hd(ejabberd_config:get_global_option(hosts)),
                 add_audit_record(MyHost, From, To, Predicate, Text)
         end),

			drop;
		{ok, NewMsgBody} ->
			{From, To, exmpp_xml:xmlel_to_xmlelement(replace_content(Packet, NewMsgBody))}
	end;

filter_packet(P) ->
	?DEBUG("Filter packet:~p~n", [P]),
	P.

inspect_message(HostFrom, HostTo, Packet) ->
	OutCriteria = get_criteria(HostFrom, ?OUTBOUND),
	MsgBody = to_text(exmpp_xml:get_element(Packet ,"body")),
	HtmlBody = case exmpp_xml:get_element(exmpp_xml:get_element(Packet, "html"),
																				"body") of undefined -> undefined;
							 H -> to_text(H)
						 end,
	%% Twitter-specific element
	TextBody = case get_twitter_x_elem(Packet) of
							 undefined -> undefined;
							 X -> to_text(exmpp_xml:get_element(X, "text"))
						 end,					 		
	R1 = inspect_message([{msg, MsgBody}, {html, HtmlBody}, {text, TextBody}], OutCriteria),
	case R1 of
		{ok, NewMsg} -> 
			InCriteria = get_criteria(HostTo, ?INBOUND),
			inspect_message(NewMsg, InCriteria);
  Drop ->
      Drop
	end.

inspect_message(Msg, undefined) ->
	Msg;

inspect_message(Msg, Criteria) ->
    try 
	NewMsg = lists:foldl(
					fun({Predicate, CrFun}, AccMsg) -> 												 
							 %% Loop over message parts
							 lists:foldl(
								 fun
									 ({_, undefined}, Acc) -> Acc;
									 ({Type, Text}, Acc)	->					 
											FilteredText = 
												try CrFun(Text) of 
													drop -> 
														throw({drop, Predicate, Text});
													keep ->
														Text;
													{keep, NewText} ->
														NewText
												catch
													_Err:Reason ->
														?CRITICAL_MSG("Problem evaluating ~p:~nText:~p, Type:~p ~p~n", [Predicate, Text, Type, Reason]),
														Text
												end, 
											[{Type, FilteredText} | Acc]
								 end,				 
								 [], AccMsg)
					end, Msg, Criteria), 
 				{ok, NewMsg}
    catch throw:Drop ->
              Drop
    end.

load_criteria(Host, Bindings) ->
	%% Read existing criteria from database
	catch ejabberd_odbc:sql_query(Host, "create table if not exists criteria (id int unsigned not null auto_increment, host text, predicate text, arguments text, action text, direction int default 0, primary key (id))"),	
	SQL = "select predicate, arguments, action, direction from criteria where host='" ++ Host ++ "'",
	CompiledCriteria = case catch(ejabberd_odbc:sql_query(Host, SQL)) of
											 {selected, _Header, Rs} when is_list(Rs) ->
												 lists:foldl(
													 fun({P, Args, Action, Direction}, L) ->
																R = compile(P, Args, Action, Direction, Bindings, Host),
																case R of																	
																	{ok, C} ->																		
																		[{{P, Args, Action, Direction}, C} | L];																	
																	{error, _} ->																		
																		L																
																end
													 end, [], Rs);
											 _ ->
												 []
										 end,
	%% Sort criteria list so "drop" rules go first
	lists:sort(fun criteria_sort/2, CompiledCriteria).	

get_criteria(Host, Direction) ->
	%% First check if the host has a filter
	FilterName = get_filter_name(Host),
	case erlang:whereis(FilterName) of
		undefined -> 
			undefined;
		_F ->
			gen_server:call(FilterName, {get_criteria, Direction})
	end.

%% Criterion compilation.
%% The result of compilation is a function that could be directly applied to the packet's message body.
%% Bindings is a list of {PredicateName, Fun}, where Fun is fun(MessageBody, Args).
compile(PredicateName, Args, Action, Direction, Bindings, Host) ->
	case lists:keysearch(PredicateName, 1, Bindings) of
		{value, {PName, Fun}} ->
			?DEBUG("Compile:~p, fun:~p~n", [PName, Fun]),
			{ok, fun(Msg) -> erlang:apply(Fun, [Msg, Args, Action, Direction, Host]) end};
		_ ->
			{error, {predicate_not_supported, PredicateName}}
	end.

get_bindings(File) ->
	{ok, Bin} = file:read_file(File),
	Code = binary_to_list(Bin),
	{ok, Scanned, _} = erl_scan:string(Code),
	{ok, Parsed} = erl_parse:parse_exprs(Scanned),
	{value, Result, _} = erl_eval:exprs(Parsed, []),
	{ok, Result}.

%% [{msg, MsgBody}, {html, HtmlBody}, {text, TextEl}]
replace_content(Packet, NewMsg) ->
	%% Replace <body> content
	MsgBody = proplists:get_value(msg, NewMsg, undefined),
	OldBodyElem = exmpp_xml:get_element(Packet, "body"),
	NewBodyElem = exmpp_xml:xmlel_to_xmlelement(exmpp_xml:set_cdata(OldBodyElem, MsgBody)),
	P1 = exmpp_xml:replace_child(Packet, OldBodyElem, NewBodyElem),
	%% Replace <html><body> content
	HtmlBody = proplists:get_value(html, NewMsg, undefined),
	P2 = case HtmlBody of
				 undefined -> P1;
				 H ->
					 ?DEBUG("html markup: ~p~nPacket:~p~n ", [H, Packet]),
					 OldHtmlElem = exmpp_xml:get_element(Packet, "html"),
					 NewHtmlBody = xml_stream:parse_element(H),
					 NewHtmlElem = exmpp_xml:xmlel_to_xmlelement(exmpp_xml:set_children(OldHtmlElem, [NewHtmlBody])),
					 exmpp_xml:replace_child(P1, OldHtmlElem, NewHtmlElem)
			 end,
	%% Replace <x><text> content
	TextBody = proplists:get_value(text, NewMsg, undefined),
	case TextBody of
		undefined -> P2;
		T ->
			?DEBUG("twitter text:~p~n", [T]),
			OldParentTextElem = exmpp_xml:xmlel_to_xmlelement(get_twitter_x_elem(Packet)),
			NewTextEl = exmpp_xml:xmlel_to_xmlelement(exmpp_xml:set_cdata(exmpp_xml:element("text"), T)),
			NewParentTextElem = exmpp_xml:xmlel_to_xmlelement(exmpp_xml:set_children(OldParentTextElem, [NewTextEl])),
			exmpp_xml:replace_child(P2, OldParentTextElem, NewParentTextElem)
	end.

%% Sort criteria so "drop" rules come first
criteria_sort({{_, _, "drop", _}, _}, {{_, _, _A, _}, _}) -> 
	true; 
criteria_sort({{_, _, A, _}, _}, {{_, _, "drop", _}, _}) when A /= "drop" -> 
	false; 
criteria_sort(_A, _B) -> 
	true.

%% Retrieve Twitter text element fro packet, if any
get_twitter_x_elem(Packet) ->
	XEls = exmpp_xml:get_elements(Packet, "x"),	
	case lists:dropwhile(fun(X) -> exmpp_xml:get_attribute(X, "type", undefined) /= "tweet" end, XEls)
		of [] -> undefined;
		L -> hd(L)
	end.

%% Turn xml to text
%% This is quick and dirty way (for xml with children we convert everything, including tags, which shouldn't be part of filtered content)
to_text(Xmlel) ->
	C = exmpp_xml:get_child_elements(Xmlel),
	case C of 
		[] -> exmpp_xml:get_cdata_as_list(Xmlel);
		_ ->  exmpp_xml:document_to_list(Xmlel)
	end.	

%% Initialize audit table
%% id - int (auto inc)
%% from - varchar (full JID)
%% to - varchar (full JID)
%% rule - varchar
%% content - varchar (what was blocked)
%% timestamp - int 
init_audit_table(Host) ->
    	catch ejabberd_odbc:sql_query(Host, "create table if not exists cleartext_audit   (id int unsigned not null auto_increment, msg_from text, msg_to text, rule text, msg_content text, msg_timestamp int, explanation text, primary key (id))"),	
    ok.

add_audit_record(Host, From, To, Rule, Content) ->
    InsertStmt = lists:flatten(io_lib:format("insert into cleartext_audit(msg_from, msg_to, rule, msg_content, msg_timestamp) values ('~s', '~s', '~s', '~s', ~p)",
                               [jlib:jid_to_string(From), jlib:jid_to_string(To), Rule, Content, content_utils:unix_timestamp(erlang:now())] )),
        ejabberd_odbc:sql_query(Host, InsertStmt).


