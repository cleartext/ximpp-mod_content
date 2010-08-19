%%-------------------------------------------------------------------
%%% File    : mod_content_filter.erl
%%% Author  : Boris Okner <b.okner@rogers.com>
%%% Description : Criteria-based message filtering
%%%
%%% Created : 14 Jun 2009 by Boris Okner <b.okner@rogers.com>
%%%-------------------------------------------------------------------
-module(mod_content_filter).

-behaviour(gen_server).
-behavior(gen_mod).

-export([start/2, stop/1, filter_packet/1]).
-export([test/0]).

%% API

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
				 terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").


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
	gen_server:start_link({local, ContentFilterName}, ?MODULE, [Host, Bindings], []).


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
	%% Read existing criteria from database
	catch ejabberd_odbc:sql_query(Host, "create table if not exists criteria (id int unsigned not null auto_increment, host text, predicate text, arguments text, action text, direction text, primary key (id))"),	
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
	{ok, #state{host = Host, criteria = CompiledCriteria, bindings = Bindings}}.

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
														D == both orelse D == Direction 
													end, State#state.criteria),
	{reply, Criteria, State};

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
	BindingFile = proplists:get_value(predicate_bindings, Opts, []),
	{ok, Bindings} = get_bindings(BindingFile),
	start_link(Host, Bindings),
	%% Start WebRoot service
	WebRootURL = proplists:get_value(webroot_url, Opts, none),
	WebRootUser = proplists:get_value(webroot_user, Opts, none),
	WebRootPasswd = proplists:get_value(webroot_password, Opts, none),
	case WebRootURL of
		none -> ok;
		_ -> webroot_service:start_link(Host, WebRootURL, WebRootUser, WebRootPasswd)
	end.

stop(Host) ->
	gen_server:cast(get_filter_name(Host), stop),
	webroot_service:stop(Host),
	ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 50).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_filter_name(Host) ->
	A = atom_to_list(?MODULE),
	list_to_atom(A ++ "_" ++ Host).

filter_packet({From, To,  {xmlelement, Name, _Attrs, _Els} = Packet}) when Name == "message" ->
	?DEBUG("Packet, From, To:~p~n, ~p~n, ~p~n", [Packet, From, To]),

	{jid, _PrepAcc, _PrepHost, _PrepRes, _Acc, HostFrom, _Res} = From,		
	{jid, _PrepAcc, _PrepHost, _PrepRes, _Acc, HostTo, _Res} = To,

	
	case inspect_message(HostFrom, HostTo, Packet)  of
		drop ->
			%%{From, To, {xmlelement, Name, [{"flag", "censored"} | Attrs], Els}};
			?INFO_MSG("Dropped by content filter:~p", [Packet]),
			drop;
		{keep, NewMsgBody} ->
			{From, To, replace_body(Packet, NewMsgBody)}
	end;

filter_packet(P) ->
	?DEBUG("Filter packet:~p~n", [P]),
	P.

inspect_message(HostFrom, HostTo, Packet) ->
	OutCriteria = get_criteria(HostFrom, out),
	MsgBody = xml:get_subtag_cdata(Packet ,"body"),
	R1 = inspect_message(MsgBody, OutCriteria),
	case R1 of
		drop -> drop;
		{keep, NewMsg} -> 
			InCriteria = get_criteria(HostTo, in),
			inspect_message(NewMsg, InCriteria)
	end.

inspect_message(Text, undefined) ->
	{keep, Text};

inspect_message(Text, Criteria) ->
	catch(lists:foldl(fun({_Predicate, CrFun}, {keep, AccMsg}) -> 
									 case CrFun(AccMsg) of 
										drop -> 
											throw(drop);
										keep ->
											{keep, AccMsg};
										{keep, M} ->
											{keep, M}
									 end
							end, {keep, Text}, Criteria)).

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

replace_body(Packet, NewBody) ->
	OldBodyElem = xml:get_subtag(Packet, "body"),
	NewBodyElem = exmpp_xml:set_cdata(OldBodyElem, NewBody),
	exmpp_xml:replace_child(Packet, OldBodyElem, NewBodyElem).

test() ->
	mod_content_filter:start("cleartext.com", [{predicate_bindings, "/opt/ejabberd-2.1.3/conf/cond_bindings.cfg"},
																						 {webroot_url, "http://72.5.172.35:3128/wwss_url_checker"},
																						 {webroot_user, "test@tagged.com"},
																						 {webroot_password, "wrtest"}
																						]).
