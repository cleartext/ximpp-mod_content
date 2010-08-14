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
	catch ejabberd_odbc:sql_query(Host, "create table if not exists criteria (host text, predicate text, arguments text)"),
	
	SQL = "select predicate, arguments from criteria where host=" ++ Host,
	CompiledCriteria = case catch(ejabberd_odbc:sql_query(Host, SQL)) of
											 {selected, _Header, Rs} when is_list(Rs) ->
												 lists:foldl(
													 fun({P, Args}, L) ->
																R = compile(P, Args, Bindings, Host),
																case R of
																	{ok, C} ->
																		[{{P, Args}, C} | L];
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
handle_call(get_criteria, _From, State) ->
	{reply, State#state.criteria, State};

handle_call({add_criterion, PredicateName, Args}, _From, #state{criteria = Criteria, bindings = Bindings, host = Host} = State) ->
	case compile(PredicateName, Args, Bindings, Host) of
		{ok, CompiledCriterion} ->
			Key = {PredicateName, Args},
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
	?INFO_MSG("mod_content_filter starting on ~p...", [Host]),
	ejabberd_hooks:add(filter_packet, global, ?MODULE, filter_packet, 50),
	BindingFile = proplists:get_value(predicate_bindings, Opts, []),
	{ok, Bindings} = get_bindings(BindingFile),
	start_link(Host, Bindings),
	%% Start WebRoot service
	WebRootURL = proplists:get_value(webroot_url, none),
	WebRootUser = proplists:get_value(webroot_user, none),
	WebRootPasswd = proplists:get_value(webroot_password, none),
	case WebRootURL of
		none -> ok;
		_ -> webroot_service:start_link(Host, WebRootURL, WebRootUser, WebRootPasswd)
	end.

stop(_Host) ->
	ejabberd_hooks:delete(filter_packet, global, ?MODULE, filter_packet, 50).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
get_filter_name(Host) ->
	A = atom_to_list(?MODULE),
	list_to_atom(A ++ "_" ++ Host).

filter_packet({From, To,  {xmlelement, Name, _Attrs, _Els} = Packet}) when Name == "message" ->
	Host = exmpp_jid:prep_domain_as_list(To),
	case isCensoredMsg(Host, Packet)  of
		true ->
			%%{From, To, {xmlelement, Name, [{"flag", "censored"} | Attrs], Els}};
			?INFO_MSG("Dropped by content filter:~p", [Packet]),
			drop;
		false ->
			{From, To, Packet}
	end;

filter_packet(P) ->
	P.


isCensoredMsg(Host, Packet) ->
	Criteria = getCriteria(Host),
	MsgBody = xml:get_subtag_cdata(Packet ,"body"),
	lists:any(fun({_Raw, CrFun}) -> CrFun(MsgBody, Host) end, Criteria).


getCriteria(Host) ->
	gen_server:call(get_filter_name(Host), get_criteria).



%% Criterion compilation.
%% The result of compilation is a function that could be directly applied to the packet's message body
%% Bindings is a list of {PredicateName, Fun}, where Fun is fun(Message, Args).
compile(PredicateName, Args, Bindings, Host) ->
	case lists:keysearch(PredicateName, 1, Bindings) of
		{value, {PName, Fun}} ->
			io:format("Compile:~p, fun:~p~n", [PName, Fun]),
			{ok, fun(Msg) -> erlang:apply(Fun, [Msg, Args, Host]) end};
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

