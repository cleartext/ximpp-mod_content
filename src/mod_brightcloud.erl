%%% -------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : BrightCloud service
%%%
%%% Created : Oct 27, 2010 
%%%-------------------------------------------------------------------
-module(mod_brightcloud).

-behaviour(gen_mod).
-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/2, stop/1, start_link/2, get_scores/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-define(DEFAULT_TIMEOUT, 10000).
-define(PROCNAME, ejabberd_mod_brightcloud).
-record(state, {host, service_url, uid, product_id, oem_id, categories = dict:new()}).

%%--------------------------------------------------------------------
%%% gen_mod behavior
%%--------------------------------------------------------------------
start_link(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  ChildSpec =
    {Proc,
     {?MODULE, start_link, [Host, Opts]},
	 2000,
	 worker,
	 [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec),
		?DEBUG("BrighCloud service started on ~p~n", [Host]).

stop(Host) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  ?DEBUG("Stopping server ~p~n", [Proc]),  
  gen_server:call(Proc, stop),
  supervisor:delete_child(ejabberd_sup, Proc).



%% ====================================================================
%% External functions
%% ====================================================================

get_scores(Host, URLs) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
        gen_server:call(Proc, {get_scores, URLs}, ?DEFAULT_TIMEOUT).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Host, Opts]) ->
  ServiceUrl = proplists:get_value(service_url, Opts),
  UID = proplists:get_value(uid, Opts),
  ProductId = proplists:get_value(product_id, Opts),
  OemId = proplists:get_value(oem_id, Opts),

    {ok, #state{host = Host, service_url = ServiceUrl, uid = UID, product_id = ProductId, oem_id = OemId, categories = dict:new()}, 0}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}   
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({get_scores, URLs}, _From, #state{service_url = ServiceUrl, uid = UID, product_id = ProductId, oem_id = OemId} = State) ->
    Reply = brightcloud_utils:get_scores(URLs, ServiceUrl, UID, ProductId, OemId),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
  ?DEBUG("Stopping BrighCloud service...~nState:~p~n", [State]),  
  {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, #state{service_url = ServiceUrl, uid = UID, product_id = ProductId, oem_id = OemId} = State) ->
  Categories = brightcloud_utils:get_categories(ServiceUrl, UID, ProductId, OemId),
		{noreply, State#state{categories = dict:from_list(Categories)}};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

