%%% -------------------------------------------------------------------
%%% Copyright 2010 Cleartext
%%% Author  : Boris Okner <boris.okner@gmail.com>
%%% Description : BrightCloud service
%%%
%%% Created : Oct 27, 2010 
%%%-------------------------------------------------------------------
-module(brightcloud_service).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/5, stop/1, get_scores/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-define(DEFAULT_TIMEOUT, 10000).
-record(state, {service_url, uid, product_id, oem_id, categories = dict:new()}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Host, ServiceUrl, UID, ProductId, OemId) ->
	ServiceName = get_service_name(Host),
  ServiceProc = gen_mod:get_module_proc(Host, ServiceName),
  ServiceChildSpec =
	{ServiceProc,
	 {gen_server, start_link, [{local, ServiceName}, ?MODULE, [ServiceUrl, UID, ProductId, OemId], []]},
	 permanent,
	 2000,
	 worker,
	 [gen_server]},
    supervisor:start_child(ejabberd_sup, ServiceChildSpec),
		?DEBUG("BrighCloud service started on ~p~n", [Host]).

get_scores(Host, URLs) ->
        gen_server:call(get_service_name(Host), {get_scores, URLs}, ?DEFAULT_TIMEOUT).

stop(Host) ->
        gen_server:cast(get_service_name(Host), stop).
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
init([ServiceUrl, UID, ProductId, OemId]) ->
    {ok, #state{service_url = ServiceUrl, uid = UID, product_id = ProductId, oem_id = OemId, categories = dict:new()}, 0}.

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
  brightcloud_utils:get_categories(ServiceUrl, UID, ProductId, OemId),
		{noreply, State};

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
get_service_name(Host) ->
        A = atom_to_list(?MODULE),
        list_to_atom(A ++ "_" ++ Host).