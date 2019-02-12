%%%-------------------------------------------------------------------
%%% @author forgottenEntity
%%% @doc
%%%
%%% @end
%%% Created : 12. Feb 2019 13:18
%%%-------------------------------------------------------------------
-module(yarc_schema_index).
-author("forgottenEntity").

%% API
-export([get_index/1,
         put_index/2,
         add_index_entry/2
        ]).

-define(YARC_SCHEMA_INDEX, <<"yarc_schema_index">>).
-define(DEFAULT_TIMEOUT, 5000).


%% ******************************************************************************
%% API Functions
%% ******************************************************************************

get_index(SchemaCache) ->
  case yarc_schema_cache:get_definition_index(SchemaCache) of
    Index when is_list(Index) ->
      Index;
    _AnythingElse ->
      Index = get_definition_index_from_riak(),
      yarc_schema_cache:put_definition_index(Index, SchemaCache),
      Index
  end.

put_index(Index, SchemaCache) ->
  put_definition_index_to_riak(Index),
  yarc_schema_cache:put_definition_index(Index, SchemaCache),
  ok.

add_index_entry(Key, SchemaCache) ->
  Index = get_index(SchemaCache),
  NewIndex = ordsets:add_element(Key, Index),
  put_index(NewIndex, SchemaCache).


%%====================================================================
%% Internal functions
%%====================================================================

get_definition_index_from_riak() ->
  RiakConnection = yarc_riak_connection_pool:get_connection(yarc_riak_pool),
  Definition = case yarc_riak_connection:get(RiakConnection, ?YARC_SCHEMA_INDEX, ?YARC_SCHEMA_INDEX, ?DEFAULT_TIMEOUT) of
                 {ok, RiakObj} ->
                   riakc_obj:get_value(RiakObj);
                 Error ->
                   Error
               end,
  yarc_riak_connection_pool:return_connection(yarc_riak_pool, RiakConnection),
  Definition.

put_definition_index_to_riak(Index) ->
  RiakConnection = yarc_riak_connection_pool:get_connection(yarc_riak_pool),
  RiakObj = riakc_obj:new(?YARC_SCHEMA_INDEX, ?YARC_SCHEMA_INDEX, Index),
  yarc_riak_connection:put(RiakConnection, ?DEFAULT_TIMEOUT, RiakObj),
  yarc_riak_connection_pool:return_connection(yarc_riak_pool, RiakConnection),
  ok.