%%%-------------------------------------------------------------------
%%% @author forgottenEntity
%%% @doc
%%%
%%% @end
%%% Created : 29. Jan 2019 00:32
%%%-------------------------------------------------------------------
-module(yarc_schema).
-author("forgottenEntity").


%% API exports
-export([list_all_definitions/0,
         get_definition/2,
         put_definition/2,
         new/0
        ]).

-define(YARC_SCHEMA_BUCKET, <<"yarc_schema">>).
-define(DEFAULT_TIMEOUT, 5000).

-type binary_json() :: binary().

%%====================================================================
%% API functions
%%====================================================================

new() ->
  yarc_schema_cache:new().

list_all_definitions() ->
  [].

-spec(get_definition(Name :: binary(), YarcSchemaState :: tuple()) -> binary_json() | {error, notfound}).
get_definition(Name, SchemaCache) ->
  case yarc_schema_cache:get_definition(Name, SchemaCache) of
    undefined ->
      case get_definition_from_riak(Name) of
        Definition when is_binary(Definition) ->
          yarc_schema_cache:put_definition(Name, Definition, SchemaCache),
          Definition;
        _AnythingElse ->
          {error, notfound}
      end;
    Definition ->
      Definition
  end.

-spec(put_definition(Definition :: binary_json(), YarcSchemaState :: tuple()) -> ok | {error, term()}).
put_definition(Definition, SchemaCache) ->
  Name = yarc_schema_definition:get_definition_name(Definition),
  yarc_schema_cache:put_definition(Name, Definition, SchemaCache),
  put_definition_to_riak(Name, Definition).



%%====================================================================
%% Internal functions
%%====================================================================

get_definition_from_riak(Name) ->
  RiakConnection = yarc_riak_connection_pool:get_connection(yarc_riak_pool),
  Definition = case yarc_riak_connection:get(RiakConnection, ?YARC_SCHEMA_BUCKET, Name, ?DEFAULT_TIMEOUT) of
                 {ok, RiakObj} ->
                   riakc_obj:get_value(RiakObj);
                 Error ->
                   Error
               end,
  yarc_riak_connection_pool:return_connection(yarc_riak_pool, RiakConnection),
  Definition.

put_definition_to_riak(Name, Definition) ->
  RiakConnection = yarc_riak_connection_pool:get_connection(yarc_riak_pool),
  RiakObj = riakc_obj:new(?YARC_SCHEMA_BUCKET, Name, Definition),
  yarc_riak_connection:put(RiakConnection, ?DEFAULT_TIMEOUT, RiakObj),
  yarc_riak_connection_pool:return_connection(yarc_riak_pool, RiakConnection),
  ok.