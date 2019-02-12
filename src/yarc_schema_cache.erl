%%%-------------------------------------------------------------------
%%% @author forgottenEntity
%%% @doc
%%%
%%% @end
%%% Created : 12. Feb 2019 00:22
%%%-------------------------------------------------------------------
-module(yarc_schema_cache).
-author("forgottenEntity").

%% API
-export([
         get_definition/2,
         put_definition/2,
         put_definition/3,
         replace_definition/2,
         replace_definition/3,
         new/0,
         get_definition_index/1,
         put_definition_index/2
        ]).

-define(YARC_SCHEMA_INDEX, <<"yarc_schema_index">>).

-record(yarc_schema_cache, {etscache = undefined}).
-type binary_json() :: binary().

%%====================================================================
%% API functions
%%====================================================================

new() ->
  ETSCache = ets:new(yarc_schema_cache, [public, ordered_set, {read_concurrency, true}]),
  #yarc_schema_cache{etscache = ETSCache}.

-spec(get_definition(Name :: binary(), YarcSchemaCache :: tuple()) -> undefined | binary_json()).
get_definition(Name, #yarc_schema_cache{etscache = ETSCache}) ->
  case ets:lookup(ETSCache, Name) of
    [] ->
      undefined;
    [{_Name, Definition}] ->
      Definition
  end.

-spec(put_definition(Definition :: binary_json(), YarcSchemaCache :: tuple()) -> ok).
put_definition(Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  Name = yarc_schema_definition:get_definition_name(Definition),
  ets:insert_new(ETSCache, {Name, Definition}).

-spec(put_definition(Name :: binary(), Definition :: binary_json(), YarcSchemaCache :: tuple()) -> ok).
put_definition(Name, Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  ets:insert_new(ETSCache, {Name, Definition}).

-spec(replace_definition(Definition :: binary_json(), YarcSchemaCache :: tuple()) -> ok).
replace_definition(Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  Name = yarc_schema_definition:get_definition_name(Definition),
  ets:insert(ETSCache, {Name, Definition}),
  ok.

-spec(replace_definition(Name :: binary(), Definition :: binary_json(), YarcSchemaCache :: tuple()) -> ok).
replace_definition(Name, Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  ets:insert(ETSCache, {Name, Definition}),
  ok.

-spec(get_definition_index(YarcSchemaCache :: tuple()) -> undefined | list()).
get_definition_index(#yarc_schema_cache{etscache = ETSCache}) ->
  case ets:lookup(ETSCache, ?YARC_SCHEMA_INDEX) of
    [] ->
      undefined;
    [{_Name, Index}] ->
      Index
  end.

-spec(put_definition_index(Index :: list(), YarcSchemaCache :: tuple()) -> ok).
put_definition_index(Index, #yarc_schema_cache{etscache = ETSCache}) ->
  ets:insert(ETSCache, {?YARC_SCHEMA_INDEX, Index}),
  ok.