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
-export([list_all_definition_names/1,
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


list_all_definition_names(#yarc_schema_cache{etscache = ETSCache}) ->
  [].

get_definition(Name, #yarc_schema_cache{etscache = ETSCache}) ->
  case ets:lookup(ETSCache, Name) of
    [] ->
      undefined;
    [{_Name, Definition}] ->
      Definition
  end.

put_definition(Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  Name = yarc_schema_definition:get_definition_name(Definition),
  ets:insert_new(ETSCache, {Name, Definition}).

put_definition(Name, Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  ets:insert_new(ETSCache, {Name, Definition}).


replace_definition(Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  Name = yarc_schema_definition:get_definition_name(Definition),
  ets:insert(ETSCache, {Name, Definition}),
  ok.

replace_definition(Name, Definition, #yarc_schema_cache{etscache = ETSCache}) ->
  ets:insert(ETSCache, {Name, Definition}),
  ok.


get_definition_index(#yarc_schema_cache{etscache = ETSCache}) ->
  case ets:lookup(ETSCache, ?YARC_SCHEMA_INDEX) of
    [] ->
      undefined;
    [{_Name, Index}] ->
      Index
  end.

put_definition_index(Index, #yarc_schema_cache{etscache = ETSCache}) ->
  ets:insert(ETSCache, {?YARC_SCHEMA_INDEX, Index}),
  ok.