%%%-------------------------------------------------------------------
%%% @author chris
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Feb 2019 00:22
%%%-------------------------------------------------------------------
-module(yarc_schema_cache).
-author("chris").

%% API
-export([list_all_definition_names/1,
         get_definition/2,
         put_definition/2,
         put_definition/3,
         new/0
        ]).

-record(yarc_schema_cache, {etscache = undefined}).
-type binary_json() :: binary().

%%====================================================================
%% API functions
%%====================================================================

new() ->
  ETSCache = ets:new(yarc_schema_cache, [private, ordered_set, {read_concurrency, true}]),
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
