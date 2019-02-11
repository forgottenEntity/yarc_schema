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
         get_definition/1,
         put_definition/1
        ]).

%%====================================================================
%% API functions
%%====================================================================

list_all_definitions() ->
  [].

get_definition(_Name) ->
  undefined.

put_definition(_Definition) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
