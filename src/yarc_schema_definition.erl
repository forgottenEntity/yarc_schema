%%%-------------------------------------------------------------------
%%% @author forgottenEntity
%%% @doc
%%% Represents Schema Entries as JSon, outwardly
%%% @end
%%% Created : 09. Feb 2019 02:47
%%%-------------------------------------------------------------------
-module(yarc_schema_definition).
-author("forgottenEntity").

%% API
-export([
          get_definition_name/1,
          create_definition_entry/1,
          add_definition_entry/3,
          new/1,
          filter_deleted_definition_entries/1
        ]).

-define(datatype_integer, 1).
-define(datatype_float, 2).
-define(datatype_binary, 3).
-define(datatype_boolean, 4).
-define(datatype_decimal, 5).
-define(datatype_string, 6).

%% ******************************************************************************
%% API Functions
%% ******************************************************************************

get_definition_name(Definition) ->
  Def = definition_from_json(Definition),
  maps:get(<<"name">>, Def).

%% ******************************************************************************
%% Internal Functions
%% ******************************************************************************

create_definition_entry(DataType) ->
  #{<<"datatype">> => DataType}.

new(Name) ->
  #{<<"name">> => Name}.

add_definition_entry(DefinitionEntryName, DefinitionEntry, Definition) ->
  case maps:is_key(DefinitionEntryName, Definition) of
    true ->
      %% entry already exists - either as a deleted or current entry - check here...begin
      ExistingDefinitionEntry = maps:get(DefinitionEntryName, Definition),
      case is_entry_deleted(ExistingDefinitionEntry) of
        true ->
          %% entry has existed in the past but has been deleted - so need to readd and increment version...
          DefinitionWithReAddedStatus = maps:put(<<"status">>, <<"entry_readded">>, ExistingDefinitionEntry),
          maps:put(DefinitionEntryName, increment_version(DefinitionWithReAddedStatus), Definition);
        _False ->
          {error, definition_entry_already_exists}
      end;
    _False ->
      %% no existing entry with the specified name - so just add the new entry...
      maps:put(DefinitionEntryName, increment_version(DefinitionEntry), Definition)
  end.

remove_definition_entry(Definition_Entry_Name, Definition) ->
  DefinitionEntry = maps:get(Definition_Entry_Name, Definition),
  RemovedEntry = maps:put(<<"status">>, <<"entry_deleted">>, DefinitionEntry),
  maps:put(Definition_Entry_Name, RemovedEntry, Definition).

is_entry_deleted(Definition) ->
  case maps:is_key(<<"status">>, Definition) of
    true ->
      case maps:get(<<"status">>, Definition) of
        <<"entry_deleted">> ->
          true;
        _AnyOtherStatus ->
          false
      end;
    _False ->
      false
  end.

get_definition_entry(Name, Definition) ->
  case maps:is_key(Name, Definition) of
    true ->
      maps:get(Name, Definition);
    _False ->
      {error, notfound}
  end.

get_definition_entry_datatype(DefinitionEntry) ->
  maps:get(<<"datatype">>, DefinitionEntry).

get_definition_entry_version(DefinitionEntry) ->
  case maps:is_key(<<"version">>, DefinitionEntry) of
    true ->
      maps:get(<<"version">>, DefinitionEntry);
    _False ->
      0
  end.

get_definition_entry_status(DefinitionEntry) ->
  case maps:is_key(<<"status">>, DefinitionEntry) of
    true ->
      maps:get(<<"status">>, DefinitionEntry);
    _False ->
      undefined
  end.

increment_version(Definition) ->
  maps:put(<<"version">>, get_definition_entry_version(Definition) + 1, Definition).


filter_deleted_definition_entries(DefinitionMap) ->
  maps:filter(
    fun(Key, Value) ->
      case Key of
        <<"name">> ->
          true;
        Key ->
          case get_definition_entry_status(Value) of
            <<"entry_deleted">> ->
              false;
            _AnythingElse ->
              true
          end
      end
      end,
    DefinitionMap).



definition_to_json(Definition) ->
  jsx:encode(Definition).

definition_from_json(JSon) ->
  jsx:decode(JSon, [return_maps]).

%% ******************************************************************************
%% Unit Tests
%% ******************************************************************************

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

add_definition_entry_new_test() ->
  DefinitionEntry = create_definition_entry(?datatype_string),
  Definition = add_definition_entry(test_string, DefinitionEntry, new(<<"test">>)),
  FetchedDefinitionEntry = get_definition_entry(test_string, Definition),
  ?assertEqual(?datatype_string, get_definition_entry_datatype(FetchedDefinitionEntry)),
  ?assertEqual(1, get_definition_entry_version(FetchedDefinitionEntry)),
  ?assertEqual(undefined, get_definition_entry_status(FetchedDefinitionEntry)).

add_definition_entry_existing_test() ->
  DefinitionEntry = create_definition_entry(?datatype_string),
  Definition = #{name => <<"test">>, test_string => #{datatype => ?datatype_string, version => 2}},
  ?assertEqual({error, definition_entry_already_exists}, add_definition_entry(test_string, DefinitionEntry, Definition)).

add_definition_entry_existing_deleted_test() ->
  DefinitionEntry = create_definition_entry(?datatype_string),
  Definition = #{name => <<"test">>, test_string => #{datatype => ?datatype_string, version => 2, status => entry_deleted}},
  NewDefinition = add_definition_entry(test_string, DefinitionEntry, Definition),
  FetchedDefinitionEntry = get_definition_entry(test_string, NewDefinition),
  ?assertEqual(?datatype_string, get_definition_entry_datatype(FetchedDefinitionEntry)),
  ?assertEqual(entry_readded, get_definition_entry_status(FetchedDefinitionEntry)),
  ?assertEqual(3, get_definition_entry_version(FetchedDefinitionEntry)).


add_two_definition_entries_and_remove_one_definition_entry_test() ->
  DefinitionEntry = create_definition_entry(?datatype_string),
  DefinitionA = add_definition_entry(forename, DefinitionEntry, new(<<"test">>)),
  DefinitionB = add_definition_entry(surname, DefinitionEntry, DefinitionA),

  ForenameDefinitionEntry = get_definition_entry(forename, DefinitionB),
  SurnameDefinitionEntry = get_definition_entry(surname, DefinitionB),
  ?assertEqual(?datatype_string, get_definition_entry_datatype(ForenameDefinitionEntry)),
  ?assertEqual(?datatype_string, get_definition_entry_datatype(SurnameDefinitionEntry)),
  ?assertEqual(1, get_definition_entry_version(ForenameDefinitionEntry)),
  ?assertEqual(1, get_definition_entry_version(SurnameDefinitionEntry)),
  ?assertEqual(undefined, get_definition_entry_status(ForenameDefinitionEntry)),
  ?assertEqual(undefined, get_definition_entry_status(SurnameDefinitionEntry)),


  DefinitionC = remove_definition_entry(surname, DefinitionB),
  FinalForenameDefinitionEntry = get_definition_entry(forename, DefinitionC),
  FinalSurnameDefinitionEntry = get_definition_entry(surname, DefinitionC),
  ?assertEqual(?datatype_string, get_definition_entry_datatype(FinalForenameDefinitionEntry)),
  ?assertEqual(?datatype_string, get_definition_entry_datatype(FinalSurnameDefinitionEntry)),
  ?assertEqual(1, get_definition_entry_version(FinalForenameDefinitionEntry)),
  ?assertEqual(1, get_definition_entry_version(FinalSurnameDefinitionEntry)),
  ?assertEqual(undefined, get_definition_entry_status(FinalForenameDefinitionEntry)),
  ?assertEqual(entry_deleted, get_definition_entry_status(FinalSurnameDefinitionEntry)).



  -endif.





