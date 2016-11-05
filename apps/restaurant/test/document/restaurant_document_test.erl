
-module(restaurant_document_test).
-author("weemen").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

it_gets_an_element_from_restaurant_document_test_() ->
  DecodedMessage = jiffy:decode(<<"{\"foo\": \"bar\",\"bar\": \"baz\"}">>),
  {Message} = DecodedMessage,
  ?_assert({ok,<<"bar">>} =:= restaurantDocument:getDocumentProperty(Message,<<"foo">>)).

it_gets_false_if_element_from_restaurant_document_is_not_found_test_() ->
  DecodedMessage = jiffy:decode(<<"{\"foo\": \"bar\",\"bar\": \"baz\"}">>),
  {Message} = DecodedMessage,
  ?_assert({false,<<"barbaz">>} =:= restaurantDocument:getDocumentProperty(Message,<<"barbaz">>)).

it_gets_a_documentId_from_the_restaurant_document_after_spawn_test_() ->
  {
    setup,
    fun create_document/0,
    fun destroy_document/1,
    fun get_documentId_from_restaurant_document/1
  }.

a_restaurant_document_can_be_updated_test_() ->
  {
    setup,
    fun create_document/0,
    fun destroy_document/1,
    fun update_restaurant_document/1
  }.

get_documentId_from_restaurant_document(Pid) ->
  {Success, DocumentId} = get_restaurant_document(Pid),
  [
    ?_assertEqual(ok,Success),
    ?_assert(
      uuid:is_v4(
        uuid:string_to_uuid(
          binary_to_list(DocumentId)
        )
      ) == true
    )
  ].

update_restaurant_document(Pid) ->
  {Success, State} = update_tablenumber_in_restaurant_document(Pid),
  [
    ?_assertEqual(Success, ok),
    ?_assert({ok,<<"1">>} =:= restaurant_document:getDocumentProperty(State,<<"tableNumber">>))
  ].

create_document() ->
  restaurant_document:new().

destroy_document(Pid) ->
  exit(Pid, kill).

get_restaurant_document(Pid) ->
  Ref = make_ref(),
  Pid ! {get_document_property, Ref, <<"id">>, self()},
  receive
    {Msg, Ref} -> Msg
  after
    500 -> timeouted
  end.

update_tablenumber_in_restaurant_document(Pid) ->
  Ref = make_ref(),
  Pid ! {set_document_property, Ref, <<"tableNumber">>, <<"1">>, self()},
  receive
    {Msg, Ref} -> Msg
  after
    500 -> timeouted
  end.
