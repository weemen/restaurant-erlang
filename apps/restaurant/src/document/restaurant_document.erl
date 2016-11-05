
-module(restaurant_document).
-author("weemen").

-define(PROCESS_TIME_OUT, 45000).

%% API
-export([new/0, getDocumentProperty/2, setDocumentProperty/2]).


new() ->
  spawn(
    fun() ->
      init(),
      process_flag(trap_exit, true)
    end
  ).

init() ->
  Uuid = uuid:uuid_to_string(uuid:get_v4()),
  State = [
      {<<"id">>, list_to_binary(Uuid)},
      {<<"tableNumber">>, <<"0">>},
      {<<"items">>, []},
      {<<"ingredients">>, []},
      {<<"timeToCookMs">>, <<"500">>},
      {<<"tax">>, <<"1.06">>},
      {<<"total">>, <<"0">>},
      {<<"paid">>, false}
    ],
  loop(State).

loop(State) ->
  receive
    {get_document_property, Ref, DocumentProperty, Pid} ->
      Pid ! {getDocumentProperty(State, DocumentProperty), Ref},
      loop(State);
    {set_document_property, Ref, Property, Value, Pid} ->
      UpdatedState = setDocumentProperty(State, {Property, Value}),
      Pid ! {UpdatedState, Ref},
      loop(UpdatedState);
    Unknown ->
      error_logger:warning_msg("Received unknown message (~p)~n", [Unknown]),
      loop(State)
  after ?PROCESS_TIME_OUT ->
    error_logger:warning_msg("Process timed out (~p)~n", [?MODULE]),
%%    repository:remove_from_cache(Id),
    exit(normal)
  end.

getDocumentProperty([],PropertyOfInterest) ->
  {false, PropertyOfInterest};

getDocumentProperty([{PropertyName,PropertyValue}|Tail], PropertyOfInterest) ->
  if
    PropertyName == PropertyOfInterest -> {ok, PropertyValue};
    true -> getDocumentProperty(Tail, PropertyOfInterest)
  end.

setDocumentProperty(State, {PropertyOfInterest, NewValue}) ->
  NewState = lists:append(
    lists:keydelete(PropertyOfInterest, 1, State),
    [{PropertyOfInterest, NewValue}]
  ),
  {ok, NewState}.
