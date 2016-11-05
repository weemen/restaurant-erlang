-module(pubsub).
-author("weemen").

-define(PROCESS_TIME_OUT, 45000).

%% API
-export([new/0]).

new() ->
  spawn(
    fun() ->
      init(),
      process_flag(trap_exit, true)
    end
  ).

init() ->
  error_logger:info_msg("initializing pubsub"),
  ets:new(pubsub_subscribers,[bag, named_table]),
  loop().

loop() ->
  receive
    {publish, Ref, Topic, Content, Pid} ->
      Pid ! {publish(Topic, Content), Ref},
      loop();
    {subscribe, Ref, Topic, Pid} ->
      Pid ! {subscribe(Topic, Pid), Ref},
      loop();
    {unsubscribe, Ref, Topic, Pid} ->
      Pid ! {unsubscribe(Topic, Pid), Ref},
      loop();
    {"EXIT", _, _} ->
      cleanup(),
      exit(normal);
    Unknown ->
      error_logger:warning_msg("Received unknown message (~p)~n", [Unknown]),
      loop()
  after ?PROCESS_TIME_OUT ->
    error_logger:warning_msg("Process timed out (~p)~n", [?MODULE]),
    exit(normal)
  end.

subscribe(Topic, Pid) ->
  case ets:lookup(pubsub_subscribers, {Topic,Pid}) of
    [_] ->
      error_logger:warning_msg("Topic: ~p already exists for process: ~p~n", [Topic, Pid]),
      false;
    [] ->
      ets:insert(pubsub_subscribers, {Topic, Pid})
  end.

unsubscribe(Topic, Pid) ->
  case ets:match_object(pubsub_subscribers, {Topic, Pid}) of
    [] ->
      error_logger:info_msg("no process ~p is not subscribed on topic: ~p~n", [Pid, Topic]),
      false;
    [Result] ->
      {Topic, Pid} = Result,
      ets:delete(pubsub_subscribers, {Topic, Pid}),
      true
  end.

publish(Topic, Content) ->
  case ets:lookup(pubsub_subscribers, Topic) of
    [] ->
      error_logger:info_msg("no process is subscribed on topic: ~p~n", [Topic]),
      {true, no_one_to_notify};
    Results ->
      notify_subscribers(Results, Content),
      {true, notified_subscribers}
  end.

notify_subscribers([{Topic,Pid}|Tail], Content) ->
  Pid ! {Topic, Content},
  error_logger:info_msg("notify topic: ~p~n", [Topic]),
  notify_subscribers(Tail, Content);

notify_subscribers([], Topic) ->
  error_logger:info_msg("All subscribers notified for topic: ~p~n", [Topic]).

cleanup() ->
  error_logger:warning_msg("Cleaning up pubsub (~p)~n", [?MODULE]),
  ets:delete(pubsub_subscribers).
