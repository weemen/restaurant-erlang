-module(pubsub_test).
-author("weemen").
-include_lib("eunit/include/eunit.hrl").
%% API
-export([]).

it_can_subscribe_to_a_topic_test_() ->
  {
    setup,
    fun create_pubsub/0,
    fun destroy_pubsub/1,
    fun subscribe_to_a_topic/1
  }.

it_can_subscribe_to_multiple_topics_test_() ->
  {
    setup,
    fun create_pubsub/0,
    fun destroy_pubsub/1,
    fun subscribe_to_multiple_topics/1
  }.

it_can_publish_without_subscribers_test_() ->
  {
    setup,
    fun create_pubsub/0,
    fun destroy_pubsub/1,
    fun publish_without_subscribers/1
  }.

it_can_publish_with_subscribers_test_() ->
  {
    setup,
    fun create_pubsub/0,
    fun destroy_pubsub/1,
    fun publish_with_subscribers/1
  }.

it_can_unsubscribe_without_subscribers_test_() ->
  {
    setup,
    fun create_pubsub/0,
    fun destroy_pubsub/1,
    fun unsubscribe_without_subscribers/1
  }.

it_can_unsubscribe_with_subscriber_test_() ->
  {
    setup,
    fun create_pubsub/0,
    fun destroy_pubsub/1,
    fun unsubscribe_with_subscriber/1
  }.

it_cleans_up_on_exit_signal_test_() ->
  {
    setup,
    fun create_pubsub/0,
    fun destroy_pubsub/1,
    fun cleanup_pubsub/1
  }.


subscribe_to_a_topic(Pid) ->
  Topic = "eunittest",
  Status = get_subscription_status(Pid, Topic),
  [
    ?_assertEqual(true, Status),
    ?_assertEqual(1,ets:select_count(pubsub_subscribers, [{ {Topic,'_'}, [], [true]} ]) )
  ].

subscribe_to_multiple_topics(Pid) ->
  Status1 = get_subscription_status(Pid, "eunittest1"),
  Status2 = get_subscription_status(Pid, "eunittest2"),
  [
    ?_assertEqual(true, Status1),
    ?_assertEqual(true, Status2),
    ?_assertEqual(2,ets:select_count(pubsub_subscribers, [{ {'_','_'}, [], [true]} ]) ),
    ?_assertEqual(1,ets:select_count(pubsub_subscribers, [{ {"eunittest1",'_'}, [], [true]} ]) ),
    ?_assertEqual(1,ets:select_count(pubsub_subscribers, [{ {"eunittest2",'_'}, [], [true]} ]) )
  ].


publish_without_subscribers(Pid) ->
  [
    ?_assertEqual({true,no_one_to_notify}, get_publish_status(Pid, "eunittest", "this is a test"))
  ].


publish_with_subscribers(Pid) ->
  get_subscription_status(Pid, "eunittest"),
  [
    ?_assertEqual({true,notified_subscribers}, get_publish_status(Pid, "eunittest", "this is a test"))
  ].

unsubscribe_without_subscribers(Pid) ->
  [
    ?_assertEqual(false, get_unsubscribe_status(Pid, "eunittest"))
  ].

unsubscribe_with_subscriber(Pid) ->
  [
    ?_assertEqual(true, get_subscription_status(Pid, "eunittest")),
    ?_assertEqual(true, get_unsubscribe_status(Pid, "eunittest"))
  ].

cleanup_pubsub(Pid) ->
  get_cleanup_status(Pid),
  [
    ?_assertEqual(undefined, ets:info(pubsub_subscribers))
  ].

create_pubsub() ->
  pubsub:new().

destroy_pubsub(Pid) ->
  exit(Pid, kill).

get_subscription_status(Pid, Topic) ->
  Ref = make_ref(),
  Pid ! {subscribe, Ref, Topic, self()},
  receive
    {Msg,Ref} -> Msg
  after
    500 -> timeouted
  end.

get_publish_status(Pid, Topic, Content) ->
  Ref = make_ref(),
  Pid ! {publish, Ref, Topic, Content, self()},
  receive
    {Msg, Ref} -> Msg
  after
    500 -> timeouted
  end.

get_unsubscribe_status(Pid, Topic) ->
  Ref = make_ref(),
  Pid ! {unsubscribe, Ref, Topic, self()},
  receive
    {Msg, Ref} -> Msg
  after
    500 -> timeouted
  end.

get_cleanup_status(Pid) ->
  Pid ! {"EXIT", self(), shutdown}.
