:- module(aop_runtime,[
  op(900, xfx, ::),
  (::)/2,
  (::)/3,
  (::)/4,

  op(950, yfx, to),
  (to)/2,
  (to)/3,

  start_aspect_event_dispatcher/1
  ]).

:- use_module('./helpers').
:- use_module('./inspection').
:- use_module(library(prolog_stack)).

% 
% Runtime support
% 

% do(Object, Message)
:- dynamic aop:do/3.
:- multifile aop:do/3.
:- multifile aop:do/3.

% Events -- on(Listener, EventType, Object, Message),
% where EventType is before or after
:- dynamic aop:on/5.
:- multifile aop:on/5.
:- multifile aop:on/5.

% For name of queue for aspect events
% aop:aspect_event_queue(Aspect, Queue, DispatcherThread)
:- dynamic aop:aspect_event_queue/3.
:- multifile aop:aspect_event_queue/3.
:- multifile aop:aspect_event_queue/3.

% If true, then event dispatcher started
% aop:aspect_events_started(Aspect).
:- dynamic aop:aspect_events_started/1.
:- multifile aop:aspect_events_started/1.
:- multifile aop:aspect_events_started/1.

% Actions -- at(Listener, ActionType, Object, Message),
% where ActionType is before or after
:- dynamic aop:at/5.
:- multifile aop:at/5.
:- multifile aop:at/5.

% 
% Central mechanics of method dispatching
% 

:- module_transparent to/2, to/3.

% When the right-hand side of a `to` expression is a list,
% then expand to execute Left to Element, where Element
% is each element in the list.
to(Left, [Right]) :-
  !,
  Left to Right.

to(Left, [Right|More]) :-
  More \= [],
  !,
  Left to Right,
  Left to More.

% Expand Left to have an additional parameter for obtaining
% a result, then use that as a target of :: to send the Right
% message
to(Left, Right) :-
  Left =.. [Functor | Args],
  append(Args, [Result], ExtendedArgs),
  ExtendedLeft =.. [Functor | ExtendedArgs],
  ExtendedLeft,
  Result::Right.

% Useful for chained instances of to (3 or more).
% When called this way, the 3rd argument is the result
% for the right side, in preparation for chaining.
to(Left, Right, Result) :-
  Right =.. [Functor | Args],
  append(Args, [Result], ExtendedArgs),
  ExtendedRight =.. [Functor | ExtendedArgs],
  Left to ExtendedRight.

% 
% Central mechanics of method dispatching
% 

::(_Object, []).

::(Object, [Message | Messages]) :- 
  Object :: Message,
  Object :: Messages.

::(Object, Message) :- send_message(Object, Message).

::(Object, Message, Extra) :- send_message(Object, Message, Extra).

::(Object, Message, Extra1, Extra2) :- send_message(Object, Message, Extra1, Extra2).

send_message(Object, Message) :-
  ( extended(_Aspect, Object, Message)
    -> (
      before(Object, Message),
      % run it
      aop:do(_, Object, Message),
      after(Object, Message)
      )  % run it
    ; aop:do(_, Object, Message)
    ).

send_message(Object, Message, ExtraArg) :-
  Message =.. MessageList,
  append(MessageList,[ExtraArg], ExtendedMessageList),
  ExtendedMessage =.. ExtendedMessageList,
  send_message(Object, ExtendedMessage).

send_message(Object, Message, ExtraArg1, ExtraArg2) :-
  Message =.. MessageList,
  append(MessageList,[ExtraArg1, ExtraArg2], ExtendedMessageList),
  ExtendedMessage =.. ExtendedMessageList,
  send_message(Object, ExtendedMessage).

extended(Aspect, Object, Message) :-
  functor(Message,Name,Arity),
  aop:extension(Aspect, Object, Name/Arity),
  !.

before(Object, Message) :-
  trigger_method_events(Object, before, Message),
  invoke_method_actions(Object, before, Message).

find_method(Aspect, Object, Message, Module, ExtendedMessage) :-
  extend([Aspect, Object], Message, ExtendedMessage),
  functor(ExtendedMessage, Name, Arity),
  find_predicate(Aspect, Module, Name, Arity).

:- table find_predicate/4.
find_predicate(Aspect, Module, Name, Arity) :-
  current_enabled_aspect(Aspect, Module),
  % check its a viable predicate -- if not, will
  % likely backtrack into assuming a built-in predicate
  current_predicate(Module:Name/Arity).


after(Object, Message) :-
  trigger_method_events(Object, after, Message),
  invoke_method_actions(Object, after, Message).

trigger_method_events(Object, EventType, Message) :-
  % TODO put this on a background worker pool, so that its all async
  % from each other and from actions
  findall( 
    [Aspect, Listener, Object, Message], 
    ( 
      current_enabled_aspect(Aspect),
      % aop:on(Aspect, Listener, EventType, Object, Message) 
      Event = aop:on(Aspect, Listener, EventType, Object, Message) ,
      clause(Event, _),
      post_aspect_event(Aspect, Event)
    ), 
    _
    ),
    !.

invoke_method_actions(Object, EventType, Message) :- 
  findall( 
    [Aspect, Listener, Object, Message], 
    (
      current_enabled_aspect(Aspect),
      aop:at(Aspect, Listener, EventType, Object, Message)
    ), 
    _
    ),
    !.

start_aspect_event_dispatcher(Aspect) :-
  aop:aspect_event_queue(Aspect, Queue, Thread),
  (aop:aspect_events_started(Aspect)
    -> true
    ; (
      message_queue_create(_Queue, [alias(Queue)]),
      thread_create(
        dispatch_aspect_events(Aspect, Queue), 
        _Id, 
        [alias(Thread), detatched(true)]
        ),
      assertz(aop:aspect_events_started(Aspect))        
      )
    ).  

post_aspect_event(Aspect, Event) :-
  aop:aspect_event_queue(Aspect, Queue, _Thread),
  thread_send_message(Queue, Event),
  !.

dispatch_aspect_events(Aspect, Queue) :-
  thread_get_message(Queue, Event),
  % only process events of this type
  ( Event = aop:on(Aspect, _Listener, _EventType, _Object, _Message)
    -> (
      % Only process events if aspect enabled
      current_enabled_aspect(Aspect)
        -> catch_with_backtrace(
          Event,
          Error,
          print_message(error, Error)
          )
        ; true
        )
    ; true
    ),
  dispatch_aspect_events(Aspect, Queue).
  