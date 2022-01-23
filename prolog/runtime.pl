:- module(aop_runtime,[
  op(900, xfx, ::),
  (::)/2,
  (::)/3,
  (::)/4,

  op(950, yfx, to),
  (to)/2,
  (to)/3
  ]).

:- use_module('./helpers').
:- use_module('./inspection').

% 
% Runtime support
% 

% Events -- on(Listener, EventType, Object, Message),
% where EventType is before or after
:- dynamic aop:on/5.
:- discontiguous aop:on/5.
:- multifile aop:on/5.

% Actions -- at(Listener, ActionType, Object, Message),
% where ActionType is before or after
:- dynamic aop:at/5.
:- discontiguous aop:at/5.
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
  % Inlining this expression for performance
  % extend([Aspect, Object], Message, ExtendedMessage),
  Message =.. [MethodName | Args],
  ExtendedMessage =.. [MethodName, Aspect, Object | Args],
  ( extended(Aspect, Object, Message)
    -> (
      before(Object, Message),
      % run it
      aop_rt:ExtendedMessage,
      after(Object, Message)
      )  % run it
    ; aop_rt:ExtendedMessage
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
      aop:on(Aspect, Listener, EventType, Object, Message) 
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

