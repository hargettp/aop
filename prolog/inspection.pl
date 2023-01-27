:- module(aop_inspection,[

  enable_aspect/1,
  disable_aspect/1,

  enable_extension/3,
  disable_extension/3,

  current_enabled_aspect/1,
  current_enabled_aspect/2,
  current_disabled_aspect/1,
  current_aspect/1,
  current_object/1,
  current_method/1
  ]).

% 
% Essential definitions for objects,  actions, methods, events
% 

% aspect(Name)
:- dynamic(aop:aspect/1).
:- multifile(aop:aspect/1).

% aspect_module(Name,Module)
:- dynamic(aop:aspect_module/2).
:- multifile(aop:aspect_module/2).

:- dynamic(aop:aspect_enabled/2).
:- multifile(aop:aspect_enabled/2).

% object(Aspect,Object,Module) -- used to record objects
:- dynamic(aop:object/3).
:- multifile(aop:object/3).

% augmented(Aspect,Object,Module) -- used to record augmentation to objects
:- dynamic(aop:augmented/3).
:- multifile(aop:augmented/3).

% method(Aspect, Object, Message)
:- dynamic(aop:method/3).
:- multifile(aop:method/3).
:- multifile(aop:method/3).

% method_signature(Aspect, Object, Name/MethodArity, Signature, VariableNames)
:- dynamic(aop:method_signature/5).
:- multifile(aop:method_signature/5).
:- multifile(aop:method_signature/5).

% extended(Aspect, Object, Name/MethodArity)
:- dynamic(aop:extension/3).
:- multifile(aop:extension/3).
:- multifile(aop:extension/3).

% Enable extensions (e.g., events and actions)
enable_extension(Aspect, Object, Name/Arity) :-
  aop:extension(Aspect, Object, Name/Arity)
  -> true
  ; assertz(aop:extension(Aspect, Object, Name/Arity)).

% Disable extensions (e.g., events and actions)
disable_extension(Aspect, Object, Name/Arity) :-
  retractall(aop:extension(Aspect, Object, Name/Arity)).

% 
% Toggle aspect status -- disabled aspects do not execute
% 
enable_aspect(Aspect) :-
    retractall(aop:aspect_enabled(Aspect,_)),
    assertz(aop:aspect_enabled(Aspect, true)).

disable_aspect(Aspect) :-
    retractall(aop:aspect_enabled(Aspect,_)),
    assertz(aop:aspect_enabled(Aspect, false)).

% 
% Enumerators
% 
current_enabled_aspect(Aspect) :-
  current_aspect(Aspect),
  aop:aspect_enabled(Aspect,true).

current_enabled_aspect(Aspect, Module) :-
  current_enabled_aspect(Aspect),
  aop:aspect_module(Aspect, Module),
  aop:aspect_enabled(Aspect,true).

current_disabled_aspect(Aspect) :-
  current_aspect(Aspect),
  aop:aspect_enabled(Aspect,false).

current_aspect(Aspect) :-
  aop:aspect(Aspect).

current_object(Def) :-
  aop:object(Aspect, Object, Module),
  Def = aop:object(Aspect, Object, Module).

current_method(Def) :-
  aop:method(Aspect, Object, Message),
  Def = aop:method(Aspect, Object, Message).
