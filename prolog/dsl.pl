:- module(aop_dsl, [  
  
  new_aspect/1,
  in_aspect/1,
  end_aspect/0,

  new_object/1,
  new_object/2,
  in_object/1,
  end_object/0,

  term_expansion/2,
  goal_expansion/2,

  op(650, fx, ::),
  (::)/1,

  op(1100, fx, method),
  (method)/1

  ]).

:- use_module(library(apply)).

:- use_module('./context').
:- use_module('./helpers').
:- use_module('./runtime').

:- dynamic new_aspect/1.
:- dynamic in_aspect/1.
:- dynamic new_object/1.
:- dynamic new_object/2.

% Used for declaring methods that take a reference
% to the receiver as the first or "this" parameter
:- dynamic (::)/1.

% For declaring unimplemented methods in an object
:- dynamic (method)/1.

end_aspect :-
  aop_pop_active_aspect.

in_object(Object) :-
  aop_push_active_object(Object).

end_object :-
  aop_pop_active_object.

% - - - - - - - - - - - - - - - - - - -
% 
% Term expansion
% 
% - - - - - - - - - - - - - - - - - - -

term_expansion(:- new_aspect(Aspect),[
  aop:aspect(Aspect),
  aop:aspect_enabled(Aspect,true) | InAspectExpansions
  ]) :-
  term_expansion(:- in_aspect(Aspect), InAspectExpansions).

term_expansion(:- in_aspect(Aspect), [
  aop:aspect_module(Aspect, Module)
  ]) :-
  prolog_load_context(module, Module),
  aop_push_active_aspect(Aspect).

term_expansion(:- new_object(Object), Expansion) :- 
  term_expansion(:- new_object(Object,[]), Expansion).

term_expansion(:- new_object(Object, Accessors), Expansion) :-
  aop_load_context(aspect, Aspect),
  prolog_load_context(module, Module),
  ObjectExpansion = aop:object(Aspect, Object, Module),
  expand_accessors(Aspect, Object, Accessors, AccessorExpansions),
  term_expansion(:- in_object(Object), InObjectExpansion),
  append([ObjectExpansion | AccessorExpansions], InObjectExpansion, Expansion).

term_expansion(:- in_object(Object), Expansion) :-
  aop_load_context(aspect, Aspect),
  prolog_load_context(module, Module),
  Expansion = [
    aop:augmented(Aspect, Object, Module)
  ],
  aop_push_active_object(Object).

term_expansion(:- nested_object(Object), [ObjectExpansion]) :-
  aop_load_context(object, Current),
  aop_load_context(aspect, Aspect),
  prolog_load_context(module, Module),
  Object =.. [Kind | Args],
  NestedObject =.. [Kind, Current | Args],
  ObjectExpansion = aop:object(Aspect, NestedObject, Module),
  aop_push_active_object(NestedObject).

term_expansion(:- extension(Name/MethodArity), Expansion ) :-
    aop_load_context(object, Object),
    aop_load_context(aspect, Aspect),
    ( aop:extension(Aspect, Object, Name/MethodArity)  
      -> Expansion = []
      ; Expansion = [aop:extension(Aspect, Object, Name/MethodArity)]
      ).

term_expansion(:- method(Name/MethodArity), Expansion) :-
    aop_load_context(object, Object),
    aop_load_context(aspect, Aspect),
    PredicateArity is MethodArity + 2,
    prolog_load_context(module, Module),
    method_expansion(Aspect, Object, Module:Name/MethodArity, PredicateArity, Expansion).

% Events -- ::on(This, EventType, Object, Message) :- baz.
% expand_object_declaration(Aspect, This, (::on(This,EventType,Object,Message) :- Body), [
term_expansion(::on(This,EventType,Object,Message) :- Body, [
    (aop:on(Aspect, This, EventType, Object, Message) :- Body)
    ]) :-
  aop_load_context(object, This),
  aop_load_context(aspect, Aspect).

% Events -- ::at(This, EventType, Object, Message) :- baz.
% expand_object_declaration(Aspect, This, (::at(This,EventType,Object,Message) :- Body), [
term_expansion(::at(This,EventType,Object,Message) :- Body, [
    (aop:at(Aspect, This, EventType, Object, Message) :- Body)
    ]) :-
  aop_load_context(object, This),
  aop_load_context(aspect, Aspect).

% Rule -- ::foo(This, bar) :- baz.
% expand_object_declaration(Aspect, Object, (::Message :- Body), [
term_expansion(::Message :- Body, [
    aop:do(Aspect, Object, ContractedMessage) :- ExtendedBody |
    MethodExpansion
    ]) :-
  aop_load_context(object, Object),
  aop_load_context(aspect, Aspect),
  contract(Message, ContractedMessage),
  functor(Message, Name, Arity),
  PredicateArity is Arity + 1,
  MethodArity is Arity - 1,
  % We need to check "this" against the object, to ensure
  % there is a match for the method
  arg(1,Message,This),
  prolog_load_context(module, Module),
  ExtendedBody = (This = Object, Body),
  method_expansion(Aspect, Object, Module:Name/MethodArity, PredicateArity, MethodExpansion).

% Fact -- ::foo(This, bar)
term_expansion(::Message, [
    aop:do(Aspect, Object, ContractedMessage) :- ExtendedBody |
    MethodExpansion
    ]) :-
  aop_load_context(object, Object),
  aop_load_context(aspect, Aspect),
  contract(Message, ContractedMessage),
  functor(Message, Name, Arity),
  PredicateArity is Arity + 1,
  MethodArity is Arity - 1,
  % We need to check "this" against the object, to ensure
  % there is a match for the method
  arg(1,Message,This),
  prolog_load_context(module, Module),
  ExtendedBody = (This = Object),
  method_expansion(Aspect, Object, Module:Name/MethodArity, PredicateArity, MethodExpansion).

% Rule -- foo(bar) :- baz.
% expand_object_declaration(Aspect, Object, (Message :- Body), [
term_expansion((Message :- Body), [
    aop:do(Aspect, Object, Message) :- Body |
    MethodExpansion
    ]) :-
  aop_load_context(object, Object),
  aop_load_context(aspect, Aspect),
  functor(Message,Name, MethodArity),
  PredicateArity is MethodArity + 2,
  prolog_load_context(module, Module),
  method_expansion(Aspect, Object, Module:Name/MethodArity, PredicateArity, MethodExpansion).

% Fact -- foo(bar)
% expand_object_declaration(Aspect, Object, Message, [
term_expansion(Message, [
    aop:do(Aspect, Object, Message) |
    MethodExpansion
    ]) :-
  aop_load_context(object, Object),
  ( Message = begin_of_file -> fail ; true ),
  ( Message = end_of_file -> fail ; true),
  ( (functor(Message,Op,1),  current_op(_,_,Op) ) -> fail ; true),
  aop_load_context(aspect, Aspect),
  functor(Message,Name,MethodArity),
  PredicateArity is MethodArity + 2,
  prolog_load_context(module, Module),
  method_expansion(Aspect, Object, Module:Name/MethodArity, PredicateArity,  MethodExpansion).

expand_accessors(_Aspect, _Object, [],[]).

expand_accessors(Aspect, Object, [Accessor | Accessors], [
   aop:do(Aspect, Object, Accessor),
   MethodExpansion | AccessorExpansions
   ]) :-
  functor(Accessor,Name,MethodArity),
  PredicateArity is MethodArity + 2,
  method_expansion(Aspect, Object, Module:Name/MethodArity, PredicateArity, MethodExpansion),
  prolog_load_context(module, Module),
  expand_accessors(Aspect, Object, Accessors, AccessorExpansions).

method_expansion(Aspect, Object, Module:Name/MethodArity, PredicateArity, MethodExpansion) :-
  aop:method(Aspect, Object, Module:Name/MethodArity)
    -> MethodExpansion = []
    ; (
      method_signature(Name, Signature, VariableNames),
      MethodExpansion = [
        :- dynamic Name/PredicateArity,
        :- multifile(Name/PredicateArity),
        :- discontiguous(Name/PredicateArity),
        aop:method(Aspect, Object, Module:Name/MethodArity),
        aop:method_signature(Aspect, Object, Module:Name/MethodArity, Signature, VariableNames)
        ]
        ).

method_signature(Method, Signature, Names) :-
  prolog_load_context(variable_names, Variables),
  findall(Name, member(Name=_Var, Variables), Names),
  MethodCall =.. [Method| Names],
  with_output_to(
    string(SignatureString),
    write_term(MethodCall,[])
    ),
  atom_string(Signature, SignatureString).

% 
% Goal expansion
% 

% The idiom `::here(Here)` where `Here` can be any variable name
% will bind the current aspect to that variable
goal_expansion(::here(Here), Aspect = Here) :-
  % This has to be done at compile time because the
  % context isn't availale at run time
  aop_load_context(aspect, Aspect).

% Support messages to `This` object without
% explicitly needing a `This` variable
goal_expansion(::Message, Object::ExpandedMessage) :-
  aop_load_context(object, Object),
  expand_goal(Message, ExpandedMessage).

% Ensure that even nested terms are properly
% expanded as well
goal_expansion(Goal, ExpandedGoal) :-
  aop_load_context(object, _Object),
  Goal =.. [Name | Args],
  maplist(expand_goal, Args, ExpandedArgs),
  ExpandedGoal =.. [Name | ExpandedArgs].