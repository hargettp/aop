:- module(aop_reflection,[
  ]).

:- use_module('./internal').
:- use_module('./docs').

:- new_aspect(reflection).

  :- new_object(aop:aspect(Aspect),[
    name(Aspect)
    ]).

    ::defined_object(This, Object) :-
      current_object(Object),
      This::name(Aspect),
      Object = aop:object(Aspect, _, _).

    augmented_object(Object) :-
      ::name(Aspect),
      Object = aop:augmented(Aspect, _, _),
      Object.

    ::doc_comments(This, Docs) :-
      This::name(Aspect),
      aop:aspect_comment(Aspect, _, Docs).

  :- end_object.

  :- new_object(aop:object(Aspect, Object, Module), [
    aspect(Aspect),
    object(Object),
    original_module(Module)
    ]).

    ::doc_comments(This, Docs) :-
      This::aspect(Aspect),
      This::object(Object),
      aop:object_comment(Aspect, Object, _, Docs).

  :- end_object.

  :- new_object(aop:augmented(Aspect, Object, Module), [
    aspect(Aspect),
    object(Object),
    augmenting_module(Module)
    ]).

  :- end_object.

  :- in_object(_Any).

    ::this(This, This).

    % Return the aspect where the object is defined
    ::where(This, Aspect) :-
      aop:object(Aspect, This, _Module).

    ::defining_object(This, aop:object(Aspect, This, Module)) :-
      aop:object(Aspect, This, Module).

    ::method(This, Method) :-
      This::method(_Aspect, Method).

    ::method(This, Aspect, Method) :-
      current_enabled_aspect(Aspect),
      current_method(Method),
      Method::object(This),
      Method::aspect(Aspect).

    ::clause(This, Clause) :-
      This::predicate(Name/Arity),
      \+member(Name, [clause, method]),
      ExtendedArity is Arity + 2,
      current_predicate(Module:Name/ExtendedArity),
      functor(Head, Name, ExtendedArity),
      Module:clause(Head, Body),
      arg(2,Head, Object),
      Object =@= This,
      ( Body = true -> Clause = Head ; Clause = ( Head :- Body)).

    ::perform(This, Message) :-
      This::Message.

    ::apply(This, Partial, Args) :-
      Partial =.. PartialMessage,
      append(PartialMessage, Args, Parts),
      Message =.. Parts,
      This::Message.

    % Returns a nested term if term is defined
    % on the resceiver
    ::nested(This, Term, Nested) :-
      This::Term,
      This::nest(Term, Nested).

    ::nest(This, Term, Nested) :-
      extend([This],Term, Nested).

    ::ground(This) :-
      ground(This).

    listing :-
      ::listing(_).

    listing(MethodPattern) :-
      compound(MethodPattern),
      !,
      ::this(This),
      findall(
        Clause,
        (
          % clause(aop:perform_method(This, Method), Body),
          % MethodPattern = Method,
          % Clause = (Method :- Body)
          MethodPattern =.. [Name | Args],
          Predicate =.. [Name, _Aspect, This | Args],
          clause(aop_rt:Predicate, Body),
          Method =.. [Name | Args],
          Clause = (This::Method :- Body)
          ),
        Clauses
        ),
      ::portray_method_clauses(Clauses).

    listing(MethodName) :-
      ::this(This),
      findall(
        Clause,
        (
          current_predicate(aop_rt:MethodName/PredicateArity),
          MethodArity is PredicateArity - 2,
          MethodArity > 0,
          length(Args, MethodArity),
          Predicate =.. [MethodName, _Aspect, This | Args],
          clause(aop_rt:Predicate, Body),
          Method =.. [MethodName | Args],
          Clause = (This::Method :- Body)
          ),
        Clauses
        ),
      ::portray_method_clauses(Clauses).

    portray_method_clauses(Clauses) :-
      forall(
        member(Clause, Clauses),
        portray_clause(Clause)
        ).    

  :- end_object.

  :- new_object(aop:method(Aspect, Object, Module:Name/Arity),[
    aspect(Aspect),
    object(Object),
    name(Name),
    arity(Arity),
    declaring_module(Module),
    predicate(Name/Arity)
  ]).

    ::doc_comments(This, Doc) :-
      This::aspect(Aspect),
      This::object(Object),
      This::predicate(Predicate),
      aop:method_comment(Aspect, Object, Predicate, _, Doc ).

  :- end_object.
:- end_aspect.
