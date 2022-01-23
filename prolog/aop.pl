:- module(aop,[
  use_aspect/1
  ]).

% Aspects are also loaded as modules
user:file_search_path(aspect, './aspects').

use_aspect(Base/Rest) :-
  use_aspect(Base),
  % format("Loading aspect from module ~w~n",[aspect(Base/Rest)]),
  use_module(aspect(Base/Rest)),
  !.

use_aspect(Aspect) :-
  % format("Loading aspect from module ~w~n",[aspect(Aspect)]),
  use_module(aspect(Aspect)).

:- use_module('./helpers').
:- reexport('./inspection').
:- reexport('./runtime').
:- reexport('./dsl').

:- use_module('./docs').

% Built-in aspects
:- use_module('./assertions').
:- use_module('./reflection').

:- new_aspect(aop).

  :- in_object(_Any).

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

:- end_aspect.