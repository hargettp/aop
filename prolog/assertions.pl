:- module(aop_assertions,[
  ]).

:- use_module('./internal').

% 
% Default methods: Assertions -- extensions of baseline
% 

% 
% asserta
% 

:- new_aspect(assertions).

  % This definition means that all objects obtain these methods
  :- in_object(_This).

    ::assert(This,Assertion) :- This::assert(Assertion, _Ref).
    ::assert(This,Assertion, Ref) :- This::assertz(Assertion, Ref).
    % Any assertion, add it in front of prior assertions
    ::asserta(This,Assertion) :- This::asserta(Assertion, _Ref).
    % Rules -- asserta(Head :- Body, Ref)
    ::asserta(This, (Head :- Body), Ref) :- 
      !,
      asserta( (aop:do(asserted, This, Head) :- Body), Ref ).

    % Facts -- asserta(Fact, Ref)
    ::asserta(This, Fact, Ref) :-
      !,
      asserta( aop:do(asserted, This, Fact), Ref ).

    ::assertz(This,Assertion) :- This::assertz(Assertion, _Ref).

    % Rules -- assertz(Head :- Body)
    ::assertz(This, (Head :- Body), Ref) :- 
      !,
      assertz( (aop:do(asserted, This, Head) :- Body), Ref ).

    % Facts -- assertz(Fact, Ref)
    ::assertz(This, Fact, Ref) :-
      !,
      assertz( aop:do(asserted, This, Fact), Ref ).

    ::retract(This, Head :- Body) :-
      !,
      retract(aop:do(asserted, This, Head) :- Body).

    ::retract(This, Body) :-
      !,
      retract(aop:do(asserted, This, Body)).

    ::retractall(This, Head :- Body) :-
      !,
      retractall(aop:do(asserted, This, Head) :- Body).

    ::retractall(This, Body) :-
      !,
      retractall(aop:do(asserted, This, Body)).

    :- end_object.

:- end_aspect.
