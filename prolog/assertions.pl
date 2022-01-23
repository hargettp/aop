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
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Head, Extended),
      aop_rt:asserta( (Extended :- Body), Ref ).

    % Facts -- asserta(Fact, Ref)
    ::asserta(This, Fact, Ref) :-
      !,
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Fact, Extended),
      aop_rt:asserta( Extended, Ref ).

    ::assertz(This,Assertion) :- This::assertz(Assertion, _Ref).

    % Rules -- assertz(Head :- Body)
    ::assertz(This, (Head :- Body), Ref) :- 
      !,
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Head, Extended),
      aop_rt:assertz( (Extended :- Body), Ref ).

    % Facts -- assertz(Fact, Ref)
    ::assertz(This, Fact, Ref) :-
      !,
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Fact, Extended),
      aop_rt:assertz( Extended, Ref ).

    ::retract(This, Head :- Body) :-
      !,
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Head, ExtendedHead),
      aop_rt:retract(ExtendedHead :- Body).

    ::retract(This, Body) :-
      !,
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Body, ExtendedBody),
      aop_rt:retract(ExtendedBody).

    ::retractall(This, Head :- Body) :-
      !,
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Head, ExtendedHead),
      aop_rt:retractall(ExtendedHead :- Body).

    ::retractall(This, Body) :-
      !,
      aop:object(Aspect, This, _Module),
      extend([Aspect, This], Body, ExtendedBody),
      aop_rt:retractall(ExtendedBody).

    :- end_object.

:- end_aspect.
