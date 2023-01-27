:- begin_tests(aop).
:- use_module(library(aop)).

test(default_aspects) :-
  findall(
    Aspect, 
    current_aspect(Aspect),
    [assertions, reflection]
    ).

test(default_enabled_aspects) :-
  findall(
    Aspect, 
    current_enabled_aspect(Aspect),
    [assertions, reflection]
    ).

:- end_tests(aop).
