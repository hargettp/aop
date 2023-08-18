:- module(aspect_test, []).

:- use_module(library(aop)).

:- new_aspect(aspect_test).

  :- new_object(object_test,[]).

    try_me(Callback) :-
      call(Callback).

  :- end_object.

:- end_aspect.