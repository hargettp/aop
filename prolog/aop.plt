:- begin_tests(aop).
:- use_module(library(aop)).

test(default_aspects) :-
  forall(
    member(Aspect, [assertions, reflection]),
    current_aspect(Aspect)
    ).

test(default_enabled_aspects) :-
  forall(
    member(Aspect, [assertions, reflection]),
    current_enabled_aspect(Aspect)
    ).

test(aspect_search_path) :-
  user:file_search_path(aspect,'./aspects'),
  !.

test(load_aspect_test) :-
  use_aspect(aspect_test),
  current_aspect(aspect_test),
  current_enabled_aspect(aspect_test).


test(invoke_try_me) :-
  call_cleanup( 
    ( 
      object_test::try_me(assertz(we_did_it)),
      aspect_test:we_did_it
      ), 
    retractall(aspect_test:we_did_it)
    ).
  
:- end_tests(aop).
