% Need to be explicit about local libraries
user:file_search_path(library, './prolog').

:- use_module(library(aop)).

% Unit test support
:- use_module(library(plunit)).

:- set_test_options([
  run(make(all))
  ]).

:- load_test_files(_X).

:- run_tests.
