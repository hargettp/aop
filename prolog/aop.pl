:- module(aop,[
  use_aspect/1
  ]).

% Aspects are also loaded as modules
user:file_search_path(aspect, './aspects').

user:file_search_path(aspect, Path) :-
  pack_property(_Package, directory(Directory)),
  directory_file_path(Directory, 'prolog/aspects', Path).

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
