:- module(aop_docs,[
  ]).

:- use_module('./internal').
:- use_module('./context').

% aop:aspect_comment(Aspect, File, Comment)
:- dynamic aop:aspect_comment/3.

% aop:object_comment(Aspect, Object, File, Comment)
:- dynamic aop:object_comment/4.

% aop:method_comment(Aspect, Object, Method, File, Comment)
:- dynamic aop:method_comment/5.

prolog:comment_hook(PosComments, TermPos, Term) :-
  prolog_load_context(file, File),
  % filter for just our project files
  working_directory(Cwd, Cwd),
  string_concat(Cwd, SourceFile, File),
  process_comments(PosComments, SourceFile, TermPos, Term).

process_comments(PosComments, SourceFile, TermPos, :- new_aspect(Aspect)) :-
  process_aspect_comments(Aspect, PosComments, SourceFile, TermPos).

process_comments(PosComments, SourceFile, TermPos, :- in_aspect(Aspect)) :-
  process_aspect_comments(Aspect, PosComments, SourceFile, TermPos).

process_comments(PosComments, SourceFile, TermPos, :- new_object(Object)) :-
  aop_load_context(aspect, Aspect),
  process_object_comments(Aspect, Object, PosComments, SourceFile, TermPos).

process_comments(PosComments, SourceFile, TermPos, :- new_object(Object,_)) :-
  aop_load_context(aspect, Aspect),
  process_object_comments(Aspect, Object, PosComments, SourceFile, TermPos).

process_comments(PosComments, SourceFile, TermPos, :- in_object(Object)) :-
  aop_load_context(aspect, Aspect),
  process_object_comments(Aspect, Object, PosComments, SourceFile, TermPos).

process_comments(PosComments, SourceFile, TermPos, :- method Name/Arity) :-
  aop_load_context(object, Object),
  aop_load_context(aspect, Aspect),
  process_method_comments(Aspect, Object, Name/Arity, PosComments, SourceFile, TermPos).

process_comments(PosComments, SourceFile, TermPos, ::Message :- Body ) :-
  process_comments(PosComments, SourceFile, TermPos, Message :- Body).

process_comments(PosComments, SourceFile, TermPos, Message :- _Body ) :-
  aop_load_context(object, Object),
  aop_load_context(aspect, Aspect),
  functor(Message, Name, Arity),
  process_method_comments(Aspect, Object, Name/Arity, PosComments, SourceFile, TermPos).

process_comments(PosComments, SourceFile, TermPos, ::Message ) :-
  process_comments(PosComments, SourceFile, TermPos, Message ).

process_comments(PosComments, SourceFile, TermPos, Message ) :-
  aop_load_context(object, Object),
  aop_load_context(aspect, Aspect),
  functor(Message, Name, Arity),
  process_method_comments(Aspect, Object, Name/Arity, PosComments, SourceFile, TermPos).


process_aspect_comments(Aspect, PosComments, SourceFile, _TermPos) :-
  collect_comments(PosComments, Comments),
  retractall(aop:aspect_comment(Aspect, SourceFile, _)),
  retractall(aop:object_comment(Aspect, _, SourceFile, _)),
  retractall(aop:method_comment(Aspect, _, _, SourceFile, _)),
  assertz(aop:aspect_comment(Aspect, SourceFile, Comments)).

process_object_comments(Aspect, Object, PosComments, SourceFile, _TermPos) :-
  collect_comments(PosComments, Comments),
  retractall(aop:object_comment(Aspect, Object, SourceFile, _)),
  retractall(aop:method_comment(Aspect, Object, _, SourceFile, _)),
  assertz(aop:object_comment(Aspect, Object, SourceFile, Comments)).

process_method_comments(Aspect, Object, Name/Arity, PosComments, SourceFile, _TermPos) :-
  collect_comments(PosComments, Comments),
  assertz(aop:method_comment(Aspect, Object, Name/Arity, SourceFile, Comments)).

% Comments are always in form of a list of Pos-Comment pairs, so just group them together
collect_comments(PosComments, Comments) :-
  findall(Comment,member(_Pos-Comment, PosComments),Comments).
