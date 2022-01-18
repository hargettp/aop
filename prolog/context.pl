:- module(aop_context,[
  aop_push_active_aspect/1,
  aop_pop_active_aspect/0,
  aop_push_active_object/1,
  aop_pop_active_object/0,
  aop_push_active_variables/1,
  aop_pop_active_variables/0,
  aop_load_context/2
  ]).

aop_push_context(Type, Value) :-
  % format("Pushing ~w~n",[Type]),
  ( nb_current(Type, History) 
    -> nb_setval(Type, [Value| History])
    ; nb_setval(Type, [Value])
    ).

aop_pop_context(Type, Value) :-
  % format("Popping ~w~n",[Type]),
  ( nb_current(Type, History) 
    -> (
      History = [Value | MoreHistory],
      nb_setval(Type, MoreHistory)
      )
    ; true % format("No history for ~w~n",[Type])
    ).

aop_pop_context(Type) :-
  aop_pop_context(Type, _).

aop_current_context(Type, Value) :-
  nb_current(Type, History) -> History = [ Value | _].

aop_push_active_aspect(Aspect) :-
  aop_push_context(aop_aspect_context, Aspect).
% 
aop_pop_active_aspect :-
  aop_pop_context(aop_aspect_context).

aop_push_active_object(Object) :-
  % format("Pushing object ~w~n",[Object]),
  aop_push_context(aop_object_context, Object).

aop_pop_active_object :-
  aop_pop_context(aop_object_context).

aop_push_active_variables(Variables) :-
  aop_push_context(aop_variables_context, Variables).
% 
aop_pop_active_variables :-
  aop_pop_context(aop_variables_context).

aop_load_context(aspect, Context) :-
  aop_current_context(aop_aspect_context, Context).

aop_load_context(object, Context) :- 
  aop_current_context(aop_object_context, Context).

aop_load_context(variables, Context) :- 
  aop_current_context(aop_variables_context, Context).
