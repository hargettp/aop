:- module(aop_helpers,[
  extend/3,
  contract/2
  ]).

% 
% General helpers
% 

% Add Context to a term as the first argument, e.g., 
% Term(Args) becomes Term(Context,Args...)
extend(Context,Term,Extended) :-
  is_list(Context),
  Term =.. [Functor|Args],
  append(Context, Args, ExtendedArgs),
  Extended =.. [Functor | ExtendedArgs],
  !.

% Add Context to a term as the first argument, e.g., 
% Term(Args) becomes Term(Context,Args...)
extend(Context,Term,Extended) :-
  Term =.. [Functor|Args],
  Extended =.. [Functor, Context | Args].

contract(Term, Contracted) :-
  Term =.. [Functor|Args],
  Args = [_Drop|ContractedArgs],
  Contracted =.. [Functor|ContractedArgs].