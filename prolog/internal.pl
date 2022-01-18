% This module is useful for internal aspects that
% need to leverage the same public interface, but still be
% exported from the whole package
:- module(aop_internal,[]).

:- reexport('./helpers').
:- reexport('./inspection').
:- reexport('./runtime').
:- reexport('./dsl').
