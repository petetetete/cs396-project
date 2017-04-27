:- consult('assignment2-1').

% member/2
member(X, cons(X, Z)) :- list(Z).
member(X, cons(_Y, Z)) :- member(X, Z).

% get/3
get(K, L, V) :- member(pair(K, V), L).

% value/3
value(con(X, Y), L, V) :- get(X, L, V), get(Y, L, V).
value(dis(X, Y), L, V) :- get(X, L, V); get(Y, L, V).
value(imp(X, Y), L, V) :- not(get(X, L, V)); get(Y, L, V).
