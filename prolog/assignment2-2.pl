% member/2
% is true if and only if an element is
% in a list
member(X, cons(X, _Z)).
member(X, cons(_Y, Z)) :- member(X, Z).

% get/3
% is true if and only if an element in L
% matches the pair(K, V)
get(K, L, V) :- member(pair(K, V), L).

% value/3
value(F, L, V) :- get(F, L, V).
value(con(X, Y), L, true) :- value(X, L, true), value(Y, L, true).
value(con(X, Y), L, false).
value(dis(X, Y), L, true) :- value(X, L, true); value(Y, L, true).
value(dis(X, Y), L, false).
value(imp(X, Y), L, true) :- value(X, L, false); value(Y, L, true).
value(imp(X, Y), L, false).
