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
value(X, L, V) :- get(X, L, V).
value(dis(X, Y), L, V) :- value(X, L, V); value(Y, L, V).
