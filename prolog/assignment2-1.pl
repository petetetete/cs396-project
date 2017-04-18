% nat/1
% check if natural number
% o = 0, s(o) = 1, etc...
nat(o).
nat(s(X)) :- nat(X).

% plus/3
% add two natural numbers
% plus(X, Y, Z) where Z = X + Y
plus(o, Y, Y) :- nat(Y).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).

% list/1
% check if list
% nil = empty list, cons(X, nil) = single element, etc...
list(nil).
list(cons(_X, L)) :- list(L).

% len/2
% find length of list
% len(X, Y) where Y = the length of Z
len(nil, o).
len(cons(_X, L), s(Y)) :- len(L, Y).

% all_nat/1
% check if all elements of a list are natural numbers
% all_nat(cons(X, L)) true if X is a natural number, and so on
all_nat(nil).
all_nat(cons(X, L)) :- nat(X), all_nat(L).
