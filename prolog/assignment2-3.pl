% remove/3
% remove an element from a list
% and return the new list

remove(_, nil, nil).
remove(X, cons(X,L), L).
remove(X, cons(Y,L1), cons(Y,L2)) :- dif(X,Y), remove(X, L1, L2).


% member/2
% is true if and only if an
% element is in a list

member_(X, cons(X, _)).
member_(X, cons(_, Z)) :- member_(X, Z).


% infer/2
% automatic theorem prover using
% propositional logic sequents

% axioms
infer(A, F) :- member_(F, A).

% rules
% conjunction rules
infer(A, con(F, G)) :- infer(A, F), infer(A, G). 
infer(A, H) :- member_(con(F, G), A), remove(con(F, G), A, X), ( infer(cons(F, X), H); infer(cons(G, X), H) ).

% implies rules
infer(A, imp(F, G)) :- infer(cons(F, A), G). 
infer(A, H) :- member_(imp(F, G), A), remove(imp(F, G), A, X), infer(X, F), infer(cons(G, X), H).

% disjunction rules
infer(A, dis(F, G)) :- infer(A, F); infer(A, G). 
infer(A, H) :- member_(dis(F, G), A), remove(dis(F, G), A, X), infer(cons(F, X), H), infer(cons(G, X), H).
