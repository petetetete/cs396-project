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


% infer/3
infer(A, F, assumption) :- member_(F, A).

% rules
% conjunction rules
infer(A, con(F, G), con_intro(_)) :- infer(A, F), infer(A, G). 
infer(A, H, con_elim_left(_)) :- member_(con(F, G), A), remove(con(F, G), A, X), infer(cons(F, X), H).
infer(A, H, con_elim_right(_)) :- member_(con(F, G), A), remove(con(F, G), A, X), infer(cons(G, X), H).

% implies rules
infer(A, imp(F, G), imp_intro(_)) :- infer(cons(F, A), G). 
infer(A, H, imp_elim(_)) :- member_(imp(F, G), A), remove(imp(F, G), A, X), infer(X, F), infer(cons(G, X), H).

% disjunction rules
infer(A, dis(F, _), dis_intro_left(_)) :- infer(A, F).
infer(A, dis(_, G), dis_intro_right(_)) :-  infer(A, G). 
infer(A, H, dis_elim(_)) :- member_(dis(F, G), A), remove(dis(F, G), A, X), infer(cons(F, X), H), infer(cons(G, X), H).



% infer/3
% automatic theorem prover using
% propositional logic sequents

% axioms
infer(A, F, assumption) :- member_(F, A).

% rules
% conjunction rules
infer(A, con(F, G), con_intro(P)) :- infer(A, F, P), infer(A, G, P). 
infer(A, H, con_elim_left(P)) :- member_(con(F, G), A), remove(con(F, G), A, X), infer(cons(F, X), H, P).
infer(A, H, con_elim_right(P)) :- member_(con(F, G), A), remove(con(F, G), A, X), infer(cons(G, X), H, P).

% implies rules
infer(A, imp(F, G), imp_intro(P)) :- infer(cons(F, A), G, P). 
infer(A, H, imp_elim(P)) :- member_(imp(F, G), A), remove(imp(F, G), A, X), infer(X, F, P), infer(cons(G, X), H, P).

% disjunction rules
infer(A, dis(F, _), dis_intro_left(P)) :- infer(A, F, P).
infer(A, dis(_, G), dis_intro_right(P)) :-  infer(A, G, P). 
infer(A, H, dis_elim(P)) :- member_(dis(F, G), A), remove(dis(F, G), A, X), infer(cons(F, X), H, P), infer(cons(G, X), H, P).