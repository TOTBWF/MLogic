:- [parser].
:- [syntax].
:- [types].

% Used to generate new names for type variables
geometric(R, K, O) :- K > 0, !, K1 is K - 1, R1 is R**K, geometric(R, K1, O1), O is R1 + O1.
geometric(_, 0, 0).

% Generates types in order : a ... z, aa, ab ... az, ba ...
new_name(N, tvar(A)) :- 
    N >= 26,
    !,
    A1 is floor(log(N)/log(26)) - 1,
    geometric(26, A1, N1),
    A2 is floor(log(N - N1)/log(26)),
    new_name_r(N, A2, Cs),
    atom_chars(A, Cs).
new_name(N, tvar(A)) :- N < 26, !, new_name_r(N, 0, Cs), atom_chars(A, Cs).

new_name_r(N, A, [C|Cs]) :- 
    A > 0, !, 
    geometric(26, A, S),
    N1 is mod(div(N - S, 26**A), 26) + 97, 
    A1 is A - 1, char_code(C, N1), new_name_r(N, A1, Cs).

new_name_r(N, 0, [C]) :- N1 is mod(N, 26) + 97, char_code(C, N1).

% Generates a new type name when given a current count
fresh_type(N, NN, A) :- NN is N + 1, new_name(N, A).

% Applies the substitution [T/A] in a type
apply(_, tcons(X), tcons(X)).
apply(T/tvar(A), tvar(A), T).
apply(_/tvar(A), tvar(B), tvar(B)) :- A \= B.
apply(T/tvar(A), tarrow(T1, T2), tarrow(T3, T4)) :- apply(T/tvar(A), T1, T3), apply(T/tvar(A), T2, T4). 
apply(T/tvar(A), tpoly(tvar(A), T0), T1) :- apply(T/tvar(A), T0, T1).
apply(T/tvar(A), tpoly(tvar(B), T0), tpoly(tvar(B), T1)) :- A \= B, apply(T/tvar(A), T0, T1).
apply(_/tvar(_), tpoly([], T0), T0).
% It is useful to be able to apply substitutions onto the type environment
apply(T/tvar(A), [X:T0|Cs0], [X:T1|Cs]) :- apply(T/tvar(A), T0, T1), apply(T/tvar(A), Cs0, Cs).
apply(_/tvar(_), [], []).
apply([], T, T).
apply([S|Ss], T1, T) :- apply(S, T1, T2), apply(Ss, T2, T).

% Compose together substitutions
compose(S1, S2, S) :- compose_r(S1, S2, S3), union(S3, S1, S). 
compose(S1, S2, S3, S) :- compose(S1, S2, S4), compose(S4, S3, S).
compose(S1, S2, S3, S4, S) :- compose(S1, S2, S3, S5), compose(S5, S4, S).
compose(S1, S2, S3, S4, S5, S) :- compose(S1, S2, S3, S4, S6), compose(S6, S5, S).

compose_r(S, [], S).
compose_r(S1, [T1/tvar(A)|S2], [T/tvar(A)|S]) :- apply(S1, T1, T), compose_r(S1, S2, S).

% Get the set of free type variables in a type
ftv(tcons(_), []).
ftv(tvar(A), [tvar(A)]).
ftv(tarrow(T1, T2), F) :- ftv(T1, F1), ftv(T2, F2), union(F1, F2, F).
ftv(tpoly(tvar(A), T), F) :- ftv(T, F1), subtract(F1, [tvar(A)], F).
% It is useful to be able to get the free type variables of an environment
ftv([], []).
ftv([_:T|Ts], F) :- ftv(T, F1), ftv(Ts, F2), union(F1, F2, F).

% Binds a type variable to a type
bind(tvar(A), tvar(A), []) :- !.
bind(tvar(A), T, [T/tvar(A)]) :- ftv(T, F), \+ member(tvar(A), F), !.
bind(tvar(A), T) :- throw(infiniteType(tvar(A), T)).

% Finds the most general unifier (substitution set) for 2 types
unify(tcons(X), tcons(X), []) :- !.
unify(tvar(A), T, S) :- bind(tvar(A), T, S), !.
unify(T, tvar(A), S) :- bind(tvar(A), T, S), !.
unify(tarrow(L1, R1), tarrow(L2, R2), S) :- 
    unify(L1, L2, SL), 
    apply(SL, R1, R1S), 
    apply(SL, R2, R2S), 
    unify(R1S, R2S, SR),
    compose(SL, SR, S), !. 
unify(T1, T2, _) :- 
    pretty_print(T1, S1),
    pretty_print(T2, S2),
    throw(unifcationError(S1, S2)).

% Replaces all bound type variables with fresh ones
instantiate(N, tpoly(Tv1, T), NN, T2) :- fresh_type(N, N1, Tv2), apply(Tv2/Tv1, T, T1), instantiate(N1, T1, NN, T2).
instantiate(N, T, N, T) :- T \= tpoly(_, _).

% Transforms a type into a polytype over type variables which are free in the type but not free in the context
generalize_r([], T, T).
generalize_r([Tvar|Tvars], T, tpoly(Tvar, T0)) :- generalize_r(Tvars, T, T0).
generalize(C, T, T0) :- ftv(C, FC), ftv(T, FT), subtract(FT, FC, F), generalize_r(F, T, T0).

% Encodes the types of our binary operations
binop_type(bplus, tarrow(tcons(int), tarrow(tcons(int), tcons(int)))).
binop_type(bsub, tarrow(tcons(int), tarrow(tcons(int), tcons(int)))).
binop_type(bmul, tarrow(tcons(int), tarrow(tcons(int), tcons(int)))).
binop_type(beq, tarrow(tcons(int), tarrow(tcons(int), tcons(bool)))).

% Infers the most general type for an expression
infer(_, N, lint(_), N, [], tcons(int)).
infer(_, N, lbool(_), N, [], tcons(bool)).
infer(C, N, var(X), NN, [], T) :- member(var(X):T1, C), !, instantiate(N, T1, NN, T).
infer(_, _, var(X), _, _, _) :- throw(undefinedError(var(X))).
infer(C, N, app(E1, E2), NN, S, T) :- 
    infer(C, N, E1, N1, S1, T1), 
    apply(S1, C, C1),
    infer(C1, N1, E2, N2, S2, T2),
    fresh_type(N2, NN, Tv),
    apply(S2, T1, T3),
    unify(T3, tarrow(T2, Tv), S3),
    compose(S3, S2, S1, S),
    apply(S3, Tv, T).
infer(C, N, lam(X, E), NN, S, tarrow(T2, T1)) :-
    fresh_type(N, N1, Tv),
    subtract(C, [X:_], C1), 
    infer([X:Tv|C1], N1, E, NN, S, T1),
    apply(S, Tv, T2).
infer(C, N, let(X, E1, E2), NN, S, T) :-
    infer(C, N, E1, N1, S1, T1),
    apply(S1, C, C1),
    generalize(C1, T1, T2),
    subtract(C, [X:_], C2),
    apply(S1, C2, C3),
    infer([X:T2|C3], N1, E2, NN, S2, T),
    compose(S1, S2, S).
infer(C, N, fix(E), NN, S, T) :-
    infer(C, N, E, N1, S1, T1),
    fresh_type(N1, NN, Tv),
    unify(T1, tarrow(Tv, Tv), S2),
    compose(S1, S2, S),
    apply(S, Tv, T).
infer(C, N, op(B, E1, E2), NN, S, T) :-
    infer(C, N, E1, N1, S1, T1),
    infer(C, N1, E2, N2, S2, T2),
    binop_type(B, TB),
    fresh_type(N2, NN, Tv),
    unify(tarrow(T1, tarrow(T2, Tv)), TB, S3),
    compose(S1, S2, S3, S),
    apply(S3, Tv, T).
infer(C, N, ite(E1, E2, E3), NN, S, T) :-
    infer(C, N, E1, N1, S1, T1),
    infer(C, N1, E2, N2, S2, T2),
    infer(C, N2, E3, NN, S3, T3),
    unify(T1, tcons(bool), S4),
    unify(T2, T3, S5),
    compose(S5, S4, S3, S2, S1, S),
    apply(S5, T2, T).

% After inference, the names are often quite ugly, so we normalize them so they start at 'a'
normalize_r(N, tpoly(Tv1, T1), tpoly(Tv, T)) :- fresh_type(N, NN, Tv), apply(Tv/Tv1, T1, T2), normalize_r(NN, T2, T).
normalize_r(_, T, T) :- T \= tpoly(_, _).
normalize(T, T1) :- normalize_r(0, T, T1).
    
% Infers the type of our top level declarations
infer_top(C, N, [], N, C).
infer_top(C, N, [X:E|Es], NN, C1) :-
    infer(C, N, let(X, E, X), N1, _, T),
    normalize(T, Tn),
    infer_top([X:Tn|C], N1, Es, NN, C1).

run_infer(Ds, E, Ts, T) :- infer_top([], 0, Ds, N, Ts), infer(Ts, N, E, _, _, T).

pretty_infer(Ds, E, Ss, S) :- run_infer(Ds, E, Ts, T), pretty_infer_r(Ts, Ss), pretty_print(T, S).
pretty_infer_r([], []).
pretty_infer_r([var(X):T|Ts], [S|Ss]) :- 
    atom_concat(X, ': ', Cs), 
    atom_string(Cs, Sv), 
    pretty_print(T, St), 
    string_concat(Sv, St, S),
    pretty_infer_r(Ts, Ss).

pprint(tcons(X)) --> {atom_codes(X, Cs)}, Cs.
pprint(tvar(X)) --> {atom_codes(X, Cs)}, Cs.
pprint(tarrow(T1, T2)) --> pprint(T1), " -> ", pprint(T2).
pprint(tpoly(Tv, T)) --> "∀", pprint(Tv), ".", pprint(T).
pretty_print(T, S) :- phrase(pprint(T), Cs), string_codes(S, Cs).


