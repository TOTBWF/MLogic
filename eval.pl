:- module(eval, [run_parse/4, run_infer/4, pretty_infer/4, run_eval/3]).

:- [infer].
:- [parser].
:- [syntax].
:- [types].

% A closure consists of a variable, expression, and scope
closure(var(_), E, _) :- expr(E).

eval(_, lint(N), N).
eval(_, lbool(B), B).
eval(Env, var(X), V) :- member(var(X):E, Env), expr(E), eval(Env, E, V).
eval(Env, var(X), V) :- member(var(X):V, Env), \+ expr(V). 
eval(Env, lam(X, E), closure(X, E, Env)).
eval(Env, app(E1, fix(E2)), V) :- eval(Env, E1, X), apply_clo(X, fix(E2), V), !.
eval(Env, app(E1, E2), V) :- eval(Env, E1, X), eval(Env, E2, Y), apply_clo(X, Y, V).
eval(Env, let(X, E1, E2), V) :- eval(Env, E1, V1), eval([X:V1|Env], E2, V).
eval(Env, fix(E), V) :- eval(Env, app(E, fix(E)), V).
eval(Env, op(bplus, E1, E2), V) :- eval(Env, E1, V1), eval(Env, E2, V2), V is V1 + V2.
eval(Env, op(bsub, E1, E2), V) :- eval(Env, E1, V1), eval(Env, E2, V2), V is V1 - V2.
eval(Env, op(bmul, E1, E2), V) :- eval(Env, E1, V1), eval(Env, E2, V2), V is V1 * V2.
eval(Env, op(beq, E1, E2), true) :- eval(Env, E1, V1), eval(Env, E2, V2), V1 = V2.
eval(Env, op(beq, E1, E2), false) :- eval(Env, E1, V1), eval(Env, E2, V2), V1 \= V2.
eval(Env, ite(C, E, _), V) :- eval(Env, C, true), eval(Env, E, V).
eval(Env, ite(C, _, E), V) :- eval(Env, C, false), eval(Env, E, V).

apply_clo(closure(X, E, Scope), V, V1) :- eval([X:V|Scope], E, V1).
apply_clo(closure(X, E, Scope), fix(E), V) :- eval([X:fix(E)|Scope], E, V).

% Runs an expression when provided a set of declarations
run_eval(D, E, V) :- eval(D, E, V). 