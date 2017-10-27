% :- module(typed_parser, [parse/2]).
:- use_module(library(dcg/basics)).

:- [syntax].

is_reserved(I) :- memberchk(I, ["false", "true", "->", ".", "let", "rec", "=", "fix", "+", "-", "*", "==", "λ", "if", "then", "else"]).

% Matches the parser P surrounded by parens
parens(P) --> "(", P, ")".

% Matches some "symbol" that begins with a letter and is followed by any alphanumeric (e.g. a, x1)
symbol([A|As]) --> [A], { char_type(A, alpha) }, symbolr(As).
symbolr([A|As]) --> [A], { char_type(A, alnum) }, !, symbolr(As).
symbolr([]) --> [].

% Matches a non-reserved identifier
identifier(X) --> symbol(A), { atom_chars(X, A), \+ is_reserved(A) }.

% Matches a list of non-reserved identifiers
identifiers([X|Xs]) --> identifier(X), white, identifiers(Xs), !.
identifiers([X]) --> identifier(X).

% Matches a variable identifier
variable(var(X)) --> identifier(X).

% Matches a lambda expression
lambda(lam(var(X), E)) --> "λ", identifiers([X|Xs]), ".", white, lambda_r(Xs, E).

% When given a list of arguments, this parses an expression and then curries it.
lambda_r([X|Xs], lam(var(X), E)) --> lambda_r(Xs, E), !.
lambda_r([], E) --> expression(E).

% Matches either a boolean or numeric literal
literal(lint(N)) --> digit(H), digits(T), { number_chars(N, [H|T]) }.
literal(lbool(false)) --> "false".
literal(lbool(true)) --> "true".

% Matches let ... in ... expressions
let_in(let(V, E1, E2)) --> "let", white, variable(V), white, "=", white, expression(E1), white, "in", white, expression(E2).
let_rec_in(let(V, E1, E2)) --> "let rec", white, variable(V), white, "=", white, expression(E1), white, "in", white, expression(E2).

% Matches our primitive infix operators
infix_op(op(bplus, E1, E2)) --> expression(E1), white, "+", white, expression(E2), !.
infix_op(op(bsub, E1, E2)) --> expression(E1), white, "-", white, expression(E2), !.
infix_op(op(bmul, E1, E2)) --> expression(E1), white, "*", white, expression(E2), !.
infix_op(op(beq, E1, E2)) --> expression(E1), white, "==", white, expression(E2), !.

% Matches an if-then-else statement
if_else(ite(C, E1, E2)) --> "if", white, expression(C), white, "then", white, expression(E1), white, "else", white, expression(E2).

% Matches a term in an expression
term(E) --> lambda(E), !.
term(E) --> literal(E), !.
term(E) --> variable(E), !.
term(E) --> parens(let_in(E)), !.
term(E) --> parens(let_rec_in(E)), !.
term(E) --> parens(if_else(E)), !.
term(E) --> parens(expression(E)).
term(E) --> parens(infix_op(E)).

% Matches a full expression, which is the application of 2 terms, or a lone term
expression(app(E1, E2)) --> term(E1), white, "$", white, term(E2).
expression(E) --> term(E).

% Matches a comment
comment --> "--", string(_), blanks_to_nl.

% Matches a top-level declaration
declaration(V:E1) --> "let", white, variable(V), white, "=", white, lambda_r([], E1), blanks_to_nl.
declaration(var(V):fix(E1)) --> "let rec", white, variable(var(V)), white, "=", white, lambda_r([V], E1), blanks_to_nl.

% Matches a list top-level declarations
declarations([D|Ds]) --> declaration(D), declarations(Ds), !.
declarations(Ds) --> comment, declarations(Ds).
declarations([]) --> [].



run_parse(D, E) --> declarations(D), expression(E), blanks_to_nl, !.
run_parse(_, _) --> { throw(parseFailed) }.

