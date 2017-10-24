% Our base literals
lit(lint(N)) :- number(N).
lit(lbool(true)).
lit(lbool(false)).

binop(bplus). % Addition
binop(bsub). % Subtraction
binop(bmul). % Multiplication
binop(beq). % Equality

% Core Lambda Calculus
expr(var(X)) :- atom(X). % Variables
expr(app(E1, E2)) :- expr(E1), expr(E2). % Application
expr(lam(var(_), E)) :- expr(E). % Lambda abstractions

% Extensions
expr(L) :- lit(L). % Literals
expr(let(var(_), E1, E2)) :- expr(E1), expr(E2). % Let Binding
expr(fix(E)) :- expr(E). % Fixed point 

% These aren't necessary, but are useful
expr(op(B, E1, E2)) :- binop(B), expr(E1), expr(E2). % Builtin binary operators
expr(ite(C, E1, E2)) :- expr(C), expr(E1), expr(E2). % If Expression

% Top level declarations
decl(N, E) :- atom(N), expr(E).