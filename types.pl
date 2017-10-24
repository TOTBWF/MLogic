ty(tcons(T)) :- atom(T). % Type Constructor
ty(tvar(V)) :- atom(V). % Type Variable
ty(tarrow(T1, T2)) :- ty(T1), ty(T2). % Function Type
ty(tpoly(Tvar, T)) :- Tvar = tvar(_), ty(T). % Polymorphic Type