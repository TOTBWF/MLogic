:- use_module(eval).

:- initialization main.

main :- current_prolog_flag(argv, Arguments), main(Arguments).
main([]) :- halt.
main([File|_]) :- 
    phrase_from_file(run_parse(D, E), File), 
    run_infer(D, E, _, _),
    pretty_infer(D, E, Ds, Es),
    format('Declarations:~n'),
    print_decls(Ds),
    format('Expression: ~s~n', Es),
    run_eval(D, E, V),
    format('Result: ~w~n', V),
    halt.

print_decls([D|Ds]) :- format('~s~n', D), print_decls(Ds).
print_decls([]).