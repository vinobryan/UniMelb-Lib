% vim: ts=4 sw=4 et ft=mercury

:- module prefix.

% simple version of checking if a substring exists in a string
% (probably should change the name of a few things)

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    io.write_string("the full list:\n", !IO),
	io.read(LR, !IO),
    io.write_string("the smaller list:\n", !IO),
	io.read(SR, !IO),
    (
        LR = ok(L),
        SR = ok(S)
    ->
        ( is_sublist(S, L) ->
            io.write_string("yes, it is a sublist\n", !IO)
        ;
            io.write_string("no, it is not a sublist\n", !IO)
        ),
        main(!IO)
    ;
        io.write_string("exiting\n", !IO)
    ).

:- pred is_sublist(list(int)::in, list(int)::in) is semidet.

is_sublist([], _).
is_sublist([A | As], [B | Bs]) :-
    (
        is_prefix([A | As], [B | Bs])
    ;
        is_sublist([A | As], Bs)
    ).

:- pred is_prefix(list(int)::in, list(int)::in) is semidet.

is_prefix([], _).
is_prefix([A | As], [B | Bs]) :-
    A = B,
    is_prefix(As, Bs).
