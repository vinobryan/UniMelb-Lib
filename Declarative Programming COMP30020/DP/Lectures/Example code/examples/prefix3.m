% vim: ts=4 sw=4 et ft=mercury

:- module prefix3.

% See prefix.m

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

is_sublist(As, Bs) :- append3(_Prefix, As, _Suffix, Bs).

% append3 is actually semidet in this mode but the Mercury compiler is
% (unsurprisingly) not able to determine this so we use nondet.
% However, because the outputs are never used, is_sublist can still be
% semidet.

:- pred append3(list(T), list(T), list(T), list(T)).
:- mode append3(out, in, out, in) is nondet.

append3(A, B, C, ABC) :-
    append(A, B, AB), append(AB, C, ABC).
