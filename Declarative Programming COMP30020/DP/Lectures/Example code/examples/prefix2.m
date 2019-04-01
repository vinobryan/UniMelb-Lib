% vim: ts=4 sw=4 et ft=mercury

:- module prefix2.

% See prefix.m

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module solutions.

main(!IO) :-
    io.write_string("the full list:\n", !IO),
	io.read(LR, !IO),
    io.write_string("the smaller list:\n", !IO),
	io.read(SR, !IO),
    (
        LR = ok(L),
        SR = ok(S)
    ->
        solutions(is_sublist(S, L, 1), MatchPoss),
        io.write_string("matching start positions: ", !IO),
        io.write(MatchPoss, !IO),
        io.nl(!IO),
        main(!IO)
    ;
        io.write_string("exiting\n", !IO)
    ).

:- pred is_sublist(list(int)::in, list(int)::in, int::in, int::out) is nondet.

is_sublist([], _, Pos, Pos).
is_sublist([A | As], [B | Bs], Pos, MatchPos) :-
    (
        is_prefix([A | As], [B | Bs]),
        MatchPos = Pos
    ;
        is_sublist([A | As], Bs, Pos + 1, MatchPos)
    ).

:- pred is_prefix(list(int)::in, list(int)::in) is semidet.

is_prefix([], _).
is_prefix([A | As], [B | Bs]) :-
    A = B,
    is_prefix(As, Bs).
