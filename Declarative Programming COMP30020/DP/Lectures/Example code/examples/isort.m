:- module isort.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module solutions, list, int.

main(!IO) :-
	solutions(isort.isort([3,1,2]), S),
	io.write(S, !IO),
	io.nl(!IO).

:- pred isort.isort(list(int)::in, list(int)::out) is nondet.

isort.isort([], []).
isort.isort([H | T], S) :-
	isort.isort(T, ST),
	isort.insert(H, ST, S).

	% Insert a number into a sorted list.
	%
:- pred insert(int::in, list(int)::in, list(int)::out) is nondet.

isort.insert(N, [], [N]).
isort.insert(N, [H | T], [N | T]) :-
	N =< H.
isort.insert(N, [H | T], [H | NT]) :-
	N > H,
	isort.insert(N, T, NT).
