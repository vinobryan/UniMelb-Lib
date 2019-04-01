len1([], N, N).
len1([_|L], N0, N) :-
	N1 is N0 + 1,
	len1(L, N1, N).

len2(L, N) :-
	(   N =:= 0
	->  L = []
	;   N1 is N - 1,
	    L = [_|L1],
	    len2(L1, N1)
	).

len(L, N) :-
    (   integer(N)
    ->  len2(L, N)
    ;   nonvar(N)
    ->  throw(error(type_error(integer, N),
		    context(len/2, '')))
    ;   len1(L, 0, N)
    ).
