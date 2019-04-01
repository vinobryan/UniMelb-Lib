:- ensure_loaded(library(clpfd)).

member_at_n(Member, 0, [Member|_]).
member_at_n(Member, N, [_|Tail]):-
    N1 is N-1, member_at_n(Member, N1, Tail), N1>=0.

diagonal_equal([_|Rows]):-
    Rows = [First_row|_],
    First_row = [_,First_ele|_],
    diagonal_equal(First_ele, 1, Rows).
diagonal_equal(_, _, []).
diagonal_equal(Compare, N, [Head|Tail]):-
    N1 is N+1, member_at_n(X, N, Head), diagonal_equal(Compare, N1, Tail),
    X=Compare.

no_repeat_all([_|Rows]):-
    transpose(Rows, [_|Columns]),
    maplist(all_distinct, Rows),
    maplist(all_distinct, Columns).

cal_sum_product([], 0, 1).
cal_sum_product([Head|Tail], Sum, Product):-
    cal_sum_product(Tail, Sum_rest, Product_rest), Head in 1..9,
    Sum #= Head + Sum_rest, Product #= Head * Product_rest.

sum_or_product_row([]).
sum_or_product_row([[Head|Values]|Rows]):-
    cal_sum_product(Values, Sum, Product),
    (Head = Sum; Head = Product),
    sum_or_product_row(Rows).

sum_or_product(Rows):-
    Rows = [_|Values_row],
    sum_or_product_row(Values_row),
    transpose(Rows, [_|Columns]),
    sum_or_product_row(Columns).

puzzle_solution(Solution):-
    diagonal_equal(Solution), no_repeat_all(Solution), sum_or_product(Solution).
