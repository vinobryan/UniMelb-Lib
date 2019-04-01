%  Author   : Ziren Xiao
%  Origin   : Mon Oct 9 2017
%  Purpose  : Solution of proj2
%  Copyright: 2017 Ziren Xiao.  All rights reserved.
%
%  Find the solution of a puzzle
%  All rows and columns have to satisfy:
%   1. no repeat number
%   2. number between 1 and 9
%   3. the first number is the sum or the product of
%      other numbers
%   4. diagonal equal

:- ensure_loaded(library(clpfd)).

%% Check if the number on the diagonal are equal
% Input
%   Rows: The list of all numbers
% Return
%   True if all number on the diagonal are equvalent
diagonal_equal([_|Rows]):-
    Rows = [[_,First_ele|_]|_],
    diagonal_equal(First_ele, 1, Rows).
diagonal_equal(_, _, []).
diagonal_equal(Compare, N, [Head|Tail]):-
    N1 is N+1,
    nth0(N, Head, Compare),
    diagonal_equal(Compare, N1, Tail).

%% The product of two variables
% Input
%   X: The first one of variables
%   Y: The second one of variables
%   R: The result of product of two variables
% Return
%   True if R is the product of X and Y
mult(X, Y, R) :- R #= X * Y.

%% The sum of two variables
% Input
%   X: The first one of variables
%   Y: The second one of variables
%   R: The result of sum of two variables
% Return
%   True if R is the sum of X and Y
add(X, Y, R) :- R #= X + Y.

%% Check if the product or sum on a row equals to Result
% Input
%   Rows: The list of all numbers
%   Result: The compare result
% Return
%   True if the sum or product of the row equals to result
product_sum_row(Xs, Result):-
    foldl(mult,Xs,1,Result); foldl(add,Xs,0,Result).

%% Check if the row is valid row
% A valid row has to satisfy:
%   1. no repeat number
%   2. number between 1 and 9
%   3. the first number is the sum or the product of
%      other numbers
% Input
%   Rows: The list of all numbers
% Return
%   True if the row is valid
valid_row([Head|Values]):-
    maplist(between(1,9), Values),
    all_distinct(Values),
    product_sum_row(Values, Head).

%% Check if the row and transpose row are valid
% Input
%   Rows: The list of all numbers
% Return
%   True if the row and columns is valid
valid_both_row_column(Rows):-
    Rows = [_|Values_row],
    maplist(valid_row, Values_row),
    transpose(Rows, [_|Columns]),
    maplist(valid_row, Columns).

%% Check if the puzzle solution is valid
% Input
%   Solution: The list of all numbers
% Return
%   True if the solution is valid
puzzle_solution(Solution):-
    diagonal_equal(Solution),
    valid_both_row_column(Solution).
