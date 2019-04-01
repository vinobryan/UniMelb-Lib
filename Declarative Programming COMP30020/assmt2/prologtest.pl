%  File     : prologtest.pl
%  RCS      : $Id$
%  Author   : Peter Schachte
%  Origin   : Fri Sep 19 16:29:36 2014
%  Purpose  : Prolog testing harness
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Define the test suite by defining a predicate test/4:
%%	test(Goal,Expected,LimitSecs,Weight)
%% such that Goal is a test goal, Expected is a list of all the solutions of Goal
%% specified as a list of instantiations of Goal.  LimitSecs is the time to allow
%% for all solutions of Goal to be found, and Weight is the value of this one test,
%% relative to all the others, expressed as a number.  The number itself is
%% unimportant; only its value relative to other weights matters.  This permits
%% some tests to be given greater than others.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			Running the test suite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile test/4.

%% do_tests
%% do_tests(Marks)
%% Run the full test suite, printing the test results and summarising the marks
%% earned, out of a maximum score of Marks.  Marks defaults to 100.

do_tests :-
	do_tests(100).

do_tests(Marks) :-
        do_tests(current_output, Marks, _).


%% file_tests(Logfile, Resultfile, Marks)
%% Run the full test suite, logging the test results and summarising the marks
%% earned, out of a maximum score of Marks, to Logfile, and writing
%% the actual score to Resultfile.

file_tests(Logfile, Resultfile, Marks) :-
        open(Logfile, write, LogStream),
        do_tests(LogStream, Marks, Earned),
        close(LogStream),
        open(Resultfile, write, ResultStream),
	write(ResultStream, Earned),
	nl(ResultStream),
	close(ResultStream).


%% do_tests(Stream, Marks, Earned)
%% Run the full test suite, logging the test results and summarising the marks
%% earned, out of a maximum score of Marks, to output stream Stream.  Earned
%% is the number of marks actually earned.

do_tests(Stream, Marks, Earned) :-
	print_test_header(Stream),
	findall(test(Goal,Expected,LimitSecs,Weight),
		test(Goal,Expected,LimitSecs,Weight),
		Tests),
        do_tests(Tests, Stream, 0, Count, 0, Score, 0, TotalWeight),
	Earned is Marks * Score / TotalWeight,
	print_summary(Stream, Marks, Count, Score, TotalWeight, Earned).


%% do_tests(Tests, Stream, Count0, Count, Points0, Points, TotalWeight0, TotalWeight)
%% Run all tests on Tests, logging the output to Stream.  Count0 is the number of
%% tests completed so far, and Count is the total number of tests.  Points0 is
%% the number of tests earned so far and Points is the total number of points
%% earned.  TotalWeight0 is the total weight of the tests run so far, and
%% TotalWeight is the total weight of all tests.

do_tests([], _, Count, Count, Points, Points, TotalWeight, TotalWeight).
do_tests([test(Goal,Expected0,LimitSecs,Weight)|Tests], Stream, Count0, Count,
	  Points0, Points, TotalWeight0, TotalWeight) :-
	Count1 is Count0 + 1,
	announce_test(Stream, Count1, Goal),
	sort(Expected0, Expected),
	statistics(cputime, Tm0),
	run_test(Goal, Expected, LimitSecs, Result),
	statistics(cputime, Tm1),
	test_points(Result, Fraction),
	Pts is Fraction * Weight,
	Tm is Tm1 - Tm0,
	announce_result(Stream, Result, Weight, Pts, Tm),
	Points1 is Points0 + Pts,
	TotalWeight1 is TotalWeight0 + Weight,
	do_tests(Tests, Stream, Count1, Count, Points1, Points,
		  TotalWeight1, TotalWeight).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			Running a single test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate user:run_test2(:,+,+,-).

%% run_test(Goal, Expected, LimitSecs, Result)
%% Run test Goal with expected result list Expected, imposing timeout LimitSecs.
%% Result is the test result.

run_test(Goal, Expected, LimitSecs, Result) :-
	catch(call_with_time_limit(LimitSecs,
				   run_test2(Goal, Goal, Expected, Result)),
	      Exception,
	      exception_result(Exception, Result)).


run_test2(Goal, Template, Expected, Result) :-
	(   setof(Template, Goal, Solutions)
	->  true
	;   Solutions = []
	),
	categorise_test_result(Goal, Solutions, Expected, Result).


exception_result(Exception, Result) :-
	(   Exception == time_limit_exceeded
	->  Result = timeout
	;   Result = error(Exception)
	).


categorise_test_result(Goal, Solutions, Expected, Result) :-
	numbervars(Goal, 0, SolStart),
	numbervars(Solutions, SolStart, _),
	numbervars(Expected, SolStart, _),
	length(Expected, CorrectCount),
	length(Solutions, ActualCount),
	(   Solutions = Expected
	->  Result = pass
	;   CorrectCount > 0,
	    \+ (member(S,Solutions), \+ member(S, Expected))
	->  Result = missing_solutions(ActualCount,CorrectCount)
	;   \+ (member(S,Expected), \+ member(S, Solutions))
	->  Extras is ActualCount - CorrectCount,
	    Result = wrong_solutions(Extras,CorrectCount)
	;   Result = wrong_and_missing
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%			 Producing logging output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_test_header(Stream) :-
        format(Stream,
	       'Num Test~t~36| ~tSecs~42| ~tStatus~t~50|~tScore~t~62|Remark~n', []),
        format(Stream,
	       '--- ----~t~36| ~t----~42| ~t------~t~50|~t-----~t~62|------~n', []).


announce_test(Stream, Num, Goal) :-
	goal_description(Goal, Descrip),
	format(Stream, '~t~d~3| ~w ...~t~36|', [Num,Descrip]),
	flush.


announce_result(Stream, Result, Weight, Pts, Time) :-
	result_description(Result, Summary, DetailFmt, FmtArgs),
	format(Stream, '~t~2f~42| ~t~w~t~50|~t~1f/~1f~t~62|', [Time,Summary,Pts,Weight]),
	format(Stream, DetailFmt, FmtArgs),
	nl(Stream).


print_summary(Stream, Marks, Count, Score, TotalWeight, Earned) :-
	format(Stream, '~nTotal tests executed: ~d~n', [Count]),
	Pct is 100 * Score / TotalWeight,
	format(Stream, 'Total correctness : ~2f / ~2f = ~2f%~n',
	       [Score,TotalWeight, Pct]),
	format(Stream, 'Marks earned : ~2f / ~2f~n',
	       [Earned, Marks]).


result_description(timeout, '* Timeout', '', []).
result_description(error(Exception), 'Error', '~w', [Exception]).
result_description(pass, '  PASS', '', []).
result_description(missing_solutions(ActualCount,CorrectCount),
		   '* miss', 'missed ~d of ~d',
		   [MissCount,CorrectCount]) :-
	MissCount is CorrectCount - ActualCount.
result_description(wrong_solutions(Extras,CorrectCount),
		   '* wrong', '~d wrong solutions',
		   [Extras,CorrectCount]).
result_description(wrong_and_missing, '* wrong',
		   'wrong and missing', []).
	

test_points(timeout, 0.0).
test_points(error(_), 0.0).
test_points(pass, 1.0).
test_points(missing_solutions(ActualCount, CorrectCount), Ratio) :-
	Ratio is (ActualCount/CorrectCount)^2.
test_points(wrong_solutions(_,_), 0.0).
test_points(wrong_and_missing, 0.0).
	



goal_description(Goal, Descrip) :-
	functor(Goal, Name, Arity),
	functor(Descrip, Name, Arity),
	goal_discrip_args(Goal, Descrip, Arity).

goal_discrip_args(Goal, Descrip, N) :-
	(   N =< 0
	->  true
	;   arg(N, Goal, Arg),
	    arg(N, Descrip, ArgDescrip),
	    arg_descrip(Arg, ArgDescrip),
	    N1 is N - 1,
	    goal_discrip_args(Goal, Descrip, N1)
	).
	
arg_descrip(Arg, Descrip) :-
	(   var(Arg)
	->  Descrip = out
	;   ground(Arg)
	->  Descrip = in
	;   Descrip = inout
	).
