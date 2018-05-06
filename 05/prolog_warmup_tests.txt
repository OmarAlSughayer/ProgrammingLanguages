/* unit tests for HW 5, Prolog Warmup, CSE 341, Autumn 2015 */


/* read in a file prolog_warmup.pl (so you should call your homework this!) */
:- consult(prolog_warmup).

:- begin_tests(hw5).
test(repeat,  [nondet]) :- repeat(squid,[]).
test(repeat, [nondet]) :- repeat(squid,[squid,squid,squid,squid]).
test(repeat, [fail]) :- repeat(squid,[squid,squid,squid,clam]).

/* about the tests of repeat .... this simpler version
    test(repeat) :- repeat(squid,[]).
would also work, which also checks that the goal repeat(squid,[]) succeeds.
However, with the simpler version the unit test framework would give a warning
    Test succeeded with choicepoint
meaning that at the time the test succeeded Prolog didn't know yet
that there weren't more possibilities.  (It only finds that out when
it checks.)  We can tell Prolog that this might be nondeterministic
using the [nondet] argument, which gets rid of the warning.

The last test says that we expect repeat(squid,[squid,squid,squid,clam])
to fail.  So the test succeeds if that goal fails. */


test(sentence, [nondet]) :- sentence(all,dolphins,frolic).
test(sentence, [nondet]) :- sentence(few,clams,dream).
test(sentence, [fail]) :- sentence(few,clams,fly).

:- end_tests(hw5).

/* Run the tests.  Also print out a note and some blank lines so that
  the test results don't get lost in the other introductory stuff */
:- write("\nRUNNING ALL TESTS FOR HW 5 \n").
:- run_tests.
:- write("\n\n\n").

/* After the file is loaded you can also type the goal
         run_tests.
   to run the tests again. */













































