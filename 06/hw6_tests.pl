/* Omar Adel AlSughayer, 1337255
 *  CSE 341, AB
 *  Assignment 06, unit tests */

/* Starter file for tests for HW 6, Prolog Project, CSE 341, Autumn 2015
*  You'll need to add some other tests, as described in the assignment.
Hint: SWI Prolog allows nested comments - this may be useful if you
want to comment out most of the tests initially and then gradually
move the boundary as you get more things working.
*/

/* read in a file hw6.pl (so you should call your homework this!) */
:- consult(hw6).

:- begin_tests(question1).
test(average) :- average([1,2,3,4,5],3).
test(average,  [fail]) :- average([],_).
:- end_tests(question1).

:- begin_tests(question2).
test(enqueue) :- enqueue([],2,[2]).
test(enqueue) :- enqueue([2],3,[2,3]).
test(dequeue) :- dequeue([2,3],X,Y), X = 2, Y = [3].
test(head) :- head([2,2],X), X = 2.
test(head, [fail]) :- head([],_).
test(empty) :- empty([]).
test(empty, [fail]) :- empty([2,3]).
:- end_tests(question2).

:- begin_tests(question3).
test(grandmother) :- grandmother('Martha', 'Haakon').
test(son) :- son('Haakon', 'Sonja').
test(ancestor, [nondet]) :- ancestor('Haakon VII', 'Haakon').
test(ancestor, [nondet]) :- ancestor('Sonja','Haakon').
test(grandmother, [fail]) :- grandmother('Maha', 'Haakon').
test(son, [fail]) :- son('Sonja', 'Haakon').
test(ancestor, [fail]) :- ancestor('Haakon', 'Haakon VII').
test(ancestor, [fail]) :- ancestor('Haakon', 'Sonja').
:- end_tests(question3).


:- begin_tests(question4).
/* First a nondeterministic test of one path: */
test(maze, [nondet]) :- path(allen_basement, ave,
     [allen_basement, atrium, hub, odegaard, ave], 200).
/* A test for all of the costs for the 4 possible paths
   (ignoring the route).  We use 'set' rather than 'all' so that
   it doesn't matter what order the results come back. */
test(maze, set(C==[20, 195, 200, 210])) :- path(allen_basement, ave, _, C).
/* Finally an exhaustive test of all 4 paths, including both route and costs.
  To make this work with Prolog's unit test framework, we bundle the route
  and cost together into a 'solution', and have a list of solutions.  (The
  name 'solution' is arbitrary -- this could be any name and would
  still work.) */
test(maze, set(Soln==[
   solution([allen_basement, ave],20),
   solution([allen_basement, atrium, hub, odegaard, ave],200),
   solution([allen_basement, atrium, hub, red_square, ave],195),
   solution([allen_basement, atrium, hub, red_square, odegaard, ave],210)
   ])) :-
   path(allen_basement, ave, S, C), Soln = solution(S,C).
:- end_tests(question4).


:- begin_tests(question5).

test(const) :- deriv(3,x,0).
test(x) :- deriv(x,x,1).
test(y) :- deriv(y,x,0).
test(plus) :- deriv(x+3,x,1).
test(plus) :- deriv(x+y,x,1).
test(plus_unsimp) :- deriv((2+3)*x,x,5).
test(times) :- deriv( 10*(x+3), x, 10).
test(times) :- deriv( (x*y)*(x+3), x, (x*y)+(y*(x+3))).
test(minus) :- deriv(x-3,x,1).
test(minus) :- deriv(x-y,x,1).
test(minus) :- deriv((2-3)*x,x,-1).
test(sin) :- deriv(sin(x),x,cos(x)).
test(sin) :- deriv(sin(3*x),x,3*cos(3*x)).
test(sin) :- deriv(sin(0)*x,x,0).
test(cos) :- deriv(cos(x),x,-1*sin(x)).
test(cos) :- deriv(cos(3*x),x,-3*sin(3*x)).
test(cos) :- deriv(cos(0)*x,x,O), {O > 0.99}, {O < 1.01}.
test(power) :- deriv(x^5,x,5*x^4).
test(power) :- deriv((3*x+2)^5,x,15*(3*x+2)^4).
test(power) :- deriv(cos(0)*x^5,x,5*x^4).

:- end_tests(question5).


:- begin_tests(question6).
/* to avoid roundoff errors, these tests check whether the actual answer is
   within 0.01 of the expected answer */

/* the expected answer to the following test is A=3 */
test(better_average, [nondet]) :- better_average([1,2,3,4,5],A), {A>2.99}, {A<3.01}.

/* the expected answer to the following test is X=5 */
test(better_average, [nondet]) :- better_average([1,X,3],3), {X>4.99}, {X<5.01}.

/* the expected answer to the following test is X=10 */
test(better_average, [nondet]) :- better_average(Xs,10), Xs=[X], {X>9.99}, {X<10.01}.

/* the expected answer to the following test is Xs=[5,15] and B=15 */
test(better_average, [nondet]) :- better_average(Xs,10), Xs=[5,B], {B>14.99}, {B<15.01}.

test(better_average,  [fail]) :- better_average([],_).

:- end_tests(question6).


- run_tests.














































