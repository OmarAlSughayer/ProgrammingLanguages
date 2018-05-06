/* Omar Adel AlSughayer, 1337255
 *  CSE 341, AB
 *  Assignment 06, Questions 1-7 */

 :- use_module(library(clpr)).
 :- use_module(library(clpfd)).



not(X) :- call(X), !, fail.
not(_).

/* Answer to Question 1 */
/* finds the average of a list of numbers */
average(X,A) :- sum_list(X, S),
		length(X, L),
		L \= 0,
		A is S/L.

/* Answer to Questoin 2 */
/* if Q1 is a queue, Q2 is a new queue that has the same elements as
Q1 plus X at the end of the queue */
enqueue([],H,[H]).
enqueue([Q|Qs],H,Q2) :- enqueue(Qs,H,Q1), Q2 = [Q|Q1].

/* if Q1 is a queue, X will be the first element in the queue,
and Q2 is a new queue that results from removing X */
dequeue([H|Q],H,Q).

/* if Q1 is a queue, X will be the first element in the queue. Fail
if Q1 is empty. */
head([H|_],H).

/* succeeds if Q1 is the empty queue; otherwise fail */
empty(Q) :- length(Q,L), L == 0.

/* Answer to Question 3 */
/* succeds if X is a grandmother of Z */
grandmother(X,Z) :- woman(X), parent(X,Y), parent(Y,Z).

/* succeeds if X is a direct son of Y */
son(X,Y) :- parent(Y,X), man(X).

/* succeeds if X is an ancestor of Z */
ancestor(X,Z) :- parent(X,Z).
ancestor(X,Z) :- parent(X, Y), ancestor(Y, Z).

/* Norwegian royal family facts */
man('Haakon VII').
man('Olav V').
man('Harald V').
man('Haakon').
woman('Martha').
woman('Mette-Marit').
woman('Maud').
woman('Sonja').
parent('Haakon VII','Olav V').
parent('Maud','Olav V').
parent('Olav V','Harald V').
parent('Martha','Harald V').
parent('Harald V','Haakon').
parent('Sonja','Haakon').

/* Answer to Question 4 */
/* finds a path from X to Y with stops P and cost C */
path(X, Y, [X, Y], C) :- edge(X, Y, C).
path(X, Y, P, TC) :-  P = [X|[S|Ss]],
		      edge(X, S, OC),
	              path(S, Y, [S|Ss], RC),
	              TC is OC+RC.

/* maze facts */
edge(allen_basement, atrium, 5).
edge(atrium, hub, 10).
edge(hub, odegaard, 140).
edge(hub, red_square, 130).
edge(red_square, odegaard, 20).
edge(red_square, ave, 50).
edge(odegaard, ave, 45).
edge(allen_basement, ave, 20).

/* Answer to Question 5 */

/* Symbolic differentiation in Prolog */

/* Find the derivative of expression A with respect to variable X.
   Use cut to prune away any alternate solutions. */

/* The expression and variable should be ground and the result a variable.
   Prolog has mode declarations (just comments for SWI Prolog though):
      deriv(+Expr,+Var,-Result)
   See http://www.swi-prolog.org/pldoc/man?section=preddesc
*/

deriv(A,X,C) :- basic_deriv(A,X,B), simplify(B,C), !.

basic_deriv(N,_,0) :- number(N).
basic_deriv(X,X,1).
basic_deriv(Y,X,0) :- atom(Y), Y\==X.

basic_deriv(A+B,X,A1+B1) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

basic_deriv(A-B,X,A1-B1) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

basic_deriv(A*B,X,A*B1+A1*B) :- basic_deriv(A,X,A1), basic_deriv(B,X,B1).

basic_deriv(sin(A),X,A1*cos(A)) :- basic_deriv(A,X,A1).

basic_deriv(cos(A),X,-1*A1*sin(A)) :- basic_deriv(A,X,A1).

basic_deriv(A^B,X,B*A1*A^(B-1)):- number(B), basic_deriv(A,X,A1).

simplify(X,X) :- atom(X).
simplify(N,N) :- number(N).

simplify(A+B,C) :-
   simplify(A,A1),
   simplify(B,B1),
   simplify_sum(A1+B1,C).

simplify(A-B,C) :-
   simplify(A,A1),
   simplify(B,B1),
   simplify_minus(A1-B1,C).

simplify(A*B,C) :-
   simplify(A,A1),
   simplify(B,B1),
   simplify_product(A1*B1,C).

simplify(A^B,C) :-
   simplify(A,A1),
   simplify(B,B1),
   simplify_power(A1^B1,C).

simplify(sin(A),S) :- simplify(A,A1), simplify_sin(A1,S).

simplify(cos(A),C) :- simplify(A,A1), simplify_cos(A1,C).

simplify_sin(A,S) :- number(A), S is sin(A).
simplify_sin(A,sin(A)).

simplify_cos(A,C) :- number(A), C is cos(A).
simplify_cos(A,cos(A)).

simplify_sum(0+A,A).
simplify_sum(A+0,A).
simplify_sum(A+B,C) :- number(A), number(B), C is A+B.
simplify_sum(A+B,A+B).

simplify_minus(0-0,0).
simplify_minus(0-A,-1*A).
simplify_minus(A-0,A).
simplify_minus(A-B,C) :- number(A), number(B), C is A-B.
simplify_minus(A-B,A-B).

/*simplify_product(0*_,0).
simplify_product(_*0,0).*/
simplify_product(A*O,A) :- number(O), {O > 0.99}, {O < 1.01}.
simplify_product(O*A,A) :- number(O), {O > 0.99}, {O < 1.01}.
simplify_product(_*Z,0) :- number(Z), {Z > -0.01}, {Z < 0.01}.
simplify_product(Z*_,0) :- number(Z), {Z > -0.01}, {Z < 0.01}.
simplify_product(A*1.0,A).
simplify_product(1*A,A).
simplify_product(A*1,A).
simplify_product(A*B,C) :- number(A), number(B), C is A*B.
simplify_product(A*B,A*B).

simplify_power(0^0,_) :- !, fail.
simplify_power(_^0,1).
simplify_power(0^_,0).
simplify_power(A^1,A).
simplify_power(1^_,1).
simplify_power(A^B,A^B).


/* Answer to Question 6 */
/* finds the average of a list using constraints */
better_average([X|Xs], A) :-
    length([X|Xs], L),
    foldl(create_sum_expr, Xs, X, S),
    {A*L =:= S}.

/* helper rule for question 6, takes two
 *  elements and creates and expression of their sum */
create_sum_expr(B, C, +(B,C)).

/* Answer to Question 7 */
/* answers the multiplicative puzzle BOB*BOB = MARLEY */
puzzle([B,O,B] * [B,O,B] = [M,A,R,L,E,Y]) :-
        Vars = [B,O,M,A,R,L,E,Y],
        Vars ins 0..9,
        all_different(Vars),
                  B*B*10201 + B*O*2020 + O*O*100 #=
        M*100000 + A*10000 + R*1000 + L*100 + E*10 + Y,
        M #\= 0, B #\= 0.

/* using the query
 *  ?- puzzle([B,O,B] * [B,O,B] = [M,A,R,L,E,Y]), label([B,O]).
 *  or any other query with two distinct labels, we can find the
 *  two unique solutions. The first solutions are:
 *  B = 3,
 *  O = 5,
 *  M = 1,
 *  A = 2,
 *  R = 4,
 *  L = 6,
 *  E = 0,
 *  Y = 9; given by the equation (353*353 = 124,609).
 *  The second solution is:
 *  B = 9,
 *  O = 2,
 *  M = 8,
 *  A = 6,
 *  R = 3,
 *  L = 0,
 *  E = 4,
 *  Y = 1; given by the equation (929*929 = 863,041). */





















