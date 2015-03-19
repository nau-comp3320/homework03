% All male members of the family
male(miguel).
male(davi).
male(arthur).
male(pedro).
male(gabriel).
male(bernardo).
male(lucas).
male(matheus).
male(rafael).
male(heitor).
male(theuncle).
male(thedad).

% All female members of the family
female(sophia).
female(alice).
female(julia).
female(isabella).
female(manuela).
female(laura).
female(luiza).
female(valentina).
female(giovanna).
female(maria_eduarda).
female(theniece).
female(thegrandmother).

% all parent/child relationships
parent(miguel, davi).
parent(miguel, arthur).
parent(miguel, alice).
parent(sophia, davi).
parent(sophia, arthur).
parent(sophia, alice).
parent(davi, gabriel).
parent(davi, manuela).
parent(julia, gabriel).
parent(julia, manuela).
parent(gabriel, giovanna).
parent(gabriel, heitor).
parent(laura, giovanna).
parent(laura, heitor).
parent(bernardo, maria_eduarda).
parent(manuela, maria_eduarda).
parent(arthur, luiza).
parent(isabella, luiza).
parent(lucas, rafael).
parent(luiza, rafael).
parent(pedro, matheus).
parent(pedro, valentina).
parent(alice, matheus).
parent(alice, valentina).
parent(thedad, theniece).
parent(thegrandmother, thedad).
parent(thegrandmother, theuncle).
% Step 1
%
% Create a rule mother/2 to determine if the first argument is the mother of
% the second argument
%
mother(X,Y):- parent(X,Y), female(X).

:- begin_tests(mother).

test(mother) :-
  setof([Mother, Child], mother(Mother, Child), MotherChildren),
  MotherChildren == [
    [alice, matheus],
    [alice, valentina],
    [isabella, luiza],
    [julia, gabriel],
    [julia, manuela],
    [laura, giovanna],
    [laura, heitor],
    [luiza, rafael],
    [manuela, maria_eduarda],
    [sophia, alice],
    [sophia, arthur],
    [sophia, davi]
  ].

:- end_tests(mother).


% Step 2
%
% Create a rule father/2 to determine if the first argument is the father of
% the second argument.  Be sure to enable the test.
%

father(X,Y):- parent(X,Y), male(X).

:- begin_tests(father, [blocked('step 2')]).

test(father) :-
  setof([Father, Child], father(Father, Child), FatherChildren),
  FatherChildren == [
    [arthur, luiza],
    [bernardo, maria_eduarda],
    [davi, gabriel],
    [davi, manuela],
    [gabriel, giovanna],
    [gabriel, heitor],
    [lucas, rafael],
    [miguel, alice],
    [miguel, arthur],
    [miguel, davi],
    [pedro, matheus],
    [pedro, valentina]
  ].

:- end_tests(father).


% Step 3
%
% Create a rule ancestor/2 to determine if the first argument is an ancestor of
% the second argument.  Note that this will be a recursive rule.  Be sure to
% enable the test.
%

ancestor(X,Y):- parent(X,Y).
ancestor(X,Y):- parent(X,A), ancestor(A,Y).

:- begin_tests(ancestor, [blocked('step 3')]).

test(ancestor, [nondet]) :-
  ancestor(miguel, davi),
  ancestor(sophia, maria_eduarda),
  ancestor(davi, heitor),
  ancestor(pedro, valentina),
  \+ ancestor(manuela, rafael),
  \+ ancestor(arthur, sophia).

:- end_tests(ancestor).


% Step 4
%
% Create a rule descendant/2 to determine if the first argument is an ancestor
% of the second argument.  Note that this should also be a recursive rule.  Be
% sure to enable the test.
%
% Bonus: Can you define descendant non-recursively using a single clause?  If
% so, define a rule descendant2/2 that does this.
%


descendant(X,Y):- parent(Y,X).
descendant(X,Y):- parent(Y,Z), descendant(X,Z).

:- begin_tests(descendant, [blocked('step 4')]).

test(descendant, [nondet]) :-
  descendant(davi, miguel),
  descendant(maria_eduarda, sophia),
  descendant(heitor, davi),
  descendant(valentina, pedro),
  \+ descendant(rafael, manuela),
  \+ descendant(sophia, arthur).

:- end_tests(descendant).


% Step 5
%
% Create a rule sibling/2 to determine if the first argument is a sibling
% of the second argument.  Be sure to enable the test.
%
% Bonus: What happens when you try to run the goal sibling(A,B)?  Do you get
% any surprising results?  How would you explain what is happening?
%
sibling(X,Y):- parent(Z,Y),parent(Z,X).

:- begin_tests(sibling, [blocked('step 5')]).

test(sibling, [nondet]) :-
  sibling(davi, arthur),
  sibling(matheus, valentina),
  \+ sibling(giovanna, rafael).

:- end_tests(sibling).


% Step 6
%
% Now it's your turn to define a rule that describes a family relation.
% Impress me.
%
daughter(X,Y):- parent(Y,X), female(X).
son(X,Y):- parent(Y,X), male(X).
uncle(X,Y):- male(X), parent(Z,Y), sibling(X,Z).
aunt(X,Y):- female(X), parent(Z,Y), sibling(X,Z).

% vim:set ft=prolog:
