% Valid crossword words
word('Lisp', l, i, s, p).
word('Java', j, a, v, a).
word('ALGOL', a, l, g, o, l).
word('COBOL', c, o, b, o, l).
word('Pascal', p, a, s, c, a, l).
word('Prolog', p, r, o, l, o, g).
word('Clojure', c, l, o, j, u, r, e).
word('Fortran', f, o, r, t, r, a, n).
word('JavaScript', j, a, v, a, s, c, r, i, p, t).
word('Smalltalk', s, m, a, l, l, t, a, l, k).

% Step 1
%
% create a rule for fourLetterWord/1 here.  This means that it will be a rule
% that takes a single argument with the functor called 'fourLetterWord'.
% Remember that you can use '_' to unify with anything and not keep the
% instantiated value.
fourLetterWord(X) :- word(X,_,_,_,_).


% The following test is enabled and will fail until you have defined a valid
% fourLetterWord/1 rule
:- begin_tests(fourLetterWords).

test(fourLetterWord) :-
  setof(X, fourLetterWord(X), Xs),
  Xs == ['Java', 'Lisp'].

:- end_tests(fourLetterWords).


% Step 2
%
% Create a rule for endsWithOL/1 here.  This rule will check to see if a
% five-character word ends with the letters o and l.

endsWithOL(X) :-  word(X,_,_,_,o,l).
:- begin_tests(endsWithOL, [blocked('step 2')]).

test(endsWithOL) :-
  setof(X, endsWithOL(X), Xs),
  Xs = ['ALGOL', 'COBOL'].

:- end_tests(endsWithOL).


% Step 3
% Now, create crossword/10, a rule that will solve the crossword puzzle from the readme.
%
sixLetterWord(X) :- word(X,_,_,_,_,_,_).
fiveLetterWord(X) :- word(X,_,_,_,_,_).
sevenLetterWord(X) :- word(X,_,_,_,_,_,_,_).
tenLetterWord(X) :- word(X,_,_,_,_,_,_,_,_,_,_).
nineLetterWord(X) :- word(X,_,_,_,_,_,_,_,_,_).

endsWith1(X,Y) :-
	word(X,_,_,_,A),
	word(Y,A,_,_,_,_,_).

middleOf2(X,Y) :-
	word(X,_,_,_,A,_,_),
	word(Y,_,_,_,_,_,A).

middleOf8(X,Y) :-
	word(X,_,_,A,_,_),
	word(Y,_,_,_,_,_,A).

headOf3(X,Y) :-
	word(X,_,A,_,_,_,_,_),
	word(Y,_,A,_,_,_).

tailOf3(X,Y) :-
	word(X,_,_,_,_,_,A,_),
	word(Y,_,_,_,A,_,_,_,_,_,_).

headOf4(X,Y) :-
	word(X,_,A,_,_,_,_,_),
	word(Y,_,_,_,_,A).

tailOf4(X,Y) :-
	word(X,_,_,_,_,_,A,_),
	word(Y,_,_,_,_,_,_,A,_,_,_).

startOf9(X,Y) :-
	word(X,A,_,_,_),
	word(Y,A,_,_,_,_,_,_,_,_,_).

crossword(Across1,Across6,Across7,Across8,Across9,Down2,Down3,Down4,Down5,Down9) :-
	fourLetterWord(Across1), endsWith1(Across1,Down2),
	sixLetterWord(Down2), middleOf2(Down2,Across7),
	sevenLetterWord(Down3), headOf3(Down3,Across6), tailOf3(Down3,Across9),
	sevenLetterWord(Down4), headOf4(Down4,Across6), tailOf4(Down4,Across9),
	nineLetterWord(Down5),
	fiveLetterWord(Across6),
	sixLetterWord(Across7),
	fiveLetterWord(Across8), middleOf8(Across8,Down2),
	tenLetterWord(Across9),
	fourLetterWord(Down9), startOf9(Down9,Across9).

:- begin_tests(crossword, [blocked('step 3')]).

test(crossword) :-
  crossword('Lisp', 'COBOL', 'Pascal', 'ALGOL', 'JavaScript', 'Prolog', 'Fortran', 'Clojure', 'Smalltalk', 'Java').

:- end_tests(crossword).

% vim:set ft=prolog:
