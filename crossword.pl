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



fourLetterWord(FourWords) :- word(FourWords,_,_,_,_).




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



endsWithOL(FiveWords) :- word(FiveWords,_,_,_,o,l).



:- begin_tests(endsWithOL).

test(endsWithOL) :-
  setof(X, endsWithOL(X), Xs),
  Xs = ['ALGOL', 'COBOL'].

:- end_tests(endsWithOL).


% Step 3
%
% Now, create crossword/10, a rule that will solve the crossword puzzle from the readme.



crossword(Across1,Across6,Across7,Across8,Across9,Down2,Down3,Down4,Down5,Down9):-
	word(Across1,_,_,_,Q),
	word(Across6,_,U,_,_,Y),
	word(Across7,_,R,_,_,_,W),
	word(Across8,_,_,E,_,_),
	word(Across9,A,_,_,O,_,_,I,_,_,T),
	word(Down2,Q,_,_,W,_,E),
	word(Down3,_,U,_,_,_,O,_),
	word(Down4,_,Y,_,_,_,I,_),
	word(Down5,_,_,R,_,_,T,_,_,_),
	word(Down9,A,_,_,_).


:- begin_tests(crossword).

test(crossword) :-
  crossword('Lisp', 'COBOL', 'Pascal', 'ALGOL', 'JavaScript', 'Prolog', 'Fortran', 'Clojure', 'Smalltalk', 'Java').

:- end_tests(crossword).

% vim:set ft=prolog:
