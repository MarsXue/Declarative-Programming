% Question 1
replace(E1, [E1|Tail], E2, [E2|Tail]).
replace(E1, [Head|L1], E2, [Head|L2]) :- replace(E1, L1, E2, L2).

% Question 2
zip([], [], []).
zip([A|As], [B|Bs], [AB|ABs]) :- zip(As, Bs, ABs),
                                 length(As, L), length(Bs, L),
                                 AB = A - B.

% Question 3
sublist([], _).
sublist([X|XS], [X|XSS]) :- sublist(XS, XSS).
sublist(XS, [_|XSS]) :- sublist(XS, XSS).
