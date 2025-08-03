
lenght([], 0).
lenght([_|T], L) :- 
    lenght(T, L1),
    L is L1+1.

is_9_same(T) :- lenght(T,9), is_same(T).

is_same([]).
is_same([_]).
is_same([H,H|T]) :- is_same([H|T]).


kostka([])