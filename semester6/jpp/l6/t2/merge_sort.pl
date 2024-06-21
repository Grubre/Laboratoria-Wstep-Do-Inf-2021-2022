split([], [], []).
split([X], [X], []).
split([L,R|X], [L|Y], [R|Z]) :- split(X, Y, Z).

merge(X,X,[]).
merge(X,[],X).
merge([L|X], [L|Y], [R|Z]) :- L =< R, merge(X,Y,[R|Z]).
merge([R|X], [L|Y], [R|Z]) :- L > R, merge(X,[L|Y],Z).

merge_sort([],[]).
merge_sort([X],[X]).
merge_sort(X,Y) :- split(X, LS, RS), merge_sort(L, LS), merge_sort(R, RS), merge(Y, LS, RS).
