f([A], X) :- X is A - 1.
f([Y,Y|A], X) :- f([Y|A], X1), X is Y * X1.
f([Y,Z|A], X) :- f([Z|A], X1), X is (Y - 1) * X1.  

totient(N, 0) :- N =< 1.
totient(N, T) :- prime_factors(N, X), f(X, T).
