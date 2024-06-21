factor(N,I,[N]) :- II is I * I, N < II.
factor(N,I,[I|X]) :- divmod(N, I, Q, R), R = 0, factor(Q, I, X).
factor(N,I,X) :- II is I + 1, factor(N, II, X).

prime_factors(N, []) :- N =< 1.
prime_factors(N, X) :- factor(N,2,X).
