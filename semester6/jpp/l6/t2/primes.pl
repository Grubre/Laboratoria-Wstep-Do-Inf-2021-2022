primes(N, X) :- primes_helper(2, N, [], X).

primes_helper(P, N, Acc, Result) :-
    P > N,
    reverse(Acc, Result).

primes_helper(P, N, Acc, Result) :-
    P =< N,
    (   is_prime(P)
    ->  primes_helper(P + 1, N, [P | Acc], Result)
    ;   primes_helper(P + 1, N, Acc, Result)
    ).

is_prime(2).
is_prime(3).
is_prime(P) :-
    P > 3,
    P mod 2 =\= 0,
    \+ has_factor(P, 3).

has_factor(N, L) :-
    L * L =< N,
    (   N mod L =:= 0
    ;   Next is L + 2,
        has_factor(N, Next)
    ).
