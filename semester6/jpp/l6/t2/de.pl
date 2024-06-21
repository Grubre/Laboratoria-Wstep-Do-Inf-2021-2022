de(A, 0, 1, 0, A).
de(A, B, X, Y, Z) :- divmod(A, B, Q, R), de(B, R, X1, Y1, Z), X = Y1, Y is X1 - Y1 * Q.
