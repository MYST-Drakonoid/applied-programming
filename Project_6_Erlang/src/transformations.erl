%% File: transformations.erl
-module(transformations).

-export([
identity/1,
square/1,
double/1,
triple/1,
abs_val/1,
is_even/1,
is_large/1,
is_positive/1,
factorize_row/1

]).


%% --- transformation functions ---

identity(N) -> N.

square(N) -> N * N.

double(N) ->N * 2.

triple(N) -> N * 3.

abs_val(N) ->
    if 
        N < 0 -> -N;
        N > 0 ->  N;
        true  ->  0
    end.

is_even(N) -> N rem 2 =:= 0.

is_large(N) -> N > 1000.

is_positive(N) -> N > 0.


factorize(N) when N > 1 ->
    factorize(N, 2).

factorize(1, _) ->
    [].

factorize(N, D) when N rem D =:= 0 ->
    [D | factorize(N div D, D)];

factorize(N, D) ->
    factorize(N, D + 1).

factorize_row(N) -> 
    #{
        number => N,
        factors => factorize(N)
    }.



