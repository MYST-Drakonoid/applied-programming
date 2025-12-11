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
factorial_man/1,
factorize/1,
factors_to_string/1

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



factorize({N, D}) ->
    if
        N =< 1 ->
            [];                         % base case

        N rem D =:= 0 ->
            [D | factorize({N div D, D})];   % dependent recursion

        D < N ->
            factorize({N, D + 1});      % continue searching

        true ->
            [N]                         % remaining N is prime
    end.

%% Wrapper for clean usage
factorial_man(N) ->
    if
        N < 0 ->
            error({invalid_input, N});

        N > 30 ->
            "Too_Large";

        true ->
            factorial_gen_internal(N)
    end.


factorial_gen_internal(N) ->
    if
        N == 0 ->
            1;

        N > 0 ->
            N * factorial_gen_internal(N - 1);

        true ->
            error({invalid_input, N})
    end.

factors_to_string(Factors) ->
    Strings = [integer_to_list(F) || F <- Factors],
    string:join(Strings, ",").
