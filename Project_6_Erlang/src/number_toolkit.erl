%% File: number_toolkit.erl
-module(number_toolkit).
-export([run/0]).

read_numbers(FilePath) -> 
    {ok, Binary} = file:read_file(FilePath),
    String = binary_to_list(Binary),
    Lines = string:split(String, "\n", all),

    % Remove empty lines
    NonEmpty = lists:filter(fun(Line) -> Line =/= "" end, Lines),

    % Remove header row if present
    DataLines =
        case NonEmpty of
            ["number" | Rest] -> Rest;
            ["number\r" | Rest] -> Rest;
            _ -> NonEmpty
        end,

    % Convert each cleaned line to integer
    lists:map(
        fun(Line) ->
            Clean = string:trim(Line),  % removes \r, spaces, tabs
            erlang:list_to_integer(Clean)
        end,
        DataLines
    ).

header_row() ->
    ["Identity", "Squared", "Doubled", "Tripled", "Abs_Val",
     "Is_Even", "Is_Large", "Is_Pos", "Factors", "Factorial"].

%% YOU KEEP THIS — and we make it WORK
factorize_row(N) ->
    Factors = transformations:factorize({N, 2}),
    String = transformations:factors_to_string(Factors),
    String.   % <-- RETURN STRING, not a map (CSV-safe)

make_row(N) ->
    [
        transformations:identity(N),
        transformations:square(N),
        transformations:double(N),
        transformations:triple(N),
        transformations:abs_val(N),
        transformations:is_even(N),
        transformations:is_large(N),
        transformations:is_positive(N),
        factorize_row(N),                 % <-- YOUR FUNCTION
        transformations:factorial_man(N)
    ].

make_table(List) ->
    [ header_row() | lists:map(fun make_row/1, List) ].

run() ->
    Input = read_numbers("../data/random_numbers.csv"),
    TransformTable = make_table(Input),
    CSV = fileconverter:table_to_csv(TransformTable),
    io:format("CSV Output:~n~s~n", [CSV]),
    file:write_file("output.csv", CSV).

