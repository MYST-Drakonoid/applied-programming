-module(number_toolkit).
-export([run/0]).

read_numbers(FilePath) -> 

{ok, Binary} = file:read_file(FilePath),

String = binary_to_list(Binary),

Lines = string:split(String, "\n", all),

NonEmptyLines = 
    lists:filter(
        fun(Line) -> Line =/= "" end,
        Lines
    ),

Numbers = 
    lists:map(
        fun list_to_integer/1,
        NonEmptyLines
),

Numbers.

header_row() ->
    ["Identity", "Squared", "Doubled", "Tripled", "Abs_Val",
     "Is_Even", "Is_Large", "Is_Pos"].

make_row(N) ->
    [
        transformations:identity(N),
        transformations:square(N),
        transformations:double(N),
        transformations:triple(N),
        transformations:abs_val(N),
        transformations:is_even(N),
        transformations:is_large(N),
        transformations:is_positive(N)
    ].

make_table(List) ->
    [ header_row() | lists:map(fun make_row/1, List) ].

transform_rows(List) ->
    make_table(List).


factorization_table(List) ->
    lists:map(fun transformations:factorize_row/1, List).



run() ->
    Input = read_numbers("Project_6_Erlang/data/random_numbers.csv"),

    % Main transform table
    TransformTable = make_table(Input),
    CSV = fileconverter:table_to_csv(TransformTable),

    % Factorization JSON table
    FactorTable = factorization_table(Input),
    Json = fileconverter:json_encode_factors(FactorTable),

    io:format("CSV Output:~n~s~n", [CSV]),
    io:format("JSON Output:~n~s~n", [Json]).

