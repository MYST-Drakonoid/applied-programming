
-module(fileconverter).
-export([table_to_csv/1, json_encode_factors/1]).


% -export to json

encode_map(#{number := N, factors := F}) ->
    io_lib:format("{\"number\":~p, \"factors\":~p}", [N, F]).


json_encode_factors(List) ->
    Encoded = lists:map(fun encode_map/1, List),
    Flat = [lists:flatten(E) || E <- Encoded],
    "[" ++ string:join(Flat, ",") ++ "]".


% export to CSV

row_to_csv(Row) ->
    Values = [to_string(V) || V <- Row],
    string:join(Values, ",").


to_string(V) when is_integer(V) ->
    integer_to_list(V);

to_string(true) -> "true";
to_string(false) -> "false";

to_string(V) ->
    lists:flatten(io_lib:format("~p", [V])).


table_to_csv(Table) ->
    CSV_Rows = [row_to_csv(Row) || Row <- Table],
    string:join(CSV_Rows, "\n").
