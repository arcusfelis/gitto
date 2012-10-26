-module(gitto_syntax).
-compile([export_all]).


ex() ->
    {ok, Fd} = file:open("src/gitto_store.erl", []),
    Attrs = parse_attributes(Fd),
    PTs = lists:map(fun find_parse_transform_attr/1, Attrs),
    io:format(user, "~nPTs: ~p~n", [PTs]),
    Libs = lists:map(fun find_include_lib_attr/1, Attrs),
    io:format(user, "~nLibs: ~p~n", [Libs]).


parse_attributes(Fd) ->
    F = fun is_attribute_form/1,
    parse_form_while(F, Fd, 1).

parse_form_while(F, Fd, StartLine) ->
    {ok, Tree, StartLine2} = epp_dodger:parse_form(Fd, StartLine),
    case F(Tree) of
        true -> [Tree|parse_form_while(F, Fd, StartLine2)];
        false -> []
    end.

is_attribute_form(Form) ->
    Type = erl_syntax:type(Form),
    io:format(user, "~2nForm of type ~p: ~n~p", [Type, Form]),
    case Type of
        attribute -> true;
        _ -> false
    end.


%% ----------------------------------------------------------------

find_parse_transform_attr(Form) ->
    case is_attribute_form(Form, compile) of
        false -> [];
        true ->
            Args = as_list(to_term(erl_syntax:attribute_arguments(Form))),
            io:format(user, "~nArguments: ~p~n", [Args]),
            [PTName
             || {parse_transform, PTName} <- Args]
    end.

as_list(X) when is_list(X) -> X;
as_list(X) -> [X].


is_attribute_form(Form, Name) ->
    try
        erl_syntax:atom_value(erl_syntax:attribute_name(Form)) =:= Name
    catch error:_ ->
        false
    end.


to_term(Expr) ->
    {value, Value, _} = erl_eval:exprs(revert_tree(Expr), []),
    Value.

revert_tree(Tree) ->
    [erl_syntax:revert(T) || T <- lists:flatten(Tree)].


%% ----------------------------------------------------------------

find_include_lib_attr(Form) ->
    case is_attribute_form(Form, include_lib) of
        false -> [];
        true ->
            Path = to_term(erl_syntax:attribute_arguments(Form)),
            [unicode:characters_to_binary(Path)]
    end.
