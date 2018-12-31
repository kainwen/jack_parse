-module(jack_parse).

-export([scan_and_parse/3]).

-export_type([class/0]).

scan_and_parse(SourceFile, TokenOutFile, ParseOutFile) ->
    Toks = tokenize_file(SourceFile, TokenOutFile),
    parse(Toks, ParseOutFile).

tokenize_file(SourceFile, TokenOutFile) ->
    {ok, Data} = file:read_file(SourceFile),
    Code = string:concat(binary_to_list(Data), "\n"),
    {ok, Toks_with_comment, _} = jack_tok:string(Code),
    Toks = [Tok || Tok <- Toks_with_comment, Tok /= {comment}],
    dump_token(Toks, TokenOutFile),
    Toks.

parse(Toks, ParseOutFile) ->
    {Class, []} = parse_class(Toks),
    dump_class(Class, ParseOutFile).
%%
dump_class(Class, ParseOutFile) ->
    Xmls = ["<class>"] ++
           class_to_xml(Class) ++
           ["</class>"],
    Text = string:join(Xmls, "\n"),
    file:write_file(ParseOutFile, list_to_binary(Text)).

dump_token(Toks, TokenOutFile) ->
    Xmls = ["<tokens>"] ++
           [tok_to_xml(Tok) || Tok <- Toks] ++ 
           ["</tokens>"],
    Text = string:join(Xmls, "\n"),
    file:write_file(TokenOutFile, list_to_binary(Text)).

tok_to_xml({T, Tok}) when is_atom(Tok);
                          is_integer(Tok);
                          is_list(Tok) ->
    Fn = get_type_convert_func(Tok),
    enclose_tag(T, Fn(Tok)).

get_type_convert_func(T) ->
    case is_atom(T) of
        true ->
            fun (X) -> atom_to_list(X) end;
        false ->
            case is_integer(T) of
                true ->
                    fun (X) -> integer_to_list(X) end;
                false ->
                    fun (X) -> X end
            end
    end.

safe_replace(S) ->
    S0 = string:join(string:replace(S, "&", "&amp;"), ""),
    S1 = string:join(string:replace(S0, ">", "&gt;"), ""),
    S2 = string:join(string:replace(S1, "<", "&lt;"), ""),
    string:join(string:replace(S2, "\"", "&quot;"), "").

%%
parse_multi(Toks, Fun, Delim) ->
    parse_multi(Toks, Fun, Delim, []).

parse_multi(Toks, Fun, Delim, Acc) ->
    try Fun(Toks) of
        {A, R} ->
            case Delim of
                nil ->
                    parse_multi(R, Fun, Delim, [A|Acc]);
                _ ->
                    case R of
                        [Delim|R1] ->
                            parse_multi(R1, Fun, Delim, [A|Acc]);
                        _ ->
                            {lists:reverse([A|Acc]), R}
                    end
            end
    catch
        _:_ ->
            {lists:reverse(Acc), Toks}
    end.

%%
parse_class(Toks) ->
    [{keyword, class}, {identifier, ClassName}, {symbol, '{'}|R1] = Toks,
    {ClassVarDecs, R2} = parse_classvar_decs(R1),
    {Subroutines, [{symbol, '}'}|R3]} = parse_subroutines(R2),
    {build_class(ClassName, ClassVarDecs, Subroutines), R3}.

parse_classvar_decs(Toks) ->    
    parse_multi(Toks, fun parse_classvar_dec/1, {symbol, ';'}).

parse_classvar_dec([{keyword, Qualifier}|Toks]) when Qualifier =:= static;
                                                     Qualifier =:= field ->
    {Type, R0} = parse_type(Toks),
    {Ids, R} = parse_multi(R0, fun parse_id/1, {symbol, ','}),
    {build_classvar_dec(Qualifier, Type, Ids), R}.

parse_subroutines(Toks) ->
    parse_multi(Toks, fun parse_subroutine/1, nil).

parse_subroutine([SubRoutineType, ReturnType, {identifier, SubRoutineName}, {symbol, '('}|Toks]) ->
    SRT = parse_subroutine_type(SubRoutineType),
    RT = parse_return_type(ReturnType),
    {Args, [{symbol, ')'}, {symbol, '{'}|R1]} = parse_multi(Toks, fun parse_arg/1, {symbol, ','}),
    {LocalVarDecs, R2} = parse_multi(R1, fun parse_local_var_dec/1, {symbol, ';'}),
    {Statements, [{symbol, '}'}|R3]} = parse_multi(R2, fun parse_statement/1, nil),
    {build_subroutine(SRT, RT, SubRoutineName, Args, LocalVarDecs, Statements), R3}.

parse_id([{identifier, Id}|RemToks]) -> {Id, RemToks}.

parse_arg(Toks) ->
    {Type, [{identifier, Arg}|R]} = parse_type(Toks),
    {build_arg(Type, Arg), R}.

parse_type([{keyword, void}|R]) -> {{void_type}, R};
parse_type([{keyword, Tp}|R]) ->
    Types = [int, char, boolean],
    case lists:member(Tp, Types) of
        true -> {{base_type, Tp}, R};
        false -> erlang:error({"want a type of {int, boolean, char} but found", Tp})
    end;
parse_type([{identifier, Tp}|R]) -> {{class_type, Tp}, R}.

parse_local_var_dec([{keyword, var}|Toks]) ->
    {Type, R0} = parse_type(Toks),
    {Vars, R} = parse_multi(R0, fun parse_id/1, {symbol, ','}),
    {build_local_var_dec(Type, Vars), R}.

parse_statement(Toks=[{keyword, 'if'}|_]) ->
    parse_if_statement(Toks);
parse_statement(Toks=[{keyword, 'do'}|_]) ->
    parse_do_statement(Toks);
parse_statement(Toks=[{keyword, 'let'}|_]) ->
    parse_let_statement(Toks);
parse_statement(Toks=[{keyword, 'while'}|_]) ->
    parse_while_statement(Toks);
parse_statement(Toks=[{keyword, 'return'}|_]) ->
    parse_return_statement(Toks).

parse_if_statement([{keyword, 'if'}, {symbol, '('}|R]) ->
    {Condition, [{symbol, ')'}, {symbol, '{'}|R1]} = parse_exp(R),
    {ThenBody, [{symbol, '}'}|R2]} = parse_multi(R1, fun parse_statement/1, nil),
    case R2 of
        [{keyword, 'else'}, {symbol, '{'}|R3] ->
            {ElseBody, [{symbol, '}'}|R4]} = parse_multi(R3, fun parse_statement/1, nil),
            {build_if_statement(Condition, ThenBody, ElseBody), R4};
        _ ->
            {build_if_statement(Condition, ThenBody), R2}
    end.

-type do_statement() :: {do_statement, subroutinecall()}.
parse_do_statement([{keyword, 'do'}|Toks]) ->
    {SubRoutineCall, [{symbol, ';'}|R]} = parse_subroutinecall(Toks),
    {{do_statement, SubRoutineCall}, R}.

parse_let_statement([{keyword, 'let'}|Toks]) ->
    [{identifier, Var}|R] = Toks,
    case R of
        [{symbol, '='}|R1] ->
            {Exp, [{symbol, ';'}|R2]} = parse_exp(R1),
            {build_let_statement(Var, Exp), R2};
        [{symbol, '['}|R1] ->
            {IdxExp, [{symbol, ']'}, {symbol, '='}|R2]} = parse_exp(R1),
            {Exp, [{symbol, ';'}|R3]} = parse_exp(R2),
            {build_let_statement(Var, IdxExp, Exp), R3}
    end.

parse_while_statement([{keyword, 'while'}, {symbol, '('}|Toks]) ->
    {StopCondition, [{symbol, ')'}, {symbol, '{'}|R]} = parse_exp(Toks),
    {WhileBody, [{symbol, '}'}|R1]} = parse_multi(R, fun parse_statement/1, nil),
    {build_while_statement(StopCondition, WhileBody), R1}.

parse_return_statement([{keyword, 'return'}|Toks]) ->
    case Toks of
        [{symbol, ';'}|R] ->
            {build_return_statement(), R};
        _ ->
            {Exp, [{symbol, ';'}|R]} = parse_exp(Toks),
            {build_return_statement(Exp), R}
    end.

parse_exp(Toks) ->
    parse_exp(Toks, [], []).

parse_exp(Toks, Terms, Ops) ->
    {Term, R} = parse_term(Toks),
    case R of
        [{symbol, Op}|R1] when Op =:= '+';
                               Op =:= '-';
                               Op =:= '*';
                               Op =:= '/';
                               Op =:= '<';
                               Op =:= '>';
                               Op =:= '=';
                               Op =:= '|';
                               Op =:= '&'->
            parse_exp(R1, [Term|Terms], [Op|Ops]);
        _ ->
            {build_exp(lists:reverse([Term|Terms]), lists:reverse(Ops)), R}
    end.

parse_term([{symbol, '('}|RemToks]) ->
    {Exp, [{symbol, ')'}|R]} = parse_exp(RemToks),
    {build_term_from_exp(Exp), R};
parse_term([Term={integerConstant, _}|RemToks]) -> {Term, RemToks};
parse_term([Term={stringConstant, _}|RemToks]) -> {Term, RemToks};
parse_term([{keyword, true}|RemToks]) -> {{const, true}, RemToks};
parse_term([{keyword, false}|RemToks]) -> {{const, false}, RemToks};
parse_term([{keyword, null}|RemToks]) -> {{const, null}, RemToks};
parse_term([{keyword, this}|RemToks]) -> {{const, this}, RemToks};
parse_term([{identifier, Var}, {symbol, '['}|RemToks]) ->
    {IdxExp, [{symbol, ']'}|R]} = parse_exp(RemToks),
    {build_array_ref_term(Var, IdxExp), R};
parse_term(Toks=[{identifier, _Var}, {symbol, '.'}|_]) ->
    parse_subroutinecall(Toks);
parse_term([{identifier, Var}|RemToks]) ->
    {build_var_term(Var), RemToks};
parse_term([{symbol, Op}|RemToks]) when Op =:= '-';
                                        Op =:= '~' ->
    {Term, R} = parse_term(RemToks),
    {build_unary_term(Op, Term), R};
parse_term(Toks) ->
    parse_subroutinecall(Toks).

parse_subroutine_type({keyword, Type}) when Type =:= constructor;
                                            Type =:= method;
                                            Type =:= function ->
    {subroutine_type, Type}.

parse_return_type({keyword, void}) ->
    {subroutine_return_type, {void_type}};
parse_return_type({keyword, Tp}) ->
    Types = [int, char, boolean],
    case lists:member(Tp, Types) of
        true -> {subroutine_return_type, {base_type, Tp}};
        false -> erlang:error({"want a type of {int, boolean, char} but found", Tp})
    end;
parse_return_type({identifier, Tp}) ->
    {subroutine_return_type, {class_type, Tp}}.

parse_subroutinecall([{identifier, Name1}, {symbol, '.'}, {identifier, Name2}, {symbol, '('}|R]) ->
    {Args, [{symbol, ')'}|R1]} = parse_multi(R, fun parse_exp/1, {symbol, ','}),
    {build_subroutinecall(Name1, Name2, Args), R1};
parse_subroutinecall([{identifier, Name}, {symbol, '('}|R]) ->
    {Args, [{symbol, ')'}|R1]} = parse_multi(R, fun parse_exp/1, {symbol, ','}),
    {build_subroutinecall(Name, Args), R1}.

%% constructors
-type class() :: {class,
                  ClassName::atom(),
                  ClassVarDecs::[classvar_dec()],
                  Subroutines::[subroutine()]}.
build_class(ClassName, ClassVarDecs, Subroutines) ->
    {class, ClassName, ClassVarDecs, Subroutines}.

-type classvar_dec() :: {classvar_dec,
                         qualifier(),
                         jack_type(),
                         [atom()]}.
-type qualifier() :: static | field.
-type jack_type() :: {void_type}
                   | {base_type, int}
                   | {base_type, char}
                   | {base_type, boolean}
                   | {class_type, atom()}.
build_classvar_dec(Qualifier, Type, Ids) ->
    {classvar_dec, Qualifier, Type, Ids}.

-type subroutine() :: {subroutine,
                       subroutine_type(),
                       subroutine_return_type(),
                       atom(),
                       [arg()],
                       [local_var_dec()],
                       [statement()]}.
-type subroutine_type() :: constructor | method | function.
-type subroutine_return_type() :: atom().
build_subroutine(SubRoutineType, ReturnType, Name, Args, LocalVarDecs, Body) ->
    {subroutine, SubRoutineType, ReturnType, Name, Args, LocalVarDecs, Body}.

-type arg() :: {arg, jack_type(), atom()}.
build_arg(Type, Arg) ->
    {arg, Type, Arg}.

-type local_var_dec() :: {local_var_dec, jack_type(), [atom()]}.
build_local_var_dec(Tp, Vars) ->
    {local_var_dec, Tp, Vars}.

-type statement() :: if_statement()
                   | let_statement()
                   | return_statement()
                   | while_statement()
                   | do_statement().

-type else_type() :: nil | [statement()].
-type if_statement() :: {if_statement,
                         Condition::exp(),
                         Then::[statement()],
                         Else::else_type()}.
build_if_statement(Condition, Then, Else) ->
    {if_statement, Condition, Then, Else}.
build_if_statement(Condition, Then) ->
    {if_statement, Condition, Then, nil}.

-type array_ref() :: {array_ref, atom(), exp()}.
build_array_ref(Array, IdxExp) ->
    {array_ref, Array, IdxExp}.

-type let_statement() :: {let_statement,
                          Left::left(),
                          Right::exp()}.
-type left() :: array_ref() | var().
build_let_statement(Var, IdxExp, Exp) ->
    ArrayRef = build_array_ref(Var, IdxExp),
    {let_statement, ArrayRef, Exp}.
-type var() :: {var, atom()}.
build_let_statement(Var, Exp) ->
    {let_statement, {var, Var}, Exp}.

-type while_statement() :: {while,
                             exp(),
                             [statement()]}.
build_while_statement(StopCondition, Body) ->
    {while_statement, StopCondition, Body}.

-type return_statement() :: {return, return_val()}.
-type return_val() :: nil | exp().
build_return_statement() ->
    {return, nil}.

build_return_statement(Exp) ->
    {return, Exp}.

-type op() :: '+' | '-' | '*' | '/' | '&' | '|' | '=' | '>' | '<'.
-type exp() :: {exp, Terms::[term()], Ops::[op()]}.
build_exp(Terms, Ops) ->
    {exp, Terms, Ops}.

-type jack_term() :: {integerConstant, integer()}
                   | {stringConstant, string()}
                   | {const, true}
                   | {const, false}
                   | {const, null}
                   | {const, this}
                   | array_ref()
                   | var()
                   | unary_term()
                   | subroutinecall().

build_array_ref_term(Var, IdxExp) ->
    build_array_ref(Var, IdxExp).

build_var_term(Var) ->
    {var, Var}.

build_term_from_exp(Exp) ->
    {exp_term, Exp}.

-type unary_term() :: {unary, unary_op(), jack_term()}.
-type unary_op() :: '~' | '-'.
build_unary_term(Op, Term) ->
    {unary, Op, Term}.

-type subroutinecall() :: {subroutinecall,
                           NameSpace::atom(),
                           FuncName::atom(),
                           Args::[exp()]}.
build_subroutinecall(Name1, Name2, Args) ->
    {subroutinecall, Name1, Name2, Args}.

build_subroutinecall(Name1, Args) ->
    {subroutinecall, nil, Name1, Args}.

%% dump class
enclose_tag(Tag, Content) ->
    TagStr = atom_to_list(Tag),
    SafeContent = safe_replace(Content),
    OpenTag = string:join(["<", TagStr, ">"], ""),
    CloseTag = string:join(["</", TagStr, ">"], ""),
    string:join([OpenTag, SafeContent, CloseTag], " ").

flatten(LList) ->
    lists:foldl(fun (E, Acc) -> Acc ++ E end, [], LList).

class_to_xml({class, ClassName, ClassVarDecs, Subroutines}) ->
    [
     enclose_tag(keyword, atom_to_list(class)),
     enclose_tag(identifier, atom_to_list(ClassName)),
     enclose_tag(symbol, "{") %class {
    ] ++
    flatten([classvardec_to_xml(ClassVarDec) || ClassVarDec <- ClassVarDecs]) ++
    flatten([subroutine_to_xml(Subroutine) || Subroutine <- Subroutines]) ++
    [
     enclose_tag(symbol, "}") %class }
    ].

classvardec_to_xml({classvar_dec, Qualifier, Type, Ids}) ->
    {TypeAtom, TypeStr} = case Type of
                              {base_type, T} ->
                                  {keyword, atom_to_list(T)};
                              {class_type, C} ->
                                  {identifier, atom_to_list(C)}
                          end,
    [
     "<classVarDec>",
     enclose_tag(keyword, atom_to_list(Qualifier)),
     enclose_tag(TypeAtom, TypeStr)
    ]++
    lists:join("<symbol> , </symbol>",
               [enclose_tag(identifier, atom_to_list(Id)) || Id <- Ids]) ++
    [
     enclose_tag(symbol, ";"),
     "</classVarDec>"
    ].

subroutine_to_xml({subroutine, SubRoutineType, ReturnType,
                   Name, Args, LocalVarDecs, Body}) ->
    {subroutine_type, SRT} = SubRoutineType,
    {subroutine_return_type, RT} = ReturnType,
    {ReturnTypeAtom, ReturnTypeStr} = case RT of
                                          {void_type} -> 
                                              {keyword, "void"};
                                          {base_type, T} ->
                                              {keyword, atom_to_list(T)};
                                          {class_type, C} ->
                                              {identifier, atom_to_list(C)}
                                      end,
    [
     "<subroutineDec>",
     enclose_tag(keyword, atom_to_list(SRT)),
     enclose_tag(ReturnTypeAtom, ReturnTypeStr),
     enclose_tag(identifier, atom_to_list(Name)),
     enclose_tag(symbol, "("),
     "<parameterList>"
    ] ++
    args_to_xml(Args) ++
    [
     "</parameterList>",
     "<symbol> ) </symbol>",
     "<subroutineBody>",
     "<symbol> { </symbol>"
    ] ++
    subroutinebody_to_xml(LocalVarDecs, Body) ++
    [
     "<symbol> } </symbol>",
     "</subroutineBody>",
     "</subroutineDec>"
    ].

args_to_xml(Args) ->
    args_to_xml(Args, []).

args_to_xml([], Acc) -> lists:reverse(Acc);
args_to_xml([{arg, Type, Arg}|Args], Acc) ->
    {TypeAtom, TypeStr} = case Type of
                              {base_type, T} ->
                                  {keyword, atom_to_list(T)};
                              {class_type, C} ->
                                  {identifier, atom_to_list(C)}
                          end,
    New = [enclose_tag(TypeAtom, TypeStr), enclose_tag(identifier, atom_to_list(Arg))],
    case Args of
        [] ->
            args_to_xml([], lists:reverse(New) ++ Acc);
        _ ->
            New1 = New ++ ["<symbol> , </symbol>"],
            args_to_xml(Args, lists:reverse(New1) ++ Acc)
    end.

subroutinebody_to_xml(LocalVarDecs, Body) ->
    flatten([localvardec_to_xml(LocalVarDec) || LocalVarDec <- LocalVarDecs]) ++
    ["<statements>"] ++
    flatten([statement_to_xml(Statement) || Statement <- Body]) ++
    ["</statements>"].
    
localvardec_to_xml({local_var_dec, Type, Vars}) ->
    {TypeAtom, TypeStr} = case Type of
                              {base_type, T} ->
                                  {keyword, atom_to_list(T)};
                              {class_type, C} ->
                                  {identifier, atom_to_list(C)}
                          end,
    [
     "<varDec>",
     "<keyword> var </keyword>",
     enclose_tag(TypeAtom, TypeStr)
    ] ++
    lists:join("<symbol> , </symbol>",
               [enclose_tag(identifier, atom_to_list(Var)) || Var <- Vars]) ++
    [
     "<symbol> ; </symbol>",
     "</varDec>"
    ].
        
statement_to_xml({if_statement, Condition, Then, Else}) ->
    [
     "<ifStatement>",
     "<keyword> if </keyword>",
     "<symbol> ( </symbol>"
    ] ++
    exp_to_xml(Condition) ++
    [
     "<symbol> ) </symbol>",
     "<symbol> { </symbol>",
     "<statements>"
    ] ++
    flatten([statement_to_xml(Statement) || Statement <- Then]) ++
    [
     "</statements>",
     "<symbol> } </symbol>"
    ] ++
    case Else of
        nil ->
            [];
        _ ->
            [
             "<keyword> else </keyword>",
             "<symbol> { </symbol>",
             "<statements>"
            ] ++
            flatten([statement_to_xml(Statement) || Statement <- Else]) ++
            [
             "</statements>",
             "<symbol> } </symbol>"
            ]
    end ++
    [
     "</ifStatement>"
    ];
statement_to_xml({do_statement, SubroutineCall}) ->
    [
     "<doStatement>",
     "<keyword> do </keyword>"
    ] ++
    subroutinecall_to_xml(SubroutineCall) ++
    [
     "<symbol> ; </symbol>",
     "</doStatement>"
    ];
statement_to_xml({let_statement, Left, Exp}) ->
    L = case Left of
            {var, Var} ->
                [enclose_tag(identifier, atom_to_list(Var))];
            {array_ref, Array, IdxExp} ->
                [
                 enclose_tag(identifier, atom_to_list(Array)),
                 "<symbol> [ </symbol>"
                ] ++
                exp_to_xml(IdxExp) ++
                ["<symbol> ] </symbol>"]
        end,
    [
     "<letStatement>",
     "<keyword> let </keyword>"
    ] ++
    L ++
    ["<symbol> = </symbol>"] ++
    exp_to_xml(Exp) ++
    [
     "<symbol> ; </symbol>",
     "</letStatement>"
    ];
statement_to_xml({while_statement, StopCondition, Body}) ->
    [
     "<whileStatement>",
     "<keyword> while </keyword>",
     "<symbol> ( </symbol>"
    ] ++
    exp_to_xml(StopCondition) ++
    [
     "<symbol> ) </symbol>",
     "<symbol> { </symbol>",
     "<statements>"
    ] ++
    flatten([statement_to_xml(Statement) || Statement <- Body]) ++
    [
     "</statements>",
     "<symbol> } </symbol>",
     "</whileStatement>"
    ];
statement_to_xml({return, E}) ->
    V = case E of
            nil ->
                [];
            _ ->
                exp_to_xml(E)
        end,
    [
     "<returnStatement>",
     "<keyword> return </keyword>"
    ] ++
    V ++
    [
     "<symbol> ; </symbol>",
     "</returnStatement>"
    ].

opterm_to_xml(Op, T) ->
    [
     enclose_tag(symbol, atom_to_list(Op))
    ] ++
    term_to_xml(T).

exp_to_xml({exp, [Term|Terms], Ops}) ->
    ["<expression>"] ++
    term_to_xml(Term) ++
    flatten([opterm_to_xml(Op, T) || {Op, T} <- lists:zip(Ops, Terms)]) ++
    ["</expression>"].

term_to_xml({integerConstant, Int}) ->
    [
     "<term>",
     enclose_tag(integerConstant, integer_to_list(Int)),
     "</term>"
    ];
term_to_xml({stringConstant, S}) ->
    [
     "<term>",
     enclose_tag(stringConstant, S),
     "</term>"
    ];
term_to_xml({const, V}) ->
    [
     "<term>",
     enclose_tag(keyword, atom_to_list(V)),
     "</term>"
    ];
term_to_xml({array_ref, Array, IdxExp}) ->
    [
     "<term>",
     enclose_tag(identifier, atom_to_list(Array)),
     "<symbol> [ </symbol>"
    ] ++
    exp_to_xml(IdxExp) ++
    [
     "<symbol> ] </symbol>",
     "</term>"
    ];
term_to_xml({var, Var}) ->
    [
     "<term>",
     enclose_tag(identifier, atom_to_list(Var)),
     "</term>"
    ];
term_to_xml({unary, Op, Term}) ->
    [
     "<term>",
     enclose_tag(symbol, atom_to_list(Op))
    ] ++
    term_to_xml(Term)
    ++ 
    [
     "</term>"
    ];
term_to_xml({exp_term, Exp}) ->
    [
     "<term>",
     "<symbol> ( </symbol>"
    ] ++
    exp_to_xml(Exp) ++
    [
     "<symbol> ) </symbol>",
     "</term>"
    ];
term_to_xml(SubroutineCall={subroutinecall, _, _, _}) ->
    ["<term>"] ++
    subroutinecall_to_xml(SubroutineCall) ++
    ["</term>"].

subroutinecall_to_xml({subroutinecall, Name1, Name2, Args}) ->
    Func = case Name1 of
               nil ->
                   [enclose_tag(identifier, atom_to_list(Name2))];
               _ ->
                   [
                    enclose_tag(identifier, atom_to_list(Name1)),
                    "<symbol> . </symbol>",
                    enclose_tag(identifier, atom_to_list(Name2))
                   ]
           end,                 
    Func ++
    [
     "<symbol> ( </symbol>",
     "<expressionList>"
    ] ++
    explist_to_xml(Args) ++
    [
     "</expressionList>",
     "<symbol> ) </symbol>"
    ].

explist_to_xml(Exps) -> explist_to_xml(Exps, []).

explist_to_xml([], Acc) -> Acc;    
explist_to_xml([Exp|Exps], Acc) ->
    ToAdd = exp_to_xml(Exp),
    case Exps of
        [] ->
            explist_to_xml([], Acc++ToAdd);
        _ ->
            explist_to_xml(Exps, Acc++ToAdd++["<symbol> , </symbol>"])
    end.

