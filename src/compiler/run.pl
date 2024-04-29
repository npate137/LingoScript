% Author: Devanshu Desai, Nimit Patel
% version: 1.0
% purpose: This code defines a parser and evaluator for the programming language LingoScript.
% Date: 04/24/2024

:- table main_block/2.

ls(Lexername) :-
    PossiblePythonCmds = ['python', 'python3.', 'py'],
    member(PythonCmd, PossiblePythonCmds),
    process_create(path(PythonCmd), [Lexername], [stdout(pipe(In))]),
    read_string(In, _, X),
    write(X),
    term_to_atom(Y, X),
    write('LingoScript Programming Language v1.0'), nl,
    write('Spring 2024 - SER 502 - Team 22'), nl,
    program(Tree, Y, []),
    write('List of Tokens:'), nl, write(Y),nl, nl,
    write('Parse Tree:'), nl, write(Tree),nl, nl, write('Output:'), nl,
    program_eval(Tree, [], Output),
    write(Output).


program(main(CodeBlock)) --> [main], block(CodeBlock).

program_eval(Prog, EnvIn, EnvOut) :-
    program_evaluation(Prog, EnvIn, EnvOut),
    write(EnvOut).

program_evaluation(main(CodeBlock), EnvIn, EnvOut) :-
    block_eval(CodeBlock, EnvIn, EnvOut).


block(empty()) --> ['{'], ['}'].
block(code_block(CodeBlock)) --> ['{'], sub_block(CodeBlock),['}'].

block_eval(code_block(SubBlock), EnvIn, EnvOut):-
    sub_block_eval(SubBlock, EnvIn, EnvOut).

block_eval(empty(), EnvIn, EnvIn).

sub_block(sub_block(Decl)) --> declaration(Decl).
sub_block(sub_block(Decl, Rest)) --> declaration(Decl), sub_block(Rest).
sub_block(sub_block(Cmd, Rest)) --> noLeftRecurCmd(Cmd), sub_block(Rest).
sub_block(sub_block(Cmd)) --> noLeftRecurCmd(Cmd).
sub_block([]) --> [].

sub_block_eval(sub_block(Decl, Rest), EnvIn, EnvOut) :- declaration_eval(Decl, EnvIn, EnvTemp), sub_block_eval(Rest, EnvTemp, EnvOut).
sub_block_eval(sub_block(Cmd, Rest), EnvIn, EnvOut) :- cmd_eval(Cmd, EnvIn, EnvTemp), sub_block_eval(Rest, EnvTemp, EnvOut).
sub_block_eval(sub_block(Decl), EnvIn, EnvOut) :- declaration_eval(Decl, EnvIn, EnvOut).
sub_block_eval(sub_block(Cmd), EnvIn, EnvOut) :- cmd_eval(Cmd, EnvIn, EnvOut).
sub_block_eval([], EnvOut, EnvOut).

declaration(Decl) --> constant_decl(Decl).
declaration(Decl) --> variable_decl(Decl).

declaration_eval(variable_decl(var, Datatype, Iden, ';', Rest), EnvIn, EnvOut) :-
    is_variable_declared(Iden, Datatype, EnvIn, EnvTemp),
    declaration_eval(Rest, EnvTemp, EnvOut).

declaration_eval(variable_decl(var, Datatype, Iden, ';'), EnvIn, EnvOut) :-
    is_variable_declared(Iden, Datatype, EnvIn, EnvOut).

declaration_eval(variable_decl(var, Datatype, Iden, =, Val, ';', Rest), EnvIn, EnvOut) :-
    is_variable_declared(Iden, Datatype, Val, EnvIn, EnvTemp),
    declaration_eval(Rest, EnvTemp, EnvOut).

declaration_eval(variable_decl(var, Datatype, Iden, =, Val, ';'), EnvIn, EnvOut) :-
    is_variable_declared(Iden, Datatype, Val, EnvIn, EnvOut).

declaration_eval(constant_decl(const, Datatype, Iden, =, Val, ;, Rest), EnvIn, EnvOut) :-
    is_const_declared(Iden, Datatype, Val, EnvIn, EnvTemp),
    declaration_eval(Rest, EnvTemp, EnvOut).

declaration_eval(constant_decl(const, Datatype, Iden, =, Val, ';'), EnvIn, EnvOut) :-
    is_const_declared(Iden, Datatype, Val, EnvIn, EnvOut).

constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;].
constant_decl(constant_decl(const, Dtype, Iden, =, Bool, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Bool), [;].
constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expr(Expr), [;].
constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;], declaration(Rest).
constant_decl(constant_decl(const, Dtype, Iden, =, Bool, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Bool), [;], declaration(Rest).
constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expr(Expr), [;], declaration(Rest).

is_const_declared(variable(Iden), datatype(Datatype), Val, EnvIn, [(Iden, const, Datatype, N) | EnvTemp]) :-
    eval_extract(Val, EnvIn, EnvTemp, N),
    \+ member((Iden, _, _, _), EnvTemp),
    ( valid_datatype_value(Datatype, N)
    -> true
    ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail
    ).

eval_extract(Val, EnvIn, EnvOut, N) :-
    extract_value(Val, EnvIn, EnvTemp, N),
    (var(EnvTemp) -> EnvOut = EnvIn; EnvOut = EnvTemp).

variable_decl(variable_decl(var, Dtype, Iden, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [;].
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;].
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Expr), [;].
variable_decl(variable_decl(var, Dtype, Iden, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [;], declaration(Rest).
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;], declaration(Rest).
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Expr), [;], declaration(Rest).

is_variable_declared(variable(Iden), datatype(Datatype), EnvIn, [(Iden, var, Datatype, _) | EnvIn]) :-
    (\+ member((Iden, _, _, _), EnvIn); throw(error(variable_already_declared, [Iden]))).

is_variable_declared(variable(Iden), datatype(Datatype), Val, EnvIn, [(Iden, var, Datatype, N) | EnvTemp]) :-
    eval_extract(Val, EnvIn, EnvTemp, N),
    (\+ member((Iden, _, _, _), EnvTemp)
    -> ( valid_datatype_value(Datatype, N)
    -> true ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail)
    ; throw(error(variable_already_declared, [Iden]))).

variable_datatype(datatype(int)) --> [int].
variable_datatype(datatype(bool)) --> [bool].
variable_datatype(datatype(str)) --> [str].

valid_datatype_value(int, Value) :-
    integer(Value).
valid_datatype_value(str, Value) :-
    string(Value).
valid_datatype_value(bool, Value) :-
    (Value == true ; Value == false).

extract_value(number(N), _, _, N).
extract_value(Bool, EnvIn, EnvOut, B) :- bool_eval(Bool, EnvIn, EnvOut, B).
extract_value(Expr, EnvIn, EnvOut, Num) :- eval_expr(Expr, EnvIn, EnvOut, Num).
extract_value(Expr, EnvIn, EnvOut, Num) :- eval_ternary_expr(Expr, EnvIn, EnvOut, Num).


noLeftRecurCmd(cmd_assign(T)) --> assignment(T), [;].
noLeftRecurCmd(cmd_assign(T,Cmd)) --> assignment(T), [;], noLeftRecurCmd(Cmd).

noLeftRecurCmd(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd).
noLeftRecurCmd(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd).
noLeftRecurCmd(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd).
noLeftRecurCmd(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd).

noLeftRecurCmd(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).
noLeftRecurCmd(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).
noLeftRecurCmd(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).
noLeftRecurCmd(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).

noLeftRecurCmd(for_loop_range(Iden, in, range, From, To, Jump, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(From), [','], simple_expr(To), [','], simple_expr(Jump), [')'], noLeftRecurCmd(Cmd).
noLeftRecurCmd(for_loop_range_single(Iden, in, range, To, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(To), [')'], noLeftRecurCmd(Cmd).

noLeftRecurCmd(for_loop_range(Iden, in, range, From, To, Jump, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(From), [','], simple_expr(To), [','], simple_expr(Jump), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).
noLeftRecurCmd(for_loop_range_single(Iden, in, range, To, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(To), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).

noLeftRecurCmd(if_else(if, Bool, Cmd, else, Cmd1)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd), [else], noLeftRecurCmd(Cmd1).
noLeftRecurCmd(if_else_if(if, Bool, Cmd, Rest)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd),  else_if_ladder(Rest).
noLeftRecurCmd(if(if, Bool, Cmd)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd).
noLeftRecurCmd(if_else(if, Bool, Cmd, else, Cmd1, Cmd2)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd), [else], noLeftRecurCmd(Cmd1), noLeftRecurCmd(Cmd2).
noLeftRecurCmd(if_else_if(if, Bool, Cmd, Rest, Cmd1)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd),  else_if_ladder(Rest), noLeftRecurCmd(Cmd1).
noLeftRecurCmd(if(if, Bool, Cmd, Cmd1)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).

noLeftRecurCmd(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_bracket_left, boolCondition(Bool), ['?'], cmdForTernary(Expr1), [:], cmdForTernary(Expr2), optional_bracket_right, [;], noLeftRecurCmd(Cmd).

noLeftRecurCmd(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_bracket_left, boolCondition(Bool), ['?'], cmdForTernary(Expr1), [:], cmdForTernary(Expr2), optional_bracket_right, [;].

noLeftRecurCmd(while_loop(Boolean, Cmd)) --> [while], ['('], boolCondition(Boolean), [')'], noLeftRecurCmd(Cmd).
noLeftRecurCmd(while_loop(Boolean, Cmd, Cmd1)) --> [while], ['('], boolCondition(Boolean), [')'], noLeftRecurCmd(Cmd), noLeftRecurCmd(Cmd1).

noLeftRecurCmd(print(Arg)) --> [print], print_statement(Arg), [;].
noLeftRecurCmd(print(Arg,Cmd)) --> [print], print_statement(Arg), [;],noLeftRecurCmd(Cmd).

noLeftRecurCmd(increment(Iden, +, +)) --> variable(Iden), [+], [+], [;].
noLeftRecurCmd(increment(Iden, +, +, Cmd)) --> variable(Iden), [+], [+], [;], noLeftRecurCmd(Cmd).
noLeftRecurCmd(decrement(Iden, -, -)) --> variable(Iden), [-], [-], [;].
noLeftRecurCmd(decrement(Iden, -, -, Cmd)) --> variable(Iden), [-], [-], [;], noLeftRecurCmd(Cmd).


% noLeftRecurCmd(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expr(Expr), [;].
% noLeftRecurCmd(add_(Iden, +, =, Expr, Cmd)) --> variable(Iden), [+], [=], simple_expr(Expr), [;], noLeftRecurCmd(Cmd).

% noLeftRecurCmd(sub_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expr(Expr), [;].
% noLeftRecurCmd(sub_(Iden, -, =, Expr, Cmd)) --> variable(Iden), [-], [=], simple_expr(Expr), [;], noLeftRecurCmd(Cmd).

% noLeftRecurCmd(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expr(Expr), [;].
% noLeftRecurCmd(multiply_(Iden, *, =, Expr, Cmd)) --> variable(Iden), [*], [=], simple_expr(Expr), [;], noLeftRecurCmd(Cmd).

% noLeftRecurCmd(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expr(Expr), [;].
% noLeftRecurCmd(divide_(Iden, /, =, Expr, Cmd)) --> variable(Iden), [/], [=], simple_expr(Expr), [;], noLeftRecurCmd(Cmd).

noLeftRecurCmd(cmd_block(Blk)) --> block(Blk).

update_variables([], EnvIn, EnvIn).
update_variables([(Var, Type, Datatype, Val)|Rest], EnvIn, UpdatedEnv) :-
    (select((Var, Type, Datatype, _), EnvIn, _) ->
        valid_datatype_value(Datatype, Val),
        replace(EnvIn, (Var, Type, Datatype, _), (Var, Type, Datatype, Val), NewTempEnv)
    ;
        NewTempEnv = EnvIn
    ),
    update_variables(Rest, NewTempEnv, UpdatedEnv).

replace_variable((Var, Type, Datatype, Val), [], [(Var, Type, Datatype, Val)]).
replace_variable((Var, Type, Datatype, Val), [(Var, Type, Datatype, _)|Rest], [(Var, Type, Datatype, Val)|Rest]).
replace_variable((Var, Type, Datatype, Val), [Head|Rest], [Head|NewRest]) :-
    Head \= (Var, Type, Datatype, _),
    replace_variable((Var, Type, Datatype, Val), Rest, NewRest).


eval_cmd_env(Cmd, EnvIn, EnvOut) :-
    cmd_eval(Cmd, EnvIn, EnvTemp),
    (var(EnvTemp) -> EnvOut = EnvIn; EnvOut = EnvTemp).

cmd_eval(cmd_assign(T), EnvIn, EnvOut) :-
    assignment_eval(T, EnvIn, EnvOut).

cmd_eval(cmd_assign(T, Cmd), EnvIn, EnvOut) :-
    assignment_eval(T, EnvIn, EnvTemp),
    cmd_eval(Cmd, EnvTemp, EnvOut).

cmd_eval(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd), EnvIn, EnvOut):-
    declaration_eval(Decl, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_eval(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd, Cmd1), EnvIn, EnvOut):-
    declaration_eval(Decl, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_eval(Cmd1, EnvTemp3, EnvOut).

cmd_eval(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd), EnvIn, EnvOut):-
    assignment_eval(Asg, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp3),
    update_variables(EnvTemp3, EnvIn, EnvOut).

cmd_eval(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1), EnvIn, EnvOut):-
    assignment_eval(Asg, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_eval(Cmd1, EnvTemp3, EnvOut).

cmd_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvOut).

cmd_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, From) | EnvTemp], EnvOut).

cmd_eval(for_loop_range(Iden, _, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').

cmd_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvTemp2),
    cmd_eval(Cmd1, EnvTemp2, EnvOut).

cmd_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, From) | EnvTemp], EnvTemp3),
    cmd_eval(Cmd1, EnvTemp3, EnvOut).

cmd_eval(for_loop_range(Iden, _, _, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').


cmd_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvOut).

cmd_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, 0) | EnvTemp], EnvOut).

cmd_eval(for_loop_range_single(Iden, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').

cmd_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvTemp2),
    cmd_eval(Cmd1, EnvTemp2, EnvOut).

cmd_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, 0) | EnvTemp], EnvTemp3),
    cmd_eval(Cmd1, EnvTemp3, EnvOut).

cmd_eval(for_loop_range_single(Iden, _, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').


% cmd_eval(ternary_operator(variable(Iden), Bool, '?', Expr1, :, Expr2, Cmd), EnvIn, EnvOut) :-
%     member((Iden, var, _, _), EnvIn),
%     bool_eval(Bool, EnvIn, EnvOut, BoolResult),
%     (BoolResult = true -> eval_ternary_expr(Expr1, EnvIn, EnvOut, Result)
%     ; eval_ternary_expr(Expr2, EnvIn, EnvOut, Result)).

cmd_eval(if(if, Bool, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_eval(if(if, Bool, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvOut, false).

cmd_eval(if(if, Bool, Cmd, Cmd1), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_eval(Cmd1, EnvTemp3, EnvOut).

cmd_eval(if(if, Bool, _, Cmd1), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    cmd_eval(Cmd1, EnvTemp, EnvOut).

cmd_eval(if_else(if, Bool, Cmd, else, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_eval(if_else(if, Bool, _, else, Cmd1),EnvIn,EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    cmd_eval(Cmd1, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_eval(if_else(if, Bool, Cmd, else, _, Cmd1), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_eval(Cmd1, EnvTemp3, EnvOut).

cmd_eval(if_else(if, Bool, _, else, Cmd1, Cmd2),EnvIn,EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    cmd_eval(Cmd1, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_eval(Cmd2, EnvTemp3, EnvOut).

cmd_eval(if_else_if(if, Bool, Cmd, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_eval(if_else_if(if, Bool, _, Rest), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    else_if_eval(Rest, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_eval(if_else_if(if, Bool, Cmd, _, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_eval(Cmd, EnvTemp3, EnvOut).

cmd_eval(if_else_if(if, Bool, _, Rest, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    else_if_eval(Rest, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_eval(Cmd, EnvTemp3, EnvOut).

cmd_eval(while_loop(Bool, Cmd), EnvIn, EnvOut):-
    while_eval(Bool, Cmd, EnvIn, EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvOut).

cmd_eval(while_loop(Bool, Cmd, Cmd1), EnvIn, EnvOut):-
    while_eval(Bool, Cmd, EnvIn, EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvTemp2),
    cmd_eval(Cmd1, EnvTemp2, EnvOut).

cmd_eval(print(Print), EnvIn, EnvOut):-
    eval_print_cmd_(Print, EnvIn, EnvOut).

cmd_eval(print(Print, Cmd), EnvIn, EnvOut):-
    eval_print_cmd_(Print, EnvIn, EnvTemp),
    cmd_eval(Cmd, EnvTemp, EnvOut).

cmd_eval(increment(Iden, +, +), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Increment is Value + 1,
    update_env(Iden, Increment, EnvTemp, EnvOut).

cmd_eval(increment(Iden, +, +, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Increment is Value + 1,
    update_env(Iden, Increment, EnvTemp, EnvTemp2),
    cmd_eval(Cmd, EnvTemp2, EnvOut).

cmd_eval(decrement(Iden, -, -), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Decrement is Value - 1,
    update_env(Iden, Decrement, EnvTemp, EnvOut).

cmd_eval(decrement(Iden, -, -, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Decrement is Value - 1,
    update_env(Iden, Decrement, EnvTemp, EnvTemp2),
    cmd_eval(Cmd, EnvTemp2, EnvOut).

cmd_eval(add_(Iden, +, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 + Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_eval(add_(Iden, +, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 + Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_eval(Cmd, EnvTemp2, EnvOut).

cmd_eval(sub_(Iden, -, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 - Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_eval(sub_(Iden, -, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 - Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_eval(Cmd, EnvTemp2, EnvOut).

cmd_eval(multiply_(Iden, *, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 * Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_eval(multiply_(Iden, *, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 * Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_eval(Cmd, EnvTemp2, EnvOut).

cmd_eval(divide_(Iden, /, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 / Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_eval(divide_(Iden, /, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 / Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_eval(Cmd, EnvTemp2, EnvOut).

cmd_eval(ternary_operator(Bool, '?', Expr1, :, Expr2), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, BoolResult),
    (BoolResult = true -> cmd_ternary_eval(Expr1, EnvTemp, EnvOut)
    ; cmd_ternary_eval(Expr2, EnvTemp, EnvOut)).

cmd_eval(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, BoolResult),
    (BoolResult = true -> cmd_ternary_eval(Expr1, EnvTemp, EnvTemp2)
    ; cmd_ternary_eval(Expr2, EnvTemp, EnvTemp2)),
    cmd_eval(Cmd, EnvTemp2, EnvOut).

cmd_eval(cmd_block(Block), EnvIn, EnvOut) :-
    block_eval(Block, EnvIn, EnvOut).

eval_print_cmd_(Print, EnvIn, EnvOut) :-
    eval_print_cmd(Print, EnvIn, EnvTemp),
    (var(EnvTemp) -> EnvOut = EnvIn; EnvOut = EnvTemp).

eval_print_cmd(print_string(string_(P)), EnvIn, EnvIn) :-
    write(P),
    nl.

eval_print_cmd(print_bool(Bool), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvOut, Result),
    write(Result),
    nl.

eval_print_cmd(print_expr(Expr), EnvIn, EnvOut) :-
    eval_expr_env(Expr, EnvIn, EnvOut, Result),
    write(Result),
    nl.

while_eval(Bool, Cmd, EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvTemp2),
    while_eval(Bool, Cmd, EnvTemp2, EnvOut).

while_eval(B, _, EnvIn, EnvOut) :-
    eval_bool_env(B, EnvIn, EnvOut, false).

else_if_eval(elif(elif, Bool, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_eval(Cmd, EnvTemp, EnvOut).

else_if_eval(elif(elif, Bool, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvOut, false).

else_if_eval(elif(elif, Bool, Cmd, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool,EnvIn,EnvTemp,true),
    cmd_eval(Cmd, EnvTemp, EnvOut).

else_if_eval(elif(elif, Bool, _, Rest), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp,false),
    else_if_eval(Rest, EnvTemp, EnvOut).

else_if_eval(elif(elif, Bool, Cmd, else, _),EnvIn,EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp,true),
    cmd_eval(Cmd, EnvTemp, EnvOut).

else_if_eval(elif(elif, Bool, _, else, Cmd1), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp,false),
    cmd_eval(Cmd1, EnvTemp, EnvOut).
% for_loop_range_eval(Iden, Current, To, Jump, Cmd, EnvIn, EnvOut) :-
%     Current < To,
%     cmd_eval(Cmd, EnvIn, EnvIn),
%     NewCurrent is Current + Jump,
%     update_iden_value(Iden, NewCurrent, EnvIn, UpdatedEnv),
%     for_loop_range_eval(Iden, NewCurrent, To, Jump, Cmd, UpdatedEnv, EnvOut).

for_loop_range_eval(Iden, Current, To, Jump, Cmd, EnvIn, EnvOut) :-
    Current < To,
    eval_cmd_env(Cmd, EnvIn, EnvTemp),
    NewCurrent is Current + Jump,
    update_iden_value(Iden, NewCurrent, EnvTemp, UpdatedEnv),
    update_variables(UpdatedEnv, EnvIn, EnvTemp4),
    for_loop_range_eval(Iden, NewCurrent, To, Jump, Cmd, EnvTemp4, EnvOut).

for_loop_range_eval(_, Current, To, _, _, EnvOut, EnvOut) :-
    Current >= To.

update_iden_value(Iden, Value, EnvOut, [(Iden, var, int, Value) | EnvTemp]) :-
    select((Iden, var, int, _), EnvOut, EnvTemp).

for_loop_eval(Bool, Expr, Cmd, EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, BoolValue),
    (   BoolValue == true
    ->  eval_cmd_env(Cmd, EnvTemp, EnvTemp2),
        (   is_assignment(Expr)
        ->  assignment_eval(Expr, EnvTemp2, EnvTemp3)
        ;
        eval_extract(Expr, EnvTemp2, EnvTemp3, _)
        ),
        update_variables(EnvTemp3, EnvIn, EnvTemp4),
        for_loop_eval(Bool, Expr, Cmd, EnvTemp4, EnvOut)
    ;   EnvOut = EnvIn
    ).

is_assignment(assign(variable(_), =, _)).
is_assignment(assign(variable(_), +, =, _)).
is_assignment(assign(variable(_), -, =, _)).
is_assignment(assign(variable(_), *, =, _)).
is_assignment(assign(variable(_), /, =, _)).


% for_loop_eval(Bool, _, _, EnvIn, EnvIn) :-
%     eval_bool_env(Bool, EnvIn, EnvIn, false).


ternary_expr(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_bracket_left(), boolCondition(Bool), ['?'], ternary_expr(Expr1), [:], ternary_expr(Expr2), optional_bracket_right().
ternary_expr(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_bracket_left(), variable(Iden), [=], boolCondition(Bool), ['?'], ternary_expr(Expr1), [:], ternary_expr(Expr2), optional_bracket_right().
ternary_expr(Expr) --> simple_expr(Expr).
ternary_expr(Bool) --> boolCondition(Bool).
optional_bracket_left() --> ['('].
optional_bracket_left() --> [].

optional_bracket_right() --> [')'].
optional_bracket_right() --> [].

eval_ternary_expr(Expr, EnvIn, EnvOut, Result) :-
    eval_expr_env(Expr, EnvIn, EnvOut, Result).

eval_ternary_expr(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2), EnvIn, EnvOut, Result) :-
    eval_expr_env(Iden, EnvIn, EnvTemp, _),
    eval_bool_env(Bool, EnvIn, EnvTemp, BoolResult),
    (BoolResult = true -> eval_ternary_expr(Expr1, EnvTemp, EnvOut, Result)
    ; eval_ternary_expr(Expr2, EnvTemp, EnvOut, Result)).

eval_ternary_expr(ternary_operator(Bool, '?', Expr1, :, Expr2), EnvIn, EnvOut, Result) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, BoolResult),
    (BoolResult = true -> eval_ternary_expr(Expr1, EnvTemp, EnvOut, Result)
    ; eval_ternary_expr(Expr2, EnvTemp, EnvOut, Result)).

else_if_ladder(elif(elif, Bool, Cmd)) --> [else, if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd).
else_if_ladder(elif(elif, Bool, Cmd, Rest)) --> [else, if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd), else_if_ladder(Rest).
else_if_ladder(elif(elif, Bool, Cmd, else, Cmd1)) --> [else, if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd), [else], noLeftRecurCmd(Cmd1).

cmdForTernary(cmd_assign(T)) --> assignment(T).
cmdForTernary(cmd_assign(T,Cmd)) --> assignment(T), [;], cmdForTernary(Cmd).

cmdForTernary(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd).
cmdForTernary(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd).
cmdForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd).
cmdForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd).

cmdForTernary(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).
cmdForTernary(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], declaration(Decl), boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).
cmdForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], simple_expr(Expr), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).
cmdForTernary(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1)) --> [for], ['('], assignment(Asg), [;], boolCondition(Bool), [;], assignment(Expr), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).

cmdForTernary(for_loop_range(Iden, in, range, From, To, Jump, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(From), [','], simple_expr(To), [','], simple_expr(Jump), [')'], noLeftRecurCmd(Cmd).
cmdForTernary(for_loop_range_single(Iden, in, range, To, Cmd)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(To), [')'], noLeftRecurCmd(Cmd).

cmdForTernary(for_loop_range(Iden, in, range, From, To, Jump, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(From), [','], simple_expr(To), [','], simple_expr(Jump), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).
cmdForTernary(for_loop_range_single(Iden, in, range, To, Cmd, Cmd1)) --> [for], variable(Iden), [in], [range], ['('], simple_expr(To), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).

cmdForTernary(if_else(if, Bool, Cmd, else, Cmd1)) --> [if], ['('], boolCondition(Bool), [')'], cmdForTernary(Cmd), [else], noLeftRecurCmd(Cmd1).
cmdForTernary(if_else_if(if, Bool, Cmd, Rest)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd),  else_if_ladder(Rest).
cmdForTernary(if(if, Bool, Cmd)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd).

cmdForTernary(if_else(if, Bool, Cmd, else, Cmd1, Cmd2)) --> [if], ['('], boolCondition(Bool), [')'], cmdForTernary(Cmd), [else], noLeftRecurCmd(Cmd1), cmdForTernary(Cmd2).
cmdForTernary(if_else_if(if, Bool, Cmd, Rest, Cmd1)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd),  else_if_ladder(Rest), cmdForTernary(Cmd1).
cmdForTernary(if(if, Bool, Cmd, Cmd1)) --> [if], ['('], boolCondition(Bool), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).

cmdForTernary(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_bracket_left, variable(Iden), [=], boolCondition(Bool), ['?'], ternary_expr(Expr1), [:], ternary_expr(Expr2), optional_bracket_right, [;], cmdForTernary(Cmd).
cmdForTernary(ternary_operator(Bool, '?', Expr1, :, Expr2, Cmd)) --> optional_bracket_left, boolCondition(Bool), ['?'], noLeftRecurCmd(Expr1), [:], noLeftRecurCmd(Expr2), optional_bracket_right, cmdForTernary(Cmd).
cmdForTernary(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_bracket_left, variable(Iden), [=], boolCondition(Bool), ['?'], ternary_expr(Expr1), [:], ternary_expr(Expr2), optional_bracket_right.
cmdForTernary(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_bracket_left, boolCondition(Bool), ['?'], cmdForTernary(Expr1), [:], cmdForTernary(Expr2), optional_bracket_right.


cmdForTernary(while_loop(Boolean, Cmd)) --> [while], ['('], boolCondition(Boolean), [')'], noLeftRecurCmd(Cmd).
cmdForTernary(while_loop(Boolean, Cmd, Cmd1)) --> [while], ['('], boolCondition(Boolean), [')'], noLeftRecurCmd(Cmd), cmdForTernary(Cmd1).

cmdForTernary(print(Arg)) --> [print], print_statement(Arg).
cmdForTernary(print(Arg, Cmd)) --> [print], print_statement(Arg), [;], cmdForTernary(Cmd).

cmdForTernary(increment(Iden, +, +)) --> variable(Iden), [+], [+].
cmdForTernary(increment(Iden, +, +, Cmd)) --> variable(Iden), [+], [+], [;], cmdForTernary(Cmd).

cmdForTernary(decrement(Iden, -, -)) --> variable(Iden), [-], [-].
cmdForTernary(decrement(Iden, -, -, Cmd)) --> variable(Iden), [-], [-], [;], cmdForTernary(Cmd).

cmdForTernary(add_(Iden, +, =, Expr)) --> variable(Iden), [+], [=], simple_expr(Expr).
cmdForTernary(add_(Iden, +, =, Expr, Cmd)) --> variable(Iden), [+], [=], simple_expr(Expr), [;], cmdForTernary(Cmd).

cmdForTernary(sub_(Iden, -, =, Expr)) --> variable(Iden), [-], [=], simple_expr(Expr).
cmdForTernary(sub_(Iden, -, =, Expr, Cmd)) --> variable(Iden), [-], [=], simple_expr(Expr), [;], cmdForTernary(Cmd).

cmdForTernary(multiply_(Iden, *, =, Expr)) --> variable(Iden), [*], [=], simple_expr(Expr).
cmdForTernary(multiply_(Iden, *, =, Expr, Cmd)) --> variable(Iden), [*], [=], simple_expr(Expr), [;], cmdForTernary(Cmd).

cmdForTernary(divide_(Iden, /, =, Expr)) --> variable(Iden), [/], [=], simple_expr(Expr).
cmdForTernary(divide_(Iden, /, =, Expr, Cmd)) --> variable(Iden), [/], [=], simple_expr(Expr), [;], cmdForTernary(Cmd).

eval_cmd_for_ternary_env(Cmd, EnvIn, EnvOut) :-
    cmd_ternary_eval(Cmd, EnvIn, EnvTemp),
    (var(EnvTemp) -> EnvOut = EnvIn; EnvOut = EnvTemp).

cmd_ternary_eval(cmd_assign(T), EnvIn, EnvOut) :-
    assignment_eval(T, EnvIn, EnvOut).

cmd_ternary_eval(cmd_assign(T, Cmd), EnvIn, EnvOut) :-
    assignment_eval(T, EnvIn, EnvTemp),
    cmd_ternary_eval(Cmd, EnvTemp, EnvOut).

cmd_ternary_eval(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd), EnvIn, EnvOut):-
    declaration_eval(Decl, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_ternary_eval(for_loop(for, '(', Decl, Bool, ;, Expr, ')', Cmd, Cmd1), EnvIn, EnvOut):-
    declaration_eval(Decl, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_ternary_eval(Cmd1, EnvTemp3, EnvOut).

cmd_ternary_eval(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd), EnvIn, EnvOut):-
    assignment_eval(Asg, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp3),
    update_variables(EnvTemp3, EnvIn, EnvOut).

cmd_ternary_eval(for_loop(for, '(', Asg, ;, Bool, ;, Expr, ')', Cmd, Cmd1), EnvIn, EnvOut):-
    assignment_eval(Asg, EnvIn, EnvTemp),
    for_loop_eval(Bool, Expr, Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_ternary_eval(Cmd1, EnvTemp3, EnvOut).

cmd_ternary_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvOut).

cmd_ternary_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, From) | EnvTemp], EnvOut).

cmd_ternary_eval(for_loop_range(Iden, _, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').

cmd_ternary_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvTemp2),
    cmd_ternary_eval(Cmd1, EnvTemp2, EnvOut).

cmd_ternary_eval(for_loop_range(variable(Iden), in, range, number(From), number(To), number(Jump), Cmd, Cmd1), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, From, To, Jump, Cmd, [(Iden, var, int, From) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, From) | EnvTemp], EnvTemp3),
    cmd_ternary_eval(Cmd1, EnvTemp3, EnvOut).

cmd_ternary_eval(for_loop_range(Iden, _, _, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').


cmd_ternary_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvOut).

cmd_ternary_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, 0) | EnvTemp], EnvOut).

cmd_ternary_eval(for_loop_range_single(Iden, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').

cmd_ternary_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), EnvIn, EnvOut) :-
    \+ member((Iden, var, int, _), EnvIn),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvIn], EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvTemp2),
    cmd_ternary_eval(Cmd1, EnvTemp2, EnvOut).

cmd_ternary_eval(for_loop_range_single(variable(Iden), in, range, number(To), Cmd, Cmd1), EnvIn, EnvOut) :-
    member((Iden, var, int, _), EnvIn),
    select((Iden, var, int, _), EnvIn, EnvTemp),
    for_loop_range_eval(Iden, 0, To, 1, Cmd, [(Iden, var, int, 0) | EnvTemp], EnvTemp2),
    update_variables(EnvTemp2, [(Iden, var, int, 0) | EnvTemp], EnvTemp3),
    cmd_ternary_eval(Cmd1, EnvTemp3, EnvOut).

cmd_ternary_eval(for_loop_range_single(Iden, _, _, _, _, _, _), EnvIn, _) :-
    member((Iden, var, Type, _), EnvIn),
    Type \= int,
    writeln('Error: Iden should be of type int').


% cmd_ternary_eval(ternary_operator(variable(Iden), Bool, '?', Expr1, :, Expr2, Cmd), EnvIn, EnvOut) :-
%     member((Iden, var, _, _), EnvIn),
%     bool_eval(Bool, EnvIn, EnvOut, BoolResult),
%     (BoolResult = true -> eval_ternary_expr(Expr1, EnvIn, EnvOut, Result)
%     ; eval_ternary_expr(Expr2, EnvIn, EnvOut, Result)).

cmd_ternary_eval(if(if, Bool, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_ternary_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_ternary_eval(if(if, Bool, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvOut, false).

cmd_ternary_eval(if(if, Bool, Cmd, Cmd1), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_ternary_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_ternary_eval(Cmd1, EnvTemp3, EnvOut).

cmd_ternary_eval(if(if, Bool, _, Cmd1), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    cmd_ternary_eval(Cmd1, EnvTemp, EnvOut).

cmd_ternary_eval(if_else(if, Bool, Cmd, else, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_ternary_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_ternary_eval(if_else(if, Bool, _, else, Cmd1),EnvIn,EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    cmd_ternary_eval(Cmd1, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_ternary_eval(if_else(if, Bool, Cmd, else, _, Cmd1), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_ternary_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_ternary_eval(Cmd1, EnvTemp3, EnvOut).

cmd_ternary_eval(if_else(if, Bool, _, else, Cmd1, Cmd2),EnvIn,EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    cmd_ternary_eval(Cmd1, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_ternary_eval(Cmd2, EnvTemp3, EnvOut).

cmd_ternary_eval(if_else_if(if, Bool, Cmd, _), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_ternary_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_ternary_eval(if_else_if(if, Bool, _, Rest), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    else_if_eval(Rest, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvOut).

cmd_ternary_eval(if_else_if(if, Bool, Cmd, _, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, true),
    cmd_ternary_eval(Cmd, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_ternary_eval(Cmd, EnvTemp3, EnvOut).

cmd_ternary_eval(if_else_if(if, Bool, _, Rest, Cmd), EnvIn, EnvOut) :-
    eval_bool_env(Bool, EnvIn, EnvTemp, false),
    else_if_eval(Rest, EnvTemp, EnvTemp2),
    update_variables(EnvTemp2, EnvIn, EnvTemp3),
    cmd_ternary_eval(Cmd, EnvTemp3, EnvOut).

cmd_ternary_eval(while_loop(Bool, Cmd), EnvIn, EnvOut):-
    while_eval(Bool, Cmd, EnvIn, EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvOut).

cmd_ternary_eval(while_loop(Bool, Cmd, Cmd1), EnvIn, EnvOut):-
    while_eval(Bool, Cmd, EnvIn, EnvTemp),
    update_variables(EnvTemp, EnvIn, EnvTemp2),
    cmd_ternary_eval(Cmd1, EnvTemp2, EnvOut).

cmd_ternary_eval(print(Print), EnvIn, EnvOut):-
    eval_print_cmd_(Print, EnvIn, EnvOut).

cmd_ternary_eval(print(Print, Cmd), EnvIn, EnvOut):-
    eval_print_cmd_(Print, EnvIn, EnvTemp),
    cmd_ternary_eval(Cmd, EnvTemp, EnvOut).

cmd_ternary_eval(increment(Iden, +, +), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Increment is Value + 1,
    update_env(Iden, Increment, EnvTemp, EnvOut).

cmd_ternary_eval(increment(Iden, +, +, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Increment is Value + 1,
    update_env(Iden, Increment, EnvTemp, EnvTemp2),
    cmd_ternary_eval(Cmd, EnvTemp2, EnvOut).

cmd_ternary_eval(decrement(Iden, -, -), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Decrement is Value - 1,
    update_env(Iden, Decrement, EnvTemp, EnvOut).

cmd_ternary_eval(decrement(Iden, -, -, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value),
    Decrement is Value - 1,
    update_env(Iden, Decrement, EnvTemp, EnvTemp2),
    cmd_ternary_eval(Cmd, EnvTemp2, EnvOut).

cmd_ternary_eval(add_(Iden, +, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 + Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_ternary_eval(add_(Iden, +, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 + Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_ternary_eval(Cmd, EnvTemp2, EnvOut).

cmd_ternary_eval(sub_(Iden, -, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 - Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_ternary_eval(sub_(Iden, -, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 - Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_ternary_eval(Cmd, EnvTemp2, EnvOut).

cmd_ternary_eval(multiply_(Iden, *, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 * Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_ternary_eval(multiply_(Iden, *, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 * Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_ternary_eval(Cmd, EnvTemp2, EnvOut).

cmd_ternary_eval(divide_(Iden, /, =, Expr), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 / Value2,
    update_env(Iden, Result, EnvTemp, EnvOut).

cmd_ternary_eval(divide_(Iden, /, =, Expr, Cmd), EnvIn, EnvOut) :-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Value1),
    eval_expr_env(Expr, EnvIn, EnvTemp, Value2),
    Result is Value1 / Value2,
    update_env(Iden, Result, EnvTemp, EnvTemp2),
    cmd_ternary_eval(Cmd, EnvTemp2, EnvOut).

boolCondition(bool_ands(B, '&', B1)) --> boolConditionBase(B), ['&'], boolCondition(B1).
boolCondition(bool_ors(B, '|', B1)) --> boolConditionBase(B), ['|'], boolCondition(B1).
boolCondition(bool_and(B, and, B1)) --> boolConditionBase(B), ['and'], boolCondition(B1).
boolCondition(bool_or(B, or, B1)) --> boolConditionBase(B), ['or'], boolCondition(B1).
boolCondition(B) --> boolConditionBase(B).

boolConditionBase(bool_true(true)) --> [true].
boolConditionBase(bool_false(false)) --> [false].
boolConditionBase(bool_equals(E, == , E1)) --> simple_expr(E), ['=', '='], simple_expr(E1).
boolConditionBase(bool_greater(E, > , E1)) --> simple_expr(E), ['>'], simple_expr(E1).
boolConditionBase(bool_less(E, < , E1)) --> simple_expr(E), ['<'], simple_expr(E1).
boolConditionBase(bool_greater_equal(E, >= , E1)) --> simple_expr(E), ['>', '='], simple_expr(E1).
boolConditionBase(bool_less_equal(E, <= , E1)) --> simple_expr(E), ['<', '='], simple_expr(E1).
boolConditionBase(bool_negation(not, B)) --> ['not'], boolCondition(B).
boolConditionBase(bool_negation(!, B)) --> ['!'], boolCondition(B).
boolConditionBase(bool_not_equal(E, '!=', E1)) --> simple_expr(E), ['!', '='], simple_expr(E1).

eval_bool_env(Bool, EnvIn, EnvOut, Result) :-
    bool_eval(Bool, EnvIn, EnvTemp, Result),
    (var(EnvTemp) -> EnvOut = EnvIn; EnvOut = EnvTemp).

eval_expr_env(Expr, EnvIn, EnvOut, Result) :-
    eval_expr(Expr, EnvIn, EnvTemp, Result),
    (var(EnvTemp) -> EnvOut = EnvIn; EnvOut = EnvTemp).

bool_eval(bool_true(true), _, _, true).
bool_eval(bool_false(false), _, _, false).
bool_eval(bool_equals(E, == ,E1), EnvIn, EnvOut, Result) :-
    eval_expr_env(E, EnvIn, EnvTemp, Val),
    eval_expr_env(E1, EnvTemp, EnvOut, Val1),
    compare_value(Val, Val1, Result).
bool_eval(bool_greater(E, > ,E1), EnvIn, EnvOut, Result) :-
    eval_expr_env(E, EnvIn, EnvTemp, Val),
    eval_expr_env(E1, EnvTemp, EnvOut, Val1),
    (Val > Val1 -> Result = true
    ; Result = false).
bool_eval(bool_less(E, < ,E1), EnvIn, EnvOut, Result) :-
    eval_expr_env(E, EnvIn, EnvTemp, Val),
    eval_expr_env(E1, EnvTemp, EnvOut, Val1),
    (Val < Val1 -> Result = true
    ; Result = false).
bool_eval(bool_greater_equal(E, >= ,E1), EnvIn, EnvOut, Result) :-
    eval_expr_env(E, EnvIn, EnvTemp, Val),
    eval_expr_env(E1, EnvTemp, EnvOut, Val1),
    (Val >= Val1 -> Result = true
    ; Result = false).
bool_eval(bool_less_equal(E, <= ,E1), EnvIn, EnvOut, Result) :-
    eval_expr_env(E, EnvIn, EnvTemp, Val),
    eval_expr_env(E1, EnvTemp, EnvOut, Val1),
    (Val =< Val1 -> Result = true
    ; Result = false).
bool_eval(bool_ands(E, &, E1), EnvIn, EnvOut, Result) :-
    eval_bool_env(E, EnvIn, EnvTemp, Val),
    eval_bool_env(E1, EnvTemp, EnvOut, Val1),
    (Val = true, Val1 = true -> Result = true; Result = false).
bool_eval(bool_ors(E, '|', E1), EnvIn, EnvOut, Result) :-
    eval_bool_env(E, EnvIn, EnvTemp, Val),
    eval_bool_env(E1, EnvTemp, EnvOut, Val1),
    (Val = false, Val1 = false -> Result = false; Result = true).
bool_eval(bool_and(E, and, E1), EnvIn, EnvOut, Result) :-
    eval_bool_env(E, EnvIn, EnvTemp, Val),
    eval_bool_env(E1, EnvTemp, EnvOut, Val1),
    (Val = true, Val1 = true -> Result = true; Result = false).
bool_eval(bool_or(E, or, E1), EnvIn, EnvOut, Result) :-
    eval_bool_env(E, EnvIn, EnvTemp, Val),
    eval_bool_env(E1, EnvTemp, EnvOut, Val1),
    (Val = false, Val1 = false -> Result = false; Result = true).
bool_eval(bool_negation(not, B), EnvIn, EnvOut, Result) :-
    eval_bool_env(B, EnvIn, EnvOut, TempResult),
    negate(TempResult, Result).
bool_eval(bool_negation(!, B), EnvIn, EnvOut, Result) :-
    eval_bool_env(B, EnvIn, EnvOut, TempResult),
    negate(TempResult, Result).
bool_eval(bool_not_equal(E, '!=' ,E1), EnvIn, EnvOut, Result) :-
    eval_expr_env(E, EnvIn, EnvTemp, Val),
    eval_expr_env(E1, EnvTemp, EnvOut, Val1),
    (\+ Val = Val1 -> Result = true
    ; Result = false).

compare_value(X, X, true).
compare_value(X, Y, false) :- \+ X = Y.
negate(true, false).
negate(false, true).

simple_expr(Str) --> string_(Str).
simple_expr(E0) --> e1(E1), e0(E1, E0).
simple_expr(increment(Iden, +, +)) --> variable(Iden), [+], [+].
simple_expr(decrement(Iden, -, -)) --> variable(Iden), [-], [-].

e0(EIn, EOut) --> [+], e1(E1), e0(add(EIn, E1), EOut).
e0(EIn, EOut) --> [-], e1(E1), e0(subtract(EIn, E1), EOut).
e0(E, E) --> [].
e1(E1) --> e2(E2), e1_(E2, E1).
e1_(EIn, EOut) --> [*], e2(E2), e1_(multiply(EIn, E2), EOut).
e1_(EIn, EOut) --> [/], e2(E2), e1_(divide(EIn, E2), EOut).
e1_(E, E) --> [].
e2(parenthesis(E)) --> ['('], simple_expr(E), [')'].
e2(I) --> variable(I).
e2(N) --> number(N).

check_same_datatype(Val1, Val2) :-
    ((number(Val1), number(Val2));
    (string(Val1), string(Val2))).

get_value(I, EnvIn, Val) :-
    member((I, _, _, Val), EnvIn).

eval_expr(number(Num), _, _, Num).
eval_expr(string_(Str), _, _, Str).
eval_expr(variable(Iden), EnvIn, _, Value) :-
    (get_value(Iden, EnvIn, Value) -> true; throw(error(undeclared_variable(Iden)))).

eval_expr(add(E1, E2), EnvIn, EnvOut, Result) :-
    eval_expr_env(E1, EnvIn, EnvTemp, R1),
    eval_expr_env(E2, EnvTemp, EnvOut, R2),
    (check_same_datatype(R1, R2) ->
        Result is R1 + R2
    ; Result = throw(error(type_mismatch))).

eval_expr(subtract(E1, E2), EnvIn, EnvOut, Result) :-
    eval_expr_env(E1, EnvIn, EnvTemp, R1),
    eval_expr_env(E2, EnvTemp, EnvOut, R2),
    (check_same_datatype(R1, R2) ->
        Result is R1 - R2
    ; Result = throw(error(type_mismatch))).

eval_expr(multiply(E1, E2), EnvIn, EnvOut, Result) :-
    eval_expr_env(E1, EnvIn, EnvTemp, R1),
    eval_expr_env(E2, EnvTemp, EnvOut, R2),
    (check_same_datatype(R1, R2) ->
        Result is R1 * R2
    ; Result = throw(error(type_mismatch))).

eval_expr(divide(E1, E2), EnvIn, EnvOut, Result) :-
    eval_expr_env(E1, EnvIn, EnvTemp, R1),
    eval_expr_env(E2, EnvTemp, EnvOut, R2),
    (check_same_datatype(R1, R2) ->
        (R2 =:= 0 -> throw(error(divide_by_zero)); Result is R1 / R2)
    ; Result = throw(error(type_mismatch))).

eval_expr(parenthesis(E), EnvIn, EnvOut, Result) :-
    eval_expr(E, EnvIn, EnvOut, Result).

eval_expr(increment(Iden, +, +), EnvIn, EnvOut, Result) :-
    eval_expr(Iden, EnvIn, EnvOut, Value),
    Result is Value + 1,
    update_env(Iden, Result, EnvIn, EnvOut),
    asserta(updated_env(EnvOut)).

eval_expr(decrement(Iden, -, -), EnvIn, EnvOut, Result) :-
    eval_expr(Iden, EnvIn, EnvOut, Value),
    Result is Value - 1,
    update_env(Iden, Result, EnvIn, EnvOut),
    asserta(updated_env(EnvOut)).


update_env(variable(Iden), Val, EnvIn, EnvOut) :-
    (select((Iden, Decl, Dtype, _), EnvIn, _) ->
        (valid_datatype_value(Dtype, Val) ->
            replace(EnvIn, (Iden, Decl, Dtype, _), (Iden, Decl, Dtype, Val), EnvOut)
        ;
            throw(error(invalid_datatype, context('Provided value does not match the expected datatype'))),
            EnvOut = EnvIn
        )
    ;
        EnvOut = EnvIn
    ).



replace([H|T], H, New, [New|T]).
replace([H|T], Old, New, [H|NewT]) :-
    replace(T, Old, New, NewT).

print_statement(print_string(Value)) --> ['('], string_(Value), [')'].
print_statement(print_bool(Bool)) --> ['('], boolCondition(Bool), [')'].
print_statement(print_expr(Expression)) --> ['('], simple_expr(Expression), [')'].


string_value(string_(Variable), [Variable | Tail], Tail) :- string(Variable).

variable(variable(Iden)) --> [Iden], {atom(Iden), not(number(Iden)), not(member(Iden, [int, float, bool, string, true, false, for,
    if, elif, else, while, range, and, or, not, in, range, <, >, <=, >=, ==,
    '!=', ++, --, +, -, *, /, '&', '|']))}.
number(number(Num)) --> [Num], { number(Num) }.
string_(string_(String)) --> [String], {string(String)}.

assignment(assign(Iden,=,Expr)) --> variable(Iden),[=],ternary_expr(Expr).
assignment(assign(Iden,=,Bool)) --> variable(Iden),[=],boolCondition(Bool).
assignment(assign(Iden,=,Str)) --> variable(Iden),[=],string_value(Str).
assignment(assign(Iden, +, =, Expr)) --> variable(Iden), [+], [=], ternary_expr(Expr).
assignment(assign(Iden, -, =, Expr)) --> variable(Iden), [-], [=], ternary_expr(Expr).
assignment(assign(Iden, *, =, Expr)) --> variable(Iden), [*], [=], ternary_expr(Expr).
assignment(assign(Iden, /, =, Expr)) --> variable(Iden), [/], [=], ternary_expr(Expr).

assignment_eval(assign(Iden, =, Expr), EnvIn, EnvOut):-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_extract(Expr, EnvIn, EnvTemp, Val),
    update_env(Iden, Val, EnvTemp, EnvOut).

assignment_eval(assign(Iden, +, =, Expr), EnvIn, EnvOut):-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Val),
    eval_expr_env(Expr, EnvTemp, EnvTemp2, Val2),
    (check_same_datatype(Val, Val2) ->
        NewVal is Val + Val2
    ; NewVal = throw(error(type_mismatch))),
    update_env(Iden, NewVal, EnvTemp2, EnvOut).

assignment_eval(assign(Iden, -, =, Expr), EnvIn, EnvOut):-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Val),
    eval_expr_env(Expr, EnvTemp, EnvTemp2, Val2),
    (check_same_datatype(Val, Val2) ->
        NewVal is Val - Val2
    ; NewVal = throw(error(type_mismatch))),
    update_env(Iden, NewVal, EnvTemp2, EnvOut).

assignment_eval(assign(Iden, *, =, Expr), EnvIn, EnvOut):-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Val),
    eval_expr_env(Expr, EnvTemp, EnvTemp2, Val2),
    (check_same_datatype(Val, Val2) ->
        NewVal is Val * Val2
    ; NewVal = throw(error(type_mismatch))),
    update_env(Iden, NewVal, EnvTemp2, EnvOut).

assignment_eval(assign(Iden, /, =, Expr), EnvIn, EnvOut):-
    is_member_of_program_var_int(Iden, EnvIn),
    eval_expr_env(Iden, EnvIn, EnvTemp, Val),
    eval_expr_env(Expr, EnvTemp, EnvTemp2, Val2),
    (check_same_datatype(Val, Val2) ->
        (Val2 =:= 0 -> throw(error(divide_by_zero)); NewVal is Val / Val2)
    ; NewVal = throw(error(type_mismatch))),
    update_env(Iden, NewVal, EnvTemp2, EnvOut).

is_member_of_program(variable(Iden), EnvOut):-
    memberchk((Iden, _, _, _), EnvOut).

is_member_of_program_var_int(variable(Iden), EnvOut):-
    memberchk((Iden, var, _, _), EnvOut).