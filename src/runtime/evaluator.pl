:- module(program_eval, [program_eval/3]).
program_eval(Prog, EnvIn, EnvOut) :-
    program_evaluation(Prog, EnvIn, EnvOut).

program_evaluation(main(CodeBlock), EnvIn, EnvOut) :-
    block_eval(CodeBlock, EnvIn, EnvOut).

block_eval(code_block(SubBlock), EnvIn, EnvOut):-
    sub_block_eval(SubBlock, EnvIn, EnvOut).

block_eval(empty(), EnvIn, EnvIn).

sub_block_eval(sub_block(Decl, Rest), EnvIn, EnvOut) :- declaration_eval(Decl, EnvIn, EnvTemp), sub_block_eval(Rest, EnvTemp, EnvOut).
sub_block_eval(sub_block(Cmd, Rest), EnvIn, EnvOut) :- cmd_eval(Cmd, EnvIn, EnvTemp), sub_block_eval(Rest, EnvTemp, EnvOut).
sub_block_eval(sub_block(Decl), EnvIn, EnvOut) :- declaration_eval(Decl, EnvIn, EnvOut).
sub_block_eval(sub_block(Cmd), EnvIn, EnvOut) :- cmd_eval(Cmd, EnvIn, EnvOut).
sub_block_eval([], EnvOut, EnvOut).

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

is_variable_declared(variable(Iden), datatype(Datatype), EnvIn, [(Iden, var, Datatype, _) | EnvIn]) :-
    (\+ member((Iden, _, _, _), EnvIn); throw(error(variable_already_declared, [Iden]))).

is_variable_declared(variable(Iden), datatype(Datatype), Val, EnvIn, [(Iden, var, Datatype, N) | EnvTemp]) :-
    eval_extract(Val, EnvIn, EnvTemp, N),
    (\+ member((Iden, _, _, _), EnvTemp)
    -> ( valid_datatype_value(Datatype, N)
    -> true ;  format('Error: invalid value "~w" for datatype "~w"~n', [N, Datatype]), fail)
    ; throw(error(variable_already_declared, [Iden]))).

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