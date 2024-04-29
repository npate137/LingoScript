% Author: Devanshu Desai
% version: 1.0
% purpose: SER 502 - Spring 2024 - Team 22 - LingoScript Programming Language
% Date: 03/31/2024

:- module(program, [program/3]).
:- table main_block/2.

ls(Lexername) :-
    (   which('python3') -> PythonCmd = 'python3'
    ;   which('python') -> PythonCmd = 'python'
    ;   which('py') -> PythonCmd = 'py'
    ;
        throw(error('Python interpreter not found', _))
    ),
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

block(empty()) --> ['{'], ['}'].
block(code_block(CodeBlock)) --> ['{'], sub_block(CodeBlock),['}'].

sub_block(sub_block(Decl)) --> declaration(Decl).
sub_block(sub_block(Decl, Rest)) --> declaration(Decl), sub_block(Rest).
sub_block(sub_block(Cmd, Rest)) --> noLeftRecurCmd(Cmd), sub_block(Rest).
sub_block(sub_block(Cmd)) --> noLeftRecurCmd(Cmd).
sub_block([]) --> [].

declaration(Decl) --> constant_decl(Decl).
declaration(Decl) --> variable_decl(Decl).

constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;].
constant_decl(constant_decl(const, Dtype, Iden, =, Bool, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Bool), [;].
constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expr(Expr), [;].
constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;], declaration(Rest).
constant_decl(constant_decl(const, Dtype, Iden, =, Bool, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Bool), [;], declaration(Rest).
constant_decl(constant_decl(const, Dtype, Iden, =, Expr, ;, Rest)) --> [const], variable_datatype(Dtype), variable(Iden), [=], ternary_expr(Expr), [;], declaration(Rest).

variable_decl(variable_decl(var, Dtype, Iden, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [;].
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;].
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;)) --> [var], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Expr), [;].
variable_decl(variable_decl(var, Dtype, Iden, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [;], declaration(Rest).
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], simple_expr(Expr), [;], declaration(Rest).
variable_decl(variable_decl(var, Dtype, Iden, =, Expr, ;, Rest)) --> [var], variable_datatype(Dtype), variable(Iden), [=], boolCondition(Expr), [;], declaration(Rest).

variable_datatype(datatype(int)) --> [int].
variable_datatype(datatype(bool)) --> [bool].
variable_datatype(datatype(str)) --> [str].

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


ternary_expr(ternary_operator(Bool, '?', Expr1, :, Expr2)) --> optional_bracket_left(), boolCondition(Bool), ['?'], ternary_expr(Expr1), [:], ternary_expr(Expr2), optional_bracket_right().
ternary_expr(ternary_operator(Iden, Bool, '?', Expr1, :, Expr2)) --> optional_bracket_left(), variable(Iden), [=], boolCondition(Bool), ['?'], ternary_expr(Expr1), [:], ternary_expr(Expr2), optional_bracket_right().
ternary_expr(Expr) --> simple_expr(Expr).
ternary_expr(Bool) --> boolCondition(Bool).
optional_bracket_left() --> ['('].
optional_bracket_left() --> [].

optional_bracket_right() --> [')'].
optional_bracket_right() --> [].

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