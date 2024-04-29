% Author: Neel
% version: 1.0
% purpose: The main program structured to read, parse, and evaluate LingoScript code file.
% Date: 04/23/2024

:- use_module(token_reader).
:- use_module(parser).
:- use_module(evaluator).
:- set_prolog_flag(double_quotes, string).

main(Filename) :- nl,
    ansi_format([bold,fg(yellow)], 'Initiating Parser', []), nl,
    custom_read_file(Filename, FileData),
    program(ParseTree, FileData, []),
    write("Generating Parse Tree: "), success_flag, nl,
    write(ParseTree), nl,
    ansi_format([bold,fg(yellow)], 'Initiating  Evaluation', []), nl,
    program_eval(ParseTree, [], NewEnv), nl,
    ansi_format([bold,fg(yellow)], 'Environment after Evaluation', []), nl,
    write(NewEnv), nl,
    halt.

success_flag :- ansi_format([bold,fg(green)], 'SUCCESS', []).