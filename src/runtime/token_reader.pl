% Author: Nimit Patel
% version: 1.0
% purpose: Reads a file line by line and converts its contents into a suitable format, handling both text and numeric data.
% Date: 04/23/2024

:- module(custom_read_file, [custom_read_file/2]).
:- set_prolog_flag(double_quotes, string).


custom_read_file(FileName, ConvertedData) :-
    open(FileName, read, Stream),
    read_stream_contents(Stream, FileData),
    convert_data(FileData, ConvertedData), !,
    close(Stream).
read_stream_contents(Stream, [CurrentLineCharacters | List]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_codes(CurrentLineCharacters, Codes),
    read_stream_contents(Stream, List), !.

read_stream_contents(Stream, []) :- at_end_of_stream(Stream).

convert_data([H|T], [N|R]) :- atom_number(H, N), convert_data(T, R).
convert_data([H|T], [H|R]) :- atom(H), convert_data(T, R).
convert_data([], []).