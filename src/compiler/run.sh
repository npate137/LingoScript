#!/bin/sh

tokens=$(python lexical_analyzer_1.py)
swipl -q -f ../runtime/parser_.pl -g "program(P, ${tokens}, []), program_eval(P, [], Env)." -t halt