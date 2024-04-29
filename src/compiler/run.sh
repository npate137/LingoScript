#!/bin/sh

tokens=$(python lexAnalyser.py)
swipl -q -f ../runtime/combineParser.pl -g "program(P, ${tokens}, []), program_eval(P, [], Env)." -t halt