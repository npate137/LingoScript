#Author: Nimit Patel
#version: 1.0
#purpose: This script executes a Python lexical analyzer to generate tokens, then uses SWI-Prolog to run a Prolog program with the parsed tokens for program evaluation.
#Date: 04/19/2024
#!/bin/sh

tokens=$(python lexAnalyser.py)
swipl -q -f ../runtime/combineParser.pl -g "program(P, ${tokens}, []), program_eval(P, [], Env)." -t halt