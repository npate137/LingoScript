# LingoScript Programming Language
# SER502-LingoScript-Team22
### LingoScript is a custom programming language designed for handling basic arithmetic operations, conditional statements, ternary expressions, and loops.

#### Team Members:
1. Devanshu Amitkumar Desai
2. Aditya Gupta
3. Nimit Pragnesh Patel
4. Neel Nirajkumar Shah

## System Used To Develop Project:
- Mac OS
- Windows

## ⚙ Tools Used
- SWI-Prolog
- Python3
- Bash

## Project Video Link
- Youtube Video Link : ([Link 🚀]())

## Installation and Execution Instructions
- Download SWI-Prolog ([Link 🚀](https://www.swi-prolog.org/Download.html))
- Download Python3 ([Link 🚀](https://www.python.org/downloads/))
- Sample test programs can be found in the 'data' folder with a .ls extension

### Installing Prolog
#### Linux
```
$ apt-get install swi-prolog
```
#### macOS(Requires brew)
```
brew install swi-prolog
```
### Executing LingoScript Programs
#### Method 1
Navigate to the Compiler Folder
```
bash run.sh
```
#### Method 2
```
$ swipl
```
- Input the path to the .pl file
```
?- ['/Users/user/Desktop/SER502-LingoScript-Team22/src/compiler/run.pl'].  
```
- Execute the sample program by parsing tokens
```
?- pixel('/Users/user/Desktop/SER502-LingoScript-Team22/src/compiler/lexical_analyer_1.py').
```