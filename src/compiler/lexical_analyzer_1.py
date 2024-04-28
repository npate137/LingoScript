from tokenize import tokenize
from io import BytesIO
import tkinter as tk
from tkinter import filedialog

keywords = ["main", "const", "var", "int", "bool", "str", "if", "else",
            "for", "while", "print", "true", "false", "not", "and", "or", "elseif"]
operators = ["+", "-", "*", "/", "=", ">", "<", "!", "?", ":", "&"]
arithmetic_assignment = ["+=", "-=", "/=", "*=", "=="]
separators = ["(", ")", "{", "}", ",", "|"]


def lexical_analyzer(filename):
    assert filename[-2:] == "ls", "Please open a file with .ls extension"
    tokens_list = "["
    file_content = open(filename, "r").read()
    line = BytesIO(file_content.encode("utf-8")).readline
    tokenized_line = tokenize(line)
    values = []
    for number, value, v1, v2, v3 in tokenized_line:
        if len(value) == 0:
            continue
        else:
            if value == "\n" or value == " " or value == "\t":
                continue
            else:
                if value == "[":
                    values.append(value)
                elif value == "]":
                    values.append(value)
                    for val in values:
                        tokens_list += val
                        tokens_list += ", "
                    values = []
                else:
                    if value in keywords or value in operators or value == ";":
                        if value == "elseif":
                            tokens_list += "else, if, "
                            continue
                        tokens_list += value
                        tokens_list += ", "
                    elif value in separators:
                        tokens_list += "'"
                        tokens_list += value
                        tokens_list += "', "
                    elif value.startswith("'"):
                        temp = value[1:-1]
                        tokens_list += "'" + temp + "', "
                    elif value.startswith('"'):
                        temp = value[1:-1]
                        tokens_list += '"' + temp + '", '
                    elif value.isdigit() or value.isalpha():
                        tokens_list += value
                        tokens_list += ", "
                    elif value in arithmetic_assignment:
                        tokens_list += value[0]
                        tokens_list += ", "
                        tokens_list += value[1]
                        tokens_list += ", "
    tokens_list = tokens_list[:-2]
    tokens_list += "]"
    print(tokens_list)


if __name__ == "__main__":
    root = tk.Tk()
    root.withdraw()
    file_path = filedialog.askopenfilename()
    lexical_analyzer(file_path)
