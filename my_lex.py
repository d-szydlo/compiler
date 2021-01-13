#!/usr/bin/python3

import ply.lex as lex
import sys

tokens = (
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MODULO',   # +, -, *, /, %
    'EQ', 'NEQ', 'LE', 'GR', 'LEQ', 'GEQ',          # =, !=, <, >, <=, >=
    'LPAR', 'RPAR', 'COLON', 'SCOLON', 'COMMA',     # (, ), :, ;, ,
    'ASSIGN', 'NUM', 'PID',                         # :=, number, variable identifier
    'DECLARE', 'BEGIN', 'END',                      # file structure
    'IF', 'THEN', 'ELSE', 'ENDIF',                  # if statements
    'WHILE', 'DO', 'ENDWHILE',                      # while loop
    'REPEAT', 'UNTIL',                              # repeat-until loop
    'FOR', 'FROM', 'TO', 'DOWNTO', 'ENDFOR',        # for loop
    'READ', 'WRITE'                                 # i/o
)

t_PLUS      = r'\+'
t_MINUS     = r'-'
t_TIMES     = r'\*'
t_DIVIDE    = r'/'
t_MODULO    = r'\%'
t_EQ        = r'='
t_NEQ       = r'!='
t_LE        = r'<'
t_GR        = r'>'
t_LEQ       = r'<='
t_GEQ       = r'>='
t_LPAR      = r'\('
t_RPAR      = r'\)'
t_COLON     = r':'
t_SCOLON    = r';'
t_COMMA     = r','
t_ASSIGN    = r':='
t_DECLARE   = r'DECLARE'
t_BEGIN     = r'BEGIN'
t_END       = r'END'
t_IF        = r'IF'
t_THEN      = r'THEN'
t_ELSE      = r'ELSE'
t_ENDIF     = r'ENDIF'
t_WHILE     = r'WHILE'
t_DO        = r'DO'
t_ENDWHILE  = r'ENDWHILE'
t_REPEAT    = r'REPEAT'
t_UNTIL     = r'UNTIL'
t_FOR       = r'FOR'
t_FROM      = r'FROM'
t_TO        = r'TO'
t_DOWNTO    = r'DOWNTO'
t_ENDFOR    = r'ENDFOR'
t_READ      = r'READ'
t_WRITE     = r'WRITE'
t_ignore = ' \t'

def t_NUM(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_PID(t):
    r'[_a-z]+'
    t.value = str(t.value)
    return t

def t_COM(t):
    r'\[[^\]]*\]'
    pass

def t_NL(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print('Nielegalny znak {0:} w linii {1: d}'.format(t.value[0], t.lexer.lineno))
    t.lexer.skip(1)

def build_lex():
    return lex.lex()

if __name__ == '__main__':
    data = ''
    with open(sys.argv[1], 'r') as f:
        data = f.read()
    lexer = build_lex()
    lexer.input(data)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)