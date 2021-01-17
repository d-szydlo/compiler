import ply.yacc as yacc
import sys
from my_lex import build_lex, tokens

mem_index = 0 # id of first empty memory cell
symbol_table = [] #every record is a list which holds following attributes of a symbol:
                  #(name, if_tab, span_start, span_end, mem_id, if_initialized)
iterators = []

### SYMBOL TABLE FUNCTIONS ###

# adding a symbol to the table
def add_symbol(name, if_tab, start, stop, lineno):
    n = ''
    try:
        n = get_symbol_by_name(name, lineno)
    except:
        n = None
    if n is not None:
        raise Exception('Błąd w linii {0:d}: Druga deklaracja zmiennej {1:}'.format(lineno, name))
    global symbol_table, mem_index
    if if_tab:
        if start > stop:
            raise Exception('Błąd w linii {0:d}: Niewłaściwy zakres w deklaracji tablicy'.format(lineno))
    symbol_table.append([name, if_tab, start, stop, mem_index, False])
    mem_index += stop - start + 1

# searching for symbol's position in table by it's name
def get_symbol_by_name(name, ln):
    global symbol_table
    for i in range(len(symbol_table)):
        if symbol_table[i][0] == name:
            return i
    raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(ln, name))

# searching for symbol's memory address by it's name
def get_mem_id_by_name(name, ln):
    global symbol_table
    for i in range(len(symbol_table)):
        if symbol_table[i][0] == name:
            return symbol_table[i][4]
    raise Exception('Błąd w linii {0:d}: użycie niezadeklarowanej zmiennej {1:}'.format(ln, name))

# searching for symbol's position in table by it's memory address
def get_symbol_by_mem_id(mem_id):
    global symbol_table
    for i in range(len(symbol_table)):
        if symbol_table[i][4] == mem_id:
            return i
        elif symbol_table[i][1]:
            if mem_id > symbol_table[i][4] and mem_id <= symbol_table[i][4] + symbol_table[i][3] - symbol_table[i][2]:
                return i

### LOADING VALUES TO REGISTERS ###

def load_val_to_register(val, reg):
    code = ''
    while val != 0:
        if val % 2 == 0:
            code = 'SHL {}\n'.format(reg) + code
            val /= 2
        else:
            code = 'INC {}\n'.format(reg) + code
            val -= 1
    code = 'RESET {}\n'.format(reg) + code
    return code

def load_var_to_register(var_addr, reg, ln):
    global symbol_table
    i = get_symbol_by_mem_id(var_addr)
    if not symbol_table[i][5]:
        raise Exception('Błąd w linii {0:}: Użycie niezainicjowanej zmiennej {1:}'.format(ln, symbol_table[i][0]))
    if symbol_table[i][0].startswith('TI'):
        iterators.append(symbol_table[i][0])
    return load_val_to_register(var_addr, reg) + 'LOAD {} {}\n'.format(reg, reg)

# loading a value held by a table cell indexed by another variable 
def load_tab_to_register(cell_id, tab_addr, min_tab_id, reg, reg_h, ln):
    code = load_var_to_register(cell_id, reg_h, ln)
    code += load_val_to_register(tab_addr, reg) + 'ADD {} {}\n'.format(reg, reg_h) +\
            load_val_to_register(min_tab_id, reg_h) + 'SUB {} {}\n'.format(reg, reg_h) +\
            'LOAD {} {}\n'.format(reg, reg)
    return code

# loading two values to registers in regs
# values = [val1, val2] where valx is p[0] from production 'value : NUM | identifier' 
# regs = [(reg1, reg2), (reg3, reg4)] where reg1 and reg2 are regs to load corresponding value to
#                                       and reg3 and reg4 are helper regs
def load_values_to_registers(values, regs, ln):
    code = ''
    for i in range(len(values)):
        if values[i][1]: #value is a variable
            if type(values[i][0]) is tuple: #variable is a table cell indexed by another variable
                code += load_tab_to_register(*values[i][0], *regs[i], ln)
            else:
                code += load_var_to_register(values[i][0], regs[i][0], ln)
        else: # value is a number
            code += load_val_to_register(values[i][0], regs[i][0])
    return code

### PROGRAM ###

def p_program(p):
    '''program : DECLARE declarations BEGIN commands END
               | BEGIN commands END'''
    if len(p) == 6:
        p[0] = p[4] + 'HALT'
    else:
        p[0] = p[2] + 'HALT'

### DECLARATIONS ###

def p_declare_array(p):
    '''declarations : declarations COMMA PID LPAR NUM COLON NUM RPAR 
                    | PID LPAR NUM COLON NUM RPAR'''
    if p[1] != None:
        name = p[1]
        start = p[3]
        stop = p[5]
        ln = p.lineno(1)
    else:
        name = p[3]
        start = p[5]
        stop = p[7]
        ln = p.lineno(3)
    add_symbol(name, True, int(start), int(stop), ln)

def p_declare_variable(p):
    '''declarations : declarations COMMA PID
                    | PID'''
    if p[1] != None:
        name = p[1]
        ln = p.lineno(1)
    else:
        name = p[3]
        ln = p.lineno(3)
    add_symbol(name, False, 0, 0, ln)

### COMMANDS ###

def p_commands(p):
    '''commands : commands command
                | command'''
    if len(p) == 3:
        p[0] = p[1] + p[2]
    else:
        p[0] = p[1]

### ASSIGNMENT ###

def p_assingment(p):
    'command : identifier ASSIGN expr SCOLON'
    global symbol_table
    code = p[3] # loading expr to register a
    if type(p[1]) is tuple: #expr is assigned to a table cell indexed by another variable
        code += load_var_to_register(p[1][0], 'd', p.lineno(2)) + load_val_to_register(p[1][1], 'b') + \
                'ADD b d\n' + load_val_to_register(p[1][2], 'd') + 'SUB b d\n'
        i = i = get_symbol_by_mem_id(p[1][1])
    else: #expr is assigned to a variable or a table cell indexed by a number
        code += load_val_to_register(p[1], 'b')
        i = get_symbol_by_mem_id(p[1])
    symbol_table[i][5] = True # marking variable as initialized
    if symbol_table[i][0].startswith('TI'):
        raise Exception('Błąd w linii {0:d}: Modyfikacja wartości iteratora pętli'.format(p.lineno(2)))
    code += 'STORE a b\n'
    p[0] = code

### IF STATEMENTS ###

def p_if_statment(p):
    'command : IF condition THEN commands ENDIF'
    code = p[2]
    com_len = len(p[4].split('\n'))
    code += 'JZERO a 2\n' + 'JUMP {}\n'.format(com_len) + p[4]
    p[0] = code

def p_if_else_statement(p):
    'command : IF condition THEN commands ELSE commands ENDIF'
    code = p[2]
    com1_len = len(p[4].split('\n')) + 1
    com2_len = len(p[6].split('\n'))
    code += 'JZERO a 2\n' + 'JUMP {}\n'.format(com1_len) + p[4] + 'JUMP {}\n'.format(com2_len) + p[6]
    p[0] = code

### LOOPS ###

def p_while_loop(p):
    'command : WHILE condition DO commands ENDWHILE'
    code = p[2]
    cond_len = len(p[2].split('\n'))
    com_len = len(p[4].split('\n'))
    code += 'JZERO a 2\n' + 'JUMP {}\n'.format(com_len + 1) + p[4] + 'JUMP -{}\n'.format(com_len + cond_len)
    p[0] = code

def p_repeat_until_loop(p):
    'command : REPEAT commands UNTIL condition SCOLON'
    cond_len = len(p[2].split('\n'))
    com_len = len(p[4].split('\n'))
    p[0] = p[2] + p[4] + 'JZERO a 2\n' + 'JUMP -{}\n'.format(com_len + cond_len-1)

def p_for_iter(p):
    'iter : PID'
    add_symbol('TI'+p[1], False, 0, 0, p.lineno(1)) # adding iterator as temp value
    add_symbol('TE'+p[1], False, 0, 0, p.lineno(1)) # adding final value of iterator as temp value
    i = get_symbol_by_name('TI'+p[1], p.lineno(1))
    symbol_table[i][5] = True
    p[0] = p[1]

def p_for_to_loop(p):
    '''command : FOR iter FROM value TO value DO commands ENDFOR
               | FOR iter FROM value DOWNTO value DO commands ENDFOR'''
    global symbol_table, mem_index, iterators
    iter_addr = get_mem_id_by_name('TI'+p[2], p.lineno(1)) # iterator's address
    end_addr = get_mem_id_by_name('TE'+p[2], p.lineno(1)) # final value's address
    code = ''
    if (not p[4][1]) and (not p[6][1]) and (abs(p[4][0]-p[6][0])+1 < 21):
        if 'TI'+p[2] in iterators:
            iterators.pop(iterators.index('TI'+p[2]))
            load_addr = load_val_to_register(iter_addr, 'f')
            load_iter = load_var_to_register(iter_addr, 'c', p.lineno(1))
            code +=  load_val_to_register(p[4][0], 'c') + load_addr + 'STORE c f\n'
            mod = ''
            if p[5] == 'TO':
                mod += 'INC c\n'
            else:
                mod += 'DEC c\n'
            loop = p[8] + load_iter + mod + load_addr + 'STORE c f\n'
            for _ in range(abs(p[4][0]-p[6][0])+1):
                code += loop
        else:
            for _ in range(abs(p[4][0]-p[6][0])+1):
                code += p[8]
    else:
        if p[4][1]: #value1 is a variable
            if type(p[4][0]) is tuple: #variable is a table cell indexed by another variable
                code += load_tab_to_register(*p[4][0], 'c', 'b', p.lineno(1))
            else:
                if p[4][0] == iter_addr:
                    code += load_var_to_register(get_mem_id_by_name(p[2], p.lineno(1)), 'c', p.lineno(1))
                else:
                    code += load_var_to_register(p[4][0], 'c', p.lineno(1))
        else: # value1 is a number
            code += load_val_to_register(p[4][0], 'c')
        # iterator's initial value is now loaded to reg c
        code += load_val_to_register(iter_addr, 'f') + 'STORE c f\n' # storing iterator
        i = get_symbol_by_mem_id(iter_addr)
        symbol_table[i][5] = True

        if p[6][1]: #value2 is a variable
            if type(p[6][0]) is tuple: #variable is a table cell indexed by another variable
                code += load_tab_to_register(*p[6][0], 'e', 'b', p.lineno(1))
            else:
                if p[6][0] == iter_addr:
                    code += load_var_to_register(get_mem_id_by_name(p[2], p.lineno(1)), 'f', p.lineno(1))
                else:
                    code += load_var_to_register(p[6][0], 'e', p.lineno(1))
        else: # value2 is a number
            code += load_val_to_register(p[6][0], 'e')
        # final value of iterator is now loaded to reg e
        code += 'INC f\n' + 'STORE e f\n'# temp's are next to each other in memory, so by inc f we get end_addr in reg f
        i = get_symbol_by_mem_id(end_addr)
        symbol_table[i][5] = True

        load_end =  load_var_to_register(end_addr, 'e', p.lineno(1))
        ld_end_len = len(load_end.split('\n'))

        load_iter = load_var_to_register(iter_addr, 'c', p.lineno(1))
        ld_iter_len = len(load_iter.split('\n'))

        load_iter_addr = load_val_to_register(iter_addr, 'f')
        ld_iter_addr_len = len(load_iter_addr.split('\n'))

        com_len = len(p[8].split('\n'))

        if p[5] == 'TO':
            code += load_end + load_iter + 'SUB c e\n' + 'JZERO c 2\n' + 'JUMP {}\n'.format(com_len+ld_iter_addr_len + 3) + p[8] + load_iter_addr +\
                 'LOAD c f\n'+ 'INC c\n' + 'STORE c f\n' +\
                'JUMP -{}\n'.format(com_len + ld_iter_addr_len + ld_end_len + ld_iter_len + 2)
        else:
            code += load_end + load_iter + 'JZERO c {}\n'.format(6+com_len+ld_iter_addr_len) + 'SUB e c\n' + 'JZERO e 2\n' + 'JUMP {}\n'.format(com_len + ld_iter_addr_len + 4) +\
                    p[8] + load_iter_addr + 'LOAD c f\n'+ 'DEC c\n' + 'STORE c f\n' +\
                    'JUMP -{}\n'.format(com_len + ld_end_len + ld_iter_addr_len + ld_iter_len + 3) + 'JZERO e 2\n' + 'JUMP {}\n'.format(com_len) + p[8]
    p[0] = code
    mem_index -= 2
    symbol_table.pop(get_symbol_by_mem_id(iter_addr))
    symbol_table.pop(get_symbol_by_mem_id(end_addr))

### I/O ###

def p_read(p):
    'command : READ identifier SCOLON'
    code = ''
    if type(p[2]) is tuple: #variable is a table cell indexed by another variable
        code += load_var_to_register(p[2][0], 'b', p.lineno(3)) + load_val_to_register(p[2][1], 'a') +\
                'ADD a b\n' + load_val_to_register(p[2][2], 'b') + 'SUB a b\n' #loading cell's memory index
    else: #variable is an ordinary variable or a table cell indexed by a number
        code += load_val_to_register(p[2], 'a')
        i = get_symbol_by_mem_id(p[2]) 
        symbol_table[i][5] = True # marking variable as initialized
    code += 'GET a\n'
    p[0] = code

def p_write(p):
    'command : WRITE value SCOLON'
    global mem_index
    code = ''
    if p[2][1]: #value is a variable, meaning p[2][0] is its address
        if type(p[2][0]) is tuple: #variable is a table cell indexed by another variable
            code += load_tab_to_register(*p[2][0], 'a', 'b', p.lineno(1))
        else: #variable is an ordinary variable or a table cell indexed by a number
            code += load_var_to_register(p[2][0], 'a', p.lineno(1))
        code = code[:code.rfind('\n')]
        code = code[:code.rfind('\n')] + '\n'
    else: #value is a number
        code += load_val_to_register(p[2][0], 'b') + \
                load_val_to_register(mem_index, 'a') + 'STORE b a\n'
    code += 'PUT a\n'
    p[0] = code

### EXPRESSION ###

def p_expr_value(p):
    'expr : value'
    global symbol_table
    code = ''
    if p[1][1]: #value is a variable
        if type(p[1][0]) is tuple: #variable is a table cell indexed by another variable
            code += load_tab_to_register(*p[1][0], 'a', 'b', p.linespan(1)[0])
        else:
            code += load_var_to_register(p[1][0], 'a', p.linespan(1)[0])
    else: # value is a number
        code += load_val_to_register(p[1][0], 'a')
    p[0] = code

###  BINARY OPERATIONS ###

# result is in reg a

def p_expr_add_sub(p):
    '''expr : value PLUS value
            | value MINUS value'''
    code = ''
    if not p[3][1] and p[3][0] < 10:
        if p[1][1]:
            if type(p[1][0]) is tuple:
                code += load_tab_to_register(*p[1][0], 'a', 'b', p.lineno(2))
            else:
                code += load_var_to_register(p[1][0], 'a', p.lineno(2))
        else:
            code += load_val_to_register(p[1][0], 'a')
        mod = ''
        if p[2] == '+':
            mod = 'INC a\n'
        else:
            mod = 'DEC a\n'
        for _ in range(p[3][0]):
            code += mod
    else:
        code += load_values_to_registers([p[1], p[3]], [('a', 'b'), ('b', 'd')], p.lineno(2)) # loading val1 to reg a and val2 to reg b
        if p[2] == '+':
            code += 'ADD a b\n'
        else:
            code += 'SUB a b\n'
    p[0] = code

def p_expr_mul(p):
    'expr : value TIMES value'
    code = ''
    if not p[3][1] and p[3][0] < 3:
        if p[3][0] == 0:
            code = 'RESET a\n'
        else:
            if p[1][1]:
                if type(p[1][0]) is tuple:
                    code = load_tab_to_register(*p[1][0], 'a', 'b', p.lineno(2))
                else:
                    code = load_var_to_register(p[1][0], 'a', p.lineno(2))
            else:
                code = load_val_to_register(p[1][0], 'a')
            if p[3][0] == 2:
                code += 'SHL a\n' 
    else:
        code = load_values_to_registers([p[1], p[3]], [('b', 'd'), ('d', 'e')], p.lineno(2)) # loading val1 to reg b and val2 to reg d
        code += 'RESET a\n' + 'RESET e\n' + 'ADD e d\n' + 'SUB d b\n' + 'JZERO d 6\n' + 'RESET d\n' + 'ADD d b\n' +\
                'RESET b\n' + 'ADD b e\n' + 'JUMP 2\n' + 'ADD d e\n' + 'JZERO d 9\n' + 'JODD d 4\n' + 'SHL b\n' +\
                'SHR d\n' + 'JUMP -4\n' + 'ADD a b\n' + 'SHL b\n' + 'SHR d\n' + 'JUMP -8\n'
    p[0] = code

def p_expr_div(p):
    'expr : value DIVIDE value'
    code = ''
    if not p[3][1] and p[3][0] < 3:
        if p[3][0] == 0:
            code = 'RESET a\n'
        else:
            if p[1][1]:
                if type(p[1][0]) is tuple:
                    code = load_tab_to_register(*p[1][0], 'a', 'b', p.lineno(2))
                else:
                    code = load_var_to_register(p[1][0], 'a', p.lineno(2))
            else:
                code = load_val_to_register(p[1][0], 'a')
            if p[3][0] == 2:
                code += 'SHR a\n' 
    else:
        code = load_values_to_registers([p[1], p[3]], [('b', 'd'), ('d', 'e')], p.lineno(2)) # loading val1 to reg b and val2 to reg d
        code += 'RESET a\n' + 'JZERO d 31\n' + 'RESET e\n' + 'INC e\n' + 'RESET f\n' + 'ADD f b\n' + 'SUB f d\n' + 'JZERO f 5\n' + 'ADD f d\n' +\
                'SHL d\n' + 'SHL e\n' + 'JUMP -5\n' + 'ADD f d\n' + 'SUB f b\n' + 'JZERO f 3\n' + 'SHR d\n' + 'SHR e\n' + 'SUB b d\n' +\
                'ADD a e\n' + 'SHR d\n' + 'SHR e\n'  + 'JZERO e 11\n'+ 'RESET f\n' + 'ADD f b\n' + 'SUB f d \n' + 'JZERO f 2\n' + 'JUMP -9\n' +\
                'ADD f d\n' + 'SUB f b\n' + 'JZERO f 2\n' + 'JUMP -11\n' + 'ADD a e\n'
    p[0] = code

def p_expr_mod(p):
    'expr : value MODULO value'
    code = ''
    if not p[3][1] and p[3][0] < 3:
        if p[3][0] < 2:
            code = 'RESET a\n'
        else:
            if p[1][1]:
                if type(p[1][0]) is tuple:
                    code = load_tab_to_register(*p[1][0], 'c', 'd', p.lineno(2))
                else:
                    code = load_var_to_register(p[1][0], 'b', p.lineno(2))
            else:
                code = load_val_to_register(p[1][0], 'b')
            code += 'RESET a\n' + 'JODD b 2\n' + 'JUMP 2\n' + 'INC a\n'
    else:
        code = load_values_to_registers([p[1], p[3]], [('a', 'b'), ('d', 'e')], p.lineno(2)) # loading val1 to reg a and val2 to reg d
        code += 'RESET b\n' + 'JZERO d 34\n' + 'RESET e\n' + 'INC e\n' + 'RESET f\n' + 'ADD f a\n' + 'SUB f d\n' + 'JZERO f 5\n' + 'ADD f d\n' +\
                'SHL d\n' + 'SHL e\n' + 'JUMP -5\n' + 'ADD f d\n' + 'SUB f a\n' + 'JZERO f 4\n' + 'SHR d\n' + 'SHR e\n' + 'JZERO e 19\n' + 'SUB a d\n' +\
                'ADD b e\n' + 'SHR d\n' + 'SHR e\n'  + 'JZERO e 12\n'+ 'RESET f\n' + 'ADD f a\n' + 'SUB f d \n' + 'JZERO f 2\n' + 'JUMP -9\n' +\
                'ADD f d\n' + 'SUB f a\n' + 'JZERO f 2\n' + 'JUMP -11\n' + 'ADD b e\n' + 'SUB a d\n' + 'JUMP 2\n' + 'RESET a\n'
    p[0] = code

### CONDITIONS ###

# reg a is set to 0 if cond is true

def p_cond_eq(p):
    'condition : value EQ value'
    code = load_values_to_registers([p[1], p[3]], [('a', 'b'), ('b', 'd')], p.lineno(2)) # loading val1 to reg a and val2 to reg b
    code += 'RESET d\n' + 'ADD d a\n' + 'SUB a b\n' + 'SUB b d\n' + 'ADD a b\n'
    p[0] = code

def p_cond_neq(p):
    'condition : value NEQ value'
    code = load_values_to_registers([p[1], p[3]], [('a', 'b'), ('b', 'd')], p.lineno(2)) # loading val1 to reg a and val2 to reg b
    code += 'RESET d\n' + 'ADD d a\n' + 'SUB a b\n' + 'SUB b d\n' + 'ADD a b\n' + 'JZERO a 3\n' + 'RESET a\n' + 'JUMP 2\n' + 'INC a\n'
    p[0] = code

def p_cond_le_gr(p):
    '''condition : value LE value
                 | value GR value'''
    code = ''
    if p[2] == '<':
        code = load_values_to_registers([p[1], p[3]], [('a', 'b'), ('b', 'd')], p.lineno(2)) # loading val1 to reg a and val2 to reg b
    else:
        code = load_values_to_registers([p[3], p[1]], [('a', 'b'), ('b', 'd')], p.lineno(2)) # loading val1 to reg b and val2 to reg a
    code += 'RESET d\n' + 'ADD d a\n' + 'SUB a b\n' + 'SUB b d\n' + 'JZERO b 2\n' + 'JUMP 2\n' + 'INC a\n'
    p[0] = code

def p_cond_leq_geq(p):
    '''condition : value LEQ value
                 | value GEQ value'''
    code = ''
    if p[2] == '<=':
        code = load_values_to_registers([p[1], p[3]], [('a', 'b'), ('b', 'd')], p.lineno(2)) # loading val1 to reg a and val2 to reg b
    else:
        code = load_values_to_registers([p[3], p[1]], [('a', 'b'), ('b', 'd')], p.lineno(2)) # loading val1 to reg b and val2 to reg a
    code += 'SUB a b\n'
    p[0] = code

### VALUE ###

def p_value_num(p):
    'value : NUM'
    p[0] = (p[1], False) # value is a number

def p_value_id(p):
    'value : identifier'
    p[0] = (p[1], True) # value is a variable

### INDENTIFIER ###

def p_identifier_variable(p):
    'identifier : PID'
    global symbol_table
    try:
        var_id = get_symbol_by_name('TI'+p[1], p.lineno(1))
    except Exception:
        var_id = get_symbol_by_name(p[1], p.lineno(1))
    if symbol_table[var_id][1]:
        raise Exception('Błąd w linii {0:d}: Niewłaściwe użycie zmiennej tablicowej {1:}'.format(p.lineno(1), p[1]))
    p[0] = symbol_table[var_id][4] # identifier = variable's memory address

def p_identifier_array_pid(p):
    'identifier : PID LPAR PID RPAR'
    try:
        id_mem = get_mem_id_by_name('TI'+p[3], p.lineno(3))
    except Exception:
        id_mem = get_mem_id_by_name(p[3], p.lineno(3))
    tab_id = get_symbol_by_name(p[1], p.lineno(1))
    if not symbol_table[tab_id][1]:
        raise Exception('Błąd w linii {0:d}: Zmienna {1:} nie jest tablicą'.format(p.lineno(1), p[1]))
    p[0] = (id_mem, symbol_table[tab_id][4], symbol_table[tab_id][2]) # identifier = (memory address of index, memory address of table, min id of table)

def p_identifier_array_num(p):
    'identifier : PID LPAR NUM RPAR'
    tab_id = get_symbol_by_name(p[1], p.lineno(1))
    if not symbol_table[tab_id][1]:
        raise Exception('Błąd w linii {0:d}: Zmienna {1:} nie jest tablicą'.format(p.lineno(1), p[1]))
    if p[3] < symbol_table[tab_id][2] or p[3] > symbol_table[tab_id][3]:
        raise Exception('Błąd w linii {0:d}: Indeks {1:d} jest poza zakresem tablicy {2:}'.format(p.lineno(1), p[3], p[1]))
    p[0] = symbol_table[tab_id][4] + int(p[3]) - symbol_table[tab_id][2] # identifier = table cell's memory address

### SYNTAX ERROR ###

def p_error(p):
    raise SyntaxError('Błąd składniowy w linii {0:d}'.format(p.lexer.lineno))

### MAIN ###

if __name__ == '__main__':
    data = ''
    with open(sys.argv[1], 'r') as f:
        data = f.read()
    lexer = build_lex()
    parser = yacc.yacc()
    try:
        compiled = parser.parse(data, tracking=True)
        with open(sys.argv[2], 'w') as f:
            f.write(compiled)
    except Exception as e:
        print(e)