import ply.lex as lex

# List of token names
tokens = [
    'NAME',
    'NUMBER',
    'LPAREN',
    'RPAREN',
    'DEFVAR',
    'ASSIGN',
    'MOVE',
    'SKIP',
    'TURN',
    'FACE',
    'PUT',
    'PICK',
    'MOVEDIR',
    'RUNDIRS',
    'MOVEFACE',
    'NULL',
    'IF',
    'LOOP',
    'REPEAT',
    'DEFUN',
    'DIRECTION',
    'ORIENTATION',
    'FACING',
    'BLOCKED',
    'CANPUT',
    'CANPICK',
    'CANMOVE',
    'ISZERO',
    'NOT',
    'CONSTANT',
    'ITEM'
]

# Define rules for tokens
t_ignore = ' \t\n'

# Define rules for parentheses
t_LPAREN = r'\('
t_RPAREN = r'\)'

# Define tokens
def t_MOVEDIR(t):
    r'move-dir'
    return t

def t_RUNDIRS(t):
    r'run-dirs'
    return t

def t_MOVEFACE(t):
    r'move-face'
    return t

def t_CANPUT(t):
    r'can-put\?'
    return t

def t_CANPICK(t):
    r'can-pick\?'
    return t

def t_CANMOVE(t):
    r'can-move\?'
    return t

def t_ISZERO(t):
    r'iszero\?'
    return t

def t_FACING(t):
    r'facing\?'
    return t

def t_BLOCKED(t):
    r'blocked\?'
    return t

def t_DEFVAR(t):
    r'\bdefvar\b'
    return t

def t_ASSIGN(t):
    r'\b=\b'
    return t

def t_SKIP(t):
    r'\bskip\b'
    return t

def t_TURN(t):
    r'\bturn\b'
    return t

def t_PUT(t):
    r'\bput\b'
    return t

def t_PICK(t):
    r'\bpick\b'
    return t

def t_MOVE(t):
    r'\bmove\b'
    return t

def t_FACE(t):
    r'\bface\b'
    return t

def t_NULL(t):
    r'\bnull\b'
    return t

def t_IF(t):
    r'\bif\b'
    return t

def t_LOOP(t):
    r'\bloop\b'
    return t

def t_REPEAT(t):
    r'\brepeat\b'
    return t

def t_DEFUN(t):
    r'\bdefun\b'
    return t

def t_NOT(t):
    r'\bnot\b'
    return t

def t_DIRECTION(t):
    #TODO: Solo en el programa ejemplo incluyen up y down, toca preguntar si se incluyen
    r':(left|right|around|front|back|up|down)\b'
    return t

def t_ORIENTATION(t):
    r':(north|south|east|west)\b'
    return t

def t_ITEM(t):
    r':(balloons|chips)\b'
    return t

def t_CONSTANT(t):
    r'\b(dim|myxpos|myypos|mychips|myballoons|balloonshere|chipshere|spaces)\b'
    return t

def t_NUMBER(t):
    r'\b\d+\b'
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*' #TODO: Preguntar que reglas siguen los nombres
    return t

# Define EOF token rule
def t_eof(t):
    r'\Z'
    t.type = 'EOF'
    return t

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Test the lexer
data = '''
( defvar rotate 3)




( if (can-move? :north ) ( move-dir 1 :north ) ( null ) )
 
(
( if (not (blocked?)) ( move 1) ( null) )
(turn :left )
)

( defvar one 1)

( defun foo ( c p )
	(put :chips c )
	(put :balloons p )
	( move rotate ) )
( foo 1 3)
 
( defun goend ()
	( if (not (blocked?) )
	(( move one )
 	( goend ) )
 	( null ) ) )

( defun fill ()
( repeat Spaces ( if (not ( isZero? myChips ) ) (put :chips 1) ) )
)

( defun pickAllB ()
 (pick :balloons balloonsHere )
)


( run-dirs :left :up :left :down :right )

'''

data = data.lower()

lexer.input(data)

stack = []

token_actual = None

def parse():
    global token_actual
    global stack
    token_actual = lexer.token().type
    while token_actual != "EOF":
        statement()
    if len(stack) > 0:
        print("Parentesis impares")
    print("Final del archivo")
    
def statement():
    global token_actual
    global stack
    if token_actual == "LPAREN":
        #Siguiente token / funcion token_es_tipo
        token_es_tipo("LPAREN")
        #Parentesis iz en stack
        stack.append("LPAREN")
        statement()
    elif token_actual == "RPAREN":
        if len(stack) == 0:
            print("No hay parentesis izq para emparejar el parentisis der")
        else:
            stack.pop()
        token_es_tipo("RPAREN")
    elif token_actual == "IF":
        print("if statement")
        token_es_tipo("IF")
        pass
    elif token_actual == "DEFVAR":
        print("defvar statement")
        token_es_tipo("DEFVAR")
        pass
    else:
        print("otro")
        token_actual = lexer.token().type
        
def token_es_tipo(token_type):
    global token_actual
    if token_actual == token_type:
        token_actual = lexer.token().type
    else:
        print("Se esperaba un token tipo " + str(token_type) + " se obtuvo un token tipo " + str(token_actual))
parse()