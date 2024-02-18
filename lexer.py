import token
import ply.lex as lex
import sys

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
    r':(left|right|around|front|back)\b'
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
    r'[a-z][a-z0-9]*'
    return t

# Define EOF token rule
def t_eof(t):
    r'\Z'
    t.type = 'EOF'
    return t

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)#TODO: Toca cambiar esto para matarlo si hay un ilegal character?

# Build the lexer
lexer = lex.lex()

# Test the lexer
#TODO: Cambiar :front del primer move-dir por :north (así es originalmente en el ejemplo); se preguntó a la monitora sobre la inconsistencia
data = '''
( defvar rotate 3)




( if (can-move? :north ) ( move-dir 1 :front ) ( null ) )

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


( run-dirs :left :front :left :back :right )

'''

data = data.lower()

lexer.input(data)

#Excepcion
class InvalidSyntaxException(Exception):
    "Lanzada si hay un error de syntax"
    pass

stack = []

token_actual = None

diccionario_funciones = {}
lista_variables = []

def parse():
    global token_actual
    global stack
    token_actual = lexer.token()
    try:
        while token_actual.type != "EOF":
            statement()
        if len(stack) > 0:
            print("Parentesis impares")
            raise InvalidSyntaxException
        print("Final del archivo")
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def statement(funcion=None):
    global token_actual
    global stack

    try:
        if token_actual.type == "LPAREN":
            #Siguiente token / funcion token_es_tipo
            token_es_tipo("LPAREN")
            #Parentesis iz en stack
            stack.append("LPAREN")
            statement()
        elif token_actual.type == "RPAREN":
            if len(stack) == 0:
                print("No hay parentesis izq para emparejar el parentisis der")
                raise InvalidSyntaxException
            else:
                stack.pop()
            token_es_tipo("RPAREN")
        elif token_actual.type == "DEFVAR":
            print("defvar statement")
            verificar_defvar()
        elif token_actual.type == "ASSIGN":
            print("assign statement")
            verificar_assign()
        elif token_actual.type == "MOVE":
            print("move statement")
            verificar_move()
        elif token_actual.type == "SKIP":
            print("skip statement")
            verificar_skip()
        elif token_actual.type == "TURN":
            print("turn statement")
            verificar_turn()
        elif token_actual.type == "FACE":
            print("face statement")
            verificar_face()
        elif token_actual.type == "PUT":
            print("put statement")
            verificar_put(funcion)
        elif token_actual.type == "PICK":
            print("pick statement")
            verificar_pick()
        elif token_actual.type == "MOVEDIR":
            print("movedir statement")
            verificar_movedir()
        elif token_actual.type == "RUNDIRS":
            print("rundirs statement")
            verificar_rundirs()
        elif token_actual.type == "MOVEFACE":
            print("moveface statement")
            verificar_moveface()
        elif token_actual.type == "NULL":
            print("null statement")
            verificar_null()
        elif token_actual.type == "IF":
            print("if statement")
            token_es_tipo("IF")
            if token_actual.type != "LPAREN":
                    raise InvalidSyntaxException
            verificarIf()
        elif token_actual.type == "DEFUN":
            print("defun statement")
            verificar_defun(funcion)
        elif token_actual.type == "LOOP":
            print("Loop statement")
            token_es_tipo("LOOP")
            if token_actual.type != "LPAREN":
                    raise InvalidSyntaxException
            verificarLoop()
        elif token_actual.type == "REPEAT":
            print("Repeat statement")
            token_es_tipo("REPEAT")
            verificarRepeat()
        elif token_actual.type == "NAME":
            if token_actual.value in diccionario_funciones.keys():
                print("FunCall statement")
                verificarCallFun()
        else:
            raise InvalidSyntaxException
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def token_es_tipo(token_type):
    global token_actual
    try:
        if token_actual.type == token_type:
            token_actual = lexer.token()
        else:
            print("Se esperaba un token tipo " + str(token_type) + " se obtuvo un token tipo " + str(token_actual.type))
            raise InvalidSyntaxException
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_defvar():
    global lista_variables
    try:
        token_es_tipo("DEFVAR")
        if token_actual.value in lista_variables:
            raise InvalidSyntaxException
        nombre_variable = token_actual.value
        token_es_tipo("NAME")
        tipo_n = token_actual.type
        if not (tipo_n == "NUMBER" or tipo_n == "CONSTANT"):
            raise InvalidSyntaxException
        token_es_tipo(tipo_n)
        lista_variables.append(nombre_variable)
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)
#TODO: A una variable se le puede asignar un parámetro si el assign está dentro de una definición de función; modificar para que n pueda ser parámetro de una función
def verificar_assign():
    try:
        token_es_tipo("ASSIGN")
        if token_actual.value not in lista_variables:
            raise InvalidSyntaxException
        token_es_tipo("NAME")
        tipo_n = token_actual.type
        if not (tipo_n == "NUMBER" or tipo_n == "CONSTANT"):
            raise InvalidSyntaxException
        token_es_tipo(tipo_n)
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_move(funcion=None):
    try:
        token_es_tipo("MOVE")
        tipo_n = token_actual.type
        if not (tipo_n == "NUMBER" or tipo_n == "CONSTANT" or tipo_n == "NAME"):
            raise InvalidSyntaxException
        if (tipo_n == "NAME") and (token_actual.value not in lista_variables) and (funcion is None):
            raise InvalidSyntaxException
        if (funcion is not None):
            if token_actual.value not in funcion:
                raise InvalidSyntaxException
        token_es_tipo(tipo_n)
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_skip(funcion=None):
    try:
        token_es_tipo("SKIP")
        tipo_n = token_actual.type
        if not (tipo_n == "NUMBER" or tipo_n == "CONSTANT" or tipo_n == "NAME"):
            raise InvalidSyntaxException
        if (tipo_n == "NAME") and (token_actual.value not in lista_variables) and (funcion is None):
            raise InvalidSyntaxException
        if (funcion is not None):
            if token_actual.value not in funcion:
                raise InvalidSyntaxException
        token_es_tipo(tipo_n)
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)
#TODO: A un turn se le puede meter un parámetro si esta dentro de la definición de una función y el parámetro
# es de tipo dirección, pero no se puede saber el tipo del parámetro de antemano
# aunque se puede cambiar; modificar para que n pueda ser parámetro de una función
def verificar_turn():
    try:
        token_es_tipo("TURN")
        if token_actual.value not in [":left", ":right", ":around"]:
            raise InvalidSyntaxException
        token_es_tipo("DIRECTION")
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_face():
    try:
        token_es_tipo("FACE")
        token_es_tipo("ORIENTATION")
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_put(funcion=None):
    try:
        token_es_tipo("PUT")
        token_es_tipo("ITEM")
        tipo_n = token_actual.type
        if not (tipo_n == "NUMBER" or tipo_n == "NAME"):
            raise InvalidSyntaxException
        if (tipo_n == "NAME") and (token_actual.value not in lista_variables) and (funcion is None):
            raise InvalidSyntaxException
        if (funcion is not None):
            if token_actual.value not in funcion:
                raise InvalidSyntaxException
        token_es_tipo(tipo_n)
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_pick(funcion=None):
    try:
        token_es_tipo("PICK")
        token_es_tipo("ITEM")
        tipo_n = token_actual.type
        # if not (tipo_n == "NUMBER" or tipo_n == "NAME"):
            # raise InvalidSyntaxException
        # if (tipo_n == "NAME") and (token_actual.value not in lista_variables) and (funcion is None): #TODO: Pick es diferente en el ejemplo (se preguntó a la monitora sobre la inconsistencia)
        #     raise InvalidSyntaxException
        # if (funcion is not None):
        #     if token_actual.value not in funcion:
        #         raise InvalidSyntaxException
        token_es_tipo(tipo_n)
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_movedir(funcion=None):
    try:
        token_es_tipo("MOVEDIR")
        tipo_n = token_actual.type
        if not (tipo_n == "NUMBER" or tipo_n == "NAME"):
            raise InvalidSyntaxException
        if (tipo_n == "NAME") and (token_actual.value not in lista_variables) and (funcion is None):
            raise InvalidSyntaxException
        if (funcion is not None):
            if token_actual.value not in funcion:
                raise InvalidSyntaxException
        token_es_tipo(tipo_n)
        if token_actual.value not in [":front", ":right", ":left", ":back"]:
            raise InvalidSyntaxException
        token_es_tipo("DIRECTION")
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_rundirs():
    try:
        token_es_tipo("RUNDIRS")
        if token_actual.type != "DIRECTION":#TODO: Cambiar dependiendo de como se ve la lista de direcciones
            raise InvalidSyntaxException
        while token_actual.type == "DIRECTION":
            token_es_tipo("DIRECTION")
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_moveface(funcion=None):
    try:
        token_es_tipo("MOVEFACE")
        tipo_n = token_actual.type
        if not (tipo_n == "NUMBER" or tipo_n == "NAME"):
            raise InvalidSyntaxException
        if (tipo_n == "NAME") and (token_actual.value not in lista_variables) and (funcion is None):
            raise InvalidSyntaxException
        if (funcion is not None):
            if token_actual.value not in funcion:
                raise InvalidSyntaxException
        token_es_tipo(tipo_n)
        token_es_tipo("ORIENTATION")
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_null():
    try:
        token_es_tipo("NULL")
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificarIf():
    global token_actual
    global stack
    global lista_variables
    try:
        while len(stack) > 0 and token_actual.type != "EOF":
            if token_actual.type == "LPAREN":
            #Siguiente token / funcion token_es_tipo
                token_es_tipo("LPAREN")
            #Parentesis iz en stack
                stack.append("LPAREN")
                verificarIf()
            elif token_actual.type == "RPAREN":
                if len(stack) == 0:
                    print("No hay parentesis izq para emparejar el parentisis der")
                    raise InvalidSyntaxException
                else:
                    stack.pop()
                token_es_tipo("RPAREN")
            elif token_actual.type == "FACING":
                token_es_tipo("FACING")
                if token_actual.type == "ORIENTATION":
                    token_es_tipo("ORIENTATION")
                    verificarBloque()
                else:
                    print("No es de tipo ORIENTATION")
                    raise InvalidSyntaxException
            elif token_actual.type == "BLOCKED":
                token_es_tipo("BLOCKED")
                verificarBloque()
            elif token_actual.type == "CANPUT":
                token_es_tipo("CANPUT")
                if token_actual.type == "ITEM":
                    token_es_tipo("ITEM")
                    if token_actual.type == "NUMBER":
                        token_es_tipo("NUMBER")
                    elif token_actual.type == "NAME":
                        if token_actual.value in lista_variables:
                            token_es_tipo("NAME")
                        else:
                            raise InvalidSyntaxException
                    elif token_actual.type == "CONSTANT":
                        token_es_tipo("CONSTANT")
                    else:
                        print("No es de tipo NUMBER/CONSTANT/NAME")
                        raise InvalidSyntaxException
                else:
                    print("No es de tipo ITEM")
                    raise InvalidSyntaxException
            elif token_actual.type == "CANPICK":
                token_es_tipo("CANPICK")
                if token_actual.type == "ITEM":
                    token_es_tipo("ITEM")
                    if token_actual.type == "NUMBER":
                        token_es_tipo("NUMBER")
                    elif token_actual.type == "NAME":
                        if token_actual.value in lista_variables:
                            token_es_tipo("NAME")
                        else:
                            raise InvalidSyntaxException
                    elif token_actual.type == "CONSTANT":
                        token_es_tipo("CONSTANT")
                    else:
                        print("No es de tipo NUMBER/CONSTANT/NAME")
                        raise InvalidSyntaxException
                else:
                    print("No es de tipo ITEM")
                    raise InvalidSyntaxException
            elif token_actual.type == "CANMOVE":
                token_es_tipo("CANMOVE")
                if token_actual.type == "ORIENTATION":
                    token_es_tipo("ORIENTATION")
                    verificarBloque()
                else:
                    print("No es de tipo ORIENTATION")
                    raise InvalidSyntaxException
            elif token_actual.type == "ISZERO":
                token_es_tipo("ISZERO")
                if token_actual.type == "NUMBER":
                    token_es_tipo("NUMBER")
                elif token_actual.type == "NAME":
                    if token_actual.value in lista_variables:
                        token_es_tipo("NAME")
                    else:
                        raise InvalidSyntaxException
                elif token_actual.type == "CONSTANT":
                    token_es_tipo("CONSTANT")
                else:
                    print("No es de tipo NUMBER/CONSTANT/NAME")
                    raise InvalidSyntaxException
            elif token_actual.type == "NOT":
                token_es_tipo("NOT")
                #Revisar condición
                if token_actual.type == "LPAREN":
                    # token_es_tipo("LPAREN")
                    # if token_actual.type in condiciones:
                    #     token_es_tipo(token_actual.type)
                    # if token_actual.type == "RPAREN"
                    verificarIf()
                else:
                    print("La condición no es válida")
                    raise InvalidSyntaxException
            else:
                statement()
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificar_defun(funcion=None):
    global diccionario_funciones
    try:
        token_es_tipo("DEFUN")
        if token_actual.type != "NAME":
            raise InvalidSyntaxException
        nombre_funcion = token_actual.value
        if nombre_funcion in diccionario_funciones:
            raise InvalidSyntaxException
        token_es_tipo("NAME")
        token_es_tipo("LPAREN")
        if token_actual.type != "NAME" and token_actual.type != "RPAREN":
            raise InvalidSyntaxException
        lista_parametros = []
        while token_actual.type == "NAME":
            lista_parametros.append(token_actual.value)
            token_es_tipo("NAME")
        token_es_tipo("RPAREN")
        diccionario_funciones[nombre_funcion] = lista_parametros
        funcion=lista_parametros
        verificarBloque(funcion=funcion)
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)

def verificarBloque(funcion=None):
    global token_actual
    global stack
    try:
        while len(stack) > 0 and token_actual.type != "EOF":
            if token_actual.type == "LPAREN":
                token_es_tipo("LPAREN")
                stack.append("LPAREN")
                verificarBloque(funcion=funcion)
            elif token_actual.type == "RPAREN":
                if len(stack) == 0:
                    print("No hay parentesis iz para emparejar el parentesis der")
                    raise InvalidSyntaxException
                else:
                    stack.pop()
                token_es_tipo("RPAREN")
            else:
                statement(funcion=funcion)
    except InvalidSyntaxException:
        print("No")

def verificarLoop():
    global token_actual
    global stack
    try:
        while len(stack) > 0 and token_actual.type != "EOF":
            if token_actual.type == "LPAREN":
                token_es_tipo("LPAREN")
                stack.append("LPAREN")
                verificarLoop()
            elif token_actual.type == "RPAREN":
                if len(stack) == 0:
                    print("No hay parentesis iz para emparejar el parentesis der")
                    raise InvalidSyntaxException
                else:
                    stack.pop()
                token_es_tipo("RPAREN")
            elif token_actual.type == "FACING":
                token_es_tipo("FACING")
                if token_actual.type == "ORIENTATION":
                    token_es_tipo("ORIENTATION")
                    verificarBloque()
                else:
                    print("No es de tipo ORIENTATION")
                    raise InvalidSyntaxException
            elif token_actual.type == "BLOCKED":
                token_es_tipo("BLOCKED")
                verificarLoop()
            elif token_actual.type == "CANPUT":
                token_es_tipo("CANPUT")
                if token_actual.type == "ITEM":
                    token_es_tipo("ITEM")
                    if token_actual.type == "NUMBER":
                        token_es_tipo("NUMBER")
                    elif token_actual.type == "NAME":
                        if token_actual.value in lista_variables:
                            token_es_tipo("NAME")
                        else:
                            raise InvalidSyntaxException
                    elif token_actual.type == "CONSTANT":
                        token_es_tipo("CONSTANT")
                    else:
                        print("No es de tipo NUMBER/CONSTANT/NAME")
                        raise InvalidSyntaxException
                else:
                    print("No es de tipo ITEM")
                    raise InvalidSyntaxException
            elif token_actual.type == "CANPICK":
                token_es_tipo("CANPICK")
                if token_actual.type == "ITEM":
                    token_es_tipo("ITEM")
                    if token_actual.type == "NUMBER":
                        token_es_tipo("NUMBER")
                    elif token_actual.type == "NAME":
                        if token_actual.value in lista_variables:
                            token_es_tipo("NAME")
                        else:
                            raise InvalidSyntaxException
                    elif token_actual.type == "CONSTANT":
                        token_es_tipo("CONSTANT")
                    else:
                        print("No es de tipo NUMBER/CONSTANT/NAME")
                        raise InvalidSyntaxException
                else:
                    print("No es de tipo ITEM")
                    raise InvalidSyntaxException
            elif token_actual.type == "CANMOVE":
                token_es_tipo("CANMOVE")
                if token_actual.type == "ORIENTATION":
                    token_es_tipo("ORIENTATION")
                    verificarBloque()
                else:
                    print("No es de tipo ORIENTATION")
                    raise InvalidSyntaxException
            elif token_actual.type == "ISZERO":
                token_es_tipo("ISZERO")
                if token_actual.type == "NUMBER":
                    token_es_tipo("NUMBER")
                elif token_actual.type == "NAME":
                    if token_actual.value in lista_variables:
                        token_es_tipo("NAME")
                    else:
                        raise InvalidSyntaxException
                elif token_actual.type == "CONSTANT":
                    token_es_tipo("CONSTANT")
                else:
                    print("No es de tipo NUMBER/CONSTANT/NAME")
                    raise InvalidSyntaxException
            elif token_actual.type == "NOT":
                token_es_tipo("NOT")
                #Revisar condición
                if token_actual.type == "LPAREN":
                    verificarLoop()
                else:
                    print("La condición no es válida")
                    raise InvalidSyntaxException
            else:
                statement()
    except:
        print("No")
        sys.exit(1)

def verificarRepeat():
    global token_actual
    global stack
    try:
        while len(stack) > 0 and token_actual.type != "EOF":
            if token_actual.type == "LPAREN":
                token_es_tipo("LPAREN")
                stack.append("LPAREN")
                verificarRepeat()
            elif token_actual.type == "RPAREN":
                if len(stack) == 0:
                    print("No hay parentesis iz para emparejar el parentesis der")
                    raise InvalidSyntaxException
                else:
                    stack.pop()
                token_es_tipo("RPAREN")
            elif token_actual.type == "NUMBER":
                token_es_tipo("NUMBER")
                if token_actual.type != "LPAREN":
                    raise InvalidSyntaxException
            elif token_actual.type == "NAME":
                if token_actual.value in lista_variables:
                    token_es_tipo("NAME")
                    if token_actual.type != "LPAREN":
                        raise InvalidSyntaxException
                else:
                    raise InvalidSyntaxException
            elif token_actual.type == "CONSTANT":
                token_es_tipo("CONSTANT")
                if token_actual.type != "LPAREN":
                    raise InvalidSyntaxException
            else:
                statement()
    except:
        print("No")
        sys.exit(1)

def verificarCallFun():
    global diccionario_funciones
    global lista_variables
    contadorParametros = 0
    funName = ""
    try:
        funName = token_actual.value
        token_es_tipo("NAME")
        while token_actual.type != "RPAREN":
            if token_actual.type == "NUMBER":
                token_es_tipo("NUMBER")
                contadorParametros += 1
            elif token_actual.type == "NAME":
                if token_actual.value in lista_variables:
                    token_es_tipo("NAME")
                    contadorParametros +=1
                else:
                    print("Variable no definida")
                    raise InvalidSyntaxException
            elif token_actual.type == "CONSTANT":
                token_es_tipo("CONSTANT")
                contadorParametros += 1
            elif token_actual.type == "ITEM":
                token_es_tipo("ITEM")
                contadorParametros += 1
            elif token_actual.type == "DIRECTION":
                token_es_tipo("DIRECTION")
                contadorParametros += 1
            elif token_actual.type == "ORIENTATION":
                token_es_tipo("ORIENTATION")
                contadorParametros += 1
            else:
                raise InvalidSyntaxException

        #Revisar cantidad de parametros
        if contadorParametros != len(diccionario_funciones[funName]):
            raise InvalidSyntaxException        
    except InvalidSyntaxException:
        print("No")
        sys.exit(1)


parse()
print("Si")


