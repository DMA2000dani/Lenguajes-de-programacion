from antlr4 import *
from llullLexer import llullLexer
from llullParser import llullParser
from sys import argv
from colorama import Fore, Style

from llullVisitor import llullVisitor

if __name__ is not None and "." in __name__:
    from .llullParser import llullParser
    from .llullVisitor import llullVisitor
else:
    from llullParser import llullParser
    from llullVisitor import llullVisitor


class EvalVisitor(llullVisitor):

    def __init__(self):
        self.nivell = 0
        self.nivellIndentacio = 0
        self.bucle = 0
        return

    def visitRoot(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])
        return

    def visitFuncions(self, ctx):
        l = list(ctx.getChildren())
        print('entro a funcions')
        print(' la mida de la llista es', len(l))
        for i in l:
            self.visit(i)
        return

    def visitFuncio(self, ctx):
        l = list(ctx.getChildren())
        print(Fore.RED + 'void ', end='')
        print(Fore.WHITE + Style.BRIGHT + l[1].getText(), end='')
        print(Style.NORMAL, end='')

        print('(', end='')
        if l[3].getText() != '':
            self.visit(l[3])
        print(') {')
        self.nivellIndentacio = self.nivellIndentacio + 1
        self.visit(l[6])
        self.nivellIndentacio = self.nivellIndentacio - 1
        if(self.nivellIndentacio == 1):
            print('El nivel de identacion esta mal, es > 1')
        print('}')
        return

    def visitParametresF(self, ctx):
        l = list(ctx.getChildren())
#       Mirar que llega si no hay parametros
        if l[0].getText() != '':
            self.visit(l[0])
        if len(l) > 1:
            for i in range(1, len(l)):
                print(', ', end='')
                self.visit(l[i])
        return

    def visitParametreF(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])

    def visitInstruccions(self, ctx):
        l = list(ctx.getChildren())
        if len(l) == 1:
            self.visit(l[0])
        if(len(l) > 1):
            for elem in l:
                self.visit(elem)
        return

    def visitMetode(self, ctx):
        l = list(ctx.getChildren())

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print(Style.BRIGHT, l[0].getText(), end='')
        print(Style.NORMAL, '(', end='')
        self.visit(l[2])
        print(')', end='')

        return

    def visitParametresM(self, ctx):
        l = list(ctx.getChildren())
        for i in range(0, len(l)):
            if i % 2 == 0:
                self.visit(l[i])
            else:
                print(', ', end='')

        return

    def visitParametreM(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])
        return

    def visitAssignacio(self, ctx):
        l = list(ctx.getChildren())

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print(Fore.BLUE + l[0].getText(), end='')
        print(Fore.YELLOW, ' = ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])
        if self.bucle == 0:
            print()

        return

    def visitRead(self, ctx):
        l = list(ctx.getChildren())

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print('read(', end='')
        print(Fore.BLUE + l[2].getText(), end='')
        print(Fore.WHITE, end='')
        print(')')

        return

    def visitWrite(self, ctx):
        l = list(ctx.getChildren())

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print('write(', end='')
        self.visit(l[2])
        print(')')

        return

    def visitParamsWrite(self, ctx):
        l = list(ctx.getChildren())
        for i in range(0, len(l)):
            if(i % 2 == 0):
                self.visit(l[i])
            else:
                print(', ', end='')
        return

    def visitParamWrite(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])
        return

    def visitParametreExpr(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])
        return

    def visitParametreVariable(self, ctx):
        l = list(ctx.getChildren())
        print(Fore.BLUE, l[0].getText(), end='')
        print(Fore.WHITE, end='')
        return

    def visitParametreFrase(self, ctx):
        l = list(ctx.getChildren())
        print(Fore.GREEN, l[0], end='')
        print(Fore.WHITE, end='')
        return

    def visitWhiles(self, ctx):
        l = list(ctx.getChildren())

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print('while(', end='')
        self.visit(l[2])
        print(') {')

        self.nivellIndentacio = self.nivellIndentacio + 1
        self.visit(l[5])
        self.nivellIndentacio = self.nivellIndentacio - 1

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print('}')

        return

    def visitFors(self, ctx):
        l = list(ctx.getChildren())

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print('for(', end='')
        self.bucle = 1
        indentacio = self.nivellIndentacio
        self.nivellIndentacio = 0
        self.visit(l[2])
        print('; ', end='')

        self.visit(l[4])
        print('; ', end='')

        self.visit(l[6])
        print(') {')
        self.bucle = 0
        self.nivellIndentacio = indentacio + 1
        self.visit(l[9])
        self.nivellIndentacio = self.nivellIndentacio - 1

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')
        print('}')

        return

    def visitIfelse(self, ctx):
        l = list(ctx.getChildren())

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')

        print('if(', end='')
        self.visit(l[2])
        print(') {')

        self.nivellIndentacio = self.nivellIndentacio + 1
        self.visit(l[5])
        self.nivellIndentacio = self.nivellIndentacio - 1

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')
        print('}')

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')
        print('else {')

        self.nivellIndentacio = self.nivellIndentacio + 1
        self.visit(l[9])
        self.nivellIndentacio = self.nivellIndentacio - 1

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')
        print('}')

        return

    def visitIfs(self, ctx):
        l = list(ctx.getChildren())

        print('if(', end='')
        self.visit(l[2])
        print(') {')

        self.nivellIndentacio = self.nivellIndentacio + 1
        self.visit(l[5])
        self.nivellIndentacio = self.nivellIndentacio - 1

        for i in range(0, self.nivellIndentacio):
            print('    ', end='')
        print('}')

        return

    def visitInsexprCond(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])
        return

    def visitExprCondeq(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW, ' == ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprConddif(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW, ' <> ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

    def visitExprCondmen(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW, ' < ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprCondmajeq(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW, ' >= ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprCondmaj(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW, ' > ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprDiv(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW, ' / ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprMult(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW, ' * ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprSum(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW + ' + ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprRes(self, ctx):
        l = list(ctx.getChildren())

        self.visit(l[0])
        print(Fore.YELLOW + ' - ', end='')
        print(Fore.WHITE, end='')
        self.visit(l[2])

        return

    def visitExprNum(self, ctx):
        l = list(ctx.getChildren())
        print(Fore.CYAN + l[0].getText(), end='')
        print(Fore.WHITE, end='')

    def visitExprID(self, ctx):
        l = list(ctx.getChildren())
        print(Fore.BLUE + l[0].getText(), end='')
        print(Fore.WHITE, end='')

input_stream = FileStream(argv[1])
lexer = llullLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = llullParser(token_stream)
tree = parser.root()
print(tree.toStringTree(recog=parser))
visitor = EvalVisitor()
visitor.visit(tree)
