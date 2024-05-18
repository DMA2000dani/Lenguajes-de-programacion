from antlr4 import *
from ExprLexer import ExprLexer
from ExprParser import ExprParser
from sys import argv

from ExprVisitor import ExprVisitor

if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
    from .ExprVisitor import ExprVisitor
else:
    from ExprParser import ExprParser
    from ExprVisitor import ExprVisitor


class EvalVisitor(ExprVisitor):
    
    def __init__ (self):
        self.nivell = 0
        self.parametres = {'main' : ['']}
        self.variables = {}
        self.pila = [self.variables]
        return
    
    def visitRoot(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])
        return
    
    def visitFuncio(self, ctx):
        l = list(ctx.getChildren())
        
        #inicialitzacio dels diccionaris de variables i la pila
        nom = l[1].getText()
        llistaNoms = self.visit(l[3]) #noms dels paràmetres
        llistaValors = self.parametres[nom] # valors dels paràmetres
        variablesNoves = {} # Diccionari de variables nou per aquesta funció
        if llistaNoms != None:
            for i in range(0, len(llistaNoms)):
                variablesNoves[llistanoms[i]] = llistavalors[i]
        self.pila.append(variablesNoves)
        self.variables = self.pila[len(self.pila) - 1]
        
        self.visit(l[6]) #execució de les instruccions de la funció
        
        self.pila.pop() #Eliminar les variables que hi havien
        return
   
    def visitParametresF(self, ctx):
        l = list(ctx.getChildren())
        llista = []
        for i in range(0, len(l)):
            if i % 2 == 0:
                llista = llista + [l[i].getText()]
        return
        
    def visitParametreF(self, ctx):
        l = list(ctx.getChildren())    
        return [l[0].getText()]
        
    def visitInstruccions(self, ctx):
        l = list(ctx.getChildren())
        if len(l) ==  1:
            self.visit(l[0])
        if(len(l) > 1):
            for elem in l:
                self.visit(elem)
        return
    
    def visitMetode(self, ctx):
        l = list(ctx.getChildren())
        if len(l) > 3:
            self.parametres[l[0].getText()] = self.visit(l[2])
        else:
            self.parametres[l[0].getText()] = []
        return
        
    def visitParametresM(self, ctx):
        l = list(ctx.getChildren())
        llista = []
        if(len(l) == 1):
            llista = [self.visit(l[0])]
        if len(l) > 3:
            for i in range(0, len(l)):
                if i % 2 == 0:
                    llista = llista + [self.visit(l[i])]
        
        return llista
    
    def visitParametreM(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0])
    
    
    def visitAssignacio(self, ctx):
        l = list(ctx.getChildren())
        evaluacio = self.visit(l[2])
        var = l[0].getText()
        self.variables[var] = evaluacio
        return
        
    def visitRead(self, ctx):
        l = list(ctx.getChildren())
        var = l[2].getText()
        lectura = int(input('? '))
        self.variables[var] = lectura
        return
    
    def visitWrite(self, ctx):
        l = list(ctx.getChildren())
        #print(len(l))
        self.visit(l[2])
        print('')
        return
    
    def visitParamsWrite(self, ctx):
        l = list(ctx.getChildren())
        for i in range(0, len(l)):
            if(i % 2 == 0):
                self.visit(l[i])
        return
    
    def visitParamWrite(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[0])
        return
    
    def visitParametreExpr(self, ctx):
        l = list(ctx.getChildren())
        print(self.visit(l[0]), end=' ')
        return
        
    def visitParametreVariable(self, ctx):
        l = list(ctx.getChildren())
        print(self.variables[l[0].getText()], end=' ')
        return
        
    
    def visitWhiles(self, ctx):
        l = list(ctx.getChildren())
        while self.visit(l[2]) == 1:
            self.visit(l[5])
        return
    
    def visitFors(self, ctx):
        l = list(ctx.getChildren())
        self.visit(l[2])
        while self.visit(l[4]):
            self.visit(l[9])
            self.visit(l[6])
        return
    
    def visitIfelse(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[2]) == 1:
            return self.visit(l[5])
        else:
            return self.visit(l[9])

    
    def visitIfs(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[2]) == 1:
            return self.visit(l[5])
        else:
            return
    
    def visitInsexprCond(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0])
    
    def visitExprCondeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) == self.visit(l[2]): 
            return 1
        else: 
            return 0
    
    def visitExprConddif(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) != self.visit(l[2]): 
            return 1
        else: 
            return 0
    
    def visitExprCondmen(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) < self.visit(l[2]): 
            return 1
        else: 
            return 0
    
    
    def visitExprCondmajeq(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) >= self.visit(l[2]): 
            return 1
        else: 
            return 0
    
    def visitExprCondmaj(self, ctx):
        l = list(ctx.getChildren())
        if self.visit(l[0]) > self.visit(l[2]): 
            return 1
        else: 
            return 0

    def visitExprDiv(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) / self.visit(l[2]) 
     
    def visitExprMult(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) * self.visit(l[2]) 
     
    def visitExprSum(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) + self.visit(l[2])
    
    def visitExprRes(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) - self.visit(l[2])
        
    def visitExprNum(self, ctx):
        l = list(ctx.getChildren())
        return int(l[0].getText())
    
    def visitExprID(self, ctx):
        l = list(ctx.getChildren())
        return self.variables[l[0].getText()]

input_stream = FileStream(argv[1])
lexer = ExprLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = ExprParser(token_stream)
tree = parser.root()
#print(tree.toStringTree(recog=parser))

visitor = EvalVisitor()
visitor.visit(tree)
