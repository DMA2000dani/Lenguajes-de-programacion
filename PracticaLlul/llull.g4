grammar llull;

root : funcions EOF ;

funcions: funcio* ;

funcio: 'void' ID '(' parametresFunc ')' '{' instruccions '}';

parametresFunc: ID (',' ID)*    #ParametresF
    | ID                        #ParametreF
    ;

instruccions : instruccio*;

instruccio : whiles        
    | fors          
    | ifelse        
    | ifs           
    | assignacio    
    | read          
    | write
    | metode 
    // No he posat com a instrucció una expressió ja que només s'utilitza
    // per calcular alguna cosa i no s'utilitza sola.
    ;

metode: ID '(' parametresMetode ')';

parametresMetode: exprCond (',' exprCond)*   #ParametresM
    | exprCond                               #ParametreM
    ;
    
whiles : 'while' '(' exprCond ')' '{' instruccions '}';

fors : 'for' '(' assignacio ';' exprCond ';' assignacio ')' '{' instruccions '}'; 

ifelse : 'if' '(' exprCond ')' '{' instruccions '}' 'else' '{' instruccions '}'; 

ifs : 'if' '(' exprCond ')' '{' instruccions '}';

assignacio :  ID '=' exprCond;

read : 'read' '(' ID ')';

write : 'write' '(' parametresWrite ')';

parametresWrite: parametreWrite             #paramWrite
    | parametreWrite (',' parametreWrite)*  #paramsWrite
    ;
    
parametreWrite: Frase                 #parametreFrase
    | ID          #parametreVariable
    | exprCond              #parametreExpr
    ;

Frase: '"' ID (' ' ID)* '"' ;

exprCond : exprCond '==' exprCond #exprCondeq
    | exprCond '<>' exprCond      #exprConddif
    | exprCond '<'  exprCond       #exprCondmen
    | exprCond '>'  exprCond       #exprCondmaj
    | exprCond '<=' exprCond      #exprCondmeneq
    | exprCond '>=' exprCond      #exprCondmajeq
    | exprCond '*'  exprCond    #ExprMult
    | exprCond '/'  exprCond     #ExprDiv
    | exprCond '%'  exprCond     #ExprMod
    | exprCond '+'  exprCond     #ExprSum
    | exprCond '-'  exprCond     #ExprRes
    | NUM               #ExprNum
    | ID                #ExprID
    ; 
    
NUM : [0-9]+;
ID : [a-zA-Z]+;
WS : [ \n]+ -> skip;

