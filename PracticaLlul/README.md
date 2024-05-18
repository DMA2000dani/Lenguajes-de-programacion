Aquest treball és una pràctica de la UPC feta en l'assignatura Llenguatges de Programació i entregada el 10-1-2022.
Aquesta pràctica tracta d'interpretar un llenguatge inventat (llull) definit en l'enunciat de la pràctica.

Aquesta pràctica consta de 4 fitxers:

- lull.g4: Gramàtica del llenguatge.

- llull.py: script en python que llegeix un programa des d'un fitxer i l'executa

- beat.py: script en python que llegeix un programa des d'un fitxer i li canvia l'estètica.

- requeriments.txt: fitxer amb les llibreries de pip3 que utilitza el beat.py.

Execució:

    1.  Compilar la gramàtica amb 'antlr4 -Dlanguage=Python3 -no-listener -visitor llull.g4'
    
    2.1 Si es vol executar el programa, només cal executar 'python3 llull.py nomPrograma.llull' 
        on nomPrograma és el nom del fitxer on està el programa.llull per executar.
    
    2.2 Si es vol veure estèticament millor: 
    
        2.2.1 executar 'pip3 install -r requerimets.txt' per descarregar les llibreries que utilitza el programa
        
        2.2.2 Executar el programa amb 'python3 llull.py nomPrograma.llull' on nomPrograma.llull és el fitxer amb el
              programa per veure-ho estèticament millor
