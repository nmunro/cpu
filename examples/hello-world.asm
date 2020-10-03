*---- This is a hello world program

START   ORG $1000
        LEA     MESSAGE,a1
        MOVE.B  #14,d0
        TRAP    #15

        MOVE.B  #9,d0
        TRAP    #15

MESSAGE DC.B    "HELLO WORLD",0

        END     START
