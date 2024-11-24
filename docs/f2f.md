
# Forth in Forth

just stubs

## pseudo PDP-11, DTC, all compiled words have DOCOL at start

define next
        mov (ip)+, wr
        jmp @(wr)+

define exec
        mov (sp)+, wr
        jmp @(wr)+

define nest // aka docol, enter
        mov ip, -(rp)
        mov wr, ip
        jmp next

define unnest // aka semis, exit
        move (rp)+, ip
        jmp next

define dovar
        mov ip+, -(sp)
        jmp next

define docon
        mov (ip)+, -(sp)
        jmp next

define branch   
        add (ip), ip
        jmp next

define qbranch
        mov (sp)+, wr
        bne wr, branch
        jmp next

define douse
        add up, (sp)
        jmp next


## in Forth

: unnest 
    R> IP ! next ;

: next  
    RDROP
    IP @ @ WR ! 
    IP @ CELL + IP ! 
    WR @ LIMIT @ <
    IF jump THEN nest ;

: nest
    RDROP
    IP @ >R
    WR @ IP !
    next ;

: jump
    RDROP
    WR @ JUMP ;


