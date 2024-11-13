
( stub )

( @ ! + nand S 0# )

: s@ S @ ;

: -1 s@ s@ nand s@ nand ;
:  0 -1 -1 nand ;
:  1 -1 -1 + -1 nand ;

: 2 1 1 + ;

: cell 2;

: 4 2 2 + ;
: 6 2 4 + ;
: 8 4 4 + ;

: drop sp@ ! ;
: dup  sp@ @ ;
: over sp@ cell + @ ;

: invert dup nand ;
: and nand invert ;

: - invert 1 + + ;
: true sp@ invert nand ; 
: false true true nand ; 

: p@ @ @ ;
: p! @ ! ;
: p++ @ cell + ;
: p-- @ cell - ;

: sp0 S cell + ;
: sp sp0 ;
: sp@ sp p@ ;
: sp! sp p! ;
: sp++ sp p++ sp p! ;
: sp-- sp p-- sp p! ;

: rp0 sp cell + ;
: rp rp0 ;
: rp@ rp p@ ;
: rp! rp p! ;
: rp++ rp p++ rp p! ;
: rp-- rp p-- rp p! ;

: ip0 rp cell + ;
: ip ip0 ;
: ip@ ip p@ ;
: ip! ip p! ;
: ip++ ip p++ ip p! ;
: ip-- ip p-- ip p! ;

: tp0 ip cell + ;
: tp tp0 ;
: tp@ tp p@ ;
: tp! tp p! ;
: tp++ tp p++ tp p! ;
: tp-- tp p-- tp p! ;

