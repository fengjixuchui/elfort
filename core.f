# ===== Constants =====
LEXI REFER [core] EDIT

-1 as: ok
 0 as: ng

-1 as: yes
 0 as: no

-1 as: GO
 0 as: STOP



# ===== Inner Interpreter =====

code: jmp ( adr -- )
    TP rax movq:rr
    TP pop-ds
    rax jmpq
;

prim: LIT ( -- )
    #       | LIT
    # ip -> | n
    #       | next
    rax pop  # ip
    TP push-ds
    rax TP movq:mr
    8 rax rax leaq
    rax push
;

prim: >r ( adr -- )
    rax pop
    TP push
    rax push
    TP pop-ds
;

prim: r> ( -- adr )
    rax pop
    TP push-ds
    TP pop
    rax push
;

prim: rdrop ( -- )
    rax pop
    rdx pop
    rax push
;

: call ( q -- ) >r ;



# ===== Stack =====

prim: dup
    TP push-ds
;

prim: swap
    TP rax movq:rr
    SP TP 0 movq:mro
    rax SP 0 movq:rmo
;

prim: over
    TP push-ds
    SP TP 8 movq:mro
;

prim: drop
    TP pop-ds
;

prim: nip ( a b -- b )
    rax pop-ds
;

prim: tuck ( a b -- b a b )
    rax pop-ds
    TP push-ds
    rax push-ds
;

prim: 2dup ( a b -- a b a b )
    SP rax 0 movq:mro # a   | rax:a TP:b
    TP push-ds        # a b | rax:a TP:b
    rax push-ds
;

prim: 2drop ( a b -- )
    TP pop-ds  TP pop-ds
;

prim: 3drop ( a b c -- )
    TP pop-ds  TP pop-ds  TP pop-ds
;

: ?dup ( ? -- | ? ) dup IF dup THEN ;



# ===== Memory =====

prim: @
    TP TP movq:mr
;

prim: !
    rax pop-ds  # value
    rax TP movq:rm
    TP pop-ds
;

prim: b@
    rax rax xorq
    TP rax movb:mr
    rax TP movq:rr
;

prim: b!
    rax pop-ds  # value
    rax TP movb:rm
    TP pop-ds
;

prim: w@
    rax rax xorq
    TP rax 0 movw:mro
    rax TP movq:rr
;

prim: w!
    rax pop-ds  # value
    rax TP 0 movw:rmo
    TP pop-ds
;

prim: memclear ( adr len -- )
    rax rax xorq
    TP rcx movq:rr    
    rdi pop-ds    
    rep-stosb    
    TP pop-ds
;



# ===== Arithmetics =====

prim: +
    rax pop-ds
    rax TP addq:rr
;

prim: -
    rax pop-ds
    TP rax subq:rr
    rax TP movq:rr
;

prim: *
    rax pop-ds
    rax TP imulq:rr
;

prim: /mod ( a b -- q r )
    rdx rdx xorq   # rdx:rax
    rax pop-ds     # dividee: a
    TP idivq:r     # divider: b
    rax push-ds    # quotient
    rdx TP movq:rr # remainder
;

prim: / ( a b -- q )
    rdx rdx xorq   # rdx:rax
    rax pop-ds     # dividee: a
    TP idivq:r     # divider: b
    rax TP movq:rr # quotient
;

prim: mod ( a b -- r )
    rdx rdx xorq   # rdx:rax
    rax pop-ds     # dividee: a
    TP idivq:r     # divider: b
    rdx TP movq:rr # remainder
;

prim: 1+
    TP incq:r
;

prim: 1-
    TP decq:r
;

prim: cell
    TP push-ds
    8 TP movq:wr
;

prim: cells
    8 rax movq:wr
    rax TP imulq:rr
;



# ===== Compare =====

prim: =
    rax pop-ds ( rax TP -- TP )
    TP rax cmpq:rr    
    rax sete    
    rax TP movzbq    
;

: != = IF 0 ELSE -1 THEN ;

: not IF no ELSE yes THEN ;

prim: > ( a b -- a>b )
    rax pop-ds ( rax TP -- rax>TP )
    TP rax cmpq:rr
    rax setg
    rax TP movzbq
;

prim: >= ( a b -- a>=b )
    rax pop-ds ( rax TP -- rax>TP )
    TP rax cmpq:rr
    rax setge
    rax TP movzbq    
;

: <  swap > ;
: <= swap >= ;



# ===== Logical =====

prim: and
    rax pop-ds
    rax TP andq
;

prim: or
    rax pop-ds
    rax TP orq
;

prim: inv
    TP invq
;



# ===== Syscall I/O =====

prim: sys:read ( adr len fd -- len )
    ( read ) 0x00 rax movq:wr
    ( fd   ) TP rdi movq:rr
    ( len  ) rdx pop-ds
    ( adr  ) rcx pop-ds
    rsi push
    rcx rsi movq:rr
    syscall
    rsi pop
    rax TP movq:rr
;

prim: sys:write ( adr len fd -- )
    # TODO: check to save/restore registers required?
    ( write ) 0x01 rax movq:wr
    ( fd    ) TP rdi movq:rr
    ( len   ) rdx pop-ds
    ( adr   ) rcx pop-ds  # actual: rsi
    rsi push
    rcx rsi movq:rr
    syscall
    rsi pop
    TP pop-ds
;



# ===== Syscall Process =====

prim: sys:exit
    0x3C rax movq:wr  # exit
    TP rdi movq:rr    # exit code
    syscall
;

: bye 0 sys:exit ;



# ===== Quotation =====

: if ( ? qthen qelse -- .. ) >r >r IF r> rdrop ELSE rdrop r> THEN >r ;

: when   ( ? q -- .. ) >r IF RET   THEN rdrop ;
: unless ( ? q -- .. ) >r IF rdrop THEN ;


: ;when   ( ? q -- .. ) swap IF rdrop >r ELSE drop     THEN ;
: ;unless ( ? q -- .. ) swap IF drop     ELSE rdrop >r THEN ;


: ;case ( a b q -- a | .. )
    >r over = IF drop r> rdrop >r RET THEN rdrop
;


# ----- Iterator -----

: while ( q -- .. )  # q: -- ?
    dup >r call IF r> AGAIN THEN rdrop
;


# ----- Combinator -----

: dip ( a q -- ... a ) swap >r call r> ;
    # escape a, call q, then restore a
    # example:
    #   1 3 [ inc ] dip  => 2 3


: sip ( a q -- ... a ) over >r call r> ;
    # copy & restore a
    # eample:
    #   1 [ inc ] => 2 1


: biq ( a q1 q2 -- aq1 aq2 ) >r over >r call r> ; ( return to quotation )
    # biq - bi quotations application


: bia ( a b q -- aq bq ) swap over >r >r call r> ; ( return to quotation )
    # bia - bi arguments application


: bi* ( a b q1 q2 -- aq1 bq2 ) >r swap >r call r> ; ( return to quotation )


: bibi ( a b q1 q2 -- abq1 abq2 )
    >r >r 2dup ( a b a b | q2 q1 )
    r> swap >r ( a b a q1 | q2 b )
    swap >r    ( a b q1 | q2 b a )
    call r> r> ; ( return to quotation )



# ===== String =====

: s= ( s1 s2 -- ? )
    [ 2dup [ b@ ] bia  # s1 s2 c1 c2
        ( diff ) over != [ 3drop no STOP ] ;when
        ( end  ) 0 [ 2drop yes STOP ] ;case
        ( next ) drop [ 1+ ] bia GO
    ] while
;


# ===== Standard I/O =====

0x00 as: fd:stdin
0x01 as: fd:stdout


# ----- stdout -----

256 buf: obuf  # output buffer
8   buf: obp   # output buffer pointer

: olen obp @ obuf - ;
: flush  obuf olen fd:stdout sys:write  obuf obp ! ;

: >obuf ( c -- c )
    dup
    olen 255 > IF flush THEN
    obp @ b!
    obp @ 1+ obp !
;

: pr ( s -- )
    obuf obp !
    [ dup b@ >obuf
      0 [ drop flush STOP ] ;case
      drop 1+ GO
    ] while
;

: putc ( c -- )  obuf b!  obuf 1 fd:stdout sys:write ;

: cr    0x0A putc ;
: space 0x20 putc ;

: prn ( s -- ) pr cr ;



# ----- numerical output -----

: >hex ( n -- c ) dup 10 < IF 48 ELSE 55 THEN + ;

var: np
var: nsign
var: nbase
var: n
65 as: nmax
nmax buf: nbuf  # max: 64(bit) for 0/1
: >nbuf ( c -- ) np 1- np! np b! ;

: <$ ( n nbase -- )
    nbase! n!
    nbuf nmax + np!
    0 >nbuf
;

: sign nsign [ CHAR: - >nbuf ] when ;

: $u
    [ n nbase /mod ( q r )
      >hex >nbuf
      0 [ STOP ] ;case
      n! GO
    ] while
;

: $s
    n 0 < [ n -1 * n! yes ] [ no ] if nsign!
    $u
;

: $> ( -- buf ) np ;

: $d ( n -- s ) 10 <$ $s sign $> ;
: $x ( n -- s ) 16 <$ $u      $> ;
: $b ( n -- s ) 2  <$ $u      $> ;

: ..  ( n -- ) $d pr ;
: ..x ( n -- ) $x pr ;
: ..b ( n -- ) $b pr ;

: .  ( n -- ) ..  cr ;
: .x ( n -- ) ..x cr ;
: .b ( n -- ) ..b cr ;

: ?  ( n -- n ) dup ..  space ;
: ?x ( n -- n ) dup ..x space ;
: ?b ( n -- n ) dup ..b space ;


( TODO: yokusuru )
: dump ( adr len -- )
    [
        dup 0 <= [ drop STOP ] ;when
        over b@ ..x space
        [ 1+ ] [ 1- ] bi* GO
    ] while cr
;



# ----- stdin -----

256 buf: ibuf     # input buffer
255  as: ibufmax  # null terminated

: gets ( -- adr )
    ibuf 256 memclear
    ibuf ibufmax fd:stdin sys:read
    ibuf + 0 swap b!
    ibuf
;



# ----- take -----

256 buf: cbuf     # character buffer
256  as: cbufmax  # null terminated
  8 buf: cbp      # character buffer pointer
  8 buf: clast    # last peeked character

: cbuf:clear
    cbuf cbufmax memclear
    cbuf cbp !
;

: cbuf:init
    cbuf:clear
    0 clast !
    0 cbuf cbufmax + b!
;

: cur cbp @ b@ ;

: fill ( -- )
    cbuf:clear
    cbuf cbufmax fd:stdin sys:read ( len )
    cbuf + 0 swap b!
;

: scoop ( -- c )
    cur ?dup IF RET THEN fill cur
;

: peek ( -- c )
    clast b@ ?dup IF RET THEN
    scoop dup clast b!
;

: take ( -- c )
    peek
    cbp @ 1+ cbp !
    0 clast b!
;



# ===== Parse Number =====

: c>hex ( c -- n yes | no )
    dup CHAR: 0 <  [ drop no  ] ;when  # < 0
    dup CHAR: 9 <= [ 48 - yes ] ;when  # 0-9
    dup CHAR: A <  [ drop no  ] ;when  # 9 < c < A
    dup CHAR: G <  [ 55 - yes ] ;when  # A-F
    dup CHAR: a <  [ drop no  ] ;when  # A < c < a
    CHAR: a - dup 6 > [ drop no ] [ 10 + yes ] if
;

var: sbase
var: ssign
var: sacc

: s>n ( s base -- n yes | no )
    sbase!
    dup b@ CHAR: - = [ 1+ -1 ] [ 1 ] if ssign!
    dup b@ [ drop no ] ;unless ( null string )
    0 sacc! ( s )
    [ ( s ) dup b@
      ( done ) 0 [ drop yes STOP ] ;case
      ( skip ) CHAR: _ [ 1+ GO ] ;case
      ( NaN  ) c>hex [ drop no STOP ] ;unless ( s c )
      ( over ) dup sbase >= [ 2drop no STOP ] ;when
      ( ok   ) sacc sbase * + sacc!  1+ GO
    ] while ( dec? )
    [ sacc ssign * yes ] [ no ] if
;

: s>bin 2  s>n ;
: s>dec 10 s>n ;
: s>hex 16 s>n ;



# =============================
# ===== Outer Interpreter =====
# =============================

64 buf: tbuf  # token buffer
63  as: tmax  # token max
var: tp       # token pointer

: space? ( c -- ? )
    0x00 [ yes ] ;case
    0x0A [ yes ] ;case
    0x20 [ yes ] ;case
    drop no
;

: skip-spaces ( -- )
    [ peek  space? [ take drop GO ] ;when  STOP ] while
;

: read-token ( -- buf )
    tbuf tp!
    skip-spaces
    [ take
      dup space? [ drop STOP ] ;when
      tp b! tp 1+ tp! GO
    ] while
    0 tp b!
    tbuf
;

# ----- Dictionary -----

var: mode    # compile:-1  run: 0
var: here    # dictionary pointer

: align ( n -- ) 7 + 7 inv and ;
: here:align! here align here! ;

: ,  ( v -- ) here  ! here cell + here! ;
: b, ( v -- ) here b! here 1+     here! ;
: w, ( v -- ) here w! here 4 +    here! ;

: allot ( bytes -- adr ) here tuck + here! ;

: s, ( s -- )
    here >r
    [ dup b@ dup b,
      0 [ drop STOP ] ;case
      drop 1+ GO
    ] while
    r>
;


# ----- Word Header -----
# 4 cells
# | next
# | name
# | flags
# | code

lexicon: [forth]

LEXI [forth] REFER [forth] EDIT


# ----- Lexicon -----
# 2 cells
# | name
# | last


[forth] EDIT

    var: lcur           # current editting lexicon
    256 buf: lstack  # lexicon stack
    lstack var> lp      # lexicon stack pointer )

    : lexi:name  @ ;
    : lexi:name! ! ;
    : lexi:last  cell + @ ;
    : lexi:last! cell + ! ;

    : last  lcur lexi:last  ;
    : last! lcur lexi:last! ;

[root] EDIT

    &core as: [core]
    &root as: [root]

    : ALSO ( lexi -- ) lp ! lp cell + lp! ;
    : EDIT ( lexi -- ) lcur! ;
    : LEXI ( -- 0 ) 0 ;
    : ORDER ( 0 lexi .. -- )
        lstack lp!
        [ 0 [ STOP ] ;case  ALSO ] while
    ;
    : REFER [core] [root] ORDER ;
    : CONTEXT ( -- 0 lexi .. )
        0
        lstack [
            lp over < [ drop STOP ] ;when
            dup @ swap cell + GO
        ] while
    ;
    : CORE
        lstack lp!
        [root] lp ! lp cell + lp!
        [core] lp ! lp cell + lp!
        [core] lcur!
    ;

[core] EDIT

    : lexi:find ( q -- )  # q: lexi -- ?
        lp cell - [
            lstack over > [ 2drop no STOP ] ;when
            2dup >r >r
            @ swap call IF rdrop rdrop yes STOP RET THEN
            r> r> cell - GO
        ] while
    ;

    : lexi:each ( q -- )  # q: lexi --
        lp cell - [
            lstack over > [ 2drop no STOP ] ;when
            2dup >r >r
            @ swap call
            r> r> cell - GO
        ] while
    ;

# ----- Word -----

[forth] EDIT

    0x01 as: flag:immed
    0x02 as: flag:hidden

[core] EDIT

    : word:next   ( w -- adr ) @ ;
    : word:next!  ( adr w -- ) ! ;
    : word:name   ( w -- adr ) cell + @ ;
    : word:name!  ( adr w -- ) cell + ! ;
    : word:flags  ( w -- f   ) 2 cells + @ ;
    : word:flags! ( f w --   ) 2 cells + ! ;
    : word:cfa    ( w -- adr ) 3 cells + @ ;
    : word:cfa!   ( adr w -- ) 3 cells + ! ;

[forth] EDIT    

    : word:flag-on!  ( w flag -- )     over word:flags or  swap word:flags! ;
    : word:flag-off! ( w flag -- ) inv over word:flags and swap word:flags! ;
    
    : word:immed! ( w -- ) flag:immed  word:flag-on!  ;
    : word:hide!  ( w -- ) flag:hidden word:flag-on!  ;
    : word:show!  ( w -- ) flag:hidden word:flag-off! ;
    
    : word:immed?  ( w -- ? ) word:flags flag:immed  and ;
    : word:hidden? ( w -- ? ) word:flags flag:hidden and ;

[core] EDIT

    : <IMMED> ( -- ) <IMMED> last word:immed! ;
    
    : word:header, ( name -- )
        here:align! s,
        here:align! 4 cells allot
        last over word:next!  last!
             last word:name!
        0    last word:flags!
        here last word:cfa!
    ;
    
    : call, ( adr -- )
        here 5 + ( to from ) - ( diff )
        0xE8 b, w,
    ;
    
    : ret,  0xC3 b, ;
    : lit, ' LIT call, ;
    
    : RET <IMMED> ret, ;

[forth] EDIT

    : const ( n -- ) mode [ lit, , ] when ;
    
    : word:create ( name -- ) word:header, ;
    
    : word:handle ( word -- )
        [ word:cfa ] [ word:immed? ] biq
        ( immediate ) [ call ] ;when
        mode [ call, ] [ call ] if
    ;

[core] EDIT

    : : ( -- q )
       read-token word:create
       last word:hide!
       yes mode!
       [ ret,  no mode!  last word:show! ]
    ;
    
    : ; ( q -- ) >r ; <IMMED>
    
    : handle-num ( n -- )
        mode [ lit, , ] ;when
    ;

    : word:find-in ( name lexi -- word yes | name no )
        lexi:last [ ( name word )
            0 [ no STOP ] ;case
            dup word:hidden? [ word:next GO ] ;when
            2dup word:name s= [ nip yes STOP ] ;when
            word:next GO
        ] while
    ;

    : word:find ( name -- word yes | name no )
        [ ( name lexi ) word:find-in ] lexi:find
    ;

[forth] EDIT

    : tk>hex ( s -- n yes | no )
        dup b@  CHAR: 0 = [ drop no ] ;unless 1+
        dup b@  CHAR: x = [ drop no ] ;unless 1+
        s>hex
    ;
    
    : tk>bin ( s -- n yes | no )
        dup b@  CHAR: 0 = [ drop no ] ;unless 1+
        dup b@  CHAR: b = [ drop no ] ;unless 1+
        s>bin
    ;

[core] EDIT

    : word:eval ( token -- ... ok | name ng )
        word:find [ word:handle yes ] ;when  # found
        dup s>dec  [ nip handle-num yes ] ;when
        dup tk>hex [ nip handle-num yes ] ;when
        dup tk>bin [ nip handle-num yes ] ;when
        no
    ;



# ===================
# ===== Testing =====
# ===================
LEXI REFER [core] EDIT

: die 1 sys:exit ;
: panic ( s -- ) prn die ;
: assert ( ? s -- )
    swap IF drop RET THEN "Assertion failed: " pr panic
;

: (test-rdrop) rdrop RET ;
: test-rdrop (test-rdrop) ;

: test-while
    0 [ dup 5 = IF STOP RET THEN 1+ GO ] while
    5 = "while" assert
;

: test-2dup
    2 1 2dup
    1 = "2dup a" assert
    2 = "2dup b" assert
    1 = "2dup c" assert
    2 = "2dup d" assert
;

: test-s=
    "foo" "foo" s=     "s= 1" assert
    "foo" "fo"  s= not "s= 2" assert
    "fo"  "foo" s= not "s= 3" assert
    "foo" "bar" s= not "s= 4" assert
;

: foo ok  ;
: foo foo ;

: test-hyperstatic
    foo  "hyper static" assert
;

: test-all
    test-rdrop
    test-while
    test-2dup
    test-s=
    test-hyperstatic
;

: hello-s "hello!" ;
: hello hello-s prn ;

: repl
    [ read-token
      word:eval [ pr " ?" prn ] unless GO
    ] while
;



# ======================
# ===== Entrypoint =====
# ======================

LEXI [forth] REFER [core] EDIT

: start
    cbuf:init
    test-all
    CORE
    "[forth]" word:find [ word:name [forth] lexi:name! ] when
    "[root]" word:find [ word:name [root] lexi:name! ] when
    "[core]" word:find [ word:name [core] lexi:name! ] when
    repl
    bye
;


code: main
    <EntryPoint>

    setup-dsp

    ( clear TOS ) 0 TP movq:wr

    ( start inner interpreter )
    start jmp:w
;

