yes var> DEBUG-MODE
: #DEBUG <IMMED> DEBUG-MODE [ POSTPONE: # ] ;unless ;

: defer ( q -- ) r> swap >r >r ;

: align4096 ( n -- aligned ) 4095 + 4095 inv and ;
: kilo 1024 * ;
: mega kilo kilo ;

: ;scase ( s1 s2 q -- RET | s1 )
    >r over s= IF drop r> rdrop >r RET THEN rdrop
;



# ===== Setup Lexicons =====

LEXI REFER [root] EDIT
lexicon: [asm]
lexicon: [meta]
lexicon: [cross:root]
lexicon: [cross:core]

: into:meta LEXI [meta] [cross:core] [root] ORDER [cross:core] EDIT ;
: into:code LEXI [asm] [meta] [cross:core] [root] ORDER [cross:core] EDIT ;



LEXI REFER [core] EDIT

# ===== Const & Config =====

4096 10 * as: MaxFileSize
4096 as: DataStackSize
10 mega as: DictionarySize

8 as: xcell  ( 64bit = 8byte )
: xcell+ xcell + ;
: xcells xcell * ;
: qalign 7 + 7 inv and ;



# ===== Elf Header Const & Config =====

0x01 as: Elf32
0x02 as: Elf64
Elf64 as: Class

0x01 as: LittleEndian
0x02 as: BigEndian
LittleEndian as: Endian

0x00 as: Version:None
0x01 as: Version:Current
Version:Current as: Version

0x00 as: ABI:System/V
ABI:System/V as: ABI

0x00 as: ABIVersion

0x01 as: Type:REL
0x02 as: Type:EXE
0x03 as: Type:DYN
0x04 as: Type:CORE
Type:EXE as: Type


03 as: EM:386
62 as: EM:X86-64
EM:X86-64 as: Machine

0x04 as: Perm:Read
0x02 as: Perm:Write
0x01 as: Perm:Exec



# ===== Size =====

1 as: Byte
2 as: Half
4 as: Word
8 as: Off
8 as: Adr
8 as: XWord

: half@ [ b@ ] [ inc b@ ] biq 8 << or ;
: half! 2dup b! [ 8 >> 0xFF and ] dip inc b! ;
: xword@ ( -- lo hi ) [ @ ] [ cell + @ ] biq ;
: xword! ( lo hi adr -- ) tuck cell + ! ! ;

: >word ( a b c d -- word )
    24 << >r
    16 << >r
    8 << or
    r> or
    r> or
;

: get ( adr width -- v | lo hi )
  1 [ b@     ] ;case
  2 [ half@  ] ;case
  4 [ @      ] ;case
  8 [ xword@ ] ;case
  .. " : unknown width" panic
;

: set ( v adr width | lo hi adr width -- )
  1 [ b!     ] ;case
  2 [ half!  ] ;case
  4 [ !      ] ;case
  8 [ xword! ] ;case
  .. " : unknown width" panic
;



# ===== Target Area =====

var: target-area
var: tp ( target area pointer )

: tp:align! tp qalign tp! ;
: target:init MaxFileSize allot target-area! 0 tp! ;

: t>r target-area + ;
: r>t target-area - ;

: t@  t>r @ ;
: t!  t>r ! ;
: tb@ t>r b@ ;
: tb! t>r b! ;

: t,  tp t!  tp cell + tp! ;
: tb, tp tb! tp 1    + tp! ;

: t:allot ( bytes -- tadr ) tp dup >r + tp! r> ;

: t:copy ( src size t -- ) t>r swap memcopy ;
: t:put  ( src size --   ) dup >r tp t:copy r> tp + tp! ;

: t:size tp ;

: s>t ( s -- sadr ) tp >r dup s:len t:put 0 tb, r> ;



# ----- Struct accessor -----

COVER

    var: latest
    var: width

    "01234567890123456789012345678901" as: buf

    : close ( -- &back offset ) swap ! ;

    : name   latest forth:name 1 + ;
    : copy   name buf s:copy ;
    : offset latest forth:code cell + @ ;
    : getter copy buf ;
    : setter getter dup "!" s:append! ;

    : mkgetter getter forth:create LIT, offset , LIT, width , JMP, [ >r + r> get ] , ;
    : mksetter setter forth:create LIT, offset , LIT, width , JMP, [ >r + r> set ] , ;

SHOW

    : member: ( offset q width -- offset+width q )
        dup width!
        field: forth:latest latest!
        mkgetter mksetter
    ;

END



# ===== Structs =====


STRUCT: %EHeader
    ( ident )
    Word  member: &e-magic
    Byte  member: &e-class
    Byte  member: &e-data
    Byte  member: &e-version
    Byte  member: &e-abi
    XWord member: &e-pad

    Half  member: &e-type
    Half  member: &e-machine
    Word  member: &e-eversion
    Adr   member: &e-entry
    Off   member: &e-phoff
    Off   member: &e-shoff
    Word  member: &e-flags
    Half  member: &e-ehsize
    Half  member: &e-phentsize
    Half  member: &e-phnum
    Half  member: &e-shentsize
    Half  member: &e-shnum
    Half  member: &e-shstrndx
END


STRUCT: %PHeader
    Word  member: &p-type
    Word  member: &p-flags
    Off   member: &p-offset
    Adr   member: &p-vadr
    Adr   member: &p-padr
    XWord member: &p-filesize
    XWord member: &p-memsize
    XWord member: &p-align
END


STRUCT: %SHeader
    Word  member: &s-name
    Word  member: &s-type
    XWord member: &s-flags
    Adr   member: &s-adr
    Off   member: &s-offset
    XWord member: &s-size
    Word  member: &s-link
    Word  member: &s-info
    XWord member: &s-adralign
    XWord member: &s-entsize
END



# ===== Layout =====

# | Elf Header
# | Program Header: main code
# | Program Header: bss
# | Section Header: .text
# | Section Header: .bss
# | main code
# | bss
# |     return stack 4096bytes
# |     dictionary 10Mib

( header addresses )
var: t:eheader
var: t:pheader-code
var: t:pheader-bss
var: t:sheader-code
var: t:sheader-bss

0x400000 var> LoadAdr
: v>r ( &v -- &r ) LoadAdr - t>r ;

LoadAdr MaxFileSize + align4096 var> BssAdr

var: BssPtr ( current )

BssAdr                          as: DSAdr
DSAdr DataStackSize +           as: DSBottom
DSBottom                        as: DictAdr
DictAdr DictionarySize + qalign BssPtr!

: bss:allot ( bytes -- adr ) BssPtr qalign dup >r + BssPtr! r> ;
: bss-size BssPtr BssAdr - align4096 ;


var: ProgAdr
var: CodeAdr

: code-size tp CodeAdr - ;  # main code should be at tail of ELF

: EntryAdr ProgAdr LoadAdr + ;
: CurAdr tp LoadAdr + ;

: FileSize tp ;
: MemSize FileSize ;

0 var> NProgHeader
0 var> NSecHeader

: sec-offset ( -- bytes ) %EHeader %PHeader NProgHeader * + ;



# ===== Header Temporary Variable =====

var: thdr # target header
: hdr thdr t>r ;



# ===== ELF Header =====

0x7f 0x45 0x4c 0x46 >word as: Magic

: allot-eheader ( -- tadr ) %EHeader t:allot ;

: build-eheader ( tadr -- ) thdr!
    Magic        hdr e-magic!
    Class        hdr e-class!
    Endian       hdr e-data!
    Version      hdr e-version!
    ABI          hdr e-abi!
    0 0          hdr e-pad!
    Type         hdr e-type!
    Machine      hdr e-machine!
    Version      hdr e-eversion!
    EntryAdr 0   hdr e-entry!
    %EHeader 0   hdr e-phoff!
    sec-offset 0 hdr e-shoff!
    0            hdr e-flags!
    %EHeader     hdr e-ehsize!
    %PHeader     hdr e-phentsize!
    NProgHeader  hdr e-phnum!
    %SHeader     hdr e-shentsize!
    NSecHeader   hdr e-shnum!
    0            hdr e-shstrndx!    
;



# ===== Program Header =====

: allot-pheader ( -- tadr )
    %PHeader t:allot
    NProgHeader inc NProgHeader!
;

: build-pheader-code ( tadr -- ) thdr!
    0x01        hdr p-type!
    LoadAdr  0  hdr p-vadr!
    LoadAdr  0  hdr p-padr!
    0 0         hdr p-offset!
    FileSize 0  hdr p-filesize!
    MemSize  0  hdr p-memsize!    
    4096 0      hdr p-align!
    Perm:Read Perm:Write or Perm:Exec or hdr p-flags!
;

: build-pheader-bss ( tadr -- ) thdr!
    0x01       hdr p-type!
    BssAdr 0   hdr p-vadr!
    BssAdr 0   hdr p-padr!
    0 0        hdr p-offset!
    0 0        hdr p-filesize!
    4096 0     hdr p-align!
    bss-size 0 hdr p-memsize!
    Perm:Read Perm:Write or Perm:Exec or hdr p-flags!
;



# ===== Section Headers =====

: allot-sheader ( -- tadr )
    %SHeader t:allot
    NSecHeader inc NSecHeader!
;


( section types )
0x01 as: SHT_PROGBITS
0x08 as: SHT_NOBITS

( section flags )
0x01 as: SHF_WRITE
0x02 as: SHF_ALLOC
0x04 as: SHF_EXECINSTR

: build-sheader-code ( tadr -- ) thdr!
    SHT_PROGBITS hdr s-type!
    SHF_ALLOC SHF_EXECINSTR or 0 hdr s-flags!

    0   hdr s-name!
    0   hdr s-link!
    0   hdr s-info!
    4 0 hdr s-adralign!
    0 0 hdr s-entsize!

    EntryAdr  0 hdr s-adr!
    CodeAdr   0 hdr s-offset!
    code-size 0 hdr s-size!
;

: bss-sec-flags SHF_ALLOC SHF_WRITE or SHF_EXECINSTR or ;
: bss-sec-offset BssAdr LoadAdr - ;

: build-sheader-bss ( tadr -- ) thdr!
    SHT_NOBITS      hdr s-type!
    bss-sec-flags 0 hdr s-flags!

    0   hdr s-name!
    0   hdr s-link!
    0   hdr s-info!
    4 0 hdr s-adralign!
    0 0 hdr s-entsize!

    BssAdr 0         hdr s-adr!
    bss-sec-offset 0 hdr s-offset!
    bss-size 0       hdr s-size!
;



# ===== Setup Header =====


: allot-headers
    allot-eheader t:eheader!
    allot-pheader t:pheader-code!
    allot-pheader t:pheader-bss!
    allot-sheader t:sheader-code!
    allot-sheader t:sheader-bss!
;

: build-headers
    t:eheader build-eheader
    t:pheader-code build-pheader-code
    t:pheader-bss  build-pheader-bss
    t:sheader-code build-sheader-code
    t:sheader-bss  build-sheader-bss
;



# ====================
# ===== Assembly =====
# ====================

# ----- convention -----
# movq:wr
# movq:mro
# w: 32bit immediate
# b: 8bit
# h: 16bit
# l: 32bit
# q: 64bit
# r: register
# m: memory in register
# o: +offset

TEMPORARY LEXI [asm] REFER [asm] EDIT
COVER


    SHOW
    : <EntryPoint> tp ProgAdr! ;
    : t>adr LoadAdr + ;
    : tp-adr tp t>adr ;


    SHOW # ----- Registers -----

    0x00 as: rax
    0x01 as: rcx
    0x02 as: rdx
    0x03 as: rbx
    0x04 as: rsp
    0x05 as: rbp
    0x06 as: rsi
    0x07 as: rdi

    rbp as: SP  # Data stack Pointer
    rsp as: RP  # Return stack Pointer
    rsi as: IP  # Instruction Pointer
    rbx as: TP  # Top of data stack Pointer

    HIDE # ----- mrm: mod r/m -----

    : mrm:mem 0b 00000000 or ; ( no-op )
    : mrm:d8  0b 01000000 or ;
    : mrm:reg 0b 11000000 or ;

    : mrm:op/1 ( m -- m ) 0b 00001000 or ;
    : mrm:op/2 ( m -- m ) 0b 00010000 or ;
    : mrm:op/3 ( m -- m ) 0b 00011000 or ;
    : mrm:op/4 ( m -- m ) 0b 00100000 or ;
    : mrm:op/5 ( m -- m ) 0b 00101000 or ;
    : mrm:op/6 ( m -- m ) 0b 00110000 or ;
    : mrm:op/7 ( m -- m ) 0b 00111000 or ;

    : mrm:RM ( src dst -- m )
        # src:reg dst:r/m
        3 << or
    ;

    : mrm:MR ( src dst -- m )
        # src:r/m dst:reg
        [ 3 << ] dip or
    ;


    HIDE # ----- Emit utils -----

    : prefix64 0x48 tb, ;

    var: sib
    : sib,
        [ 0 sib! ] defer
        sib
        0   [ ( no-op ) ] ;case
        rsp [ 0x24 tb,   ] ;case
        drop
    ;

    : >sib ( reg -- ) sib! ;

    : >rel8 ( byte -- rel8 )
        dup  127 > [ .. "rel8 too big"   panic ] ;when
        dup -128 < [ .. "rel8 too small" panic ] ;when
        0xFF and
    ;

    : here>rel8 ( tadr -- )
        tp-adr - >rel8
    ;


    SHOW # ----- Mov -----

    : movq:wr ( w dst -- ) # w:32bit
        prefix64  0xB8 or tb,  t, 0 t,
    ;

    : movq:rr ( src dst -- )
        prefix64 0x89 tb,  mrm:MR mrm:reg tb,
    ;

    : movq:rm ( src dst -- )
        dup rbp = [ "use movq:rmo for rbp(SP)" panic ] ;when    
        dup >sib
        prefix64 0x89 tb,  mrm:MR mrm:mem tb,  sib,
    ;

    : movq:mr ( src dst -- )
        over rbp = [ "use movq:mro for rbp(SP)" panic ] ;when
        over >sib
        prefix64 0x8b tb,  mrm:RM mrm:mem tb,  sib,
    ;

    : movq:rmo ( src dst offset -- )
        >r
        dup >sib
        prefix64 0x89 tb,  mrm:MR mrm:d8 tb,  sib,
        r> tb,
    ;

    : movq:mro ( src dst offset -- )
        >r
        over >sib
        prefix64 0x8b tb,  mrm:RM mrm:d8 tb,  sib,
        r> tb,
    ;

    : movzbq ( src dst -- )  # move zero extended byte to quad
        prefix64 0x0F tb, 0xB6 tb, mrm:RM mrm:reg tb,
    ;

    : movb:mr ( src dst -- )
        over >sib  0x8A tb,  mrm:RM mrm:mem tb,  sib,
    ;

    : movb:rm ( src dst -- )
        dup >sib  0x88 tb,  mrm:MR mrm:mem tb,  sib,
    ;

    : movw:mro ( src dst offset -- )
        >r
        over >sib  0x8B tb, mrm:RM mrm:d8 tb, sib,
        r> tb,
    ;

    : movw:rmo ( src dst offset -- )
        >r
        dup >sib  0x89 tb,  mrm:MR mrm:d8 tb, sib,
        r> tb,
    ;


    SHOW # ----- Logical -----

    : xorq ( src dst -- )
        prefix64  0x31 tb,  mrm:MR mrm:reg tb,
    ;

    : invq ( dst -- )
        prefix64  0xF7 tb,  mrm:op/2 mrm:reg tb,
    ;

    : andq ( src dst -- )
        prefix64  0x21 tb,  mrm:MR mrm:reg tb,
    ;

    : orq ( src dst -- )
        prefix64  0x09 tb,  mrm:MR mrm:reg tb,
    ;


    SHOW # ----- Call -----

    : call:w ( tadr -- )
        tp-adr 5 + ( to from ) - ( diff )
        0xE8 tb, t,
    ;

    : retq
        0xC3 tb,
    ;


    SHOW # ----- Jmp -----

    : jmpq ( r -- )
        # FF/4 near, direct, absolute
        # example: rax jmpq
        0xFF tb,  mrm:reg mrm:op/4 tb,
    ;

    : jmpq:d ( r -- )
        # FF/4 near, indirect, absolute
        0xFF tb,  mrm:mem mrm:op/4 tb,
    ;

    : jz:8 ( rel8 -- )
        0x74 tb, tb,
    ;

    : jz:prep ( -- &v:patch )
        0x74 tb,  tp-adr  0 tb,
    ;

    : jz:patch ( &v:patch &v:dst -- )
        over 1 + - ( diff ) >rel8
        swap v>r b!
    ;

    : jz:adr ( tadr -- )
        here>rel8  # before jz:8 )
        2 -        # jz:8 jumps from address after itself.
        jz:8
    ;

    : jmp:w ( &v:dst -- )
        tp-adr 5 + ( to from ) - ( diff )
        0xE9 tb, t,    
    ;

    : jmp:prep ( -- &v:patch )
        0xE9 tb,  tp-adr  0 t,
    ;

    : jmp:patch ( &v:patch &v:dst -- )
        over  4 + ( after jmp )  - ( diff )
        swap v>r !
    ;


    SHOW # ----- Test ----

    : testq:rr ( r r -- )
        prefix64  0x85 tb,  mrm:MR mrm:reg tb,
    ;


    SHOW # ----- Cmp -----

    : cmpq:rr ( r r -- )
        prefix64  0x39 tb,  mrm:MR mrm:reg tb,
    ;


    SHOW # ----- SetCC -----

    : sete ( r -- )
        0x0F tb, 0x94 tb, mrm:reg tb,
    ;

    : setg ( r -- )
        0x0F tb, 0x9F tb, mrm:reg tb,
    ;

    : setge ( r -- )
        prefix64  0x0F tb,  0x9D tb,  mrm:reg tb,
    ;


    SHOW # ----- Lodsq -----

    : lodsq 0x48 tb, 0xAD tb, ;


    SHOW # ----- rep -----

    : rep-stosb
         # fill rcx bytes at rdi with al
         0xF3 tb, prefix64 0xAA tb,
    ;


    SHOW # ----- Lea -----

    : leaq ( n src dst-- )
        dup >sib  prefix64  0x8D tb,  mrm:RM mrm:d8 tb,  tb,  sib,
    ;


    SHOW # ----- arithmetics -----

    : addq:rr ( src dst -- )
        prefix64  0x01 tb,  mrm:MR mrm:reg tb,
    ;

    : addq:rm ( src dst -- )
        dup >sib  prefix64  0x01 tb,  mrm:MR mrm:mem tb,  sib,
    ;

    : subq:rr ( src dst -- )
        prefix64  0x29 tb,  mrm:MR mrm:reg tb,
    ;

    : incq:r ( dst -- )
        prefix64  0xFF tb,  mrm:reg tb,
    ;

    : decq:r ( dst -- )
        prefix64  0xFF tb,  mrm:reg mrm:op/1 tb,
    ;

    : imulq:rr ( src dst -- )
        prefix64  0x0F tb,  0xAF tb,  mrm:RM mrm:reg tb,
    ;

    : idivq:r ( dst -- )
        # rdx:rax by dst
        prefix64  0xF7 tb,  mrm:reg mrm:op/7 tb,
    ;


    SHOW # ----- Push/Pop -----

    : push ( r -- ) 0x50 + tb, ;

    : pop ( r -- ) 0x58 + tb, ;


    SHOW # ----- dq -----

    : dq:q ( w w -- ) swap t, t, ;
    
    : dq:w ( w -- )  # signed 32 -> signed 64
        dup 0 < dq:q
    ;

    : dq:uw ( w -- )  # unsigned 32 -> unsigned 64
        0 dq:q
    ;


    SHOW # ----- Syscall -----

    : syscall 0x0f tb, 0x05 tb, ;


END ( COVER ) END ( TEMPORARY )



# =========================
# ===== Meta Compiler =====
# =========================

# ----- Forth -----
# Direct Threaded

# ------ Registers -----
# rsp: data stack pointer
# rbp: return stack pointer
# rsi: instruction pointer
# rbx: top of stack pointer

# ----- Layout -----
# | code
# | return stack (grows toward low address)
# | dictionary



TEMPORARY LEXI [asm] REFER [meta] EDIT

    [core] EDIT
    lexicon: [meta:aux]
    [meta:aux] ALSO


    # ----- Word address -----
    [meta:aux] EDIT
    
    var: AdrLIT
    var: AdrRET
    var: AdrJMP
    var: AdrJZ
    var: AdrFetch
    var: AdrStore
    var: AdrLast
    var: AdrHere
    var: AdrConst
    var: AdrALSO
    var: AdrEDIT
    var: AdrLEXI
    var: AdrREFER

    : check-ref ( adr s -- adr ) over [ drop ] [ "not defined" epr panic ] if ;
    : adr-LIT   AdrLIT   "LIT"     check-ref ;
    : adr-RET   AdrRET   "RET"     check-ref ;
    : adr-JMP   AdrJMP   "JMP"     check-ref ;
    : adr-JZ    AdrJZ    "JZ"      check-ref ;
    : adr-Fetch AdrFetch "@"       check-ref ;
    : adr-Store AdrStore "!"       check-ref ;
    : adr-Here  AdrHere  "here"    check-ref ;
    : adr-Const AdrConst "const"   check-ref ;
    : adr-ALSO  AdrALSO  "ALSO"    check-ref ;
    : adr-EDIT  AdrEDIT  "EDIT"    check-ref ;
    : adr-LEXI  AdrLEXI  "LEXI"    check-ref ;
    : adr-REFER AdrREFER "REFER"   check-ref ;

    : meta:register-prim ( tadr name -- tadr name )
        "LIT"   [ dup AdrLIT!   "LIT"   ] ;scase
        "RET"   [ dup AdrRET!   "RET"   ] ;scase
        "JMP"   [ dup AdrJMP!   "JMP"   ] ;scase
        "JZ"    [ dup AdrJZ!    "JZ"    ] ;scase
        "@"     [ dup AdrFetch! "@"     ] ;scase
        "!"     [ dup AdrStore! "!"     ] ;scase
        "here"  [ dup AdrHere!  "here"  ] ;scase
        "const" [ dup AdrConst! "const" ] ;scase
        "ALSO"  [ dup AdrALSO!  "ALSO"  ] ;scase
        "EDIT"  [ dup AdrEDIT!  "EDIT"  ] ;scase
        "LEXI"  [ dup AdrLEXI!  "LEXI"  ] ;scase
        "REFER" [ dup AdrREFER! "REFER" ] ;scase
    ;
    

    # ----- Macros -----
    [asm] EDIT
    
    : push-ds ( r -- )
        xcell neg SP SP leaq    
        SP 0 movq:rmo
    ;
    
    : pop-ds ( r -- )
        SP swap 0 movq:mro
        xcell SP SP leaq
    ;

    : save-regs    SP push RP push IP push TP push ;
    : restore-regs TP pop  IP pop  RP pop  SP pop  ;


    # ----- meta header -----
    [meta:aux] EDIT

    STRUCT: %mheader
         Word member: &mh-next     # radr: next entry
         Word member: &mh-word     # radr: meta word
         Word member: &mh-flags    # word: flags
         Word member: &mh-builder  # radr: dictbuilder ( metaword -- )
         Word member: &mh-xheader  # built xheader ( radr )
         Word member: &mh-mlexi    # mlexi defined below
    END

    STRUCT: %xheader
         XWord member: &xh-next
         XWord member: &xh-name
         XWord member: &xh-flags
         XWord member: &xh-code
    END

    STRUCT: %mlexi
        Word member: &ml-next
        Word member: &ml-last  ( mheader )
        Word member: &ml-lexi  ( lexi on meta )
        Word member: &ml-xlexi ( mheader of lexi )
    END

    STRUCT: %xlexi
        XWord member: &xl-name
        XWord member: &xl-last
    END

    0 var> mlexi:last

    : mlexi:create ( -- ml )
        %mlexi allot
        mlexi:last over ml-next!
        dup mlexi:last!
        %xlexi t:allot t>adr over ml-xlexi!
    ;

    var: mlexi:root
    var: mlexi:core

    : mlexi:setup
        mlexi:create mlexi:root!
       [cross:root] mlexi:root ml-lexi!

        mlexi:create mlexi:core!
        [cross:core] mlexi:core ml-lexi!    
    ;

    mlexi:root var> mcurrent ( editting )
    : mlatest  ( -- mh ) mcurrent ml-last  ;
    : mlatest! ( mh -- ) mcurrent ml-last! ;

    : mlexi:each ( q -- )  # q: mlexi --
        mlexi:last [
            0 [ drop STOP ] ;case
            2dup ml-next >r >r
            swap call
            r> r> GO
        ] while
    ;

    : mh-xcode ( mh -- vadr ) mh-xheader xh-code ;

    COVER
        var: mh
        var: word
        var: flags
        var: name
        var: thdr
        var: cfa
        : hdr thdr LoadAdr - t>r ;
        : mlexi mh mh-mlexi ;
        : xlexi mlexi ml-xlexi ;
        : xlatest  ( -- w )   xlexi v>r xl-last drop ;
        : xlatest! ( w -- ) 0 xlexi v>r xl-last! ;
    SHOW
        : build-xnormal ( mh -- ) mh!
            mh mh-word word!
            mh mh-flags flags!
            ( put xh-name )
            tp:align!  word forth:name  s>t t>adr name!
            ( allot xheader )
            tp:align! %xheader t:allot t>adr thdr!
            ( next  ) xlatest 0 hdr xh-next!  thdr xlatest!
            ( name  ) name  0 hdr xh-name!
            ( flags ) flags 0 hdr xh-flags!
            ( code  ) word forth:code cell + @ dup cfa! 0 hdr xh-code!
            ( save  ) thdr v>r mh mh-xheader!
        ;

        : build-xconst ( mh -- )
            build-xnormal
            tp:align! tp-adr >r
            adr-LIT call:w  cfa dq:w  adr-Const jmp:w
            r> 0 hdr xh-code!
        ;
    END

    COVER
        0x01 as: fimmed
        0x02 as: fhidden
        : set-flag ( f -- ) mlatest mh-flags or  mlatest mh-flags! ;
    SHOW
        : meta:immed-last fimmed  set-flag ;
        : meta:hide-last  fhidden set-flag ;
    END

    : var-adr ( tadr -- radr )
        5 + ( call LIT )  LoadAdr -  t@  LoadAdr - t>r
    ;

    : patch-var! ( v tadr -- )
        var-adr !
    ;

    : meta:patch-vars
        DictAdr adr-Here patch-var!
    ;

    var: mhdr
    : create-mheader ( -- )
        %mheader allot mhdr!
        mlatest mhdr mh-next!
        mhdr mlatest!
        mcurrent mhdr mh-mlexi!
        0 mhdr mh-word! ( STUB )
        [ build-xnormal ] mhdr mh-builder!
    ;

    : meta:create-header ( name -- tadr )
        here:align! create-mheader
        tp:align! tp t>adr swap meta:register-prim
        forth:create
        forth:latest mlatest mh-word!
    ;

    COVER
        var: before
        var: next
    SHOW
        : reverse! ( xs -- xs )
            # reverse linked list
            [ before! ] [ @ ] biq
            0 before !
            [ 0 [ STOP ] ;case
              dup @ next!
              before over ! before! next GO
            ] while
            before
        ;
    END

    : meta:build-xheaders
        [
            ml-last
            0 [ ( no-op ) ] ;case
            reverse! [ ( mheader -- )
                0 [ STOP ] ;case
                dup >r
                dup mh-builder call
                r> mh-next GO
            ] while
        ] mlexi:each
    ;

    # ----- meta word creation -----

    : meta:create ( name -- )
        # create normal word
        meta:create-header
        POSTPONE: <IMMED>
        LIT, , JMP, [ forth:mode [ call:w ] when ] ,
    ;

    : meta: ( name: -- )
        forth:read [ "meta name required" panic ] ;unless meta:create
    ;

    : meta:const ( n name -- )
        meta:create-header drop POSTPONE: <IMMED>
        [ build-xconst ] mlatest mh-builder!
        LIT, , JMP, [ forth:mode [ adr-LIT call:w dq:w ] when ] ,
    ;

    TEMPORARY [forth] ALSO
        : meta:lexicon ( -- ml )
            mlexi:create 
            lexi:new over ml-lexi!
        ;

        : meta:lexiword ( ml name -- )
            meta:create-header drop POSTPONE: <IMMED>
            [ build-xconst ] mlatest mh-builder!
            dup ml-xlexi
            LIT, ( xlexi ) ,  LIT, ( mlexi ) , JMP, [
                forth:mode [ drop adr-LIT call:w dq:w ] [ nip ] if
            ] ,
        ;

        : meta:lexicon: ( name: -- )
            meta:lexicon
            forth:read [ "lexicon name required" panic ] ;unless
            meta:lexiword
        ;
    END

    var: var-name
    var: var-adr
    : meta:var ( n name -- )
        # 42 var> x
        # xadr: 42
        # in-cross:
        #   x:  LIT xadr @ RET
        #   x!: LIT xadr ! RET
        var-name!
        ( var area )
        tp:align! tp t>adr var-adr!
        dq:w
        ( getter )
        var-name meta:create
        adr-LIT call:w var-adr dq:w adr-Fetch call:w retq
        ( setter )
        var-name "!" s:append!
        var-name meta:create
        adr-LIT call:w var-adr dq:w adr-Store call:w retq
    ;

    : meta:var> ( n name: -- )
        forth:read [ "var name required" panic ] ;unless meta:var
    ;

    : meta:handle-num ( n -- )
        forth:mode [ adr-LIT call:w dq:w ] when
    ;

    : meta:parse-string
        forth:mode [ ( -- &v:str &v:patch )
          jmp:prep tp:align! tp-adr swap
        ] [ tp:align! tp-adr ] if
        forth:take drop ( skip first double quote )
        [ forth:take
            0  [ "Unclosed string" panic STOP ] ;case
            CHAR: " [ STOP ] ;case
            dup CHAR: \\ = [
                drop ' forth:take c:escaped
                [ "Escape sequence required" panic STOP ] ;unless
                tb, GO
            ] ;when
            tb, GO
        ] while
        0 tb, tp:align!
        forth:mode [ tp-adr jmp:patch adr-LIT call:w dq:w ] when 
    ;

    # ----- Lexicons -----
    [core] EDIT
    : meta:EDIT ( mlexi )
        dup mcurrent! ml-lexi EDIT
    ;

    : meta:ALSO ( mlexi )
        ml-lexi ALSO
    ;

    : meta:ORDER ( 0 mlexi ... -- )
        LEXI ORDER
        [root] ALSO
        [ 0 [ STOP ] ;case
          meta:ALSO GO
        ] while
        [meta] ALSO
    ;

    : meta:REFER ( 0 mlexi ... -- )
        mlexi:core mlexi:root meta:ORDER
    ;

    # ----- Main routines -----
    [asm] EDIT
    
    : setup-dsp
         DSBottom xcell - SP movq:wr
    ;

    # ----- debug -----
    [core] EDIT
    : bye-prim
        0x3C rax movq:wr  # exit
        0 rdi movq:wr    # exit code
        syscall
    ;

    # ----- Codeword -----
    [core] EDIT
    : start-codeword ( name: -- q )
        TEMPORARY into:code
        meta:
        ( q: END TEMPORARY )
    ;
    
    [meta] EDIT

    TEMPORARY [forth] ALSO
    : : ( name: -- q )
        meta:
        forth:latest forth:hide!
        yes forth:mode!
        [ retq
          no forth:mode!
          forth:latest forth:show!
        ]
    ;
    END

    : BYEPRIM bye-prim ;

    : ; ( q -- ) <IMMED> >r ;

    : code: ( name: -- q ) start-codeword ;
    
    : prim: ( name: -- q )
        start-codeword [ retq >r ( END TEMPORARY )]
    ;

    : as: ( n name: -- )
        forth:read [ "const name required" panic ] ;unless
        meta:const
    ;

    : var> ( n name: -- ) meta:var> ;
    : var: ( name: -- ) 0 meta:var> ;

    : buf: ( bytes name: -- )
        bss:allot forth:read [ "buf name required" panic ] ;unless
        meta:const
    ;

    : IF <IMMED> ( -- &v:patch q )
        TP rax movq:rr
        TP pop-ds
        rax rax testq:rr
        jz:prep
        [ tp-adr jz:patch ]
    ;

    : THEN <IMMED> ( &v:patch q -- )
        >r
    ;

    : ELSE <IMMED> ( &v:patch q -- &v:patch q )
        jmp:prep  ( &v:patch )  >r
        call
        r> [ tp-adr jmp:patch ]
    ;

    : RET <IMMED> retq ;

    : [ <IMMED> ( -- &quot &patch q )
        jmp:prep tp-adr swap
        [ retq
          tp-adr jmp:patch
          adr-LIT call:w dq:w
        ]
    ;

    : ] <IMMED> ( q -- ) >r ;

    : AGAIN <IMMED>
        forth:latest forth:code cell + @ ( cfa )
        jmp:w
    ;

    : ' <IMMED> ( name: -- )
        forth:read-find [ die ] ;unless
        forth:code cell + @ ( CFA )
        forth:mode [ adr-LIT call:w dq:w ] ;when
    ;

    : <IMMED> <IMMED> meta:immed-last ;

    : CHAR: <IMMED>
        forth:read [ "A character required" panic ] ;unless
        dup b@ dup CHAR: \\ = [ drop inc
            [ [ inc ] [ b@ ] biq ] c:escaped
            [ "Escape sequence required" panic ] ;unless
        ] when nip
        forth:mode [ adr-LIT call:w dq:w ] when
    ;

    : LEXI  <IMMED> forth:mode [ adr-LEXI call:w ] [ 0 ] if ;
    : EDIT  <IMMED> forth:mode [ adr-EDIT call:w ] [ meta:EDIT ] if ;
    : ALSO  <IMMED> forth:mode [ adr-ALSO call:w ] [ meta:ALSO ] if ;
    : REFER <IMMED> forth:mode [ adr-REFER call:w ] [ meta:REFER ] if ;
    : lexicon: meta:lexicon: ;
 
    : &core mlexi:core ml-xlexi ;
    : [core] <IMMED>
        mlexi:core forth:mode [ adr-LIT call:w ml-xlexi dq:w ] when
    ;

    : &root mlexi:root ml-xlexi ;
    : [root] <IMMED>
        mlexi:root forth:mode [ adr-LIT call:w ml-xlexi dq:w ] when
    ;
    
    : ?tp <IMMED> "tp " pr tp ..hex " adr " pr tp-adr .hex ;
    : ?h <IMMED> "HERE " pr ?stack ;
    : ?w ( w -- ) .hex ;

END



# ===== Save =====

TEMPORARY [file] ALSO COVER
    var: id
SHOW
    : save ( fname -- )
        "wb" file:open! id!
        target-area t:size id file:write!
        id file:close!
    ;
END ( COVER ) END ( TEMPORARY )



# ===== Main =====

TEMPORARY LEXI [meta:aux] [asm] [forth] REFER

: main
    target:init
    allot-headers

    ( Code )
    tp:align! tp CodeAdr!
    ' meta:handle-num   -> forth:handle-num
    ' meta:parse-string -> forth:parse-string
    mlexi:setup
    TEMPORARY into:meta
    "core.f" include
    call ( END of TEMPORARY )
    tp:align!

    meta:build-xheaders
    meta:patch-vars
    build-headers

    #DEBUG "LoadAdr  " pr LoadAdr  .hex
    #DEBUG "<bss>" prn
    #DEBUG "    DSAdr    " pr DSAdr    .hex
    #DEBUG "    DSBottom " pr DSBottom .hex
    #DEBUG "    BssAdr   " pr BssAdr   .hex
    #DEBUG "    BssSize  " pr bss-size .hex
    #DEBUG "<code>" prn
    #DEBUG "    FileSize " pr FileSize .hex
    #DEBUG "    CodeAdr  " pr CodeAdr .hex
    #DEBUG "    EntryAdr " pr EntryAdr .hex
    #DEBUG "    ProgAdr  " pr ProgAdr .hex
    #DEBUG "    CodeSize " pr code-size .. "bytes" prn

    "./out/elfort" save
;

END

main
