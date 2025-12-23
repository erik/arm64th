VARIABLE stack-effect
    1024 CELLS ALLOT
VARIABLE stack-effect-ptr
    stack-effect stack-effect-ptr !

: GUARD{ ( -- )
    DEPTH stack-effect-ptr !
    CELL stack-effect-ptr +!
;

: }GUARD ( delta -- )
    DEPTH 1-
    CELL stack-effect-ptr -!
    stack-effect-ptr @
    -
    2DUP != IF
        cr
        ." [DEPTH GUARD] unexpected stack effect." cr
        ."      change:   " . cr
        ."      expected: " . cr
        cr
    ELSE
        2DROP
    THEN
;

: 0}GUARD ( -- ) 0 }GUARD ;

: printable? ( ch -- b ) BL '~' BETWEEN ;

: dump-ascii ( addr len -- )
    0 ?DO
        DUP C@
        DUP printable? UNLESS DROP '.' THEN EMIT
        1+
    LOOP
    DROP
;

\ Print out `cnt` bytes of memory starting at `addr`
: DUMP ( addr cnt -- )
    BASE @ >R HEX
    DUP >R

    0 ?DO
        I CELL MOD 0= IF
            DUP . SPACE
        THEN

        DUP C@ 2 u.r0 SPACE 1+

        I CELL MOD CELL 1- = IF
            SPACE
            DUP CELL- CELL dump-ascii CR
        THEN
    LOOP

    R> CELL MOD ?DUP IF
        SPACE
        CELL OVER - 3 * SPACES
        dump-ascii CR
    THEN

    R> BASE !
    CR
;


: find-word ( addr - addr-word? )
    LATEST @
    BEGIN
        ?DUP
    WHILE
        2DUP >cfa = IF
            NIP
            EXIT
        THEN

        word>prev
    REPEAT
    DROP

    0
;

\ Try to find the word which "contains" addr (as in, `addr` is between the CFA
\ and end of word marker)
\
\ Won't help with native words yet.
: find-containing-word ( addr -- addr? )
    LATEST @
    BEGIN
        ?DUP
    WHILE
        2DUP >cfa CELL-
        DUP BEGIN CELL+ DUP @ $fafafafa = UNTIL
        WITHIN IF NIP EXIT THEN
        word>prev
    REPEAT

    DROP
    0
;

\ Where am I and what's going on?
: ~~ ( -- )
    CR
    ." ~~{ " CR
    3 SPACES print-location CR
    3 SPACES ." HERE: " HERE @ hex. CR
    3 SPACES ." <s: " DEPTH 0 u.r '>' EMIT SPACE
    DEPTH IF
        DEPTH 5 MIN 0 DO
            SP0 CELL- I CELLS - @ .
        LOOP
        ." <- top"
    THEN
    CR 3 SPACES ." SOURCE: " SOURCE TELL
    ." }~~ " CR
;

\ Primitive return stack backtrace functionality. Tries to map return stack
\ entries to words.
\
\ First addresses printed are last words in.
: .bt ( -- )
    rdepth
    ." <backtrace: " 0 u.r ." > " cr
    rp0 rp@ ( beg end )
    begin
        2dup >=
    while
        dup @
        dup h.
        find-containing-word ?DUP IF
            word>name ?DUP unless DROP s" :NONAME" THEN tell space
        ELSE
        ." (unk) " THEN
        cr
        cell+
    repeat 2drop
    cr
;

\ Shorthand to print the value at an address
: ? ( addr -- ) @ . ;

: see-prefix ( addr -- )
    4 SPACES
    '(' EMIT SPACE hex. ')' EMIT SPACE
;

: SEE ( -- )
    BL WORD
    2DUP FIND DUP UNLESS
        DROP
        ." No such word: " TELL cr
        EXIT
    THEN

    -ROT
    ." : " TELL

    DUP word>flags immediate-bit AND IF ."  IMMEDIATE " THEN
    CR

    ."     \ addr  " DUP hex. CR
    ."     \ prev  '" DUP word>prev word>name TELL
           ." ' [ " DUP @ hex. ']' EMIT CR
    CR

    >CFA
    DUP @ DOCOL = UNLESS
        ."   ( assembled word )" cr
        ';' EMIT CR
        DROP
        EXIT
    THEN

    BEGIN
        CELL+
        DUP @ $fafafafa
    != WHILE
        DUP see-prefix
        DUP @
        DUP find-word ?DUP IF
            word>name TELL
        ELSE
            ." ( unknown ) " DUP hex.
        THEN

        CR

        CASE
            ['] LIT OF
                CELL+
                DUP see-prefix
                3 SPACES '[' EMIT SPACE
                DUP @
                DUP printable? IF
                    DUP . ." ( '" EMIT ." ' )"
                ELSE
                    .
                THEN
                ." ] LITERAL"
                CR
            ENDOF

            ['] LITSTRING OF
                CELL+
                DUP see-prefix
                3 SPACES ." ( len ) "
                DUP @
                DUP >R . CR
                CELL+
                DUP see-prefix
                3 SPACES ." ( str ) '"
                DUP R@ TELL ." '" CR
                R> + ALIGNED
                CELL-
            ENDOF

            ['] 0BRANCH OF
                CELL+
                DUP see-prefix
                3 SPACES '[' EMIT SPACE
                DUP @ DUP . >R
                ." ] LITERAL    \ jump addr "
                DUP R> + hex. CR
            ENDOF

            ['] BRANCH OF
                CELL+
                DUP see-prefix
                3 SPACES '[' EMIT SPACE
                DUP @ DUP . >R
                ." ] LITERAL    \ jump addr "
                DUP R> + hex. CR
            ENDOF

            ( else )
        ENDCASE
    REPEAT
    DROP

    ';' EMIT CR
;
