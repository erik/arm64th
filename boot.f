: \ IMMEDIATE
    [ HERE @ ] KEY 10 = 0BRANCH [ HERE @ - , ] ;

\ Now we've defined line comments! We implemented them at a very low level because
\ we don't have language constructs like loops or if statements yet.
\
\ Explaining what happened:
\
\   [ HERE @ ]       \ Push current pos to stack (at compile time)
\   KEY 10 =         \ Read next key of input and check if it's a newline (0x0A)
\   0BRANCH          \ If not, jump back ...
\   [ HERE @ - , ]   \ By however many words we've compiled since start of fn

\ Now let's build a simple version of block comments. We'll replace this
\ definition later with one that balances parens so we can nest them.
\
\ Implementation is the same as line comments, but using 41 (i.e. ')') as the
\ delimiter.
: ( IMMEDIATE
    [ HERE @ ] KEY 41 = 0BRANCH [ HERE @ - , ]
;

( Now it'll be easier to build more of the language in an understandable way. Onward!)

\ Pop a value off the stack and compile it into `LIT [val]`
: LITERAL IMMEDIATE ( a -- )
    ' LIT , ,
;

\ We don't have a native concept of characters yet ('\n' is simply a word called
\ "'\n'" which pushes 0x0A to the stack)
: '\n' 10 ; ( -- a )
: BL   32 ; \ space

\ Later on we want to swap out the implementations of the native words KEY and
\ WORD. We can't (easily) modify the assembly implementations, so we add a
\ layer of indirection here that's easier to swap.
\
\ NOTE: this isn't a recursive call, it compiles the reference to the OLD
\ definition of the word.
: KEY  ( -- a ) KEY ;
: WORD ( delim -- addr len ) WORD ;

\ Return first character of word after this call.
\
\ CHAR abc => a
: CHAR ( -- a ) BL WORD DROP C@ ;

\
\ Conditional logic
\

\ cond IF true-path THEN
\
\ This is adding some structure to the 0BRANCH approach we did above to define
\ line comments. We don't want to have to count the number of words to jump by
\ hand (it could change!), so we use THEN to rewrite the code compiled by IF
\ based on how far we've moved (tracked by HERE).
: IF IMMEDIATE ( cond -- cond offset )
    ' 0BRANCH ,     \ compile 0BRANCH
    HERE @          \ push our current position to the stack
    0 ,             \ compile placeholder jump offset (overwritten later by THEN)
;

\ Finalizer for IF
: THEN IMMEDIATE ( offset_addr -- )
    DUP             \ dup previous position set by IF
    HERE @          \ push current position to the stack
    SWAP -          \ how far have we moved? (curr - prev)
    SWAP !          \ *offset_addr = (curr - prev)
;

\ cond IF true-part ELSE false-part THEN
: ELSE IMMEDIATE ( if_offset_addr -- else_offset_addr )
    ' BRANCH ,      \ compile a jump to end of ELSE
    HERE @          \ push current position to stack
    0 ,             \ compile placeholder offset for BRANCH (overwritten by THEN)
    SWAP            \ move if_offset_addr to top of stack
    DUP             \ see defintion of THEN above
    HERE @
    SWAP -
    SWAP !
;

\ BEGIN loop-part cond UNTIL
\
\ Loop as long as `cond` evaluates to 0.
: BEGIN IMMEDIATE ( -- jmp_offset_addr )
    HERE @          \ push current position to stack
;

\ Finalizer for BEGIN.
: UNTIL IMMEDIATE ( jmp_offset_addr cond -- )
    ' 0BRANCH ,    \ compile conditional branch
    HERE @ -       \ how far have we moved from BEGIN?
    ,              \ compile the offset here
;

\ BEGIN loop-part AGAIN
\
\ Loop forever (or at least until EXIT is called).
: AGAIN ( jmp_addr -- ) IMMEDIATE
    ' BRANCH ,     \ branch unconditionally
    HERE @ - ,     \ to however far we've moved from BEGIN
;

\ BEGIN cond WHILE loop-part REPEAT
: WHILE IMMEDIATE ( cond -- )
    ' 0BRANCH ,   \ compile a conditional branch
    HERE @        \ push current location to stack
    0 ,           \ compile a placeholder branch offset (set by REPEAT)
;

\ BEGIN cond WHILE loop-part REPEAT
: REPEAT IMMEDIATE
    ' BRANCH ,    \ compile a conditional branch
    SWAP          \ ( begin_off while_off -- while_off begin_off )
    HERE @ - ,    \ compile how far we've moved since BEGIN
    DUP           \ ( w b -- w b b )
    HERE @        \ ( w b b -- w b b here )
    SWAP -        \ ( w b b h -- w b h b -- w b diff )
    SWAP !        \ ( w b d -- w d b ) -- write diff to begin offset
;

\
\ Basic helper utils
\

: /   ( a b -- a/b ) /MOD SWAP DROP ;
: MOD ( a b -- a%b ) /MOD DROP ;
: NEGATE ( a -- -a ) 0 SWAP - ;
: NOP ;
: CELL+ ( a -- a+CELL ) CELL + ;
: CELL- ( a -- a-CELL ) CELL - ;
: CELLS ( a -- a*CELL) CELL * ;
: CHARS ( a -- a*CHAR) ;
: TRUE  ( -- a )     -1 ;
: FALSE ( -- a )      0 ;
: NOT   ( a -- b )   FALSE = ;
: !=    ( a b -- c ) = NOT ;
: 1+    ( a -- a+1 ) 1 + ;
: 1-    ( a -- a-1 ) 1 - ;

: +! ( val addr -- )
    SWAP   ( addr val )
    OVER   ( addr val addr )
    @      ( addr val *addr )
    +      ( addr nval )
    SWAP ! ( val addr )
;
: -! ( val addr -- ) SWAP OVER @ SWAP - SWAP ! ;

\ Push to return stack from data stack.
: >R ( a -- R:a )
    RP@          \ get current rstack ptr         (a -- a &rp)
    RP@ @        \ get current return addr        (a &rp -- a &rp *rp)
    RP@ CELL-    \ get next rstack ptr            (a &rp *rp -- a &rp *rp &rp-1)
    DUP RP!      \ set rstack ptr to next
    !            \ store cur return addr in       (a &rp *rp &rp-1 -- a &rp)
                 \ next position (so it's on top)
    !            \ store `a` in *rsp              (a &rp -- )
;

\ Push to data stack from return stack
: R> ( R:a -- a )
    RP@ CELL+ @  \ get data before current return addr   ( -- a )
    RP@ @        \ get current return addr               ( a -- a *rp )
    RP@ CELL+    \ get addr below top of stack           ( a *rp -- a *rp &rp-1 )
    DUP RP!      \ "pop" rstack ptr
    !            \ move return addr to new top of stack  ( a *rp &rp-1 -- a )
;

\ Copy head of return stack to data stack
: R@ ( R: a -- a ) RP@ CELL+ @ ;
: RDROP ( R:a -- ) R> RP@ ! ;
: RPICK ( R:xn x0 n -- xn ) CELLS RP@ + CELL + @ ;

\ Simple stack effects, best described by their signature
: NIP   ( a b -- b )          SWAP DROP ;
: TUCK  ( a b -- b a b )      SWAP OVER ;
: 2DUP  ( a b -- a b a b )    OVER OVER ;
: 2DROP ( a b -- )            DROP DROP ;
: 3DROP ( a b c -- )          2DROP DROP ;
: ROT   ( a b c -- b c a )    >R SWAP R> SWAP ;
: -ROT  ( a b c -- c a b )    SWAP >R SWAP R> ;

\ Grab the n'th value from the stack and put it on top
: PICK  ( x0 ... xn i -- x0 ... xi x0 )
    CELLS SP@ CELL+ + @ ;

: 3DUP  ( a b c -- a b c a b c )
    2 PICK 2 PICK 2 PICK ;

\ DUP if top of stack is not zero
: ?DUP  ( 0 -- 0 | x -- x x )
    DUP DUP 0 = IF DROP THEN ;

\
\ Alignment
\

: aligned-by ( a b -- a' ) 1- DUP INVERT -ROT + AND ;  \ Align addr `a` to size `b`
: align-by   ( a -- ) HERE @ SWAP aligned-by HERE ! ;  \ Align HERE to size `a`
: ALIGNED    ( a -- a ) CELL aligned-by ;              \ Align addr cell size
: ALIGN      ( -- ) CELL align-by ;                    \ Align HERE to cell size

\
\ Word helpers
\

\ Given a word address, return various components of the word
: word>prev     ( addr -- addr2 ) @ ;
: word>flags    ( addr -- val ) CELL+ c@ ;
: word>name-len ( addr -- len ) CELL+ 1+ C@ ;
: word>name     ( addr -- addr len ) DUP word>name-len SWAP CELL+ 2 + SWAP ;

: immediate-bit 1 ;
: hidden-bit    2 ;

: word>immediate? ( addr -- bool ) word>flags immediate-bit AND ;
: word>hidden?    ( addr -- bool ) word>flags hidden-bit AND ;

\ Return the address of the entry point of a word. Pointer will generally
\ contain the address of `DOCOL` if this is a Forth word.
: >CFA ( addr -- addr ) word>name + ALIGNED ;

\ '[COMPILE] word' compiles a word that would otherwise be executed immediately
\
\ Conceptually similar to `' word ,` if `word` is immediate.
: [COMPILE] IMMEDIATE BL WORD FIND >CFA , ;

\ "compile time tick". Leaves execution token (addr) of word on the stack
: ['] IMMEDIATE ( -- xt ) ' LIT , ;

\ Compile-time CHAR
: [CHAR] IMMEDIATE CHAR [COMPILE] LITERAL ;

\
\ Comparisons
\

: >   ( a b -- c )  SWAP < ;
: <=  ( a b -- c )  > NOT ;
: >=  ( a b -- c )  < NOT ;
: 0=  ( a -- b )    0 = ;
: 0<  ( a -- b )    0 < ;
: 0>  ( a -- b )    0 > ;
: 0<= ( a -- b )    0 <= ;
: 0>= ( a -- b )    0 >= ;

: MAX ( a b -- c ) 2DUP > IF DROP ELSE NIP THEN ;
: MIN ( a b -- c ) 2DUP < IF DROP ELSE NIP THEN ;

\ Now that we have better control flow constructs, let's reimplement block
\ comments so they can be nested.
: ( IMMEDIATE
    1                         \ nested level
    BEGIN
        KEY
        DUP [CHAR] ( = IF
            DROP 1+           \ increase depth
        ELSE [CHAR] ) = IF
            1-                \ decrease depth
        THEN THEN

        ?DUP 0=               \ do we have matched parens?
    UNTIL
;

( now we have
    ( nested )
  comment syntax! )

\
\ String utils
\

\ CR prints a carriage return
: CR '\n' EMIT ;

\ SPACE prints a space
: SPACE BL EMIT ;

\ Compile single character to HERE. Won't leave an aligned pointer!
: C, ( a -- )
    HERE @ C!     \ write a character to *HERE
    1 HERE +!     \ *HERE++
;

\ cond UNLESS false-part ELSE true-part THEN
\
\ An inverted IF
: UNLESS IMMEDIATE ( cond -- offset_addr )
    ' NOT ,        \ compile NOT (invert cond)
    [COMPILE] IF
;

\ Return the number of elements in the stack
: DEPTH     SP0 SP@ - CELL- CELL / ;
: RDEPTH    RP0 RP@ - CELL / ;

\ Compile the compilation semantics of following word to current definition
: POSTPONE IMMEDIATE ( -- )
    BL WORD FIND
    DUP word>immediate? IF
        >CFA ,
    ELSE
        ' LIT , >CFA ,
        ' , ,
    THEN
;

: ?interpreting ( -- a )
    STATE @ 0= ;

: ?compiling ( -- a )
    STATE @ ;

: S" IMMEDIATE ( -- addr len )
    ?compiling IF
        ' LITSTRING ,  \ compile LITSTRING
        HERE @         \ push current position to stack
        0 ,            \ placeholder string length

        BEGIN          \ compile byte by byte until we reach end of string
            KEY
            DUP [CHAR] " !=
        WHILE
            C,
        REPEAT
        DROP           \ drop final quote char
        DUP            \ ( len_addr len_addr )
        HERE @ SWAP -  \ push length of string
        CELL-          \ placeholder one cell before start of string
        SWAP !         \ write length back to original addr

        ALIGN          \ keep HERE aligned

    ELSE               \ immediate mode
        HERE @         \ push current addr

        BEGIN          \ write byte by byte until end of string
            KEY
            DUP [CHAR] " !=
        WHILE
            OVER C!
            1+
        REPEAT
        DROP           \ ignore final quote
        HERE @ -       \ calculate length
        HERE @         \ push start addr (doesn't change because of immediate mode)
        SWAP           \ ( addr len )
        DUP HERE +!    \ advance here to end of str
    THEN
;

\ Create and immediately print a literal string.
: ." IMMEDIATE ( -- )
    ?compiling IF
        [COMPILE] S"  \ read + compile the literal string
        ' TELL ,      \ compile word to print it
    ELSE              \ we're in immediate mode
        BEGIN         \ loop through each char of string until closing quote
            KEY
            DUP [CHAR] " !=
        WHILE
            EMIT     \ print each byte
        REPEAT
        DROP         \ drop closing quote
    THEN
;

\ val >= lo && val < hi
: WITHIN ( val lo hi -- a )
    2 PICK >       \ ( val lo hi val --> val lo <hi )
    -ROT >=        \ ( <hi val lo    --> <hi >=lo)
    AND
;

\ Not a standard word, but in practice I need inclusive intervals
\ more often.
\
\ val >= lo && val <= hi
: BETWEEN ( val lo hi -- a) 1+ WITHIN ;

\ Compile a reference to the word currently being defined.
: RECURSE IMMEDIATE ( -- )
    LATEST @ >CFA ,
;

\ CASE c1 OF v1 ENDOF c2 OF v2 ENDOF ... default ENDCASE
\
\ This word pushes a marker word to the stack to be consumed by the child
\ branches.
\
\ Compiles into an IF/ELSE/THEN chain.
: CASE IMMEDIATE ( -- 0 )
    0
;

\ ... cond OF value ENDOF ...
: OF IMMEDIATE ( 0 ... -- 0 ... if-marker )
    ' OVER ,
    ' = ,
    [COMPILE] IF
    ' DROP ,
;

\ ... lo hi RANGEOF value ENDOF ...
: RANGEOF IMMEDIATE ( 0 ... -- 0 ... if-marker )
    ' LIT , 2 ,
    ' PICK ,
    ' -ROT ,
    ' WITHIN ,
    [COMPILE] IF
    ' DROP ,
;

\ ... cond OF value ENDOF ...
: ENDOF IMMEDIATE ( 0 ... if-marker -- 0 ... else-marker )
    [COMPILE] ELSE
;

\ Pops the if/else jump positions until we reach the 0 placed by CASE.
: ENDCASE IMMEDIATE ( 0 ... marker -- )
    ' DROP ,
    BEGIN
        ?DUP
    WHILE
        [COMPILE] THEN
    REPEAT
;

: >BODY ( xt -- a-addr )
    5 CELLS +
;

\ Copy u bytes from c-from to c-to.
\ The memory regions must not be overlapped.
: memcpy ( c-from c-to u -- )
    BEGIN
        DUP 0>
    WHILE
        1- >R     \ decrement u, save
        OVER C@
        OVER C!   \ copy character
        1+ >R     \ increment c-to, save
        1+        \ increment c-from
        R> R>
    REPEAT
    3DROP
;

: memcpy, ( addr-from size -- )
    TUCK           \ (.. -- size a-from size)
    HERE @ SWAP    \ (.. -- size a-from a-here size)
    memcpy         \ Copy `size` bytes from `a-from` to `HERE`
    HERE +!        \ Increment *here
;

\ Print all known (and not hidden) words
: WORDS
    LATEST @
    BEGIN
        ?DUP
    WHILE
        DUP word>hidden? UNLESS
            DUP word>name TELL SPACE
        THEN
        word>prev
    REPEAT
    CR
;

\ Create a new named word which pushes its own address to the stack when called.
\
\ This can be used as a primitive to build variables, constants, etc.
: CREATE ( -- )
    ALIGN
    LATEST @ ,               \ prev word link
    HERE @ CELL- LATEST !    \ update latest
    0 C,                     \ flags byte
    BL WORD
    DUP C,                   \ length byte
    memcpy,                  \ copy name
    ALIGN                    \ add padding
    DOCOL ,                  \ compile docol
    ' LIT ,
    HERE @ 3 CELLS + ,       \ compile the address after the EXIT
    ' NOP ,                  \ DOES>, can modify this later
    ' EXIT ,                 \ compile exit
;

\ :NONAME body-part ; EXECUTE
\
\ Anonymous (unnamed) word. Leaves an "execution token" on the stack which can
\ be EXECUTE'd or compiled to run the body of the word.
: :NONAME ( -- xt )
    ALIGN
    LATEST @ ,             \ back-link
    HERE @ CELL- LATEST !  \ update word
    0 C,                   \ flags byte
    0 C,                   \ length byte
    ( notice: no name here )
    ALIGN                  \ padding
    HERE @                 \ the execution token (= DOCOL entry)
    [ DOCOL ] LITERAL ,    \ compile DOCOL
    ]                      \ start compiling body
;

\ Runtime helper for DOES>
\
\ Swaps out the NOP of a CREATE'd word with xt from the stack
: (does>) ( xt -- )
    LATEST @ >CFA  \ DOCOL addr of last defined word
    3 CELLS +      \ Point to addr of NOP
    !              \ Swap it out with the xt
;

\ Magic trick which allows the call behavior of a word to be changed at runtime
\
\   CREATE a 0 , DOES> @ 1+ ;
\   a @    --> 0
\   a      --> 1
\   1 a !  --> 1
\   a      --> 2
: DOES> IMMEDIATE
    ALIGN
    0                  \ Placeholder, will store xt from :NONAME call
    [COMPILE] LITERAL  \ (run) push value in prev cell
    HERE @ CELL-       \ Push pointer to the placeholder cell

    ' (does>) ,        \ (run) rewrite NOP addr with :NONAME xt
    [COMPILE] ;        \ (run) finish compilation of DOES>

    :NONAME            \ (comp) write body of DOES> into anon word
    SWAP !             \ (comp) (xt &placholder)
;

\ val CONSTANT name
\
\ Define a word (immediate value) which pushes a constant value to the stack
: CONSTANT ( val -- )
    CREATE , DOES> @
;

\ "allocate memory" by incrementing the data pointer by n bytes
: ALLOT ( n -- ) HERE +! ;

\ Allocate a cell of memory as a named variable
\
\ Define: VARIABLE foo
\ Read  : foo @
\ Write : 1 foo !
: VARIABLE CREATE 0 , ;

: ON  ( addr -- ) TRUE SWAP ! ;
: OFF ( addr -- ) FALSE SWAP ! ;

VARIABLE exc-handler

\ TODO: document me
\
\ CATCH and THROW are from the implementation given in
\ https://forth-standard.org/standard/exception/THROW
: CATCH ( xt -- exception# | 0 )
   SP@ >R                ( xt )       \ save data stack pointer
   exc-handler @ >R      ( xt )       \ and previous handler
   RP@ exc-handler !     ( xt )       \ set current handler
   EXECUTE               ( )          \ execute returns if no THROW
   R> exc-handler !      ( )          \ restore previous handler
   R> DROP               ( )          \ discard saved stack ptr
   0                     ( 0 )        \ normal completion
;

: THROW ( ??? exception# -- ??? exception# )
    ?DUP IF                 ( exc# )     \ 0 THROW is no-op
      exc-handler @ RP!     ( exc# )     \ restore prev return stack
      R> exc-handler !      ( exc# )     \ restore prev handler
      R> SWAP >R            ( saved-sp ) \ exc# on return stack
      SP! DROP R>           ( exc# )     \ restore stack

      \ Return to the caller of CATCH because return
      \ stack is restored to the state that existed
      \ when CATCH began execution
    THEN
;

: ABORT -1 THROW ;

\
\ Syscalls
\

0 CONSTANT no-error

1 CONSTANT sys-exit  \ void exit(int rval)
2 CONSTANT sys-fork  \ int fork(void)
3 CONSTANT sys-read  \ ssize_t read(int fd, void *buf, size_t count)
4 CONSTANT sys-write \ ssize_t write(int fd, const void *buf, size_t count)
5 CONSTANT sys-open  \ int open(const char *path, int flags, mode_t mode)
6 CONSTANT sys-close \ int close(int fd)

: SYSCALL1 ( a op -- r )     >R >R 0 0 R> R> SYSCALL ;
: SYSCALL2 ( b a op -- r )   >R >R >R 0 R> R> R> SYSCALL ;
: SYSCALL3 ( c b a op -- r ) SYSCALL ;

: DIE ( val -- ) sys-exit SYSCALL1 ;
: BYE ( -- )     no-error DIE ;

\
\ DO..LOOP
\

: I 2 RPICK ; \ iteration of inner-most loop param
: J 4 RPICK ; \ ... 2nd loop
: K 4 RPICK ; \ ... 3rd loop

1 CONSTANT DO-MARK
2 CONSTANT LEAVE-MARK

CREATE DO-STACK 16 CELLS ALLOT
VARIABLE DO-IDX
    DO-STACK 16 CELLS + DO-IDX !

\ push `a` onto the `do-stack`
: >do ( a -- do: a )
    CELL DO-IDX -!
    DO-IDX @ !
;

\ pop `a` from the `do-stack`
: do> ( do: a -- a )
    DO-IDX @ @
    CELL DO-IDX +!
;

\ peek at top of `do-stack`
: do@ ( -- ) DO-IDX @ @ ;

\ limit initial DO loop-part LOOP
\ limit initial DO loop-part increment +LOOP
: DO IMMEDIATE ( C: -- do-sys ) ( limit init -- )
    ' >R , ' >R ,   \ save init and limit
    HERE @ >do
    do-mark >do
;

\ Early return from a DO loop
: LEAVE IMMEDIATE
    ' BRANCH , 0 ,     \ jump with placeholder, later overwritten
    HERE @ CELL- >do   \ push addr of placeholder to do stack
    leave-mark >do
;

\ Update all positions where the loop called LEAVE to jump to `addr`
: (leave) ( addr -- )
    BEGIN
        do@
        leave-mark
    = WHILE
        do> DROP   \ drop marker
        do>        \ LEAVE addr
        2DUP -
        SWAP !
    REPEAT
    DROP
;

\ DO will always loop at least once, but ?DO will skip if there's nothing to
\ do.
: ?DO IMMEDIATE ( C: -- do-sys ) ( limit init -- )
    ' 2DUP , ' = , [COMPILE] IF
        ' 2DROP ,
        ' EXIT ,
    [COMPILE] THEN

    ' >R , ' >R ,   \ save init and limit
    HERE @ >do
    do-mark >do
;

\ Pop the loop control parameters off the return stack so the loop can be
\ cleanly exited early.
: UNLOOP IMMEDIATE ( -- ) ( R: loop-sys -- )
    ' RDROP ,
    ' RDROP ,
;

\ Check loop bounds and jump back to the top
: LOOP IMMEDIATE ( C: do-sys -- ) ( -- ) ( R: loop-sys -- )
    ' R> ,   ' R> , ' 1+ ,  \ (run) restore limit and index, increment index
    ' 2DUP , ' >R , ' >R ,  \ (run) copy and save
    ' = , ' 0BRANCH ,       \ (run) if we haven't reached the end
    HERE @ CELL+ (leave)    \ (comp) update `LEAVE` calls to jump past end of loop
    do> DROP                \ (comp) drop do-marker
    do> HERE @ - ,          \ (comp) compile distance from matching `DO`
    POSTPONE UNLOOP         \ (run) drop the loop params from return stack
;


\ Because +LOOP has an arbitrary increment, we can't check for exact equality.
\ We don't know the starting value here, just the current index and the "limit",
\ so the logic is to tell if we've crossed over that value on this iteration
\
\ Wizardry stolen from planckforth
: ?LOOP-END
    SWAP -
    2DUP +
    OVER XOR
    >R XOR R>
    AND 0<
;

: +LOOP IMMEDIATE ( C: do-sys -- ) ( -- ) ( R: loop-sys -- )
    ' R> , ' R> , ' 3DUP ,      \ restore limit and index and copy ( incr limit index )
    ' ROT , ' + ,               \ apply the increment              ( limit index+incr )
    ' >R , ' >R ,               \ save limit and index             ( -- incr limit index )
    ' ?LOOP-END , ' 0BRANCH ,   \ if we haven't reached the end
    do> DROP                    \ ignore the do marker
    do> HERE @ - ,              \ compile distance from matching `DO`
    POSTPONE UNLOOP
;

: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- )     16 BASE ! ;

: print-uint ( base val -- )
    OVER /MOD ?DUP IF
        >R OVER R>
        RECURSE
    THEN
    DUP 10 < IF
        [CHAR] 0 +
    ELSE
        10 - [CHAR] a +
    THEN EMIT
    DROP
;

: print-int ( base val -- )
    DUP 0< IF
        [CHAR] - EMIT
        NEGATE
    THEN
    print-uint
;

: U.   ( val -- ) BASE @ SWAP print-uint SPACE ;
: H.   ( val -- ) 16 SWAP print-uint SPACE ;
: hex. ( val -- ) [CHAR] $ EMIT H. ;
: dec. ( val -- ) 10 SWAP print-int SPACE ;
: .    ( val -- ) BASE @ SWAP print-int SPACE ;

: SPACES ( n -- ) BEGIN DUP 0> WHILE SPACE 1- REPEAT DROP ;
: UWIDTH ( val -- w ) BASE @ / ?DUP IF RECURSE 1+ ELSE 1 THEN ;

\ Print `val` right aligned to at least `pad` characters wide
: U.R  ( val pad -- )
    OVER UWIDTH - SPACES
    BASE @ SWAP print-uint
;

\ Print right aligned number, padded with zero instead of spaces
: U.R0  ( val pad -- )
    OVER UWIDTH - ?DUP IF 0 DO [CHAR] 0 EMIT LOOP THEN
    BASE @ SWAP print-uint
;

: .R  ( val pad -- )
    OVER 0>= IF
        U.R
    ELSE
        SWAP NEGATE
        DUP UWIDTH 1+
        ROT SWAP - SPACES
        [CHAR] - EMIT
        BASE @ SWAP print-uint
    THEN
;

\ Convert string to uppercase (mutates in place)
: upcase ( addr u -- addr u )
    DUP 0 DO
        OVER I +
        DUP C@ DUP [CHAR] a [CHAR] z BETWEEN IF
            32 - SWAP c!
        ELSE
            2DROP
        THEN
    LOOP
;

: streq? ( addr1 u addr2 u -- b )
    \ Check string len first
    ROT ( a1 a2 u u )
    OVER = UNLESS        \ length mismatch
        3DROP FALSE EXIT
    ELSE ?DUP UNLESS     \ length match, but both are zero
        2DROP TRUE EXIT
    THEN THEN

    0 DO
        ( a1 a2 )
        DUP C@        ( a1 a2 c2 )
        ROT DUP C@    ( a2 c2 a1 c1 )
        ROT = UNLESS
            2DROP
            UNLOOP
            FALSE EXIT
        THEN
        1+ SWAP 1+
    LOOP

    2DROP
    TRUE
;

\ Counted string to null-terminated string.
: >c-str ( addr u -- c-str )
    \ Sanity check. There should always be a padding null, but this is going to
    \ be painful to debug if it's missing for some reason.
    2DUP + C@ 0= UNLESS
        ." [bug] str missing null terminator: " tell cr
        -1 throw
    THEN
    DROP
;

\ Push length of null terminated string to stack
: c-str> ( addr -- addr len ) 0 BEGIN 2DUP + C@ WHILE 1+ REPEAT ;

: ?digit     ( ch -- bool ) [CHAR] 0 [CHAR] 9 BETWEEN ;
: ?lowercase ( ch -- bool ) 97 122 BETWEEN ;
: ?uppercase ( ch -- bool ) 65 90 BETWEEN ;

: parse-uint ( addr n base -- val ok? )
    ROT >R >R \ save base + addr ( R: addr base )
    0         \ accumulator
    SWAP
    BEGIN
        ?DUP
    WHILE    ( acc n )
        SWAP          ( n acc )
        R>            \ restore base
        DUP ROT       ( base acc base )
        * SWAP        \ acc *= base           ( acc' base )
        R>            \ restore addr
        DUP C@        \ get next char         ( acc n base addr ch )
        -ROT 1+       \ increment addr
        >R >R         \ save base, addr       ( acc n ch )

        \ Get decimal value of character
        DUP ?digit     IF [CHAR] 0 - ELSE
        DUP ?lowercase IF [CHAR] a - 10 + ELSE
        DUP ?uppercase IF [CHAR] A - 10 + THEN THEN THEN

        DUP R@           \ restore base
        > IF             \ check if the character is invalid in the base
            DROP 2DROP   \ clean up and exit with 0
            RDROP RDROP
            0 FALSE
            EXiT
        THEN

        +             \ acc += val ( n acc )
        SWAP 1-       \ decr string length
    REPEAT
    RDROP RDROP

    TRUE              \ success
;

: escaped-char ( ch -- val )
    CASE
        [CHAR] a OF  7 ENDOF  \ alert
        [CHAR] b OF  8 ENDOF  \ backspace
        [CHAR] t OF  9 ENDOF  \ tab
        [CHAR] n OF 10 ENDOF  \ newline
        [CHAR] v OF 11 ENDOF  \ vertical tab
        [CHAR] f OF 12 ENDOF  \ formfeed
        [CHAR] r OF 13 ENDOF  \ carriage return
        [CHAR] e OF 27 ENDOF  \ escape
        [CHAR] ' OF 39 ENDOF  \ single quote
        [CHAR] \ OF 92 ENDOF  \ backslash
        ( else )
            0 SWAP
    ENDCASE
;

\ Attempt to parse a number from string at `addr`.
\
\ NOTE: Doesn't follow Forth standard signature (because it feels too cumbersome to
\ me?)
: >NUMBER ( addr n -- val ok? )
    ?DUP UNLESS          \ handle empty string
        DROP
        0 FALSE
        EXIT
    THEN

    SWAP DUP C@     \ get first char of str

    CASE
        [CHAR] - OF       \ negative
            1+ SWAP 1-
            RECURSE IF NEGATE TRUE ELSE FALSE THEN
        ENDOF

        [CHAR] $ OF        \ hex
            1+ SWAP 1-
            16 parse-uint
        ENDOF

        [CHAR] # OF        \ decimal
            1+ SWAP 1-
            10 parse-uint
        ENDOF

        [CHAR] ' OF        \ character code
            1+ SWAP 1-     ( addr size )

            \ Check we have at least 2 more char (the actual char plus closing
            \ quote, or \escaped-char plus closing quote)
            DUP 2 3 BETWEEN UNLESS
                2DROP      \ drop addr + size
                0 FALSE    \ return value
                EXIT
            THEN

            1-            \ decr size for next char
            SWAP DUP C@   ( size addr ch )
            DUP [CHAR] \ = IF
                DROP
                SWAP 1- SWAP
                1+ DUP C@    \ get escaped char
                escaped-char ?DUP UNLESS
                    2DROP
                    0 FALSE
                    EXIT
                THEN
            THEN

            -ROT               ( ch size addr )
            1+ C@ [CHAR] ' =   \ next char is a quote
            SWAP 1- 0=         \ and it's the last one
            AND
        ENDOF

        DROP SWAP
        BASE @      \ fall back to current base
        parse-uint

        0           \ ENDCASE drops top of stack
    ENDCASE
;

\
\ Structures
\

\ STRUCT{ cell% FIELD name1 ... }STRUCT name
: STRUCT{ ( -- offset ) 0 ;
: }STRUCT ( offset -- ) CREATE , DOES> @ CELL SWAP ;

: CELL% ( -- align size ) CELL CELL ;
: BYTE% ( -- align size ) 1 1 ;

: FIELD ( offset align size -- addr )
    -ROT aligned-by ( size offset align -- size offset' )
    CREATE DUP , + DOES> @ + ;

\ Reserve space to create a structure
: %ALLOT ( align size -- addr )
    HERE @
    -ROT SWAP ( here size addr )
    align-by ALLOT
;

\
\ File I/O
\

0 CONSTANT R/O
1 CONSTANT W/O
2 CONSTANT R/W

: OPEN-FILE ( addr u mode -- fd err )
    >R >c-str R> SWAP
    sys-open SYSCALL2
    DUP 0< IF DUP ELSE no-error THEN ;

: CLOSE-FILE ( fd -- err )
    sys-close SYSCALL1
    ?DUP 0< IF DUP ELSE no-error THEN ;

: READ-FILE ( addr len fd -- len err )
    >R SWAP R>
    sys-read SYSCALL3 \ read(fd, &addr, len)
    DUP 0< IF DUP ELSE no-error THEN ;

\
\ Stream reader
\

1024 CONSTANT BUFSIZE
0    CONSTANT stdin

\ TODO: more distinctive marker value
0   CONSTANT EOF

STRUCT{
    cell%           FIELD stream>fd
    byte% 128 *     FIELD stream>name
    byte% BUFSIZE * FIELD stream>buf
    cell%           FIELD stream>buf-pos
    cell%           FIELD stream>buf-end
    cell%           FIELD stream>lineno
    cell%           FIELD stream>next
}STRUCT stream%

VARIABLE streams
    0 streams !

: push-stream ( fd -- )
    stream% %allot            \ ( fd addr )
    TUCK stream>fd !          \ ( addr )
    0 OVER stream>buf-pos !
    0 OVER stream>buf-end !
    0 OVER stream>lineno !
    streams @ OVER stream>next !
    streams !
;

: pop-stream ( -- fd )
    streams @ DUP
    stream>next @ streams !
    stream>fd @
;

\ Get the length in bytes of the data currently in this stream's
\ buffer.
: stream-buffer-count ( s -- len )
    DUP stream>buf-end @ SWAP stream>buf-pos @ - ;

: stream-inspect ( s -- s )
    ." stream { " cr
    ."    base: " DUP . cr
    ."    fd:   " DUP stream>fd DUP . @ .  cr
    ."    name: " DUP stream>name c-str> TELL cr
    ."    buf:  " DUP stream>buf DUP . @ . cr
    ."    bpos  " DUP stream>buf-pos DUP . @ . cr
    ."    bend  " DUP stream>buf-end DUP . @ . cr
    ."    line  " DUP stream>lineno DUP . @ . cr
    ."    next  " DUP stream>next DUP . @ . cr
    ." }" cr
;

: stream-buffer-clear ( s -- )
    0 OVER stream>buf-end !
    0 SWAP stream>buf-pos !
;

: stream-buffer-refill ( s -- len e )
    DUP stream-buffer-clear

    \ non-file based buffer, can't refill it
    DUP stream>fd @ 0< IF DROP 0 no-error EXIT THEN

    DUP >R

    stream>buf
        OVER stream>fd @
        BUFSIZE
        SWAP
        READ-FILE

    OVER R> stream>buf-end !
;

\ Consume `len` bytes from the stream's buffer. Doesn't refill or check that
\ enough data is in the buffer
: stream-buffer-copy ( to-addr len s -- len )
    DUP stream>buf-pos @ OVER stream>buf +
        3 PICK             \ addr
        3 PICK             \ len
        memcpy

    stream>buf-pos OVER SWAP +!
    NIP
;

\ Read `len` bytes from the stream into `addr`. If there aren't enough bytes
\ left in the buffer, will read more data from the source, or return smaller
\ than `len` bytes on EOF.
: stream-read ( addr len s -- len err )
    \ check if we have enough buffered data to avoid calling read()
    DUP stream-buffer-count ( addr len s cnt )
    2 PICK >= IF            ( addr len s )
        stream-buffer-copy
        no-error EXIT
    THEN

    DUP stream-buffer-count 0> IF
        \ we need to read more data, first copy whatever was in the
        \ buffer
        DUP stream>buf-pos @
        OVER stream-buffer-count DUP >R
        4 PICK SWAP memcpy             ( addr len s )
        ROT R@ + -ROT                  ( addr' len s )
        SWAP R@ - SWAP                 ( addr rem' s )
    ELSE
        0 >R
    THEN

    \ Refill the buffer, read next `BUFSIZE` bytes from stream
    DUP stream-buffer-refill

    ?DUP IF
        ." err read:" DUP . CR
        >R 2DROP
        0 R>
        EXIT
    THEN

    ?DUP UNLESS             \ check that we've read something
        3DROP
        R> no-error
        EXIT
    THEN

    SWAP >R MIN R>          \ avoid reading past end of buffer
    stream-buffer-copy      ( addr rem s -- len )
    R> +                    \ add whatever we transfered from the old buffer
    no-error
;

\ read single character from stream, return EOF on end
: stream-key ( s -- ch e )
    >R
    0 SP@
    1 R> stream-read THROW

    0= IF DROP EOF no-error EXIT THEN

    no-error
;

\
\ Stage 2 interpreter
\

-1 CONSTANT err-abort
-2 CONSTANT err-unexpected-eof
-3 CONSTANT sig-eof
-4 CONSTANT err-undefined-word
-5 CONSTANT err-quit
-6 CONSTANT err-stack-underflow

VARIABLE source-buffer     BUFSIZE ALLOT
VARIABLE source-buffer-pos 0 source-buffer-pos !
VARIABLE source-buffer-end 0 source-buffer-end !

\ TODO: make use of this
VARIABLE SOURCE-ID -1 SOURCE-ID !

\ TODO: clean up native definition.
: (source) ( -- addr len ) SOURCE ;
: SOURCE ( -- addr u ) source-buffer source-buffer-end @ ;
: >IN    ( -- addr )   source-buffer-pos ;

CREATE word-buffer 64 CHARS ALLOT
VARIABLE word-len

\ Forth REPL, this time hosted in Forth
: INTERPRET ( -- )
    BL ['] WORD CATCH CASE
        0 OF ( no error ) ENDOF

        \ An EOF here isn't really an issue, it's just the end of
        \ the file.
        err-unexpected-eof OF
            sig-eof THROW
        ENDOF

        ( else )
            ." throw: " DUP . cr DUP THROW
    ENDCASE

    DUP word-len !             \ Update len
    2DUP word-buffer TUCK DROP ( addr len addr buf len )
    memcpy                     \ copy word into word buffer

    FIND ?DUP IF               \ Found the word
        \ If word is immediate or we're interpreting, run now.
        \ Otherwies, compile the word
        ?interpreting OVER word>immediate? OR IF
            >CFA EXECUTE
        ELSE
            >CFA ,
        THEN
    ELSE
        \ Word not found, try to parse it as a number
        word-buffer word-len @ >NUMBER UNLESS
            err-undefined-word THROW
        THEN

        \ If we're interpreting we leave the number on the stack
        ?compiling IF
            [COMPILE] LITERAL
        THEN
    THEN
;

\ Print an error along with the line of code raising it.
: format-error ( err -- )
    streams @
        DUP stream>lineno @
        SWAP stream>name c-str>

    CR TELL ." :" .
    word-len @ ?DUP IF
        ." @ '"
        word-buffer SWAP TELL
        ." ' "
    THEN

    CASE
        err-abort OF ." [ Aborted ]" ENDOF
        err-unexpected-eof OF ." [ Unexpected EOF ]" ENDOF
        err-undefined-word OF ." undefined word" ENDOF
        err-stack-underflow OF ." stack underflow" ENDOF
        ( else ) ." unknown exception: " DUP .
    ENDCASE

    CR

    \ Print out where the error occurred if there's
    \ something useful in the input buffer
    SOURCE ?DUP IF
        SPACE TELL           \ print the line
        >IN @ word-len @ -   \ point to start of word
        1 MAX 0 ?DO
            [CHAR] ~ EMIT
        LOOP
        [CHAR] ^ EMIT CR
    ELSE
        DROP
    THEN
;

: (interpret-loop)
    BEGIN
        ['] INTERPRET CATCH
        \ EOF here is "clean" and represents the actual end of file (rather
        \ than in the middle of reading something important). Propagate it up.
        DUP sig-eof = IF THROW THEN

        \ All other errors are real and should be reported
        ?DUP IF format-error THEN

        \ Check if the stack pointer is out of bounds
        SP@ SP0 > IF err-stack-underflow format-error THEN
    AGAIN
;

\ Enter second stage interpreter - we're self-hosted baby
:NONAME
    \ Clear out the return stack
    RP0 RP!

    ['] (interpret-loop) CATCH

    \ This is the top-level interpreter, so when we catch an
    \ exception here it should exit the process.
    \
    \ When we get a "normal" EOF (which happens when the entire
    \ file is consumed), we exit cleanly, otherwise, return the
    \ error status.
    DUP sig-eof = IF DROP BYE ELSE DIE THEN
; EXECUTE

\
\ INCLUDE / REQUIRE
\

\ Read from already open file descriptor
: INCLUDE-FILE ( fd -- )
    push-stream

    \ We can't know the name from the file descriptor, caller can overwrite
    \ later
    s" (INCLUDE-FILE fd)" streams @ stream>name SWAP memcpy

    ['] (interpret-loop) CATCH DROP
    pop-stream CLOSE-FILE THROW
;


\ Read from a named file
: INCLUDED ( addr u -- )
    2DUP R/O OPEN-FILE THROW
        push-stream

    streams @ stream>name SWAP memcpy

    ['] (interpret-loop) CATCH DROP

    pop-stream CLOSE-FILE THROW
;

\ Convenience wrapper for INCLUDED, read from a named word
: INCLUDE ( -- ) BL WORD INCLUDED ;

STRUCT{
    cell%       FIELD required>next
    byte% 128 * FIELD required>name
}STRUCT required%

VARIABLE required-files
    0 required-files !

: required? ( addr u -- b )
    required-files @
    BEGIN
        ?DUP UNLESS
            2DROP
            FALSE EXIT
        THEN

        ( addr u ptr )
        DUP required>name c-str> ( addr u ptr name len )
        4 PICK 4 PICK            ( addr u ptr name len addr u )
        streq? IF
            DROP 2DROP
            TRUE EXIT
        THEN

        required>next @
    AGAIN
;

\ Read from a named file, but dedupe so it's only done once.
: REQUIRED ( addr u -- )
    2DUP required? IF
        2DROP
    ELSE
        2DUP

        required% %allot

        DUP required-files @ SWAP required>next !
        DUP required-files !

        ( addr len ptr )
        required>name SWAP memcpy

        INCLUDED
    THEN
;

\ Just like `INCLUDE`, but checking if the import has already been done.
: REQUIRE ( -- ) BL WORD REQUIRED ;

\
\ Stream-aware KEY and WORD
\

\ Read next line of input from current file
: REFILL ( -- ok )
    streams @ 0= IF
        ." [EOF] exhausted all inputs" cr
        FALSE EXIT
    THEN

    1 streams @ stream>lineno +!

    0 source-buffer-pos !
    0 source-buffer-end !

    BEGIN
        streams @ stream-key THROW

        DUP EOF = IF
            DROP
            FALSE EXIT
        THEN

        DUP
        source-buffer source-buffer-end @ + c!
        1 source-buffer-end +!

        \ Make sure we don't overrun the buffer on especially long
        \ lines.
        source-buffer-end @ BUFSIZE >= IF
            ." [REFILL] max buf size exceeded." CR
            DROP TRUE EXIT
        THEN
    '\n' = UNTIL

    TRUE
;

\ Reimplement KEY to pull from current buffer
: new-key ( -- a )
    source-buffer-pos @ source-buffer-end @ >= IF
        REFILL UNLESS
            err-unexpected-eof THROW
        THEN
    THEN

    source-buffer
        source-buffer-pos @ +

    1 source-buffer-pos +!
    C@
;

: whitespace? ( ch -- bool )
    CASE
        '\n' OF TRUE ENDOF
        BL   OF TRUE ENDOF
        '\t' OF TRUE ENDOF
        '\r' OF TRUE ENDOF
        ( else )
            FALSE SWAP
    ENDCASE
;

: skip-whitespace ( d -- k )
    BEGIN
        KEY                ( delim key )
        DUP whitespace?    ( delim key key -- delim key is-sp )
    WHILE
        DROP               ( delim )
    REPEAT
;

\ Advance past the delimiter given. If delimiter is BL, will skip ALL
\ whitespace chars.
: skip-delim ( d -- k )
    DUP BL = IF
        DROP
        skip-whitespace
    ELSE
        BEGIN
            KEY  ( delim k )
            OVER ( delim k delim )
            OVER ( delim k delim k )
        = WHILE
            DROP ( delim )
        REPEAT
        NIP      ( k )
    THEN
;

: new-word ( delim -- c-addr u )
    DUP skip-delim
    word-buffer TUCK C!
    1+                     ( delim buf )
    BEGIN
        SWAP KEY           ( buf delim k )
        OVER BL = IF
            DUP whitespace?
        ELSE
            2DUP =
        THEN               ( buf delim k is-delim? )

        IF                 \ We've found the delimiter, end the word
            2DROP
            0 SWAP C!
            word-buffer c-str>
            EXIT
        THEN
                          \ Haven't hit the delim yet, write the char and
                          \ continue
        ROT               ( delim k buf )
        2DUP c!           ( delim k buf k buf -- delim k buf )
        1+
        NIP
    AGAIN
;

\ Re-define line comments to use the updated key implementation
: \ IMMEDIATE BEGIN KEY '\n' = UNTIL ;

\ Similarly, redefine creation word `:` in Forth so we don't reference the
\ native `WORD` definition.
: :
    ALIGN
    LATEST @ ,
    HERE @ CELL- LATEST !
    hidden-bit C,
    BL WORD
    DUP C,
    memcpy,
    ALIGN
    DOCOL ,
    ]
;

:NONAME
    \ Transfer remaining content of boot.f into Forth-hosted buffer.
    \
    \ This will contain everything AFTER the :NONAME block.
    (source) DUP BUFSIZE > IF
        ." [BUG] too many characters remain in boot.f: " . CR
        ABORT
    THEN

    -1 push-stream
    streams @
        9000 OVER stream>lineno !      \ bogus line number, but will let us find the offset
        2DUP stream>buf-end !          \ Update buffer length of Forth impl
        stream>buf SWAP memcpy         \ Copy bytes from native buffer

    s" (bootstrap)"
        streams @ stream>name SWAP
        memcpy

    \ Transfer control of reading to our self-hosted implementations
    ' new-key
        ' KEY CELL+ !

    ' new-word
        ' WORD CELL+ !

    \ From this point on the interpreter is fully self-hosted. The assembly
    \ implementations are no longer running
; EXECUTE

\
\ CLI entry point
\

VARIABLE argc (argc) @ argc !
VARIABLE argv (argv) @ argv !

: SHIFT-ARGS ( -- )
    argc @ ?DUP UNLESS EXIT THEN
    1- argc !
    argv @ CELL+ argv !
;

: NEXT-ARG ( -- addr u )
    argc @ UNLESS 0 0 EXIT THEN
    argv @ @ c-str>
    SHIFT-ARGS
;

: ARG ( i -- addr u )
    DUP argc @ < UNLESS DROP 0 0 EXIT THEN
    CELLS argv @ + @ c-str>
;

: print-usage
    ." Usage: arm64th [options] [file...]" cr
    ." Options:" cr
    ."   -h       Display this very helpful message." cr
;

:NONAME
    SHIFT-ARGS        \ skip past executable name

    argc @ 1 >= IF
        BEGIN NEXT-ARG DUP WHILE
            2DUP s" -h" streq? IF
                2DROP print-usage
            ELSE
                INCLUDED
            THEN
        REPEAT
        2DROP
    ELSE
        ." [ " UNUSED CELL / . ." CELLS FREE ]" cr
        ." arm64th ok." cr
        stdin INCLUDE-FILE
    THEN
; EXECUTE
