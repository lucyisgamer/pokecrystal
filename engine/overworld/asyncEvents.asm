processAsyncEvents:: ; runs after the overworld loop is done. put things that don't need to run every frame here
    ld hl, .timings - 2
.loop
    ld a, [rLY]
    ld b, a
    ld a, 141 ; number of active lines of display - 2
    sub a, b
.search
    inc hl
    inc hl
    cp a, [hl]
    jr c, .search
    push hl
    inc hl
    ld a, [hl]
    ld hl, .jumptable
    rst JumpTable
    pop hl
    jr nc, .loop ; if an async function sets it's carry flag it can run multiple times
    dec hl
    dec hl
    jr .loop

.jumptable
    dw .End
    dw LoadNewChunk
    dw BigLongFunction

.timings: ; timings here means how many scanlines it takes for the routine to run in the worst case
    db $FF, BIG_LONG_FUNCTION
    db $01, LOAD_CHUNK ; these are listed in priority order
    db $00, END

DEF END EQU $00 ; jumps to a return to break out of processing
DEF LOAD_CHUNK EQU $01
DEF BIG_LONG_FUNCTION EQU $02

.End
    inc sp ; evil stack mangling to break out of processing events
    inc sp ; note that the gameboy's stack grows DOWNWARD
    inc sp ; forgetting that will cause fun times
    inc sp
    ret

LoadNewChunk::
    ld a, $02
    cp a, d
    ld d, $02
    scf
    ret nz
    ccf
    ret

BigLongFunction::
    ld b, b
    ret