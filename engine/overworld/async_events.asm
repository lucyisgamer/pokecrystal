processAsyncEvents:: ; runs after the overworld loop is done. put things that don't need to run every frame here
    ld hl, .timings - 2
.loop
    ld a, [rLY] ; check what scanline we're on
    ld b, a
    ld a, 141 ; number of active lines of display - 2
    sub a, b ; a now has the number of lines remaining for async work
    ret c ; if we're out of time return
.search
    inc hl ; march down the routine list
    inc hl
    cp a, [hl]
    jr c, .search ; if a routine needs more scanlines than are available keep looking
    push hl ; we've found a routine, set up the jumptable
    inc hl ; hl now points to the jumptable index
    ld a, [hl]
    ld hl, .jumptable
    rst JumpTable
    pop hl
    jr nc, .loop ; if an async function sets it's carry flag it can run multiple times
    dec hl ; back it up and try this function again
    dec hl
    jr .loop

.jumptable ; functions either need stubs here or need to be in the same bank as this jumptable
    dw .End
    dw LoadNewChunk
    dw TileDMA
    dw CopyBlocksetIDs
    dw ResolveCharblockLUT
    dw ApplyCharblockLUT
    dw CopyCharblock
    dw ResolveCharblockTiles

; note: each scanline is 228 cycles
.timings: ; timings here means how many scanlines it takes for the routine to run once in the worst case
    db $22, LOAD_CHUNK ; these are listed in priority order
    db $04, COPY_BLOCKSET_IDS
    db $14, RESOLVE_CHARBLOCK_LUT
    db $09, APPLY_CHARBLOCK_LUT
    db $20, COPY_CHARBLOCK
    db $40, RESOLVE_CHARBLOCK_TILES
    db $04, DMA_TILE
    db $00, END ; this is here to provide a simple escape hatch

DEF END EQU $00 ; jumps to a return to break out of processing
DEF LOAD_CHUNK EQU $01
DEF DMA_TILE EQU $02
DEF COPY_BLOCKSET_IDS EQU $03
DEF RESOLVE_CHARBLOCK_LUT EQU $04
DEF APPLY_CHARBLOCK_LUT EQU $05
DEF COPY_CHARBLOCK EQU $06
DEF RESOLVE_CHARBLOCK_TILES EQU $07

.End
    add sp, 4 ; evil stack mangling to break out of processing events
    ret ; note that the gameboy's stack grows DOWNWARD


TransferCharblock::
    call CopyCharblock
    ccf
    ret nc
    call ResolveCharblockTiles
    scf
    ret