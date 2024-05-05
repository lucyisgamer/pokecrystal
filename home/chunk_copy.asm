Copy8bpc:: ; assumes wChunkHeader and wChunkQuadrant are correct
    ldh a, [hROMBank]
    ld l, a
    ldh a, [hROMBankHigh]
    ld h, a
    push hl
    call SwitchToChunkBank
    
    ld a, [wChunkQuadrant] ; get our pointer to the map blocks
    ld de, wOverworldMapBlocks
    bit 0, a
    jr z, .left
    set 4, e
.left
    bit 1, a
    jr z, .top
    set 1, d
.top
    
    ld a, [wChunkHeader + 2]
    and a, %00111111
    or a, %01000000
    ld h, a
    ld a, [wChunkHeader + 3]
    ld l, a ; hl now points to the chunk

    ld b, $00
.loop
    ld a, [hli]
    ld [de], a
    inc b ; check for completion after copying
    jr z, .done ; otherwise we miss the last byte
    inc de
    ld a, e
    and a, $0F
    jr nz, .loop
    ld a, e
    add a, $10
    ld e, a
    ld a, d
    adc a, $00
    ld d, a
    jr .loop

.done
    pop hl
    ld a, l
    jp BigBankswitch

SwitchToChunkBank::
    ld a, [wChunkHeader]
    add a, HIGH(CHUNK_START_BANK)
    ld h, a
    ld a, [wChunkHeader + 1]
    add a, LOW(CHUNK_START_BANK)
    jp BigBankswitch ; tail call optimization
    
DEF CHUNK_START_BANK EQU $0E01