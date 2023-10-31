LoadNewChunk::
    ld a, [wNewChunkFlags]
    and a, %11110000 ; mask out just the flags for new chunks
    ret z ; if no new chunk loads are scheduled we can return early
    ld hl, wChunkCoordsArray
    ld bc, $01FF
.search
    rrc b ; b is used to figure out which
    inc c
    sla a
    jr nc, .search
    ld a, b
    cpl
    ld b, a
    ld a, [wNewChunkFlags]
    and a, b
    ld [wNewChunkFlags], a ; clear the bit requesting this chunk
    ld a, c ; a now has the chunk quadrant we need to load
    sla c
    ld b, $00
    add hl, bc
    ld b, [hl]
    inc hl
    ld c, [hl]
    ret