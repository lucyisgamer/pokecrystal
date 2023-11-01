LoadNewChunk::
    ld a, [wNewChunkFlags]
    and a, %11110000 ; mask out just the flags for new chunks
    ret z ; if no new chunk loads are scheduled we can return early
    ld hl, wChunkCoordsArray
    ld bc, $11FF
.search
    rrc b ; b is used to figure out which chunk needs it's flag cleared
    inc c
    sla a
    jr nc, .search ; find the first chunk that is requesting a load
    ld a, b
    and a, $0F ; mask off just the new blockset flags
    ld c, a
    ld a, b
    and a, $F0 ; mask off just the new chunk flags
    cpl
    ld b, a
    ld a, [wNewChunkFlags]
    and a, b
    or a, c
    ld [wNewChunkFlags], a ; clear the bit requesting this chunk
    ld a, c ; a now has the chunk quadrant we need to load
    sla c
    ld b, $00
    add hl, bc
    ld a, [hli] ; put the requested chunk x and y into the right spot in ram
    ld [wChunkX], a
    ld a, [hl]
    ld [wChunkY], a
    farcall CopyChunkHeader ; i.. want... CHUNK HEADER!
    ret