LoadNewChunk::
    ld a, [wNewChunkFlags]
    and a
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
    ld d, a
    ld a, b
    and a, $F0 ; mask off just the new chunk flags
    cpl ; flip so we can clear just the flag we want
    ld b, a
    ld a, [wNewChunkFlags]
    and a, b
    or a, d ; combine all the flags
    ld [wNewChunkFlags], a ; clear the bit requesting this chunk
    ld a, c ; a now has the chunk quadrant we need to load
    ld [wChunkQuadrant], a ; save the quadrant for later

    sla c
    ld b, $00
    add hl, bc
    ld a, [hli] ; put the requested chunk x and y into the right spot in ram
    ld [wChunkX], a
    ld a, [hl]
    ld [wChunkY], a
    farcall CopyChunkHeader ; i.. want... CHUNK HEADER!
    farcall MarkUnrefrencedBlocks ; remove any refrences for the old chunk we are replacing

    ld a, [wChunkHeader + 2]
    and a, %11000000 ; get the bit depth of this chunk
    rlca
    rlca
    ld hl, .jumptable
    rst JumpTable

.jumptable
    dw Unpack1bpc
    dw Unpack2bpc
    dw Unpack4bpc
    dw Unpack8bpc

Unpack1bpc:

Unpack2bpc:

Unpack4bpc:

Unpack8bpc:
    ld de, wOverworldMapBlocks
    ld a, [wChunkQuadrant]
    bit 1, a
    jr z, .top
    set 1, d
.top
    bit 0, a
    jr z, .left
    set 0, d ; make sure we copy to the correct address within the chunks
.left

    ld hl, wChunkHeader
    ld b, [hl]
    inc hl
    ld c, [hl]
    inc hl
    ld a, [hli]
    ld l, [hl]
    ld h, a
    cpl
    or a, h ; set a to $FF
    call ReallyFarCopyBytes
    ret