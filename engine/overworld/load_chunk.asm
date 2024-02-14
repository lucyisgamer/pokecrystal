LoadNewChunk::
    ld a, [wNewChunkFlags]
    and a, $0F
    ld b, a
    ld a, [wNewChunkFlags + 1]
    or a, b
    ld b, a
    ld a, [wNewChunkFlags + 2]
    or a, b
    ret nz ; if any chunks are currently in the process of being loaded we bail

    ld a, [wNewChunkFlags]
    and a, $F0
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
    call MarkUnrefrencedBlocks ; remove any refrences for the old chunk we are replacing

    ld a, [wChunkHeader + 2]
    and a, %11000000 ; get the bit depth of this chunk
    rlca
    rlca
    ld hl, .jumptable
    rst JumpTable
    ret

.jumptable
    dw Unpack1bpc
    dw Unpack2bpc
    dw Unpack4bpc
    dw Copy8bpc

Unpack1bpc:

Unpack2bpc:

Unpack4bpc:

Unpack8bpc:
    ld de, wOverworldMapBlocks
    ld a, [wChunkQuadrant]
    or a, d
    ld d, a
    ld hl, wChunkHeader
    ld b, [hl]
    ld a, $E
    add a, b
    ld b, a
    inc hl
    ld c, [hl]
    inc hl
    ld a, [hli]
    and a, %00111111
    add a, $40
    ld l, [hl]
    ld h, a
    cpl
    or a, h ; set a to $FF and clear carry in the process
    call ReallyFarCopyBytes
    ret


