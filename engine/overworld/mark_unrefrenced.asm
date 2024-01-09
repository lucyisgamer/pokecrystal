MarkUnrefrencedBlocks::
    ld a, BANK(sUsedCharblockFlags)
    call OpenSRAM

    ld a, [wChunkQuadrant]
    ld b, a
    inc b
    ld a, %10001000
.setFlag
    rlca
    dec b
    jr nz, .setFlag
    cpl
    ld c, a

    ld hl, sUsedCharblockFlags
    ld b, $80
.loop
    ld a, [hl]
    and a, c
    ld [hli], a
    dec b
    jr nz, .loop

    call CloseSRAM
    ret