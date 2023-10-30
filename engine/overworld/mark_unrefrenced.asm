MarkUnrefrencedBlocks::
    ldh a, [rSVBK]
    push af
    ld a, BANK(wCharblockLUT)
    ldh [rSVBK], a ; switch WRAM bank
.unrefrenceBlocks ; clear the corresponding bit for the current chunk quadrant 
    ld bc, $0000
    ld a, [wChunkQuadrant]
    ld hl, .jumptable
    rst JumpTable
.jumptable
    dw .topLeft
    dw .topRight
    dw .bottomLeft
    dw .bottomRight

.topLeft
    ld hl, wCharblockLUT
    res $07, [hl]
    inc hl
    inc hl
    dec b
    jr nz, .topLeft
    jr .zeroed

.topRight
    ld hl, wCharblockLUT
    res $06, [hl]
    inc hl
    inc hl
    dec b
    jr nz, .topRight
    jr .zeroed

.bottomLeft
    ld hl, wCharblockLUT
    res $05, [hl]
    inc hl
    inc hl
    dec b
    jr nz, .bottomLeft
    jr .zeroed

.bottomRight
    ld hl, wCharblockLUT
    res $04, [hl]
    inc hl
    inc hl
    dec b
    jr nz, .bottomRight ; fallthrough

.zeroed
    ld b, b
    pop af
    pop af
    ldh [rSVBK], a
    ret