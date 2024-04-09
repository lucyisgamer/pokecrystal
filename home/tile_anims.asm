AnimateTiles::
    ld a, BANK(sTileAnimationTables)
    call OpenSRAM
    ld hl, sTileAnimationTables
    ld a, l
.loop
    push hl
    call AnimateSingleTile
    pop hl
    add a, $04
    ld l, a
    cp a, LOW(sTileAnimationTablesEnd)
    jr nz, .loop
    xor a
    ldh [rVBK], a
    ret

AnimateSingleTile:: ; hl points to the tile animation table entry
    ld b, [hl] ; destination
    inc l
    ld c, [hl]
    inc l
    ld d, [hl] ; source
    inc l
    ld e, [hl]
    ldh a, [hTileAnimFrame]
    ld l, a
    ld h, $00
    add hl, hl ; pointers are 16 bit
    add hl, de
    ld a, [hli] ; actual destination pointers, stored in wrong endianess (saves a cycle)
    ld b, [hl]
    ld c, a
    ldh [rVBK], a

; Uses GDMA to copy a tile, must be called in VBLANK
GDMATile:: ; source in bc, destination in de
    ld a, b
    ldh [rHDMA1], a
    ld a, c
    ldh [rHDMA2], a
    ld a, d
    ldh [rHDMA3], a
    ld a, e
    ldh [rHDMA4], a
    xor a
    ldh [rHDMA5], a
    ret