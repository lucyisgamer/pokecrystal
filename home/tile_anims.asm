AnimateTiles::
    ld a, BANK(sTileAnimationTables)
    call OpenSRAM
    ld hl, sTileAnimationTables
    ld a, l
.loop
    push hl
    call AnimateSingleTile
    pop hl
    ld a, l
    add a, $04 ; the end of the table is $100 aligned, so a == 0 means we are done
    ld l, a
    jr nz, .loop
    xor a
    ldh [rVBK], a ; leave VBK in a known state after animating
    ret

AnimateSingleTile:: ; hl points to the tile animation table entry
    ld b, [hl] ; destination
    inc l ; entries don't cross $100 boundaries, so we can get away with only incremneting l
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
    ldh [rVBK], a ; the bottom bit of the destination pointer tells which VRAM bank to go to

; Uses GDMA to copy a tile, must be called in VBLANK
GDMATile:: ; source in ba, destination in de
    ldh [rHDMA2], a
    ld a, b
    ldh [rHDMA1], a
    ld a, d
    ldh [rHDMA3], a
    ld a, e
    ldh [rHDMA4], a
    xor a
    ldh [rHDMA5], a
    ret