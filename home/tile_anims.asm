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
    ret

AnimateSingleTile:: ; hl points to the tile animation table entry
    ld a, [hli]
    ld b, a ; destination high
    ld a, [hli]
    ld c, a ; destination low
    ld d, h ; save the pointer to our table pointer
    ld e, l

    ld a, [hli] ; table high
    ld l, [hl] ; table low
    ld h, a
    ld a, [hli]
    inc a
    jr z, Rollover
    dec a
    ld d, a ; frame number high

    ldh a, [hTileAnimFrame]
    cp a, d 
    ret nz ; not the right frame, bail
    ld a, [hli]
    ld e, a ; frame number low
    ldh a, [hTileAnimFrame + 1]
    cp a, e
    ret nz

    ld a, [hli] ; source high
    ld d, [hl] ; source low
    

; Uses GDMA to copy a tile, must be called in VBLANK
GDMATile:: ; source in ad, destination in bc
    ld hl, rHDMA1
    ld [hli], a
    ld a, d
    ld [hli], a
    ld a, b
    ld [hli], a
    ld a, c
    ld [hli], a
    xor a
    ld [hl], a
    ret

Rollover::
    inc l
    ld a, [hli]
    ld [de], a
    inc e
    ld a, [hl]
    ld [de], a
    ret

TickAnimations::
    ret ; dummy this out for now
    ldh a, [rHDMA5]
	and a, $80
    ret z ; shit! we have an ongoing DMA transfer! abort!

    ld h, HIGH(rHDMA1)
    ld de, sTileAnimationTables
    ld c, LOW(rVBK)
    ld b, $01
.loop
    ld l, LOW(rHDMA1)
    ld a, [de] ; load source high
    inc e
    ld [hli], a ; write source high

    ld a, [de] ; load source low
    inc e
    ld [hli], a ; write source low (bottom bit specifies destination VBK)
    ldh [c], a ; write rVBK

    ld a, [de] ; load destination high AND low (swapped)
    inc e
    ld [hli], a ; write destination high
    ld [hli], a ; write destination low
    ld [hl], $00 ; start transfer
    dec b
    jr nz, .loop
    ret
