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
    ldh a, [hCGB] ; ensure the game doesn't crash if we're using a DMG
    and a
    ret z

    ldh a, [rLY]
    cp a, 149
    ret nc ; if we're too far in VBlank this will overrun into rendering and cause *problems*

    ldh a, [rHDMA5]
	and a, $80
    ret z ; shit! we have an ongoing DMA transfer! abort!
    ; normally i'd be worried about interrupts here, but vblank already disables interrupts
    ldh a, [rVBK]
    push af ; save our VBK
    ld a, $01
    ldh [rVBK], a

    ldh a, [hSPBuffer]
    ld h, a
    ldh a, [hSPBuffer + 1]
    ld l, a
    push hl

    ld [hSPBuffer], sp ; weirdly enough, this stores sp as little-endian. interesting.
    ld sp, sTileAnimationTables ; time for some fun pop slides

    ld a, HIGH(vTiles5 + $60 tiles) ; dma destionation registers keep their contents between transfers, so we only have to initialize once
    ldh [rHDMA3], a
    ld a, LOW(vTiles5 + $60 tiles)
    ldh [rHDMA4], a

    ld hl, rHDMA1
    ld de, sTileAnimationTables
    ld c, LOW(rHDMA5)
    ld b, $20
.loop
    pop de
    ld a, d
    ld [hli], a ; write source high
    ld a, e
    ld [hld], a ; write source low
    xor a
    ldh [c], a ; start DMA
    dec b
    jr nz, .loop
    ldh a, [hSPBuffer]
    ld l, a
    ldh a, [hSPBuffer + 1]
    ld h, a
    ld sp, hl ; bring back the stack! NO

    pop hl ; bring back the old value for hSPBuffer
    ld a, h ; maybe this will keep the pause menu from shitting itself
    ldh [hSPBuffer], a
    ld a, l
    ldh [hSPBuffer], a

    pop af
    ldh [rVBK], a ; restore VBK
    reti