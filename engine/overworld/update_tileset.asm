UpdateBlockset:: ; this function is SLOW
    ldh a, [rSVBK] ; this will be the single biggest performance hog for this game
    push af
    ld a, BANK(wCharblockLUT)
    ldh [rSVBK], a ; switch WRAM bank
    ldh a, [hROMBank]
    push af
    ld hl, wChunkHeader + BLOCKSET_ID
    ld a, [hli]
    ld e, [hl]
    ld d, a

    ld a, [wChunkHeader + BLOCKSET_SIZE]
    ld c, a ; c is tracking how many more charblocks we need to get through

.outerLoop
    ld hl, wCharblockLUT
    ld b, $FF ; b has the slot id the charblock goes into
.innerLoop
    call CheckUnused ; if the current slot we're checking is the first unused one we've reached, set b to be index to that slot
    ld a, [hli]
    cp a, d
    jr nz, .continue
    ld a, [hl]
    cp a, e
    jr nz, .continue
    ld b, l
    ld a, h
    rra
    rr b
    jr .endInnerUsed

.continue
    inc hl
    cp h, HIGH(wCharblockLUTEnd)
    jr nz, .innerLoop
    cp l, LOW(wCharblockLUTEnd)
    jr nz, .innerLoop ; fall through to inserting the charblock into an unused slot

.endInnerUnused ; we need to insert the chunk data into an empty slot and update all the gajillion refrences
    call UnrefrenceTiles ; this shit will take a while
    call UnpackCharblockData
    call CopyCharblockData ; this copies all the chunk data and tile ids, but doen't actually copy the tile data into

.endInnerUsed ; we already added the refrence we need, we can just move on to the next id
    call AddCharblockRefrence
    dec de
    dec c
    cp c, $FF
    jr nz, .outerLoop

.endOuter
    pop af
    ldh [rSVBK], a ; switch back to our original WRAM bank
    ret

AddCharblockRefrence::
    push hl
    push de

    ld hl, wUsedCharblockFlags ; b should have the slot number that we need to insert into
    ld e, b
    ld d, $00
    srl e
    add hl, de ; hl should now point to the slot flags we need
    ld a, b
    rra
    jr nc, .swapStart ; we swap [hl] if we are updating the flags on an odd slot.
    swap [hl]
.swapStart

    ld a, [wcChunkQuadrant]
    and a
    jr z, .topLeft
    dec a
    jr z, .topRight
    dec a
    jr z, .bottomLeft

.bottomRight
    set $04, [hl]
    jr .end

.bottomLeft
    set $05, [hl]
    jr .end

.topRight
    set $06, [hl]
    jr .end

.topLeft
    set $07, [hl]

.end
    ld a, b
    rra ; if we're pointing to an odd slot we need to swap hl back to the way it should be
    jr nc, .swapEnd
    swap [hl]
.swapEnd ; we have finished updating the usage flags
    ld h, HIGH(wChunkLoadLUT)
    ld l, c ; hl should now point to the slot within wChunkLoadLUT that we need to put the charblock index into
    ld [hl], b
    pop de
    pop hl
    ret

CheckUnused::
    push hl
    push bc
    ld c, l
    ld a, h
    rra 
    rr c ; c has the slot index we need
    srl c
    push af
    ld b, $00
    ld hl, wUsedCharblockFlags
    add hl, bc
    pop af
    ld a, [hl]
    jr nc, .swapStart
    swap a
.swapStart
    pop bc                  
    and a, $F0
    jr nz, .inUse
    cp b, $FF
    jr nz, .inUse ; if b isn't FF, we've already found an unused charblock
    ld b, c
.inUse
    pop hl
    ret

UnrefrenceTiles:: ; this assumes we are already in the correct WRAM bank. if we're not, then too bad!
    ; b is the slot id we need to unrefrence the tiles frome
    ldh a, [hSRAMBank]
    push af
    push de
    push hl
    push bc

    ld hl, wUsedTileIds
    ld bc, $0020
    ld a, $FF
    call ByteFill ; fill the id buffer with FF so we can easily tell which ids aren't used yet
    pop bc
    push bc
    
    ld a, BANK(sCharblockData)
    ld [hSRAMBank], a
    ld [MBC3SRamBank], a
    ld l, b
    ld h, $05 ; when rotated left by 5 we get $A0, which is the top half of the starting address of SRAM
    sla l ; we're gettin schwifty here
    rl h
    sla l
    rl h
    sla l
    rl h
    sla l
    rl h
    sla l
    rl h ; I wish this cpu had 16 bit shifts. oh well, this still works
    ld de, $0000
    ld a, $10
    ld [wTileRefrenceLoopCounter], a
.loop
    push hl ; hl points to the start of the tile data we need to check
    ld c, [hl]
    ld b, $00
    inc hl
    bit OAM_TILE_BANK, [hl]
    jr nz, .secondBank
    inc b ; the needed tile is in VRAM bank 1
.secondBank ; bc is the tile number we need to update
    ld hl, wUsedTileIds
    add hl, de
.innerLoop
    ld a, [hli]
    inc a
    jr z, .unusedTile
    inc hl
    jr .innerLoop
.unusedTile
    ld [hl], c
    dec hl
    ld [hl], b
    pop hl
    inc hl
    inc hl
    ld a, [wTileRefrenceLoopCoutner]
    dec a
    ld [wTileRefrenceLoopCoutner], a
    jr nz, .loop

.updateRefrences
    ld hl, wUsedTileIds
.loopRefrences
    push hl
    ld d, [hl]
    inc hl
    ld e, [hl]
    cp d, $FF ; if we find a tile with the top id byte equal to $FF, then we know we have reached the end of the list
    jr z, .end
    ld hl, wTileRefrenceCounts
    add hl, de
    dec [hl]
    pop hl
    inc hl
    inc hl
    jr .loopRefrences

.end
    pop hl
    pop bc
    pop hl
    pop de
    pop af
    ldh [hSRAMBank], a
    ld [MBC3SRamBank], a
    ret ; finally we're done!

UnpackCharblockData:: ; copies and unpacks charblock id de into wDecompressedCharblockBuffer. Doesn't actually copy the data into SRAM. Also puts the charblock id into the LUT
    push hl
    push de
    push bc
    
    ld c, b
    ld b, $00
    sla c
    rl b
    ld hl, wCharblockLUT
    add hl, bc
    ld [hl], d
    inc hl
    ld [hl], e ; the charblock id has now been put into the LUT

    ld a, d
    and a, $03 ; we can fit 10 bits worth of charblock data in a bank, so the top 6 bits of the id are used to find the bank we want
    ld b, a
    ld c, e
    ld a, d
    and a, %00111100 ; a has the bank we need to index from

    ld hl, $4000 ; start of switchable ROM bank
    add hl, bc
    ld de, wDecompressedCharblockBuffer
    ld bc, $0010
    push af
    push hl
    call FarCopyBytes ; first 16 bytes from parallel ROM bank 0
    ld bc, $0010 ; de ends up pointing to the byte after the copied region when CopyBytes is done, so we don't need to update it
    pop hl
    pop af
    inc a
    push af
    push hl
    call FarCopyBytes ; next 16 bytes from parallel ROM bank 1
    ld bc, $0010
    pop hl
    pop af
    inc a
    call FarCopyBytes ; next 16 bytes from parallel ROM bank 2

    ld hl, wDecompressedCharblockBuffer + $3F ; hl now points to the end of the decompressed charblock buffer
    ld bc, wDecompressedCharblockbuffer + $0F ; bc points to the end of the rest of the tile attributes
    dec de

.attrLoop
    and a, $F0 ; mask off the top 4 bits
    rlca ; bit 5 is now in bit 0
    rra 
    rra
    rlca
    rra
    rra ; bit 5 is now in bit 7
    srl a
    srl a
    and a, %00100111 ; mask off the bits we don't have yet
    ld [hl], a
    ld a, [bc]
    and a, %11000000
    or a, [hl]
    ld [hld], a ; attribute byte has been reconstructed and is in [hl]
    ld a, [bc]
    rla
    rla
    ld [bc] a
    ld a, [de]
    and a, $0F
    ld [de], a
    dec bc
    dec de
    dec de
    cp l, LOW(wDecompressedCharblockBuffer + $1F)
    jr nz, .attrLoop ; when this loop is done we have finished unpacking the tile ids and attribute data

    ld hl, wDecompressedCharblockBuffer + $0F ; point to the end of the unpacked tilemap data
    ld de, wDecompressedCharblockBuffer + $07 ; point to the end of the packed tilemap data

.tileLoop
    ld a, [de]
    and a, $0F
    ld [hld], a
    ld a, [de]
    swap a
    and a, $0F
    ld [hld], a
    dec de
    cp e, LOW(wDecompressedCharblockBuffer - 1)
    jr nz, .tileLoop ; when this loop is done we have finished unpacking the tilemap data

    pop bc
    pop de
    pop hl
    ret

CopyCharblockData::
    ldh a, [hSRAMBank]
    push af
    push hl 
    push de
    push bc ; b is the slot we are inserting into

    ld a, BANK(sCharblockData)
    call OpenSRAM ; remmeber to close SRAM later

    ld hl, wUsedTileIds
    ld bc, wUsedTileIdsEnd - wUsedTileIds
    ld a, $FF
    call ByteFill ; fill UsedTileIds with FF so we can easily find the end of the ids that are used

    call GetUsedTileIds
    call FindTileSlots ; we now have a proper LUT for both the tile ids and the charblock ids

    ld e, b
    ld d, $05 ; when rotated left by 5 we get $A0, which is the top half of the starting address of SRAM
    sla e ; we're gettin real schwifty now
    rl d
    sla e
    rl d
    sla e
    rl d
    sla e
    rl d
    sla e
    rl d ; de now points to the address we need to copy our charblock data into

    ld bc, wDecompressedCharblockBuffer
.loop
    ld hl, wUsedTileIds
    ld a, [bc]
    sla a ; account for the fact that the tile ids are 16 bit
    inc a ; we need the second byte of the tile id bc the attribute byte (which goes in after the tile id) gets the bank from the first byte of the tile id
    add a, l
    ld l, a
    ld a, h
    adc a, $00
    ld h, a
    ld a, [hld] ; put the low byte of the tile id into a and also get hl to point to the high bit of it
    ld [de], a ; at this point hl points to the first byte of the tile id
    inc de ; tile id byte has been copied, now time for the attribute byte

    push bc
    ld a, [bc]
    add a, #30 ; make sure we load from the attribute data
    add a, c
    ld c, a
    ld a, b
    adc a, $00
    ld b, a ; bc should now point to the attribute byte we need
    ld a, [bc]
    ld b, [hl]
    rr b
    jr nc, .firstBank
    set OAM_TIl_BANK, a ; make sure to set the bank bit if the tile we need is in the second VRAM bank
.firstBank
    ld [de], a
    inc de
    pop bc
    inc bc
    cp c, LOW(wDecompressedCharblockBuffer + $10)
    jr nz, .loop

    pop bc
    pop de
    pop hl
    call CloseSRAM
    d hl, wUsedTileIds
    ld [MBC3SRamBank], a
    ret
    
GetUsedTileIds:: ; this copies all the tile ids used by this charblock into wUsedTileIds
    ld de, wDecompressedCharblockBuffer
    ld h, HIGH(wUsedTileIds)
    ld b, h
.loop
    ld l, LOW(wUsedTileIds)
    ld c, LOW(wDecompressedCharblockBuffer + $10) ; bc points to the beginning of the tile ids
    ld a, [de]
    sla a
    add a, l ; this only works because these buffers don't cross a $100 boundary. If they did, we would have many many problem
    ld l, a
    ld a, [de]
    sla a
    add a, c
    ld c, a
    ld a, [hli]
    ld [bc], a
    inc bc
    ld a, [hl]
    ld [bc], a
    inc de
    cp e, LOW(wDecompressedCharblockBuffer + $10)
    jr nz, .loop
    ret 

FindTileSlots::
    xor a
    dec a
    ld [wUsedTileIdsEnd], a ; make sure we poison wUsedTileIdsEnd so we don't accidentally run past the end of this loop
    ld hl, wUsedTileIds
.outerLoop
    ld d, [hl]
    inc d ; incrementing and decrementing like this is faster than directly comparing d to $FF
    ret z ; if the top byte of the tile id is $FF, then we know that we have reached the end of the used tile ids and can return
    dec d
    inc hl
    ld e, [hl] ; de now has the tile id we need to find / insert
    push hl
    push de

    ld hl, wTileIdLUT
    ld bc, $0080
.loop
    call CheckEmptyTile
    ld a, [hli]
    cp a, d
    jr nz, .noMatch
    ld a, [hl]
    cp a, e
    jr nz, .noMatch
    ; we've found a match, so put the slot index into bc and get the blazes out of here
    ld bc, (-wTileIdLUT - $80)
    add hl, bc
    ld b, h
    ld c, l
    jr .endInner

.noMatch
    inc hl ; point to the next tile slot
    cp h, HIGH(wTileIdLUTEnd)
    jr nz, .loop
    cp l, LOW(wTileIdLUTEnd)
    jr nz, .loop
.endInner
    pop de
    bit $07, b ; check to see if we need to insert the tile id into an unused slot
    call nz, InsertTileId
    ld hl, wTileRefrenceCounts - $80
    add hl, bc
    inc [hl] ; increment the refrence count for the tile
    pop hl
    ld [hl], c ; we can now finally put the proper tile slot number into wUsedTileIds
    dec hl
    ld [hl], b
    inc hl
    inc hl
    jr .outerLoop


CheckEmptyTile:: ; sets the top bit of b when we have found the first empty slot and stops incrementing it.
    bit $07, b ; if bit 7 of b is set we have already found an empty slot for this run
    ret nz
    push hl
    ld hl, wTileRefrenceCounts
    add hl, bc
    ld a, [hl]
    and a
    jr nz, .inUse
    set $07, b ; flag that we have found an empty slot
    dec bc
.inUse
    inc bc
    pop hl
    ret
    
InsertTileId:: ; insert tile id de into tile slot bc. this resets the "new tile" flag in b and also marks the tile slot as needing its underlying graphics updated
    res $07, b ; clear the top bit of b so we can use it for addition and stuff
    ld hl, wTileIdLUT - $80
    add hl, bc
    ld [hl], d ; put the tile id into the LUT
    inc hl
    ld [hl], e
    xor a
    srl b ; bit shifter go brr
    rr c
    rra
    srl b
    rr c
    rra
    srl b
    rr c
    rra
    rlca
    rlca
    rlca
    ld hl, wOutdatedTileFlags - $10 ; this is to work with the $80 offset for the slot id
    add hl, bc
    push af
.loop1
    and a
    jr nz, .exit1
    rrc [hl]
    dec a
    jr .loop1
.exit1
    set $00, [hl] ; set the flag telling the VBLANK routine to 
    pop af
.loop2
    and a
    jr nz, .exit2
    rlc [hl]
    dec a
    jr .loop2
.exit2
    ret
