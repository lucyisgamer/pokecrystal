DEF CHARBLOCK_SIZE EQU $40
DEF BLOCKSET_START_BANK EQU $101
DEF CHARBLOCK_START_BANK EQU $105

CopyBlocksetIDs::

    ld a, [wNewChunkFlags]
    and a, $0F
    ret z ; bail if we don't need to do anything

    ld a, [wChunkHeader + 5] ; get second byte of blockset address
    ld l, a
    ld a, [wChunkHeader + 4] ; get first byte of blockset address
    ld b, a
    and a, %00111111 ; mask out the bank bits for the blockset
    add a, %01000000
    ld h, a ; hl now has the address of the blockset
    ld a, b
    and a, %11000000 ; mask in the bank bits
    rlca
    rlca
    add a, LOW(BLOCKSET_START_BANK)
    ld c, a
    ld a, HIGH(BLOCKSET_START_BANK)
    ld b, a

    ld a, BANK(sCharblockLUT)
    call OpenSRAM
    ld de, sCharblockLUT
    ld a, [wChunkHeader + 6]
    sla a ; each entry is two bytes
    call ReallyFarCopyBytes
    call CloseSRAM

    ld hl, wNewChunkFlags
    ld a, [hl]
    and a, $0F
    ld d, a
    ld a, [hl]
    and a, $F0
    ld [hli], a ; clear the flag that requested this function

    swap d
    ld a, [hl]
    or a, d
    ld [hl], d ; set the flag to signal the next function to start
    ret

; oh hey look it's the next function
ResolveCharblockLUT::
    ld a, [wNewChunkFlags + 1]
    and a, $F0
    ret z

    ld a, BANK(sCharblockLUT)
    call OpenSRAM

    ld a, [sCharblockResolveCounter]

    ld c, a
    ld b, $00
    ld hl, sCharblockLUT
    add hl, bc
    add hl, bc ; hl now points to the charblock we need to fuck with
    
    ld a, [hli]
    ld c, [hl] ; bc now has the id of the charblock we need to fuck with
    ld b, a
    ld hl, sCharblockIDs
.searchLoop
    ld a, [hli]
    cp a, b
    jr nz, .next
    ld a, [hli]
    cp a, c
    jr z, .match
.next
    bit $02, h ; test if we are at the end of sCharblockIDs
    jr z, .searchLoop

.insert ; the block isn't in the list at all, we need to insert the index
    ld hl, sUsedCharblockFlags - 1
    ld e, $FF ; e has the block index
.insertLoop
    inc hl
    inc e
    ld a, [hl]
    and a, $F0 ; if this nybble is empty, the slot is unused
    jr z, .open
    inc e
    ld a, [hl]
    and a, $0F
    jr nz, .insertLoop
.open
    ld a, [wNewChunkFlags + 1]
    and a, $F0
    bit $0, e
    jr z, .noSwapInsert
    swap a
.noSwapInsert
    or a, [hl]
    ld [hl], a
.insertIndex
    ld d, $00
    ld hl, sCharblockIDs
    add hl, de
    add hl, de

    ld a, [hl] ; save our old id so we know if we need derefrence it or not
    ld [hl], b
    inc hl
    ld b, [hl] ; ab now holds our old id, if it's $FFFF we shouldn't derefrence it
    ld [hl], c
    cp a, b
    call nz, DerefrenceOldBlock
    inc a
    call nz, DerefrenceOldBlock
.setDirtyBit
    ld a, e
    and a, %00000111
    ld d, a ; d holds which bit we need to set
    inc d
    ld a, $80
.dirtyLoop
    rlca
    dec d
    jr nz, .dirtyLoop
    ld hl, sDirtyCharblockFlags
    ld d, $00
    ld b, e
    srl e
    srl e
    srl e
    add hl, de
    or a, [hl]
    ld [hl], a ; put the dirty flag into the right place

    ld hl, sCharblockLUT
    ld a, [sCharblockResolveCounter]
    ld e, a
    add hl, de
    ld [hl], b
    jr .end

.match
    dec hl
    rr h
    rr l ; l has the index in sCharblockIDs that matches the id we're trying to insert
    ld b, l
    ld a, [sCharblockResolveCounter]
    ld hl, sCharblockLUT
    add a, l
    ld l, a
    ld [hl], b
    
    ld d, $00
    ld a, [wNewChunkFlags + 1] ; add refrence flag for this block
    and a, $F0
    ld hl, sUsedCharblockFlags
    ld e, b
    srl e
    jr nc, .noSwapMatch
    swap a
.noSwapMatch
    add hl, de
    or a, [hl]
    ld [hl], a
    
.end
    ld a, [sCharblockResolveCounter]
    inc a
    ld b, a
    ld a, [wChunkHeader + 6]
    cp a, b
    jr z, .finished
    ld a, b
    ld [sCharblockResolveCounter], a
    call CloseSRAM
    scf
    ret

.finished
    ld a, [wNewChunkFlags + 1]
    and a, $F0
    swap a
    ld [wNewChunkFlags + 1], a
    xor a
    ld [sCharblockResolveCounter], a
    call CloseSRAM
    ret

DerefrenceOldBlock:: ; derefrence the tiles in block e
    push de ; de is the only register pair this function needs to save
    
    ld a, BANK(sCharblockData) ; switch to the charblock data bank
    call OpenSRAM
    ldh a, [rSVBK] ; save our current WRAM bank
    push af
    ld a, BANK(wDecompressedCharblockBuffer)
    ldh [rSVBK], a
    swap e ; we're about to perform swapmagic to shift by 4
    ld a, e
    and a, $0F
    ld d, a
    ld a, e
    and a, $F0
    ld e, a
    
    ld hl, wDecompressedCharblockBuffer
    ld d, h
    ld bc, $0022 ; we need $20 bytes to store the tile slots plus 2 more as a terminator
    xor a
    dec a
    call ByteFill
    
    ld hl, sCharblockAttributes ; we want the high bit first
    add hl, de ; hl now points to the start of the block we're fucking with
.loop
    ld e, LOW(wDecompressedCharblockBuffer) ; the buffer doesn't cross a $100 boundary
    bit 3, [hl]
    jr z, .bank0
    inc b ; bytefill zeros bc, so we can skip rezeroing it
.bank0
    res 4, h ; load from the tile ids instead
    ld a, [hli] ; it's a cycle faster to use a and a subtraction instead of loading straight to c
    set 4, h ; switch back to the attributes
    sub a, $80 ; deal with tile ids starting at $80*
    ld c, a ; bc now has the tile slot we need to check for

.search
    ld a, [de]
    cp a, $FF
    jr z, .newSlot ; we don't need to worry about overflow because there can only ever be 16 tiles per block
    inc e ; incrementing here allows us to save one inc and one dec
    cp a, b ; check if we have already found this slot id
    jr nz, .next ; no? then try the next slot (we will ALWAYS find an empty slot)
    ld a, [de]
    cp a, c
    jr z, .existing
.next
    inc e
    jr .search ; we will always find an $FF byte eventually due to the terminator stuck to the end

.newSlot
    ld a, b ; put the slot id into the list
    ld [de], a
    ld a, c
    inc e
    ld [de], a
.existing
    ld a, l
    and a, $0F ; this is very clever. since blocks are aligned to $10, the bottom 4 bits will only ever be 0
    jr nz, .loop ; if we have moved to a new block.
; the list is complete

    ld a, BANK(sTileRefrenceCounts)
    call OpenSRAM ; switch back to our original SRAM bank

    ld e, LOW(wDecompressedCharblockBuffer) ; we still haven't touched d
.decrementLoop
    ld h, HIGH(sTileRefrenceCounts - $80) ; account for the $80 offset in tile slots
    ld a, [de]
    add a, h ; calculate our high address byte
    ld h, a
    inc e
    ld a, [de]
    ld l, a ; the tile slot already corresponds to the address of it's refrence count
    dec [hl]
    ; call z, CheckAnimated ; if we've completely derefrenced a tile, clear out it's animation data, if any
    inc e
    ld a, [de]
    inc a
    jr nz, .decrementLoop ; if the top byte of the next item isn't $FF, we still have items to process

    pop af
    ldh [rSVBK], a ; restore our WRAM bank
    xor a
    dec a
    pop de
    ret

CheckAnimated::
    dec e
    ld a, [de]
    inc e
    ld b, a ; b has the tile we need to check
    cpl
    and a, %11111100 ; if z flag is set our tile is animated
    ret nz ; bail if it isn't

    push de
    ld de, $10000 - sCharblockLUTEnd
    add hl, de ; hl now has our tile slot ID

    swap l ; swapmagic time
    ld a, l
    and a, $F0
    or a, h ; the lowest bit of h determines what VRAM bank this goes into, we shove it into the bottom bit of l
    ld d, a ; the VBLANK routine uses that lower bit to switch banks really fast
    swap h
    ld a, l
    and a, $0F
    or a, h
    ld h, a
    ld l, d ; retrieve our touched value for l

    ld de, vTiles1 ; start of background tiles (technically font tiles, but IDs already have a $80 offset)
    add hl, de ; hl now has the VRAM destination
    ld d, h
    ld e, l
    ld hl, sTileAnimationTables - 3
.search
    inc l
    inc l
    inc l
if DEF(_DEBUG)
	ld a, l
	cp a, LOW(sTileAnimationTablesEnd)
	jr z, .done ; @!#?@! we didn't find an active animation table for this tile?! bailing I guess
endc
    ld a, [hli]
    cp a, d
    jr nz, .search ; if we're not in debug mode not finding an active table will hang. too bad!
    ld a, [hl]
    cp a, e
    jr nz, .search

    dec l ; found the slot, clear it out
    dec a
    ld [hli], a
    ld [hli], a
    ld [hli], a
    ld [hli], a
.done
    pop de
    ret

ApplyCharblockLUT::
    ld a, [wNewChunkFlags + 1]
    and a, $0F
    ret z

    ld b, $04
.search
    dec b
    rra
    jr nc, .search
    ld a, b ; a now has the chunk quadrant we're working on

    ld de, wOverworldMapBlocks
    bit 0, a
    jr z, .left
    set 4, e
.left
    bit 1, a
    jr z, .top
    set 1, d
.top

    ld a, BANK(sCharblockLUT)
    call OpenSRAM ; open wide sram

    ld h, HIGH(sCharblockLUT) ; sCharblockLUT is also aligned to $100
    ld b, $00 ; now that we can't use the address as a loop counter, we need a separate one
.loop
    inc b
    jr z, .done
    ld a, [de]
    ld l, a
    ld a, [hl]
    ld [de], a
    inc de
    ld a, e
    and a, $0F ; interlacing time
    jr nz, .loop

    ld a, e
    add a, $10
    ld e, a
    ld a, d
    adc a, $00
    ld d, a
    jr .loop

.done
    call CloseSRAM
    xor a
    ld [wNewChunkFlags + 1], a
    ret
    
CopyCharblock::
    ldh a, [rSVBK]
    push af
    ld a, BANK(wDecompressedCharblockBuffer)
    ldh [rSVBK], a ; switch WRAM bank

    ld a, [wCharblockBufferID]
    inc a
    jp nz, .finished
    ld a, [wCharblockBufferID + 1]
    inc a
    jp nz, .finished

.start
    ld a, BANK(sDirtyCharblockFlags)
    call OpenSRAM ; switch SRAM bank

    ld hl, sDirtyCharblockFlags
    ld e, $00 ; e holds the index of whatever charblock we're fucking with
.outerLoop
    ld d, $08
    ld a, [hli]
.searchLoop
    rrca
    jr c, .found
    inc e
    jp z, .noMore
    dec d
    jr z, .outerLoop
    jr .searchLoop

.found ; we've found a charblock that needs rescuing
    ld d, $00
    ld a, e
    ld [wCharblockBufferSlotID], a
    ld hl, sCharblockIDs
    add hl, de
    add hl, de
    ld a, [hli]
    ld l, [hl] ; hl now has the charblock id we need to get
    ld c, a
    ld [wCharblockBufferID], a
    ld a, l
    ld [wCharblockBufferID + 1], a ; save our id for laterz

    ld a, l ; this is just a fancy way to calculate the address and bank offset for our charblock
    scf ; it's twice as fast as just shifting
    rra
    srl a
    ld h, a
    ld a, l
    rrca
    rrca
    and a, %11000000
    ld l, a
    ld a, c
    
    ld bc, CHARBLOCK_START_BANK
    add a, c
    ld c, a
    ld de, wDecompressedCharblockBuffer
    ld a, CHARBLOCK_SIZE
    and a ; clear carry flag
    call ReallyFarCopyBytes
    ld a, LOW(wDecompressedCharblockTileIDs)
    ld [wCharblockTilePointer], a

.finished
    call CloseSRAM
    pop af
    ldh [rSVBK], a ; return to our old wram bank
    xor a ; make sure carry flag is clear and z flag is set
    ret

.noMore ; it's time to STOP
    call CloseSRAM
    pop af
    ldh [rSVBK], a ; return to our old wram bank
    ld a, $01 ; ensure anding a clears the z flag
    and a ; make sure carry flag is clear
    ret

ResolveCharblockTiles::
    ldh a, [rSVBK]
    push af
    ld a, BANK(wCharblockBufferID)
    ldh [rSVBK], a
    ld a, BANK(sTileIDLUT)
    call OpenSRAM
    ld a, [wCharblockBufferID] ; check that wCharblockBufferID isn't $FFFF
    inc a
    jr nz, .start
    ld a, [wCharblockBufferID + 1]
    inc a
    jp z, .finished
.start
    ld b, HIGH(wDecompressedCharblockTileIDs)
    ld a, [wCharblockTilePointer]
    ld c, a
    ld a, [bc]
    inc c
    ld d, a
    ld a, [bc]
    ld e, a ; de has the tile id we are searching for
    
    ld hl, sTileIDLUT
.search ; check if the tile ID is already in the LUT
    ld a, [hli]
    cp a, d
    jr nz, .next
    ld a, [hl]
    cp a, e
    jr z, .existing ; it is, update the reference count and copy back the index
.next
    inc hl
    ld a, h
    cp a, HIGH(sAnimatedTileIDLUTEnd) ; this search checks both regular and animated tiles
    jr nz, .search
    ld a, d
    and a, %11111100
    cp a, %11111100
    jr z, .animatedNew ; if a tile is animated it gets inserted into a different place

.new ; the tile isn't already in the LUT, insert it
    ld hl, sTileRefrenceCounts ; animated tiles have their extra data populated by TileDMA
.newLoop
    ld a, [hli]
    and a
    jr z, .slotFound
if DEF(_DEBUG) ; if we're in debug mode alert if we run out of tile slots
    ld a, h
    cp a, HIGH(sTileRefrenceCounts)
    jr nz, .newLoop
    ld a, l
    cp a, LOW(sTileRefrenceCountsEnd)
    jr nz, .newLoop
    ld b, b
    stop
else
    jr nz, .newLoop ; if not, don't bother bounds checking
endc

.animatedNew ; the tile isn't already in the LUT, insert it
    ld hl, sAnimatedTileRefrenceCounts ; animated tiles have their extra data populated by TileDMA
.animateNewLoop
    ld a, [hli]
    and a
if DEF(_DEBUG) ; if we're in debug mode alert if we run out of tile slots
    jr z, .slotFound
    ld a, h
    cp a, HIGH(sAnimatedTileRefrenceCounts)
    jr nz, .animateNewLoop
    ld a, l
    cp a, LOW(sAnimatedTileRefrenceCountsEnd)
    jr nz, .animateNewLoop
    ld b, b
    stop
else
    jr nz, .animateNewLoop ; if not, don't bother bounds checking
endc

.slotFound
    dec hl
    ld a, h ; the low byte of hl is already adjusted, so we don't need to mess with it
    sub a, HIGH(sTileRefrenceCounts - $80)
    ld h, a
    call PutSlotIDAndUpdateRefrenceCount
.insertID
    push hl ; hl has the tile slot
    add hl, hl
    ld a, h
    add a, HIGH(sTileIDLUT - $100) ; tile numbers start at $80 (becomes $100 when shifted left)
    ld h, a
    ld [hl], d
    inc l
    ld [hl], e
    pop hl

.markDirty
    ld a, l
    and a, %00000111
    ld d, a
    
    ld a, l
REPT 3 ; divide hl by 8 to convert from bytes to bits
    srl h
    rra
ENDR
    ld l, a
    inc d
    ld a, %10000000
.dirtyLoop
    rlca
    dec d
    jr nz, .dirtyLoop
    ld de, wOutdatedTileFlags - ($80 / 8) ; tile numbers start at $80
    add hl, de
    or a, [hl]
    ld [hl], a
    jr .checkDone

.existing
    dec hl
    ld a, l
    sub a, LOW(sTileIDLUT - $100) ; accout for tile slots starting at $80
    ld l, a
    ld a, h
    sbc a, HIGH(sTileIDLUT - $100) ; this is an elegant way to get the slot number
    ld h, a
    srl h
    rr l ; divide by 2
    call PutSlotIDAndUpdateRefrenceCount
.checkDone
    ld hl, wDecompressedCharblockTileLength
    dec [hl]
    jr z, .finished
    inc c
    ld a, c
    ld [wCharblockTilePointer], a
    call CloseSRAM
    pop af
    ldh [rSVBK], a
    scf
    ret

.finished
    call PutCharblock
    call CloseSRAM
    
    ld a, $FF
    ld hl, wCharblockBufferID
    ld [hli], a
    ld [hl], a
    pop af
    ldh [rSVBK], a
    and a
    ret
    

PutSlotIDAndUpdateRefrenceCount:
    ld a, l
    ld [bc], a ; put the slot id where the tile id was
    dec c
    ld a, h
    rlca
    rlca
    rlca ; set up the top byte of the tile slot for easy merging of attributes
    ld [bc], a
    inc c
    push de
    ld d, h
    ld e, l
    ld hl, sTileRefrenceCounts - $80 ; tile numbers start at $80
    add hl, de
    inc [hl]
    ld h, d
    ld l, e
    pop de
    ret
    
PutCharblock:: ; assumes we're already in the correct banks!
    ld a, BANK(sCharblockData)
    call OpenSRAM
    ld a, [wCharblockBufferSlotID]
    swap a
    ld e, a
    and a, $0F
    ld d, a
    ld a, e
    and a, $F0
    ld e, a ; faster way to shift left by 4
    ld hl, sCharblockAttributes
    add hl, de ; hl now points to the right place in sCharblockTiles
    push hl
    ld de, wDecompressedCharblockTileIndices
    ld b, HIGH(wDecompressedCharblockTileIDs)
.tileLoop
    ld c, LOW(wDecompressedCharblockTileIDs)
    ld a, [de]
    and a, $0F
    sla a
    add a, c ; bc now points to the high byte of our tile id
    ld c, a
    ld a, [bc]
    ld [hl], a ; put our high byte
    inc c
    res 4, h
    ld a, [bc]
    sub a, $80
    ld [hli], a ; and put our low byte
    set 4, h

    ld c, LOW(wDecompressedCharblockTileIDs)
    ld a, [de]
    swap a
    and a, $0F
    sla a
    add a, c ; bc now points to the high byte of our tile id
    ld c, a
    ld a, [bc]
    ld [hl], a ; put our high byte
    inc c
    res 4, h
    ld a, [bc]
    sub a, $80
    ld [hli], a ; and put our low byte
    set 4, h

    inc e
    ld a, e
    cp a, LOW(wDecompressedCharblockCollision)
    jr nz, .tileLoop
    
    pop hl
    ld de, wDecompressedCharblockAttributes
.attrLoop
    ld a, [de]
    or a, [hl]
    ld [hli], a
    inc e
    ld a, e
    cp a, LOW(wDecompressedCharblockBufferEnd)
    jr nz, .attrLoop

.done
    ld a, BANK(sDirtyCharblockFlags)
    call OpenSRAM
    ld a, [wCharblockBufferSlotID]
    ld e, a
    and a, %00000111
    srl e
    srl e
    srl e
    ld d, $00
    ld hl, sDirtyCharblockFlags
    add hl, de
    
    ld d, a
    inc d
    ld a, $7F
.bitLoop
    rlca
    dec d
    jr nz, .bitLoop
    and a, [hl]
    ld [hl], a
    ret