DEF CHARBLOCK_SIZE EQU $40
DEF BLOCKSET_START_BANK EQU $101
DEF CHARBLOCK_START_BANK EQU $201

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
    xor a
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
    sla c
    rl b ; bc now has the index of the charblock that we need to fuck with
    ld hl, sCharblockLUT
    add hl, bc ; hl now points to the charblock we need to fuck with
    
    ld b, [hl]
    inc hl
    ld c, [hl] ; bc now has the id of the charblock we need to fuck with
    ld hl, sCharblockIDs
.searchLoop
    ld a, b
    cp a, [hl]
    inc hl
    jr nz, .next
    ld a, c
    cp a, [hl]
    jr z, .match
.next
    inc hl
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
    sla e
    rl d
    ld hl, sCharblockIDs
    add hl, de
    ld [hl], b
    inc hl
    ld [hl], c
    rr d
    rr e
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
    add hl, de
    or a, [hl]
    ld [hl], a ; put the dirty flag into the right place

    ld b, e
    ld hl, sCharblockLUT
    ld a, [sCharblockResolveCounter]
    ld e, a
    add hl, de
    ld [hl], b
    jr .end

.match
    rr h
    rr l ; l has the index in sCharblockIDs that matches the id we're trying to insert
    ld b, l
    ld a, [sCharblockResolveCounter]
    ld hl, sCharblockLUT
    ld d, $00
    ld e, a
    add hl, de
    ld [hl], b

    ld a, [wNewChunkFlags + 1] ; add refrence flag for this block
    and a, $F0
    ld hl, sUsedCharblockFlags
    srl e
    jr c, .noSwapMatch
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
    add a, d ; de now points to the start of the chunk we're working on
    ld d, a ; this only works if wOverworldMapBlocks is aligned to $100

    ld a, BANK(sCharblockLUT)
    call OpenSRAM ; open wide sram

    ld h, HIGH(sCharblockLUT) ; sCharblockLUT is also aligned to $100
.loop
    ld a, [de]
    ld l, a
    ld a, [hl]
    ld [de], a
    inc e
    jr nz, .loop

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
    ld b, a
    ld a, [wCharblockBufferID + 1]
    cp a, b
    jp nz, .finished
    inc a
    jp nz, .finished

.start
    ld a, BANK(sDirtyCharblockFlags)
    call OpenSRAM ; switch SRAM bank

    ld hl, sDirtyCharblockFlags
    ld e, $00 ; e holds the index of whatever charblock we're fucking with
.outerLoop
    ld d, $07
    ld a, [hli]
.searchLoop
    rrca
    jr c, .found
    inc e
    jp z, .finished
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
    ld h, a
    ld [wCharblockBufferID], a
    ld a, l
    ld [wCharblockBufferID + 1], a ; save our id for laterz

    ld bc, CHARBLOCK_START_BANK - 1 ; calculate the bank and address that holds the charblock
REPT 6 ; log2(CHARBLOCK_SIZE)
    sla l
    rl h
    rl c
ENDR
    inc c
    ld de, wDecompressedCharblockBuffer
    ld a, CHARBLOCK_SIZE
    and a ; clear carry flag
    call ReallyFarCopyBytes

.finished
    call CloseSRAM
    pop af
    ldh [rSVBK], a ; return to our old wram bank
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
    ld a, [wCharblockBufferID]
    inc a
    jr z, .finished
    
.start
    ld bc, wDecompressedCharblockTileIDs - 1
    ld a, [bc] ; bc now points to the tile ids and a has the number of tiles this block uses
    inc bc
    push af
.loop
    ld a, [bc]
    inc bc
    ld d, a
    ld a, [bc]
    ld e, a ; de has the tile id we are searching for
    ld hl, sTileIDLUT

.search
    ld a, [hli]
    cp a, d
    jr nz, .next
    ld a, [hl]
    cp a, e
    jr z, .existing
.next
    inc hl
    ld a, h
    cp a, HIGH(sTileIDLUTEnd)
    jr nz, .search
.new
    ld hl, sTileRefrenceCounts
.newLoop
    ld a, [hli]
    and a
if DEF(_DEBUG) ; if we're in debug mode alert if we run out of tile slots
    jr z, .slotFound
    ld a, h
    cp a, HIGH(sTileRefrenceCounts) + 1
    jr nz, .newLoop
    ld b, b
    stop
    nop
else
    jr nz, .newLoop ; if not, don't bother bounds checking
endc
.slotFound
    ld a, l
    sub a, LOW(sTileRefrenceCounts - $80)
    ld l, a
    ld a, h
    sbc a, HIGH(sTileRefrenceCounts - $80)
    ld h, a
    call PutSlotIDAndUpdateRefrenceCount
.insertID
    push hl
    sla l
    rl h
    ld a, l
    add a, LOW(sTileIDLUT)
    ld l, a
    ld a, h
    adc a, HIGH(sTileIDLUT)
    ld h, a
    ld d, [hl]
    inc hl
    ld e, [hl]
    pop hl

.markDirty
    ld a, %10000000
    ld d, $00
REPT 3 ; divide hl by 8 to convert from bytes to bits
    srl h
    rr l
    rl d
ENDR
    inc d
.dirtyLoop
    rlca
    dec d
    jr nz, .dirtyLoop
    ld de, wOutdatedTileFlags
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
    srl h
    rr l ; divide by 2
    call PutSlotIDAndUpdateRefrenceCount
.checkDone
    pop af
    dec a
    jr z, .finished
    push af
    jr .loop

.finished
    ld a, BANK(sCharblockData)
    call OpenSRAM
    call PutCharblock


    ld a, $FF
    ld [wCharblockBufferID], a
    ld [wCharblockBufferID + 1], a


PutSlotIDAndUpdateRefrenceCount:
    ld a, l
    ld [bc], a ; put the slot id where the tile id was
    dec bc
    ld a, h
    rlca
    rlca
    rlca ; set up the top byte of the tile slot for easy merging of attributes
    ld [bc], a
    inc bc
    inc bc
    ld d, h
    ld e, l
    ld hl, sTileRefrenceCounts
    add hl, de
    inc [hl]
    ld h, d
    ld l, e
    ret

PutCharblock:: ; assumes we're already in the correct banks!
    ld hl, sCharblockData
    