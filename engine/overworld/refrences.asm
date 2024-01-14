DEF BLOCKSET_START_BANK EQU $101


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


    
    
