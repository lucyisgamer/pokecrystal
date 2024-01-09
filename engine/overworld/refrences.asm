DEF BLOCK_START_BANK EQU $101

CopyBlocksetIDs::

    ld a, [wNewChunkFlags]
    and a, $0F
    ret z ; bail if we don't need to do anything
;     ld de, $10FF
; .search
;     rrc d ; d is used to figure out which chunk needs it's flag cleared
;     inc e ; e is which chunk quadrant we need to fuck with
;     sra a
;     jr nc, .search ; find the first chunk that is requesting a load
;     push de

;     ld a, e ; now we need to get the header of the chunk we're messing with
;     sla a
;     ld hl, wChunkCoordsArray
;     add a, l
;     ld l, a ; hl now points to the coordinates of our chunk we're fucking with
;     ld a, [hli]
;     ld [wChunkX], a
;     ld a, [hl]
;     ld [wChunkY], a
;     call CopyChunkHeader

    ld a, BANK(sCharblockLUT)
    call OpenSRAM
    
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
    add a, LOW(BLOCK_START_BANK)
    ld c, a
    ld a, HIGH(BLOCK_START_BANK)
    ld b, a
    
    ld de, sCharblockLUT
    ld a, [wChunkHeader + 6]
    call ReallyFarCopyBytes
    call CloseSRAM

    pop de ; d has the correct flag we need to update
    ld a, d
    cpl
    ld e, a

    ld hl, wNewChunkFlags
    ld a, [hl]
    and a, e
    ld [hli], a ; clear the flag that requested this function

    swap d
    ld a, [hl]
    or a, d
    ld [hl], d ; set the flag to signal the next function to start
    ret
; oh hey look it's the next function
ResolveCharblockLUT::
    ld a, [wNewChunkFlags + 1]
    
