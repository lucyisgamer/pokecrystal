; Functions dealing with VRAM.

DMATransfer::
; Return carry if the transfer is completed.

	ldh a, [hDMATransfer]
	and a
	ret z

; Start transfer
	ldh [rHDMA5], a

; Execution is halted until the transfer is complete.

	xor a
	ldh [hDMATransfer], a
	scf
	ret

UpdateBGMapBuffer::
; Copy [hBGMapTileCount] 16x8 tiles from wBGMapBuffer
; to bg map addresses in wBGMapBufferPointers.

; [hBGMapTileCount] must be even since this is done in pairs.

; Return carry on success.

	ldh a, [hBGMapUpdate]
	and a
	ret z

	ldh a, [rVBK]
	push af

; Relocate the stack pointer to wBGMapBufferPointers
	ld [hSPBuffer], sp
	ld hl, wBGMapBufferPointers
	ld sp, hl

; We can now pop the addresses of affected spots on the BG Map

	ld hl, wBGMapPalBuffer
	ld de, wBGMapBuffer

.next
; Copy a pair of 16x8 blocks (one 16x16 block)

rept 2
; Get our BG Map address
	pop bc

; Palettes
	ld a, 1
	ldh [rVBK], a

	ld a, [hli]
	ld [bc], a
	inc c
	ld a, [hli]
	ld [bc], a
	dec c

; Tiles
	ld a, 0
	ldh [rVBK], a

	ld a, [de]
	inc de
	ld [bc], a
	inc c
	ld a, [de]
	inc de
	ld [bc], a
endr

; We've done 2 16x8 blocks
	ldh a, [hBGMapTileCount]
	dec a
	dec a
	ldh [hBGMapTileCount], a

	jr nz, .next

; Restore the stack pointer
	ldh a, [hSPBuffer]
	ld l, a
	ldh a, [hSPBuffer + 1]
	ld h, a
	ld sp, hl

	pop af
	ldh [rVBK], a

	xor a
	ldh [hBGMapUpdate], a
	scf
	ret

WaitTop::
; Wait until the top third of the BG Map is being updated.

	ldh a, [hBGMapMode]
	and a
	ret z

	ldh a, [hBGMapThird]
	and a
	jr z, .done

	call DelayFrame
	jr WaitTop

.done
	xor a
	ldh [hBGMapMode], a
	ret

UpdateBGMap::
; Update the BG Map, in thirds, from wTilemap and wAttrmap.

	ldh a, [hBGMapMode]
	and a ; 0
	ret z

; BG Map 0
	dec a ; 1
	jr z, .Tiles
	dec a ; 2
	jr z, .Attr

; BG Map 1

	ldh a, [hBGMapAddress]
	ld l, a
	ldh a, [hBGMapAddress + 1]
	ld h, a
	push hl

	xor a ; LOW(vBGMap1)
	ldh [hBGMapAddress], a
	ld a, HIGH(vBGMap1)
	ldh [hBGMapAddress + 1], a

	ldh a, [hBGMapMode]
	push af
	cp 3
	call z, .Tiles
	pop af
	cp 4
	call z, .Attr

	pop hl
	ld a, l
	ldh [hBGMapAddress], a
	ld a, h
	ldh [hBGMapAddress + 1], a
	ret

.Attr:
	ld a, 1
	ldh [rVBK], a

	hlcoord 0, 0, wAttrmap
	call .update

	ld a, 0
	ldh [rVBK], a
	ret

.Tiles:
	hlcoord 0, 0

.update
	ld [hSPBuffer], sp

; Which third?
	ldh a, [hBGMapThird]
	and a ; 0
	jr z, .top
	dec a ; 1
	jr z, .middle
	; 2

DEF THIRD_HEIGHT EQU SCREEN_HEIGHT / 3

; bottom
	ld de, 2 * THIRD_HEIGHT * SCREEN_WIDTH
	add hl, de
	ld sp, hl

	ldh a, [hBGMapAddress + 1]
	ld h, a
	ldh a, [hBGMapAddress]
	ld l, a

	ld de, 2 * THIRD_HEIGHT * BG_MAP_WIDTH
	add hl, de

; Next time: top third
	xor a
	jr .start

.middle
	ld de, THIRD_HEIGHT * SCREEN_WIDTH
	add hl, de
	ld sp, hl

	ldh a, [hBGMapAddress + 1]
	ld h, a
	ldh a, [hBGMapAddress]
	ld l, a

	ld de, THIRD_HEIGHT * BG_MAP_WIDTH
	add hl, de

; Next time: bottom third
	ld a, 2
	jr .start

.top
	ld sp, hl

	ldh a, [hBGMapAddress + 1]
	ld h, a
	ldh a, [hBGMapAddress]
	ld l, a

; Next time: middle third
	ld a, 1

.start
; Which third to update next time
	ldh [hBGMapThird], a

; Rows of tiles in a third
	ld a, THIRD_HEIGHT

; Discrepancy between wTilemap and BGMap
	ld bc, BG_MAP_WIDTH - (SCREEN_WIDTH - 1)

.row
; Copy a row of 20 tiles
rept SCREEN_WIDTH / 2 - 1
	pop de
	ld [hl], e
	inc l
	ld [hl], d
	inc l
endr
	pop de
	ld [hl], e
	inc l
	ld [hl], d

	add hl, bc
	dec a
	jr nz, .row

	ldh a, [hSPBuffer]
	ld l, a
	ldh a, [hSPBuffer + 1]
	ld h, a
	ld sp, hl
	ret

Serve1bppRequest::
; Only call during the first fifth of VBlank

	ld a, [wRequested1bppSize]
	and a
	ret z

; Back out if we're too far into VBlank
	ldh a, [rLY]
	cp LY_VBLANK
	ret c
	cp LY_VBLANK + 2
	ret nc

; Copy [wRequested1bppSize] 1bpp tiles from [wRequested1bppSource] to [wRequested1bppDest]

	ld [hSPBuffer], sp

; Source
	ld hl, wRequested1bppSource
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld sp, hl

; Destination
	ld hl, wRequested1bppDest
	ld a, [hli]
	ld h, [hl]
	ld l, a

; # tiles to copy
	ld a, [wRequested1bppSize]
	ld b, a

	xor a
	ld [wRequested1bppSize], a

.next

rept 3
	pop de
	ld [hl], e
	inc l
	ld [hl], e
	inc l
	ld [hl], d
	inc l
	ld [hl], d
	inc l
endr
	pop de
	ld [hl], e
	inc l
	ld [hl], e
	inc l
	ld [hl], d
	inc l
	ld [hl], d

	inc hl
	dec b
	jr nz, .next

	ld a, l
	ld [wRequested1bppDest], a
	ld a, h
	ld [wRequested1bppDest + 1], a

	ld [wRequested1bppSource], sp

	ldh a, [hSPBuffer]
	ld l, a
	ldh a, [hSPBuffer + 1]
	ld h, a
	ld sp, hl
	ret

Serve2bppRequest::
; Only call during the first fifth of VBlank

	ld a, [wRequested2bppSize]
	and a
	ret z

; Back out if we're too far into VBlank
	ldh a, [rLY]
	cp LY_VBLANK
	ret c
	cp LY_VBLANK + 2
	ret nc
	jr _Serve2bppRequest

Serve2bppRequest_VBlank::
	ld a, [wRequested2bppSize]
	and a
	ret z

_Serve2bppRequest:: ; 
; Copy [wRequested2bppSize] 2bpp tiles from [wRequested2bppSource] to [wRequested2bppDest]

	ld [hSPBuffer], sp

; Source
	ld hl, wRequested2bppSource
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld sp, hl

; Destination
	ld hl, wRequested2bppDest
	ld a, [hli]
	ld h, [hl]
	ld l, a

; # tiles to copy
	ld a, [wRequested2bppSize]
	ld b, a

	xor a
	ld [wRequested2bppSize], a

.next

rept 7
	pop de
	ld [hl], e
	inc l
	ld [hl], d
	inc l
endr
	pop de
	ld [hl], e
	inc l
	ld [hl], d

	inc hl
	dec b
	jr nz, .next

	ld a, l
	ld [wRequested2bppDest], a
	ld a, h
	ld [wRequested2bppDest + 1], a

	ld [wRequested2bppSource], sp

	ldh a, [hSPBuffer]
	ld l, a
	ldh a, [hSPBuffer + 1]
	ld h, a
	ld sp, hl
	ret

AnimateTileset::
; Only call during the first fifth of VBlank

	ldh a, [hMapAnims]
	and a
	ret z

; Back out if we're too far into VBlank
	ldh a, [rLY]
	cp LY_VBLANK
	ret c
	cp LY_VBLANK + 7
	ret nc

	ldh a, [rSVBK]
	push af
	ld a, BANK(wTilesetAnim)
	ldh [rSVBK], a

	ldh a, [rVBK]
	push af

	ldh a, [hSRAMBank]
	push af

	call AnimateTiles

	ld hl, hTileAnimFrame ; somehow this is faster than using ldh
	ld a, [hl]
	add a, $01
	ld [hli], a
	ld a, [hl]
	adc a, $00
	and a, $0F
	ld [hl], a

	pop af
	call OpenSRAM

	pop af
	ldh [rVBK], a
	pop af
	ldh [rSVBK], a
	ret

TileDMA::
	ldh a, [rHDMA5]
	and a, $80
	ret z ; another dma transfer is active, abort!
	
    ldh a, [rSVBK]
    push af
    ld a, BANK(wOutdatedTileFlags)
    ldh [rSVBK], a

    ld hl, wOutdatedTileFlags
.firstSearch
    ld a, [hl]
    and a
    jr nz, .secondSearch
    inc l
    jr nz, .firstSearch
	pop af ; restore our wram bank before bailing
	ldh [rSVBK], a
    and a ; no tiles need DMA, so clear carry and exit
    ret
.secondSearch
	ldh a, [hROMBank] ; we know we need to copy a tile, save our current rom bank
	ld c, a
	ldh a, [hROMBankHigh]
	ld b, a
	push bc

	ld d, $00
    ld a, l
    sub a, LOW(wOutdatedTileFlags)
    sla a
    sla a
    sla a ; shift out our ninth bit for later
    ld e, a
    dec e ; dec weirdly doesn't touch the carry flag
    rl d ; shift that ninth bit into d
    ld a, [hl]
    ld b, $7F
.loop
    inc e
    rlc b
    rrca
    jr nc, .loop
    ld a, b
    and a, [hl]
    ld [hl], a
    ld b, d
    ld c, e ; bc has our slot we need to dma into

    sla e
    rl d
    ld hl, sTileIDLUT
    add hl, de
    ld a, BANK(sTileIDLUT)
    call OpenSRAM
    ld d, [hl]
    inc hl
    ld e, [hl] ; de now has the tle id we need to dma
    call CloseSRAM
	push de ; save our tile id for (maybe) later

.calcROMBank
    swap d ; amazingly, this is 25% faster and smaller than just shifting by 4
    swap e
    ld a, e
    and a, $0F
    or a, d ; the hardware doesn't check the bottom 4 bits of the source address
	ld d, a
    xor a
    rlc d
    rlca
    rlc d
    rlca
	inc a ; bank X00 isn't accessible bc mbc30 shenanigans
	ld l, a ; save our bank for later checking for animated tiles
	scf ; set bit 6 of d
    rr d
    srl d
    ld h, HIGH(TILES_START_BANK)
	di ; interrupts could cause us problems. best avoid them
	rst BigBankswitch
	ld a, d
	ldh [rHDMA1], a
	ld a, e
	ldh [rHDMA2], a

.calcVRAMBank
	ld d, $00
	sla c
	rl b
	ld a, b
	cp a, 1
	ccf
	rl d
	ldh a, [rVBK]
	push af
	ld a, d
	ldh [rVBK], a ; VRAM bank is now set

.calcVRAMAddr
	ld a, b
	sla c
	rla
	sla c
	rla
	sla c
	rla
	xor a, %00001000 ; flip to account for tile $00 being in the middle of VRAM
	and a, $0F

	add a, $88 ; high byte of address, $8800 is the start of tiles
	ldh [rHDMA3], a
	ld b, a ; save our high byte for easy retrieval later
	ld a, c ; low byte of address
	ldh [rHDMA4], a ; destination address now set up

.timingCheck ; starting a transfer during HBLANK causes problems
	ldh a, [rSTAT]
	and a, %00000011
	cp a, $02 ; most reliable way to check timing
	jr nz, .timingCheck
	ld a, SINGLE_TILE_DMA
	ldh [rHDMA5], a
	

.checkAnimated ; bc has our destination address, hl has the tile id
	ld a, l
	cp a, $40 ; last possible bank for tiles, reserved for animated tiles
	pop hl ; get our tile id back, this wastes a few cycles if we're not animating
	; we have to waste them anyways to wait for the transfer to finish, might as well get somethign out of it
	jr nz, .doneCheck ; if the bank value isn't $40, this tile isn't animated, so skip it
	ld a, BANK(sTileAnimationTables)
	call OpenSRAM
	ld a, h
	and a, $03 ; mask off the bank bits
	sla l
	rla
	add a, $78 ; offset for animation table pointers being stored at the end of the graphics bank
	ld h, a
	ld a, [hli]
	ld e, [hl]
	ld d, a ; de now has the pointer to our animation table
	ld hl, sTileAnimationTables - 3
.searchTableSlot
	inc l
	inc l
	inc l
if DEF(_DEBUG)
	ld a, l
	cp a, LOW(sTileAnimationTablesEnd) ; @!#?@! we're out of animation slots! dropping the last animation...
	jr z, .doneCheck
endc
	ld a, [hli]
	inc a
	jr nz, .searchTableSlot ; if we're not in debug mode then having too many animations at once will (likely) cause an infinite loop and a hang. spicy!
	dec l
	ld [hl], c ; destination pointers are stored little endian to save a cycle in the animation routine
	inc l
	ld [hl], b
	inc l
	ld [hl], d
	inc l
	ld [hl], e

.doneCheck
	ldh a, [rHDMA5]
	and a, $80
	jr z, .doneCheck
	pop af
	ldh [rVBK], a ; restore VRAM bank
	pop hl
	ld a, l
	rst BigBankswitch ; restore our ROM bank
	pop af
	ldh [rSVBK], a
	scf
	reti


DEF SINGLE_TILE_DMA EQU $80
DEF TILES_START_BANK EQU $201