DEF TILES_PER_CYCLE EQU 8
DEF MOBILE_TILES_PER_CYCLE EQU 6

Get2bppViaHDMA::
	ldh a, [rLCDC]
	bit rLCDC_ENABLE, a
	jp z, Copy2bpp

	homecall HDMATransfer2bpp

	ret

Get1bppViaHDMA::
	ldh a, [rLCDC]
	bit rLCDC_ENABLE, a
	jp z, Copy1bpp

	homecall HDMATransfer1bpp

	ret

FarCopyBytesDouble_DoubleBankSwitch::
	ldh [hTempBank], a
	ldh a, [hROMBank]
	push af
	ldh a, [hTempBank]
	rst Bankswitch

	call FarCopyBytesDouble

	pop af
	rst Bankswitch
	ret

SafeHDMATransfer: ; unreferenced
	dec c
	ldh a, [hBGMapMode]
	push af
	xor a
	ldh [hBGMapMode], a
	ldh a, [hROMBank]
	push af
	ld a, b
	rst Bankswitch

.loop
; load the source and target MSB and LSB
	ld a, d
	ldh [rHDMA1], a ; source MSB
	ld a, e
	and $f0
	ldh [rHDMA2], a ; source LSB
	ld a, h
	and $1f
	ldh [rHDMA3], a ; target MSB
	ld a, l
	and $f0
	ldh [rHDMA4], a ; target LSB
; stop when c < TILES_PER_CYCLE
	ld a, c
	cp TILES_PER_CYCLE
	jr c, .done
; decrease c by TILES_PER_CYCLE
	sub TILES_PER_CYCLE
	ld c, a
; DMA transfer state
	ld a, $f
	ldh [hDMATransfer], a
	call DelayFrame
; add $100 to hl and de
	ld a, l
	add LOW($100)
	ld l, a
	ld a, h
	adc HIGH($100)
	ld h, a
	ld a, e
	add LOW($100)
	ld e, a
	ld a, d
	adc HIGH($100)
	ld d, a
	jr .loop

.done
	ld a, c
	and $7f ; pretty silly, considering at most bits 0-2 would be set
	ldh [hDMATransfer], a
	call DelayFrame
	pop af
	rst Bankswitch

	pop af
	ldh [hBGMapMode], a
	ret

UpdatePlayerSprite::
	farcall _UpdatePlayerSprite
	ret

LoadStandardFont::
	farcall _LoadStandardFont
	ret

LoadFontsBattleExtra::
	farcall _LoadFontsBattleExtra
	ret

LoadFontsExtra::
	farcall _LoadFontsExtra1
	farcall _LoadFontsExtra2
	ret

LoadFontsExtra2: ; unreferenced
	farcall _LoadFontsExtra2
	ret

DecompressRequest2bpp::
	push de
	ld a, BANK(sScratch)
	call OpenSRAM
	push bc

	ld de, sScratch
	ld a, b
	call FarDecompress

	pop bc
	pop hl

	ld de, sScratch
	call Request2bpp
	call CloseSRAM
	ret

FarCopyBytes::
; copy bc bytes from a:hl to de

	ldh [hTempBank], a
	ldh a, [hROMBank]
	push af
	ldh a, [hTempBank]
	rst Bankswitch

	call CopyBytes

	pop af
	rst Bankswitch
	ret

ReallyFarCopyBytes:: ; copy a bytes from bc:hl to de. carry flag is used as 9th length bit
	push af
	ld a, c
	ldh [hTempBank], a
	ldh a, [hROMBank]
	ld c, a
	ld a, b
	ldh [hTempBankHigh], a ; put our requested bank into the hTempBank values
	ldh a, [hROMBankHigh]
	ld b, a
	pop af
	push bc ; our top stack entry now has our old bank that we need to get back to at some point

	push af
	push hl
	ldh a, [hTempBankHigh]
	ld h, a
	ldh a, [hTempBank]
	rst BigBankswitch ; actually switch banks
	pop hl
	pop af

	ld c, a
	ld b, $00
	rl b
	call CopyBytes ; do the copy

	pop hl
	ld a, l
	rst BigBankswitch
	ret

FarCopyBytesDouble:
; Copy bc bytes from a:hl to bc*2 bytes at de,
; doubling each byte in the process.

	ldh [hTempBank], a
	ldh a, [hROMBank]
	push af
	ldh a, [hTempBank]
	rst Bankswitch

; switcheroo, de <> hl
	ld a, h
	ld h, d
	ld d, a
	ld a, l
	ld l, e
	ld e, a

	inc b
	inc c
	jr .dec

.loop
	ld a, [de]
	inc de
	ld [hli], a
	ld [hli], a
.dec
	dec c
	jr nz, .loop
	dec b
	jr nz, .loop

	pop af
	rst Bankswitch
	ret

Request2bpp::
; Load 2bpp at b:de to occupy c tiles of hl.
	ld a, h
	sub a, $80
	cp a, $20
	call nc, NotVRAM
	ld a, d
	sub a, $80
	cp a, $20
	call c, SourceVRAM


	ldh a, [hROMBank]
	push af
	ld a, b
	rst Bankswitch

	; let's try HDMA for a change

	ld b, c
.loop
	ld c, LOW(rHDMA1)
	ld a, d
	ldh [c], a
	inc c
	ld a, e
	ldh [c], a
	inc c
	ld a, h
	ldh [c], a
	inc c
	ld a, l
	ldh [c], a
	inc c

	ld a, b
	cp a, $80
	jr c, .notTooManyTiles
	ld a, $08 ; $80 shifted left by 4
	add a, d ; we only need to deal with the high byte here bc we're adding $800 ($80 tiles)
	ld d, a
	ld a, $08
	add a, h
	ld h, a
	ld a, $7F
.notTooManyTiles
	or a, $80 ; top bit set means HDMA
	ldh [c], a ; a already has the tile count from the previous lines

	jr .wait
	
.wait
	ldh a, [rHDMA5]
	and a, $80
	jr z, .wait

	ld a, b
	sub a, $80
	ld b, a
	jr nc, .loop
	pop af
	jp Bankswitch

; 	ld a, e
; 	ld [wRequested2bppSource], a
; 	ld a, d
; 	ld [wRequested2bppSource + 1], a
; 	ld a, l
; 	ld [wRequested2bppDest], a
; 	ld a, h
; 	ld [wRequested2bppDest + 1], a
; .loop
; 	ld a, c
; 	ld hl, hTilesPerCycle
; 	cp [hl]
; 	jr nc, .cycle

; 	ld [wRequested2bppSize], a
; .wait
; 	call DelayFrame
; 	ld a, [wRequested2bppSize]
; 	and a
; 	jr nz, .wait

; 	pop af
; 	ldh [hTilesPerCycle], a

; 	pop af
; 	rst Bankswitch

; 	pop af
; 	ldh [hBGMapMode], a
; 	ret

; .cycle
; 	ldh a, [hTilesPerCycle]
; 	ld [wRequested2bppSize], a

; .wait2
; 	call DelayFrame
; 	ld a, [wRequested2bppSize]
; 	and a
; 	jr nz, .wait2

; 	ld a, c
; 	ld hl, hTilesPerCycle
; 	sub [hl]
; 	ld c, a
; 	jr .loop

NotVRAM:
	ld b, b
	ret
SourceVRAM:
	ld b, b
	ret

Request1bpp::
; Load 1bpp at b:de to occupy c tiles of hl.
	ld a, h
	sub a, $80
	cp a, $20
	call nc, NotVRAM
	ld a, d
	sub a, $80
	cp a, $20
	call c, SourceVRAM

	ldh a, [hBGMapMode]
	push af
	xor a
	ldh [hBGMapMode], a

	ldh a, [hROMBank]
	push af
	ld a, b
	rst Bankswitch

	ldh a, [hTilesPerCycle]
	push af
	ld a, TILES_PER_CYCLE
	ldh [hTilesPerCycle], a

.NotMobile:
	ld a, e
	ld [wRequested1bppSource], a
	ld a, d
	ld [wRequested1bppSource + 1], a
	ld a, l
	ld [wRequested1bppDest], a
	ld a, h
	ld [wRequested1bppDest + 1], a
.loop
	ld a, c
	ld hl, hTilesPerCycle
	cp [hl]
	jr nc, .cycle

	ld [wRequested1bppSize], a
.wait
	call DelayFrame
	ld a, [wRequested1bppSize]
	and a
	jr nz, .wait

	pop af
	ldh [hTilesPerCycle], a

	pop af
	rst Bankswitch

	pop af
	ldh [hBGMapMode], a
	ret

.cycle
	ldh a, [hTilesPerCycle]
	ld [wRequested1bppSize], a

.wait2
	call DelayFrame
	ld a, [wRequested1bppSize]
	and a
	jr nz, .wait2

	ld a, c
	ld hl, hTilesPerCycle
	sub [hl]
	ld c, a
	jr .loop

Get2bpp::
; copy c 2bpp tiles from b:de to hl
	ldh a, [rLCDC]
	bit rLCDC_ENABLE, a
	jp nz, Request2bpp
	
	; fallthrough

Copy2bpp:
	ld a, h
	sub a, $80
	cp a, $20
	call nc, NotVRAM
	ld a, d
	sub a, $80
	cp a, $20
	call c, SourceVRAM

	push hl
	ld h, d
	ld l, e
	pop de

; bank
	ld a, b

; bc = c * LEN_2BPP_TILE
	push af
	swap c
	ld a, $f
	and c
	ld b, a
	ld a, $f0
	and c
	ld c, a
	pop af

	jp FarCopyBytes

Get1bpp::
; copy c 1bpp tiles from b:de to hl
	ldh a, [rLCDC]
	bit rLCDC_ENABLE, a
	jp nz, Request1bpp
	; fallthrough

Copy1bpp::
	ld a, h
	sub a, $80
	cp a, $20
	call nc, NotVRAM
	ld a, d
	sub a, $80
	cp a, $20
	call c, SourceVRAM

	push de
	ld d, h
	ld e, l

; bank
	ld a, b

; bc = c * LEN_1BPP_TILE
	push af
	ld h, 0
	ld l, c
	add hl, hl
	add hl, hl
	add hl, hl
	ld b, h
	ld c, l
	pop af

	pop hl
	jp FarCopyBytesDouble
