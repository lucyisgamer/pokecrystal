OpenSRAM:: ; Enable and switch to SRAM bank a. if invalid bank, sram is disabled
	; TODO - update to switch to far sram banks
	cp NUM_SRAM_BANKS
	jr c, .valid
if DEF(_DEBUG)
	push af
	push bc
	ld b, 1
.loop
	sla b
	dec a
	jr nz, .loop
	ld a, BANK(sOpenedInvalidSRAM)
	call OpenSRAM
	ld a, [sOpenedInvalidSRAM]
	or b
	ld [sOpenedInvalidSRAM], a
	pop bc
	pop af
endc
	jr CloseSRAM

.valid:
; switch to sram bank a
	push af
; latch clock data
	ld a, 1
	ld [MBC3LatchClock], a
	nop
	nop
	nop
	nop ; extra delay to ensure we don't fucky the timer registers due to double speed mode
; enable sram/clock write
	ld a, SRAM_ENABLE
	ld [MBC3SRamEnable], a
; select sram bank
	pop af
	ld [MBC3SRamBank], a
	ldh [hSRAMBank], a
	ret

CloseSRAM::
	push af
	ld a, SRAM_DISABLE
; reset clock latch for next time
	ld [MBC3LatchClock], a
	nop
	nop
	nop
	nop
; disable sram/clock write
	ld [MBC3SRamEnable], a
	pop af
	ret
