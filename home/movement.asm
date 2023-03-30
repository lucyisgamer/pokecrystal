InitMovementBuffer::
	ld [wMovementBufferObject], a
	xor a
	ld [wMovementBufferCount], a
	ld a, $0 ; was BANK(wMovementBuffer) in G/S
	ld [wUnusedMovementBufferBank], a
	ld a, LOW(wMovementBuffer)
	ld [wUnusedMovementBufferPointer], a
	ld a, HIGH(wMovementBuffer)
	ld [wUnusedMovementBufferPointer + 1], a
	ret

DecrementMovementBufferCount::
	ld a, [wMovementBufferCount]
	and a
	ret z
	dec a
	ld [wMovementBufferCount], a
	ret

AppendToMovementBuffer::
	push hl
	push de
	ld hl, wMovementBufferCount
	ld e, [hl]
	inc [hl]
	ld d, 0
	ld hl, wMovementBuffer
	add hl, de
	ld [hl], a
	pop de
	pop hl
	ret

AppendToMovementBufferNTimes::
	push af
	ld a, c
	and a
	jr nz, .okay
	pop af
	ret

.okay
	pop af
.loop
	call AppendToMovementBuffer
	dec c
	jr nz, .loop
	ret

ComputePathToWalkToPlayer:: ; Note: this only works if the object is less than 128 tiles away from the player. Shouldn't be a problem due to objects unloading
	push af ; Player coords are pointed to by de, object coords that need to path to player are pointed to by bc
	ld h, d
	ld l, e
; compare x coords, load left/right into top bit of d, and distance into the bottom 7 bits
	inc bc
	inc hl
	ld a, [bc] ; high x coord of the object trying to reach the player
	sub a, [hl] ; follower - player
	ld e, a ; low byte of difference is in e

	dec bc
	dec hl
	ld a, [bc]
	sbc a, [hl]
	ld d, a ; de now contains the difference between the follower's x pos and the player's

	jr nc, .left ; follower needs to move left to get to player
	dec de ; decrement and complement de to invert it
	ld a, d
	cpl
	ld d, a
	ld a, e
	cpl
	ld e, a ; fallthrough to .right
	
.right
	ld d, e
	set $07, d ; set top bit of d to indicate that we need to move right to get to the player
	push de
	jr .checkY

.left
	ld d, e
	res $07, d
	push de

.checkY
	inc hl ; bruh
	inc hl ; i
	inc hl ; want
	inc bc ; to
	inc bc ; fucking
	inc bc ; die

	ld a, [bc]
	sub a, [hl] ; follower - player y
	ld e, a

	dec bc
	dec hl
	ld a, [bc]
	sbc a, [hl]
	ld d, a

	jr nc, .up ; follower needs to move up to get to player
	dec de
	ld a, d
	cpl
	ld d, a
	ld a, e
	cpl
	ld e, a

.down
	res $07, e
	ld a, e
	pop de
	ld e, a
	jr .distancesFound

.up
	set $07, e
	ld a, e
	pop de
	ld e, a

.distancesFound
	bit $07, d ; this converts the data i had earlier into the format expected by the original ending of this routine, so i don't have to rewrite it
	ld h, RIGHT
	jr nz, .setRight
	ld h, LEFT
.setRight
	res $07, d

	bit $07, e
	ld l, UP
	jr nz, .setUp
	ld l, DOWN
.setUp
	res $07, e

.checkSwap
	ld e, a
; if the x distance is less than the y distance, swap h and l, and swap d and e
	cp d
	jr nc, .done
	ld a, h
	ld h, l
	ld l, a
	ld a, d
	ld d, e
	ld e, a

.done
	pop af
	ld b, a
; Add movement in the longer direction first...
	ld a, h
	call .GetMovementData
	ld c, d
	call AppendToMovementBufferNTimes
; ... then add the shorter direction.
	ld a, l
	call .GetMovementData
	ld c, e
	call AppendToMovementBufferNTimes
	ret

.GetMovementData:
	push de
	push hl
	ld l, b
	ld h, 0
	add hl, hl
	add hl, hl
	ld e, a
	ld d, 0
	add hl, de
	ld de, .MovementData
	add hl, de
	ld a, [hl]
	pop hl
	pop de
	ret

.MovementData:
	slow_step DOWN
	slow_step UP
	slow_step LEFT
	slow_step RIGHT
	step DOWN
	step UP
	step LEFT
	step RIGHT
	big_step DOWN
	big_step UP
	big_step LEFT
	big_step RIGHT
