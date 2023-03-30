CheckTrainerBattle::
	ldh a, [hROMBank]
	push af

	;call SwitchToMapScriptsBank
	call _CheckTrainerBattle

	pop bc
	ld a, b
	rst Bankswitch
	ret

_CheckTrainerBattle::
; Check if any trainer on the map sees the player and wants to battle.

; Skip the player object.
	ld a, 1
	ld de, wMap1Object

.loop

; Start a battle if the object:
	push af
	push de

; Has a sprite
	ld hl, MAPOBJECT_SPRITE
	add hl, de
	ld a, [hl]
	and a
	jr z, .next

; Is a trainer
	ld hl, MAPOBJECT_TYPE
	add hl, de
	ld a, [hl]
	and MAPOBJECT_TYPE_MASK
	cp OBJECTTYPE_TRAINER
	jr nz, .next

; Is visible on the map
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, de
	ld a, [hl]
	cp -1
	jr z, .next

; Is facing the player...
	call GetObjectStruct
	call FacingPlayerDistance_bc
	jr nc, .next

; ...within their sight range
	ld hl, MAPOBJECT_SIGHT_RANGE
	add hl, de
	ld a, [hl]
	cp b
	jr c, .next

; And hasn't already been beaten
	push bc
	push de
	ld hl, MAPOBJECT_SCRIPT_POINTER
	add hl, de
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld e, [hl]
	inc hl
	ld d, [hl]
	ld b, CHECK_FLAG
	call EventFlagAction
	ld a, c
	pop de
	pop bc
	and a
	jr z, .startbattle

.next
	pop de
	ld hl, MAPOBJECT_LENGTH
	add hl, de
	ld d, h
	ld e, l

	pop af
	inc a
	cp NUM_OBJECTS
	jr nz, .loop
	xor a
	ret

.startbattle
	pop de
	pop af
	ldh [hLastTalked], a
	ld a, b
	ld [wSeenTrainerDistance], a
	ld a, c
	ld [wSeenTrainerDirection], a
	jr LoadTrainer_continue

TalkToTrainer::
	ld a, 1
	ld [wSeenTrainerDistance], a
	ld a, -1
	ld [wSeenTrainerDirection], a

LoadTrainer_continue::
	; call GetMapScriptsBank
	ld [wSeenTrainerBank], a

	ldh a, [hLastTalked]
	call GetMapObject

	ld hl, MAPOBJECT_SCRIPT_POINTER
	add hl, bc
	ld a, [wSeenTrainerBank]
	call GetFarWord
	ld de, wTempTrainer
	ld bc, wTempTrainerEnd - wTempTrainer
	ld a, [wSeenTrainerBank]
	call FarCopyBytes
	xor a
	ld [wRunningTrainerBattleScript], a
	scf
	ret

FacingPlayerDistance_bc::
	push de
	call FacingPlayerDistance
	ld b, d
	ld c, e
	pop de
	ret

FacingPlayerDistance::
; Return carry if the sprite at bc is facing the player,
; its distance in d, and its direction in e.

	ld hl, OBJECT_MAP_X_HIGH ; x
	add hl, bc
	push bc ; save bc for later
	ld a, [hli] ; load all 16 bits
	ld b, a
	ld a, [hli]
	ld c, a
	ld a, [hli]
	ld d, a
	ld a, [hl]
	ld e, a

	ld a, [wChunkX]
	cp a, b
	jr nz, .badY
	ld a, [wChunkX + 1]
	cp a, c
	jr z, .CheckY ; fallthrough

.badY:
	ld a, [wYCoord]
	cp a, d
	jr nz, .badX
	ld a, [wYCoord + 1]
	cp a, e
	jr z, .CheckX ; fallthrough and escape

.badX:
	and a ; if none of the coordinates match then we know for sure the object isn't facing the player
	ret

.CheckY:
	ld a, [wYCoord]
	ld h, a
	ld a, [wYCoord + 1]
	ld l, a
	ld a, d
	cpl
	ld d, a
	ld a, e
	cpl
	ld e, a
	inc de
	add hl, de ; subtract the sprite's y coord from wYCoord
	jr nc, .Above

; Below
	ld a, h
	cpl
	ld h, a
	ld a, l
	cpl
	ld l, a
	inc hl
	ld d, l
	ld e, OW_UP
	jr .CheckFacing

.Above:
	ld d, l
	ld e, OW_DOWN
	jr .CheckFacing

.CheckX:
	ld a, [wYCoord]
	ld h, a
	ld a, [wYCoord + 1]
	ld l, a
	ld a, b
	cpl
	ld b, a
	ld a, c
	cpl
	ld c, a
	inc bc
	add hl, bc ; subtract the sprite's y coord from wYCoord
	jr nc, .Left

; Below
	ld a, h
	cpl
	ld h, a
	ld a, l
	cpl
	ld l, a
	inc hl
	ld d, l
	ld e, OW_LEFT
	jr .CheckFacing

.Left:
	ld d, l
	ld e, OW_RIGHT
	jr .CheckFacing

.CheckFacing:
	pop bc
	call GetSpriteDirection
	cp e
	jr nz, .NotFacing
	scf
	ret

.NotFacing:
	and a
	ret

PrintWinLossText::
	ld a, [wBattleType]
	cp BATTLETYPE_CANLOSE
	; code was probably dummied out here
	jr .canlose

; unused
	ld hl, wWinTextPointer
	jr .ok

.canlose
	ld a, [wBattleResult]
	ld hl, wWinTextPointer
	and $f ; WIN?
	jr z, .ok
	ld hl, wLossTextPointer

.ok
	ld a, [hli]
	ld h, [hl]
	ld l, a
	; call GetMapScriptsBank
	call FarPrintText
	call WaitBGMap
	call WaitPressAorB_BlinkCursor
	ret
