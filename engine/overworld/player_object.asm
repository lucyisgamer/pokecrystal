BlankScreen:
	call DisableSpriteUpdates
	xor a
	ldh [hBGMapMode], a
	call ClearBGPalettes
	call ClearSprites
	hlcoord 0, 0
	ld bc, wTilemapEnd - wTilemap
	ld a, " "
	call ByteFill
	hlcoord 0, 0, wAttrmap
	ld bc, wAttrmapEnd - wAttrmap
	ld a, $7
	call ByteFill
	call WaitBGMap2
	call SetPalettes
	ret

SpawnPlayer::
	ld a, -1
	ld [wObjectFollow_Leader], a
	ld [wObjectFollow_Follower], a
	ld a, PLAYER
	ld hl, PlayerObjectTemplate
	call CopyPlayerObjectTemplate
	ld a, PLAYER
	call PlayerSpawn_CopyCoords
	ld a, PLAYER_OBJECT
	call GetMapObject
	ld hl, MAPOBJECT_PALETTE
	add hl, bc
	ln e, PAL_NPC_RED, OBJECTTYPE_SCRIPT
	ld a, [wPlayerSpriteSetupFlags]
	bit PLAYERSPRITESETUP_FEMALE_TO_MALE_F, a
	jr nz, .ok
	ld a, [wPlayerGender]
	bit PLAYERGENDER_FEMALE_F, a
	jr z, .ok
	ln e, PAL_NPC_BLUE, OBJECTTYPE_SCRIPT

.ok
	ld [hl], e
	ld a, PLAYER_OBJECT
	ldh [hMapObjectIndex], a
	ld bc, wMapObjects
	ld a, PLAYER_OBJECT
	ldh [hObjectStructIndex], a
	ld de, wObjectStructs
	call CopyMapObjectToObjectStruct
	ld a, PLAYER
	ld [wCenteredObject], a
	ret

PlayerObjectTemplate:
; A dummy map object used to initialize the player object.
; Shorter than the actual amount copied by two bytes.
; Said bytes seem to be unused.
	object_event $0010, $0010, SPRITE_CHRIS, SPRITEMOVEDATA_PLAYER, 15, 15, -1, -1, 0, OBJECTTYPE_SCRIPT, 0, 0, -1

CopyBCDECoordsToMapObject::
	push bc
	push de
	call GetMapObject
	pop de
	ld hl, MAPOBJECT_X_COORD_HIGH
	add hl, bc
	pop bc
	ld [hl], b
	inc hl
	ld [hl], c
	inc hl
	ld [hl], d
	inc hl
	ld [hl], e
	ret

PlayerSpawn_CopyCoords:
	push hl
	ld hl, wXCoord
	ld b, [hl]
	inc hl
	ld c, [hl]
	inc hl
	ld d, [hl]
	inc hl
	ld e, [hl]
	pop hl
	call CopyBCDECoordsToMapObject
	ret

WriteObjectXY::
	ld a, b
	call CheckObjectVisibility
	ret c

	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc
	ld b, [hl]
	inc hl
	ld c, [hl]
	inc hl
	ld d, [hl]
	inc hl
	ld e, [hl]
	ldh a, [hMapObjectIndex]
	call CopyBCDECoordsToMapObject
	and a
	ret

CopyObjectStruct::
	call CheckObjectMask
	and a
	ret nz ; masked

	ld hl, wObjectStructs + OBJECT_LENGTH * 1
	ld a, 1
	ld de, OBJECT_LENGTH
.loop
	ldh [hObjectStructIndex], a
	ld a, [hl]
	and a
	jr z, .done
	add hl, de
	ldh a, [hObjectStructIndex]
	inc a
	cp NUM_OBJECT_STRUCTS
	jr nz, .loop
	scf
	ret ; overflow

.done
	ld d, h
	ld e, l
	call CopyMapObjectToObjectStruct
	ld hl, wVramState
	bit 7, [hl]
	ret z

	ld hl, OBJECT_FLAGS2
	add hl, de
	set 5, [hl]
	ret

CopyMapObjectToObjectStruct:
	call .CopyMapObjectToTempObject
	call CopyTempObjectToObjectStruct
	ret

.CopyMapObjectToTempObject:
	ldh a, [hObjectStructIndex]
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld [hl], a

	ldh a, [hMapObjectIndex]
	ld [wTempObjectCopyMapObjectIndex], a

	ld hl, MAPOBJECT_SPRITE
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopySprite], a

	call GetSpriteVTile
	ld [wTempObjectCopySpriteVTile], a

	ld a, [hl]
	call GetSpritePalette
	ld [wTempObjectCopyPalette], a

	ld hl, MAPOBJECT_PALETTE
	add hl, bc
	ld a, [hl]
	and MAPOBJECT_PALETTE_MASK
	jr z, .skip_color_override
	swap a
	and PALETTE_MASK
	ld [wTempObjectCopyPalette], a

.skip_color_override
	ld hl, MAPOBJECT_MOVEMENT
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopyMovement], a

	ld hl, MAPOBJECT_SIGHT_RANGE
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopyRange], a

	ld hl, MAPOBJECT_X_COORD_HIGH
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopyXHigh], a

	ld hl, MAPOBJECT_X_COORD_LOW
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopyXLow], a

	ld hl, MAPOBJECT_Y_COORD_HIGH
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopyYHigh], a

	ld hl, MAPOBJECT_Y_COORD_LOW
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopyYLow], a

	ld hl, MAPOBJECT_RADIUS
	add hl, bc
	ld a, [hl]
	ld [wTempObjectCopyRadius], a
	ret

InitializeVisibleSprites:
	ld bc, wMap1Object
	ld a, 1
.loop
	ldh [hMapObjectIndex], a
	ld hl, MAPOBJECT_SPRITE
	add hl, bc
	ld a, [hl]
	and a
	jr z, .next

	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld a, [hl]
	cp -1
	jr nz, .next

	ld a, [wXCoord + 1]
	ld d, a
	ld a, [wYCoord + 1]
	ld e, a

	ld hl, MAPOBJECT_X_COORD_LOW
	add hl, bc
	ld a, [hl]
	add 1
	sub d
	jr c, .next

	cp MAPOBJECT_SCREEN_WIDTH
	jr nc, .next

	ld hl, MAPOBJECT_Y_COORD_LOW
	add hl, bc
	ld a, [hl]
	add 1
	sub e
	jr c, .next

	cp MAPOBJECT_SCREEN_HEIGHT
	jr nc, .next

	push bc
	call CopyObjectStruct
	pop bc
	jp c, .ret

.next
	ld hl, MAPOBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	ldh a, [hMapObjectIndex]
	inc a
	cp NUM_OBJECTS
	jr nz, .loop
	ret

.ret
	ret

CheckObjectEnteringVisibleRange:: ; If objects do weird shit with visibility check here first
	ld a, [wPlayerStepDirection]
	cp STANDING
	ret z
	ld hl, .dw
	rst JumpTable
	ret

.dw
	dw .Down
	dw .Up
	dw .Left
	dw .Right

.Up:
	ld a, [wYCoord + 1]
	sub 1
	jr .Vertical

.Down:
	ld a, [wYCoord + 1]
	add 9
.Vertical:
	ld d, a
	ld a, [wXCoord + 1]
	ld e, a
	ld bc, wMap1Object
	ld a, 1
.loop_v
	ldh [hMapObjectIndex], a
	ld hl, MAPOBJECT_SPRITE
	add hl, bc
	ld a, [hl]
	and a
	jr z, .next_v
	ld hl, MAPOBJECT_Y_COORD_LOW
	add hl, bc
	ld a, d
	cp [hl]
	jr nz, .next_v
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld a, [hl]
	cp -1
	jr nz, .next_v
	ld hl, MAPOBJECT_X_COORD_LOW
	add hl, bc
	ld a, [hl]
	add 1
	sub e
	jr c, .next_v
	cp MAPOBJECT_SCREEN_WIDTH
	jr nc, .next_v
	push de
	push bc
	call CopyObjectStruct
	pop bc
	pop de

.next_v
	ld hl, MAPOBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	ldh a, [hMapObjectIndex]
	inc a
	cp NUM_OBJECTS
	jr nz, .loop_v
	ret

.Left:
	ld a, [wXCoord + 1]
	sub 1
	jr .Horizontal

.Right:
	ld a, [wXCoord + 1]
	add 10
.Horizontal:
	ld e, a
	ld a, [wYCoord + 1]
	ld d, a
	ld bc, wMap1Object
	ld a, 1
.loop_h
	ldh [hMapObjectIndex], a
	ld hl, MAPOBJECT_SPRITE
	add hl, bc
	ld a, [hl]
	and a
	jr z, .next_h
	ld hl, MAPOBJECT_X_COORD_LOW
	add hl, bc
	ld a, e
	cp [hl]
	jr nz, .next_h
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld a, [hl]
	cp -1
	jr nz, .next_h
	ld hl, MAPOBJECT_Y_COORD_LOW
	add hl, bc
	ld a, [hl]
	add 1
	sub d
	jr c, .next_h
	cp MAPOBJECT_SCREEN_HEIGHT
	jr nc, .next_h
	push de
	push bc
	call CopyObjectStruct
	pop bc
	pop de

.next_h
	ld hl, MAPOBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	ldh a, [hMapObjectIndex]
	inc a
	cp NUM_OBJECTS
	jr nz, .loop_h
	ret

CopyTempObjectToObjectStruct:
	ld a, [wTempObjectCopyMapObjectIndex]
	ld hl, OBJECT_MAP_OBJECT_INDEX
	add hl, de
	ld [hl], a

	ld a, [wTempObjectCopyMovement]
	call CopySpriteMovementData

	ld a, [wTempObjectCopyPalette]
	ld hl, OBJECT_PALETTE
	add hl, de
	or [hl]
	ld [hl], a

	ld a, [wTempObjectCopyYHigh]
	ld b, a
	ld a, [wTempObjectCopyYLow]
	ld c, a
	call .InitYCoord

	ld a, [wTempObjectCopyXHigh]
	ld b, a
	ld a, [wTempObjectCopyXLow]
	ld c, a
	call .InitXCoord

	ld a, [wTempObjectCopySprite]
	ld hl, OBJECT_SPRITE
	add hl, de
	ld [hl], a

	ld a, [wTempObjectCopySpriteVTile]
	ld hl, OBJECT_SPRITE_TILE
	add hl, de
	ld [hl], a

	ld hl, OBJECT_STEP_TYPE
	add hl, de
	ld [hl], STEP_TYPE_RESET

	ld hl, OBJECT_FACING
	add hl, de
	ld [hl], STANDING

	ld a, [wTempObjectCopyRadius]
	call .InitRadius

	ld a, [wTempObjectCopyRange]
	ld hl, OBJECT_RANGE
	add hl, de
	ld [hl], a

	and a
	ret

.InitYCoord:
	ld hl, OBJECT_INIT_Y_HIGH
	add hl, de
	ld [hl], b ; b is high, c is low

	ld hl, OBJECT_INIT_Y_LOW
	add hl, de
	ld [hl], c

	ld hl, OBJECT_MAP_Y_HIGH
	add hl, de
	ld [hl], b

	ld hl, OBJECT_MAP_Y_LOW
	add hl, de
	ld [hl], c


	ld a, c
	ld hl, wYCoord + 1 ; low byte of y pos
	sub [hl]
	add a, $04
	and $f
	swap a
	ld hl, wPlayerBGMapOffsetY
	sub [hl]
	ld hl, OBJECT_SPRITE_Y
	add hl, de
	ld [hl], a
	ret

.InitXCoord:
	ld hl, OBJECT_INIT_X_HIGH
	add hl, de
	ld [hl], b

	ld hl, OBJECT_INIT_X_LOW
	add hl, de
	ld [hl], c

	ld hl, OBJECT_MAP_X_HIGH
	add hl, de
	ld [hl], b

	ld hl, OBJECT_MAP_X_LOW
	add hl, de
	ld [hl], c

	ld hl, wXCoord + 1
	sub [hl]
	add a, $04
	and $f
	swap a
	ld hl, wPlayerBGMapOffsetX
	sub [hl]
	ld hl, OBJECT_SPRITE_X
	add hl, de
	ld [hl], a
	ret

.InitRadius:
	ld h, a
	inc a
	and $f
	ld l, a
	ld a, h
	add $10
	and $f0
	or l
	ld hl, OBJECT_RADIUS
	add hl, de
	ld [hl], a
	ret

TrainerWalkToPlayer:
	ldh a, [hLastTalked]
	call InitMovementBuffer
	ld a, movement_step_sleep
	call AppendToMovementBuffer
	ld a, [wSeenTrainerDistance]
	dec a
	jr z, .TerminateStep
	ldh a, [hLastTalked]
	ld b, a
	ld c, PLAYER
	ld d, 1
	call .GetPathToPlayer
	call DecrementMovementBufferCount

.TerminateStep:
	ld a, movement_step_end
	call AppendToMovementBuffer
	ret

.GetPathToPlayer:
	push de
	push bc
; get player object struct, load to de
	ld a, c
	call GetMapObject
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld a, [hl]
	call GetObjectStruct
	ld d, b
	ld e, c

; get last talked object struct, load to bc
	pop bc
	ld a, b
	call GetMapObject
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld a, [hl]
	call GetObjectStruct

; get last talked coords pointer, load to bc
	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc
	ld b, h
	ld c, l

; get player coords pointer, load to de
	ld hl, OBJECT_MAP_X_HIGH
	add hl, de
	ld d, h
	ld e, l

	pop af
	call ComputePathToWalkToPlayer
	ret

SurfStartStep:
	call InitMovementBuffer
	call .GetMovementData
	call AppendToMovementBuffer
	ld a, movement_step_end
	call AppendToMovementBuffer
	ret

.GetMovementData:
	ld a, [wPlayerDirection]
	srl a
	srl a
	maskbits NUM_DIRECTIONS
	ld e, a
	ld d, 0
	ld hl, .movement_data
	add hl, de
	ld a, [hl]
	ret

.movement_data
	slow_step DOWN
	slow_step UP
	slow_step LEFT
	slow_step RIGHT

GetRelativeFacing:: ; Determines which way map object e would have to turn to face map object d. Returns carry if it's impossible for whatever reason. If successful, returns the facing direction in d. THIS ONLY WORKS IF THE TWO OBJECTS SHARE A COORDINATE
	ld a, d
	call GetMapObject
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld a, [hl]
	cp NUM_OBJECT_STRUCTS
	jr nc, .carry
	ld d, a
	ld a, e
	call GetMapObject
	ld hl, MAPOBJECT_OBJECT_STRUCT_ID
	add hl, bc
	ld a, [hl]
	cp NUM_OBJECT_STRUCTS
	jr nc, .carry
	ld e, a
	call .GetFacing_e_relativeto_d
	ret

.carry
	scf
	ret

.GetFacing_e_relativeto_d: ; Determines which way object e would have to turn to face object d. Returns carry if it's impossible. If successful, returns the facing direction in d
; load the pointer to the coordinates of object d into bc
	ld a, d
	call GetObjectStruct
	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc
	ld b, h
	ld c, l
	push bc
; load the pointer to the coordinates of object e into hl
	ld a, e
	call GetObjectStruct
	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc
	pop bc ; if the fucking register pressure gets any higher it's gonna start trying to sell me drugs

.checkX
	ld a, [bc] ; high byte of x coordinate of object d
	cp a, [hl] ; high byte of x coordinate of object e
	jr c, .left; a is less than hl
	jr nz, .right
	inc bc
	inc hl
	ld a, [bc]
	cp a, [hl]
	jr c, .left
	jr z, .checkY

.right
	ld d, RIGHT
	and a
	ret

.left
	ld d, LEFT
	and a
	ret

.checkY
	inc bc
	inc hl
	ld a, [bc]
	cp a, [hl]
	jr c, .up
	jr nz, .down
	inc bc
	inc hl
	ld a, [bc]
	cp a, [hl]
	jr c,  .up
	jr z, .sameXY

.down
	ld d, DOWN
	and a
	ret

.up
	ld d, UP
	and a
	ret

.sameXY
	scf
	ret

QueueFollowerFirstStep:
	call .QueueFirstStep
	jr c, .same
	ld [wFollowMovementQueue], a
	xor a
	ld [wFollowerMovementQueueLength], a
	ret

.same
	ld a, -1
	ld [wFollowerMovementQueueLength], a
	ret

.QueueFirstStep: ; returns movement_step + direction in a, sets carry if x and y are already equal
	ld a, [wObjectFollow_Leader]
	call GetObjectStruct
	ld hl, OBJECT_MAP_X_HIGH ; Leader object X
	add hl, bc
	ld b, [hl] ; bc has the x corodinate
	inc hl
	ld c, [hl]

	inc hl
	ld d, [hl] ; de has the y coordinate
	inc hl
	ld e, [hl]

	ld a, [wObjectFollow_Follower]
	push bc
	call GetObjectStruct
	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc ; hl now has a pointer to the follower object's high x coordinate
	pop bc 

	ld a, b ; leader x coord (high)
	cp a, [hl]
	jr c, .left 
	jr nz, .right
	inc hl
	ld a, c
	cp a, [hl]
	jr c, .left
	jr z, .check_y

.right ; fallthrough
	and a
	ld a, movement_step + RIGHT
	ret

.left
	and a
	ld a, movement_step + LEFT
	ret

.check_y
	inc hl
	ld a, d
	cp [hl]
	jr c, .up
	jr nz, .down
	inc hl
	ld a, e ; leader y coord (high)
	cp [hl] ; hl points to the follower's coordinates
	jr z, .same_xy
	jr c, .up

.down ; fallthrough
	and a
	ld a, movement_step + DOWN
	ret

.up
	and a
	ld a, movement_step + UP
	ret

.same_xy
	scf
	ret
