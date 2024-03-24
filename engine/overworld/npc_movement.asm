CanObjectMoveInDirection:
	ld hl, OBJECT_PALETTE
	add hl, bc
	bit SWIMMING_F, [hl]
	jr z, .not_swimming

	ld hl, OBJECT_FLAGS1
	add hl, bc
	bit NOCLIP_TILES_F, [hl]
	jr nz, .noclip_tiles
	push hl
	push bc
	call WillObjectBumpIntoLand
	pop bc
	pop hl
	ret c
	jr .continue

.not_swimming
	ld hl, OBJECT_FLAGS1
	add hl, bc
	bit NOCLIP_TILES_F, [hl]
	jr nz, .noclip_tiles
	push hl
	push bc
	call WillObjectBumpIntoWater
	pop bc
	pop hl
	ret c

.noclip_tiles
.continue
	bit NOCLIP_OBJS_F, [hl]
	jr nz, .noclip_objs

	push hl
	push bc
	call WillObjectBumpIntoSomeoneElse
	pop bc
	pop hl
	ret c

.noclip_objs
	bit MOVE_ANYWHERE_F, [hl]
	jr nz, .move_anywhere
	push hl
	push bc
	call HasObjectReachedMovementLimit
	pop bc
	pop hl
	ret c

	push hl
	push bc
	call IsObjectMovingOffEdgeOfScreen
	pop bc
	pop hl
	ret c

.move_anywhere
	and a
	ret

WillObjectBumpIntoWater:
	call CanObjectLeaveTile
	ret c
	ld hl, OBJECT_MAP_X_LOW ; these get fed into 
	add hl, bc
	ld d, [hl]
	ld hl, OBJECT_MAP_Y_LOW
	add hl, bc
	ld e, [hl]
	ld hl, OBJECT_PALETTE
	add hl, bc
	bit OAM_PRIORITY, [hl]
	jp nz, WillObjectRemainOnWater
	ld hl, OBJECT_COLL
	add hl, bc
	ld a, [hl]
	ld d, a
	call GetTileCollision
	and a ; LAND_TILE
	jr z, WillObjectBumpIntoTile
	scf
	ret

WillObjectBumpIntoLand:
	call CanObjectLeaveTile
	ret c
	ld hl, OBJECT_COLL
	add hl, bc
	ld a, [hl]
	call GetTileCollision
	cp WATER_TILE
	jr z, WillObjectBumpIntoTile
	scf
	ret

WillObjectBumpIntoTile:
	ld hl, OBJECT_COLL
	add hl, bc
	ld a, [hl]
	call GetSideWallDirectionMask
	ret nc
	push af
	ld hl, OBJECT_WALKING
	add hl, bc
	ld a, [hl]
	maskbits NUM_DIRECTIONS
	ld e, a
	ld d, 0
	ld hl, .dir_masks
	add hl, de
	pop af
	and [hl]
	ret z
	scf
	ret

.dir_masks
	db DOWN_MASK  ; DOWN
	db UP_MASK    ; UP
	db RIGHT_MASK ; LEFT
	db LEFT_MASK  ; RIGHT

CanObjectLeaveTile:
	ld hl, OBJECT_LAST_COLL
	add hl, bc
	ld a, [hl]
	call GetSideWallDirectionMask
	ret nc
	push af
	ld hl, OBJECT_WALKING
	add hl, bc
	maskbits NUM_DIRECTIONS
	ld e, a
	ld d, 0
	ld hl, .dir_masks
	add hl, de
	pop af
	and [hl]
	ret z
	scf
	ret

.dir_masks
	db UP_MASK    ; DOWN
	db DOWN_MASK  ; UP
	db LEFT_MASK  ; LEFT
	db RIGHT_MASK ; RIGHT

GetSideWallDirectionMask:
	ld d, a
	and $f0
	cp HI_NYBBLE_SIDE_WALLS
	jr z, .continue
	cp HI_NYBBLE_SIDE_BUOYS
	jr z, .continue
	xor a
	ret

.continue
	ld a, d
	and $7
	ld e, a
	ld d, 0
	ld hl, .side_wall_masks
	add hl, de
	ld a, [hl]
	scf
	ret

.side_wall_masks
	db RIGHT_MASK             ; COLL_RIGHT_WALL/BUOY
	db LEFT_MASK              ; COLL_LEFT_WALL/BUOY
	db DOWN_MASK              ; COLL_UP_WALL/BUOY
	db UP_MASK                ; COLL_DOWN_WALL/BUOY
	db UP_MASK | RIGHT_MASK   ; COLL_DOWN_RIGHT_WALL/BUOY
	db UP_MASK | LEFT_MASK    ; COLL_DOWN_LEFT_WALL/BUOY
	db DOWN_MASK | RIGHT_MASK ; COLL_UP_RIGHT_WALL/BUOY
	db DOWN_MASK | LEFT_MASK  ; COLL_UP_LEFT_WALL/BUOY

WillObjectRemainOnWater:
	ld hl, OBJECT_WALKING
	add hl, bc
	ld a, [hl]
	maskbits NUM_DIRECTIONS
	jr z, .down
	dec a
	jr z, .up
	dec a
	jr z, .left
	jr .right

.down
	inc e
	push de
	inc d
	jr .continue

.up
	push de
	inc d
	jr .continue

.left
	push de
	inc e
	jr .continue

.right
	inc d
	push de
	inc e

.continue
	call GetCoordTile
	call GetTileCollision
	pop de
	and a ; LAND_TILE
	jr nz, .not_land
	call GetCoordTile
	call GetTileCollision
	and a ; LAND_TILE
	jr nz, .not_land
	xor a
	ret

.not_land
	scf
	ret

CheckFacingObject::
	call GetFacingTileCoord
	push af ; tile id is in a

	ld bc, wPlayerStruct
	call GetFacingCoords

	pop af
	call CheckCounterTile
	
	jr nz, .not_counter ; double the distance for counter tiles

	ld a, [wPlayerStruct + OBJECT_DIRECTION]
	maskbits NUM_DIRECTIONS, 2
	and a ; OW_DOWN but more fasterer
	jr z, .down
	cp OW_UP
	jr z, .up
	cp OW_LEFT
	jr z, .left

.right
	inc bc
	jr .not_counter

.down
	inc de
	jr .not_counter

.up
	dec de
	jr .not_counter

.left
	dec bc

.not_counter

	ldh a, [rSVBK]
	ld h, a
	push hl
	ld a, BANK(wTempCoordinateBuffer)
	ldh [rSVBK], a ; switch wram bank

	ld hl, wTempCoordinateBuffer ; put the coordinates into the buffer
	ld a, b
	ld [hli], a
	ld a, c
	ld [hli], a
	ld a, d
	ld [hli], a
	ld a, e
	ld [hli], a

	ld de, wTempCoordinateBuffer

	call IsNPCAtCoord

	pop hl
	ld a, h
	ldh [rSVBK], a ; switch back to original wram bank

	ret nc

	ld hl, OBJECT_WALKING
	add hl, bc
	ld a, [hl]
	cp STANDING
	jr z, .standing
	xor a
	ret

.standing
	scf
	ret



GetFacingCoords:: ; returns the tile coordinates that the sprite at bc is facing in bc and de
	call GetSpriteDirection

	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc ; hl has a pointer to the x coordinate
	ld b, [hl] ; x is in hl, y is in de
	inc hl
	ld c, [hl]
	inc hl
	ld d, [hl]
	inc hl
	ld e, [hl]

	and a ; OW_DOWN but more faster
	jr z, .down
	cp OW_UP
	jr z, .up
	cp OW_LEFT
	jr z, .left
.right
	inc bc
	ret

.down
	inc de
	ret

.up
	dec de
	ret

.left
	dec bc
	ret

WillObjectBumpIntoSomeoneElse: ; does not preserve any registers
	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc
	ld d, h
	ld e, l

IsNPCAtCoord: ; expects a refrence to the coords in question in de
	ld bc, wObjectStructs
	xor a
.loop
	ldh [hObjectStructIndex], a
	call DoesObjectHaveASprite
	jr z, .next

	ldh a, [hMapObjectIndex] ; make sure we aren't checking an object against itself
	ld l, a
	ldh a, [hObjectStructIndex]
	cp l
	jr z, .next

	ld hl, OBJECT_FLAGS1
	add hl, bc
	bit 7, [hl]
	jr nz, .next

	ld hl, OBJECT_PALETTE
	add hl, bc
	bit BIG_OBJECT_F, [hl]
	jr z, .not_big
	call WillObjectIntersectBigObject
	jr nc, .check_current_coords
	jr .yes 

.not_big
	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc
	push de
	ld a, [de]
	cp a, [hl]
	inc de
	inc hl
	jr nz, .check_current_coords
	ld a, [de]
	cp a, [hl]
	inc de
	inc hl
	jr nz, .check_current_coords
	ld a, [de]
	cp a, [hl]
	inc de
	inc hl
	jr nz, .check_current_coords
	ld a, [de]
	cp a, [hl]
	jr nz, .check_current_coords
	pop de
	jr .yes

.check_current_coords ; check against an object's last position for some reason
	pop de
	ld hl, OBJECT_LAST_MAP_X_HIGH
	add hl, bc
	ld a, [de]
	cp a, [hl]
	jr nz, .next
	inc de
	inc hl
	ld a, [de]
	cp a, [hl]
	jr nz, .next
	inc de
	inc hl
	ld a, [de]
	cp a, [hl]
	jr nz, .next
	inc de
	inc hl
	ld a, [de]
	cp a, [hl]
	jr nz, .next
	jr .yes

.next
	ld hl, OBJECT_LENGTH
	add hl, bc
	ld b, h
	ld c, l
	ldh a, [hObjectStructIndex]
	inc a
	cp NUM_OBJECT_STRUCTS
	jr nz, .loop
	and a
	ret

.yes
	scf
	ret

HasObjectReachedMovementLimit: ; this only works for objects that have moved less than 128 tiles away from their initial position
	ld hl, OBJECT_RADIUS
	add hl, bc
	ld a, [hl]

	ld hl, OBJECT_INIT_X_HIGH ; initial x coord is in de
	add hl, bc

	ld d, [hl]
	inc hl
	ld e, [hl]

	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc ; hl now has a pointer to the current coords of the object
	push bc ; save our object number for later consumption

	ld b, [hl]
	inc hl
	ld c, [hl]

	scf ; flag that we are checking x right now

.loop
	push af 

	ld a, d ; this is just to negate de
	cpl
	ld h, a
	ld a, e
	cpl
	ld l, a
	inc de

	add hl, bc ; subtracting
	bit $07, l
	jr z, .pos ; if bit 7 is clear then our result was under 127, so we treat it as positive
	ld a, l
	cpl
	inc a
	ld l, a ; fallthrough
	
.pos
	pop af ; get our movement radius
	push af
	cp a, l
	jr c, .yes
	jr z, .yes
	pop af ; if our carry flag isn't set, then we must have gone through this twice and not hit the radius at all
	jr nc, .nope

.checkY
	pop bc

	ld hl, OBJECT_INIT_Y_HIGH ; initial x coord is in de
	add hl, bc

	ld d, [hl]
	inc hl
	ld e, [hl]

	ld hl, OBJECT_MAP_Y_HIGH
	add hl, bc ; hl now has a pointer to the current coords of the object

	ld b, [hl]
	inc hl
	ld c, [hl]
	and a ; clear the carry flag to signal that we are checking y now
	jr .loop


.nope
	xor a
	ret

.yes
	scf
	ret

IsObjectMovingOffEdgeOfScreen:: ; note: this is expecting the offset of the object in question to be in bc. also, this routine is obnoxiously long for what it does. god i wish this game used c
	ld hl, OBJECT_MAP_X_HIGH
	add hl, bc

ManualScreenCoordCheck:: ; shortcut this so we can reuse this functionality. hl needs to point to the coordinates to check
	call GetTopLeftScreenCoords ; top left corner coords are now in bc and de

	ld a, [hli]
	cp a, b ; high x coord
	jr c, .yes
	jr nz, .checkYTopLeft
	ld a, [hl]
	cp a, c ; low x coord
	jr c, .yes
	
.checkYTopLeft
	inc hl
	ld a, [hli]
	cp a, d ; high y coord
	jr c, .yes
	jr nz, .checkBottomRight
	ld a, [hl]
	cp a, e ; low y coord
	jr c, .yes
	
.checkBottomRight
	dec hl
	dec hl
	dec hl
	call GetBottomRightScreenCoords ; bottom right coords are now in bc and de

	ld a, b ; we swap the order of the operands so i can reuse the same jump conditions and don't have to think again
	cp a, [hl] ; i fucked it up the first time and had to think again
	inc hl
	jr c, .yes
	jr nz, .checkYBottomRight
	ld a, c
	cp a, [hl]
	jr c, .yes

.checkYBottomRight
	inc hl
	ld a, d
	cp a, [hl]
	inc hl
	jr c, .yes
	jr nz, .nope
	ld a, e
	cp a, [hl]
	jr c, .yes

.nope
	and a
	ret

.yes ; object will be offscreen
	scf
	ret

WillObjectIntersectBigObject: ; stubbed for now. Can rewrite later if needed
	and a
	ret

	/*ld hl, OBJECT_MAP_X
	add hl, bc
	ld a, d
	sub [hl]
	jr c, .nope
	cp 2 ; big doll width
	jr nc, .nope
	ld hl, OBJECT_MAP_Y
	add hl, bc
	ld a, e
	sub [hl]
	jr c, .nope
	cp 2 ; big doll height
	jr nc, .nope
	scf
	ret

.nope
	and a
	ret*/
