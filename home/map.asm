; Functions dealing with rendering and interacting with maps.

CheckScenes::
; Checks wCurMapSceneScriptPointer.  If it's empty, returns -1 in a.  Otherwise, returns the active scene ID in a.
	push hl
	ld hl, wCurMapSceneScriptPointer
	ld a, [hli]
	ld h, [hl]
	ld l, a
	or h
	ld a, [hl]
	jr nz, .scene_exists
	ld a, -1

.scene_exists
	pop hl
	ret

GetCurrentMapSceneID::
; Grabs the wram map scene script pointer for the current map and loads it into wCurMapSceneScriptPointer.
; If there is no scene, both bytes of wCurMapSceneScriptPointer are wiped clean.
; Copy the current map group and number into bc.  This is needed for GetMapSceneID.
	ld a, [wMapGroup]
	ld b, a
	ld a, [wMapNumber]
	ld c, a
; Blank out wCurMapSceneScriptPointer; this is the default scenario.
	xor a
	ld [wCurMapSceneScriptPointer], a
	ld [wCurMapSceneScriptPointer + 1], a
	call GetMapSceneID
	ret c ; The map is not in the scene script table
; Load the scene script pointer from de into wCurMapSceneScriptPointer
	ld a, e
	ld [wCurMapSceneScriptPointer], a
	ld a, d
	ld [wCurMapSceneScriptPointer + 1], a
	xor a
	ret

GetMapSceneID::
; Searches the scene_var table for the map group and number loaded in bc, and returns the wram pointer in de.
; If the map is not in the scene_var table, returns carry.
	push bc
	ldh a, [hROMBank]
	push af
	ld a, BANK(MapScenes)
	rst Bankswitch

	ld hl, MapScenes
.loop
	push hl
	ld a, [hli] ; map group, or terminator
	cp -1
	jr z, .end ; the current map is not in the scene_var table
	cp b
	jr nz, .next ; map group did not match
	ld a, [hli] ; map number
	cp c
	jr nz, .next ; map number did not match
	jr .found ; we found our map

.next
	pop hl
	ld de, 4 ; scene_var size
	add hl, de
	jr .loop

.end
	scf
	jr .done

.found
	ld e, [hl]
	inc hl
	ld d, [hl]

.done
	pop hl
	pop bc
	ld a, b
	rst Bankswitch

	pop bc
	ret

OverworldTextModeSwitch::

LoadMapPart::
	call GetScreenCoordinates
	ld a, BANK(sCharblockData)
	call OpenSRAM

	ld hl, wTilemap
	ld a, HIGH(sCharblockTiles)
	ld [wTilesetBlocksAddress + 1], a
	ld a, LOW(sCharblockTiles)
	ld [wTilesetBlocksAddress], a
	call LoadMetatiles
	ld hl, wAttrmap
	ld a, HIGH(sCharblockAttributes)
	ld [wTilesetBlocksAddress + 1], a
	ld a, LOW(sCharblockAttributes)
	ld [wTilesetBlocksAddress], a
	call LoadMetatiles
	call CloseSRAM
	ret

GetScreenCoordinates:: ; Gets the coordinates of the top left 16*16 tile of the screen within the tilemap
	ld a, [wXCoord + 1] ; only need the low byte of the x coord for now
	sub a, $04
	and a, $3F ; wOverworldMapBlocks buffer is 64 tiles wide and heigh
	ld [wScreenXCoord], a ; X coordinate within the map buffer
	ld a, [wYCoord + 1] ; only load the low byte for now
	sub a, $04
	and a, $3F
	ld [wScreenYCoord], a ; Y Coordinate within the map buffer

	ld a, [wPlayerWalking] ; checking if the player is actually walking or not. fixes bugs related to calling LoadMetatiles at odd times
	inc a
	ret z

	ld a, [wPlayerStepDirection] ; Player coordinates aren't updated until after tiles are loaded
	
	and a
	jr z, .Down
	cp a, $01
	jr z, .Up
	cp a, $02
	jr z, .Left
	cp a, $03
	jr z, .Right
	ret ; Should never be reached unless this function gets called while the player isn't moving

.Up:
	ld a, [wScreenYCoord]
	dec a
	jr .finishY
.Down:
	ld a, [wScreenYCoord]
	inc a
.finishY
	and a, $3F
	ld [wScreenYCoord], a
	ret
.Left:
	ld a, [wScreenXCoord]
	dec a
	jr .finishX
.Right:
	ld a, [wScreenXCoord]
	inc a
.finishX
	and a, $3F
	ld [wScreenXCoord], a
	ret

LoadMetatiles:: ; hl points to whatever buffer we want to load the metatiles to
	; wTilesetBlocksAddress points to whatever buffer we're loading from
	; This is kind of a mess but it actually works surprisingly well... unless the game gets paused while the player is moving

	; Player is 4 16 x 16px tiles right and down of the left edge of the screen.
	; If player coordinates are odd, the metatile is half cut off
	xor a
	ld [wTilemapCopyX], a
	ld [wTilemapCopyY], a

	ld a, [wScreenXCoord]
	ld b, a
	ld a, [wScreenYCoord]
	ld c, a

	ld e, b
	srl e
	ld d, c
	srl d ; e, d is the coordinates within wOverworldMapBlocks that we need
	ld a, e
	ld [wBlockX], a
	ld a, d
	ld [wBlockY], a

	; ld hl, wTilemap

	ld a, b
	and a, $01 ; Clear out all of b except for bit 1, to indicate parity
	ld b, a
	ld a, c
	and a, $01 ; same with c
	ld c, a

.loop
	push hl ; hl contains the counter for how much data we've copied
	ld a, [wBlockX]
	ld e, a
	ld a, [wBlockY]
	ld d, a
	srl d 		   ; this is evil bit hacking, but it's much more efficient than actually multiplying
	jr nc, .shift1 ; this shit only works becuase wOverworldMapBlocks is square and has power of 2 dimensions
	set $05, e     ; this makes me cry on the inside. Too bad!
.shift1:
	srl d
	jr nc, .shift2
	set $06, e
.shift2:
	srl d
	jr nc, .shift3
	set $07, e
.shift3: ; de should now have our offset within wOverworldMapBlocks
	ld hl, wOverworldMapBlocks
	add hl, de ; hl now has the address of the block we need
	ld d, $00
	ld e, [hl] ; e has the block id
	sla e ; x2
	rl d
	sla e ; x4
	rl d
	sla e ; x8
	rl d
	sla e ; x16
	rl d
	ld a, [wTilesetBlocksAddress] ; we should already be in the right bank, so we only need to worry about the address
	ld l, a
	ld a, [wTilesetBlocksAddress + 1]
	ld h, a
	add hl, de
	ld d, h ; de should now have the address of the block we need within the tileset
	ld e, l
	pop hl

	bit $00, b
	jr nz, .rightSide
.leftSide:
	bit $00, c
	jr nz, .bottomLeft
.topLeft:
	ld a, [de] ; copy two 8x8 tiles
	ld [hli], a
	inc de
	ld a, [de]
	ld [hl], a
	push hl ; save hl so i don't have to do 16 bit subtraction bc ugggggh

	ld a, SCREEN_WIDTH - 1; god i hate doing 16 bit math manually, but it's a cycle faster than using the stack to do it
	add a, l
	ld l, a
	ld a, h
	adc a, $00
	ld h, a

	inc de ; effectively add 3 to de but without having to do actual math, horray!
	inc de
	inc de

	ld a, [de] ; copy two more 8x8 tiles
	ld [hli], a
	inc de
	ld a, [de]
	ld [hl], a

	pop hl ; restore hl 
	inc hl
	jr .tileDone
.bottomLeft:
	ld a, $08 ; the only difference between topLeft and the goal of the other copy functions is which tiles are copied from the tileset. they all get copied to the same place regardless
	add a, e
	ld e, a
	ld a, d
	adc a, $00
	ld d, a
	jr .topLeft

.rightSide:
	bit $00, c
	jr nz, .bottomRight
.topRight:
	inc de
	inc de
	jr .topLeft
.bottomRight:
	inc de
	inc de
	jr .bottomLeft

.tileDone: 
	inc b ; if b is even after this instruction we have moved to a new map block
	bit $00, b
	jr nz, .sameBlockX
	ld a, [wBlockX]
	inc a
	and a, $1F ; and a with 31 to ensure x wraps around properly
	ld [wBlockX], a
.sameBlockX:
	ld a, [wTilemapCopyX]
	inc a
	ld [wTilemapCopyX], a
	cp a, SCREEN_WIDTH / 2
	jr z, .newRow
	jp .loop
.newRow:
	ld de, SCREEN_WIDTH ; move hl to the next row
	add hl, de

	inc c
	bit $00, c
	jr nz, .sameBlockY
	ld a, [wBlockY]
	inc a
	and a, $1F ; and a with 31 to make sure y wraps around properly
	ld [wBlockY], a
.sameBlockY:

	ld a, [wScreenXCoord] ; reset blockX
	srl a; divide ScreenXCoord by two to get BlockX
	ld [wBlockX], a

	xor a
	ld [wTilemapCopyX], a
	ld a, [wTilemapCopyY]
	inc a
	ld [wTilemapCopyY], a
	cp a, SCREEN_HEIGHT / 2
	jp nz, .loop

.done:
	ret

ReturnToMapFromSubmenu::
	push hl
	;ld hl, wTempBlocksAddress
	inc [hl]
	pop hl
	ld a, MAPSETUP_SUBMENU
	ldh [hMapEntryMethod], a
	farcall RunMapSetupScript
	xor a
	ldh [hMapEntryMethod], a
	ret

CheckOutdoorMap::
	cp ROUTE
	ret z
	cp TOWN
	ret

CheckIndoorMap::
	cp INDOOR
	ret z
	cp CAVE
	ret z
	cp DUNGEON
	ret z
	cp GATE
	ret

ReadMapEvents::
	push af
	ld hl, wMapEventsPointer
	ld a, [hli]
	ld h, [hl]
	ld l, a
	inc hl
	inc hl
	call ReadCoordEvents
	call ReadBGEvents

	pop af
	and a ; skip object events?
	ret nz

	call ReadObjectEvents
	ret

CopyMapAttributes::
	ld de, wMapAttributes
	ld c, wMapAttributesEnd - wMapAttributes
.loop
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .loop
	ret

ReadCoordEvents::
	ld a, [hli]
	ld c, a
	ld [wCurMapCoordEventCount], a
	ld a, l
	ld [wCurMapCoordEventsPointer], a
	ld a, h
	ld [wCurMapCoordEventsPointer + 1], a

	ld a, c
	and a
	ret z

	ld bc, COORD_EVENT_SIZE
	call AddNTimes
	ret

ReadBGEvents::
	ld a, [hli]
	ld c, a
	ld [wCurMapBGEventCount], a
	ld a, l
	ld [wCurMapBGEventsPointer], a
	ld a, h
	ld [wCurMapBGEventsPointer + 1], a

	ld a, c
	and a
	ret z

	ld bc, BG_EVENT_SIZE
	call AddNTimes
	ret

ReadObjectEvents::
	push hl
	call ClearObjectStructs
	pop de
	ld hl, wMap1Object
	ld a, [de]
	inc de
	ld [wCurMapObjectEventCount], a
	ld a, e
	ld [wCurMapObjectEventsPointer], a
	ld a, d
	ld [wCurMapObjectEventsPointer + 1], a

	ld a, [wCurMapObjectEventCount]
	call CopyMapObjectEvents

; get NUM_OBJECTS - [wCurMapObjectEventCount]
; BUG: ReadObjectEvents overflows into wObjectMasks (see docs/bugs_and_glitches.md)
	ld a, [wCurMapObjectEventCount]
	ld c, a
	ld a, NUM_OBJECTS
	sub c
	jr z, .skip

	; could have done "inc hl" instead
	ld bc, 1
	add hl, bc
	ld bc, MAPOBJECT_LENGTH
.loop
	ld [hl],  0
	inc hl
	ld [hl], -1
	dec hl
	add hl, bc
	dec a
	jr nz, .loop

.skip
	ld h, d
	ld l, e
	ret

CopyMapObjectEvents::
	and a
	ret z

	ld c, a
.loop
	push bc
	push hl
	ld a, $ff
	ld [hli], a
	ld b, OBJECT_EVENT_SIZE
.loop2
	ld a, [de]
	inc de
	ld [hli], a
	dec b
	jr nz, .loop2

	pop hl
	ld bc, MAPOBJECT_LENGTH
	add hl, bc
	pop bc
	dec c
	jr nz, .loop
	ret

ClearObjectStructs::
	ld hl, wObject1Struct
	ld bc, OBJECT_LENGTH * (NUM_OBJECT_STRUCTS - 1)
	xor a
	call ByteFill

; Just to make sure (this is rather pointless)
	ld hl, wObject1Struct
	ld de, OBJECT_LENGTH
	ld c, NUM_OBJECT_STRUCTS - 1
	xor a
.loop
	ld [hl], a
	add hl, de
	dec c
	jr nz, .loop
	ret

GetChunkCoords::
	; Takes 16 bit x and y tile coordinates in bc and de and returns the chunk's x and y positions in wChunkX and wChunkY
REPT 5
	srl b ; Divide bc by 32
	rr c
ENDR
REPT 5
	srl d ; Divide de by 32
	rr e
ENDR
	ld a, c
	ld [wChunkX], a
	ld a, e
	ld [wChunkY], a
	ret


GetChunkHeaderIndexInBank:: 
	; Takes 8 bit chunk x and y coords in e and d respetively and returns the chunk header's 16 bit index number in hl
	; Does not preserve a
	xor a
	ldh [hMultiplicand], a ; clear out hMultiplicand - this may not be strictly needed, but is good for safety
	ldh [hMultiplicand + 1], a
	ld a, d
	ldh [hMultiplicand + 2], a ; multiplicand is loaded from d
	ld a, MAP_CHUNK_WIDTH
	ldh [hMultiplier], a
	call Multiply
	ldh a, [hProduct + 2] ; put high byte of product into h - this assumes the product fits within 16 bits, which it should unless I fucked up
	ld h, a
	ldh a, [hProduct + 3] ; put low byte of product into l
	ld l, a
	ld d, $00
	add hl, de ; add the low byte to get the final number
	ret

CopyChunkHeader::
	ld a, [wChunkY]
	ld c, NUM_CHUNK_HEADERS_ROWS_IN_BANK
	call SimpleDivide ; Header bank number is in b, remainder is in a
	ld d, a; Put remainder (aka chunk y pos within the bank) into d
	set $7, b ; This assumes chunk headers start at bank $80
	ld a, [wChunkX] ; Put chunk x coord into e
	ld e, a
	call GetChunkHeaderIndexInBank ; hl now has the index of the chunk header in it's respective bank
REPT CHUNK_HEADER_SIZE_LOG2
	sla l
	rl h
ENDR
    set 6, h ; effectively add $4000 to h
	ldh a, [hROMBank] ; Save current bank to switch back to later
	push af
	ld a, b
	rst Bankswitch ; Switch to the bank with the correct chunk header
	
	ld de, wChunkHeader
	ld bc, CHUNK_HEADER_SIZE
	call CopyBytes ; Chunk header is now copied to work ram

	pop af
	rst Bankswitch
	ret

LoadMapStatus::
	ld [wMapStatus], a
	ret

CallScript::
; Call a script at a:hl.

	ld [wScriptBank], a
	ld a, l
	ld [wScriptPos], a
	ld a, h
	ld [wScriptPos + 1], a

	ld a, PLAYEREVENT_MAPSCRIPT
	ld [wScriptRunning], a

	scf
	ret

CallMapScript::
; Call a script at hl in the current bank if there isn't already a script running
	ld a, $00 ; [wScriptRunning]
	and a
	ret nz
	; call GetMapScriptsBank
	; jr CallScript

RunMapCallback::
; Will run the first callback found with execution index equal to a.
	ld b, a
	ldh a, [hROMBank]
	push af
	; call SwitchToMapScriptsBank ; Dummy this shit out. I'm not even fully sure what this function does, but it probably has to do with the fucking script system
	; call .FindCallback
	scf 
	ccf
	jr nc, .done

	; call GetMapScriptsBank
	ld b, a
	ld d, h
	ld e, l
	; call ExecuteCallbackScript

.done
	pop af
	rst Bankswitch
	ret

.FindCallback:
	ld a, [wCurMapCallbackCount]
	ld c, a
	and a
	ret z
	ld hl, wCurMapCallbacksPointer
	ld a, [hli]
	ld h, [hl]
	ld l, a
	or h
	ret z
	ld de, CALLBACK_SIZE
.loop
	ld a, [hl]
	cp b
	jr z, .found
	add hl, de
	dec c
	jr nz, .loop
	xor a
	ret

.found
	inc hl
	ld a, [hli]
	ld h, [hl]
	ld l, a
	scf
	ret

ExecuteCallbackScript::
; Do map callback de and return to script bank b.
	farcall CallCallback
	ld a, [wScriptMode]
	push af
	ld hl, wScriptFlags
	ld a, [hl]
	push af
	set 1, [hl]
	farcall EnableScriptMode
	farcall ScriptEvents
	pop af
	ld [wScriptFlags], a
	pop af
	ld [wScriptMode], a
	ret

MapTextbox::
	ldh a, [hROMBank]
	push af

	ld a, b
	rst Bankswitch

	push hl
	call SpeechTextbox
	call SafeUpdateSprites
	ld a, 1
	ldh [hOAMUpdate], a
	call ApplyTilemap
	pop hl
	call PrintTextboxText
	xor a
	ldh [hOAMUpdate], a

	pop af
	rst Bankswitch
	ret

Call_a_de::
; Call a:de.

	ldh [hTempBank], a
	ldh a, [hROMBank]
	push af
	ldh a, [hTempBank]
	rst Bankswitch

	call .de

	pop af
	rst Bankswitch
	ret

.de
	push de
	ret

GetMovementData::
; Initialize the movement data for object c at b:hl
	ldh a, [hROMBank]
	push af
	ld a, b
	rst Bankswitch

	ld a, c
	call LoadMovementDataPointer

	pop hl
	ld a, h
	rst Bankswitch
	ret

GetScriptByte::
; Return byte at wScriptBank:wScriptPos in a.

	push hl
	push bc
	ldh a, [hROMBank]
	push af
	ld a, [wScriptBank]
	rst Bankswitch

	ld hl, wScriptPos
	ld c, [hl]
	inc hl
	ld b, [hl]

	ld a, [bc]

	inc bc
	ld [hl], b
	dec hl
	ld [hl], c

	ld b, a
	pop af
	rst Bankswitch
	ld a, b
	pop bc
	pop hl
	ret

ObjectEvent::
	jumptextfaceplayer ObjectEventText

ObjectEventText::
	text_far _ObjectEventText
	text_end

BGEvent:: ; unreferenced
	jumptext BGEventText

BGEventText::
	text_far _BGEventText
	text_end

CoordinatesEvent:: ; unreferenced
	jumptext CoordinatesEventText

CoordinatesEventText::
	text_far _CoordinatesEventText
	text_end

CheckObjectMask::
	ldh a, [hMapObjectIndex]
	ld e, a
	ld d, 0
	ld hl, wObjectMasks
	add hl, de
	ld a, [hl]
	ret

MaskObject::
	ldh a, [hMapObjectIndex]
	ld e, a
	ld d, 0
	ld hl, wObjectMasks
	add hl, de
	ld [hl], -1 ; masked
	ret

UnmaskObject::
	ldh a, [hMapObjectIndex]
	ld e, a
	ld d, 0
	ld hl, wObjectMasks
	add hl, de
	ld [hl], 0 ; unmasked
	ret

if DEF(_DEBUG)
ComputeROMXChecksum::
	ldh a, [hROMBank]
	push af
	ld a, c
	rst Bankswitch
	ld hl, $4000 ; ROMX start
.loop
	ld a, [hli]
	add e
	ld e, a
	ld a, d
	adc 0
	ld d, a
	ld a, h
	cp $80 ; HIGH(ROMX end)
	jr c, .loop
	pop af
	rst Bankswitch
	ret
endc

ScrollMapUp::
	hlcoord 0, 0
	ld de, wBGMapBuffer
	call BackupBGMapRow
	hlcoord 0, 0, wAttrmap
	ld de, wBGMapPalBuffer
	call BackupBGMapRow
	ld a, [wBGMapAnchor]
	ld e, a
	ld a, [wBGMapAnchor + 1]
	ld d, a
	call UpdateBGMapRow
	ld a, $1
	ldh [hBGMapUpdate], a
	ret

ScrollMapDown::
	hlcoord 0, SCREEN_HEIGHT - 2
	ld de, wBGMapBuffer
	call BackupBGMapRow
	hlcoord 0, SCREEN_HEIGHT - 2, wAttrmap
	ld de, wBGMapPalBuffer
	call BackupBGMapRow
	ld a, [wBGMapAnchor]
	ld l, a
	ld a, [wBGMapAnchor + 1]
	ld h, a
	ld bc, BG_MAP_WIDTH tiles
	add hl, bc
; cap d at HIGH(vBGMap0)
	ld a, h
	and %00000011
	or HIGH(vBGMap0)
	ld e, l
	ld d, a
	call UpdateBGMapRow
	ld a, $1
	ldh [hBGMapUpdate], a
	ret

ScrollMapLeft::
	hlcoord 0, 0
	ld de, wBGMapBuffer
	call BackupBGMapColumn
	hlcoord 0, 0, wAttrmap
	ld de, wBGMapPalBuffer
	call BackupBGMapColumn	
	ld a, [wBGMapAnchor]
	ld e, a
	ld a, [wBGMapAnchor + 1]
	ld d, a
	call UpdateBGMapColumn
	ld a, $1
	ldh [hBGMapUpdate], a
	ret

ScrollMapRight::
	hlcoord SCREEN_WIDTH - 2, 0
	ld de, wBGMapBuffer
	call BackupBGMapColumn
	hlcoord SCREEN_WIDTH - 2, 0, wAttrmap
	ld de, wBGMapPalBuffer
	call BackupBGMapColumn	
	ld a, [wBGMapAnchor]
	ld e, a
	and %11100000
	ld b, a
	ld a, e
	add SCREEN_HEIGHT
	and %00011111
	or b
	ld e, a
	ld a, [wBGMapAnchor + 1]
	ld d, a
	call UpdateBGMapColumn
	ld a, $1
	ldh [hBGMapUpdate], a
	ret

BackupBGMapRow::
	ld c, 2 * SCREEN_WIDTH
.loop
	ld a, [hli]
	ld [de], a
	inc de
	dec c
	jr nz, .loop
	ret

BackupBGMapColumn::
	ld c, SCREEN_HEIGHT
.loop
	ld a, [hli]
	ld [de], a
	inc de
	ld a, [hl]
	ld [de], a
	inc de
	ld a, SCREEN_WIDTH - 1
	add l
	ld l, a
	jr nc, .skip
	inc h

.skip
	dec c
	jr nz, .loop
	ret

UpdateBGMapRow::
	ld hl, wBGMapBufferPointers
	push de
	call .iteration
	pop de
	ld a, BG_MAP_WIDTH
	add e
	ld e, a

.iteration
	ld c, 10
.loop
	ld a, e
	ld [hli], a
	ld a, d
	ld [hli], a
	ld a, e
	inc a
	inc a
	and $1f
	ld b, a
	ld a, e
	and $e0
	or b
	ld e, a
	dec c
	jr nz, .loop
	ld a, SCREEN_WIDTH
	ldh [hBGMapTileCount], a
	ret

UpdateBGMapColumn::
	ld hl, wBGMapBufferPointers
	ld c, SCREEN_HEIGHT
.loop
	ld a, e
	ld [hli], a
	ld a, d
	ld [hli], a
	ld a, BG_MAP_WIDTH
	add e
	ld e, a
	jr nc, .skip
	inc d
; cap d at HIGH(vBGMap0)
	ld a, d
	and %11
	or HIGH(vBGMap0)
	ld d, a

.skip
	dec c
	jr nz, .loop
	ld a, SCREEN_HEIGHT
	ldh [hBGMapTileCount], a
	ret

ClearBGMapBuffer:: ; unreferenced
	ld hl, wBGMapBuffer
	ld bc, wBGMapBufferEnd - wBGMapBuffer
	xor a
	call ByteFill
	ret

LoadTilesetGFX::
	ld hl, wTilesetAddress
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wTilesetBank]
	ld e, a

	ldh a, [rSVBK]
	push af
	ld a, BANK(wDecompressScratch)
	ldh [rSVBK], a

	ld a, e
	ld de, wDecompressScratch
	call FarDecompress

	ld hl, wDecompressScratch
	ld de, vTiles2
	ld bc, $60 tiles
	call CopyBytes

	ldh a, [rVBK]
	push af
	ld a, BANK(vTiles5)
	ldh [rVBK], a

	ld hl, wDecompressScratch + $60 tiles
	ld de, vTiles5
	ld bc, $60 tiles
	call CopyBytes

	pop af
	ldh [rVBK], a

	pop af
	ldh [rSVBK], a

; These tilesets support dynamic per-mapgroup roof tiles.
	ld a, [wMapTileset]
	cp TILESET_JOHTO
	jr z, .load_roof
	cp TILESET_JOHTO_MODERN
	jr z, .load_roof
	cp TILESET_BATTLE_TOWER_OUTSIDE
	jr z, .load_roof
	jr .skip_roof

.load_roof
	farcall LoadMapGroupRoof

.skip_roof
	xor a
	ldh [hTileAnimFrame], a
	ret

BufferScreen:: ; God I wish this shit was documented
	ld hl, wOverworldMapAnchor
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld de, wScreenSave
	ld c, SCREEN_META_HEIGHT
	ld b, SCREEN_META_WIDTH
.row
	push bc
	push hl
.col
	ld a, [hli]
	ld [de], a
	inc de
	dec b
	jr nz, .col
	pop hl
	ld a, [wMapWidth]
	add 6
	ld c, a
	ld b, 0
	add hl, bc
	pop bc
	dec c
	jr nz, .row
	ret

SaveScreen::
	ld hl, wOverworldMapAnchor
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld de, wScreenSave
	ld a, [wMapWidth]
	add 6
	ldh [hMapObjectIndex], a
	ld a, [wPlayerStepDirection]
	and a
	jr z, .down
	cp UP
	jr z, .up
	cp LEFT
	jr z, .left
	cp RIGHT
	jr z, .right
	ret

.up
	ld de, wScreenSave + SCREEN_META_WIDTH
	ldh a, [hMapObjectIndex]
	ld c, a
	ld b, 0
	add hl, bc
	jr .vertical

.down
	ld de, wScreenSave
.vertical
	ld b, SCREEN_META_WIDTH
	ld c, SCREEN_META_HEIGHT - 1
	jr SaveScreen_LoadConnection

.left
	ld de, wScreenSave + 1
	inc hl
	jr .horizontal

.right
	ld de, wScreenSave
.horizontal
	ld b, SCREEN_META_WIDTH - 1
	ld c, SCREEN_META_HEIGHT
	jr SaveScreen_LoadConnection

LoadConnectionBlockData::
	ld hl, wOverworldMapAnchor
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wMapWidth]
	add 6
	ldh [hConnectionStripLength], a
	ld de, wScreenSave
	ld b, SCREEN_META_WIDTH
	ld c, SCREEN_META_HEIGHT

SaveScreen_LoadConnection::
.row
	push bc
	push hl
	push de
.col
	ld a, [de]
	inc de
	ld [hli], a
	dec b
	jr nz, .col
	pop de
	ld a, e
	add 6
	ld e, a
	jr nc, .okay
	inc d
.okay
	pop hl
	ldh a, [hConnectionStripLength]
	ld c, a
	ld b, 0
	add hl, bc
	pop bc
	dec c
	jr nz, .row
	ret

GetMovementPermissions::
	; call .LeftRight
	; call .UpDown
; get coords of current tile
	ld a, [wXCoord + 1]
	and a, $3F
	ld d, a
	ld a, [wYCoord + 1]
	and a, $3F
	ld e, a
	call GetCoordTile
	ld [wPlayerCollision], a
	ret
	; call .CheckHiNybble
	; ret nz

	; ld a, [wPlayerCollision]
	; and 7
	; ld hl, .MovementPermissionsData
	; add l
	; ld l, a
	; ld a, 0
	; adc h
	; ld h, a
	; ld a, [hl]
	; ld hl, wTilePermissions
	; or [hl]
	; ld [hl], a
	; ret

.MovementPermissionsData:
	db DOWN_MASK
	db UP_MASK
	db LEFT_MASK
	db RIGHT_MASK
	db DOWN_MASK | RIGHT_MASK
	db UP_MASK | RIGHT_MASK
	db DOWN_MASK | LEFT_MASK
	db UP_MASK | LEFT_MASK

.UpDown:
	ld a, [wXCoord + 1]
	and a, $3F
	ld d, a
	ld a, [wYCoord + 1]
	and a, $3F
	ld e, a

	push de
	inc e
	call GetCoordTile
	ld [wCollisionDown], a
	; call .Down

	pop de
	dec e
	call GetCoordTile
	ld [wCollisionUp], a
	; call .Up
	ret

.LeftRight:
	ld a, [wXCoord + 1]
	ld d, a
	ld a, [wYCoord + 1]
	ld e, a

	push de
	dec d
	call GetCoordTile
	ld [wCollisionLeft], a
	; call .Left

	pop de
	inc d
	call GetCoordTile
	ld [wCollisionRight], a
	; call .Right
	ret

; .Down:
; 	call .CheckHiNybble
; 	ret nz
; 	ld a, [wCollisionDown]
; 	and %111
; 	cp COLL_UP_WALL & %111 ; COLL_UP_BUOY & %111
; 	jr z, .ok_down
; 	cp COLL_UP_RIGHT_WALL & %111 ; COLL_UP_RIGHT_BUOY & %111
; 	jr z, .ok_down
; 	cp COLL_UP_LEFT_WALL & %111 ; COLL_UP_LEFT_BUOY & %111
; 	ret nz

; .ok_down
; 	ld a, [wTilePermissions]
; 	or FACE_DOWN
; 	ld [wTilePermissions], a
; 	ret

; .Up:
; 	call .CheckHiNybble
; 	ret nz
; 	ld a, [wCollisionUp]
; 	and %111
; 	cp COLL_DOWN_WALL & %111 ; COLL_DOWN_BUOY & %111
; 	jr z, .ok_up
; 	cp COLL_DOWN_RIGHT_WALL & %111 ; COLL_DOWN_RIGHT_BUOY & %111
; 	jr z, .ok_up
; 	cp COLL_DOWN_LEFT_WALL & %111 ; COLL_DOWN_LEFT_BUOY & %111
; 	ret nz

; .ok_up
; 	ld a, [wTilePermissions]
; 	or FACE_UP
; 	ld [wTilePermissions], a
; 	ret

; .Right:
; 	call .CheckHiNybble
; 	ret nz
; 	ld a, [wCollisionRight]
; 	and %111
; 	cp COLL_LEFT_WALL & %111 ; COLL_LEFT_BUOY & %111
; 	jr z, .ok_right
; 	cp COLL_DOWN_LEFT_WALL & %111 ; COLL_DOWN_LEFT_BUOY & %111
; 	jr z, .ok_right
; 	cp COLL_UP_LEFT_WALL & %111 ; COLL_UP_LEFT_BUOY & %111
; 	ret nz

; .ok_right
; 	ld a, [wTilePermissions]
; 	or FACE_RIGHT
; 	ld [wTilePermissions], a
; 	ret

; .Left:
; 	call .CheckHiNybble
; 	ret nz
; 	ld a, [wCollisionLeft]
; 	and %111
; 	cp COLL_RIGHT_WALL & %111 ; COLL_RIGHT_BUOY & %111
; 	jr z, .ok_left
; 	cp COLL_DOWN_RIGHT_WALL & %111 ; COLL_DOWN_RIGHT_BUOY & %111
; 	jr z, .ok_left
; 	cp COLL_UP_RIGHT_WALL & %111 ; COLL_UP_RIGHT_BUOY & %111
; 	ret nz

; .ok_left
; 	ld a, [wTilePermissions]
; 	or FACE_LEFT
; 	ld [wTilePermissions], a
; 	ret

; .CheckHiNybble:
; 	and $f0
; 	cp HI_NYBBLE_SIDE_WALLS
; 	ret z
; 	cp HI_NYBBLE_SIDE_BUOYS
; 	ret

GetFacingTileCoord:: ; Return coordinates within wOverworldMapBlocks in (d, e) and collision byte in a of the tile the player is facing.

	ld a, [wPlayerDirection]
	and %1100
	srl a
	srl a
	ld l, a
	ld h, 0
	add hl, hl
	add hl, hl
	ld de, .Directions
	add hl, de

	ld d, [hl]
	inc hl
	ld e, [hl]
	inc hl

	ld a, [hli]
	ld h, [hl]
	ld l, a

	ld a, [wXCoord + 1]
	and a, $3F
	add d
	and a, $3F
	ld d, a
	ld a, [wYCoord + 1]
	and a, $3F
	add e
	and a, $3F
	ld e, a
	push de
	call GetCoordTile
	pop de
	ret

.Directions:
	;   x,  y
	db  0,  1
	dw wCollisionDown
	db  0, -1
	dw wCollisionUp
	db -1,  0
	dw wCollisionLeft
	db  1,  0
	dw wCollisionRight

GetCoordTile:: ; make this handle the new layout
; Get the collision byte for tile d, e
	push de
	call GetBlockLocation
	pop de
	ld a, e
	and a, $01
	sla a
	ld e, a
	ld a, d
	and a, $01
	or a, e
	add a, wDecompressedCharblockCollision - wDecompressedCharblockBuffer ; add the collision offset
	ld e, a
	ld d, $00
	ld c, [hl]
	call LookupBlockIndex
	call GetBlockDataPointer
	add hl, de ; point to our collision byte
	
	ld b, h
	ld c, l
	ld h, HIGH(CHARBLOCK_START_BANK)
	jp GetReallyFarByte	

LookupBlockIndex:: ; returns the block id in bc for block c in the id table
	ld b, $00
	sla c
	rl b
	ld hl, sCharblockIDs
	add hl, bc
	ld a, BANK(sCharblockIDs)
	call OpenSRAM
	ld b, [hl]
	inc l ; each id is always 2-byte aligned, so we only need to increment the low byte
	ld c, [hl]
	jp CloseSRAM ; tail call optimization

GetBlockDataPointer:: ; returns a pointer to map block bc in a:hl
	ld a, c
	rrca
	rrca
	ld c, a
	and a, %00111111
	ld h, a
	ld a, c
	and a, %11000000 ; lower byte is done
	ld l, a

	ld a, b
	rrca
	rrca
	ld b, a
	and a, %11000000
	or a, h ; upper byte done
	ld h, a
	ld a, b
	and a, %00111111

    sla h ; adjust for rom banks being only 1/4 of the address space
    rla
    sla h
    rla
    scf ; sneaky way to set the sixth bit of h
    rr h
    srl h
	add a, LOW(CHARBLOCK_START_BANK)
	ret

GetBlockLocation:: ; returns a pointer to the map block at tile location d, e in hl
	ld a, d
	and a, $3F
	ld d, a
	ld a, e
	and a, $3F
	ld e, d ; swap d and e
	ld d, a

	sla e ; this is very clever, me likey
	sla e

	srl d ; pack d and e together
	rr e
	srl d
	rr e
	srl d
	rr e

	ld hl, wOverworldMapBlocks
	add hl, de
	ret

CheckFacingBGEvent::
	call GetFacingTileCoord
; Load facing into b.
	ld b, a
; Convert the coordinates at de to within-boundaries coordinates.
	ld a, d
	sub 4
	ld d, a
	ld a, e
	sub 4
	ld e, a
; If there are no BG events, we don't need to be here.
	ld a, $00 ; Bypass this for now. TODO - find a  better way to do this
	and a
	ret z

	ld c, a
	ldh a, [hROMBank]
	push af
	; call SwitchToMapScriptsBank
	call CheckIfFacingTileCoordIsBGEvent
	pop hl
	ld a, h
	rst Bankswitch
	ret

CheckIfFacingTileCoordIsBGEvent::
; Checks to see if you are facing a BG event.  If so, copies it into wCurBGEvent and sets carry.
	ld hl, wCurMapBGEventsPointer
	ld a, [hli]
	ld h, [hl]
	ld l, a
.loop
	push hl
	ld a, [hli]
	cp e
	jr nz, .next
	ld a, [hli]
	cp d
	jr nz, .next
	jr .copysign

.next
	pop hl
	ld a, BG_EVENT_SIZE
	add l
	ld l, a
	jr nc, .nocarry
	inc h

.nocarry
	dec c
	jr nz, .loop
	xor a
	ret

.copysign
	pop hl
	ld de, wCurBGEvent
	ld bc, BG_EVENT_SIZE
	call CopyBytes
	scf
	ret

CheckCurrentMapCoordEvents::
; If there are no coord events, we don't need to be here.
	ld a, $00 ; Bypass this for now. TODO - come up with a better way of handling coord events
	and a
	ret z
; Copy the coord event count into c.
	ld c, a
	ldh a, [hROMBank]
	push af
	; call SwitchToMapScriptsBank
	call .CoordEventCheck
	pop hl
	ld a, h
	rst Bankswitch
	ret

.CoordEventCheck:
	ret
; Checks to see if you are standing on a coord event.  If yes, copies the event to wCurCoordEvent and sets carry.
	/*ld hl, wCurMapCoordEventsPointer ; TODO - make this handle dynamic banks of coordinate events
	ld a, [hli]
	ld h, [hl]
	ld l, a
; Load the active scene ID into b
	call CheckScenes
	ld b, a
; Load your current coordinates into de.  This will be used to check if your position is in the coord event table for the current map.
	ld a, [wPlayerMapX]
	sub 4
	ld d, a
	ld a, [wPlayerMapY]
	sub 4
	ld e, a

.loop
	push hl
	ld a, [hli]
	cp b
	jr z, .got_id
	cp -1
	jr nz, .next

.got_id
	ld a, [hli]
	cp e
	jr nz, .next
	ld a, [hli]
	cp d
	jr nz, .next
	jr .copy_coord_event

.next
	pop hl
	ld a, COORD_EVENT_SIZE
	add l
	ld l, a
	jr nc, .nocarry
	inc h

.nocarry
	dec c
	jr nz, .loop
	xor a
	ret

.copy_coord_event
	pop hl
	ld de, wCurCoordEvent
	ld bc, COORD_EVENT_SIZE
	call CopyBytes
	scf
	ret */

FadeToMenu::
	xor a
	ldh [hBGMapMode], a
	call LoadStandardMenuHeader
	farcall FadeOutPalettes
	call ClearSprites
	call DisableSpriteUpdates
	ret

CloseSubmenu::
	call ClearBGPalettes
	call ReloadTilesetAndPalettes
	call UpdateSprites
	call Call_ExitMenu
	call GSReloadPalettes
	jr FinishExitMenu

ExitAllMenus::
	call ClearBGPalettes
	call Call_ExitMenu
	call ReloadTilesetAndPalettes
	call UpdateSprites
	call GSReloadPalettes
FinishExitMenu::
	farcall LoadMapPartsOnSpawn
	ld b, SCGB_MAPPALS
	call GetSGBLayout
	farcall LoadOW_BGPal7
	call WaitBGMap2
	farcall FadeInPalettes
	call EnableSpriteUpdates
	ret

ReturnToMapWithSpeechTextbox::
	push af
	ld a, $1
	ld [wSpriteUpdatesEnabled], a
	call ClearBGPalettes
	call ClearSprites
	call ReloadTilesetAndPalettes
	hlcoord 0, 12
	lb bc, 4, 18
	call Textbox
	ld hl, wVramState
	set 0, [hl]
	call UpdateSprites
	call WaitBGMap2
	ld b, SCGB_MAPPALS
	call GetSGBLayout
	farcall LoadOW_BGPal7
	call UpdateTimePals
	call DelayFrame
	ld a, $1
	ldh [hMapAnims], a
	pop af
	ret

ReloadTilesetAndPalettes::
	call DisableLCD
	call ClearSprites
	farcall RefreshSprites
	call LoadStandardFont
	call LoadFontsExtra
	ldh a, [hROMBank]
	push af
	ld a, [wMapGroup]
	ld b, a
	ld a, [wMapNumber]
	ld c, a
	; call SwitchToAnyMapAttributesBank
	farcall UpdateTimeOfDayPal
	call OverworldTextModeSwitch
	call MarkTilesForReload
	;call LoadTilesetGFX
	ld a, 9
	call SkipMusic
	pop af
	rst Bankswitch

	call EnableLCD
	ret

MarkTilesForReload:: ; marks all overworld tiles as being dirty. TileDMA takes care of actually reloading the tiles
	ldh a, [rSVBK]
	push af
	ld a, BANK(wOutdatedTileFlags)
	ldh [rSVBK], a ; switch to WRAM bank
	ld a, $FF
	ld c, wOutdatedTileFlagsEnd - wOutdatedTileFlags + 1
	ld hl, wOutdatedTileFlags
.loop
	ld [hli], a
	dec c
	jr nz, .loop
	pop af
	ldh [rSVBK], a ; restore WRAM bank
	ret

GetMapEnvironment::
	push hl
	push de
	push bc
	ld de, MAP_ENVIRONMENT
	; call GetMapField ; TODO - make this dynamic
	ld c, INDOOR
	ld a, c
	pop bc
	pop de
	pop hl
	ret

GetAnyMapEnvironment::
	push hl
	push de
	push bc
	ld de, MAP_ENVIRONMENT
	; call GetAnyMapField ; TODO - make this dynamic
	ld c, INDOOR
	ld a, c
	pop bc
	pop de
	pop hl
	ret

GetWorldMapLocation::
; given a map group/id in bc, return its location on the Pokégear map.
	push hl
	push de
	push bc

	ld de, MAP_LOCATION
	; call GetAnyMapField ; TODO - make this dynamic. This one will probably have to be extended to handle far more landmarks than the base game
	ld c,  LANDMARK_NEW_BARK_TOWN
	ld a, c

	pop bc
	pop de
	pop hl
	ret

GetMapMusic::
	push hl
	push bc
	ld de, MAP_MUSIC
	; call GetMapField ; TODO - make this dynamic
	ld c, MUSIC_NEW_BARK_TOWN
	ld a, c
	cp MUSIC_MAHOGANY_MART
	jr z, .mahoganymart
	bit RADIO_TOWER_MUSIC_F, c
	jr nz, .radiotower
	ld e, c
	ld d, 0
.done
	pop bc
	pop hl
	ret

.radiotower
	ld a, [wStatusFlags2]
	bit STATUSFLAGS2_ROCKETS_IN_RADIO_TOWER_F, a
	jr z, .clearedradiotower
	ld de, MUSIC_ROCKET_OVERTURE
	jr .done

.clearedradiotower
	; the rest of the byte
	ld a, c
	and RADIO_TOWER_MUSIC - 1
	ld e, a
	ld d, 0
	jr .done

.mahoganymart
	ld a, [wStatusFlags2]
	bit STATUSFLAGS2_ROCKETS_IN_MAHOGANY_F, a
	jr z, .clearedmahogany
	ld de, MUSIC_ROCKET_HIDEOUT
	jr .done

.clearedmahogany
	ld de, MUSIC_CHERRYGROVE_CITY
	jr .done

GetMapTimeOfDay::
	call GetPhoneServiceTimeOfDayByte
	and $f
	ret

GetMapPhoneService::
	call GetPhoneServiceTimeOfDayByte
	and $f0
	swap a
	ret

GetPhoneServiceTimeOfDayByte::
	push hl
	push bc

	ld de, MAP_PALETTE
	; call GetMapField ; TODO - make this dynamic
	ld c, PALETTE_DAY
	ld a, c

	pop bc
	pop hl
	ret

GetFishingGroup::
	push de
	push hl
	push bc

	ld de, MAP_FISHGROUP
	; call GetMapField ; TODO - make this dynamic
	ld c, FISHGROUP_SHORE
	ld a, c

	pop bc
	pop hl
	pop de
	ret

LoadMapTileset::
	push hl
	push bc

	ld hl, Tilesets
	ld bc, TILESET_LENGTH
	ld a, [wMapTileset]
	call AddNTimes

	ld de, wTilesetBank
	ld bc, TILESET_LENGTH

	ld a, BANK(Tilesets)
	call FarCopyBytes

	pop bc
	pop hl
	ret

DummyEndPredef::
; Unused function at the end of PredefPointers.
rept 16
	nop
endr
	ret

DEF CHARBLOCK_START_BANK EQU $105