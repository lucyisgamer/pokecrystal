HandleNewMap:
	call ResetMapBufferEventFlags
	call ResetFlashIfOutOfCave
	call GetCurrentMapSceneID
	call ResetBikeFlags
	ld a, MAPCALLBACK_NEWMAP
	call RunMapCallback
HandleContinueMap:
	farcall ClearCmdQueue
	ld a, MAPCALLBACK_CMDQUEUE
	call RunMapCallback
	call GetMapTimeOfDay
	ld [wMapTimeOfDay], a
	ret

EnterMapWarp:
	call .SaveDigWarp
	call .SetSpawn
	ld a, [wNextWarp]
	ld [wWarpNumber], a
	ld a, [wNextMapGroup]
	ld [wMapGroup], a
	ld a, [wNextMapNumber]
	ld [wMapNumber], a
	ret

.SaveDigWarp:
	call GetMapEnvironment
	call CheckOutdoorMap
	ret nz
	ld a, [wNextMapGroup]
	ld b, a
	ld a, [wNextMapNumber]
	ld c, a
	call GetAnyMapEnvironment
	call CheckIndoorMap
	ret nz

; MOUNT_MOON_SQUARE and TIN_TOWER_ROOF are outdoor maps within indoor maps.
; Dig and Escape Rope should not take you to them.
	ld a, [wPrevMapGroup]
	cp GROUP_MOUNT_MOON_SQUARE
	jr nz, .not_mt_moon_square_or_tin_tower_roof
	assert GROUP_MOUNT_MOON_SQUARE == GROUP_TIN_TOWER_ROOF
	ld a, [wPrevMapNumber]
	cp MAP_MOUNT_MOON_SQUARE
	ret z
	cp MAP_TIN_TOWER_ROOF
	ret z
.not_mt_moon_square_or_tin_tower_roof

	ld a, [wPrevWarp]
	ld [wDigWarpNumber], a
	ld a, [wPrevMapGroup]
	ld [wDigMapGroup], a
	ld a, [wPrevMapNumber]
	ld [wDigMapNumber], a
	ret

.SetSpawn:
	call GetMapEnvironment
	call CheckOutdoorMap
	ret nz
	ld a, [wNextMapGroup]
	ld b, a
	ld a, [wNextMapNumber]
	ld c, a
	call GetAnyMapEnvironment
	call CheckIndoorMap
	ret nz
	ld a, [wNextMapGroup]
	ld b, a
	ld a, [wNextMapNumber]
	ld c, a
	ret
.pokecenter_pokecom

	ld a, [wPrevMapGroup]
	ld [wLastSpawnMapGroup], a
	ld a, [wPrevMapNumber]
	ld [wLastSpawnMapNumber], a
	ret

LoadMapTimeOfDay::
	ld hl, wVramState
	res 6, [hl]
	ld a, $1
	ld [wSpriteUpdatesEnabled], a
	farcall ReplaceTimeOfDayPals
	farcall UpdateTimeOfDayPal
	call OverworldTextModeSwitch
	call .ClearBGMap
	call .PushAttrmap
	ret

.ClearBGMap:
	ld a, HIGH(vBGMap0)
	ld [wBGMapAnchor + 1], a
	xor a ; LOW(vBGMap0)
	ld [wBGMapAnchor], a
	ldh [hSCY], a
	ldh [hSCX], a
	farcall ApplyBGMapAnchorToObjects

	ldh a, [rVBK]
	push af
	ld a, $1
	ldh [rVBK], a

	xor a
	ld bc, vBGMap1 - vBGMap0
	hlbgcoord 0, 0
	call ByteFill

	pop af
	ldh [rVBK], a

	ld a, "■"
	ld bc, vBGMap1 - vBGMap0
	hlbgcoord 0, 0
	call ByteFill
	ret

.PushAttrmap:
	decoord 0, 0
	call .copy
	ldh a, [hCGB]
	and a
	ret z

	decoord 0, 0, wAttrmap
	ld a, $1
	ldh [rVBK], a
.copy
	hlbgcoord 0, 0
	ld c, SCREEN_WIDTH
	ld b, SCREEN_HEIGHT
.row
	push bc
.column
	ld a, [de]
	inc de
	ld [hli], a
	dec c
	jr nz, .column
	ld bc, BG_MAP_WIDTH - SCREEN_WIDTH
	add hl, bc
	pop bc
	dec b
	jr nz, .row
	ld a, $0
	ldh [rVBK], a
	ret

LoadMapGraphics::
	call LoadMapTileset
	;call LoadTilesetGFX
	xor a
	ldh [hMapAnims], a
	xor a
	ldh [hTileAnimFrame], a
	farcall RefreshSprites
	call LoadFontsExtra
	farcall LoadOverworldFont
	ret

LoadMapPalettes::
	ld b, SCGB_MAPPALS
	jp GetSGBLayout

RefreshMapSprites::
	call ClearSprites
	farcall InitMapNameSign
	call GetMovementPermissions
	farcall RefreshPlayerSprite
	farcall CheckUpdatePlayerSprite
	ld hl, wPlayerSpriteSetupFlags
	bit PLAYERSPRITESETUP_SKIP_RELOAD_GFX_F, [hl]
	jr nz, .skip
	ld hl, wVramState
	set 0, [hl]
	call SafeUpdateSprites
.skip
	ld a, [wPlayerSpriteSetupFlags]
	and (1 << PLAYERSPRITESETUP_FEMALE_TO_MALE_F) | (1 << 3) | (1 << 4)
	ld [wPlayerSpriteSetupFlags], a
	ret