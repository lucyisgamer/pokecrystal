CheckGrassCollision:: ; set carry if player is touching grass collision
	ld a, [wPlayerCollision]
	cp a, COLL_LONG_GRASS
	ccf
	ret z ; branchless programming
	and a, $F0 ; we now just check for regular grass and water
	cp a, GRASS_NYBBLE
	ccf
	ret z
	cp a, WATER_NYBBLE
	ccf
	ret z
	and a
	ret

CheckCutCollision:
	ld a, c
	cp a, COLL_LONG_GRASS
	ccf
	ret z
	cp a, COLL_CUT_TREE
	ccf
	ret z
	and a, $F0
	cp a, GRASS_NYBBLE
	ccf
	ret z
	and a
	ret

.blocks
	db COLL_CUT_TREE
	db COLL_TALL_GRASS
	db COLL_LONG_GRASS

GetWarpSFX::
	ld a, [wPlayerCollision]
	ld de, SFX_ENTER_DOOR
	cp COLL_DOOR
	ret z
	ld de, SFX_WARP_TO
	cp COLL_WARP_PANEL
	ret z
	ld de, SFX_EXIT_BUILDING
	ret
