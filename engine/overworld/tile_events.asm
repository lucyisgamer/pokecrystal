CheckGrassCollision::
	ld a, [wPlayerTile]
	ld hl, .blocks
	ld de, 1
	call IsInArray
	ret

.blocks
	db COLL_CUT_08
	db COLL_TALL_GRASS
	db COLL_LONG_GRASS
	db COLL_CUT_28
	db COLL_WATER
	db COLL_GRASS_48
	db COLL_GRASS_49
	db COLL_GRASS_4A
	db COLL_GRASS_4B
	db COLL_GRASS_4C
	db -1

CheckCutCollision:
	ld a, c
	ld hl, .blocks
	ld de, 1
	call IsInArray
	ret

.blocks
	db COLL_CUT_TREE
	db COLL_CUT_TREE_1A
	db COLL_TALL_GRASS_10
	db COLL_TALL_GRASS
	db COLL_LONG_GRASS
	db COLL_LONG_GRASS_1C
	db -1

GetWarpSFX::
	ld a, [wPlayerTile]
	ld de, SFX_ENTER_DOOR
	cp COLL_DOOR
	ret z
	ld de, SFX_WARP_TO
	cp COLL_WARP_PANEL
	ret z
	ld de, SFX_EXIT_BUILDING
	ret
