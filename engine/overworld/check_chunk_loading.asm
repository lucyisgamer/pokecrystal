CheckChunkLoading:: ; Uses the player's coordinates to check what chunks need to be loaded and loads them
	ld hl, wXCoord
	ld a, [hli]
	ld b, a
	ld a, [hli]
	ld c, a
	ld a, [hli]
	ld d, a
	ld e, [hl]
	
	ld h, $00 ;      7      6      5       4       3       2       1       0
			  ;      x      x      up     down    left   right   original flags  

	call GetChunkCoords ; get the coordinates of our current chunk and shove them into the correct places
	ld a, [wYCoord + 1]
	and a, $3F
	ld c, a
	and a, $1F
	ld e, a
	ld a, [wXCoord + 1]
	and a, $3F
	ld b, a
	and a, $1F
	ld d, a

	bit $05, b
	jr z, .skip1
	set $00, h
.skip1:
	bit $05, c
	jr z, .skip2
	set $01, h ; put the current chunk quadrant into the lower two bits of h
.skip2:
	cp a, $08 ; a still has d loaded into it at this point
	jr c, .LeftSide
	cp a, $18
	jr nc, .RightSide
	; Fall through if we are in the middle horizontally
.Vertical:
	ld a, e
	cp a, $08
	jr c, .TopSide
	cp a, $18
	jr nc, .BottomSide
	ld a, h
	and a
    ret z; If none of these evaluated through we must be in the middle of the chunk, so return early
	jr .Edge ; If only left or right got activated then we must be on an edge

.TopSide:
	ld a, h
	and a, $FC
    set $05, h
	jr z, .TopEdge
	jr .Corner
.BottomSide:
	ld a, h
	and a, $FC
	set $04, h
	jr z, .BottomEdge
	jr .Corner
.LeftSide:
	set $03, h
	jr .Vertical
.RightSide:
	set $02, h
	jr .Vertical

	; By this point de is no longer in use
.Edge:
	bit $03, h
	jr nz, .LeftEdge
	bit $02, h
	jr nz, .RightEdge
	ret ; Should never reach this under any circumstances

.Corner:
	bit $05, h
	jr nz, .TopCorners
	bit $04, h
	jr nz, .BottomCorners
	ret ; Should never reach this under any circumstances

.TopEdge:
	ld a, [wChunkY]
	dec a
	ld [wChunkY], a
	jr .VerticalEdges

.BottomEdge:
	ld a, [wChunkY]
	inc a
	ld [wChunkY], a ; fall through to .VerticalEdges

.VerticalEdges:
	ld a, $02
	xor a, h
	ld h, a
	and a, $3
	ld [wChunkQuadrant], a
	jr LoadChunk

.LeftEdge:
	ld a, [wChunkX]
	dec a
	ld [wChunkX], a
	jr .HorizontalEdges

.RightEdge:
	ld a, [wChunkX]
	inc a
	ld [wChunkX], a
	jr .HorizontalEdges

.HorizontalEdges:
	ld a, $01
	xor a, h
	ld h, a
	and a, $3
	ld [wChunkQuadrant], a
	jr LoadChunk

.TopCorners:
	bit $03, h
	jr nz, .TopLeftCorner
	bit $02, h
	jr nz, .TopRightCorner
	ret ; Should never be reached

.BottomCorners:
	bit $03, h
	jr nz, .BottomLeftCorner
	bit $02, h
	jr nz, .BottomRightCorner
	ret ; Should never be reached

.TopLeftCorner:
	call .TopEdge
	call .LeftEdge
	call .BottomEdge
	ret

.TopRightCorner:
	call .TopEdge
	call .RightEdge
	call .BottomEdge
	ret

.BottomLeftCorner:
	call .BottomEdge
	call .LeftEdge
	call .TopEdge
	ret
	
.BottomRightCorner:
	call .BottomEdge
	call .RightEdge
	call .TopEdge
	ret

LoadChunk:
	push hl ; de is already unused by this point
	ld hl, wChunkCoordsArray
	ld a, [wChunkQuadrant]
	sla a
	ld d, $00
	ld e, a
	add hl, de
	ld a, [wChunkX]
	cp a, [hl]
	jr nz, .outdated
	inc hl
	ld a, [wChunkY]
	cp a, [hl]
	jr nz, .outdated

	pop hl
	ret

.outdated:
	farcall CopyChunkHeader
	farcall MarkUnrefrencedBlocks
	farcall UpdateBlockset
	farcall UpdateTileset
	farcall LoadChunkToMapBuffer
	pop hl
	ret
	