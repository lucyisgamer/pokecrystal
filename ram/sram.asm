SECTION "Scratch", SRAM

sScratch:: ds $60 tiles


SECTION "SRAM Bank 0", SRAM

sPartyMail::
; sPartyMon1Mail - sPartyMon6Mail
for n, 1, PARTY_LENGTH + 1
sPartyMon{d:n}Mail:: mailmsg sPartyMon{d:n}Mail
endr

sPartyMailBackup::
; sPartyMon1MailBackup - sPartyMon6MailBackup
for n, 1, PARTY_LENGTH + 1
sPartyMon{d:n}MailBackup:: mailmsg sPartyMon{d:n}MailBackup
endr

sMailboxCount:: db
sMailboxes::
; sMailbox1 - sMailbox10
for n, 1, MAILBOX_CAPACITY + 1
sMailbox{d:n}:: mailmsg sMailbox{d:n}
endr

sMailboxCountBackup:: db
sMailboxesBackup::
; sMailbox1Backup - sMailbox10Backup
for n, 1, MAILBOX_CAPACITY + 1
sMailbox{d:n}Backup:: mailmsg sMailbox{d:n}Backup
endr

sMysteryGiftData::
sMysteryGiftItem:: db
sMysteryGiftUnlocked:: db
sBackupMysteryGiftItem:: db
sNumDailyMysteryGiftPartnerIDs:: db
sDailyMysteryGiftPartnerIDs:: ds MAX_MYSTERY_GIFT_PARTNERS * 2
sMysteryGiftDecorationsReceived:: flag_array NUM_NON_TROPHY_DECOS
	ds 4
sMysteryGiftTimer:: dw
	ds 1
sMysteryGiftTrainerHouseFlag:: db
sMysteryGiftPartnerName:: ds NAME_LENGTH
sMysteryGiftUnusedFlag:: db
sMysteryGiftTrainer:: ds wMysteryGiftTrainerEnd - wMysteryGiftTrainer
sBackupMysteryGiftItemEnd::

	ds $30

sRTCStatusFlags:: db
	ds 7
sLuckyNumberDay:: db
sLuckyIDNumber::  dw


SECTION "Backup Save", SRAM

sBackupOptions:: ds wOptionsEnd - wOptions

sBackupCheckValue1:: db ; loaded with SAVE_CHECK_VALUE_1, used to check save corruption

sBackupGameData::
sBackupPlayerData::  ds wPlayerDataEnd - wPlayerData
sBackupCurMapData::  ds wCurMapDataEnd - wCurMapData
sBackupPokemonData:: ds wPokemonDataEnd - wPokemonData
sBackupGameDataEnd::

	ds $18a

sBackupChecksum:: dw

sBackupCheckValue2:: db ; loaded with SAVE_CHECK_VALUE_2, used to check save corruption

sStackTop:: dw

if DEF(_DEBUG)
sRTCHaltCheckValue:: dw
sSkipBattle:: db
sDebugTimeCyclesSinceLastCall:: db
sOpenedInvalidSRAM:: db
sIsBugMon:: db
endc


SECTION "Save", SRAM

sOptions:: ds wOptionsEnd - wOptions

sCheckValue1:: db ; loaded with SAVE_CHECK_VALUE_1, used to check save corruption

sGameData::
sPlayerData::  ds wPlayerDataEnd - wPlayerData
sCurMapData::  ds wCurMapDataEnd - wCurMapData
sPokemonData:: ds wPokemonDataEnd - wPokemonData
sGameDataEnd::

	ds $18a

sChecksum:: dw

sCheckValue2:: db ; loaded with SAVE_CHECK_VALUE_2, used to check save corruption


SECTION "Active Box", SRAM

sBox:: box sBox

	ds $100


SECTION "Link Battle Data", SRAM

sLinkBattleStats::
sLinkBattleWins::   dw
sLinkBattleLosses:: dw
sLinkBattleDraws::  dw

sLinkBattleRecord::
; sLinkBattleRecord1 - sLinkBattleRecord5
for n, 1, NUM_LINK_BATTLE_RECORDS + 1
sLinkBattleRecord{d:n}:: link_battle_record sLinkBattleRecord{d:n}
endr
sLinkBattleStatsEnd::


SECTION "SRAM Hall of Fame", SRAM

sHallOfFame::
; sHallOfFame1 - sHallOfFame30
for n, 1, NUM_HOF_TEAMS + 1
sHallOfFame{d:n}:: hall_of_fame sHallOfFame{d:n}
endr
sHallOfFameEnd::


SECTION "SRAM Crystal Data", SRAM

sMobileEventIndex:: db

sCrystalData:: ds wCrystalDataEnd - wCrystalData

sMobileEventIndexBackup:: db


; The PC boxes will not fit into one SRAM bank,
; so they use multiple SECTIONs
DEF box_n = 0
MACRO boxes
	rept \1
		DEF box_n += 1
	sBox{d:box_n}:: box sBox{d:box_n}
	endr
ENDM

SECTION "Boxes 1-7", SRAM

; sBox1 - sBox7
	boxes 7

SECTION "Boxes 8-14", SRAM

; sBox8 - sBox14
	boxes 7

; All 14 boxes fit exactly within 2 SRAM banks
	assert box_n == NUM_BOXES, \
		"boxes: Expected {d:NUM_BOXES} total boxes, got {d:box_n}"

SECTION "Chunk Load LUT", SRAM
	sCharblockLUT:: ds $200
	sCharblockIDs:: ds $200
	sCharblockLUTEnd::
	sUsedCharblockFlags:: ds $80
	sTileRefrenceCounts:: ds $160 ; refrence counters for each tile
	sTileRefrenceCountsEnd::
	sAnimatedTileRefrenceCounts:: ds $20
	sAnimatedTileRefrenceCountsEnd::
	sTileIDLUT:: ds $2C0
	sTileIDLUTEnd::
	sAnimatedTileIDLUT:: ds $40
	sAnimatedTileIDLUTEnd::
	sTileRefrenceLoopCounter::
	sCharblockResolveCounter:: db
	sDirtyCharblockFlags:: ds $20
	ds $80 - $21
	sTileAnimationTables:: ds $80 ; end of the table is now aligned to a $100 boundary
	sTileAnimationTablesEnd::
	; 5740 bytes free

SECTION "Charblock Data", SRAM
	sCharblockData::
	sCharblockTiles:: ds $1000
	sCharblockAttributes:: ds $1000
