PARTY_LENGTH EQU 6

MAX_ITEMS     EQU 20
MAX_BALLS     EQU 12
MAX_KEY_ITEMS EQU 25
MAX_PC_ITEMS  EQU 50

; strings
PLAYER_NAME_LENGTH EQU 8
BOX_NAME_LENGTH EQU 9
PKMN_NAME_LENGTH EQU 11
MOVE_NAME_LENGTH EQU 13
ITEM_NAME_LENGTH EQU 13
TRAINER_CLASS_NAME_LENGTH EQU 13
NAME_LENGTH EQU 11
LV_CHAR EQU $6e

; GetName types
PKMN_NAME     EQU 1
MOVE_NAME     EQU 2
ITEM_NAME     EQU 4
PARTY_OT_NAME EQU 5
ENEMY_OT_NAME EQU 6
TRAINER_NAME  EQU 7

; hp
HP_GREEN  EQU 0
HP_YELLOW EQU 1
HP_RED    EQU 2

; boxes
MONS_PER_BOX EQU 20
NUM_BOXES    EQU 14

; mail
MAIL_STRUCT_LENGTH EQU $2f
MAILBOX_CAPACITY   EQU 10
MAIL_MSG_LENGTH    EQU $20

; hall of fame
HOF_MON_LENGTH = 1 + 2 + 2 + 1 + (PKMN_NAME_LENGTH +- 1) ; species, id, dvs, level, nick
HOF_LENGTH = 1 + HOF_MON_LENGTH * PARTY_LENGTH + 1 ; win count, party, terminator
NUM_HOF_TEAMS = 30


; flag manipulation
RESET_FLAG EQU 0
SET_FLAG   EQU 1
CHECK_FLAG EQU 2

; Boolean checks
FALSE EQU 0
TRUE  EQU 1

; joypad

	const_def
	const A_BUTTON_F
	const B_BUTTON_F
	const SELECT_F
	const START_F
	const D_RIGHT_F
	const D_LEFT_F
	const D_UP_F
	const D_DOWN_F

NO_INPUT   EQU %00000000
A_BUTTON   EQU 1 << A_BUTTON_F
B_BUTTON   EQU 1 << B_BUTTON_F
SELECT     EQU 1 << SELECT_F
START      EQU 1 << START_F
D_RIGHT    EQU 1 << D_RIGHT_F
D_LEFT     EQU 1 << D_LEFT_F
D_UP       EQU 1 << D_UP_F
D_DOWN     EQU 1 << D_DOWN_F

BUTTONS    EQU A_BUTTON | B_BUTTON | SELECT | START
D_PAD      EQU D_RIGHT | D_LEFT | D_UP | D_DOWN

R_DPAD     EQU %00100000
R_BUTTONS  EQU %00010000

; screen
HP_BAR_LENGTH EQU 6
HP_BAR_LENGTH_PX EQU HP_BAR_LENGTH * 8
EXP_BAR_LENGTH EQU 8
EXP_BAR_LENGTH_PX EQU EXP_BAR_LENGTH * 8

SCREEN_WIDTH EQU 20
SCREEN_HEIGHT EQU 18
SCREEN_WIDTH_PX EQU SCREEN_WIDTH * 8
SCREEN_HEIGHT_PX EQU SCREEN_HEIGHT * 8

BG_MAP_WIDTH  EQU 32
BG_MAP_HEIGHT EQU 32

TILE_WIDTH EQU 8


; movement
STEP_SLOW EQU 0
STEP_WALK EQU 1
STEP_BIKE EQU 2
STEP_LEDGE EQU 3
STEP_ICE EQU 4
STEP_TURN EQU 5
STEP_BACK_LEDGE EQU 6
STEP_WALK_IN_PLACE EQU 7


; ai
CONTEXT_USE_F      EQU 6
UNKNOWN_USE_F      EQU 5
ALWAYS_USE_F       EQU 4
SWITCH_SOMETIMES_F EQU 2
SWITCH_RARELY_F    EQU 1
SWITCH_OFTEN_F     EQU 0

CONTEXT_USE        EQU 1 << CONTEXT_USE_F
UNKNOWN_USE        EQU 1 << UNKNOWN_USE_F
ALWAYS_USE         EQU 1 << ALWAYS_USE_F
SWITCH_SOMETIMES   EQU 1 << SWITCH_SOMETIMES_F
SWITCH_RARELY      EQU 1 << SWITCH_RARELY_F
SWITCH_OFTEN       EQU 1 << SWITCH_OFTEN_F
SPRITE_GFX_LIST_CAPACITY EQU $20

const_value = 1
	const MOM_ITEM
	const MOM_DOLL

BATTLETOWER_NROFPKMNS EQU 3
BATTLETOWER_TRAINERDATALENGTH EQU $24
BATTLETOWER_NROFTRAINERS EQU 7
BATTLETOWER_NRMONSPERLEVELBRACKET EQU BATTLETOWER_NROFPKMNS * BATTLETOWER_NROFTRAINERS
BATTLE_TOWER_STRUCT_LENGTH EQU $e0 ; NAME_LENGTH + 3 * (PARTYMON_STRUCT_LENGTH + PKMN_NAME_LENGTH) + BATTLETOWER_TRAINERDATALENGTH

NUM_WILDMONS_PER_AREA_TIME_OF_DAY EQU 7
WILDMON_GRASS_STRUCTURE_LENGTH EQU 2 + 3 * (1 + 2 * NUM_WILDMONS_PER_AREA_TIME_OF_DAY)

MOBILE_EVENT_OBJECT_GS_BALL EQU $b

MALE EQU 0
FEMALE EQU 1

PRINTNUM_MONEY_F        EQU 5
PRINTNUM_RIGHTALIGN_F   EQU 6
PRINTNUM_LEADINGZEROS_F EQU 7

PRINTNUM_MONEY          EQU 1 << PRINTNUM_MONEY_F
PRINTNUM_RIGHTALIGN     EQU 1 << PRINTNUM_RIGHTALIGN_F
PRINTNUM_LEADINGZEROS   EQU 1 << PRINTNUM_LEADINGZEROS_F

const_value = 1
	const HAPPINESS_GAINLEVEL         ; 01
	const HAPPINESS_USEDITEM          ; 02
	const HAPPINESS_USEDXITEM         ; 03
	const HAPPINESS_GYMBATTLE         ; 04
	const HAPPINESS_LEARNMOVE         ; 05
	const HAPPINESS_FAINTED           ; 06
	const HAPPINESS_POISONFAINT       ; 07
	const HAPPINESS_BEATENBYSTRONGFOE ; 08
	const HAPPINESS_YOUNGCUT1         ; 09
	const HAPPINESS_YOUNGCUT2         ; 0a
	const HAPPINESS_YOUNGCUT3         ; 0b
	const HAPPINESS_OLDERCUT1         ; 0c
	const HAPPINESS_OLDERCUT2         ; 0d
	const HAPPINESS_OLDERCUT3         ; 0e
	const HAPPINESS_BITTERPOWDER      ; 0f
	const HAPPINESS_ENERGYROOT        ; 10
	const HAPPINESS_REVIVALHERB       ; 11
	const HAPPINESS_MASSAGE           ; 12
	const HAPPINESS_GAINLEVELATHOME   ; 13
	

	const_def
	const LINK_NULL
	const LINK_TIMECAPSULE
	const LINK_TRADECENTER
	const LINK_COLOSSEUM
	const LINK_MOBILE

SERIAL_TIMECAPSULE EQU $60
SERIAL_TRADECENTER EQU $70
SERIAL_BATTLE      EQU $80

HMENURETURN_SCRIPT EQU %10000000
HMENURETURN_ASM    EQU %11111111

NUM_MON_SUBMENU_ITEMS EQU 8

	const_def
	const ZEPHYRBADGE
	const HIVEBADGE
	const PLAINBADGE
	const FOGBADGE
	const MINERALBADGE
	const STORMBADGE
	const GLACIERBADGE
	const RISINGBADGE
NUM_JOHTO_BADGES EQU const_value
	const_def
	const BOULDERBADGE
	const CASCADEBADGE
	const THUNDERBADGE
	const RAINBOWBADGE
	const SOULBADGE
	const MARSHBADGE
	const VOLCANOBADGE
	const EARTHBADGE
NUM_KANTO_BADGES EQU const_value
NUM_BADGES EQU NUM_JOHTO_BADGES + NUM_KANTO_BADGES

	const_def
	const SPRITE_ANIM_SEQ_00
	const SPRITE_ANIM_SEQ_01
	const SPRITE_ANIM_SEQ_02
	const SPRITE_ANIM_SEQ_03
	const SPRITE_ANIM_SEQ_04
	const SPRITE_ANIM_SEQ_05
	const SPRITE_ANIM_SEQ_06
	const SPRITE_ANIM_SEQ_07
	const SPRITE_ANIM_SEQ_08
	const SPRITE_ANIM_SEQ_09
	const SPRITE_ANIM_SEQ_0A
	const SPRITE_ANIM_SEQ_0B
	const SPRITE_ANIM_SEQ_0C
	const SPRITE_ANIM_SEQ_0D
	const SPRITE_ANIM_SEQ_0E
	const SPRITE_ANIM_SEQ_0F
	const SPRITE_ANIM_SEQ_10
	const SPRITE_ANIM_SEQ_11
	const SPRITE_ANIM_SEQ_12
	const SPRITE_ANIM_SEQ_13
	const SPRITE_ANIM_SEQ_14
	const SPRITE_ANIM_SEQ_15
	const SPRITE_ANIM_SEQ_16
	const SPRITE_ANIM_SEQ_17
	const SPRITE_ANIM_SEQ_18
	const SPRITE_ANIM_SEQ_19
	const SPRITE_ANIM_SEQ_1A
	const SPRITE_ANIM_SEQ_1B
	const SPRITE_ANIM_SEQ_1C
	const SPRITE_ANIM_SEQ_1D
	const SPRITE_ANIM_SEQ_1E
	const SPRITE_ANIM_SEQ_1F
	const SPRITE_ANIM_SEQ_20
	const SPRITE_ANIM_SEQ_21
	const SPRITE_ANIM_SEQ_22

	const_def
	const SPRITE_ANIM_INDEX_00
	const SPRITE_ANIM_INDEX_01
	const SPRITE_ANIM_INDEX_02
	const SPRITE_ANIM_INDEX_03
	const SPRITE_ANIM_INDEX_04
	const SPRITE_ANIM_INDEX_05
	const SPRITE_ANIM_INDEX_06
	const SPRITE_ANIM_INDEX_07
	const SPRITE_ANIM_INDEX_08
	const SPRITE_ANIM_INDEX_09
	const SPRITE_ANIM_INDEX_0A
	const SPRITE_ANIM_INDEX_0B
	const SPRITE_ANIM_INDEX_0C
	const SPRITE_ANIM_INDEX_0D
	const SPRITE_ANIM_INDEX_0E
	const SPRITE_ANIM_INDEX_0F
	const SPRITE_ANIM_INDEX_10
	const SPRITE_ANIM_INDEX_11
	const SPRITE_ANIM_INDEX_12
	const SPRITE_ANIM_INDEX_13
	const SPRITE_ANIM_INDEX_14
	const SPRITE_ANIM_INDEX_15
	const SPRITE_ANIM_INDEX_16
	const SPRITE_ANIM_INDEX_17
	const SPRITE_ANIM_INDEX_18
	const SPRITE_ANIM_INDEX_19
	const SPRITE_ANIM_INDEX_1A
	const SPRITE_ANIM_INDEX_1B
	const SPRITE_ANIM_INDEX_1C
	const SPRITE_ANIM_INDEX_1D
	const SPRITE_ANIM_INDEX_1E
	const SPRITE_ANIM_INDEX_1F
	const SPRITE_ANIM_INDEX_20
	const SPRITE_ANIM_INDEX_21
	const SPRITE_ANIM_INDEX_22
	const SPRITE_ANIM_INDEX_23
	const SPRITE_ANIM_INDEX_24
	const SPRITE_ANIM_INDEX_25
	const SPRITE_ANIM_INDEX_26
	const SPRITE_ANIM_INDEX_27
	const SPRITE_ANIM_INDEX_28
	const SPRITE_ANIM_INDEX_29
	const SPRITE_ANIM_INDEX_2A
	const SPRITE_ANIM_INDEX_2B
	const SPRITE_ANIM_INDEX_2C

NUM_KANA EQU $2d
