;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SMW Bob-omb (sprite 0D), by mellonpizza
;;
;; This is a disassembly of sprite 0D in SMW, the bob-omb.
;;
;; Uses first extra bit: YES
;; 
;; If the extra bit is set, the sprite spawns in its stunned state right away.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!ExplosionSFX = $09 ; SFX number to load when the bomb explodes
!ExplosionBank = $1DFC ; SFX bank to load SFX number into
!FuseTimer = $FF ; How long to set the fuse when the bomb spawns, or is kicked
!ExplosionTimer = $40 ; How long the explosion lasts for the bobomb

!Disable_Picking_Up = 0 ; Will not allow the player to pick up the bomb

BobombTiles:
db $CA,$CC

BobombProps:
db $40,$00

;; Defines stolen from Dyzen
!TrueFrameCounter = $13
!EffectiveFrameCounter = $14

!ButtonPressed_BYETUDLR = $15
!ButtonDown_BYETUDLR = $16
!ButtonPressed_AXLR0000 = $17
!ButtonDown_AXLR0000 = $18

!Layer1X = $1A

!PlayerX = $94
!PlayerY = $96
!PlayerXSpeed = $7B
!PlayerYSpeed = $7D
!PowerUp = $19
!PlayerInAirFlag = $72
!PlayerDuckingFlag = $73
!PlayerClimbingFlag = $74
!PlayerDirection = $76
!PlayerRideYoshi = $187A|!Base2

!sprite_direction = !157C
!sprite_anim_counter = !1570
!sprite_turn_timer = !15AC

!LockAnimationFlag = $9D

!OAM_XPos = $0300|!Base2
!OAM_YPos = $0301|!Base2
!OAM_Tile = $0302|!Base2
!OAM_Prop = $0303|!Base2

print "INIT ",pc
	;; Set timer for FF
	lda.b #!FuseTimer : sta !1540,x
	
	;; Set to stunned if extra bit
	lda !extra_bits,x : and #$04 : beq +
	lda #$09 : sta !14C8,x
	+
	jsr FaceMario
	rtl

FaceMario:
	%SubHorzPos()
	tya : sta !sprite_direction,x
	rts

IsOnGround:
	lda !sprite_blocked_status,x : and #$04 : rts

IsTouchingCeiling:
	lda !sprite_blocked_status,x : and #$08 : rts

SetAnimationFrame:
	;; Change animation image every 8 cycles
	inc !1570,x
	lda !1570,x : lsr #3 : and #$01 : sta !1602,x
	rts

Spr0to13SpeedX:
	db $0C,$F4

;;;;;;;;;;;;;;
;;			;;
;;	MAIN	;;
;;			;;
;;;;;;;;;;;;;;

print "MAIN ",pc
	;; Handle which state to manage
	phb : phk : plb
	lda !sprite_status,x : cmp #$08 : bne +
	jsr Bobomb_Main : bra ++
	+
	cmp #$09 : bne +
	jsr Bobomb_Stunned : bra ++
	+
	jsr HandleSprCarried
	++
	;; Set sprite to handle vanilla settings if needed
	ldy #$00
	lda !sprite_status,x : cmp #$08 : bcc +
	ldy #$80
	+
	tya : sta !extra_prop_2,x
	plb : rtl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;							;;
;;		Main routine		;;
;;							;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Bobomb_Main:
	;; Handle explosion if time to explode
	lda !1534,x : bne ExplodeBomb

	;; Branch if not on detonation timer
	lda !1540,x : bne Spr0to13Start

	;; Set sprite status to stunned
	;; Time until explosion = #$40
	lda #$09 : sta !14C8,x
	lda #$40 : sta !1540,x
	jmp Bobomb_Draw

ExplodeBomb:
	phb : lda #$02 : pha : plb
	jsl $028086|!BankB
	plb : rts

Spr0to13Start:
	;; jump to drawing sprite if locked
	lda !LockAnimationFlag : beq +
	jmp .graphics
	+
	;; Branch if not on ground
	jsr IsOnGround : beq .In_Air

	;; Set X speed factoring slope
	ldy !sprite_direction,x
	lda Spr0to13SpeedX,y : eor !sprite_slope,x : asl a
	lda Spr0to13SpeedX,y : bcc +
	clc : adc !sprite_slope,x
	+
	sta !sprite_speed_x,x
.In_Air
	;; Clear X speed if touching object in direction of motion
	ldy !sprite_direction,x : tya : inc a
	and !sprite_blocked_status,x : and #$03 : beq +
	stz !sprite_speed_x,x
	+
	;; Clear Y speed if touching ceiling
	jsr IsTouchingCeiling : beq +
	stz !sprite_speed_y,x
	+
	;; Offscreen + Speed update
	lda #$00 : %SubOffScreen()
	jsl $01802A|!BankB
	jsr SetAnimationFrame

	;; Branch if not on ground
	jsr IsOnGround : beq .SpriteInAir

	;; Yspeedthings
	lda !sprite_blocked_status,x : bmi ++
	lda #$00
	ldy !sprite_slope,x : beq +
	++
	lda #$18
	+
	sta !sprite_speed_y,x
	stz !151C,x

	;; Turn around when timer is set as needed
	lda !sprite_anim_counter,x : and #$7F : bne +
	lda !sprite_direction,x : pha
	jsr FaceMario
	pla : cmp !sprite_direction,x : beq +
	lda #$08 : sta !sprite_turn_timer,x
	+
	bra +

.SpriteInAir
	stz !1570,x
	+
	;; Interact with mario/sprites
	lda !sprite_tweaker_167a,x : pha
	and #$7F : sta !sprite_tweaker_167a,x
	jsl $01A7DC|!BankB
	pla : sta !sprite_tweaker_167a,x
	jsl $018032|!BankB

	;; Flip sprite direction if touching block from side
	lda !sprite_direction,x : inc a
	and !sprite_blocked_status,x : and #$03 : beq +
	jsr FlipSpriteDir
	+
.graphics
	jmp Bobomb_Draw

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;							;;
;;		Stunned routine		;;
;;							;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Bobomb_Stunned:
	;; Always draw a specific frame
	stz !1602,x
	lda !LockAnimationFlag : beq +
	jmp Bobomb_Draw
	+
	;; Offscreen moved from bottom of sprite routine
	%SubOffScreen()

	;; Update sprite position with gravity
	jsl $01802A|!BankB

	;; Handle touching the ground
	jsr IsOnGround : beq .Not_Touching_Ground
	jsr Handle_Touch_Ground

.Not_Touching_Ground
	;; Handle touching the ceiling
	jsr IsTouchingCeiling : beq .Not_Touching_Ceiling

	;; Set downward speed
	lda #$10 : sta !sprite_speed_y,x

	;; Branch away if touching the side
	lda !sprite_blocked_status,x : and #$03 : bne .Handle_Touch_Side

	;; Offset X position by #$08 and store to block info
	lda !sprite_x_low,x : clc : adc #$08 : sta $9A
	lda !sprite_x_high,x : adc #$00 : sta $9B

	;; Set high nybble of Y position to block info
	lda !sprite_y_low,x : and #$F0 : sta $98
	lda !sprite_y_high,x : sta $99

	;; Store "layer2 below blocked" status to layer handler
	LDA !sprite_blocked_status,x : asl #3 : rol : and #$01 : sta $1933|!Base2

	;; Run routine to handle knocking into block
	ldy #$00 : lda $1868|!Base2 : jsl $00F160|!BankB
	lda #$08 : sta !1FE2,x
	bra .Not_Touching_Side

.Not_Touching_Ceiling
	lda !sprite_blocked_status,x : and #$03 : beq .Not_Touching_Side

.Handle_Touch_Side
	jsr Hit_wall

	lda !sprite_speed_x,x : asl : php
	ror !sprite_speed_x,x : plp : ror !sprite_speed_x,x

.Not_Touching_Side
	;; Interact with sprites.
	jsl $018032|!BankB
	
	;; Interact with player. 'PowInteract' extracted from default interaction handler.
	jsl $01A7DC|!BankB : bcc +
	jsr BombInteract
	+
	jsr Check_Fuse ; handle bomb explosion ticking
	jmp Bobomb_Draw

BombInteract:
	;; Handle interaction while mario has star
	lda $1490|!Base2 : beq +
	jsl $01AB6F|!BankB
	%Star()
	rts
	+
	;; Check disable interaction timer
	stz $18D2|!Base2
	lda !154C,x : bne .return2
	lda #$08 : sta !154C,x
	
	;; Test whether to handle being spinjumped on
	lda $140D|!Base2 : ora !PlayerRideYoshi : beq .NoSpinjump
	lda !PlayerYSpeed : bmi .NoSpinjump
	
	;; Display contact graphic and set speed
	jsl $01AB99|!BankB
	lda #$F8 : sta !PlayerYSpeed
	
	;; Only factor high bounding if player is on yoshi
	lda !PlayerRideYoshi : beq +
	jsl $01AA33|!BankB
	+
	;; Setup sprite state 4 and reset timer
	lda #$04 : sta !sprite_status,x
	lda #$1F : sta !1540,x
	
	;; Generate smoke
	jsl $07FC3B|!BankB
	
	;; Factor enemies stomped
	phy
	lda $1697|!Base2 : clc : adc !1626,x
	inc $1697|!Base2
	tay : iny : cpy #$08 : bcs +
	lda .stomp_scores-1,y
	sta $1DF9|!Base2
	+
	tya : cmp #$08 : bcc +
	lda #$08
	+
	;; Display score and generate sfx
	jsl $02ACE5|!BankB
	ply
	lda #$08 : sta $1DF9|!Base2
.return2
	rts
	
.stomp_scores
db $13,$14,$15,$16,$17,$18,$19

.NoSpinjump
if !Disable_Picking_Up == 0
	;; Test if X/Y button held
	lda $15 : and #$40 : beq .No_Carry

	;; Branch if carrying an enemy, or on yoshi
	lda $1470|!Base2 : ora !PlayerRideYoshi : bne .No_Carry

	;; Set sprite status to being carried and set mario to carry an item
	lda #$0B : sta !sprite_status,x
	inc $1470|!Base2

	;; Set pose to hold an item
	lda #$08 : sta $1498|!Base2
	rts
endif
.No_Carry
	;; handle grabbing/kicking shit
	lda #$03 : sta $1DF9|!Base2
	lda #$09 : sta !sprite_status,x
	lda.b #!FuseTimer : sta !1540,x
	lda #$10 : sta !154C,x
	%SubHorzPos()
	lda ShellSpeedX,y
	sta !sprite_speed_x,x
	rts

.return
	rts



Check_Fuse:
	;; Branch away if it's not time to explode
	lda !1540,x : cmp #$01 : bne .dont_explode

	;; SFX, explode status, explode timer, normal sprie status,
	lda.b #!ExplosionSFX : sta.w !ExplosionBank|!Base2
	lda #$01 : sta !1534,x
	lda.b #!ExplosionTimer : sta !1540,x
	lda #$08 : sta !sprite_status,x
	
	;; set to interact with other sprites
	lda !sprite_tweaker_1686,x : and #$F7 : sta !sprite_tweaker_1686,x
	
	;; set to default interaction
	lda !sprite_tweaker_167a,x : and #$7F : sta !sprite_tweaker_167a,x
	rts

.dont_explode
	;; If almost going to explode, flicker palette
	cmp #$40
	bcs .return
	asl : and #$0E : eor !sprite_oam_properties,x : sta !sprite_oam_properties,x
.return
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Bobomb hitting the ground routine	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Handle_Touch_Ground:
	;; Halve X speed
	lda !sprite_speed_x,x
	php : bpl +
	eor #$FF : inc a
	+
	lsr a
	plp : bpl +
	eor #$FF : inc a
	+
	sta !sprite_speed_x,x

	;; Preserve Y speed
	lda !sprite_speed_y,x
	pha

	;; Set y speed depending on whether the sprite is on a slope
	;; (SetSomeYSpeed??)
	lda !1588,x : bmi ++
	lda #$00 : ldy !sprite_slope,x : beq +
	++
	lda #$18
	+
	sta !sprite_speed_y,x

	;; use Yspeed to index a table of values to use as bouncing speeds
	pla
	lsr #2 : tay
	lda .Yspeed_bounce_table,y

	;; Do not change the sprite's speed adjusted previously,
	;; in the case that layer2 is touching from above.
	ldy !sprite_blocked_status,x : bmi +
	sta !sprite_speed_y,x
	+
	rts

.Yspeed_bounce_table
db $00,$00,$00,$F8,$F8,$F8,$F8,$F8
db $F8,$F7,$F6,$F5,$F4,$F3,$F2,$E8
db $E8,$E8,$E8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Bobomb hitting a wall routine	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Hit_wall:
	;; Play contact SFX
	lda #$01 : sta $1DF9|!Base2

	;; Invert speed and flip sprite direction
	lda !sprite_speed_x,x : eor #$FF : inc a : sta !sprite_speed_x,x
	lda !sprite_direction,x : eor #$01 : sta !sprite_direction,x

	;; Don't interact with blocks if offscreen
	lda !sprite_off_screen_horz,x : bne .return
	lda !sprite_x_low,x : sec : sbc !Layer1X
	clc : adc #$14 : cmp #$1C : bcc .return

	;; Store "layer2 side blocked" status to layer handler
	lda !1588,x : asl #2 : rol : and #$01 : sta $1933|!Base2

	;; Run routine to handle knocking into block
	ldy #$00 : lda $18A7|!Base2 : jsl $00F160|!BankB
	lda #$05 : sta !1FE2,x

.return
	rts

FlipSpriteDir:
	lda !15AC,x : bne .return
	lda #$08 : sta !15AC,x
	lda !sprite_speed_x,x : eor #$FF : inc a : sta !sprite_speed_x,x
	lda !sprite_direction,x : eor #$01 : sta !sprite_direction,x
.return
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;								;;
;;		Carried routine			;;
;;								;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HandleSprCarried:
	;; Handles all the logic for carrying sprites
	jsr Carried_Sprite_Main

	;; If player is in a way that the sprite needs to be drawn infront of them,
	;; set OAM index to zero.
	lda $13DD|!Base2 : bne +
	lda $1419|!Base2 : bne +
	lda $1499|!Base2 : beq ++
	+
	stz !sprite_oam_index,x
	++
	;; If signaled that player is to be drawn behind layers, set draw priority to #$10.
	ldy $1419|!Base2 : beq +
	jmp Bobomb_Draw_Behind
	+
	jmp Bobomb_Draw

Carried_Sprite_Main:
	;; call object interaction routine
	jsl $019138|!BankB

	;; Go back to stunned state if the player has a special animation,
	;; and yoshi isn't set to have a special value for drawing behind pipes
	lda $71 : cmp #$01 : bcc +
	lda $1419|!Base2 : bne +
	lda #$09 : sta !sprite_status,x
	rts
	+
	;; Return if the sprite has changed back into its normal state
	lda !sprite_status,x : cmp #$08 : bne +
	rts
	+
	lda !LockAnimationFlag : beq +
	jmp Attatch_Sprite_To_Player
	+
	JSR Check_Fuse ; handle bomb explosion ticking
	jsl $018032|!BankB

	;; Check if X/Y not held; if not, then release sprite
	lda $1419|!Base2 : bne +
	bit !ButtonPressed_BYETUDLR
	bvc ReleaseSprCarried
	+
	jmp Attatch_Sprite_To_Player

ReleaseSprCarried:
	;; Clear number of enemies killed.
	stz !1626,x
	
	;; Clear Y speed and set to stunned state.
	stz !sprite_speed_y,x
	lda #$09 : sta !sprite_status,x

	;; Branch to throw the sprite upward
	lda !ButtonPressed_BYETUDLR : and.B #$08 : bne TossUpSprCarried

	;; Branch to kick sprite left/right
	lda !ButtonPressed_BYETUDLR : and #$03 : bne KickSprCarried
	+
	;; Else, the sprite is to be dropped down.
	ldy !PlayerDirection
	lda $D1 : clc : adc Drop_Xoffset_Low,y : sta !sprite_x_low,x
	lda $D2 : adc Drop_Xoffset_High,y : sta !sprite_x_high,x
	%SubHorzPos()
	lda Drop_Xspeed,y : clc : adc !PlayerXSpeed : sta !sprite_speed_x,x
	bra StartKickPose

Drop_Xspeed:
db $FC,$04

Drop_Xoffset_Low:
db $F3,$0D
Drop_Xoffset_High:
db $FF,$00

TossUpSprCarried:
	;; Display contact graphic
	jsl $01AB6F|!BankB

	;; Set sprite speeds (-112 Y, Player/2 X)
	lda #$90 : sta !sprite_speed_y,x
	lda !PlayerXSpeed : sta !sprite_speed_x,x
	asl a : ror !sprite_speed_x,x
	bra StartKickPose

KickSprCarried:
	;; Set stun timer
	lda.b #!FuseTimer : sta !1540,x
	
	;; Display contact graphic
	jsl $01AB6F|!BankB

	;; Set sprite X speed
	ldy !PlayerDirection : lda !PlayerRideYoshi : beq +
	iny #2
	+
	lda ShellSpeedX,y : sta !sprite_speed_x,x
	eor !PlayerXSpeed : bmi StartKickPose
	lda !PlayerXSpeed : sta $00
	asl $00 : ror
	clc : adc ShellSpeedX,y : sta !sprite_speed_x,x

StartKickPose:
	;; Disable collisions with mario for 16 frames
	LDA #$10 : sta !154C,x

	;; Display kicking pose
	lda #$0C : sta $149A|!Base2
	rts

ShellSpeedX:
db $D2,$2E,$CC,$34

Attatch_Sprite_To_Player:
	;; Get index to table which will determine where in relation to the player,
	;; on the x axis the sprite will be moved to
	ldy #$00 : lda !PlayerDirection : bne +
	iny
	+
	lda $1499|!Base2 : beq +
	iny #2
	cmp #$05 : bcc +
	iny
	+
	;; if mario is facing the screen or climbing, use the final index
	lda $1419|!Base2 : beq +
	cmp #$02 : beq ++
	+
	lda $13DD|!Base2 : ora !PlayerClimbingFlag : beq +
	++
	ldy #$05
	+
	;; If the player is on a sprite that calculate's the player's position based
	;; on the current frame, then use $94-$97 to calculate the carried sprite's
	;; position. otherwise use $D1-D4. This should ensure the sprite never looks
	;; "disjointed" in relation to the player
	phy : ldy #$00
	lda $1471|!Base2 : cmp #$03 : beq +
	ldy #$3D
	+
	;; Store player positions to scratch ram
	lda $0094|!Base1,y : sta $00
	lda $0095|!Base1,y : sta $01
	lda $0096|!Base1,y : sta $02
	lda $0097|!Base1,y : sta $03
	ply
	lda $00 : clc : adc CarriedSpr_OffsetToPlayer_Low,y : sta !sprite_x_low,x
	lda $01 : adc CarriedSpr_OffsetToPlayer_High,y : sta !sprite_x_high,x

	lda #$0D : ldy !PlayerDuckingFlag : bne +
	ldy !PowerUp : bne ++
	+
	lda #$0F
	++
	ldy $1489|!Base2 : beq +
	lda #$0F
	+
	clc : adc $02 : sta !sprite_y_low,x
	lda $03 : adc #$00 : sta !sprite_y_high,x

	;; Set flags to indicate player is holding an item
	lda #$01 : sta $148F|!Base2 : sta $1470|!Base2
	rts

CarriedSpr_OffsetToPlayer_Low:
db $0B,$F5,$04,$FC,$04,$00

CarriedSpr_OffsetToPlayer_High:
db $00,$FF,$00,$FF,$00,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;	Bob-omb graphics routine	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Bobomb_Draw_Behind:
	lda #$10 : bra +
Bobomb_Draw:
	lda #$20
	+
	;; Is always 1 16x16 tile, there is a status to indicate which frame to draw
	ldy !sprite_direction,x
	ora BobombProps,y : ora !sprite_oam_properties,x : sta $02
	%GetDrawInfo()
	phx
	lda $00 : sta !OAM_XPos,y
	lda $01 : sta !OAM_YPos,y
	lda !1602,x : tax
	lda BobombTiles,x : sta !OAM_Tile,y
	lda $02 : sta !OAM_Prop,y
	plx
	lda #$00 : ldy #$02
	jsl $01B7B3|!BankB
	rts 