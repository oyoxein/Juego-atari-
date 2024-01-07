; Provided under the CC0 license. See the included LICENSE.txt for details.

 processor 6502
 include "vcs.h"
 include "macro.h"
 include "multisprite.h"
 include "2600basic_variable_redefs.h"
 ifconst bankswitch
  if bankswitch == 8
     ORG $1000
     RORG $D000
  endif
  if bankswitch == 16
     ORG $1000
     RORG $9000
  endif
  if bankswitch == 32
     ORG $1000
     RORG $1000
  endif
  if bankswitch == 64
     ORG $1000
     RORG $1000
  endif
 else
   ORG $F000
 endif

 ifconst bankswitch_hotspot
 if bankswitch_hotspot = $083F ; 0840 bankswitching hotspot
   .byte 234 ; stop unexpected bankswitches
 endif
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

FineAdjustTableBegin
	.byte %01100000		;left 6
	.byte %01010000
	.byte %01000000
	.byte %00110000
	.byte %00100000
	.byte %00010000
	.byte %00000000		;left 0
	.byte %11110000
	.byte %11100000
	.byte %11010000
	.byte %11000000
	.byte %10110000
	.byte %10100000
	.byte %10010000
	.byte %10000000		;right 8
FineAdjustTableEnd	=	FineAdjustTableBegin - 241

PFStart
 .byte 87,43,0,21,0,0,0,10
blank_pf
 .byte 0,0,0,0,0,0,0,5
; .byte 43,21,0,10,0,0,0,5
 ifconst screenheight
pfsub
 .byte 8,4,2,2,1,0,0,1,0
 endif
	;--set initial P1 positions
multisprite_setup
 lda #15
 sta pfheight

	ldx #4
; stx temp3
SetCopyHeight
;	lda #76
;	sta NewSpriteX,X
;	lda CopyColorData,X
;	sta NewCOLUP1,X
 ;lda SpriteHeightTable,X
; sta spriteheight,x
	txa
	sta SpriteGfxIndex,X
	sta spritesort,X
	dex
	bpl SetCopyHeight



; since we can't turn off pf, point PF to zeros here
 lda #>blank_pf
 sta PF2pointer+1
 sta PF1pointer+1
 lda #<blank_pf
 sta PF2pointer
 sta PF1pointer
 rts

drawscreen
 ifconst debugscore
 jsr debugcycles
 endif

WaitForOverscanEnd
	lda INTIM
	bmi WaitForOverscanEnd

	lda #2
	sta WSYNC
	sta VSYNC
	sta WSYNC
	sta WSYNC
	lsr
	sta VDELBL
	sta VDELP0
	sta WSYNC
	sta VSYNC	;turn off VSYNC
      ifconst overscan_time
        lda #overscan_time+5+128
      else
	lda #42+128
      endif
	sta TIM64T

; run possible vblank bB code
 ifconst vblank_bB_code
   jsr vblank_bB_code
 endif

 	jsr setscorepointers
	jsr SetupP1Subroutine

	;-------------





	;--position P0, M0, M1, BL

	jsr PrePositionAllObjects

	;--set up player 0 pointer

 dec player0y
	lda player0pointer ; player0: must be run every frame!
	sec
	sbc player0y
	clc
	adc player0height
	sta player0pointer

	lda player0y
	sta P0Top
	sec
	sbc player0height
	clc
	adc #$80
	sta P0Bottom
	

	;--some final setup

 ldx #4
 lda #$80
cycle74_HMCLR
 sta HMP0,X
 dex
 bpl cycle74_HMCLR
;	sta HMCLR


	lda #0
	sta PF1
	sta PF2
	sta GRP0
	sta GRP1


	jsr KernelSetupSubroutine

WaitForVblankEnd
	lda INTIM
	bmi WaitForVblankEnd
        lda #0
	sta WSYNC
	sta VBLANK	;turn off VBLANK - it was turned on by overscan
	sta CXCLR


	jmp KernelRoutine


PositionASpriteSubroutine	;call this function with A == horizontal position (0-159)
				;and X == the object to be positioned (0=P0, 1=P1, 2=M0, etc.)
				;if you do not wish to write to P1 during this function, make
				;sure Y==0 before you call it.  This function will change Y, and A
				;will be the value put into HMxx when returned.
				;Call this function with at least 11 cycles left in the scanline 
				;(jsr + sec + sta WSYNC = 11); it will return 9 cycles
				;into the second scanline
	sec
	sta WSYNC			;begin line 1
	sta.w HMCLR			;+4	 4
DivideBy15Loop
	sbc #15
	bcs DivideBy15Loop			;+4/5	8/13.../58

	tay				;+2	10/15/...60
	lda FineAdjustTableEnd,Y	;+5	15/20/...65

			;	15
	sta HMP0,X	;+4	19/24/...69
	sta RESP0,X	;+4	23/28/33/38/43/48/53/58/63/68/73
	sta WSYNC	;+3	 0	begin line 2
	sta HMOVE	;+3
	rts		;+6	 9

;-------------------------------------------------------------------------

PrePositionAllObjects

	ldx #4
	lda ballx
	jsr PositionASpriteSubroutine
	
	dex
	lda missile1x
	jsr PositionASpriteSubroutine
	
	dex
	lda missile0x
	jsr PositionASpriteSubroutine

	dex
	dex
	lda player0x
	jsr PositionASpriteSubroutine

	rts


;-------------------------------------------------------------------------








;-------------------------------------------------------------------------


KernelSetupSubroutine

	ldx #4
AdjustYValuesUpLoop
	lda NewSpriteY,X
	clc
	adc #2
	sta NewSpriteY,X
	dex
	bpl AdjustYValuesUpLoop


	ldx temp3 ; first sprite displayed

	lda SpriteGfxIndex,x
	tay
	lda NewSpriteY,y
	sta RepoLine

	lda SpriteGfxIndex-1,x
	tay
	lda NewSpriteY,y
	sta temp6

	stx SpriteIndex



	lda #255
	sta P1Bottom

	lda player0y
 ifconst screenheight
	cmp #screenheight+1
 else
	cmp #$59
 endif
	bcc nottoohigh
	lda P0Bottom
	sta P0Top		

       

nottoohigh
	rts

;-------------------------------------------------------------------------





;*************************************************************************

;-------------------------------------------------------------------------
;-------------------------Data Below--------------------------------------
;-------------------------------------------------------------------------

MaskTable
	.byte 1,3,7,15,31

 ; shove 6-digit score routine here

sixdigscore
	lda #0
;	sta COLUBK
	sta PF0
	sta PF1
	sta PF2
	sta ENABL
	sta ENAM0
	sta ENAM1
	;end of kernel here


 ; 6 digit score routine
; lda #0
; sta PF1
; sta PF2
; tax

   sta WSYNC;,x

;                STA WSYNC ;first one, need one more
 sta REFP0
 sta REFP1
                STA GRP0
                STA GRP1
 sta HMCLR

 ; restore P0pointer

	lda player0pointer
	clc
	adc player0y
	sec
	sbc player0height
	sta player0pointer
 inc player0y

 ifconst vblank_time
 ifconst screenheight
 if screenheight == 84
	lda  #vblank_time+9+128+10
 else
	lda  #vblank_time+9+128+19
 endif
 else
	lda  #vblank_time+9+128
 endif
 else
 ifconst screenheight
 if screenheight == 84
	lda  #52+128+10
 else
	lda  #52+128+19
 endif
 else
	lda  #52+128
 endif
 endif

	sta  TIM64T
 ifconst minikernel
 jsr minikernel
 endif
 ifconst noscore
 pla
 pla
 jmp skipscore
 endif

; score pointers contain:
; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
; swap lo2->temp1
; swap lo4->temp3
; swap lo6->temp5

 lda scorepointers+5
 sta temp5
 lda scorepointers+1
 sta temp1
 lda scorepointers+3
 sta temp3

 lda #>scoretable
 sta scorepointers+1
 sta scorepointers+3
 sta scorepointers+5
 sta temp2
 sta temp4
 sta temp6

 rts



;-------------------------------------------------------------------------
;----------------------Kernel Routine-------------------------------------
;-------------------------------------------------------------------------


;-------------------------------------------------------------------------
; repeat $f147-*
; brk
; repend
;	org $F240

SwitchDrawP0K1				;	72
	lda P0Bottom
	sta P0Top			;+6	 2
	jmp BackFromSwitchDrawP0K1	;+3	 5

WaitDrawP0K1				;	74
	SLEEP 4				;+4	 2
	jmp BackFromSwitchDrawP0K1	;+3	 5

SkipDrawP1K1				;	11
	lda #0
	sta GRP1			;+5	16	so Ball gets drawn
	jmp BackFromSkipDrawP1		;+3	19

;-------------------------------------------------------------------------

KernelRoutine
 ifnconst screenheight
 sleep 12
 ; jsr wastetime ; waste 12 cycles
 else
 sleep 6
 endif
	tsx
	stx stack1
	ldx #ENABL
	txs			;+9	 9

 ldx #0
 lda pfheight
 bpl asdhj
 .byte $24
asdhj
 tax

; ldx pfheight
 lda PFStart,x ; get pf pixel resolution for heights 15,7,3,1,0

 ifconst screenheight
  sec
 if screenheight == 84
  sbc pfsub+1,x
 else
  sbc pfsub,x
 endif
 endif
 
 sta pfpixelheight

 ifconst screenheight
        ldy #screenheight
 else
	ldy #88
 endif
 
;	lda #$02
;	sta COLUBK		;+5	18

; sleep 25
 sleep 2
KernelLoopa			;	50
	SLEEP 7			;+4	54
KernelLoopb			;	54
	SLEEP 2		;+12	66
	cpy P0Top		;+3	69
	beq SwitchDrawP0K1	;+2	71
	bpl WaitDrawP0K1	;+2	73
	lda (player0pointer),Y	;+5	 2
	sta GRP0		;+3	 5	VDEL because of repokernel
BackFromSwitchDrawP0K1

	cpy P1Bottom		;+3	 8	unless we mean to draw immediately, this should be set
				;		to a value greater than maximum Y value initially
	bcc SkipDrawP1K1	;+2	10
	lda (P1display),Y	;+5	15
	sta.w GRP1		;+4	19
BackFromSkipDrawP1

;fuck	
 sty temp1
 ldy pfpixelheight
	lax (PF1pointer),y
	stx PF1			;+7	26
	lda (PF2pointer),y
	sta PF2			;+7	33
 ;sleep 6
	stx PF1temp2
	sta PF2temp2
	dey
 bmi pagewraphandler
	lda (PF1pointer),y
cyclebalance
	sta PF1temp1
	lda (PF2pointer),y
	sta PF2temp1
 ldy temp1

 ldx #ENABL
 txs
	cpy bally
	php			;+6	39	VDEL ball


	cpy missile1y
	php			;+6	71

	cpy missile0y
	php			;+6	 1
	

	dey			;+2	15

	cpy RepoLine		;+3	18
	beq RepoKernel		;+2	20
;	SLEEP 20		;+23	43
 sleep 6

newrepo ; since we have time here, store next repoline
 ldx SpriteIndex
 lda SpriteGfxIndex-1,x
 tax
 lda NewSpriteY,x
 sta temp6
 sleep 4 

BackFromRepoKernel
	tya			;+2	45
	and pfheight			;+2	47
	bne KernelLoopa		;+2	49
	dec pfpixelheight
	bpl KernelLoopb		;+3	54
;	bmi donewkernel		;+3	54
;	bne KernelLoopb+1		;+3	54

donewkernel
	jmp DoneWithKernel	;+3	56

pagewraphandler
 jmp cyclebalance

;-------------------------------------------------------------------------
 
 ; room here for score?

setscorepointers
 lax score+2
 jsr scorepointerset
 sty scorepointers+5
 stx scorepointers+2
 lax score+1
 jsr scorepointerset
 sty scorepointers+4
 stx scorepointers+1
 lax score
 jsr scorepointerset
 sty scorepointers+3
 stx scorepointers
wastetime
 rts

scorepointerset
 and #$0F
 asl
 asl
 asl
 adc #<scoretable
 tay
 txa
 and #$F0
 lsr
 adc #<scoretable
 tax
 rts
;	align 256

SwitchDrawP0KR				;	45
	lda P0Bottom
	sta P0Top			;+6	51
	jmp BackFromSwitchDrawP0KR	;+3	54

WaitDrawP0KR				;	47
	SLEEP 4				;+4	51
	jmp BackFromSwitchDrawP0KR	;+3	54

;-----------------------------------------------------------

noUpdateXKR
 ldx #1
 cpy.w P0Top
 JMP retXKR

skipthis
 ldx #1
 jmp goback

RepoKernel			;	22	crosses page boundary
	tya
	and pfheight			;+2	26
	bne noUpdateXKR		;+2	28
        tax
;	dex			;+2	30
	dec pfpixelheight
;	stx Temp		;+3	35
;	SLEEP 3

	cpy P0Top		;+3	42
retXKR
	beq SwitchDrawP0KR	;+2	44
	bpl WaitDrawP0KR	;+2	46
	lda (player0pointer),Y	;+5	51
	sta GRP0		;+3	54	VDEL
BackFromSwitchDrawP0KR
	sec			;+2	56
 


	lda PF2temp1,X
	ldy PF1temp1,X

	ldx SpriteIndex	;+3	 2

	sta PF2			;+7	63

	lda SpriteGfxIndex,x
	sty PF1			;+7	70	too early?
	tax
	lda #0
	sta GRP1		;+5	75	to display player 0
	lda NewSpriteX,X	;+4	 6
 
DivideBy15LoopK				;	 6	(carry set above)
	sbc #15
	bcs DivideBy15LoopK		;+4/5	10/15.../60

	tax				;+2	12/17/...62
	lda FineAdjustTableEnd,X	;+5	17/22/...67

	sta HMP1			;+3	20/25/...70
	sta RESP1			;+3	23/28/33/38/43/48/53/58/63/68/73
	sta WSYNC			;+3	 0	begin line 2
	;sta HMOVE			;+3	 3

	ldx #ENABL
	txs			;+4	25
	ldy RepoLine ; restore y
	cpy bally
	php			;+6	 9	VDEL ball

	cpy missile1y
	php			;+6	15

	cpy missile0y
	php			;+6	21
	

 


;15 cycles
	tya
	and pfheight
 ;eor #1
	and #$FE
	bne skipthis
 tax
 sleep 4
;	sleep 2
goback

	dey
	cpy P0Top			;+3	52
	beq SwitchDrawP0KV	;+2	54
	bpl WaitDrawP0KV		;+2	56
	lda (player0pointer),Y		;+5	61
	sta GRP0			;+3	64	VDEL
BackFromSwitchDrawP0KV

; sleep 3

	lda PF2temp1,X
	sta PF2			;+7	 5
	lda PF1temp1,X
	sta PF1			;+7	74 
 sta HMOVE

	lda #0
	sta GRP1			;+5	10	to display GRP0

	ldx #ENABL
	txs			;+4	 8

	ldx SpriteIndex	;+3	13	restore index into new sprite vars
	;--now, set all new variables and return to main kernel loop


;
	lda SpriteGfxIndex,X	;+4	31
	tax				;+2	33
;



	lda NewNUSIZ,X
	sta NUSIZ1			;+7	20
 sta REFP1
	lda NewCOLUP1,X
	sta COLUP1			;+7	27

;	lda SpriteGfxIndex,X	;+4	31
;	tax				;+2	33
;fuck2
	lda NewSpriteY,X		;+4	46
	sec				;+2	38
	sbc spriteheight,X	;+4	42
	sta P1Bottom		;+3	45

 sleep 6
	lda player1pointerlo,X	;+4	49
	sbc P1Bottom		;+3	52	carry should still be set
	sta P1display		;+3	55
	lda player1pointerhi,X
	sta P1display+1		;+7	62


	cpy bally
	php			;+6	68	VDELed

	cpy missile1y
	php			;+6	74

	cpy missile0y
	php			;+6	 4



; lda SpriteGfxIndex-1,x
; sleep 3
	dec SpriteIndex	;+5	13
; tax
; lda NewSpriteY,x
; sta RepoLine

; 10 cycles below...
	bpl SetNextLine
	lda #255
	jmp SetLastLine
SetNextLine
;	lda NewSpriteY-1,x
	lda.w temp6
SetLastLine
	sta RepoLine	

 tya
 and pfheight
 bne nodec
 dec pfpixelheight
	dey			;+2	30

; 10 cycles 
 

	jmp BackFromRepoKernel	;+3	43

nodec
 sleep 4
 dey
 jmp BackFromRepoKernel

;-------------------------------------------------------------------------


SwitchDrawP0KV				;	69
	lda P0Bottom
	sta P0Top			;+6	75
	jmp BackFromSwitchDrawP0KV	;+3	 2

WaitDrawP0KV				;	71
	SLEEP 4				;+4	75
	jmp BackFromSwitchDrawP0KV	;+3	 2

;-------------------------------------------------------------------------

DoneWithKernel

BottomOfKernelLoop

	sta WSYNC
 ldx stack1
 txs
 jsr sixdigscore ; set up score


 sta WSYNC
 ldx #0
 sta HMCLR
                STx GRP0
                STx GRP1 ; seems to be needed because of vdel

                LDY #7
        STy VDELP0
        STy VDELP1
        LDA #$10
        STA HMP1
               LDA scorecolor 
                STA COLUP0
                STA COLUP1
 
        LDA #$03
        STA NUSIZ0
        STA NUSIZ1

                STA RESP0
                STA RESP1

 sleep 9
 lda  (scorepointers),y
 sta  GRP0
 ifconst pfscore
 lda pfscorecolor
 sta COLUPF
 else
 sleep 6
 endif

                STA HMOVE
 lda  (scorepointers+8),y
; sta WSYNC
 ;sleep 2
 jmp beginscore


loop2
 lda  (scorepointers),y     ;+5  68  204
 sta  GRP0            ;+3  71  213      D1     --      --     --
 ifconst pfscore
 lda.w pfscore1
 sta PF1
 else
 sleep 7
 endif
 ; cycle 0
 lda  (scorepointers+$8),y  ;+5   5   15
beginscore
 sta  GRP1            ;+3   8   24      D1     D1      D2     --
 lda  (scorepointers+$6),y  ;+5  13   39
 sta  GRP0            ;+3  16   48      D3     D1      D2     D2
 lax  (scorepointers+$2),y  ;+5  29   87
 txs
 lax  (scorepointers+$4),y  ;+5  36  108
 sleep 3
 ifconst pfscore
 lda pfscore2
 sta PF1
 else
 sleep 6
 endif
 lda  (scorepointers+$A),y  ;+5  21   63
 stx  GRP1            ;+3  44  132      D3     D3      D4     D2!
 tsx
 stx  GRP0            ;+3  47  141      D5     D3!     D4     D4
 sta  GRP1            ;+3  50  150      D5     D5      D6     D4!
 sty  GRP0            ;+3  53  159      D4*    D5!     D6     D6
 dey
 bpl  loop2           ;+2  60  180
 	ldx stack1
	txs


; lda scorepointers+1
 ldy temp1
; sta temp1
 sty scorepointers+1

                LDA #0   
               STA GRP0
                STA GRP1
 sta PF1 
       STA VDELP0
        STA VDELP1;do we need these
        STA NUSIZ0
        STA NUSIZ1

; lda scorepointers+3
 ldy temp3
; sta temp3
 sty scorepointers+3

; lda scorepointers+5
 ldy temp5
; sta temp5
 sty scorepointers+5


;-------------------------------------------------------------------------
;------------------------Overscan Routine---------------------------------
;-------------------------------------------------------------------------

OverscanRoutine



skipscore
    ifconst qtcontroller
        lda qtcontroller
        lsr    ; bit 0 in carry
        lda #4
        ror    ; carry into top of A
    else
        lda #2
    endif ; qtcontroller
	sta WSYNC
	sta VBLANK	;turn on VBLANK


	


;-------------------------------------------------------------------------
;----------------------------End Main Routines----------------------------
;-------------------------------------------------------------------------


;*************************************************************************

;-------------------------------------------------------------------------
;----------------------Begin Subroutines----------------------------------
;-------------------------------------------------------------------------




KernelCleanupSubroutine

	ldx #4
AdjustYValuesDownLoop
	lda NewSpriteY,X
	sec
	sbc #2
	sta NewSpriteY,X
	dex
	bpl AdjustYValuesDownLoop


 RETURN
	;rts

SetupP1Subroutine
; flickersort algorithm
; count 4-0
; table2=table1 (?)
; detect overlap of sprites in table 2
; if overlap, do regular sort in table2, then place one sprite at top of table 1, decrement # displayed
; if no overlap, do regular sort in table 2 and table 1
fsstart
 ldx #255
copytable
 inx
 lda spritesort,x
 sta SpriteGfxIndex,x
 cpx #4
 bne copytable

 stx temp3 ; highest displayed sprite
 dex
 stx temp2
sortloop
 ldx temp2
 lda spritesort,x
 tax
 lda NewSpriteY,x
 sta temp1

 ldx temp2
 lda spritesort+1,x
 tax
 lda NewSpriteY,x
 sec
 clc
 sbc temp1
 bcc largerXislower

; larger x is higher (A>=temp1)
 cmp spriteheight,x
 bcs countdown
; overlap with x+1>x
; 
; stick x at end of gfxtable, dec counter
overlapping
 dec temp3
 ldx temp2
; inx
 jsr shiftnumbers
 jmp skipswapGfxtable

largerXislower ; (temp1>A)
 tay
 ldx temp2
 lda spritesort,x
 tax
 tya
 eor #$FF
 sbc #1
 bcc overlapping
 cmp spriteheight,x
 bcs notoverlapping

 dec temp3
 ldx temp2
; inx
 jsr shiftnumbers
 jmp skipswapGfxtable 
notoverlapping
; ldx temp2 ; swap display table
; ldy SpriteGfxIndex+1,x
; lda SpriteGfxIndex,x
; sty SpriteGfxIndex,x
; sta SpriteGfxIndex+1,x 

skipswapGfxtable
 ldx temp2 ; swap sort table
 ldy spritesort+1,x
 lda spritesort,x
 sty spritesort,x
 sta spritesort+1,x 

countdown
 dec temp2
 bpl sortloop

checktoohigh
 ldx temp3
 lda SpriteGfxIndex,x
 tax
 lda NewSpriteY,x
 ifconst screenheight
 cmp #screenheight-3
 else
 cmp #$55
 endif
 bcc nonetoohigh
 dec temp3
 bne checktoohigh

nonetoohigh
 rts


shiftnumbers
 ; stick current x at end, shift others down
 ; if x=4: don't do anything
 ; if x=3: swap 3 and 4
 ; if x=2: 2=3, 3=4, 4=2
 ; if x=1: 1=2, 2=3, 3=4, 4=1
 ; if x=0: 0=1, 1=2, 2=3, 3=4, 4=0
; ldy SpriteGfxIndex,x
swaploop
 cpx #4
 beq shiftdone 
 lda SpriteGfxIndex+1,x
 sta SpriteGfxIndex,x
 inx
 jmp swaploop
shiftdone
; sty SpriteGfxIndex,x
 rts

 ifconst debugscore
debugcycles
   ldx #14
   lda INTIM ; display # cycles left in the score

 ifconst mincycles
 lda mincycles 
 cmp INTIM
 lda mincycles
 bcc nochange
 lda INTIM
 sta mincycles
nochange
 endif

;   cmp #$2B
;   bcs no_cycles_left
   bmi cycles_left
   ldx #64
   eor #$ff ;make negative
cycles_left
   stx scorecolor
   and #$7f ; clear sign bit
   tax
   lda scorebcd,x
   sta score+2
   lda scorebcd1,x
   sta score+1
   rts
scorebcd
 .byte $00, $64, $28, $92, $56, $20, $84, $48, $12, $76, $40
 .byte $04, $68, $32, $96, $60, $24, $88, $52, $16, $80, $44
 .byte $08, $72, $36, $00, $64, $28, $92, $56, $20, $84, $48
 .byte $12, $76, $40, $04, $68, $32, $96, $60, $24, $88
scorebcd1
 .byte 0, 0, 1, 1, 2, 3, 3, 4, 5, 5, 6
 .byte 7, 7, 8, 8, 9, $10, $10, $11, $12, $12, $13
 .byte $14, $14, $15, $16, $16, $17, $17, $18, $19, $19, $20
 .byte $21, $21, $22, $23, $23, $24, $24, $25, $26, $26
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ifnconst multisprite
 ifconst pfrowheight
 lda #pfrowheight
 else
 ifconst pfres
 lda #(96/pfres)
 else
 lda #8
 endif
 endif
 sta playfieldpos
 endif
 ldx #5
initscore
 lda #<scoretable
 sta scorepointers,x 
 dex
 bpl initscore
 lda #1
 sta CTRLPF
 ora INTIM
 sta rand

 ifconst multisprite
   jsr multisprite_setup
 endif

 ifnconst bankswitch
   jmp game
 else
   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

;standard routines needed for pretty much all games
; just the random number generator is left - maybe we should remove this asm file altogether?
; repositioning code and score pointer setup moved to overscan
; read switches, joysticks now compiler generated (more efficient)

randomize
	lda rand
	lsr
 ifconst rand16
	rol rand16
 endif
	bcc noeor
	eor #$B4
noeor
	sta rand
 ifconst rand16
	eor rand16
 endif
	RETURN
game
.L00 ;  set kernel multisprite

.
 ; 

._ReiniciarNivel
 ; _ReiniciarNivel

.
 ; 

.L01 ;  if n = 0 then playfield:

	LDA n
	CMP #0
     BNE .skipL01
.condpart0
	LDA #<PF1_data0
	STA PF1pointer
	LDA #>PF1_data0
	STA PF1pointer+1
	LDA #<PF2_data0
	STA PF2pointer
	LDA #>PF2_data0
	STA PF2pointer+1
.skipL01
.
 ; 

.L02 ;  if n = 1 then playfield:

	LDA n
	CMP #1
     BNE .skipL02
.condpart1
	LDA #<PF1_data1
	STA PF1pointer
	LDA #>PF1_data1
	STA PF1pointer+1
	LDA #<PF2_data1
	STA PF2pointer
	LDA #>PF2_data1
	STA PF2pointer+1
.skipL02
.
 ; 

.
 ; 

.L03 ;  COLUPF  =  34

	LDA #34
	STA COLUPF
.
 ; 

.L04 ;  x = 60

	LDA #60
	STA x
.L05 ;  y = 75

	LDA #75
	STA y
.
 ; 

.L06 ;  player3x = 160

	LDA #160
	STA player3x
.L07 ;  player3y = 50

	LDA #50
	STA player3y
.
 ; 

.L08 ;  i = 1

	LDA #1
	STA i
.L09 ;  j = 1

	LDA #1
	STA j
.
 ; 

.L010 ;  dim sounda  =  s

.L011 ;  dim soundb  =  r

.
 ; 

.L012 ;  sounda  =  0

	LDA #0
	STA sounda
.L013 ;  soundb  =  0

	LDA #0
	STA soundb
.
 ; 

.L014 ;  pfheight = 7

	LDA #7
	STA pfheight
.
 ; 

.L015 ;  scorecolor  =  15

	LDA #15
	STA scorecolor
.
 ; 

.main
 ; main

.
 ; 

.L016 ;  COLUP0 = 155

	LDA #155
	STA COLUP0
.L017 ;  COLUP2 = 190

	LDA #190
	STA COLUP2
.L018 ;  COLUP3 = 50

	LDA #50
	STA COLUP3
.
 ; 

.L019 ;  COLUBK = 0

	LDA #0
	STA COLUBK
.
 ; 

.L020 ;  if f = 0  ||  f = 11 then player0:

	LDA f
	CMP #0
     BNE .skipL020
.condpart2
 jmp .condpart3
.skipL020
	LDA f
	CMP #11
     BNE .skip0OR
.condpart3
	LDX #<player3then_0
	STX player0pointerlo
	LDA #>player3then_0
	STA player0pointerhi
	LDA #9
	STA player0height
.skip0OR
.
 ; 

.L021 ;  if f = 1 then player0:

	LDA f
	CMP #1
     BNE .skipL021
.condpart4
	LDX #<player4then_0
	STX player0pointerlo
	LDA #>player4then_0
	STA player0pointerhi
	LDA #9
	STA player0height
.skipL021
.
 ; 

.L022 ;  player3:

	LDX #<playerL022_3
	STX player3pointerlo
	LDA #>playerL022_3
	STA player3pointerhi
	LDA #9
	STA player3height
.
 ; 

.L023 ;  player2:

	LDX #<playerL023_2
	STX player2pointerlo
	LDA #>playerL023_2
	STA player2pointerhi
	LDA #9
	STA player2height
.
 ; 

.
 ; 

.L024 ;  player0x = x

	LDA x
	STA player0x
.
 ; 

.L025 ;  player0y = y

	LDA y
	STA player0y
.
 ; 

.L026 ;  drawscreen

 jsr drawscreen
.
 ; 

.L027 ;  if t = 0 then g = 0

	LDA t
	CMP #0
     BNE .skipL027
.condpart5
	LDA #0
	STA g
.skipL027
.
 ; 

.L028 ;  if joy0right  ||  joy0left then f = f + 1 else f = 0

 bit SWCHA
	BMI .skipL028
.condpart6
 jmp .condpart7
.skipL028
 bit SWCHA
	BVS .skip1OR
.condpart7
	INC f
 jmp .skipelse0
.skip1OR
	LDA #0
	STA f
.skipelse0
.
 ; 

.L029 ;  if joy0up  &&  t = 0  &&  u = 0 then t = 30

 lda #$10
 bit SWCHA
	BNE .skipL029
.condpart8
	LDA t
	CMP #0
     BNE .skip8then
.condpart9
	LDA u
	CMP #0
     BNE .skip9then
.condpart10
	LDA #30
	STA t
.skip9then
.skip8then
.skipL029
.L030 ;  if t > 0 then t = t - 1  :  g = 1

	LDA #0
	CMP t
     BCS .skipL030
.condpart11
	DEC t
	LDA #1
	STA g
.skipL030
.L031 ;  if !joy0right then goto _IgnorarDerecha

 bit SWCHA
	BPL .skipL031
.condpart12
 jmp ._IgnorarDerecha

.skipL031
.L032 ;  temp5  =   ( y + 1 - 11 )  / 8

; complex statement detected
	LDA y
	CLC
	ADC #1
	SEC
	SBC #11
	lsr
	lsr
	lsr
	STA temp5
.
 ; 

.L033 ;  temp6  =   ( x - 9 )  / 4

; complex statement detected
	LDA x
	SEC
	SBC #9
	lsr
	lsr
	STA temp6
.
 ; 

.L034 ;  if temp6  <  34 then if !pfread ( temp6 , temp5 )  then goto _IgnorarDerecha

	LDA temp6
	CMP #34
     BCS .skipL034
.condpart13
	LDA temp6
	LDY temp5
 jsr pfread
	BEQ .skip13then
.condpart14
 jmp ._IgnorarDerecha

.skip13then
.skipL034
.
 ; 

.L035 ;  temp3  =   ( y + 8 - 11 )  / 8

; complex statement detected
	LDA y
	CLC
	ADC #8
	SEC
	SBC #11
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L036 ;  if temp6  <  34 then if !pfread ( temp6 , temp3 )  then goto _IgnorarDerecha

	LDA temp6
	CMP #34
     BCS .skipL036
.condpart15
	LDA temp6
	LDY temp3
 jsr pfread
	BEQ .skip15then
.condpart16
 jmp ._IgnorarDerecha

.skip15then
.skipL036
.
 ; 

.L037 ;  d = 1

	LDA #1
	STA d
.L038 ;  x = x + 1

	INC x
.L039 ;  REFP0 = 0

	LDA #0
	STA REFP0
.
 ; 

._IgnorarDerecha
 ; _IgnorarDerecha

.
 ; 

.L040 ;  if !joy0left then goto _IgnorarIzquierda

 bit SWCHA
	BVC .skipL040
.condpart17
 jmp ._IgnorarIzquierda

.skipL040
.L041 ;  temp5  =   ( y + 1 - 11 )  / 8

; complex statement detected
	LDA y
	CLC
	ADC #1
	SEC
	SBC #11
	lsr
	lsr
	lsr
	STA temp5
.
 ; 

.L042 ;  temp6  =   ( x - 18 )  / 4

; complex statement detected
	LDA x
	SEC
	SBC #18
	lsr
	lsr
	STA temp6
.
 ; 

.L043 ;  if temp6  <  34 then if !pfread ( temp6 , temp5 )  then goto _IgnorarIzquierda

	LDA temp6
	CMP #34
     BCS .skipL043
.condpart18
	LDA temp6
	LDY temp5
 jsr pfread
	BEQ .skip18then
.condpart19
 jmp ._IgnorarIzquierda

.skip18then
.skipL043
.
 ; 

.L044 ;  temp3  =   ( y + 8 - 11 )  / 8

; complex statement detected
	LDA y
	CLC
	ADC #8
	SEC
	SBC #11
	lsr
	lsr
	lsr
	STA temp3
.
 ; 

.L045 ;  if temp6  <  34 then if !pfread ( temp6 , temp3 )  then goto _IgnorarIzquierda

	LDA temp6
	CMP #34
     BCS .skipL045
.condpart20
	LDA temp6
	LDY temp3
 jsr pfread
	BEQ .skip20then
.condpart21
 jmp ._IgnorarIzquierda

.skip20then
.skipL045
.
 ; 

.L046 ;  d = 0

	LDA #0
	STA d
.L047 ;  x = x - 1

	DEC x
.L048 ;  REFP0 = 8

	LDA #8
	STA REFP0
.
 ; 

._IgnorarIzquierda
 ; _IgnorarIzquierda

.
 ; 

.L049 ;  if !g = 0 then goto _IgnorarArriba

	LDA g
	CMP #0
     BNE .skipL049
.condpart22
 jmp ._IgnorarArriba

.skipL049
.
 ; 

.L050 ;  temp5  =   ( x - 10 )  / 4

; complex statement detected
	LDA x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L051 ;  temp6  =   ( y + 9 - 11 )  / 8

; complex statement detected
	LDA y
	CLC
	ADC #9
	SEC
	SBC #11
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L052 ;  if temp5  <  34 then if !pfread ( temp5 , temp6 )  then t = 0  :  goto _IgnorarArriba

	LDA temp5
	CMP #34
     BCS .skipL052
.condpart23
	LDA temp5
	LDY temp6
 jsr pfread
	BEQ .skip23then
.condpart24
	LDA #0
	STA t
 jmp ._IgnorarArriba

.skip23then
.skipL052
.
 ; 

.L053 ;  temp4  =   ( x - 17 )  / 4

; complex statement detected
	LDA x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L054 ;  if temp4  <  34 then if !pfread ( temp4 , temp6 )  then t = 0  :  goto _IgnorarArriba

	LDA temp4
	CMP #34
     BCS .skipL054
.condpart25
	LDA temp4
	LDY temp6
 jsr pfread
	BEQ .skip25then
.condpart26
	LDA #0
	STA t
 jmp ._IgnorarArriba

.skip25then
.skipL054
.
 ; 

.L055 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L056 ;  if temp3  <  34 then if !pfread ( temp3 , temp6 )  then t = 0  :  goto _IgnorarArriba

	LDA temp3
	CMP #34
     BCS .skipL056
.condpart27
	LDA temp3
	LDY temp6
 jsr pfread
	BEQ .skip27then
.condpart28
	LDA #0
	STA t
 jmp ._IgnorarArriba

.skip27then
.skipL056
.
 ; 

.L057 ;  y = y + 1

	INC y
.
 ; 

._IgnorarArriba
 ; _IgnorarArriba

.
 ; 

.
 ; 

.L058 ;  if !g = 1 then u = 1  :  goto _IgnorarAbajo

	LDA g
	CMP #1
     BNE .skipL058
.condpart29
	LDA #1
	STA u
 jmp ._IgnorarAbajo

.skipL058
.
 ; 

.L059 ;  temp5  =   ( x - 10 )  / 4

; complex statement detected
	LDA x
	SEC
	SBC #10
	lsr
	lsr
	STA temp5
.
 ; 

.L060 ;  temp6  =   ( y - 11 )  / 8

; complex statement detected
	LDA y
	SEC
	SBC #11
	lsr
	lsr
	lsr
	STA temp6
.
 ; 

.L061 ;  if temp5  <  34 then if !pfread ( temp5 , temp6 )  then goto _RevisarArriba

	LDA temp5
	CMP #34
     BCS .skipL061
.condpart30
	LDA temp5
	LDY temp6
 jsr pfread
	BEQ .skip30then
.condpart31
 jmp ._RevisarArriba

.skip30then
.skipL061
.
 ; 

.L062 ;  temp4  =   ( x - 17 )  / 4

; complex statement detected
	LDA x
	SEC
	SBC #17
	lsr
	lsr
	STA temp4
.
 ; 

.L063 ;  if temp4  <  34 then if !pfread ( temp4 , temp6 )  then goto _RevisarArriba

	LDA temp4
	CMP #34
     BCS .skipL063
.condpart32
	LDA temp4
	LDY temp6
 jsr pfread
	BEQ .skip32then
.condpart33
 jmp ._RevisarArriba

.skip32then
.skipL063
.
 ; 

.L064 ;  temp3  =  temp5  -  1

	LDA temp5
	SEC
	SBC #1
	STA temp3
.
 ; 

.L065 ;  if temp3  <  34 then if !pfread ( temp3 , temp6 )  then goto _RevisarArriba

	LDA temp3
	CMP #34
     BCS .skipL065
.condpart34
	LDA temp3
	LDY temp6
 jsr pfread
	BEQ .skip34then
.condpart35
 jmp ._RevisarArriba

.skip34then
.skipL065
.
 ; 

.L066 ;  y = y - 1

	DEC y
.
 ; 

.L067 ;  goto _IgnorarAbajo

 jmp ._IgnorarAbajo

.
 ; 

.
 ; 

._RevisarArriba
 ; _RevisarArriba

.
 ; 

.L068 ;  u = 0

	LDA #0
	STA u
.L069 ;  if joy0up then u = 1

 lda #$10
 bit SWCHA
	BNE .skipL069
.condpart36
	LDA #1
	STA u
.skipL069
.
 ; 

._IgnorarAbajo
 ; _IgnorarAbajo

.
 ; 

.L070 ;  if joy0fire then p = p + 1 else p = 0

 bit INPT4
	BMI .skipL070
.condpart37
	INC p
 jmp .skipelse1
.skipL070
	LDA #0
	STA p
.skipelse1
.
 ; 

.L071 ;  if joy0fire  &&  d = 0  &&  p = 1 then c = 1  :  missile0x = x  :  missile0y = y - 5  :  sounda  =  5

 bit INPT4
	BMI .skipL071
.condpart38
	LDA d
	CMP #0
     BNE .skip38then
.condpart39
	LDA p
	CMP #1
     BNE .skip39then
.condpart40
	LDA #1
	STA c
	LDA x
	STA missile0x
	LDA y
	SEC
	SBC #5
	STA missile0y
	LDA #5
	STA sounda
.skip39then
.skip38then
.skipL071
.L072 ;  if joy0fire  &&  d = 1  &&  p = 1 then c = 2  :  missile0x = x + 9  :  missile0y = y - 5  :  sounda  =  5

 bit INPT4
	BMI .skipL072
.condpart41
	LDA d
	CMP #1
     BNE .skip41then
.condpart42
	LDA p
	CMP #1
     BNE .skip42then
.condpart43
	LDA #2
	STA c
	LDA x
	CLC
	ADC #9
	STA missile0x
	LDA y
	SEC
	SBC #5
	STA missile0y
	LDA #5
	STA sounda
.skip42then
.skip41then
.skipL072
.
 ; 

.L073 ;  if sounda  >  0 then sounda  =  sounda  -  1  :  AUDC0  =  2  :  AUDV0  =  4  :  AUDF0  =  sounda else AUDV0  =  0

	LDA #0
	CMP sounda
     BCS .skipL073
.condpart44
	DEC sounda
	LDA #2
	STA AUDC0
	LDA #4
	STA AUDV0
	LDA sounda
	STA AUDF0
 jmp .skipelse2
.skipL073
	LDA #0
	STA AUDV0
.skipelse2
.
 ; 

.L074 ;  if c = 1 then missile0x  =  missile0x - 1

	LDA c
	CMP #1
     BNE .skipL074
.condpart45
	DEC missile0x
.skipL074
.L075 ;  if c = 2 then missile0x  =  missile0x + 1

	LDA c
	CMP #2
     BNE .skipL075
.condpart46
	INC missile0x
.skipL075
.
 ; 

.L076 ;  if missile0x < 0 then c = 0

	LDA missile0x
	CMP #0
     BCS .skipL076
.condpart47
	LDA #0
	STA c
.skipL076
.L077 ;  if missile0x > 160 then c = 0

	LDA #160
	CMP missile0x
     BCS .skipL077
.condpart48
	LDA #0
	STA c
.skipL077
.
 ; 

.L078 ;  if c = 0 then missile0x = 0  :  missile0y = 0

	LDA c
	CMP #0
     BNE .skipL078
.condpart49
	LDA #0
	STA missile0x
	STA missile0y
.skipL078
.
 ; 

.L079 ;  if i = 1 then player3x = player3x - 1

	LDA i
	CMP #1
     BNE .skipL079
.condpart50
	DEC player3x
.skipL079
.L080 ;  if i = 1  &&  player3x = 0 then player3x = 160  :  player3y  =   ( rand & 30 )  + 20

	LDA i
	CMP #1
     BNE .skipL080
.condpart51
	LDA player3x
	CMP #0
     BNE .skip51then
.condpart52
	LDA #160
	STA player3x
; complex statement detected
 jsr randomize
	AND #30
	CLC
	ADC #20
	STA player3y
.skip51then
.skipL080
.
 ; 

.
 ; 

.L081 ;  if missile0x > player3x - 7  &&  missile0x < player3x + 1  &&  missile0y > player3y - 7  &&  missile0y < player3y + 1 then i = 0  :  score = score + 10  :  soundb  =  50

; complex condition detected
	LDA player3x
	SEC
	SBC #7
; todo: this LDA is spurious and should be prevented ->	LDA  1,x
	CMP missile0x
     BCS .skipL081
.condpart53
; complex condition detected
	LDA player3x
	CLC
	ADC #1
  PHA
  TSX
  PLA
	LDA missile0x
	CMP  1,x
     BCS .skip53then
.condpart54
; complex condition detected
	LDA player3y
	SEC
	SBC #7
; todo: this LDA is spurious and should be prevented ->	LDA  1,x
	CMP missile0y
     BCS .skip54then
.condpart55
; complex condition detected
	LDA player3y
	CLC
	ADC #1
  PHA
  TSX
  PLA
	LDA missile0y
	CMP  1,x
     BCS .skip55then
.condpart56
	LDA #0
	STA i
	SED
	CLC
	LDA score+2
	ADC #$10
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
	LDA #50
	STA soundb
.skip55then
.skip54then
.skip53then
.skipL081
.L082 ;  if i = 0 then player3x = 0  :  player3y = 0

	LDA i
	CMP #0
     BNE .skipL082
.condpart57
	LDA #0
	STA player3x
	STA player3y
.skipL082
.
 ; 

.L083 ;  if j = 1 then player2x = player2x + 1

	LDA j
	CMP #1
     BNE .skipL083
.condpart58
	INC player2x
.skipL083
.L084 ;  if j = 1  &&  player2x > 160 then player2x = 0  :  player2y  =   ( rand & 30 )  + 20

	LDA j
	CMP #1
     BNE .skipL084
.condpart59
	LDA #160
	CMP player2x
     BCS .skip59then
.condpart60
	LDA #0
	STA player2x
; complex statement detected
 jsr randomize
	AND #30
	CLC
	ADC #20
	STA player2y
.skip59then
.skipL084
.
 ; 

.L085 ;  if missile0x > player2x - 7  &&  missile0x < player2x + 1  &&  missile0y > player2y - 7  &&  missile0y < player2y + 1 then j = 0  :  score = score + 10  :  soundb  =  50

; complex condition detected
	LDA player2x
	SEC
	SBC #7
; todo: this LDA is spurious and should be prevented ->	LDA  1,x
	CMP missile0x
     BCS .skipL085
.condpart61
; complex condition detected
	LDA player2x
	CLC
	ADC #1
  PHA
  TSX
  PLA
	LDA missile0x
	CMP  1,x
     BCS .skip61then
.condpart62
; complex condition detected
	LDA player2y
	SEC
	SBC #7
; todo: this LDA is spurious and should be prevented ->	LDA  1,x
	CMP missile0y
     BCS .skip62then
.condpart63
; complex condition detected
	LDA player2y
	CLC
	ADC #1
  PHA
  TSX
  PLA
	LDA missile0y
	CMP  1,x
     BCS .skip63then
.condpart64
	LDA #0
	STA j
	SED
	CLC
	LDA score+2
	ADC #$10
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
	LDA #50
	STA soundb
.skip63then
.skip62then
.skip61then
.skipL085
.L086 ;  if j = 0 then player2x = 0  :  player2y = 0

	LDA j
	CMP #0
     BNE .skipL086
.condpart65
	LDA #0
	STA player2x
	STA player2y
.skipL086
.
 ; 

.L087 ;  if soundb  >  0 then soundb  =  soundb  -  1  :  AUDC1  =  8  :  AUDV1  =  4  :  AUDF1  =  soundb else AUDV1  =  0

	LDA #0
	CMP soundb
     BCS .skipL087
.condpart66
	DEC soundb
	LDA #8
	STA AUDC1
	LDA #4
	STA AUDV1
	LDA soundb
	STA AUDF1
 jmp .skipelse3
.skipL087
	LDA #0
	STA AUDV1
.skipelse3
.
 ; 

.
 ; 

.L088 ;  if x = 0 then x = 1

	LDA x
	CMP #0
     BNE .skipL088
.condpart67
	LDA #1
	STA x
.skipL088
.L089 ;  if x > 160 - 7 then x = 160 - 7

; complex condition detected
	LDA #160
	SEC
	SBC #7
	CMP x
     BCS .skipL089
.condpart68
	LDA #160
	SEC
	SBC #7
	STA x
.skipL089
.
 ; 

.L090 ;  if y < 9 then goto _ReiniciarNivel

	LDA y
	CMP #9
     BCS .skipL090
.condpart69
 jmp ._ReiniciarNivel

.skipL090
.
 ; 

.L091 ;  if f > 21 then f = 1

	LDA #21
	CMP f
     BCS .skipL091
.condpart70
	LDA #1
	STA f
.skipL091
.
 ; 

.L092 ;  if i = 0  &&  j = 0 then n = 1  :  goto _ReiniciarNivel

	LDA i
	CMP #0
     BNE .skipL092
.condpart71
	LDA j
	CMP #0
     BNE .skip71then
.condpart72
	LDA #1
	STA n
 jmp ._ReiniciarNivel

.skip71then
.skipL092
.
 ; 

.L093 ;  if f > 21 then f = 1

	LDA #21
	CMP f
     BCS .skipL093
.condpart73
	LDA #1
	STA f
.skipL093
.
 ; 

.L094 ;  goto main

 jmp .main

.
 ; 

.L095 ;  inline pfread_msk.asm

 include pfread_msk.asm

.
 ; 

.L096 ;  
 if (<*) > (<(*+8))
	repeat ($100-<*)
	.byte 0
	repend
	endif
 if (<*) < 90
	repeat (90-<*)
	.byte 0
	repend
	endif
player3then_0
	.byte 0
	.byte  %11100111
	.byte  %00100100
	.byte  %00011000
	.byte  %01011010
	.byte  %00111100
	.byte  %00011000
	.byte  %00100100
	.byte  %00111100
 if (<*) > (<(*+8))
	repeat ($100-<*)
	.byte 0
	repend
	endif
 if (<*) < 90
	repeat (90-<*)
	.byte 0
	repend
	endif
player4then_0
	.byte 0
	.byte  %11000000
	.byte  %01100110
	.byte  %00100101
	.byte  %01011000
	.byte  %00111110
	.byte  %00011000
	.byte  %00100100
	.byte  %00111100
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
 if (<*) < 90
	repeat (90-<*)
	.byte 0
	repend
	endif
playerL022_3
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
 if (<*) < 90
	repeat (90-<*)
	.byte 0
	repend
	endif
playerL023_2
	.byte  %11111111
	.byte  %10000001
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
	.byte  %11111111
	.byte  %10000001
	.byte  %11111111
 if ((>(*+11)) > (>*))
 ALIGN 256
 endif
PF1_data0
 .byte %00000000
 .byte %00000111
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 if ((>(*+11)) > (>*))
 ALIGN 256
 endif
PF2_data0
 .byte %00000000
 .byte %11111111
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00011111
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 if ((>(*+11)) > (>*))
 ALIGN 256
 endif
PF1_data1
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %11111111
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 if ((>(*+11)) > (>*))
 ALIGN 256
 endif
PF2_data1
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %01111111
 .byte %00000000
 .byte %00000000
 .byte %11111000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 .byte %00000000
 if ECHOFIRST
       echo "    ",[(scoretable - *)]d , "bytes of ROM space left")
 endif 
ECHOFIRST = 1
 
 
 
; Provided under the CC0 license. See the included LICENSE.txt for details.

; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC-8
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 64
       ORG  $10F80-bscode_length
       RORG $1FF80-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif

; font equates
.21stcentury = 1
alarmclock = 2     
handwritten = 3    
interrupted = 4    
retroputer = 5    
whimsey = 6
tiny = 7
hex = 8

 ifconst font
   if font == hex
     ORG . - 48
   endif
 endif

scoretable

 ifconst font
  if font == .21stcentury
    include "score_graphics.asm.21stcentury"
  endif
  if font == alarmclock
    include "score_graphics.asm.alarmclock"
  endif
  if font == handwritten
    include "score_graphics.asm.handwritten"
  endif
  if font == interrupted
    include "score_graphics.asm.interrupted"
  endif
  if font == retroputer
    include "score_graphics.asm.retroputer"
  endif
  if font == whimsey
    include "score_graphics.asm.whimsey"
  endif
  if font == tiny
    include "score_graphics.asm.tiny"
  endif
  if font == hex
    include "score_graphics.asm.hex"
  endif
 else ; default font

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 

       ifnconst DPC_kernel_options
 
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000 

       endif

 endif

 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 64
       ORG  $10FE0-bscode_length
       RORG $1FFE0-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

 ifconst bankswitch
   if bankswitch == 8
     ORG $2FFC
     RORG $FFFC
   endif
   if bankswitch == 16
     ORG $4FFC
     RORG $FFFC
   endif
   if bankswitch == 32
     ORG $8FFC
     RORG $FFFC
   endif
   if bankswitch == 64
     ORG  $10FF0
     RORG $1FFF0
     lda $ffe0 ; we use wasted space to assist stella with EF format auto-detection
     ORG  $10FF8
     RORG $1FFF8
     ifconst superchip 
       .byte "E","F","S","C"
     else
       .byte "E","F","E","F"
     endif
     ORG  $10FFC
     RORG $1FFFC
   endif
 else
   ifconst ROM2k
     ORG $F7FC
   else
     ORG $FFFC
   endif
 endif
 .word (start & $ffff)
 .word (start & $ffff)
