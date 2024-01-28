; Provided under the CC0 license. See the included LICENSE.txt for details.

 processor 6502
 include "vcs.h"
 include "macro.h"
 include "2600basic.h"
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
   .byte 0 ; stop unexpected bankswitches
 endif
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

     ; This is a 2-line kernel!
     ifnconst vertical_reflect
kernel
     endif
     sta WSYNC
     lda #255
     sta TIM64T

     lda #1
     sta VDELBL
     sta VDELP0
     ldx ballheight
     inx
     inx
     stx temp4
     lda player1y
     sta temp3

     ifconst shakescreen
         jsr doshakescreen
     else
         ldx missile0height
         inx
     endif

     inx
     stx stack1

     lda bally
     sta stack2

     lda player0y
     ldx #0
     sta WSYNC
     stx GRP0
     stx GRP1
     stx PF1L
     stx PF2
     stx CXCLR
     ifconst readpaddle
         stx paddle
     else
         sleep 3
     endif

     sta temp2,x

     ;store these so they can be retrieved later
     ifnconst pfres
         ldx #128-44+(4-pfwidth)*12
     else
         ldx #132-pfres*pfwidth
     endif

     dec player0y

     lda missile0y
     sta temp5
     lda missile1y
     sta temp6

     lda playfieldpos
     sta temp1
     
     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif
     clc
     sbc playfieldpos
     sta playfieldpos
     jmp .startkernel

.skipDrawP0
     lda #0
     tay
     jmp .continueP0

.skipDrawP1
     lda #0
     tay
     jmp .continueP1

.kerloop     ; enter at cycle 59??

continuekernel
     sleep 2
continuekernel2
     lda ballheight
     
     ifconst pfres
         ldy playfield+pfres*pfwidth-132,x
         sty PF1L ;3
         ldy playfield+pfres*pfwidth-131-pfadjust,x
         sty PF2L ;3
         ldy playfield+pfres*pfwidth-129,x
         sty PF1R ; 3 too early?
         ldy playfield+pfres*pfwidth-130-pfadjust,x
         sty PF2R ;3
     else
         ldy playfield-48+pfwidth*12+44-128,x
         sty PF1L ;3
         ldy playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sty PF2L ;3
         ldy playfield-48+pfwidth*12+47-128,x ;4
         sty PF1R ; 3 too early?
         ldy playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sty PF2R ;3
     endif

     ; should be playfield+$38 for width=2

     dcp bally
     rol
     rol
     ; rol
     ; rol
goback
     sta ENABL 
.startkernel
     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawP1 ;2
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continueP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         lda (player1color),y
         sta COLUP1
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif

     ifconst pfres
         lda playfield+pfres*pfwidth-132,x 
         sta PF1L ;3
         lda playfield+pfres*pfwidth-131-pfadjust,x 
         sta PF2L ;3
         lda playfield+pfres*pfwidth-129,x 
         sta PF1R ; 3 too early?
         lda playfield+pfres*pfwidth-130-pfadjust,x 
         sta PF2R ;3
     else
         lda playfield-48+pfwidth*12+44-128,x ;4
         sta PF1L ;3
         lda playfield-48+pfwidth*12+45-128-pfadjust,x ;4
         sta PF2L ;3
         lda playfield-48+pfwidth*12+47-128,x ;4
         sta PF1R ; 3 too early?
         lda playfield-48+pfwidth*12+46-128-pfadjust,x;4
         sta PF2R ;3
     endif 
     ; sleep 3

     lda player0height
     dcp player0y
     bcc .skipDrawP0
     ldy player0y
     lda (player0pointer),y
.continueP0
     sta GRP0

     ifnconst no_blank_lines
         ifnconst playercolors
             lda missile0height ;3
             dcp missile0y ;5
             sbc stack1
             sta ENAM0 ;3
         else
             lda (player0color),y
             sta player0colorstore
             sleep 6
         endif
         dec temp1
         bne continuekernel
     else
         dec temp1
         beq altkernel2
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle
             inc paddle
             jmp continuekernel2
noreadpaddle
             sleep 2
             jmp continuekernel
         else
             ifnconst playercolors 
                 ifconst PFcolors
                     txa
                     tay
                     lda (pfcolortable),y
                     ifnconst backgroundchange
                         sta COLUPF
                     else
                         sta COLUBK
                     endif
                     jmp continuekernel
                 else
                     ifconst kernelmacrodef
                         kernelmacro
                     else
                         sleep 12
                     endif
                 endif
             else
                 lda (player0color),y
                 sta player0colorstore
                 sleep 4
             endif
             jmp continuekernel
         endif
altkernel2
         txa
         ifnconst vertical_reflect
             sbx #256-pfwidth
         else
             sbx #256-pfwidth/2
         endif
         bmi lastkernelline
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
         jmp continuekernel
     endif

altkernel

     ifconst PFmaskvalue
         lda #PFmaskvalue
     else
         lda #0
     endif
     sta PF1L
     sta PF2


     ;sleep 3

     ;28 cycles to fix things
     ;minus 11=17

     ; lax temp4
     ; clc
     txa
     ifnconst vertical_reflect
         sbx #256-pfwidth
     else
         sbx #256-pfwidth/2
     endif

     bmi lastkernelline

     ifconst PFcolorandheight
         ifconst pfres
             ldy playfieldcolorandheight-131+pfres*pfwidth,x
         else
             ldy playfieldcolorandheight-87,x
         endif
         ifnconst backgroundchange
             sty COLUPF
         else
             sty COLUBK
         endif
         ifconst pfres
             lda playfieldcolorandheight-132+pfres*pfwidth,x
         else
             lda playfieldcolorandheight-88,x
         endif
         sta.w temp1
     endif
     ifconst PFheights
         lsr
         lsr
         tay
         lda (pfheighttable),y
         sta.w temp1
     endif
     ifconst PFcolors
         tay
         lda (pfcolortable),y
         ifnconst backgroundchange
             sta COLUPF
         else
             sta COLUBK
         endif
         ifconst pfrowheight
             lda #pfrowheight
         else
             ifnconst pfres
                 lda #8
             else
                 lda #(96/pfres) ; try to come close to the real size
             endif
         endif
         sta temp1
     endif
     ifnconst PFcolorandheight
         ifnconst PFcolors
             ifnconst PFheights
                 ifnconst no_blank_lines
                     ; read paddle 0
                     ; lo-res paddle read
                     ; bit INPT0
                     ; bmi paddleskipread
                     ; inc paddle0
                     ;donepaddleskip
                     sleep 10
                     ifconst pfrowheight
                         lda #pfrowheight
                     else
                         ifnconst pfres
                             lda #8
                         else
                             lda #(96/pfres) ; try to come close to the real size
                         endif
                     endif
                     sta temp1
                 endif
             endif
         endif
     endif
     

     lda ballheight
     dcp bally
     sbc temp4


     jmp goback


     ifnconst no_blank_lines
lastkernelline
         ifnconst PFcolors
             sleep 10
         else
             ldy #124
             lda (pfcolortable),y
             sta COLUPF
         endif

         ifconst PFheights
             ldx #1
             ;sleep 4
             sleep 3 ; this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 3
             sleep 2 ; this was over 1 cycle
         endif

         jmp enterlastkernel

     else
lastkernelline
         
         ifconst PFheights
             ldx #1
             ;sleep 5
             sleep 4 ; this was over 1 cycle
         else
             ldx playfieldpos
             ;sleep 4
             sleep 3 ; this was over 1 cycle
         endif

         cpx #0
         bne .enterfromNBL
         jmp no_blank_lines_bailout
     endif

     if ((<*)>$d5)
         align 256
     endif
     ; this is a kludge to prevent page wrapping - fix!!!

.skipDrawlastP1
     lda #0
     tay ; added so we don't cross a page
     jmp .continuelastP1

.endkerloop     ; enter at cycle 59??
     
     nop

.enterfromNBL
     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

enterlastkernel
     lda ballheight

     ; tya
     dcp bally
     ; sleep 4

     ; sbc stack3
     rol
     rol
     sta ENABL 

     lda player1height ;3
     dcp player1y ;5
     bcc .skipDrawlastP1
     ldy player1y ;3
     lda (player1pointer),y ;5; player0pointer must be selected carefully by the compiler
     ; so it doesn't cross a page boundary!

.continuelastP1
     sta GRP1 ;3

     ifnconst player1colors
         lda missile1height ;3
         dcp missile1y ;5
     else
         lda (player1color),y
         sta COLUP1
     endif

     dex
     ;dec temp4 ; might try putting this above PF writes
     beq endkernel


     ifconst pfres
         ldy.w playfield+pfres*pfwidth-4
         sty PF1L ;3
         ldy.w playfield+pfres*pfwidth-3-pfadjust
         sty PF2L ;3
         ldy.w playfield+pfres*pfwidth-1
         sty PF1R ; possibly too early?
         ldy.w playfield+pfres*pfwidth-2-pfadjust
         sty PF2R ;3
     else
         ldy.w playfield-48+pfwidth*12+44
         sty PF1L ;3
         ldy.w playfield-48+pfwidth*12+45-pfadjust
         sty PF2L ;3
         ldy.w playfield-48+pfwidth*12+47
         sty PF1R ; possibly too early?
         ldy.w playfield-48+pfwidth*12+46-pfadjust
         sty PF2R ;3
     endif

     ifnconst player1colors
         rol;2
         rol;2
         sta ENAM1 ;3
     else
         ifnconst playercolors
             sleep 7
         else
             lda.w player0colorstore
             sta COLUP0
         endif
     endif
     
     lda.w player0height
     dcp player0y
     bcc .skipDrawlastP0
     ldy player0y
     lda (player0pointer),y
.continuelastP0
     sta GRP0



     ifnconst no_blank_lines
         lda missile0height ;3
         dcp missile0y ;5
         sbc stack1
         sta ENAM0 ;3
         jmp .endkerloop
     else
         ifconst readpaddle
             ldy currentpaddle
             lda INPT0,y
             bpl noreadpaddle2
             inc paddle
             jmp .endkerloop
noreadpaddle2
             sleep 4
             jmp .endkerloop
         else ; no_blank_lines and no paddle reading
             pla
             pha ; 14 cycles in 4 bytes
             pla
             pha
             ; sleep 14
             jmp .endkerloop
         endif
     endif


     ; ifconst donepaddleskip
         ;paddleskipread
         ; this is kind of lame, since it requires 4 cycles from a page boundary crossing
         ; plus we get a lo-res paddle read
         ; bmi donepaddleskip
     ; endif

.skipDrawlastP0
     lda #0
     tay
     jmp .continuelastP0

     ifconst no_blank_lines
no_blank_lines_bailout
         ldx #0
     endif

endkernel
     ; 6 digit score routine
     stx PF1
     stx PF2
     stx PF0
     clc

     ifconst pfrowheight
         lda #pfrowheight+2
     else
         ifnconst pfres
             lda #10
         else
             lda #(96/pfres)+2 ; try to come close to the real size
         endif
     endif

     sbc playfieldpos
     sta playfieldpos
     txa

     ifconst shakescreen
         bit shakescreen
         bmi noshakescreen2
         ldx #$3D
noshakescreen2
     endif

     sta WSYNC,x

     ; STA WSYNC ;first one, need one more
     sta REFP0
     sta REFP1
     STA GRP0
     STA GRP1
     ; STA PF1
     ; STA PF2
     sta HMCLR
     sta ENAM0
     sta ENAM1
     sta ENABL

     lda temp2 ;restore variables that were obliterated by kernel
     sta player0y
     lda temp3
     sta player1y
     ifnconst player1colors
         lda temp6
         sta missile1y
     endif
     ifnconst playercolors
         ifnconst readpaddle
             lda temp5
             sta missile0y
         endif
     endif
     lda stack2
     sta bally

     ; strangely, this isn't required any more. might have
     ; resulted from the no_blank_lines score bounce fix
     ;ifconst no_blank_lines
         ;sta WSYNC
     ;endif

     lda INTIM
     clc
     ifnconst vblank_time
         adc #43+12+87
     else
         adc #vblank_time+12+87

     endif
     ; sta WSYNC
     sta TIM64T

     ifconst minikernel
         jsr minikernel
     endif

     ; now reassign temp vars for score pointers

     ; score pointers contain:
     ; score1-5: lo1,lo2,lo3,lo4,lo5,lo6
     ; swap lo2->temp1
     ; swap lo4->temp3
     ; swap lo6->temp5
     ifnconst noscore
         lda scorepointers+1
         ; ldy temp1
         sta temp1
         ; sty scorepointers+1

         lda scorepointers+3
         ; ldy temp3
         sta temp3
         ; sty scorepointers+3


         sta HMCLR
         tsx
         stx stack1 
         ldx #$E0
         stx HMP0

         LDA scorecolor 
         STA COLUP0
         STA COLUP1
         ifconst scorefade
             STA stack2
         endif
         ifconst pfscore
             lda pfscorecolor
             sta COLUPF
         endif
         sta WSYNC
         ldx #0
         STx GRP0
         STx GRP1 ; seems to be needed because of vdel

         lda scorepointers+5
         ; ldy temp5
         sta temp5,x
         ; sty scorepointers+5
         lda #>scoretable
         sta scorepointers+1
         sta scorepointers+3
         sta scorepointers+5
         sta temp2
         sta temp4
         sta temp6
         LDY #7
         STY VDELP0
         STA RESP0
         STA RESP1


         LDA #$03
         STA NUSIZ0
         STA NUSIZ1
         STA VDELP1
         LDA #$F0
         STA HMP1
         lda (scorepointers),y
         sta GRP0
         STA HMOVE ; cycle 73 ?
         jmp beginscore


         if ((<*)>$d4)
             align 256 ; kludge that potentially wastes space! should be fixed!
         endif

loop2
         lda (scorepointers),y ;+5 68 204
         sta GRP0 ;+3 71 213 D1 -- -- --
         ifconst pfscore
             lda.w pfscore1
             sta PF1
         else
             ifconst scorefade
                 sleep 2
                 dec stack2 ; decrement the temporary scorecolor
             else
                 sleep 7
             endif
         endif
         ; cycle 0
beginscore
         lda (scorepointers+$8),y ;+5 5 15
         sta GRP1 ;+3 8 24 D1 D1 D2 --
         lda (scorepointers+$6),y ;+5 13 39
         sta GRP0 ;+3 16 48 D3 D1 D2 D2
         lax (scorepointers+$2),y ;+5 29 87
         txs
         lax (scorepointers+$4),y ;+5 36 108
         ifconst scorefade
             lda stack2
         else
             sleep 3
         endif

         ifconst pfscore
             lda pfscore2
             sta PF1
         else
             ifconst scorefade
                 sta COLUP0
                 sta COLUP1
             else
                 sleep 6
             endif
         endif

         lda (scorepointers+$A),y ;+5 21 63
         stx GRP1 ;+3 44 132 D3 D3 D4 D2!
         tsx
         stx GRP0 ;+3 47 141 D5 D3! D4 D4
         sta GRP1 ;+3 50 150 D5 D5 D6 D4!
         sty GRP0 ;+3 53 159 D4* D5! D6 D6
         dey
         bpl loop2 ;+2 60 180

         ldx stack1 
         txs
         ; lda scorepointers+1
         ldy temp1
         ; sta temp1
         sty scorepointers+1

         LDA #0 
         sta PF1
         STA GRP0
         STA GRP1
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
     endif ;noscore
    ifconst readpaddle
        lda #%11000010
    else
        ifconst qtcontroller
            lda qtcontroller
            lsr    ; bit 0 in carry
            lda #4
            ror    ; carry into top of A
        else
            lda #2
        endif ; qtcontroller
    endif ; readpaddle
 sta WSYNC
 sta VBLANK
 RETURN
     ifconst shakescreen
doshakescreen
         bit shakescreen
         bmi noshakescreen
         sta WSYNC
noshakescreen
         ldx missile0height
         inx
         rts
     endif

; Provided under the CC0 license. See the included LICENSE.txt for details.

; playfield drawing routines
; you get a 32x12 bitmapped display in a single color :)
; 0-31 and 0-11

pfclear ; clears playfield - or fill with pattern
 ifconst pfres
 ldx #pfres*pfwidth-1
 else
 ldx #47-(4-pfwidth)*12 ; will this work?
 endif
pfclear_loop
 ifnconst superchip
 sta playfield,x
 else
 sta playfield-128,x
 endif
 dex
 bpl pfclear_loop
 RETURN
 
setuppointers
 stx temp2 ; store on.off.flip value
 tax ; put x-value in x 
 lsr
 lsr
 lsr ; divide x pos by 8 
 sta temp1
 tya
 asl
 if pfwidth=4
  asl ; multiply y pos by 4
 endif ; else multiply by 2
 clc
 adc temp1 ; add them together to get actual memory location offset
 tay ; put the value in y
 lda temp2 ; restore on.off.flip value
 rts

pfread
;x=xvalue, y=yvalue
 jsr setuppointers
 lda setbyte,x
 and playfield,y
 eor setbyte,x
; beq readzero
; lda #1
; readzero
 RETURN

pfpixel
;x=xvalue, y=yvalue, a=0,1,2
 jsr setuppointers

 ifconst bankswitch
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon_r  ; if "on" go to on
 lsr
 bcs pixeloff_r ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixelon_r
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN
pixeloff_r
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 RETURN

 else
 jmp plotpoint
 endif

pfhline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 jmp noinc
keepgoing
 inx
 txa
 and #7
 bne noinc
 iny
noinc
 jsr plotpoint
 cpx temp3
 bmi keepgoing
 RETURN

pfvline
;x=xvalue, y=yvalue, a=0,1,2, temp3=endx
 jsr setuppointers
 sty temp1 ; store memory location offset
 inc temp3 ; increase final x by 1 
 lda temp3
 asl
 if pfwidth=4
   asl ; multiply by 4
 endif ; else multiply by 2
 sta temp3 ; store it
 ; Thanks to Michael Rideout for fixing a bug in this code
 ; right now, temp1=y=starting memory location, temp3=final
 ; x should equal original x value
keepgoingy
 jsr plotpoint
 iny
 iny
 if pfwidth=4
   iny
   iny
 endif
 cpy temp3
 bmi keepgoingy
 RETURN

plotpoint
 lda temp2 ; load on.off.flip value (0,1, or 2)
 beq pixelon  ; if "on" go to on
 lsr
 bcs pixeloff ; value is 1 if true
 lda playfield,y ; if here, it's "flip"
 eor setbyte,x
  ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixelon
 lda playfield,y
 ora setbyte,x
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts
pixeloff
 lda setbyte,x
 eor #$ff
 and playfield,y
 ifconst superchip
 sta playfield-128,y
 else
 sta playfield,y
 endif
 rts

setbyte
 ifnconst pfcenter
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 endif
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
 .byte $80
 .byte $40
 .byte $20
 .byte $10
 .byte $08
 .byte $04
 .byte $02
 .byte $01
 .byte $01
 .byte $02
 .byte $04
 .byte $08
 .byte $10
 .byte $20
 .byte $40
 .byte $80
; Provided under the CC0 license. See the included LICENSE.txt for details.

pfscroll ;(a=0 left, 1 right, 2 up, 4 down, 6=upup, 12=downdown)
 bne notleft
;left
 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
leftloop
 lda playfield-1,x
 lsr

 ifconst superchip
 lda playfield-2,x
 rol
 sta playfield-130,x
 lda playfield-3,x
 ror
 sta playfield-131,x
 lda playfield-4,x
 rol
 sta playfield-132,x
 lda playfield-1,x
 ror
 sta playfield-129,x
 else
 rol playfield-2,x
 ror playfield-3,x
 rol playfield-4,x
 ror playfield-1,x
 endif

 txa
 sbx #4
 bne leftloop
 RETURN

notleft
 lsr
 bcc notright
;right

 ifconst pfres
 ldx #pfres*4
 else
 ldx #48
 endif
rightloop
 lda playfield-4,x
 lsr
 ifconst superchip
 lda playfield-3,x
 rol
 sta playfield-131,x
 lda playfield-2,x
 ror
 sta playfield-130,x
 lda playfield-1,x
 rol
 sta playfield-129,x
 lda playfield-4,x
 ror
 sta playfield-132,x
 else
 rol playfield-3,x
 ror playfield-2,x
 rol playfield-1,x
 ror playfield-4,x
 endif
 txa
 sbx #4
 bne rightloop
  RETURN

notright
 lsr
 bcc notup
;up
 lsr
 bcc onedecup
 dec playfieldpos
onedecup
 dec playfieldpos
 beq shiftdown 
 bpl noshiftdown2 
shiftdown
  ifconst pfrowheight
 lda #pfrowheight
 else
 ifnconst pfres
   lda #8
 else
   lda #(96/pfres) ; try to come close to the real size
 endif
 endif

 sta playfieldpos
 lda playfield+3
 sta temp4
 lda playfield+2
 sta temp3
 lda playfield+1
 sta temp2
 lda playfield
 sta temp1
 ldx #0
up2
 lda playfield+4,x
 ifconst superchip
 sta playfield-128,x
 lda playfield+5,x
 sta playfield-127,x
 lda playfield+6,x
 sta playfield-126,x
 lda playfield+7,x
 sta playfield-125,x
 else
 sta playfield,x
 lda playfield+5,x
 sta playfield+1,x
 lda playfield+6,x
 sta playfield+2,x
 lda playfield+7,x
 sta playfield+3,x
 endif
 txa
 sbx #252
 ifconst pfres
 cpx #(pfres-1)*4
 else
 cpx #44
 endif
 bne up2

 lda temp4
 
 ifconst superchip
 ifconst pfres
 sta playfield+pfres*4-129
 lda temp3
 sta playfield+pfres*4-130
 lda temp2
 sta playfield+pfres*4-131
 lda temp1
 sta playfield+pfres*4-132
 else
 sta playfield+47-128
 lda temp3
 sta playfield+46-128
 lda temp2
 sta playfield+45-128
 lda temp1
 sta playfield+44-128
 endif
 else
 ifconst pfres
 sta playfield+pfres*4-1
 lda temp3
 sta playfield+pfres*4-2
 lda temp2
 sta playfield+pfres*4-3
 lda temp1
 sta playfield+pfres*4-4
 else
 sta playfield+47
 lda temp3
 sta playfield+46
 lda temp2
 sta playfield+45
 lda temp1
 sta playfield+44
 endif
 endif
noshiftdown2
 RETURN


notup
;down
 lsr
 bcs oneincup
 inc playfieldpos
oneincup
 inc playfieldpos
 lda playfieldpos

  ifconst pfrowheight
 cmp #pfrowheight+1
 else
 ifnconst pfres
   cmp #9
 else
   cmp #(96/pfres)+1 ; try to come close to the real size
 endif
 endif

 bcc noshiftdown 
 lda #1
 sta playfieldpos

 ifconst pfres
 lda playfield+pfres*4-1
 sta temp4
 lda playfield+pfres*4-2
 sta temp3
 lda playfield+pfres*4-3
 sta temp2
 lda playfield+pfres*4-4
 else
 lda playfield+47
 sta temp4
 lda playfield+46
 sta temp3
 lda playfield+45
 sta temp2
 lda playfield+44
 endif

 sta temp1

 ifconst pfres
 ldx #(pfres-1)*4
 else
 ldx #44
 endif
down2
 lda playfield-1,x
 ifconst superchip
 sta playfield-125,x
 lda playfield-2,x
 sta playfield-126,x
 lda playfield-3,x
 sta playfield-127,x
 lda playfield-4,x
 sta playfield-128,x
 else
 sta playfield+3,x
 lda playfield-2,x
 sta playfield+2,x
 lda playfield-3,x
 sta playfield+1,x
 lda playfield-4,x
 sta playfield,x
 endif
 txa
 sbx #4
 bne down2

 lda temp4
 ifconst superchip
 sta playfield-125
 lda temp3
 sta playfield-126
 lda temp2
 sta playfield-127
 lda temp1
 sta playfield-128
 else
 sta playfield+3
 lda temp3
 sta playfield+2
 lda temp2
 sta playfield+1
 lda temp1
 sta playfield
 endif
noshiftdown
 RETURN
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
; Provided under the CC0 license. See the included LICENSE.txt for details.

drawscreen
     ifconst debugscore
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

         ; cmp #$2B
         ; bcs no_cycles_left
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
         jmp done_debugscore 
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
done_debugscore
     endif

     ifconst debugcycles
         lda INTIM ; if we go over, it mucks up the background color
         ; cmp #$2B
         ; BCC overscan
         bmi overscan
         sta COLUBK
         bcs doneoverscan
     endif

overscan
     ifconst interlaced
         PHP
         PLA 
         EOR #4 ; flip interrupt bit
         PHA
         PLP
         AND #4 ; isolate the interrupt bit
         TAX ; save it for later
     endif

overscanloop
     lda INTIM ;wait for sync
     bmi overscanloop
doneoverscan

     ;do VSYNC

     ifconst interlaced
         CPX #4
         BNE oddframevsync
     endif

     lda #2
     sta WSYNC
     sta VSYNC
     STA WSYNC
     STA WSYNC
     lsr
     STA WSYNC
     STA VSYNC
     sta VBLANK
     ifnconst overscan_time
         lda #37+128
     else
         lda #overscan_time+128
     endif
     sta TIM64T

     ifconst interlaced
         jmp postsync 

oddframevsync
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #2
         sta VSYNC
         sta WSYNC
         sta WSYNC
         sta WSYNC

         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste
         LDA ($80,X) ; 11 waste

         lda #0
         sta VSYNC
         sta VBLANK
         ifnconst overscan_time
             lda #37+128
         else
             lda #overscan_time+128
         endif
         sta TIM64T

postsync
     endif

     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop
             lda player0x,x
             sec
             sbc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop
         endif
     endif
     if ((<*)>$e9)&&((<*)<$fa)
         repeat ($fa-(<*))
         nop
         repend
     endif
     sta WSYNC
     ldx #4
     SLEEP 3
HorPosLoop     ; 5
     lda player0x,X ;+4 9
     sec ;+2 11
DivideLoop
     sbc #15
     bcs DivideLoop;+4 15
     sta temp1,X ;+4 19
     sta RESP0,X ;+4 23
     sta WSYNC
     dex
     bpl HorPosLoop;+5 5
     ; 4

     ldx #4
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 18

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 32

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 46

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 60

     dex
     ldy temp1,X
     lda repostable-256,Y
     sta HMP0,X ;+14 74

     sta WSYNC
     
     sta HMOVE ;+3 3


     ifconst legacy
         if legacy < 100
             ldx #4
adjustloop2
             lda player0x,x
             clc
             adc #14 ;?
             sta player0x,x
             dex
             bpl adjustloop2
         endif
     endif




     ;set score pointers
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

vblk
     ; run possible vblank bB code
     ifconst vblank_bB_code
         jsr vblank_bB_code
     endif
vblk2
     LDA INTIM
     bmi vblk2
     jmp kernel
     

     .byte $80,$70,$60,$50,$40,$30,$20,$10,$00
     .byte $F0,$E0,$D0,$C0,$B0,$A0,$90
repostable

scorepointerset
     and #$0F
     asl
     asl
     asl
     adc #<scoretable
     tay 
     txa
     ; and #$F0
     ; lsr
     asr #$F0
     adc #<scoretable
     tax
     rts
game
.L00 ;  set kernel_options playercolors player1colors pfcolors

.L01 ;  set optimization size

.L02 ;  set optimization speed

.
 ; 

.L03 ;  dim audioDur2  =  E

.L04 ;  dim audioDur  =  Z

.L05 ;  dim goldAppleX  =  W

.L06 ;  dim goldAppleY  =  V

.L07 ;  dim roomX  =  A

.L08 ;  dim roomY  =  B

.L09 ;  dim tempC  =  C

.L010 ;  dim tempD  =  D

.L011 ;  dim tempT  =  T

.L012 ;  dim tempU  =  U

.L013 ;  dim tempG  =  G

.L014 ;  dim arrowDir  =  P

.L015 ;  dim frame  =  F

.L016 ;  dim dayNight  =  Y

.L017 ;  dim _sc1  =  score

.L018 ;  dim _sc2  =  score + 1

.L019 ;  dim _sc3  =  score + 2

.
 ; 

.L020 ;  R  =  $30

	LDA #$30
	STA R
.L021 ;  goldAppleX  =  128

	LDA #128
	STA goldAppleX
.L022 ;  goldAppleY  =  128

	LDA #128
	STA goldAppleY
.L023 ;  roomX  =  127

	LDA #127
	STA roomX
.L024 ;  roomY  =  127

	LDA #127
	STA roomY
.L025 ;  PF0  =  %11110000

	LDA #%11110000
	STA PF0
.L026 ;  scorecolor = $0E

	LDA #$0E
	STA scorecolor
.
 ; 

.L027 ;  pfcolors:

 lda # $D4
 sta COLUPF
 ifconst pfres
 lda #>(pfcolorlabel13-132+pfres*pfwidth)
 else
 lda #>(pfcolorlabel13-84)
 endif
 sta pfcolortable+1
 ifconst pfres
 lda #<(pfcolorlabel13-132+pfres*pfwidth)
 else
 lda #<(pfcolorlabel13-84)
 endif
 sta pfcolortable
.
 ; 

.L028 ;  player1x  =  70

	LDA #70
	STA player1x
.L029 ;  player1y  =  60

	LDA #60
	STA player1y
.L030 ;  playfield:

  ifconst pfres
	  ldx #(12>pfres)*(pfres*pfwidth-1)+(12<=pfres)*47
  else
	  ldx #((12*pfwidth-1)*((12*pfwidth-1)<47))+(47*((12*pfwidth-1)>=47))
  endif
	jmp pflabel0
PF_data0
	.byte %01100011, %00101001
	if (pfwidth>2)
	.byte %11110001, %00011000
 endif
	.byte %01010010, %00101000
	if (pfwidth>2)
	.byte %10000010, %00101001
 endif
	.byte %01010011, %00101001
	if (pfwidth>2)
	.byte %10110010, %00101001
 endif
	.byte %01010010, %00101000
	if (pfwidth>2)
	.byte %10010010, %00101001
 endif
	.byte %01110011, %00010001
	if (pfwidth>2)
	.byte %11110001, %00111000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00111000, %10000111
	if (pfwidth>2)
	.byte %11000111, %00111000
 endif
	.byte %01111101, %11001111
	if (pfwidth>2)
	.byte %11101111, %01111101
 endif
	.byte %01111101, %11001111
	if (pfwidth>2)
	.byte %11101111, %01111101
 endif
	.byte %00010000, %00000010
	if (pfwidth>2)
	.byte %10000010, %00010000
 endif
	.byte %00010000, %00000010
	if (pfwidth>2)
	.byte %10000010, %00010000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel0
	lda PF_data0,x
	sta playfield,x
	dex
	bpl pflabel0
.L031 ;  gosub setPlayerGraphics

 jsr .setPlayerGraphics

.L032 ;  gosub scoreColorChange

 jsr .scoreColorChange

.titleScreen
 ; titleScreen

.L033 ;  COLUBK  =  $D0

	LDA #$D0
	STA COLUBK
.L034 ;  if joy0fire then goto init

 bit INPT4
	BMI .skipL034
.condpart0
 jmp .init

.skipL034
.L035 ;  player1x  =  player1x + 1

	INC player1x
.L036 ;  if player1x  >  150 then player1x  =  1  :  player1y  =   ( rand & 70 )  + 10

	LDA #150
	CMP player1x
     BCS .skipL036
.condpart1
	LDA #1
	STA player1x
; complex statement detected
 jsr randomize
	AND #70
	CLC
	ADC #10
	STA player1y
.skipL036
.L037 ;  if  ( player1x )  & 15  >  7 then REFP1  =  8

; complex condition detected
; complex statement detected
	LDA player1x
	AND #15
  PHA
  TSX
  PLA
	LDA #7
	CMP  1,x
     BCS .skipL037
.condpart2
	LDA #8
	STA REFP1
.skipL037
.L038 ;  drawscreen

 jsr drawscreen
.L039 ;  goto titleScreen

 jmp .titleScreen

.
 ; 

.init
 ; init

.L040 ;  gosub genRoom

 jsr .genRoom

.L041 ;  goto main

 jmp .main

.
 ; 

.setEnemy
 ; setEnemy

.L042 ;  player0color:

	LDX #<playercolorL042_0
	STX player0color
	LDA #>playercolorL042_0
	STA player0color+1
.
 ; 

.L043 ;  player0:

	LDX #<playerL043_0
	STX player0pointerlo
	LDA #>playerL043_0
	STA player0pointerhi
	LDA #10
	STA player0height
.L044 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.setArrow
 ; setArrow

.L045 ;  if arrowDir  =  0 then player0:

	LDA arrowDir
	CMP #0
     BNE .skipL045
.condpart3
	LDX #<player3then_0
	STX player0pointerlo
	LDA #>player3then_0
	STA player0pointerhi
	LDA #7
	STA player0height
.skipL045
.L046 ;  if roomY  <  goldAppleY then Q  =  0 else Q  =  1

	LDA roomY
	CMP goldAppleY
     BCS .skipL046
.condpart4
	LDA #0
	STA Q
 jmp .skipelse0
.skipL046
	LDA #1
	STA Q
.skipelse0
.L047 ;  if arrowDir  =  1  &&  Q  =  1 then player0:

	LDA arrowDir
	CMP #1
     BNE .skipL047
.condpart5
	LDA Q
	CMP #1
     BNE .skip5then
.condpart6
	LDX #<player6then_0
	STX player0pointerlo
	LDA #>player6then_0
	STA player0pointerhi
	LDA #7
	STA player0height
.skip5then
.skipL047
.L048 ;  if arrowDir  =  1  &&  Q  =  0 then player0:

	LDA arrowDir
	CMP #1
     BNE .skipL048
.condpart7
	LDA Q
	CMP #0
     BNE .skip7then
.condpart8
	LDX #<player8then_0
	STX player0pointerlo
	LDA #>player8then_0
	STA player0pointerhi
	LDA #7
	STA player0height
.skip7then
.skipL048
.L049 ;  player0color:

	LDX #<playercolorL049_0
	STX player0color
	LDA #>playercolorL049_0
	STA player0color+1
.L050 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.setApple
 ; setApple

.L051 ;  player0:

	LDX #<playerL051_0
	STX player0pointerlo
	LDA #>playerL051_0
	STA player0pointerhi
	LDA #7
	STA player0height
.
 ; 

.L052 ;  player0color:

	LDX #<playercolorL052_0
	STX player0color
	LDA #>playercolorL052_0
	STA player0color+1
.L053 ;  if goldAppleX  =  roomX  &&  goldAppleY  =  roomY then player0color:

	LDA goldAppleX
	CMP roomX
     BNE .skipL053
.condpart9
	LDA goldAppleY
	CMP roomY
     BNE .skip9then
.condpart10
	LDX #<playercolor10then_0
	STX player0color
	LDA #>playercolor10then_0
	STA player0color+1
.skip9then
.skipL053
.
 ; 

.L054 ;  if goldAppleX  =  roomX  &&  goldAppleY  =  roomY then return 0

	LDA goldAppleX
	CMP roomX
     BNE .skipL054
.condpart11
	LDA goldAppleY
	CMP roomY
     BNE .skip11then
.condpart12
	LDY #0

	tya
	RTS
.skip11then
.skipL054
.L055 ;  if goldAppleX  =  roomX  ||  goldAppleY  =  roomY then player0color:

	LDA goldAppleX
	CMP roomX
     BNE .skipL055
.condpart13
 jmp .condpart14
.skipL055
	LDA goldAppleY
	CMP roomY
     BNE .skip4OR
.condpart14
	LDX #<playercolor14then_0
	STX player0color
	LDA #>playercolor14then_0
	STA player0color+1
.skip4OR
.
 ; 

.L056 ;  tempU  =  0

	LDA #0
	STA tempU
.L057 ;  if goldAppleX  =  roomX  ||  goldAppleY  =  roomY then return 0

	LDA goldAppleX
	CMP roomX
     BNE .skipL057
.condpart15
 jmp .condpart16
.skipL057
	LDA goldAppleY
	CMP roomY
     BNE .skip5OR
.condpart16
	LDY #0

	tya
	RTS
.skip5OR
.L058 ;  if  (   ( roomX & 1 )   ^   ( roomY & 1 )   )   =  1 then gosub setEnemy  :  return 0

; complex condition detected
; complex statement detected
	LDA roomX
	AND #1
	PHA
	LDA roomY
	AND #1
	TSX
	INX
	TXS
	EOR $00,x
	CMP #1
     BNE .skipL058
.condpart17
 jsr .setEnemy
	LDY #0

	tya
	RTS
.skipL058
.L059 ;  tempU  =  rand & 4

 jsr randomize
	AND #4
	STA tempU
.L060 ;  if tempU  >  0 then gosub setArrow

	LDA #0
	CMP tempU
     BCS .skipL060
.condpart18
 jsr .setArrow

.skipL060
.L061 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.setPlayerGraphics
 ; setPlayerGraphics

.L062 ;  player1color:

	LDX #<playercolorL062_1
	STX player1color
	LDA #>playercolorL062_1
	STA player1color+1
.
 ; 

.L063 ;  player1:

	LDX #<playerL063_1
	STX player1pointerlo
	LDA #>playerL063_1
	STA player1pointerhi
	LDA #10
	STA player1height
.L064 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.randomizeApplePos
 ; randomizeApplePos

.L065 ;  tempG  =  rand & 15

 jsr randomize
	AND #15
	STA tempG
.L066 ;  tempG  =  tempG + 1

	INC tempG
.L067 ;  goldAppleX  =  goldAppleX - 15  +  tempG

; complex statement detected
	LDA goldAppleX
	SEC
	SBC #15
	CLC
	ADC tempG
	STA goldAppleX
.L068 ;  tempG  =  rand & 15

 jsr randomize
	AND #15
	STA tempG
.L069 ;  tempG  =  tempG + 1

	INC tempG
.L070 ;  goldAppleY  =  goldAppleY - 15  +  tempG

; complex statement detected
	LDA goldAppleY
	SEC
	SBC #15
	CLC
	ADC tempG
	STA goldAppleY
.L071 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.changePlayer1Sprite
 ; changePlayer1Sprite

.L072 ;  if  (  player1x  +  player1y )  & 15  >  7 then REFP1  =  8  :  AUDF0  =  30

; complex condition detected
; complex statement detected
	LDA player1x
	CLC
	ADC player1y
	AND #15
  PHA
  TSX
  PLA
	LDA #7
	CMP  1,x
     BCS .skipL072
.condpart19
	LDA #8
	STA REFP1
	LDA #30
	STA AUDF0
.skipL072
.L073 ;  if  (  player1x  +  player1y )  & 15  <=  7 then AUDF0  =  31

; complex condition detected
; complex statement detected
	LDA player1x
	CLC
	ADC player1y
	AND #15
  PHA
  TSX
  PLA
	LDA #7
	CMP  1,x
     BCC .skipL073
.condpart20
	LDA #31
	STA AUDF0
.skipL073
.L074 ;  if audioDur  <  10 then AUDV0 = 1  :  AUDC0 = 12  :  audioDur  =  15

	LDA audioDur
	CMP #10
     BCS .skipL074
.condpart21
	LDA #1
	STA AUDV0
	LDA #12
	STA AUDC0
	LDA #15
	STA audioDur
.skipL074
.L075 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.forest
 ; forest

.L076 ;  playfield:

  ifconst pfres
	  ldx #(12>pfres)*(pfres*pfwidth-1)+(12<=pfres)*47
  else
	  ldx #((12*pfwidth-1)*((12*pfwidth-1)<47))+(47*((12*pfwidth-1)>=47))
  endif
	jmp pflabel1
PF_data1
	.byte %00111000, %11000111
	if (pfwidth>2)
	.byte %10000111, %00111000
 endif
	.byte %01111101, %11101111
	if (pfwidth>2)
	.byte %11001111, %01111101
 endif
	.byte %01111101, %11101111
	if (pfwidth>2)
	.byte %11001111, %01111101
 endif
	.byte %00010000, %10000010
	if (pfwidth>2)
	.byte %00000010, %00010000
 endif
	.byte %00010000, %10000010
	if (pfwidth>2)
	.byte %00000010, %00010000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00111000, %10000111
	if (pfwidth>2)
	.byte %11000111, %00111000
 endif
	.byte %01111101, %11001111
	if (pfwidth>2)
	.byte %11101111, %01111101
 endif
	.byte %01111101, %11001111
	if (pfwidth>2)
	.byte %11101111, %01111101
 endif
	.byte %00010000, %00000010
	if (pfwidth>2)
	.byte %10000010, %00010000
 endif
	.byte %00010000, %00000010
	if (pfwidth>2)
	.byte %10000010, %00010000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel1
	lda PF_data1,x
	sta playfield,x
	dex
	bpl pflabel1
.L077 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.flowers
 ; flowers

.L078 ;  playfield:

  ifconst pfres
	  ldx #(12>pfres)*(pfres*pfwidth-1)+(12<=pfres)*47
  else
	  ldx #((12*pfwidth-1)*((12*pfwidth-1)<47))+(47*((12*pfwidth-1)>=47))
  endif
	jmp pflabel2
PF_data2
	.byte %00000000, %10000000
	if (pfwidth>2)
	.byte %11000000, %00000000
 endif
	.byte %00111000, %00000000
	if (pfwidth>2)
	.byte %10000000, %00011100
 endif
	.byte %00010000, %00111000
	if (pfwidth>2)
	.byte %00000000, %00001000
 endif
	.byte %00000000, %00010000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00001110, %00000000
	if (pfwidth>2)
	.byte %00000000, %00001110
 endif
	.byte %00000100, %10000000
	if (pfwidth>2)
	.byte %11000000, %00000100
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %10000000, %00000000
 endif
	.byte %00000001, %00000011
	if (pfwidth>2)
	.byte %00001110, %00000000
 endif
	.byte %01110000, %00000001
	if (pfwidth>2)
	.byte %00000100, %00000000
 endif
	.byte %00100000, %00111000
	if (pfwidth>2)
	.byte %00000000, %00011100
 endif
	.byte %00000000, %00010000
	if (pfwidth>2)
	.byte %00000000, %00001000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
pflabel2
	lda PF_data2,x
	sta playfield,x
	dex
	bpl pflabel2
.L079 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.swamp
 ; swamp

.L080 ;  playfield:

  ifconst pfres
	  ldx #(12>pfres)*(pfres*pfwidth-1)+(12<=pfres)*47
  else
	  ldx #((12*pfwidth-1)*((12*pfwidth-1)<47))+(47*((12*pfwidth-1)>=47))
  endif
	jmp pflabel3
PF_data3
	.byte %00001100, %00110000
	if (pfwidth>2)
	.byte %00001100, %00110000
 endif
	.byte %00011110, %01111000
	if (pfwidth>2)
	.byte %00011110, %01111000
 endif
	.byte %00001100, %00110000
	if (pfwidth>2)
	.byte %00001100, %00110000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000001, %10000001
	if (pfwidth>2)
	.byte %10000001, %00000001
 endif
	.byte %00000011, %11000011
	if (pfwidth>2)
	.byte %11000011, %00000011
 endif
	.byte %00000001, %10000001
	if (pfwidth>2)
	.byte %10000001, %00000001
 endif
	.byte %00000000, %00011000
	if (pfwidth>2)
	.byte %00011000, %00000000
 endif
	.byte %00000000, %00111100
	if (pfwidth>2)
	.byte %00111100, %00000000
 endif
	.byte %00011000, %00111100
	if (pfwidth>2)
	.byte %00111100, %00011000
 endif
	.byte %00111100, %00011000
	if (pfwidth>2)
	.byte %00011000, %00111100
 endif
	.byte %00011000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00011000
 endif
pflabel3
	lda PF_data3,x
	sta playfield,x
	dex
	bpl pflabel3
.L081 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.hill
 ; hill

.L082 ;  playfield:

  ifconst pfres
	  ldx #(12>pfres)*(pfres*pfwidth-1)+(12<=pfres)*47
  else
	  ldx #((12*pfwidth-1)*((12*pfwidth-1)<47))+(47*((12*pfwidth-1)>=47))
  endif
	jmp pflabel4
PF_data4
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %00000000
	if (pfwidth>2)
	.byte %00000000, %00000000
 endif
	.byte %00000000, %11000000
	if (pfwidth>2)
	.byte %11000000, %00000000
 endif
	.byte %00000000, %01110000
	if (pfwidth>2)
	.byte %11110000, %00000000
 endif
	.byte %00000000, %11111100
	if (pfwidth>2)
	.byte %11011100, %00000000
 endif
	.byte %00000000, %11110111
	if (pfwidth>2)
	.byte %11111111, %00000000
 endif
	.byte %00000011, %01111111
	if (pfwidth>2)
	.byte %11111011, %00000011
 endif
	.byte %00001110, %11111111
	if (pfwidth>2)
	.byte %11101111, %00001111
 endif
	.byte %00111111, %11111101
	if (pfwidth>2)
	.byte %11111101, %00111111
 endif
	.byte %11111111, %11101111
	if (pfwidth>2)
	.byte %11111111, %11111011
 endif
pflabel4
	lda PF_data4,x
	sta playfield,x
	dex
	bpl pflabel4
.L083 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.setRoomColor
 ; setRoomColor

.
 ; 

.L084 ;  if D  =  0 then T  =  $C0

	LDA D
	CMP #0
     BNE .skipL084
.condpart22
	LDA #$C0
	STA T
.skipL084
.L085 ;  if D  =  1 then T  =  $D0

	LDA D
	CMP #1
     BNE .skipL085
.condpart23
	LDA #$D0
	STA T
.skipL085
.L086 ;  if D  =  2 then T  =  $E0

	LDA D
	CMP #2
     BNE .skipL086
.condpart24
	LDA #$E0
	STA T
.skipL086
.L087 ;  if D  =  3 then T  =  $F0

	LDA D
	CMP #3
     BNE .skipL087
.condpart25
	LDA #$F0
	STA T
.skipL087
.L088 ;  COLUBK  =  T

	LDA T
	STA COLUBK
.L089 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.genRoom
 ; genRoom

.L090 ;  rand  =   ( roomX + 1 )   ^   ( roomY + 2 ) 

; complex statement detected
	LDA roomX
	CLC
	ADC #1
	PHA
	LDA roomY
	CLC
	ADC #2
	TSX
	INX
	TXS
	EOR $00,x
	STA rand
.L091 ;  C  =   (  rand  )  & 3

; complex statement detected
 jsr randomize
	AND #3
	STA C
.L092 ;  D  =   (  rand  )  & 3

; complex statement detected
 jsr randomize
	AND #3
	STA D
.
 ; 

.L093 ;  player0x  =   ( rand & 120 )  + 20

; complex statement detected
 jsr randomize
	AND #120
	CLC
	ADC #20
	STA player0x
.L094 ;  player0y  =   ( rand & 60 )  + 20

; complex statement detected
 jsr randomize
	AND #60
	CLC
	ADC #20
	STA player0y
.
 ; 

.L095 ;  S  =   ( roomX & 1 )   ^   ( roomY & 1 ) 

; complex statement detected
	LDA roomX
	AND #1
	PHA
	LDA roomY
	AND #1
	TSX
	INX
	TXS
	EOR $00,x
	STA S
.L096 ;  if S  >  0 then player0x  =  80  :  player0y  =  50

	LDA #0
	CMP S
     BCS .skipL096
.condpart26
	LDA #80
	STA player0x
	LDA #50
	STA player0y
.skipL096
.L097 ;  if C  =  0 then gosub forest

	LDA C
	CMP #0
     BNE .skipL097
.condpart27
 jsr .forest

.skipL097
.L098 ;  if C  =  1 then gosub swamp

	LDA C
	CMP #1
     BNE .skipL098
.condpart28
 jsr .swamp

.skipL098
.L099 ;  if C  =  2 then gosub flowers

	LDA C
	CMP #2
     BNE .skipL099
.condpart29
 jsr .flowers

.skipL099
.L0100 ;  if C  =  3 then gosub hill

	LDA C
	CMP #3
     BNE .skipL0100
.condpart30
 jsr .hill

.skipL0100
.
 ; 

.L0101 ;  gosub setRoomColor

 jsr .setRoomColor

.
 ; 

.L0102 ;  gosub setApple

 jsr .setApple

.
 ; 

.L0103 ;  arrowDir  =  rand & 1

 jsr randomize
	AND #1
	STA arrowDir
.L0104 ;  C  =  rand & 3

 jsr randomize
	AND #3
	STA C
.
 ; 

.L0105 ;  gosub setTransColor

 jsr .setTransColor

.L0106 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.eatSoundEffect
 ; eatSoundEffect

.L0107 ;  audioDur2  =  20

	LDA #20
	STA audioDur2
.L0108 ;  AUDV1  =  2

	LDA #2
	STA AUDV1
.L0109 ;  AUDC1  =  6

	LDA #6
	STA AUDC1
.L0110 ;  AUDF1  =  20

	LDA #20
	STA AUDF1
.L0111 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.roomChange
 ; roomChange

.L0112 ;  if player1x < 1 then player1x  =  150  :  roomX  =  roomX  -  1  :  gosub genRoom

	LDA player1x
	CMP #1
     BCS .skipL0112
.condpart31
	LDA #150
	STA player1x
	DEC roomX
 jsr .genRoom

.skipL0112
.L0113 ;  if player1x > 150 then player1x  =  1  :  roomX  =  roomX  +  1  :  gosub genRoom

	LDA #150
	CMP player1x
     BCS .skipL0113
.condpart32
	LDA #1
	STA player1x
	INC roomX
 jsr .genRoom

.skipL0113
.L0114 ;  if player1y < 10 then player1y  =  90  :  roomY  =  roomY  +  1  :  gosub genRoom

	LDA player1y
	CMP #10
     BCS .skipL0114
.condpart33
	LDA #90
	STA player1y
	INC roomY
 jsr .genRoom

.skipL0114
.L0115 ;  if player1y > 90 then player1y  =  10  :  roomY  =  roomY  -  1  :  gosub genRoom

	LDA #90
	CMP player1y
     BCS .skipL0115
.condpart34
	LDA #10
	STA player1y
	DEC roomY
 jsr .genRoom

.skipL0115
.L0116 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.eatApple
 ; eatApple

.L0117 ;  C  =  111

	LDA #111
	STA C
.L0118 ;  player0x  =  0

	LDA #0
	STA player0x
.L0119 ;  player0y  =  0

	LDA #0
	STA player0y
.L0120 ;  gosub eatSoundEffect

 jsr .eatSoundEffect

.L0121 ;  if goldAppleX  =  roomX  &&  goldAppleY  =  roomY then score  =  score + 100  :  gosub randomizeApplePos  :  return 0

	LDA goldAppleX
	CMP roomX
     BNE .skipL0121
.condpart35
	LDA goldAppleY
	CMP roomY
     BNE .skip35then
.condpart36
	SED
	CLC
	LDA score+1
	ADC #$01
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
 jsr .randomizeApplePos
	LDY #0

	tya
	RTS
.skip35then
.skipL0121
.L0122 ;  if goldAppleX  =  roomX  ||  goldAppleY  =  roomY then score  =  score  +  2  :  return 0

	LDA goldAppleX
	CMP roomX
     BNE .skipL0122
.condpart37
 jmp .condpart38
.skipL0122
	LDA goldAppleY
	CMP roomY
     BNE .skip7OR
.condpart38
	SED
	CLC
	LDA score+2
	ADC #$02
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
	LDY #0

	tya
	RTS
.skip7OR
.L0123 ;  if S  <  1 then score  =  score  +  1  :  return 0

	LDA S
	CMP #1
     BCS .skipL0123
.condpart39
	SED
	CLC
	LDA score+2
	ADC #$01
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
	LDY #0

	tya
	RTS
.skipL0123
.
 ; 

.L0124 ;  if _sc3  >  0 then score  =  score  -  1

	LDA #0
	CMP _sc3
     BCS .skipL0124
.condpart40
	SED
	SEC
	LDA score+2
	SBC #$01
	STA score+2
	LDA score+1
	SBC #$00
	STA score+1
	LDA score
	SBC #$00
	STA score
	CLD
.skipL0124
.L0125 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.playerInput
 ; playerInput

.L0126 ;  D  =  1

	LDA #1
	STA D
.L0127 ;  if joy0fire then D  =  2

 bit INPT4
	BMI .skipL0127
.condpart41
	LDA #2
	STA D
.skipL0127
.L0128 ;  if joy0left then player1x  =  player1x - D  :  gosub changePlayer1Sprite

 bit SWCHA
	BVS .skipL0128
.condpart42
	LDA player1x
	SEC
	SBC D
	STA player1x
 jsr .changePlayer1Sprite

.skipL0128
.L0129 ;  if joy0right then player1x  =  player1x + D  :  gosub changePlayer1Sprite

 bit SWCHA
	BMI .skipL0129
.condpart43
	LDA player1x
	CLC
	ADC D
	STA player1x
 jsr .changePlayer1Sprite

.skipL0129
.L0130 ;  if joy0up then player1y  =  player1y - D  :  gosub changePlayer1Sprite

 lda #$10
 bit SWCHA
	BNE .skipL0130
.condpart44
	LDA player1y
	SEC
	SBC D
	STA player1y
 jsr .changePlayer1Sprite

.skipL0130
.L0131 ;  if joy0down then player1y  =  player1y + D  :  gosub changePlayer1Sprite

 lda #$20
 bit SWCHA
	BNE .skipL0131
.condpart45
	LDA player1y
	CLC
	ADC D
	STA player1y
 jsr .changePlayer1Sprite

.skipL0131
.
 ; 

.L0132 ;  S  =   ( roomX & 1 )   ^   ( roomY & 1 ) 

; complex statement detected
	LDA roomX
	AND #1
	PHA
	LDA roomY
	AND #1
	TSX
	INX
	TXS
	EOR $00,x
	STA S
.L0133 ;  if roomX  =  goldAppleX  ||  roomY  =  goldAppleY then Q  =  1 else Q  =  0

	LDA roomX
	CMP goldAppleX
     BNE .skipL0133
.condpart46
 jmp .condpart47
.skipL0133
	LDA roomY
	CMP goldAppleY
     BNE .skip8OR
.condpart47
	LDA #1
	STA Q
 jmp .skipelse1
.skip8OR
	LDA #0
	STA Q
.skipelse1
.
 ; 

.L0134 ;  if S = 1  &&  Q = 0 then gosub roomChange

	LDA S
	CMP #1
     BNE .skipL0134
.condpart48
	LDA Q
	CMP #0
     BNE .skip48then
.condpart49
 jsr .roomChange

.skip48then
.skipL0134
.L0135 ;  if player0x  =  0 then gosub roomChange

	LDA player0x
	CMP #0
     BNE .skipL0135
.condpart50
 jsr .roomChange

.skipL0135
.
 ; 

.L0136 ;  if player1x < 1 then player1x = 1

	LDA player1x
	CMP #1
     BCS .skipL0136
.condpart51
	LDA #1
	STA player1x
.skipL0136
.L0137 ;  if player1x > 155 then player1x = 155

	LDA #155
	CMP player1x
     BCS .skipL0137
.condpart52
	LDA #155
	STA player1x
.skipL0137
.L0138 ;  if player1y < 10 then player1y = 10

	LDA player1y
	CMP #10
     BCS .skipL0138
.condpart53
	LDA #10
	STA player1y
.skipL0138
.L0139 ;  if player1y > 90 then player1y = 90

	LDA #90
	CMP player1y
     BCS .skipL0139
.condpart54
	LDA #90
	STA player1y
.skipL0139
.
 ; 

.L0140 ;  if collision(player0,player1) then gosub eatApple

	bit 	CXPPMM
	BPL .skipL0140
.condpart55
 jsr .eatApple

.skipL0140
.
 ; 

.L0141 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.moveObject
 ; moveObject

.L0142 ;  if C  =  111 then return 0

	LDA C
	CMP #111
     BNE .skipL0142
.condpart56
	LDY #0

	tya
	RTS
.skipL0142
.
 ; 

.L0143 ;  AUDV1  =  1

	LDA #1
	STA AUDV1
.L0144 ;  if player0x%4  =  0 then AUDF1  =  20

	LDA player0x%4
	CMP #0
     BNE .skipL0144
.condpart57
	LDA #20
	STA AUDF1
.skipL0144
.L0145 ;  if player0y%4  =  0 then AUDF1  =  20

	LDA player0y%4
	CMP #0
     BNE .skipL0145
.condpart58
	LDA #20
	STA AUDF1
.skipL0145
.L0146 ;  if player0x%4  >  0 then AUDF1  =  30

	LDA #0
	CMP player0x%4
     BCS .skipL0146
.condpart59
	LDA #30
	STA AUDF1
.skipL0146
.L0147 ;  if player0y%4  >  0 then AUDF1  =  30

	LDA #0
	CMP player0y%4
     BCS .skipL0147
.condpart60
	LDA #30
	STA AUDF1
.skipL0147
.
 ; 

.L0148 ;  if C  =  0 then player0x  =  player0x - 1

	LDA C
	CMP #0
     BNE .skipL0148
.condpart61
	DEC player0x
.skipL0148
.L0149 ;  if C  =  1 then player0x  =  player0x + 1

	LDA C
	CMP #1
     BNE .skipL0149
.condpart62
	INC player0x
.skipL0149
.L0150 ;  if C  =  2 then player0y  =  player0y - 1

	LDA C
	CMP #2
     BNE .skipL0150
.condpart63
	DEC player0y
.skipL0150
.L0151 ;  if C  =  3 then player0y  =  player0y + 1

	LDA C
	CMP #3
     BNE .skipL0151
.condpart64
	INC player0y
.skipL0151
.
 ; 

.L0152 ;  if player0x  <  1 then player0x  =  150

	LDA player0x
	CMP #1
     BCS .skipL0152
.condpart65
	LDA #150
	STA player0x
.skipL0152
.L0153 ;  if player0x  >  150 then player0x  =  1

	LDA #150
	CMP player0x
     BCS .skipL0153
.condpart66
	LDA #1
	STA player0x
.skipL0153
.L0154 ;  if player0y  <  10 then player0y  =  90

	LDA player0y
	CMP #10
     BCS .skipL0154
.condpart67
	LDA #90
	STA player0y
.skipL0154
.L0155 ;  if player0y  >  90 then player0y  =  10

	LDA #90
	CMP player0y
     BCS .skipL0155
.condpart68
	LDA #10
	STA player0y
.skipL0155
.L0156 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.scoreColorChange
 ; scoreColorChange

.L0157 ;  if roomX  <  goldAppleX then REFP0  =  8

	LDA roomX
	CMP goldAppleX
     BCS .skipL0157
.condpart69
	LDA #8
	STA REFP0
.skipL0157
.L0158 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.advanceSound
 ; advanceSound

.L0159 ;  if audioDur  >  0 then audioDur  =  audioDur - 1

	LDA #0
	CMP audioDur
     BCS .skipL0159
.condpart70
	DEC audioDur
.skipL0159
.L0160 ;  if audioDur  <  50 then AUDV0  =  0

	LDA audioDur
	CMP #50
     BCS .skipL0160
.condpart71
	LDA #0
	STA AUDV0
.skipL0160
.L0161 ;  if audioDur2  >  0 then audioDur2  =  audioDur2 - 1  :  COLUBK  =  audioDur2  +  X  :  AUDF1  =  audioDur2  :  AUDC1  =  audioDur2%15

	LDA #0
	CMP audioDur2
     BCS .skipL0161
.condpart72
	DEC audioDur2
	LDA audioDur2
	CLC
	ADC X
	STA COLUBK
	LDA audioDur2
	STA AUDF1
	LDA audioDur2%15
	STA AUDC1
.skipL0161
.L0162 ;  if audioDur2  =  0 then AUDV1  =  0  :  COLUBK  =  T

	LDA audioDur2
	CMP #0
     BNE .skipL0162
.condpart73
	LDA #0
	STA AUDV1
	LDA T
	STA COLUBK
.skipL0162
.L0163 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.setTransColor
 ; setTransColor

.L0164 ;  if roomX  =  goldAppleX  &&  roomY  =  goldAppleY then X  =  $F0  :  return 0

	LDA roomX
	CMP goldAppleX
     BNE .skipL0164
.condpart74
	LDA roomY
	CMP goldAppleY
     BNE .skip74then
.condpart75
	LDA #$F0
	STA X
	LDY #0

	tya
	RTS
.skip74then
.skipL0164
.L0165 ;  if roomX  =  goldAppleX  ||  roomY  =  goldAppleY then X  =  $20  :  return 0

	LDA roomX
	CMP goldAppleX
     BNE .skipL0165
.condpart76
 jmp .condpart77
.skipL0165
	LDA roomY
	CMP goldAppleY
     BNE .skip11OR
.condpart77
	LDA #$20
	STA X
	LDY #0

	tya
	RTS
.skip11OR
.L0166 ;  if tempU  >  0 then X  =  $A0  :  return 0

	LDA #0
	CMP tempU
     BCS .skipL0166
.condpart78
	LDA #$A0
	STA X
	LDY #0

	tya
	RTS
.skipL0166
.L0167 ;  if S  =  1  &&  Q  =  0 then X  =  $50  :  return 0

	LDA S
	CMP #1
     BNE .skipL0167
.condpart79
	LDA Q
	CMP #0
     BNE .skip79then
.condpart80
	LDA #$50
	STA X
	LDY #0

	tya
	RTS
.skip79then
.skipL0167
.L0168 ;  X  =  $20

	LDA #$20
	STA X
.L0169 ;  return 0

	LDY #0

	tya
	RTS
.
 ; 

.main
 ; main

.L0170 ;  if S  =  1  &&  Q  =  0 then gosub moveObject

	LDA S
	CMP #1
     BNE .skipL0170
.condpart81
	LDA Q
	CMP #0
     BNE .skip81then
.condpart82
 jsr .moveObject

.skip81then
.skipL0170
.L0171 ;  gosub scoreColorChange

 jsr .scoreColorChange

.L0172 ;  gosub playerInput

 jsr .playerInput

.
 ; 

.L0173 ;  drawscreen

 jsr drawscreen
.L0174 ;  gosub advanceSound

 jsr .advanceSound

.
 ; 

.L0175 ;  goto main

 jmp .main

.
 ; 

.L0176 ;  asm

minikernel

	sta WSYNC

	lda R

	sta COLUBK

	rts

 ifconst pfres
 if (<*) > (254-pfres*pfwidth)
	align 256
	endif
 if (<*) < (136-pfres*pfwidth)
	repeat ((136-pfres*pfwidth)-(<*))
	.byte 0
	repend
	endif
 else
 if (<*) > 206
	align 256
	endif
 if (<*) < 88
	repeat (88-(<*))
	.byte 0
	repend
	endif
 endif
pfcolorlabel13
 .byte  $D2,0,0,0
 .byte  $D4,0,0,0
 .byte  $D2,0,0,0
 .byte  $D4,0,0,0
 .byte  $D2,0,0,0
 .byte  $D4,0,0,0
 .byte  $D2,0,0,0
 .byte  $D4,0,0,0
 .byte  $D2,0,0,0
 .byte  $D4,0,0,0
 .byte  $D2,0,0,0
 if (<*) > (<(*+11))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL042_0
	.byte 		$00
	.byte 		$00
	.byte 		$54
	.byte 		$53
	.byte 		$54
	.byte 		$53
	.byte 		$00
	.byte 		$00
	.byte 		$73
	.byte 		$74
	.byte 		$73
	.byte 		$74
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL043_0
	.byte 		%01101100
	.byte 		%01101100
	.byte 		%11111111
	.byte 		%01111110
	.byte 		%00111100
	.byte 		%00111100
	.byte 		%01111110
	.byte 		%11111111
	.byte 		%11111111
	.byte 		%01111110
	.byte 		%00111100
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
player3then_0
	.byte 		%00000000
	.byte 		%00110000
	.byte 		%01110000
	.byte 		%11111111
	.byte 		%11111111
	.byte 		%01110000
	.byte 		%00110000
	.byte 		%00000000
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
player6then_0
	.byte 		%00011000
	.byte 		%00111100
	.byte 		%01111110
	.byte 		%01111110
	.byte 		%00011000
	.byte 		%00011000
	.byte 		%00011000
	.byte 		%00011000
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
player8then_0
	.byte 		%00011000
	.byte 		%00011000
	.byte 		%00011000
	.byte 		%00011000
	.byte 		%01111110
	.byte 		%01111110
	.byte 		%00111100
	.byte 		%00011000
 if (<*) > (<(*+11))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL049_0
	.byte 		$74
	.byte 		$72
	.byte 		$84
	.byte 		$82
	.byte 		$74
	.byte 		$72
	.byte 		$84
	.byte 		$82
	.byte 		$74
	.byte 		$72
	.byte 		$84
	.byte 		$82
 if (<*) > (<(*+7))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL051_0
	.byte 		%00111100
	.byte 		%01111110
	.byte 		%11111111
	.byte 		%11111111
	.byte 		%01111110
	.byte 		%00111100
	.byte 		%00001000
	.byte 		%00011000
 if (<*) > (<(*+14))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL052_0
	.byte 		$32
	.byte 		$34
	.byte 		$36
	.byte 		$32
	.byte 		$34
	.byte 		$36
	.byte 		$32
	.byte 		$34
	.byte 		$36
	.byte 		$32
	.byte 		$34
	.byte 		$36
	.byte 		$32
	.byte 		$34
	.byte 		$36
 if (<*) > (<(*+14))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolor10then_0
	.byte 		$14
	.byte 		$16
	.byte 		$18
	.byte 		$14
	.byte 		$16
	.byte 		$18
	.byte 		$14
	.byte 		$16
	.byte 		$18
	.byte 		$14
	.byte 		$16
	.byte 		$17
	.byte 		$14
	.byte 		$16
	.byte 		$18
 if (<*) > (<(*+14))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolor14then_0
	.byte 		$24
	.byte 		$26
	.byte 		$28
	.byte 		$24
	.byte 		$26
	.byte 		$28
	.byte 		$24
	.byte 		$26
	.byte 		$28
	.byte 		$24
	.byte 		$26
	.byte 		$28
	.byte 		$24
	.byte 		$26
	.byte 		$28
 if (<*) > (<(*+11))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playercolorL062_1
	.byte 		$2C
	.byte 		$2C
	.byte 		$C4
	.byte 		$C3
	.byte 		$C4
	.byte 		$C3
	.byte 		$2C
	.byte 		$2C
	.byte 		$33
	.byte 		$34
	.byte 		$33
	.byte 		$34
 if (<*) > (<(*+10))
	repeat ($100-<*)
	.byte 0
	repend
	endif
playerL063_1
	.byte 		%00001100
	.byte 		%01101100
	.byte 		%11111111
	.byte 		%01111110
	.byte 		%00111100
	.byte 		%00111100
	.byte 		%01111110
	.byte 		%11111111
	.byte 		%11111111
	.byte 		%01111110
	.byte 		%00111100
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
