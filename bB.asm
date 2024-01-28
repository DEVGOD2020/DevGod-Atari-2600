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
 
 
 
