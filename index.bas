	set kernel_options playercolors player1colors pfcolors
	set optimization size
	set optimization speed

	dim audioDur2 = E
	dim audioDur = Z
	dim goldAppleX = W
	dim goldAppleY = V
	dim roomX = A
	dim roomY = B
        dim tempC = C
        dim tempD = D
        dim tempT = T
	dim tempU = U
	dim tempG = G
	dim arrowDir = P
	dim frame = F
        dim dayNight = Y
	dim _sc1 = score
	dim _sc2 = score+1
	dim _sc3 = score+2
	
	R = $30
	goldAppleX = 128
	goldAppleY = 128
	roomX = 127
	roomY = 127
	PF0 = %11110000
	scorecolor=$0E

	pfcolors:
		$D4
		$D2
		$D4
		$D2
		$D4
		$D2
		$D4
		$D2
		$D4
		$D2
		$D4
		$D2
end

	player1x = 70
	player1y = 60
	playfield:
	.XX...XXX..X.X..XXXX...X...XX...
	.X.X..X....X.X..X.....X.X..X.X..
	.X.X..XXX..X.X..X.XX..X.X..X.X..
	.X.X..X....X.X..X..X..X.X..X.X..
	.XXX..XXX...X...XXXX...X...XXX..
	................................
	..XXX...XXX....XXX...XXX...XXX..
	.XXXXX.XXXXX..XXXXX.XXXXX.XXXXX.
	.XXXXX.XXXXX..XXXXX.XXXXX.XXXXX.
	...X.....X......X.....X.....X...
	...X.....X......X.....X.....X...
	................................
end
	gosub setPlayerGraphics
	gosub scoreColorChange
titleScreen
	COLUBK = $D0
	if joy0fire then goto init
	player1x = player1x+1
	if player1x > 150 then player1x = 1 : player1y = (rand&70)+10
	if (player1x)&15 > 7 then REFP1 = 8
	drawscreen
	goto titleScreen

init
	gosub genRoom
	goto main

setEnemy
	player0color:
		$00
		$00
		$54
		$53
		$54
		$53
		$00
		$00
		$73
		$74
		$73
		$74
end

	player0:
		%01101100
		%01101100
		%11111111
		%01111110
		%00111100
		%00111100
		%01111110
		%11111111
		%11111111
		%01111110
		%00111100
end		
	return 0

setArrow
	if arrowDir = 0 then player0:
		%00000000
		%00110000
		%01110000
		%11111111
		%11111111
		%01110000
		%00110000
		%00000000
end
	if roomY < goldAppleY then Q = 0 else Q = 1
	if arrowDir = 1 && Q = 1 then player0:
		%00011000
		%00111100
		%01111110
		%01111110
		%00011000
		%00011000
		%00011000
		%00011000
end
	if arrowDir = 1 && Q = 0 then player0:
		%00011000
		%00011000
		%00011000
		%00011000
		%01111110
		%01111110
		%00111100
		%00011000
end
	player0color:
		$74
		$72
		$84
		$82
		$74
		$72
		$84
		$82
		$74
		$72
		$84
		$82
end	
	return 0

setApple
	player0:
		%00111100
		%01111110
		%11111111
		%11111111
		%01111110
		%00111100
		%00001000
		%00011000
end

	player0color:
		$32
		$34
		$36
		$32
		$34
		$36
		$32
		$34
		$36
		$32
		$34
		$36
		$32
		$34
		$36
end
	if goldAppleX = roomX && goldAppleY = roomY then player0color:
		$14
		$16
		$18
		$14
		$16
		$18
		$14
		$16
		$18
		$14
		$16
		$17
		$14
		$16
		$18
end

	if goldAppleX = roomX && goldAppleY = roomY then return 0
	if goldAppleX = roomX || goldAppleY = roomY then player0color:
		$24
		$26
		$28
		$24
		$26
		$28
		$24
		$26
		$28
		$24
		$26
		$28
		$24
		$26
		$28
end
	
	tempU = 0
	if goldAppleX = roomX || goldAppleY = roomY then return 0
	if ( (roomX&1) ^ (roomY&1) ) = 1 then gosub setEnemy : return 0
	tempU = rand&4
	if tempU > 0 then gosub setArrow
	return 0

setPlayerGraphics
	player1color:
		$2C
		$2C
		$C4
		$C3
		$C4
		$C3
		$2C
		$2C
		$33
		$34
		$33
		$34
end

	player1:
		%00001100
		%01101100
		%11111111
		%01111110
		%00111100
		%00111100
		%01111110
		%11111111
		%11111111
		%01111110
		%00111100
end
	return 0

randomizeApplePos
	tempG = rand&15
	tempG = tempG+1
	goldAppleX = goldAppleX-15 + tempG
	tempG = rand&15
	tempG = tempG+1
	goldAppleY = goldAppleY-15 + tempG
	return 0

changePlayer1Sprite
	if ( player1x + player1y)&15 > 7 then REFP1 = 8 : AUDF0 = 30
	if ( player1x + player1y)&15 <= 7 then AUDF0 = 31
	if audioDur < 10 then AUDV0=1 : AUDC0=12 : audioDur = 15
	return 0

forest
	playfield:
	..XXX...XXX...XXX....XXX...XXX..
	.XXXXX.XXXXX.XXXXX..XXXXX.XXXXX.
	.XXXXX.XXXXX.XXXXX..XXXXX.XXXXX.
	...X.....X.....X......X.....X...
	...X.....X.....X......X.....X...
	................................
	..XXX...XXX....XXX...XXX...XXX..
	.XXXXX.XXXXX..XXXXX.XXXXX.XXXXX.
	.XXXXX.XXXXX..XXXXX.XXXXX.XXXXX.
	...X.....X......X.....X.....X...
	...X.....X......X.....X.....X...
	................................
end
	return 0

flowers
	playfield:
	...............XXX..............
	..XXX...........X.........XXX...
	...X.......XXX.............X....
	............X...................
	....XXX..................XXX....
	.....X.........XXX........X.....
	................X...............
	.......XXX..........XXX.........
	.XXX....X............X..........
	..X........XXX............XXX...
	............X..............X....
	................................
end
	return 0

swamp
	playfield:
	....XX......XX......XX......XX..
	...XXXX....XXXX....XXXX....XXXX.
	....XX......XX......XX......XX..
	................................
	.......XX......XX......XX.......
	......XXXX....XXXX....XXXX......
	.......XX......XX......XX.......
	...........XX......XX...........
	..........XXXX....XXXX..........
	...XX.....XXXX....XXXX.....XX...
	..XXXX.....XX......XX.....XXXX..
	...XX......................XX...
end
	return 0

hill
	playfield:
	................................
	................................
	................................
	................................
	..............XXXX..............
	............XXX.XXXX............
	..........XXXXXXXX.XXX..........
	........XXX.XXXXXXXXXXXX........
	......XXXXXXXXX.XXXXX.XXXX......
	....XXX.XXXXXXXXXXX.XXXXXXXX....
	..XXXXXXX.XXXXXXXXXXXX.XXXXXXX..
	XXXXXXXXXXXX.XXXXXXXXXXXXX.XXXXX
end
	return 0

setRoomColor
	;if dayNight = 0 then T = $A0 : COLUPF = $20 : return 0
	if D = 0 then T = $C0
	if D = 1 then T = $D0
	if D = 2 then T = $E0
	if D = 3 then T = $F0
	COLUBK = T
	return 0

genRoom	
	rand = (roomX+1) ^ (roomY+2)
	C = ( rand )&3
	D = ( rand )&3
	
	player0x = (rand&120)+20
	player0y = (rand&60)+20
	
	S = (roomX&1) ^ (roomY&1)
	if S > 0 then player0x = 80 : player0y = 50
	if C = 0 then gosub forest
	if C = 1 then gosub swamp
	if C = 2 then gosub flowers
	if C = 3 then gosub hill
	
	gosub setRoomColor
	
	gosub setApple
	
	arrowDir = rand&1
	C = rand&3

	gosub setTransColor
	return 0

eatSoundEffect
	audioDur2 = 20
	AUDV1 = 2
	AUDC1 = 6
	AUDF1 = 20
	return 0

roomChange
	if player1x<1 then player1x = 150 : roomX = roomX - 1 : gosub genRoom
	if player1x>150 then player1x = 1 : roomX = roomX + 1 : gosub genRoom
	if player1y<10 then player1y = 90 : roomY = roomY + 1 : gosub genRoom
	if player1y>90 then player1y = 10 : roomY = roomY - 1 : gosub genRoom
	return 0

eatApple
	C = 111
	player0x = 0
	player0y = 0
	gosub eatSoundEffect
	if goldAppleX = roomX && goldAppleY = roomY then score = score+100 : gosub randomizeApplePos : return 0
	if goldAppleX = roomX || goldAppleY = roomY then score = score + 2 : return 0
	if S < 1 then score = score + 1 : return 0

	if _sc3 > 0 then score = score - 1
	return 0

playerInput
	D = 1
	if joy0fire then D = 2
	if joy0left then player1x = player1x-D : gosub changePlayer1Sprite
	if joy0right then player1x = player1x+D : gosub changePlayer1Sprite
	if joy0up then player1y = player1y-D : gosub changePlayer1Sprite
	if joy0down then player1y = player1y+D : gosub changePlayer1Sprite
	
	S = (roomX&1) ^ (roomY&1)
	if roomX = goldAppleX || roomY = goldAppleY then Q = 1 else Q = 0
	
	if S=1 && Q=0 then gosub roomChange
	if player0x = 0 then gosub roomChange
	
	if player1x<1 then player1x=1
	if player1x>155 then player1x=155
	if player1y<10 then player1y=10
	if player1y>90 then player1y=90

	if collision(player0,player1) then gosub eatApple	
	
	return 0

moveObject
	if C = 111 then return 0
	
	AUDV1 = 1
	if player0x%4 = 0 then AUDF1 = 20
	if player0y%4 = 0 then AUDF1 = 20
	if player0x%4 > 0 then AUDF1 = 30
	if player0y%4 > 0 then AUDF1 = 30
	
	if C = 0 then player0x = player0x-1
	if C = 1 then player0x = player0x+1
	if C = 2 then player0y = player0y-1
	if C = 3 then player0y = player0y+1

	if player0x < 1 then player0x = 150
	if player0x > 150 then player0x = 1
	if player0y < 10 then player0y = 90
	if player0y > 90 then player0y = 10
	return 0

scoreColorChange
	if roomX < goldAppleX then REFP0 = 8
	return 0

advanceSound
	if audioDur > 0 then audioDur = audioDur-1
	if audioDur < 50 then AUDV0 = 0
	if audioDur2 > 0 then audioDur2 = audioDur2-1 : COLUBK = audioDur2 + X : AUDF1 = audioDur2 : AUDC1 = audioDur2%15
	if audioDur2 = 0 then AUDV1 = 0 : COLUBK = T
	return 0

setTransColor
	if roomX = goldAppleX && roomY = goldAppleY then X = $F0 : return 0
	if roomX = goldAppleX || roomY = goldAppleY then X = $20 : return 0
	if tempU > 0 then X = $A0 : return 0
	if S = 1 && Q = 0 then X = $50 : return 0
	X = $20
	return 0

main	
	if S = 1 && Q = 0 then gosub moveObject
	gosub scoreColorChange
	gosub playerInput
	
	drawscreen
	gosub advanceSound 
	
	goto main

	asm
minikernel
	sta WSYNC
	lda R
	sta COLUBK
	rts
end	
