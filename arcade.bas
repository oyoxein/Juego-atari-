 set kernel multisprite
 
_ReiniciarNivel

 if n=0 then playfield:
 ................
 ................
 ................
 ................
 ................
 ........XXXXX...
 ................
 ................
 ................
 .....XXXXXXXXXXX
 ................
end

 if n=1 then playfield:
 ................
 ................
 ................
 ................
 ...........XXXXX
 ................
 ................
 XXXXXXXXXXXXXXX.
 ................
 ................
 ................
end

 
 COLUPF = 34
 
 x=60
 y=75
 
 player3x=160
 player3y=50
 
 i=1
 j=1
 
 dim sounda = s
 dim soundb = r
 
 sounda = 0
 soundb = 0
 
 pfheight=7
 
 scorecolor = 15

main 

 COLUP0=155
 COLUP2=190
 COLUP3=50
 
 COLUBK=0
 
 if f=0 || f=11 then player0:
 %11100111
 %00100100
 %00011000
 %01011010
 %00111100
 %00011000
 %00100100
 %00111100
end

 if f=1 then player0:
 %11000000
 %01100110
 %00100101
 %01011000
 %00111110
 %00011000
 %00100100
 %00111100
end

 player3:
 %11111111
 %11111111
 %11111111
 %11111111
 %11111111
 %11111111
 %11111111
 %11111111
end
 
 player2:
 %11111111
 %10000001
 %11111111
 %11111111
 %11111111
 %11111111
 %10000001
 %11111111
end

 
 player0x=x
 
 player0y=y
 
 drawscreen
 
 if t=0 then g=0
 
 if joy0right || joy0left then f=f+1 else f=0
 
 if joy0up && t=0 && u=0 then t=30
 if t>0 then t=t-1 : g=1 
 if !joy0right then goto _IgnorarDerecha
 temp5 = (y+1-11)/8
 
 temp6 = (x-9)/4
 
 if temp6 < 34 then if !pfread(temp6,temp5) then goto _IgnorarDerecha
 
 temp3 = (y+8-11)/8
 
 if temp6 < 34 then if !pfread(temp6,temp3) then goto _IgnorarDerecha
 
 d=1
 x=x+1
 REFP0=0
 
_IgnorarDerecha

 if !joy0left then goto _IgnorarIzquierda
 temp5 = (y+1-11)/8
 
 temp6 = (x-18)/4
 
 if temp6 < 34 then if !pfread(temp6,temp5) then goto _IgnorarIzquierda
 
 temp3 = (y+8-11)/8
 
 if temp6 < 34 then if !pfread(temp6,temp3) then goto _IgnorarIzquierda
 
 d=0
 x=x-1
 REFP0=8
 
_IgnorarIzquierda
 
 if !g=0 then goto _IgnorarArriba
 
 temp5 = (x-10)/4
 
 temp6 = (y+9-11)/8
 
 if temp5 < 34 then if !pfread(temp5,temp6) then t=0 : goto _IgnorarArriba
 
 temp4 = (x-17)/4
 
 if temp4 < 34 then if !pfread(temp4,temp6) then t=0 : goto _IgnorarArriba
 
 temp3 = temp5 - 1
 
 if temp3 < 34 then if !pfread(temp3,temp6) then t=0 : goto _IgnorarArriba
 
 y=y+1
 
_IgnorarArriba

 
 if !g=1 then u=1 : goto _IgnorarAbajo
 
 temp5 = (x-10)/4
 
 temp6 = (y-11)/8
 
 if temp5 < 34 then if !pfread(temp5,temp6) then goto _RevisarArriba
 
 temp4 = (x-17)/4
 
 if temp4 < 34 then if !pfread(temp4,temp6) then goto _RevisarArriba
 
 temp3 = temp5 - 1
 
 if temp3 < 34 then if !pfread(temp3,temp6) then goto _RevisarArriba
 
 y=y-1
 
 goto _IgnorarAbajo
 

_RevisarArriba

 u=0
 if joy0up then u=1

_IgnorarAbajo

 if joy0fire then p=p+1 else p=0

 if joy0fire && d=0 && p=1 then c=1 : missile0x=x : missile0y=y-5 : sounda = 5
 if joy0fire && d=1 && p=1 then c=2 : missile0x=x+9 : missile0y=y-5 : sounda = 5
 
 if sounda > 0 then sounda = sounda - 1 : AUDC0 = 2 : AUDV0 = 4 : AUDF0 = sounda else AUDV0 = 0
 
 if c=1 then missile0x = missile0x-1
 if c=2 then missile0x = missile0x+1
 
 if missile0x<0 then c=0
 if missile0x>160 then c=0
 
 if c=0 then missile0x=0 : missile0y=0
 
 if i=1 then player3x=player3x-1
 if i=1 && player3x=0 then player3x=160 : player3y = (rand&30)+20
 
 
 if missile0x>player3x-7 && missile0x<player3x+1 && missile0y>player3y-7 && missile0y<player3y+1 then i=0 : score=score+10 : soundb = 50
 if i=0 then player3x=0 : player3y=0
 
 if j=1 then player2x=player2x+1
 if j=1 && player2x>160 then player2x=0 : player2y = (rand&30)+20
 
 if missile0x>player2x-7 && missile0x<player2x+1 && missile0y>player2y-7 && missile0y<player2y+1 then j=0 : score=score+10 : soundb = 50
 if j=0 then player2x=0 : player2y=0
 
 if soundb > 0 then soundb = soundb - 1 : AUDC1 = 8 : AUDV1 = 4 : AUDF1 = soundb else AUDV1 = 0


 if x=0 then x=1
 if x>160-7 then x=160-7
  
 if y<9 then goto _ReiniciarNivel
 
 if f>21 then f=1
 
 if i=0 && j=0 then n=1 : goto _ReiniciarNivel
 
 if f>21 then f=1
 
 goto main
  
 inline pfread_msk.asm
  
 