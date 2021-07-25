PROGRAM SAMPLE(OUTPUT,INDATA,OUTDATA);
{SPACE - Sample channel centerlines at equal spacing
Copyright (C) 2021 Alan D. Howard
Developer can be contacted by ahoward@psi.edu
This program is free software; you can redistribute it and/or modify it under the terms of the
 GNU General Public License as published by the Free Software Foundation; either version 3
 of the License, or (at your option) any later version. 
This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 See the GNU General Public License for more details. 
You should have received a copy of the GNU General Public License along with this program;
 if not, write to the Free Software Foundation, Inc.,
 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA. }

CONST
      INFINSMALL = 1.0E-09;
      PI = 3.141593;
      HALFPI = 1.570796;
      NINTYDEGREE = 1.5707963;
      ANGLE_CONVERT = 0.17453E-01;
      UNITY = 1.0;
      ZERO = 0.0;


TYPE
     DATAFORM = ARRAY[1..400] OF REAL;

VAR

    XIN, YIN : DATAFORM;
    N1, F1 : INTEGER;
    DOSAMPLE, IFLAG, EFLAG : BOOLEAN;
    INFILE, OUTDATA, INDATA  : TEXT;
    RESULT : INTEGER;
    X1 , SAMPLE_LENGTH , X3 , Y1 , Y2 , Y3 : REAL;
        FINISH : BOOLEAN;
    LEAD_IN : REAL;
    X, Y, X4, Y4, X5, Y5, X6, Y6, X7 : REAL;
    S1, D1, D2, R1 : REAL;
    N, N2 : INTEGER;

PROCEDURE FINDDISTANCE (VAR DISTANCE:REAL; X1, Y1, X2, Y2 :
                        REAL);

(*  FINDS THE DISTANCE BETWEEN TWO POINTS
*)

BEGIN

  DISTANCE := SQRT ((SQR(X2 - X1)) + (SQR(Y2 - Y1)));

END;  (* FINDDISTANCE *)


PROCEDURE READPARAMETERS;

BEGIN
  READ (INFILE, SAMPLE_LENGTH);
  READLN (INFILE);
  READ(INFILE,LEAD_IN);
  READLN(INFILE);
END;(*READPARAMETERS*)


PROCEDURE READNEXT(VAR X,Y : REAL; VAR FINISH : BOOLEAN);

BEGIN

  FINISH := FALSE;
REPEAT
  IF NOT EOF(INDATA) THEN
    BEGIN
      READLN (INDATA,X,Y);
      IF X>LEAD_IN THEN DOSAMPLE := TRUE;
    END
  ELSE
      FINISH := TRUE;
  IF EOF(INDATA) THEN FINISH := TRUE;
UNTIL FINISH OR DOSAMPLE;

END;

PROCEDURE WRITENEXT(X,Y : REAL);
BEGIN
        WRITELN(OUTDATA,X:15,' ',Y:15);
END;


PROCEDURE READMORE;

LABEL 1510, 1300, 1400, 1040, 1500;
VAR I : INTEGER;

BEGIN
    N1 := N1 + 1;
    IF N1 = F1 THEN GOTO 1500;
    IF N1 > 400 THEN GOTO 1300;
    IF N1 < 1 THEN GOTO 1300;
    1040 : X4 := XIN[N1];
    Y4 := YIN[N1];
    GOTO 1510;
    1300 : F1 := 402;
    I := 0;
    REPEAT
       I := I + 1;
       READNEXT(XIN[I],YIN[I],FINISH);
    UNTIL (I = 400) OR FINISH;
    IF FINISH THEN GOTO 1400;
    N1 := 1;
    GOTO 1040;
    1400 : F1 := I;
    N1 := 1;
    IF F1 = N1 THEN GOTO 1500;
    GOTO 1040;
    1500 : EFLAG := TRUE;
1510 : END;


PROCEDURE MAINBODY;

LABEL    140, 160, 250, 275, 310, 360, 390, 500, 630, 2000, 2010;

BEGIN
    READMORE;
    IF EFLAG THEN GOTO 250;
    X3 := X4;
    Y3 := Y4;
    X := X3;
    Y := Y3;
    WRITENEXT(X3,Y3);
    140 : READMORE;
    IF EFLAG THEN GOTO 250;
    X5 := X4;
    Y5 := Y4;
    IFLAG := FALSE;
    160 : FINDDISTANCE(D1,X,Y,X3,Y3);
    FINDDISTANCE(D2,X,Y,X5,Y5);
    IF D2 = D1 THEN GOTO 140;
    IF (D1 > SAMPLE_LENGTH) AND (D2 > SAMPLE_LENGTH) THEN GOTO 2000;
    IF D2 > SAMPLE_LENGTH THEN GOTO 275;
    IF D1 > SAMPLE_LENGTH THEN GOTO 500;
    X3 := X5;
    Y3 := Y5;
    N2 := N2+1;
    GOTO 140;
    250 : WRITELN('TOTAL POINTS=',N2,' SAMPLED POINTS=',N);
    GOTO 2010;
    275 : N := N;
    IF X5 = X3 THEN GOTO 390;
    N := N + 1;
    S1 := (Y5-Y3)/(X5-X3);
    310 : X7 := Y3 - X3 * S1;
    R1 := (SAMPLE_LENGTH - D1)/(D2-D1);
    X6 := R1*(X5-X3)+X3;
    Y6 := X7 + S1 * X6;
    360 : WRITENEXT(X6,Y6);
    X := X6;
    Y := Y6;
    X3 := X6;
    Y3 := Y6;
    IFLAG := TRUE;
    GOTO 160;
    390 : X6 := X3;
    N := N + 1;
    R1 := (SAMPLE_LENGTH-D1)/(D2-D1);
    Y6 := R1 * (Y5-Y3) + Y3;
    GOTO 360;
    500 : N := N;
    IF X5 = X3 THEN GOTO 630;
    N := N+1;
    S1 := (Y5-Y3)/(X5-X3);
    X7 := Y3 - X3*S1;
    R1 :=(SAMPLE_LENGTH-D2)/(D1-D2);
    X6 := R1*(X5-X3)+X3;
    Y6 := X7+S1*X6;
    WRITENEXT(X6,Y6);
    X := X6;
    Y := Y6;
    X3 := X6;
    Y3 := Y6;
    IFLAG := TRUE;
    GOTO 160;
    630: X6 := X3;
    N := N+1;
    R1 := (SAMPLE_LENGTH-D2)/(D1-D2);
    Y6 := R1*(Y5-Y3)+Y3;
    GOTO 360;
    2000 : WRITELN('TROUBLE IN PARADISE');
    WRITELN('X=',X:12,' Y=',Y:12,' X3=',X3:12,' Y3=',Y3:12);
    WRITELN('X4=',X4:12,' Y4=',Y4:12,' X5=',X5:12,' Y5=',Y5:12);
    WRITELN('D1=',D1:12,' D2=',D2:12);
    WRITELN('N=',N,' N1=',N1,' N2=',N2);
2010 : END;(*MAINBODY*)


BEGIN (*MAIN PROGRAM*)
  ASSIGN(INDATA,'SPACE.DAT');
  RESET(INDATA);
  READLN(INDATA);
  ASSIGN(INFILE,'SPACE.PRM');
  RESET(INFILE);
  ASSIGN(OUTDATA,'STAT.DAT');
  REWRITE(OUTDATA);
  WRITELN(OUTDATA);
  READPARAMETERS;
  DOSAMPLE := FALSE;
  N1 := -5;
  EFLAG := FALSE;
  F1 := 402;
  IFLAG := FALSE;
  N := 1;
  N2 := 1;
  MAINBODY;
  CLOSE(OUTDATA);
END.
