C    
C ***********************************************************************
C
C  DW-NOMINATE.FOR----DYNAMIC-WEIGHTED NOMINATE -- THIS PROGRAM IMPLEMENTS
C                     THE ROLL CALL AND LEGISLATOR PHASES FOR THE SENATE
C
C                        1 OCTOBER 2004                 
C                 
C ***********************************************************************
C

C     with minor changes by William May for use with R
      
      module xxcom_mod
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C     WEIGHT(99),XBIGLOG(54001,2),NUMCONG(200),
     C     MCONG(200,3),ICONG(99001),NUMCONGT(200),
     C     NCONG(54001),KWHERE(99999,200),KBIGLOG(54001,4),
     C     RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C     RCVOTET1(99001,660),RCVOTET9(99001,660),
     C     RCBAD(99001),LWHERE(99999,200)
      save
      end module xxcom_mod

      module mine_mod
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
      save
      end module mine_mod
      
      SUBROUTINE dwnom(NOMSTARTIN, WEIGHTSIN, NBILLS, ICONGIN, DYNIN,
     $     ZMIDIN, MCONGIN, NROWRCT, NCOLRCT, RCVOTET1IN, RCVOTET9IN,
     $     NLEGS, NCONGIN, ID1IN, XDATAIN, NROWRC, NCOLRC, RCVOTE1IN,
     $     RCVOTE9IN, XDATAOUT, SDX1OUT, SDX2OUT, VARX1OUT, VARX2OUT,
     $     XBIGLOGOUT, KBIGLOGOUT, GMPAOUT, GMPBOUT, DYNOUT, ZMIDOUT,
     $     WEIGHTSOUT)
      use xxcom_mod
      use mine_mod
      INTEGER NOMSTARTIN(6), NBILLS, NROWRCT, NCOLRCT, NLEGS,
     C     NROWRC, NCOLRC
      INTEGER ICONGIN(NBILLS),
     C     MCONGIN(NOMSTARTIN(4) - NOMSTARTIN(3) + 1, 3),
     C     RCVOTET1IN(NROWRCT, NCOLRCT),
     C     RCVOTET9IN(NROWRCT, NCOLRCT),
     C     NCONGIN(NLEGS), ID1IN(NLEGS),
     C     RCVOTE1IN(NROWRC, NCOLRC),
     C     RCVOTE9IN(NROWRC, NCOLRC),
     C     KBIGLOGOUT(NLEGS, 4)
      DOUBLE PRECISION WEIGHTSIN(NOMSTARTIN(1) + 1),
     C     DYNIN(NBILLS, NOMSTARTIN(1)), ZMIDIN(NBILLS, NOMSTARTIN(1)),
     C     XDATAIN(NLEGS, NOMSTARTIN(1)),
     C     XDATAOUT(NLEGS, NOMSTARTIN(1)), SDX1OUT(NLEGS),
     C     SDX2OUT(NLEGS), VARX1OUT(NLEGS), VARX2OUT(NLEGS),
     C     XBIGLOGOUT(NLEGS, 2), GMPAOUT(NLEGS), GMPBOUT(NLEGS),
     C     DYNOUT(NBILLS, NOMSTARTIN(1)),
     C     ZMIDOUT(NBILLS, NOMSTARTIN(1)), WEIGHTSOUT(NOMSTARTIN(1) + 1)
      dimension ID1(54001),LVOTE(3600),YY(150000),
     C          CUMNML(150000),ZDF(150000,4),WDERV(99),
     C          XBETA(5,5),OUTX0(99,99),DERVISH(99,99),
     C          OUTX1(99,99),OUTX2(99,99),OUTX3(99,99),
     C          OLDZ(99),OLDD(99),DDERVX(99),ZDERVX(99),
     C          XMAT(1001,25),ZVEC(2901,25),WS(5802),
     C          MCUTS(2901,2),LERROR(1001,2901),
     C          LDATA(1001,2901),LL(2901),YSS(2901),
     C          KA(2901),
     C          XVAR(99999,10)
      CHARACTER*255 FTITLE,outmsg
C
  100 FORMAT(2X,I3,2I2,I4,I5,2I1,11A1,3600I1)
C
  101 format(I3,1x,i5,1x,i5,1x,i2,1x,i2,1x,7a1,1x,i4,1x,i1,1x,i1,1x,
     c       11a1,4f7.3)
  102 FORMAT(A64)
  103 FORMAT(10I5)
  104 FORMAT(15F8.4)
  150 FORMAT(6X,I5,41X,2F7.3)
  175 FORMAT(I3,I5,4F7.3)
  180 FORMAT(I3,1X,2I4)
  190 FORMAT(I4,I6,I3,I4,1X,11A1,2F7.3)
  200 FORMAT(I4,I6,I3,I2,I4,3600I1)
  201 FORMAT(I4,I6,I3,I2,I4,2F7.3,I5)
  202 FORMAT(I9,4F15.3)
  203 FORMAT(I4,I5,4I4,2I2,30F7.3)
  210 FORMAT(I3,I8,I6,I3,I2,1X,7A1,1X,I4,1X,11A1,40F7.3)
  211 FORMAT(I4,I6,I3,I2,I4,6F7.3,2F12.5,4I5,2F7.3)
  212 FORMAT(I4,I6,I3,I2,I4,F7.3,2F12.5,4I5,2F7.3)
  213 FORMAT(54X,40F7.3)
  214 FORMAT(10X,10F10.6)
  240 FORMAT(I4,I5,1X,600I1)
  250 FORMAT(I4,I5,15I4)
  251 FORMAT(I4,I5,3I4,5F7.3/17X,I4,5F7.3)
  252 FORMAT(17X,5I4,5F7.3)
  253 FORMAT(I4,I5,7I4,5F7.3)
  300 FORMAT(' TOTAL ROLL CALLS      ',3I7)
  301 FORMAT(' TOTAL LEGISLATORS     ',3I7)
  302 FORMAT(' NUMBER OF CONGRESSES  ',I7)
  303 FORMAT(4I4,6F7.3)
  304 FORMAT(' FATAL MISMATCH',I4,4I6)
  305 FORMAT(' MISMATCH ON ROLL CALL NUMBERS',3I5)
  306 FORMAT(' MISMATCH ON MISSING DATA',3I5)
  307 FORMAT(' MISMATCH ON NON-MISSING DATA',3I5)
  308 FORMAT(' UNIQUE LEGISLATORS',4I6,I9/19X,4F15.3)
  312 FORMAT(I4,I5,5I4,4F7.3)
  313 FORMAT(' NUMBER RCs & CLASSIFICATION',6I9/
     C          30X,2F8.4)
  314 FORMAT(' NUMBER READ ROLL CALL FILE',3I6)
  315 FORMAT(' NUMBER RECORDS WRITTEN',4I8)
  316 FORMAT(' FATAL MISMATCH ON LEGISLATORS',3I7)
  317 FORMAT(' MISMATCH ON STATE CODES',3I7)
  318 FORMAT(' MISMATCH ON CONGRESSIONAL DISTRICT CODES',3I7)
  319 FORMAT(' MISMATCH ON PARTY CODES',3I7)
 320  FORMAT(' Estimating ', A, '...')
 321  FORMAT('')
 1000 format(22x,4(i2.2,'.'))
C
C  READ TITLE OF RUN
C
      FTITLE = 'NOMINAL DYNAMIC-WEIGHTED MULTIDIMENSIONAL UNFOLDING '
      NS = NOMSTARTIN(1)
      NMODEL = NOMSTARTIN(2)
      NFIRST = NOMSTARTIN(3)
      NLAST = NOMSTARTIN(4)
      IHAPPY1 = NOMSTARTIN(5)
      IHAPPY2 = NOMSTARTIN(6)
      call intpr(FTITLE, 52, (/NS,NMODEL,NFIRST,NLAST,IHAPPY1,IHAPPY2/),
     C     6)
      WEIGHT(1:(NS+1)) = WEIGHTSIN
      call realpr('', 0, (/WEIGHT(NS+1),(WEIGHT(K),K=2,NS)/), NS)
C
C      NS=2
C      NMODEL=0
C      NFIRST=1
C      NLAST=57
C
C  DIMENSION WEIGHTS
C
      WEIGHT(1)=1.000
C      WEIGHT(2)=1.000
C      WEIGHT(2)=.50
C
C  SIGMA-SQUARED (BETA)
C  
C      WEIGHT(NS+1)=4.925
C
      DO 46 J=1,111
      DO 47 I=1,99999
      LWHERE(I,J)=.FALSE.
  47  CONTINUE
  46  CONTINUE
C
C
C  READ IN CUMULATIVE NORMAL DISTRIBUTION FUNCTION
C
C
      ndevit=50001
      XDEVIT=10000.0
C
      DO 501 I=1,NDEVIT
      YY(I)=FLOAT(I-1)/XDEVIT
      X=(FLOAT(I-1)/XDEVIT)/SQRT(2.0)
      XX=ERF(X)
      XX=XX/2.0 +.5
      CUMNML(I)=XX
 501  CONTINUE
C
      TWOPI=1.0/SQRT(2.0*3.1415926536)
      DO 901 I=1,NDEVIT
      ZDF(I,1)=YY(NDEVIT+1-I)*(-1.0)
      ZDF(I,2)=1.0-CUMNML(NDEVIT+1-I)
      ZDF(I,3)=ALOG(ZDF(I,2))
 901  CONTINUE
      DO 902 I=2,NDEVIT
      ZDF(I-1+NDEVIT,1)=YY(I)
      ZDF(I-1+NDEVIT,2)=CUMNML(I)
      ZDF(I-1+NDEVIT,3)=ALOG(ZDF(I-1+NDEVIT,2))
 902  CONTINUE
      DO 903 I=1,2*NDEVIT-1
      ZDF(I,4)=(TWOPI*EXP((-ZDF(I,1)**2)/2.0))/ZDF(I,2)
 903  CONTINUE    
C
      DO 10 I=1,200
      NUMCONG(I)=0
      NUMCONGT(I)=0
  10  CONTINUE
C
C  READ NHOUSE.NUM -- GIVES NUMBER OF ROLL CALLS AND LEGISLATORS FOR 
C                     EACH CONGRESS
      I = NOMSTARTIN(4) - NOMSTARTIN(3) + 1
      MCONG(1:size(MCONGIN, 1), 1:3) = MCONGIN
 425  call intpr('NUMBER OF CONGRESSES', -1, I, 1)
C
C  READ ROLL CALL STARTS -- HC01108.DAT
C
      ICONG(1:size(ICONGIN)) = ICONGIN
      DYN(1:size(DYNIN, 1), 1:size(DYNIN, 2)) = DYNIN
      ZMID(1:size(ZMIDIN, 1), 1:size(ZMIDIN, 2)) = ZMIDIN
      I=0
      DO 575 I0=1,NBILLS
         NP=MCONG(ICONG(I+1),3)
         I=I+1
         KYES=0
         KNO=0
         DO 11 JJ=1,NP
            RCVOTET1(I,JJ)=.FALSE.
            RCVOTET9(I,JJ)=.FALSE.
            IF(RCVOTET1IN(I,JJ).EQ.1)THEN
               RCVOTET1(I,JJ)=.TRUE.
               KYES=KYES+1
            ENDIF
            IF(RCVOTET9IN(I,JJ).EQ.1)THEN
               RCVOTET9(I,JJ)=.TRUE.
            ENDIF
            IF(RCVOTET1IN(I,JJ).EQ.0.AND.RCVOTET9IN(I,JJ).EQ.0)THEN
               KNO=KNO+1
            ENDIF
 11      CONTINUE
         RCBAD(I)=.FALSE.
         KRCTOT=KYES+KNO
         KRCMIN=MIN0(KYES,KNO)
         XMARG=0.0
         IF(KRCTOT.GT.0)THEN
            XMARG=FLOAT(KRCMIN)/FLOAT(KRCTOT)
            IF(XMARG.GE..025)THEN
               RCBAD(I)=.TRUE.
            ENDIF
         ENDIF
         NUMCONGT(ICONG(I))=NUMCONGT(ICONG(I))+1
 575  CONTINUE
 475  call intpr('TOTAL ROLL CALLS', -1, I, 1)
      NQTOT=I
C
C  READ PSEUDO-DYNAMIC STARTS FROM BLACK BOX -- HL01108.SRT
C
      NCONG(1:size(NCONGIN)) = NCONGIN
      ID1(1:size(ID1IN)) = ID1IN
      XDATA(1:size(XDATAIN, 1), 1:size(XDATAIN, 2)) = XDATAIN
      I=0
      DO 550 I0=1,NLEGS
C
C  USE KHIT SWITCH ONLY ON VERY FIRST RUN WITH BRAND-NEW STARTING
C     COORDINATES
C
C      IF(KHIT.LT.25)GO TO 550
      NQ=MCONG(NCONG(I+1),2)
      I=I+1
C
      LWHERE(ID1(I),NCONG(I))=.TRUE.
      KWHERE(ID1(I),NCONG(I))=I
      DO 1 JJ=1,NQ
      RCVOTE1(I,JJ)=.FALSE.
      RCVOTE9(I,JJ)=.FALSE.
      IF(RCVOTE1IN(I,JJ).EQ.1)THEN
         RCVOTE1(I,JJ)=.TRUE.
      ENDIF
      IF(RCVOTE9IN(I,JJ).EQ.1)THEN
         RCVOTE9(I,JJ)=.TRUE.
      ENDIF
  1   CONTINUE
      NUMCONG(NCONG(I))=NUMCONG(NCONG(I))+1
 550  CONTINUE
 450  call intpr('TOTAL LEGISLATORS', -1, I, 1)
      call intpr(' ', 1, 0, 0)
      NPTOT=I
C
      DO 9999 IHAPPY=IHAPPY1,IHAPPY2
C
C  ************************************
C     DIMENSION WEIGHT PHASE
C  ************************************
      IF(NS.GE.2)THEN
         call intpr('Estimating dimension weights...', -1, 0, 0)
         CALL WINT(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      ENDIF
C
C  ************************************
C     BETA (1/SIGMA) PHASE
C  ************************************
      call intpr('Estimating beta...', -1, 0, 0)
      CALL SIGMAS(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
C
C  ************************************
C     ROLL CALL PHASE
C  ************************************
C    
C
      call intpr('Estimating roll call vectors...', -1, 0, 0)
      KTOTP=0
      KTOTQ=0
      KK=0
      LASSB4=0
      LASSAF=0
      LATOT=0
      LPRE=0
      LSCALE=0
      IF(NFIRST.GT.1)THEN
         DO 1111 II=1,NFIRST-1
         NPC=NUMCONG(II)
         NQC=NUMCONGT(II)
         KTOTP=KTOTP+NPC
         KTOTQ=KTOTQ+NQC
 1111    CONTINUE
      ENDIF
      DO 2 II=NFIRST,NLAST
      IICONG=II
      NPC=NUMCONG(II)
      NQC=NUMCONGT(II)
      NQ=MCONG(II,2)
      IF(NQ.NE.NQC)THEN
         WRITE(outmsg,305)II,NQ,NQC
         call rexit(outmsg)
      ENDIF
      DO 3 J=1,NQ
      NEQ=J
      KYES=0
      KNO=0
      KMISS=0
      DO 4 I=1,NPC
      LL(I)=I
      LDATA(I,1)=0
      YSS(I)=XMAT(I,1)
      DO 4444 K=1,NS
      XMAT(I,K)=XDATA(I+KTOTP,K)
 4444 CONTINUE
      IF(RCVOTE9(I+KTOTP,J).NEQV.RCVOTET9(J+KTOTQ,I))THEN
         WRITE(outmsg,306)II,J,I
         call rexit(outmsg)
      ENDIF
      IF(RCVOTE1(I+KTOTP,J).NEQV.RCVOTET1(J+KTOTQ,I))THEN
         WRITE(outmsg,307)II,J,I
         call rexit(outmsg)
      ENDIF
      IF(RCVOTE9(I+KTOTP,J).EQV..TRUE.)KMISS=KMISS+1
C
C  IF NOT MISSING DATA
C
      IF(RCVOTE9(I+KTOTP,J).EQV..FALSE.)THEN
C
C  IF YES
C
         IF(RCVOTE1(I+KTOTP,J).EQV..TRUE.)THEN
            KYES=KYES+1
            LDATA(I,1)=1
         ENDIF
C
C  IF NO
C
         IF(RCVOTE1(I+KTOTP,J).EQV..FALSE.)THEN
            KNO=KNO+1
            LDATA(I,1)=6
         ENDIF
      ENDIF
  4   CONTINUE
      CALL RSORT(YSS,NPC,LL)
      DO 44 I=1,NPC
      KA(I)=LDATA(LL(I),1)
  44  CONTINUE
      KK=KK+1
C      RCBAD(KK)=.FALSE.
      KRCTOT=KYES+KNO
      KRCMIN=MIN0(KYES,KNO)
      IF(KRCTOT.GT.0)THEN
         XMARG=FLOAT(KRCMIN)/FLOAT(KRCTOT)
         IF(XMARG.LT..025)THEN
            DO 625 K=1,NS
            ZMID(NEQ+KTOTQ,K)=0.0
            DYN(NEQ+KTOTQ,K)=0.0
  625       CONTINUE
         ENDIF
         IF(XMARG.GE..025)THEN
C            RCBAD(KK)=.TRUE.
            LPRE=LPRE+KRCMIN
            LSCALE=LSCALE+1
C
C  DO CHECK ON MIDPOINT TO MAKE SURE ITS WITHIN THE UNIT HYPERSPHERE
C
            SUM=0.0
            DO 23 K=1,NS
            SUM=SUM+ZMID(NEQ+KTOTQ,K)**2
  23        CONTINUE
            IF(SUM.GT.1.0)THEN
               DO 24 K=1,NS
               ZMID(NEQ+KTOTQ,K)=ZMID(NEQ+KTOTQ,K)/SQRT(SUM)
  24           CONTINUE
            ENDIF
            DO 21 K=1,NS
            IF(ABS(OLDD(K)).GT.2.0)THEN
               OLDD(K)=(OLDD(K))/ABS(OLDD(K))
            ENDIF
            OLDZ(K+NS)=ZMID(NEQ+KTOTQ,K)
            OLDD(K+NS)=DYN(NEQ+KTOTQ,K)
            OLDZ(K)=ZMID(NEQ+KTOTQ,K)
            OLDD(K)=DYN(NEQ+KTOTQ,K)
  21        CONTINUE
C
C  GET INITIAL CLASSIFICATION AND LOG-LIKELIHOOD
C
C
            IF(NS.EQ.1)THEN
              IVOT=1
              KCCUT=1
              LCCUT=6
              CALL JAN11PT(NPC,IVOT,YSS,KA,WS,JCH,JEH,JCL,JEL,KCCUT,
     C              LCCUT)
C
              XPCT1=(FLOAT(JCH+JCL)/FLOAT(JCH+JEH+JCL+JEL))*100.0
C              WRITE(*,312)IICONG,NEQ,NPC,JCH,JEH,JEL,JCL,WS(1),XPCT1
C
              IVOT=2
              KCCUT=6
              LCCUT=1
              CALL JAN11PT(NPC,IVOT,YSS,KA,WS,JCH,JEH,JCL,JEL,KCCUT,
     C              LCCUT)
C
              XPCT2=(FLOAT(JCH+JCL)/FLOAT(JCH+JEH+JCL+JEL))*100.0
C              WRITE(*,312)IICONG,NEQ,NPC,JCH,JEH,JEL,JCL,WS(2),XPCT2
              IF(XPCT1.GE.XPCT2)THEN
                  MCUTS(1,1)=1
                  MCUTS(1,2)=6
                  IF(IHAPPY.EQ.1)THEN
                     OLDD(1)=.5
                     IF(ABS(WS(1)).GT.1.0)THEN
                        AA=ABS(WS(1))
                        WS(1)=WS(1)/AA
                     ENDIF
                     OLDZ(1)=WS(1)
                  ENDIF
              ENDIF
              IF(XPCT1.LT.XPCT2)THEN
                  MCUTS(1,1)=6
                  MCUTS(1,2)=1
                  IF(IHAPPY.EQ.1)THEN
                     OLDD(1)=-.5
                     IF(ABS(WS(2)).GT.1.0)THEN
                        AA=ABS(WS(2))
                        WS(2)=WS(2)/AA
                     ENDIF
                     OLDZ(1)=WS(2)
                  ENDIF
              ENDIF
            ENDIF
C
C       CALL CUTTING PLANE ROUTINE TO GET PROPER POLARITIES FOR
C          STARTS IN TWO OR MORE DIMENSIONS
C
            IF(NS.GT.1)THEN
               IFIXX=1
               NRCALL=1
               ZVEC(1,1)=1.0
               DO 4445 K=2,NS
               ZVEC(1,K)=0.0
 4445          CONTINUE
               CALL CUTPLANE(NEQ,NPC,NRCALL,NS,XMAT,ZVEC,WS,
     C                MCUTS,LERROR,IFIXX,KTT,KT,LDATA)
C
C  GET MIDPOINT AND SPREADS FROM CUTPLANE IF FIRST GLOBAL ITERATION
C
               IF(IHAPPY.EQ.1)THEN
                  IF(ZVEC(1,1).LT.0.0)THEN
                     DO 4447 K=1,NS
                     ZVEC(1,K)=-ZVEC(1,K)
 4447                CONTINUE
                     WS(1)=-WS(1)
                     KCUTTT=MCUTS(1,1)
                     MCUTS(1,1)=MCUTS(1,2)
                     MCUTS(1,2)=KCUTTT
                  ENDIF
                  SUM=0.0
                  DO 4446 K=1,NS
                  OLDZ(K)=WS(1)*ZVEC(1,K)
                  SUM=SUM+OLDZ(K)**2
                  IF(MCUTS(1,1).EQ.1)THEN
                     OLDD(K)= .5*ZVEC(1,K)
                  ENDIF
                  IF(MCUTS(1,2).EQ.1)THEN
                     OLDD(K)=-.5*ZVEC(1,K)
                  ENDIF
 4446             CONTINUE
                  IF(SUM.GT.1.0)THEN
                     DO 4448 K=1,NS
                     OLDZ(K)=OLDZ(K)/SQRT(SUM)
 4448                CONTINUE
                  ENDIF
               ENDIF
            ENDIF
C
C       GET INITIAL LOG-LIKELIHOOD
C
            CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
            GMPB4=EXP(XPLOG/FLOAT(KRC))
C
            KLASSOLD=KLASS
            KLASSOYY=KLASSYY
            KLASSONY=KLASSNY
            KLASSOYN=KLASSYN
            KLASSONN=KLASSNN
            CALL RCINT2(IICONG,NEQ,NPC,NQC,KRC,KTOTP,KTOTQ,
     C           XPLOG,OLDZ,OLDD,
     C           ZDF,NDEVIT,XDEVIT)
            GMPAF=EXP(XPLOG/FLOAT(KRC))
            DO 22 K=1,NS
            ZMID(NEQ+KTOTQ,K)=OLDZ(K)
            DYN(NEQ+KTOTQ,K)=OLDD(K)
  22        CONTINUE
C
            LASSB4=LASSB4+KLASSOLD
            LASSAF=LASSAF+KLASS
            LATOT=LATOT+KYES+KNO
         ENDIF
      ENDIF
  3   CONTINUE
      KTOTP=KTOTP+NPC
      KTOTQ=KTOTQ+NQC
  2   CONTINUE
      YCLASS=FLOAT(LASSAF)/FLOAT(LATOT)
      YPRE=FLOAT(LPRE-LATOT+LASSAF)/FLOAT(LPRE)
c$$$      WRITE(*,313)KK,LSCALE,LATOT,LASSB4,LASSAF,LPRE,YCLASS,YPRE
      DO 41 I=1,NQTOT
      DO 444 J=1,NS
         DYNOUT(I,J) = DYN(I,J)
         ZMIDOUT(I,J) = ZMID(I,J)
 444  CONTINUE
 41   CONTINUE
C
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,
     C                  ZDF,NDEVIT,XDEVIT)
C
C
C  ******************************************
C   LEGISLATOR PHASE
C  ******************************************
      call intpr('Estimating legislator coordinates...', -1, 0, 0)
      NPUNIQUE=0
      NPUNIQUF=0
      NPUNIQUG=0
      NPUNIQUH=0
      NXTOT=0
      XTOTLOG0=0.0
      XTOTLOG1=0.0
      XTOTLOG2=0.0
      XTOTLOG3=0.0
      DO 48 I=1,99999
      NEP=I
      KK=0
      KLOCATE=0
      DO 49 J=NFIRST,NLAST
      IF(LWHERE(I,J).EQV..TRUE.)THEN
         KK=KK+1
         KLOCATE=KWHERE(I,J)
      ENDIF
  49  CONTINUE
      IF(KK.EQ.0)GO TO 48
      NPUNIQUE=NPUNIQUE+1
      IF(KK.GE.5)NPUNIQUF=NPUNIQUF+1
      IF(KK.GE.6)NPUNIQUG=NPUNIQUG+1
      IF(KK.GE.7)NPUNIQUH=NPUNIQUH+1
      CALL XINT(NEP,ID1,XPLOG0,XPLOG1,XPLOG2,XPLOG3,
     C           NFIRST,NLAST,NMODEL,
     C           ZDF,NDEVIT,XDEVIT,KXTOT,XBETA,
     C           OUTX0,OUTX1,OUTX2,OUTX3,DERVISH)
      NXTOT=NXTOT+KXTOT
      XTOTLOG0=XTOTLOG0+XPLOG0
      XTOTLOG1=XTOTLOG1+XPLOG1
      XTOTLOG2=XTOTLOG2+XPLOG2
      XTOTLOG3=XTOTLOG3+XPLOG3
C
      IF(NMODEL.EQ.0)THEN
C
C  STORE NEEDED VARIANCES AND COVARIANCES
C
         XVAR(I,1)=OUTX0(1,1)
         XVAR(I,2)=0.0
         XVAR(I,3)=0.0
         XVAR(I,4)=OUTX0(2,2)
         XVAR(I,5)=0.0
         XVAR(I,6)=0.0
      ENDIF
      IF(NMODEL.EQ.1)THEN
         IF(KK.LT.5)THEN
C
C  STORE NEEDED VARIANCES AND COVARIANCES
C
            XVAR(I,1)=OUTX0(1,1)
            XVAR(I,2)=0.0
            XVAR(I,3)=0.0
            XVAR(I,4)=OUTX0(2,2)
            XVAR(I,5)=0.0
            XVAR(I,6)=0.0
         ENDIF
         IF(KK.GE.5)THEN
C
C  STORE NEEDED VARIANCES AND COVARIANCES
C
            XVAR(I,1)=OUTX1(1,1)
            XVAR(I,2)=OUTX1(3,3)
            XVAR(I,3)=OUTX1(1,3)
            XVAR(I,4)=OUTX1(2,2)
            XVAR(I,5)=OUTX1(4,4)
            XVAR(I,6)=OUTX1(2,4)
         ENDIF
      ENDIF
  48  CONTINUE
c$$$      WRITE( *,308)NPUNIQUE,NPUNIQUF,NPUNIQUG,NPUNIQUH,NXTOT,
c$$$     C                           XTOTLOG0,XTOTLOG1,XTOTLOG2,XTOTLOG3
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,
     C                  ZDF,NDEVIT,XDEVIT)
C
      SUMLOG1=0.0
      SUMLOG2=0.0
      DO 61 I=1,NPTOT
      GMPA=EXP(XBIGLOG(I,1)/FLOAT(KBIGLOG(I,1)))
      GMPB=EXP(XBIGLOG(I,2)/FLOAT(KBIGLOG(I,2)))
      SUMLOG1=SUMLOG1+XBIGLOG(I,1)
      SUMLOG2=SUMLOG2+XBIGLOG(I,2)
      IF(NS.EQ.1)THEN
         DO 445 J=1,2
            XBIGLOGOUT(I, J) = XBIGLOG(I, J)
 445     CONTINUE
         DO 446 J=1,4
            KBIGLOGOUT(I, J) = KBIGLOG(I, J)
 446     CONTINUE
         XDATAOUT(I, 1) = XDATA(I, 1)
         GMPAOUT(I) = GMPA
         GMPBOUT(I) = GMPB
      ENDIF
      IF(NS.EQ.2)THEN
         TT=XDATA(I,NS+1)
         VARX1=XVAR(ID1(I),1)+TT*TT*XVAR(ID1(I),2)+
     C           2.0*TT*XVAR(ID1(I),3)
         VARX2=XVAR(ID1(I),4)+TT*TT*XVAR(ID1(I),5)+
     C           2.0*TT*XVAR(ID1(I),6)
         SDX1=SQRT(ABS(VARX1))
         SDX2=SQRT(ABS(VARX2))
         DO 447 J=1,2
            XDATAOUT(I, J) = XDATA(I, J)
            XBIGLOGOUT(I, J) = XBIGLOG(I, J)
 447     CONTINUE
         DO 448 J=1,4
            KBIGLOGOUT(I, J) = KBIGLOG(I, J)
 448     CONTINUE
         SDX1OUT(I) = SDX1
         SDX2OUT(I) = SDX2
         VARX1OUT(I) = VARX1
         VARX2OUT(I) = VARX2
         GMPAOUT(I) = GMPA
         GMPBOUT(I) = GMPB
      ENDIF
 61   CONTINUE
c$$$      WRITE(*,202)NXTOT,SUMLOG1,SUMLOG2
C
 9999 CONTINUE
C
      WEIGHTSOUT = WEIGHT(1:(NOMSTARTIN(1) + 1))
      end
C
C  ***************************************************************************
C    SUBROUTINE SIGMAS--FINDS BEST STARTING ESTIMATE OF SIGMA-SQUARED VIA
C                       GRID SEARCH
C  ***************************************************************************
C
      SUBROUTINE SIGMAS(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      use xxcom_mod
      use mine_mod
      DIMENSION WDERV(99),ZDF(150000,4)
      NINC=15
      XINC=.1
C
C  DETERMINE DIRECTION TO START SEARCH FOR BETA
C
C    CURRENT VALUE
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      SAVECURR=XPLOG
C
C    CURRENT UP XINC
C
      WEIGHT(NS+1)=WEIGHT(NS+1)+XINC
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      SAVEUP=XPLOG
      WEIGHT(NS+1)=WEIGHT(NS+1)-XINC
C
C    CURRENT DOWN XINC
C
      WEIGHT(NS+1)=WEIGHT(NS+1)-XINC
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      SAVEDWN=XPLOG
      WEIGHT(NS+1)=WEIGHT(NS+1)+XINC
C
C  DETERMINE DIRECTION TO MOVE
C
      IF(SAVEUP.GT.SAVECURR)THEN
         WEIGHT(NS+1)=WEIGHT(NS+1)+XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVECURR=XPLOG
         DO 1 I=1,NINC
         WEIGHT(NS+1)=WEIGHT(NS+1)+XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVEUP=XPLOG
C
C  STEP FORWARD UNTIL LOG-LIKELIHOOD GOES UP -- IF IT GOES UP
C    RESTORE PREVIOUS VALUE AND HALVE THE STEP
C
         IF(SAVEUP.LT.SAVECURR)THEN
            WEIGHT(NS+1)=WEIGHT(NS+1)-XINC
            XINC=XINC/2.0
         ENDIF
         IF(SAVEUP.GT.SAVECURR)THEN
            SAVECURR=SAVEUP
         ENDIF
c$$$         WRITE(*,1111)SAVECURR,SAVEUP
 1111 FORMAT(2F15.3)
  1      CONTINUE
      ENDIF
      IF(SAVEDWN.GT.SAVECURR)THEN
         WEIGHT(NS+1)=WEIGHT(NS+1)-XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVECURR=XPLOG
         DO 2 I=1,NINC
         WEIGHT(NS+1)=WEIGHT(NS+1)-XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVEDWN=XPLOG
C
C  STEP BACKWARD UNTIL LOG-LIKELIHOOD GOES UP -- IF IT GOES UP
C    RESTORE PREVIOUS VALUE AND HALVE THE STEP
C
         IF(SAVEDWN.LT.SAVECURR)THEN
            WEIGHT(NS+1)=WEIGHT(NS+1)+XINC
            XINC=XINC/2.0
         ENDIF
         IF(SAVEDWN.GT.SAVECURR)THEN
            SAVECURR=SAVEDWN
         ENDIF
C         WRITE(*,1111)SAVECURR,SAVEDWN
  2      CONTINUE
      ENDIF
C
      RETURN
      END
C
C  ***************************************************************************
C    SUBROUTINE WINT---FINDS BEST STARTING ESTIMATE OF DIMENSION WEIGHT VIA
C                      GRID SEARCH
C  ***************************************************************************
C
      SUBROUTINE WINT(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      use xxcom_mod
      use mine_mod
      DIMENSION WDERV(99),ZDF(150000,4)
      NINC=15
      XINC=.01
C
C  DETERMINE DIRECTION TO START SEARCH FOR W
C
C    CURRENT VALUE
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      SAVECURR=XPLOG
C
C    CURRENT UP XINC
C
      WEIGHT(2)=WEIGHT(2)+XINC
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      SAVEUP=XPLOG
      WEIGHT(2)=WEIGHT(2)-XINC
C
C    CURRENT DOWN XINC
C
      WEIGHT(2)=WEIGHT(2)-XINC
C
      CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      SAVEDWN=XPLOG
      WEIGHT(2)=WEIGHT(2)+XINC
C
C  DETERMINE DIRECTION TO MOVE
C
      IF(SAVEUP.GT.SAVECURR)THEN
         WEIGHT(2)=WEIGHT(2)+XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVECURR=XPLOG
         DO 1 I=1,NINC
         WEIGHT(2)=WEIGHT(2)+XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVEUP=XPLOG
C
C  STEP FORWARD UNTIL LOG-LIKELIHOOD GOES UP -- IF IT GOES UP
C    RESTORE PREVIOUS VALUE AND HALVE THE STEP
C
         IF(SAVEUP.LT.SAVECURR)THEN
            WEIGHT(2)=WEIGHT(2)-XINC
            XINC=XINC/2.0
         ENDIF
         IF(SAVEUP.GT.SAVECURR)THEN
            SAVECURR=SAVEUP
         ENDIF
c$$$         WRITE(*,1111)SAVECURR,SAVEUP
 1111 FORMAT(2F15.3)
  1      CONTINUE
      ENDIF
      IF(SAVEDWN.GT.SAVECURR)THEN
         WEIGHT(2)=WEIGHT(2)-XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVECURR=XPLOG
         DO 2 I=1,NINC
         WEIGHT(2)=WEIGHT(2)-XINC
         CALL PLOG(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
         SAVEDWN=XPLOG
C
C  STEP BACKWARD UNTIL LOG-LIKELIHOOD GOES UP -- IF IT GOES UP
C    RESTORE PREVIOUS VALUE AND HALVE THE STEP
C
         IF(SAVEDWN.LT.SAVECURR)THEN
            WEIGHT(2)=WEIGHT(2)+XINC
            XINC=XINC/2.0
         ENDIF
         IF(SAVEDWN.GT.SAVECURR)THEN
            SAVECURR=SAVEDWN
         ENDIF
C         WRITE(*,1111)SAVECURR,SAVEDWN
  2      CONTINUE
      ENDIF
C
      RETURN
      END
C
C  **************************************************************************
C    SUBROUTINE JAN11PT -- FINDS OPTIMAL CUTTING POINT FOR ONE DIMENSION
C  **************************************************************************
C
      SUBROUTINE JAN11PT(NP,IVOT,YSS,KA,WS,JCH,JEH,JCL,JEL,KCCUT,
     C                    LCCUT)
      DIMENSION YSS(2901),KA(2901),WS(5802),LV(2901),LVB(2901),
     C          LEB(2901),Z(2901),Y(2901),
     C          LE(2901),LJEP(2901),RICE(2901),
     C          AAJEP(101),
     C          ABJEP(101),LAJEP(101),LBJEP(101),LCJEP(101),
     C          LDJEP(101),ABABJEP(101),MJEP(101)
C
      NPN=NP+1
      NPP=NP-1
      KCUT=KCCUT
      LCUT=LCCUT
      NOTE=1
      AA1=0.0
      AB1=0.0
      LA1=0
      LB1=0
      LC1=0
      LD1=0
      DO 999 III=1,NOTE
C
C  CHECK ALL POSSIBLE INTERIOR CUT POINTS  --  THE NP INPUT POINTS
C      ARE HELD FIXED.  THERE ARE NP POSSIBLE CUT-POINTS BEGINNING
C      WITH CUT-POINT 1 WHICH IS .001 UNITS TO THE LEFT OF POINT 1.
C      CUT-POINT 2 IS BETWEEN POINTS 1 AND 2, ETC.
C
C     1   2   3   4   5   6   7   8   9   10   11 ...... NP-1   NP
C    *  *   *   *   *   *   *   *   *   *    *                *
C    1  2   3   4   5   6   7   8   9  10   11  ...........  NP
C
C  IF KCUT=1 AND LCUT=6, THE FOLLOWING NP PATTERNS ARE TESTED
C
C PATTERN
C   1         6666666666666666666666
C   2         1666666666666666666666
C   3         1166666666666666666666
C   4         1116666666666666666666
C   5         1111666666666666666666
C   6         1111166666666666666666
C   7         1111116666666666666666
C   .           .....
C   .           .....
C   .           .....
C  NP-1       1111111111111111111166
C   NP        1111111111111111111116
C
C  BECAUSE THE PROGRAM TRIES BOTH KCUT=1/LCUT=6 AND KCUT=6/LCUT=1, THIS
C  WILL ALSO TEST THE ONE MISSING PATTERN ABOVE, VIZ., ALL "1"s.
C
C
      KSE=0
      KSV=0
      LSV=0
      LSE=0
      KMARK=1
      I=0
  10  I=I+1
      IF(I-NP-1)61,12,12
  61  Z(I)=999.0
      IF(I.EQ.1)THEN
         Y(I)=YSS(1)-.001
      ENDIF
      IF(I.GT.1)THEN
         Y(I)=(YSS(I)+YSS(I-1))/2.0
      ENDIF
C      IF(KA(I).EQ.9)GO TO 10
      IF(KMARK.EQ.1)THEN
         DO 3 J=I,NP
         IF(KA(J).EQ.9)GO TO 3
         IF(LCUT-KA(J))33,5,33
  33     IF(KCUT-KA(J))3,6,3
  5      LSV=LSV+1
         GO TO 3
  6      LSE=LSE+1
  3      CONTINUE
         KMARK=0
         GO TO 31
      ENDIF
      IF(KA(I-1).EQ.KCUT)THEN
         KSV=KSV+1
         LSE=LSE-1
      ENDIF
      IF(KA(I-1).EQ.LCUT)THEN
         KSE=KSE+1
         LSV=LSV-1
      ENDIF
C
  31  CONTINUE
      LJEP(I)=I
      LV(I)=KSV
      LVB(I)=LSV
      LE(I)=KSE
      LEB(I)=LSE
      KT=LV(I)+LE(I)+LVB(I)+LEB(I)
      Z(I)=FLOAT(LE(I)+LEB(I))/FLOAT(KT)
      COX1=0.0
      COX2=0.0
      IF(LV(I)+LEB(I).GT.0)COX1=FLOAT(LV(I))/FLOAT(LV(I)+LEB(I))
      IF(LVB(I)+LE(I).GT.0)COX2=FLOAT(LE(I))/FLOAT(LVB(I)+LE(I))
      RICE(I)=COX1-COX2
 1001 FORMAT(I4,F7.3,I2,4I4,F7.3)
C
      GO TO 10
  12  CONTINUE
C
C  FIND BEST CUT POINT
C
      CALL RSORT(Z,NP,LJEP)
      KIN=1
      MJEP(1)=1
      AAJEP(KIN)=Z(1)
      ABJEP(KIN)=Y(LJEP(1))
      ABABJEP(KIN)=ABS(ABJEP(KIN))
      LAJEP(KIN)=LV(LJEP(1))
      LBJEP(KIN)=LE(LJEP(1))
      LCJEP(KIN)=LVB(LJEP(1))
      LDJEP(KIN)=LEB(LJEP(1))
C
C  CHECK IF THERE ARE MULTIPLE CUT-POINTS WITH SAME CLASSIFICATION AND
C    SELECT THAT CUT-POINT CLOSEST TO THE INTERIOR OF THE SPACE
C
      DO 63 I=2,NP
      IF(ABS(Z(1)-Z(I)).LE..00001)THEN
         KIN=KIN+1
         MJEP(KIN)=KIN
         AAJEP(KIN)=Z(I)
         ABJEP(KIN)=Y(LJEP(I))
         ABABJEP(KIN)=ABS(ABJEP(KIN))
         LAJEP(KIN)=LV(LJEP(I))
         LBJEP(KIN)=LE(LJEP(I))
         LCJEP(KIN)=LVB(LJEP(I))
         LDJEP(KIN)=LEB(LJEP(I))
         IF(KIN.GT.100)GO TO 633
         GO TO 63
      ENDIF
      IF(Z(1).LT.Z(I))GO TO 633
  63  CONTINUE
  633 CONTINUE
      IF(KIN.EQ.1)THEN
         AA=AAJEP(1)
         AB=ABJEP(1)
         LA=LAJEP(1)
         LB=LBJEP(1)
         LC=LCJEP(1)
         LD=LDJEP(1)
      ENDIF
      IF(KIN.GT.1)THEN
         CALL RSORT(ABABJEP,KIN,MJEP)
         AA=AAJEP(MJEP(1))
         AB=ABJEP(MJEP(1))
         LA=LAJEP(MJEP(1))
         LB=LBJEP(MJEP(1))
         LC=LCJEP(MJEP(1))
         LD=LDJEP(MJEP(1))
      ENDIF
C
      AA1=AA
      AB1=AB
      LA1=LA
      LB1=LB
      LC1=LC
      LD1=LD
C
  999 CONTINUE
      AA=AA1
      AB=AB1
      LA=LA1
      LB=LB1
      LC=LC1
      LD=LD1
      WS(IVOT)=AB
      JCL=LA
      JEL=LB
      JCH=LC
      JEH=LD
C
      RETURN
      END
C
C  ***************************************************************************
C    SUBROUTINE PLOG---CALCULATES LOG-LIKELIHOOD FOR THE NOMINATE PROBIT   
C                      MODEL      
C  ***************************************************************************
C
      SUBROUTINE PLOG(XPLOG,WDERV,NFIRST,NLAST,
     C                  ZDF,NDEVIT,XDEVIT)
      use xxcom_mod
      use mine_mod
      DIMENSION DYES(99),DNO(99),DCC(99),DBB(99),
     C          WDERV(99),ZDF(150000,4),WDERV2(99)
 1001 FORMAT(I4,F5.2,F10.7)
 1002 FORMAT(' LOG-L',2I10,10F8.5)
 1003 FORMAT(4I4,3F15.3)
 1004 FORMAT(5I8,6F15.3)
 1005 FORMAT(I4,2I5,I8,3F15.3)
 1006 FORMAT(I10,11F20.3)
 1007 FORMAT(' LNL',2I10,F15.3,10F8.4)
C
C
      XPLOG=0.0
      XXPLOG=0.0
C
      DO 7 K=1,2*NS+2
      WDERV(K)=0.0
      WDERV2(K)=0.0
  7   CONTINUE
      KTOT=0
      KTOTP=0
      KTOTQ=0
      KLASS=0
      KLASSYY=0
      KLASSNY=0
      KLASSYN=0
      KLASSNN=0
      KLASS2=0
      IF(NFIRST.GT.1)THEN
         DO 11 II=1,NFIRST-1
         NPC=NUMCONG(II)
         NQC=NUMCONGT(II)
         KTOTP=KTOTP+NPC
         KTOTQ=KTOTQ+NQC
  11     CONTINUE
      ENDIF
      DO 1 II=NFIRST,NLAST
      NPC=NUMCONG(II)
      NQC=NUMCONGT(II)
      DO 2 I=1,NPC
      YPLOG=0.0
      KPLOG=0
      KPWRONG=0
      DO 8 K=1,2*(NS+1)
      WDERV(K)=0.0
  8   CONTINUE
      DO 33 J=1,NQC
      IF(RCBAD(J+KTOTQ).EQV..TRUE.)THEN
         DO 3 K=1,NS
         DYES(K)=0.0
         DNO(K)=0.0
         DYES(K)=(XDATA(I+KTOTP,K)-ZMID(J+KTOTQ,K)+DYN(J+KTOTQ,K))**2
         DNO(K) =(XDATA(I+KTOTP,K)-ZMID(J+KTOTQ,K)-DYN(J+KTOTQ,K))**2
  3      CONTINUE
C
C  IF NOT MISSING DATA
C
         IF(RCVOTE9(I+KTOTP,J).EQV..FALSE.)THEN
            KTOT=KTOT+1
C
C  IF YES
C
            IF(RCVOTE1(I+KTOTP,J).EQV..TRUE.)THEN
               DC=0.0
               DB=0.0
               DO 4 K=1,NS
               DC=DC+(-WEIGHT(K)*WEIGHT(K)*DYES(K))
               DB=DB+(-WEIGHT(K)*WEIGHT(K)*DNO(K))
               DCC(K)=DYES(K)
               DBB(K)=DNO(K)
  4            CONTINUE
               XCC=+1.0
            ENDIF
C
C  IF NO
C
            IF(RCVOTE1(I+KTOTP,J).EQV..FALSE.)THEN
               DC=0.0
               DB=0.0
               DO 5 K=1,NS
               DC=DC+(-WEIGHT(K)*WEIGHT(K)*DNO(K))
               DB=DB+(-WEIGHT(K)*WEIGHT(K)*DYES(K))
               DCC(K)=DNO(K)
               DBB(K)=DYES(K)
  5            CONTINUE
               XCC=-1.0
            ENDIF
C
            ZS=WEIGHT(NS+1)*(EXP(DC)-EXP(DB))
C
C  CROSS CLASSIFICATIONS
C
            IF(ABS(DC).LE.ABS(DB))KLASS=KLASS+1
            IF(ABS(DC).LE.ABS(DB).AND.XCC.EQ.+1.0)THEN
               KLASSYY=KLASSYY+1
            ENDIF
            IF(ABS(DC).GT.ABS(DB).AND.XCC.EQ.+1.0)THEN
               KLASSNY=KLASSNY+1
            ENDIF
            IF(ABS(DC).GT.ABS(DB).AND.XCC.EQ.-1.0)THEN
               KLASSYN=KLASSYN+1
            ENDIF
            IF(ABS(DC).LE.ABS(DB).AND.XCC.EQ.-1.0)THEN
               KLASSNN=KLASSNN+1
            ENDIF
            IF(ZS.GT.0.0)KLASS2=KLASS2+1
C
            WWIMJ=ZS*XDEVIT
            KWIMJ=IFIX(ABS(WWIMJ)+.5)
            IF(KWIMJ.GT.NDEVIT-2)KWIMJ=NDEVIT-2
            IF(ZS.GE.0.0)THEN
               ZDISTF=ZDF(NDEVIT-1+KWIMJ+1,2)
               CDFLOG=ZDF(NDEVIT-1+KWIMJ+1,3)
            ENDIF
            IF(ZS.LT.0.0)THEN
               ZDISTF=ZDF(NDEVIT-KWIMJ,2)
               CDFLOG=ZDF(NDEVIT-KWIMJ,3)
               KPWRONG=KPWRONG+1
            ENDIF
            XPLOG=XPLOG+CDFLOG
            YPLOG=YPLOG+CDFLOG
            KPLOG=KPLOG+1
            ZGAUSS=EXP(-(ZS*ZS)/2.0)
            DO 6 K=1,NS
            AZULU=-2.0*WEIGHT(K)*WEIGHT(NS+1)*
     C       (((1.0/SQRT(2.0*3.1415926536))*EXP(-(ZS*ZS)/2.0))/ZDISTF)*
     C               (DCC(K)*EXP(DC)-DBB(K)*EXP(DB))
            WDERV(K)=WDERV(K)+AZULU
            WDERV(NS+1+K)=WDERV(NS+1+K)+AZULU**2
  6         CONTINUE
            AZULU=
     C       (((1.0/SQRT(2.0*3.1415926536))*EXP(-(ZS*ZS)/2.0))/ZDISTF)*
     C            (EXP(DC) - EXP(DB))
            WDERV(NS+1)=WDERV(NS+1)+AZULU
            WDERV(NS+1+NS+1)=WDERV(NS+1+NS+1)+AZULU**2
         ENDIF
      ENDIF
  33  CONTINUE
      XBIGLOG(I+KTOTP,2)=YPLOG
      KBIGLOG(I+KTOTP,2)=KPLOG
      KBIGLOG(I+KTOTP,4)=KPWRONG
      XXPLOG=XXPLOG+YPLOG
      DO 9 K=1,NS+1+NS+1
      WDERV2(K)=WDERV2(K)+WDERV(K)
  9   CONTINUE
  2   CONTINUE
      KTOTP=KTOTP+NPC
      KTOTQ=KTOTQ+NQC
  1   CONTINUE
C
C      WRITE(* ,1006)KTOT,XPLOG,XXPLOG
      GMP=EXP(XXPLOG/FLOAT(KTOT))
      XCLASS=FLOAT(KLASS)/FLOAT(KTOT)
c$$$      WRITE(*,1007)KTOT,KLASS,XXPLOG,XCLASS,GMP,(WEIGHT(JJ),JJ=1,NS+1)
C      WRITE(*,1006)KTOT,XXPLOG,(WDERV2(JJ),JJ=1,NS+1)
C      WRITE(*,1006)KTOT,XXPLOG,(WDERV2(JJ),JJ=NS+2,2*(NS+1))
      XPLOG=XXPLOG
      RETURN
      END
C
C  ***************************************************************************
C    SUBROUTINE PROX--CALCULATES DERIVATIVES AND LOG-LIKELIHOODS FOR THE
C                       LEGISLATOR PARAMETERS FOR THE NOMINATE PROBIT   
C                       MODEL      
C  ***************************************************************************
C
      SUBROUTINE PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C                  XDERV,XDERV1,XDERV2,XDERV3,
     C                  XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C                  NFIRST,NLAST,
     C                  ZDF,NDEVIT,XDEVIT,NMODEL)
      use xxcom_mod
      use mine_mod
      DIMENSION DYES(99),DNO(99),DYES1(99),DNO1(99),
     C          DCC(99),DBB(99),DCC1(99),DBB1(99),XDERV(99),
     C          XDERV1(99),XDERV2(99),XDERV3(99),
     C          ATIME(152,127),MARK(99,3),XMARK(99,3),
     C          XBETA(5,5),ZDF(150000,4),OUTX0(99,99),AADERV(99),
     C          OUTX1(99,99),OUTX2(99,99),OUTX3(99,99)
  100 FORMAT(8F7.3)
  200 FORMAT(I6,2I5,2F13.5)
 1001 FORMAT(I4,F5.2,F10.7)
 1002 FORMAT(' LEG ',I6,2I6,5F7.3,2F13.5)
C
      XPLOG=0.0
      DO 40 K=1,4*NS
      DO 40 J=1,4*NS
      OUTX0(J,K)=0.0
      OUTX1(J,K)=0.0
      OUTX2(J,K)=0.0
      OUTX3(J,K)=0.0
  40  CONTINUE
      KK=0
      KTOTQ=0
      IF(NFIRST.GT.1)THEN
         DO 11 J=1,NFIRST-1
         NQ=MCONG(J,2)
         KTOTQ=KTOTQ+NQ
  11     CONTINUE
      ENDIF
      DO 51 J=NFIRST,NLAST
      NQ=MCONG(J,2)
      IF(LWHERE(NEP,J).EQV..TRUE.)THEN
         KK=KK+1
         MWHERE=KWHERE(NEP,J)
         MARK(KK,1)=MWHERE
         MARK(KK,2)=NQ
         MARK(KK,3)=KTOTQ
C
C  STORE TIME TREND TERMS TO CALCULATE STANDARD ERRORS IN MAIN
C   PROGRAM
C
         XDATA(MWHERE,NS+1)=0.0
         XDATA(MWHERE,NS+2)=0.0
         XDATA(MWHERE,NS+3)=0.0
C
         DO 52 K=1,NS
C
C  CONSTANT MODEL
C
         IF(NMODEL.EQ.0)THEN
            XDATA(MWHERE,K)=ATIME(KK,1)*XBETA(1,K)
         ENDIF
C
C  LINEAR MODEL
C
         IF(NMODEL.EQ.1)THEN
            XDATA(MWHERE,K)=ATIME(KK,1)*XBETA(1,K)+
     C                        ATIME(KK,2)*XBETA(2,K)
            XDATA(MWHERE,NS+1)=ATIME(KK,2)
         ENDIF
C
C  QUADRATIC MODEL
C
         IF(NMODEL.EQ.2)THEN
            XDATA(MWHERE,K)=ATIME(KK,1)*XBETA(1,K)+
     C                        ATIME(KK,2)*XBETA(2,K)+
     C                        ATIME(KK,3)*XBETA(3,K)
            XDATA(MWHERE,NS+1)=ATIME(KK,2)
            XDATA(MWHERE,NS+2)=ATIME(KK,3)
         ENDIF
C
C  CUBIC MODEL
C
         IF(NMODEL.EQ.3)THEN
            XDATA(MWHERE,K)=ATIME(KK,1)*XBETA(1,K)+
     C                        ATIME(KK,2)*XBETA(2,K)+
     C                        ATIME(KK,3)*XBETA(3,K)+
     C                        ATIME(KK,4)*XBETA(4,K)
            XDATA(MWHERE,NS+1)=ATIME(KK,2)
            XDATA(MWHERE,NS+2)=ATIME(KK,3)
            XDATA(MWHERE,NS+3)=ATIME(KK,4)
         ENDIF
C
         XMARK(KK,K)=XDATA(MWHERE,K)
  52     CONTINUE
      ENDIF
      KTOTQ=KTOTQ+NQ
  51  CONTINUE
      KLASS=0
      KLASS2=0
      DO 8 K=1,NS
      XDERV(K)=0.0
      XDERV1(K)=0.0
      XDERV2(K)=0.0
      XDERV3(K)=0.0
  8   CONTINUE
C
C  **************
C
C  LOOP OVER NUMBER OF CONGRESSES MEMBER SERVED IN
C
C
      KTOT=0
      DO 22 JJ=1,KK
      YPLOG=0.0
      KPLOG=0
      KPWRONG=0
      MWHERE=MARK(JJ,1)
      NQ=MARK(JJ,2)
      KTOTQ=MARK(JJ,3)
C
C  LOOP OVER NUMBER OF ROLL CALLS WITHIN THE CURRENT CONGRESS
C
      DO 2 J=1,NQ
C
C  IF AT LEAST 2.5% IN MINORITY 
C
      IF(RCBAD(J+KTOTQ).EQV..TRUE.)THEN
C
      DO 3 K=1,NS
      DYES(K)=0.0
      DNO(K)=0.0
      DYES1(K)=0.0
      DNO1(K)=0.0
      DYES(K)=(XDATA(MWHERE,K)-ZMID(J+KTOTQ,K)+DYN(J+KTOTQ,K))**2
      DNO(K) =(XDATA(MWHERE,K)-ZMID(J+KTOTQ,K)-DYN(J+KTOTQ,K))**2
      DYES1(K)=(XDATA(MWHERE,K)-ZMID(J+KTOTQ,K)+DYN(J+KTOTQ,K))
      DNO1(K) =(XDATA(MWHERE,K)-ZMID(J+KTOTQ,K)-DYN(J+KTOTQ,K))
  3   CONTINUE
C
C  IF NOT MISSING DATA
C
      IF(RCVOTE9(MWHERE,J).EQV..FALSE.)THEN
         KTOT=KTOT+1
C
C  IF YES
C
         IF(RCVOTE1(MWHERE,J).EQV..TRUE.)THEN
            DC=0.0
            DB=0.0
            DO 4 K=1,NS
            DC=DC+(-WEIGHT(K)*WEIGHT(K)*DYES(K))
            DB=DB+(-WEIGHT(K)*WEIGHT(K)*DNO(K))
            DCC(K)=DYES(K)
            DBB(K)=DNO(K)
            DCC1(K)=DYES1(K)*WEIGHT(K)*WEIGHT(K)
            DBB1(K)=DNO1(K)*WEIGHT(K)*WEIGHT(K)
  4         CONTINUE
         ENDIF
C
C  IF NO
C
         IF(RCVOTE1(MWHERE,J).EQV..FALSE.)THEN
            DC=0.0
            DB=0.0
            DO 5 K=1,NS
            DC=DC+(-WEIGHT(K)*WEIGHT(K)*DNO(K))
            DB=DB+(-WEIGHT(K)*WEIGHT(K)*DYES(K))
            DCC(K)=DNO(K)
            DBB(K)=DYES(K)
            DCC1(K)=DNO1(K)*WEIGHT(K)*WEIGHT(K)
            DBB1(K)=DYES1(K)*WEIGHT(K)*WEIGHT(K)
  5         CONTINUE
         ENDIF
C
         ZS=WEIGHT(NS+1)*(EXP(DC)-EXP(DB))
C
         IF(ABS(DC).LE.ABS(DB))KLASS=KLASS+1
         IF(ZS.GT.0.0)KLASS2=KLASS2+1
C
         WWIMJ=ZS*XDEVIT
         KWIMJ=IFIX(ABS(WWIMJ)+.5)
         IF(KWIMJ.GT.NDEVIT-2)KWIMJ=NDEVIT-2
         IF(ZS.GE.0.0)THEN
            ZDISTF=ZDF(NDEVIT-1+KWIMJ+1,2)
            CDFLOG=ZDF(NDEVIT-1+KWIMJ+1,3)
         ENDIF
         IF(ZS.LT.0.0)THEN
            ZDISTF=ZDF(NDEVIT-KWIMJ,2)
            CDFLOG=ZDF(NDEVIT-KWIMJ,3)
            KPWRONG=KPWRONG+1
         ENDIF
         XPLOG=XPLOG+CDFLOG
         YPLOG=YPLOG+CDFLOG
         KPLOG=KPLOG+1
         ZGAUSS=EXP(-(ZS*ZS)/2.0)
C
C  OUTER PRODUCT MATRIX
C
         DO 31 K=1,NS
         AADERV(K)=(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
         IF(NMODEL.EQ.1)THEN
            AADERV(K+NS)=ATIME(JJ,2)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
         ENDIF
         IF(NMODEL.EQ.2)THEN
            AADERV(K+NS)=ATIME(JJ,2)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
            AADERV(K+2*NS)=ATIME(JJ,3)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
         ENDIF
         IF(NMODEL.EQ.3)THEN
            AADERV(K+NS)=ATIME(JJ,2)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
            AADERV(K+2*NS)=ATIME(JJ,3)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
            AADERV(K+3*NS)=ATIME(JJ,4)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
         ENDIF
  31     CONTINUE
         DO 32 JJK=1,NS
         DO 32 JJJ=1,NS
         OUTX0(JJJ,JJK)=OUTX0(JJJ,JJK)+AADERV(JJK)*AADERV(JJJ)
  32     CONTINUE
         IF(NMODEL.EQ.1)THEN
            DO 33 JJK=1,2*NS
            DO 33 JJJ=1,2*NS
            OUTX1(JJJ,JJK)=OUTX1(JJJ,JJK)+AADERV(JJK)*AADERV(JJJ)
  33        CONTINUE
         ENDIF
         IF(NMODEL.EQ.2)THEN
            DO 34 JJK=1,3*NS
            DO 34 JJJ=1,3*NS
            OUTX2(JJJ,JJK)=OUTX2(JJJ,JJK)+AADERV(JJK)*AADERV(JJJ)
  34        CONTINUE
         ENDIF
         IF(NMODEL.EQ.3)THEN
            DO 35 JJK=1,4*NS
            DO 35 JJJ=1,4*NS
            OUTX3(JJJ,JJK)=OUTX3(JJJ,JJK)+AADERV(JJK)*AADERV(JJJ)
  35        CONTINUE
         ENDIF
C
         DO 6 K=1,NS
         XDERV(K)=XDERV(K)+(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
         XDERV1(K)=XDERV1(K)+ATIME(JJ,2)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
         XDERV2(K)=XDERV2(K)+ATIME(JJ,3)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
         XDERV3(K)=XDERV3(K)+ATIME(JJ,4)*(ZGAUSS/ZDISTF)*
     C            (DCC1(K)*EXP(DC)-DBB1(K)*EXP(DB))
  6      CONTINUE
      ENDIF
      ENDIF
  2   CONTINUE
      XBIGLOG(MWHERE,1)=YPLOG
      KBIGLOG(MWHERE,1)=KPLOG
      KBIGLOG(MWHERE,3)=KPWRONG
  22  CONTINUE
      GMP=EXP(XPLOG/FLOAT(KTOT))
      KRC=KTOT
      RETURN
      END
C
C  ***************************************************************************
C    SUBROUTINE XINT---FINDS LEGISLATOR PARAMETERS
C  ***************************************************************************
C
      SUBROUTINE XINT(NEP,ID1,XPLOG0,XPLOG1,XPLOG2,XPLOG3,
     C                 NFIRST,NLAST,NNMODEL,
     C                 ZDF,NDEVIT,XDEVIT,KXTOT,XBETA,OUTX0,
     C                 OUTX1,OUTX2,OUTX3,DERVISH)
      use xxcom_mod
      use mine_mod
      DIMENSION ATIME(152,127),ZDF(150000,4),
     C            YYY(152),VVV(152),ID1(54001),XMARK(99,3),
     C            XDERV(99),XDERV1(99),XDERV2(99),XDERV3(99),
     C            XXX(152),ZMAT(99,99),WVEC(199),DERVISH(99,99),
     C            LLL(152),YLOG(152),YGAMMA(152,99),
     C            XXXSAVE(152,99),XBETA(5,5),XBETASV(5,5),
     C            YGMP(152),YYGMP(152),OUTX0(99,99),
     C            OUTX1(99,99),OUTX2(99,99),OUTX3(99,99),WORK(26*99)
      CHARACTER outmsg(255)
  101 FORMAT(' PERFORMANCE INDEX EIGENVALUE/VECTOR ROUTINE=',3I6)
  102 FORMAT(2I6,30F10.4)
  103 FORMAT(12X,30F10.4)
  201 FORMAT(I6,2I4,150F10.4)
  203 FORMAT(I6,I5,F12.5,20F10.4)
  205 FORMAT(I6,I3,I5,I4,I3,F12.5,30F10.4)
  206 FORMAT(I6,I5,4F8.4,4F12.5)
  307 FORMAT(' NEGATIVE SQUARE ROOT FARTKNOCKER!!')
 1000 FORMAT(3I6,I3,1X,11A1,155F7.3)
 1001 FORMAT(' LEG',I6,2I4,2F7.3,2F13.5)
 1002 FORMAT(10X,2F10.5,55F7.3)
 1003 FORMAT(' MAJOR ERROR',I6)
C
      KK=0
      DO 50 J=NFIRST,NLAST
      IF(LWHERE(NEP,J).EQV..TRUE.)KK=KK+1
  50  CONTINUE
C
C  **********
C  GET STARTING ESTIMATES OF THE BETAS FOR THE LEGISLATOR TIME POLYNOMIAL
C    ONE DIMENSION AT A TIME USING SIMPLE OLS ON THE LEGRENDRE POLYNOMIALS
C
C        Xikt = Xik0 + Xik1*T + Xik2T**2 WHERE T IS THE LEGRENDRE POLYNOMIAL
C
C        XBETA(1,K) CONTAINS Xik0
C        XBETA(2,K) CONTAINS Xik1
C        XBETA(3,K) CONTAINS Xik2
C        XBETA(4,K) CONTAINS Xik3
C  *********
C    
      XINC=0.0
      IF(KK.GT.1)XINC=2.0/(FLOAT(KK)-1.0)
      SUM=0.0
      DO 52 K=1,NS
      XBETA(1,K)=0.0
      XBETA(2,K)=0.0
      XBETA(3,K)=0.0
      XBETA(4,K)=0.0
      KK=0
      DO 51 J=NFIRST,NLAST
      IF(LWHERE(NEP,J).EQV..TRUE.)THEN
         KK=KK+1
         MWHERE=KWHERE(NEP,J)
         ATIME(KK,1)=1.0
         XTIME=-1.0+(FLOAT(KK)-1.0)*XINC
         ATIME(KK,2)=XTIME
         ATIME(KK,3)=(3.0*XTIME*XTIME - 1.0)/2.0
         ATIME(KK,4)=(5.0*XTIME*XTIME*XTIME - 3.0*XTIME)/2.0
         YYY(KK)=XDATA(MWHERE,K)
      ENDIF
  51  CONTINUE
C
C  LINEAR MODEL
C
      IF(KK.EQ.5)THEN
         CALL REGA(KK,2,ATIME,YYY,VVV)
      ENDIF
C
C  QUADRATIC MODEL
C
      IF(KK.EQ.6)THEN
         CALL REGA(KK,3,ATIME,YYY,VVV)
      ENDIF
C
C  CUBIC MODEL
C
      IF(KK.GE.7)THEN
         CALL REGA(KK,4,ATIME,YYY,VVV)
      ENDIF
C
      IF(KK.LT.5)THEN
         VVV(1)=YYY(1)
         VVV(2)=0.0
         VVV(3)=0.0
         VVV(4)=0.0
      ENDIF
      XBETA(1,K)=VVV(1)
      XBETA(2,K)=VVV(2)
      XBETA(3,K)=VVV(3)
      XBETA(4,K)=VVV(4)
      XBETASV(1,K)=XBETA(1,K)
      XBETASV(2,K)=XBETA(2,K)
      XBETASV(3,K)=XBETA(3,K)
      XBETASV(4,K)=XBETA(4,K)
C
C  CONSTRAIN TO CONSTANT MODEL HERE
C  ****
C      XBETA(2,K)=0.0
C      XBETA(3,K)=0.0
C      XBETA(4,K)=0.0
C  ****
C
      SUM=SUM+XBETA(1,K)**2
  52  CONTINUE
C
C  DO CHECK ON STARTING VALUE OF LEGISLATOR COORDINATE -- IF OUTSIDE
C   HYPERSPHERE PULL IT BACK INSIDE
C
      IF(SUM.GT.1.0)THEN
         DO 55 K=1,NS
         XBETA(1,K)=.75*(XBETA(1,K)/SQRT(SUM))
  55     CONTINUE
      ENDIF
C
C  SAVE NUMBER OF CONGRESSES HERE
C
      NEPCONG=KK
C
C  CALCULATE NEW PROJECTED LEGISLATOR COORDINATE WITH A 
C      SIMPLE GRID SEARCH
C
      NINC=25
C
C ***********
C   CONSTANT TERM FIRST
C ***********
C
      DO 99 IIII=1,10
C
      DO 2 K=1,NS
      XBETASV(1,K)=XBETA(1,K)
      XXX(K)=0.0
  2   CONTINUE
C
      DO 40 K=1,NS
      XDERV(K)=0.0
      XDERV1(K)=0.0
      XDERV2(K)=0.0
      XDERV3(K)=0.0
  40  CONTINUE
C
C  GET DERIVATIVES
C
      NMODEL=0
      CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C        XDERV,XDERV1,XDERV2,XDERV3,
     C        XBETASV,OUTX0,OUTX1,OUTX2,OUTX3,
     C        NFIRST,NLAST,
     C        ZDF,NDEVIT,XDEVIT,NMODEL)
C
C  SAVE INITIAL LOG-LIKELIHOOD
C
      IF(IIII.EQ.1)THEN
         SAVEGMP=EXP(XPLOG/FLOAT(KRC))
      ENDIF
C
      GMP=EXP(XPLOG/FLOAT(KRC))
      SUMA=0.0
      DO 61 K=1,NS
      XXX(K)=XDERV(K)/FLOAT(KRC)
      SUMA=SUMA+XXX(K)**2
  61  CONTINUE
C
C
C  CALCULATE STEPSIZE IN .01 UNITS
C
      STEP=.01/SQRT(SUMA)
C
C
C  SEARCH ALONG VECTOR IN BEST DIRECTION
C
C
      XINC=0.0
C
      DO 212 KK=1,NINC
      SUM=0.0
      DO 24 K=1,NS
C
C  *****
C   CHECK FOR SIGN OF DERIVATIVE FOR GRADIENT!!!!!!!!
C  *****
C
      XBETA(1,K)=XBETASV(1,K)-XINC*XXX(K)
C
      SUM=SUM+XBETA(1,K)**2
  24  CONTINUE
C
C  CHECK TO SEE IF LEGISLATOR POINT OUTSIDE UNIT HYPERSPHERE
C
      IF(SUM.GT.1.0)THEN
C
C  RESET LEGISLATOR POINT TO SURFACE OF UNIT HYPERSPHERE AND
C    CALCULATE DERIVATIVES AND LOG-LIKELIHOODS.  THEN EXIT
C    SEARCH LOOP
C
         DO 241 K=1,NS
         XBETA(1,K)=XBETA(1,K)/SQRT(SUM)
  241    CONTINUE
         DO 243 K=1,NS
         XDERV(K)=0.0
         XDERV1(K)=0.0
         XDERV2(K)=0.0
         XDERV3(K)=0.0
  243    CONTINUE
         NMODEL=0
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
         GMP=EXP(XPLOG/FLOAT(KRC))
C
         YGMP(KK)=GMP
         LLL(KK)=KK
         DO 242 K=1,NS
         YGAMMA(KK,K)=XBETA(1,K)
         XXXSAVE(KK,K)=XXX(K)
  242    CONTINUE
         YLOG(KK)=XPLOG
         GO TO 2112
      ENDIF
      DO 240 K=1,NS
      XDERV(K)=0.0
      XDERV1(K)=0.0
      XDERV2(K)=0.0
      XDERV3(K)=0.0
  240 CONTINUE
C
C  GET DERIVATIVES
C
      NMODEL=0
      CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C        XDERV,XDERV1,XDERV2,XDERV3,
     C        XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C        NFIRST,NLAST,
     C        ZDF,NDEVIT,XDEVIT,NMODEL)
C
      GMP=EXP(XPLOG/FLOAT(KRC))
C
      YGMP(KK)=GMP
      LLL(KK)=KK
      DO 222 K=1,NS
      YGAMMA(KK,K)=XBETA(1,K)
      XXXSAVE(KK,K)=XXX(K)
  222 CONTINUE
      YLOG(KK)=XPLOG
      XINC=XINC+STEP
  212 CONTINUE
C
      NNINC=NINC
      GO TO 2113
 2112 NNINC=KK
 2113 CONTINUE
C
C  FIND MAXIMUM ON BEST DIRECTION THROUGH THE SPACE
C
      CALL RSORT(YGMP,NNINC,LLL)
      DO 224 K=1,NS
      XBETA(1,K)=YGAMMA(LLL(NNINC),K)
      XBETASV(1,K)=XBETA(1,K)
  224 CONTINUE
C
C  STORE GMP
C
      YYGMP(IIII)=YGMP(NNINC)
C
C  EXIT THE SEARCH IF NO IMPROVEMENT AFTER 3rd ITERATION
C
      IF(IIII.GE.3)THEN
         STOPPER=YYGMP(IIII)-YYGMP(IIII-1)
         IF(STOPPER.LE..0001)GO TO 919
      ENDIF
  99  CONTINUE
  919 CONTINUE
C
C
      NMODEL=0
      CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C        XDERV,XDERV1,XDERV2,XDERV3,
     C        XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C        NFIRST,NLAST,
     C        ZDF,NDEVIT,XDEVIT,NMODEL)
C
      GMPNOW=EXP(XPLOG/FLOAT(KRC))
      DO 666 K=1,NS
      SUM=0.0
      DO 665 JJJ=1,NEPCONG
      SUM=SUM+XMARK(JJJ,K)
 665  CONTINUE
      SUM=SUM/FLOAT(NEPCONG)
      IF((SAVEGMP-GMPNOW).GT..00001)THEN
          WRITE(outmsg,1003)NEP
          call rexit(outmsg)
      ENDIF
  666 CONTINUE
C
C  LOG-LIKELIHOOD OF CONSTANT MODEL
C
      XPLOG0=XPLOG
      XPLOG1=XPLOG
      XPLOG2=XPLOG
      XPLOG3=XPLOG
C
      NINC=10
C      
C
C ***********
C   LINEAR TERM 
C ***********
C
      IF(NNMODEL.GE.1.AND.NEPCONG.GE.5)THEN
         NMODEL=1
C
C  INITIALIZE LINEAR BETAS TO ZERO SO STARTING LOG-LIKELIHOOD
C    IS EQUAL TO THE THE ENDING LOG-LIKELIHOOD OF CONSTANT
C    MODEL
C
         DO 41 K=1,NS
         XBETA(2,K)=0.0
  41     CONTINUE
C
         DO 999 IIII=1,10
C   
         DO 30 K=1,NS
         XBETASV(2,K)=XBETA(2,K)
         XXX(K)=0.0
  30     CONTINUE
C
         DO 31 K=1,NS
         XDERV(K)=0.0
         XDERV1(K)=0.0
         XDERV2(K)=0.0
         XDERV3(K)=0.0
  31     CONTINUE
C
C  GET DERIVATIVES
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETASV,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
C  SAVE INITIAL LOG-LIKELIHOOD
C
         IF(IIII.EQ.1)THEN
            SAVEGMP=EXP(XPLOG/FLOAT(KRC))
         ENDIF
C
         GMP=EXP(XPLOG/FLOAT(KRC))
         ASUMA=0.0
         DO 32 K=1,NS
         XXX(K)=XDERV1(K)/FLOAT(KRC)
         ASUMA=ASUMA+XXX(K)**2
  32     CONTINUE
C
C  CALCULATE STEPSIZE IN .01 UNITS
C
         STEP=.01/SQRT(ASUMA)
C      
C
C  SEARCH ALONG VECTOR IN BEST DIRECTION
C
C
         XINC=0.0
C 
         DO 312 KK=1,NINC
         SUM=0.0
         DO 34 K=1,NS
C
C  *****
C   CHECK FOR SIGN OF DERIVATIVE FOR GRADIENT!!!!!!!!
C  *****
C
         XBETA(2,K)=XBETASV(2,K)-XINC*XXX(K)
C
  34     CONTINUE
C
         DO 35 K=1,NS
         XDERV(K)=0.0
         XDERV1(K)=0.0
         XDERV2(K)=0.0
         XDERV3(K)=0.0
  35     CONTINUE
C
C  GET DERIVATIVES
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
         GMP=EXP(XPLOG/FLOAT(KRC))
C
         YGMP(KK)=GMP
         LLL(KK)=KK
         DO 36 K=1,NS
         YGAMMA(KK,K)=XBETA(2,K)
         XXXSAVE(KK,K)=XXX(K)
  36     CONTINUE
         YLOG(KK)=XPLOG
         XINC=XINC+STEP
  312    CONTINUE
C
         NNINC=NINC
C
C  FIND MAXIMUM ON BEST DIRECTION THROUGH THE SPACE
C
         CALL RSORT(YGMP,NNINC,LLL)
         DO 37 K=1,NS
         XBETA(2,K)=YGAMMA(LLL(NNINC),K)
         XBETASV(2,K)=XBETA(2,K)
  37     CONTINUE
C
C  STORE GMP
C
         YYGMP(IIII)=YGMP(NNINC)
C
C  EXIT THE SEARCH IF NO IMPROVEMENT AFTER 3rd ITERATION
C
         IF(IIII.GE.3)THEN
            STOPPER=YYGMP(IIII)-YYGMP(IIII-1)
            IF(STOPPER.LE..0001)GO TO 929
         ENDIF
  999    CONTINUE
  929    CONTINUE
C
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
         GMPNOW=EXP(XPLOG/FLOAT(KRC))
         DO 766 K=1,NS
         SUM=0.0
         DO 664 JJJ=1,NEPCONG
         SUM=SUM+XMARK(JJJ,K)
 664     CONTINUE
         SUM=SUM/FLOAT(NEPCONG)
         IF((SAVEGMP-GMPNOW).GT..00001)THEN
             WRITE(outmsg,1003)NEP
             call rexit(outmsg)
         ENDIF
  766    CONTINUE
C
C  LOG-LIKELIHOOD OF LINEAR MODEL
C
         XPLOG1=XPLOG
         XPLOG2=XPLOG
         XPLOG3=XPLOG
C
      ENDIF
C      
C      
C
C ***********
C   QUADRATIC TERM 
C ***********
C
      IF(NNMODEL.GE.2.AND.NEPCONG.GE.6)THEN
         NMODEL=2
C
C  INITIALIZE QUADRATIC BETAS TO ZERO SO STARTING LOG-LIKELIHOOD
C    IS EQUAL TO THE THE ENDING LOG-LIKELIHOOD OF LINEAR
C    MODEL
C
         DO 71 K=1,NS
         XBETA(3,K)=0.0
  71     CONTINUE
C
         DO 998 IIII=1,10
C   
         DO 72 K=1,NS
         XBETASV(3,K)=XBETA(3,K)
         XXX(K)=0.0
  72     CONTINUE
C
         DO 73 K=1,NS
         XDERV(K)=0.0
         XDERV1(K)=0.0
         XDERV2(K)=0.0
         XDERV3(K)=0.0
  73     CONTINUE
C
C  GET DERIVATIVES
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETASV,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
C  SAVE INITIAL LOG-LIKELIHOOD
C
         IF(IIII.EQ.1)THEN
            SAVEGMP=EXP(XPLOG/FLOAT(KRC))
         ENDIF
C
         GMP=EXP(XPLOG/FLOAT(KRC))
         ASUMA=0.0
         DO 74 K=1,NS
         XXX(K)=XDERV2(K)/FLOAT(KRC)
         ASUMA=ASUMA+XXX(K)**2
  74     CONTINUE
C
C  CALCULATE STEPSIZE IN .01 UNITS
C
         STEP=.01/SQRT(ASUMA)
C      
C
C  SEARCH ALONG VECTOR IN BEST DIRECTION
C
C
         XINC=0.0
C 
         DO 712 KK=1,NINC
         SUM=0.0
         DO 75 K=1,NS
C
C  *****
C   CHECK FOR SIGN OF DERIVATIVE FOR GRADIENT!!!!!!!!
C  *****
C
         XBETA(3,K)=XBETASV(3,K)-XINC*XXX(K)
C
  75     CONTINUE
C
         DO 76 K=1,NS
         XDERV(K)=0.0
         XDERV1(K)=0.0
         XDERV2(K)=0.0
         XDERV3(K)=0.0
  76     CONTINUE
C
C  GET DERIVATIVES
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
         GMP=EXP(XPLOG/FLOAT(KRC))
C
         YGMP(KK)=GMP
         LLL(KK)=KK
         DO 77 K=1,NS
         YGAMMA(KK,K)=XBETA(3,K)
         XXXSAVE(KK,K)=XXX(K)
  77     CONTINUE
         YLOG(KK)=XPLOG
         XINC=XINC+STEP
  712    CONTINUE
C
         NNINC=NINC
C
C  FIND MAXIMUM ON BEST DIRECTION THROUGH THE SPACE
C
         CALL RSORT(YGMP,NNINC,LLL)
         DO 78 K=1,NS
         XBETA(3,K)=YGAMMA(LLL(NNINC),K)
         XBETASV(3,K)=XBETA(3,K)
  78     CONTINUE
C
C  STORE GMP
C
         YYGMP(IIII)=YGMP(NNINC)
C
C  EXIT THE SEARCH IF NO IMPROVEMENT AFTER 3rd ITERATION
C
         IF(IIII.GE.3)THEN
            STOPPER=YYGMP(IIII)-YYGMP(IIII-1)
            IF(STOPPER.LE..0001)GO TO 979
         ENDIF
  998    CONTINUE
  979    CONTINUE
C
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
         GMPNOW=EXP(XPLOG/FLOAT(KRC))
         DO 79 K=1,NS
         SUM=0.0
         DO 764 JJJ=1,NEPCONG
         SUM=SUM+XMARK(JJJ,K)
 764     CONTINUE
         SUM=SUM/FLOAT(NEPCONG)
         IF((SAVEGMP-GMPNOW).GT..00001)THEN
             WRITE(outmsg,1003)NEP
             call rexit(outmsg)
         ENDIF
   79    CONTINUE
C
C  LOG-LIKELIHOOD OF QUADRATIC MODEL
C
         XPLOG2=XPLOG
         XPLOG3=XPLOG
C
      ENDIF
C      
C      
C
C ***********
C   CUBIC TERM 
C ***********
C
      IF(NNMODEL.GE.3.AND.NEPCONG.GE.7)THEN
         NMODEL=3
C
C  INITIALIZE CUBIC BETAS TO ZERO SO STARTING LOG-LIKELIHOOD
C    IS EQUAL TO THE THE ENDING LOG-LIKELIHOOD OF QUADRATIC
C    MODEL
C
         DO 81 K=1,NS
         XBETA(4,K)=0.0
  81     CONTINUE
C
         DO 997 IIII=1,10
C   
         DO 82 K=1,NS
         XBETASV(4,K)=XBETA(4,K)
         XXX(K)=0.0
  82     CONTINUE
C
         DO 83 K=1,NS
         XDERV(K)=0.0
         XDERV1(K)=0.0
         XDERV2(K)=0.0
         XDERV3(K)=0.0
  83     CONTINUE
C
C  GET DERIVATIVES
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETASV,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
C  SAVE INITIAL LOG-LIKELIHOOD
C
         IF(IIII.EQ.1)THEN
            SAVEGMP=EXP(XPLOG/FLOAT(KRC))
         ENDIF
C
         GMP=EXP(XPLOG/FLOAT(KRC))
         ASUMA=0.0
         DO 84 K=1,NS
         XXX(K)=XDERV3(K)/FLOAT(KRC)
         ASUMA=ASUMA+XXX(K)**2
  84     CONTINUE
C
C  CALCULATE STEPSIZE IN .01 UNITS
C
         STEP=.01/SQRT(ASUMA)
C      
C
C  SEARCH ALONG VECTOR IN BEST DIRECTION
C
C
         XINC=0.0
C 
         DO 812 KK=1,NINC
         SUM=0.0
         DO 85 K=1,NS
C
C  *****
C   CHECK FOR SIGN OF DERIVATIVE FOR GRADIENT!!!!!!!!
C  *****
C
         XBETA(4,K)=XBETASV(4,K)-XINC*XXX(K)
C
  85     CONTINUE
C
         DO 86 K=1,NS
         XDERV(K)=0.0
         XDERV1(K)=0.0
         XDERV2(K)=0.0
         XDERV3(K)=0.0
  86     CONTINUE
C
C  GET DERIVATIVES
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
         GMP=EXP(XPLOG/FLOAT(KRC))
C
         YGMP(KK)=GMP
         LLL(KK)=KK
         DO 87 K=1,NS
         YGAMMA(KK,K)=XBETA(4,K)
         XXXSAVE(KK,K)=XXX(K)
  87     CONTINUE
         YLOG(KK)=XPLOG
         XINC=XINC+STEP
  812    CONTINUE
C
         NNINC=NINC
C
C  FIND MAXIMUM ON BEST DIRECTION THROUGH THE SPACE
C
         CALL RSORT(YGMP,NNINC,LLL)
         DO 88 K=1,NS
         XBETA(4,K)=YGAMMA(LLL(NNINC),K)
         XBETASV(4,K)=XBETA(4,K)
  88     CONTINUE
C
C  STORE GMP
C
         YYGMP(IIII)=YGMP(NNINC)
C
C  EXIT THE SEARCH IF NO IMPROVEMENT AFTER 3rd ITERATION
C
         IF(IIII.GE.3)THEN
            STOPPER=YYGMP(IIII)-YYGMP(IIII-1)
            IF(STOPPER.LE..0001)GO TO 989
         ENDIF
  997    CONTINUE
  989    CONTINUE
C
C
         CALL PROX(NEP,KRC,ATIME,XMARK,XPLOG,
     C           XDERV,XDERV1,XDERV2,XDERV3,
     C           XBETA,OUTX0,OUTX1,OUTX2,OUTX3,
     C           NFIRST,NLAST,
     C           ZDF,NDEVIT,XDEVIT,NMODEL)
C
         GMPNOW=EXP(XPLOG/FLOAT(KRC))
         DO 89 K=1,NS
         SUM=0.0
         DO 864 JJJ=1,NEPCONG
         SUM=SUM+XMARK(JJJ,K)
 864     CONTINUE
         SUM=SUM/FLOAT(NEPCONG)
         IF((SAVEGMP-GMPNOW).GT..00001)THEN
             WRITE(outmsg,1003)NEP
             call rexit(outmsg)
         ENDIF
   89    CONTINUE
C
C  LOG-LIKELIHOOD OF CUBIC MODEL
C
         XPLOG3=XPLOG
C
      ENDIF
C      
C
      KXTOT=KRC
C
C  INVERT OUTER PRODUCT MATRIX TO GET STANDARD ERRORS
C
C    ***CONSTANT MODEL***
C
C     Why did I choose size 26*99 for array WORK? I copied this from old
C     code of mine and I honestly don't remember
      LWORK = 26*99
      CALL SSYEV('V', 'U', NS, OUTX0, 99, WVEC, WORK, LWORK, INFO)
      ZMAT = OUTX0
C
C  (X'X)-1
C
      DO 60 I=1,NS
      DERVISH(1,I)=XDERV(I)/FLOAT(KRC)
      DERVISH(2,I)=XDERV1(I)/FLOAT(KRC)
      DERVISH(3,I)=XDERV2(I)/FLOAT(KRC)
      DERVISH(4,I)=XDERV3(I)/FLOAT(KRC)
      DO 60 K=1,NS
      SUM=0.0
      DO 62 J=1,NS
      IF(ABS(WVEC(NS+1-J)).GT..0001)THEN
          SUM=SUM+ZMAT(K,NS+1-J)*(1.0/WVEC(NS+1-J))*ZMAT(I,NS+1-J)
      ENDIF
  62  CONTINUE
  60  OUTX0(I,K)=SUM
C
C  ***LINEAR MODEL
C
      IF(NEPCONG.GE.5)THEN
         CALL SSYEV('V', 'U', 2*NDS, OUTX1, 99, WVEC, WORK, LWORK, INFO)
         ZMAT = OUTX1
C
C  (X'X)-1
C
         DO 63 I=1,2*NS
         DO 63 K=1,2*NS
         SUM=0.0
         DO 64 J=1,2*NS
         IF(ABS(WVEC(2*NS+1-J)).GT..0001)THEN
             SUM=SUM+ZMAT(K,2*NS+1-J)*
     C           (1.0/WVEC(2*NS+1-J))*ZMAT(I,2*NS+1-J)
         ENDIF
  64     CONTINUE
  63     OUTX1(I,K)=SUM
      ENDIF
C
C  ***QUADRATIC MODEL
C
      IF(NEPCONG.GE.6)THEN
         CALL SSYEV('V', 'U', 3*NDS, OUTX2, 99, WVEC, WORK, LWORK, INFO)
         ZMAT = OUTX2
C
C  (X'X)-1
C
         DO 65 I=1,3*NS
         DO 65 K=1,3*NS
         SUM=0.0
         DO 66 J=1,3*NS
         IF(ABS(WVEC(3*NS+1-J)).GT..0001)THEN
             SUM=SUM+ZMAT(K,3*NS+1-J)*
     C           (1.0/WVEC(3*NS+1-J))*ZMAT(I,3*NS+1-J)
         ENDIF
  66     CONTINUE
  65     OUTX2(I,K)=SUM
      ENDIF
C
C  ***CUBIC MODEL
C
      IF(NEPCONG.GE.7)THEN
         CALL SSYEV('V', 'U', 4*NS, OUTX3, 99, WVEC, WORK, LWORK, INFO)
         ZMAT = OUTX3
C
C  (X'X)-1
C
         DO 67 I=1,4*NS
         DO 67 K=1,4*NS
         SUM=0.0
         DO 68 J=1,4*NS
         IF(ABS(WVEC(4*NS+1-J)).GT..0001)THEN
             SUM=SUM+ZMAT(K,4*NS+1-J)*
     C           (1.0/WVEC(4*NS+1-J))*ZMAT(I,4*NS+1-J)
         ENDIF
  68     CONTINUE
  67     OUTX3(I,K)=SUM
      ENDIF
C
      RETURN
      END
C
C  **************************************************************************
C    SUBROUTINE REGA---CALLED BY REG2.  PERFORMS THE REGRESSION:
C
C         [W'W]-1*W'[X(I,NY) - c']
C
C      TO GET THE ROW ENTRIES OF P.
C  **************************************************************************
C
      SUBROUTINE REGA(NS,NF,A,Y,V)
      DIMENSION A(152,127),Y(152),B(127,127),C(127,127),V(152),
     C     BB(127,152),ZMAT(127,127),WVEC(127),
     C     WORK(26*127)
      COMMON /AREA1/ JR,LR
  101 FORMAT(' PERFORMANCE INDEX EIGENVALUE/VECTOR ROUTINE=',3I5)
  102 FORMAT(2I5,30F10.4)
C
C  X'X
C
      DO 7 J=1,152
      V(J)=0.0
  7   CONTINUE
      DO 1 J=1,NF
      DO 1 JJ=1,NF
      SUM=0.0
      DO 2 I=1,NS
  2   SUM=SUM+A(I,J)*A(I,JJ)
  1   B(J,JJ)=SUM
      LWORK = 26*127
      CALL SSYEV('V', 'U', NF, B, 127, WVEC, WORK, LWORK, INFO)
      ZMAT = B
C
C  (X'X)-1
C
      DO 60 I=1,NF
      DO 60 K=1,NF
      SUM=0.0
      DO 61 J=1,NF
      IF(ABS(WVEC(J)).GT..01)THEN
          SUM=SUM+ZMAT(K,J)*(1.0/WVEC(J))*ZMAT(I,J)
      ENDIF
  61  CONTINUE
  60  C(I,K)=SUM
C
C  (X'X)-1X'
C
      DO 3 I=1,NS
      DO 3 J=1,NF
      SUM=0.0
      DO 4 JJ=1,NF
  4   SUM=SUM+C(J,JJ)*A(I,JJ)
  3   BB(J,I)=SUM
C
C  BETA = (X'X)-1X'Y
C
      DO 5 JJ=1,NF
      SUM=0.0
      DO 6 J=1,NS
  6   SUM=SUM+BB(JJ,J)*Y(J)
  5   V(JJ)=SUM
      RETURN
      END
C
C  ***************************************************************************
C    SUBROUTINE PROLLC2--CALCULATES DERIVATIVES AND LOG-LIKELIHOODS FOR THE
C                       ROLL CALL PARAMETERS FOR THE NOMINATE PROBIT   
C                       MODEL      
C  ***************************************************************************
C
      SUBROUTINE PROLLC2(IICONG,NEQ,NPC,KTOT,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERV,ZDERV,
     C               ZDF,NDEVIT,XDEVIT)
      use xxcom_mod
      use mine_mod
      DIMENSION ZDF(150000,4),DYES(99),DNO(99),DYES1(99),DNO1(99),
     C          DCC(99),DBB(99),DCC1(99),DBB1(99),ZDERV(99),
     C          DDERV(99),OLDZ(99),OLDD(99)
  100 FORMAT(8F7.3)
 1001 FORMAT(I4,F5.2,F10.7)
 1002 FORMAT(' R-C ',I3,3I5,F7.3,F13.5,2F10.4,2F7.3)
C
      XPLOG=0.0
      KLASS=0
      KLASSYY=0
      KLASSNY=0
      KLASSYN=0
      KLASSNN=0
      KLASS2=0
      DO 8 K=1,NS
      ZDERV(K)=0.0
      DDERV(K)=0.0
  8   CONTINUE
C
      KTOT=0
      DO 2 I=1,NPC
      DO 3 K=1,NS
      DYES(K)=0.0
      DNO(K)=0.0
      DYES1(K)=0.0
      DNO1(K)=0.0
      DYES(K)=(XDATA(I+KTOTP,K)-OLDZ(K)+OLDD(K))**2
      DNO(K) =(XDATA(I+KTOTP,K)-OLDZ(K)-OLDD(K))**2
      DYES1(K)=(XDATA(I+KTOTP,K)-OLDZ(K)+OLDD(K))
      DNO1(K) =(XDATA(I+KTOTP,K)-OLDZ(K)-OLDD(K))
  3   CONTINUE
C
C  IF NOT MISSING DATA
C
      IF(RCVOTE9(I+KTOTP,NEQ).EQV..FALSE.)THEN
         KTOT=KTOT+1
C
C  IF YES
C
         IF(RCVOTE1(I+KTOTP,NEQ).EQV..TRUE.)THEN
            DC=0.0
            DB=0.0
            DO 4 K=1,NS
            DC=DC+(-WEIGHT(K)*WEIGHT(K)*DYES(K))
            DB=DB+(-WEIGHT(K)*WEIGHT(K)*DNO(K))
            DCC(K)=DYES(K)
            DBB(K)=DNO(K)
            DCC1(K)=DYES1(K)*WEIGHT(K)*WEIGHT(K)
            DBB1(K)=DNO1(K)*WEIGHT(K)*WEIGHT(K)
  4         CONTINUE
            XCC=+1.0
         ENDIF
C
C  IF NO
C
         IF(RCVOTE1(I+KTOTP,NEQ).EQV..FALSE.)THEN
            DC=0.0
            DB=0.0
            DO 5 K=1,NS
            DC=DC+(-WEIGHT(K)*WEIGHT(K)*DNO(K))
            DB=DB+(-WEIGHT(K)*WEIGHT(K)*DYES(K))
            DCC(K)=DNO(K)
            DBB(K)=DYES(K)
            DCC1(K)=DNO1(K)*WEIGHT(K)*WEIGHT(K)
            DBB1(K)=DYES1(K)*WEIGHT(K)*WEIGHT(K)
  5         CONTINUE
            XCC=-1.0
         ENDIF
C
         ZS=WEIGHT(NS+1)*(EXP(DC)-EXP(DB))
C
         IF(ABS(DC).LE.ABS(DB))KLASS=KLASS+1
         IF(ABS(DC).LE.ABS(DB).AND.XCC.EQ.+1.0)THEN
            KLASSYY=KLASSYY+1
         ENDIF
         IF(ABS(DC).GT.ABS(DB).AND.XCC.EQ.+1.0)THEN
            KLASSNY=KLASSNY+1
         ENDIF
         IF(ABS(DC).GT.ABS(DB).AND.XCC.EQ.-1.0)THEN
            KLASSYN=KLASSYN+1
         ENDIF
         IF(ABS(DC).LE.ABS(DB).AND.XCC.EQ.-1.0)THEN
            KLASSNN=KLASSNN+1
         ENDIF
         IF(ZS.GT.0.0)KLASS2=KLASS2+1
C
         WWIMJ=ZS*XDEVIT
         KWIMJ=IFIX(ABS(WWIMJ)+.5)
         IF(KWIMJ.GT.NDEVIT-2)KWIMJ=NDEVIT-2
         IF(ZS.GE.0.0)THEN
            ZDISTF=ZDF(NDEVIT-1+KWIMJ+1,2)
            CDFLOG=ZDF(NDEVIT-1+KWIMJ+1,3)
         ENDIF
         IF(ZS.LT.0.0)THEN
            ZDISTF=ZDF(NDEVIT-KWIMJ,2)
            CDFLOG=ZDF(NDEVIT-KWIMJ,3)
         ENDIF
         XPLOG=XPLOG+CDFLOG
         ZGAUSS=EXP(-(ZS*ZS)/2.0)
         DO 6 K=1,NS
         ZDERV(K)=ZDERV(K)+(ZGAUSS/ZDISTF)*
     C            (-DCC1(K)*EXP(DC)+DBB1(K)*EXP(DB))
         DDERV(K)=DDERV(K)+XCC*(ZGAUSS/ZDISTF)*
     C            ( DCC1(K)*EXP(DC)+DBB1(K)*EXP(DB))
  6      CONTINUE
      ENDIF
  2   CONTINUE
      GMP=EXP(XPLOG/FLOAT(KTOT))
      RETURN
      END
C
C  ***************************************************************************
C    SUBROUTINE RCINT2---FINDS ROLL CALL PARAMETERS
C  ***************************************************************************
C
      SUBROUTINE RCINT2(IICONG,NEQ,NPC,NQC,KRC,KTOTP,KTOTQ,
     C                             XPLOG,OLDZ,OLDD,
     C                             ZDF,NDEVIT,XDEVIT)
      use xxcom_mod
      use mine_mod
      DIMENSION ZDF(150000,4),DDERVX(99),ZDERVX(99),OLDZ(99),
     C          OLDD(99),DZSAVE(66),XXX(66),YGAMMA(50,66),
     C          XXXSAVE(50,66),YLOG(50),LLL(50),YGMP(50),
     C          YYGMP(50)
  203 FORMAT(I5,2I4,8X,I2,F12.5,20F10.4)
  205 FORMAT(I6,2I5,5X,F12.5,20F10.4)
  206 FORMAT(I5,4I4,I2,F12.5,20F10.4)
 1001 FORMAT(' RCS',4I5,F7.3,5I4,20F7.3)
 1002 FORMAT(I4,I5,2F12.5,20F7.3)
 1003 FORMAT(' FATAL ERROR ROLL CALL PHASE',2I5)
 1004 FORMAT(I4,I5,F12.5,12X,20F7.3)
C
C  INITIALIZE LOG-LIKELIHOOD
C
      CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
C  CALCULATE NEW ROLL CALL COORDINATES WITH A 
C      SIMPLE GRID SEARCH
C
      NINC=25
      DO 9999 JJJJ=1,5
C
C ***********
C   SPREAD TERM FIRST
C ***********
C
      DO 99 IIII=1,10
C
C  ERROR CATCH
C
      DO 2 K=1,NS
      DZSAVE(K)=OLDZ(K)
      IF(ABS(OLDD(K)).LE..001)OLDD(K)=.03
      DZSAVE(K+NS)=OLDD(K)
      XXX(K)=0.0
  2   CONTINUE
C
      DO 40 K=1,NS
      DDERVX(K)=0.0
      ZDERVX(K)=0.0
  40  CONTINUE
C
C  GET DERIVATIVES
C
      CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
C  SAVE INITIAL LOG-LIKELIHOOD
C
      IF(IIII.EQ.1)THEN
         SAVEGMP=EXP(XPLOG/FLOAT(KRC))
      ENDIF
C
      GMP=EXP(XPLOG/FLOAT(KRC))
      SUMA=0.0
      SUMB=0.0
      KCATCH=0
      DO 61 K=1,NS
      XXX(K)=ZDERVX(K)/FLOAT(KRC)
      XXX(K+NS)=DDERVX(K)/FLOAT(KRC)
      SUMA=SUMA+XXX(K)**2
      SUMB=SUMB+XXX(K+NS)**2
      IF(ABS(XXX(K+NS)).LE..0001)KCATCH=KCATCH+1
  61  CONTINUE
C
C  ERROR CATCH IF ZERO DERIVATIVE
C
      KEXIT=0
      IF(KCATCH.EQ.NS)THEN
         DO 661 K=1,NS
         DZSAVE(K+NS)=OLDD(K)
         KEXIT=1
  661    CONTINUE
      ENDIF
      IF(KEXIT.EQ.1)GO TO 919
C
C  CALCULATE STEPSIZE IN .01 UNITS
C
      STEPZ=.01/SQRT(SUMA)
      STEPD=.01/SQRT(SUMB)
C      
C
C
C  SEARCH ALONG VECTOR IN BEST DIRECTION
C
C
      XINCZ=0.0
      XINCD=0.0
C
      DO 212 KK=1,NINC
      SUM=0.0
      DO 24 K=1,NS
C
C  *****
C   CHECK FOR SIGN OF DERIVATIVE FOR GRADIENT!!!!!!!!
C  *****
C
      OLDD(K)=DZSAVE(K+NS)-XINCD*XXX(K+NS)
C
  24  CONTINUE
      DO 240 K=1,NS
      DDERVX(K)=0.0
      ZDERVX(K)=0.0
  240 CONTINUE
C
C  GET DERIVATIVES
C
      CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
      GMP=EXP(XPLOG/FLOAT(KRC))
C
      YGMP(KK)=GMP
      LLL(KK)=KK
      DO 222 K=1,NS
      YGAMMA(KK,K)=OLDZ(K)
      YGAMMA(KK,K+NS)=OLDD(K)
      XXXSAVE(KK,K)=XXX(K)
      XXXSAVE(KK,K+NS)=XXX(K+NS)
  222 CONTINUE
      YLOG(KK)=XPLOG
C
      XINCD=XINCD+STEPD
  212 CONTINUE
C
      NNINC=NINC
C
C  FIND MAXIMUM ON BEST DIRECTION THROUGH THE SPACE
C
      CALL RSORT(YGMP,NNINC,LLL)
      KEXIT=0
      DO 224 K=1,NS
      OLDZ(K)=YGAMMA(LLL(NNINC),K)
      OLDD(K)=YGAMMA(LLL(NNINC),K+NS)
      DZSAVE(K)=OLDZ(K)
      DZSAVE(K+NS)=OLDD(K)
      IF(ABS(OLDD(K)).LE..001)THEN
         OLDD(K)=.03
         DZSAVE(K+NS)=OLDD(K)
         KEXIT=1
      ENDIF
  224 CONTINUE
C
C  ERROR CATCH FOR ZERO DISTANCE
C
      IF(KEXIT.EQ.1)GO TO 919
C
C  STORE GMP
C
      YYGMP(IIII)=YGMP(NNINC)
C
C  EXIT THE SEARCH IF NO IMPROVEMENT AFTER 3rd ITERATION
C
      IF(IIII.GE.3)THEN
         STOPPER=YYGMP(IIII)-YYGMP(IIII-1)
         IF(STOPPER.LE..0001)GO TO 919
      ENDIF
  99  CONTINUE
  919 CONTINUE
C
C
      CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
      GMPNOW=EXP(XPLOG/FLOAT(KRC))
      IF((SAVEGMP-GMPNOW).GT..00001)THEN
C          WRITE(*,1003)IICONG,NEQ
C          STOP
      ENDIF
C
C
C ***********
C   MIDPOINT SECOND
C ***********
C
      DO 98 IIII=1,10
C
      DO 32 K=1,NS
      DZSAVE(K)=OLDZ(K)
      DZSAVE(K+NS)=OLDD(K)
      XXX(K)=0.0
  32  CONTINUE
C
      DO 33 K=1,NS
      DDERVX(K)=0.0
      ZDERVX(K)=0.0
  33  CONTINUE
C
C  GET DERIVATIVES
C
      CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
C  SAVE INITIAL LOG-LIKELIHOOD
C
      IF(IIII.EQ.1)THEN
         SAVEGMP=EXP(XPLOG/FLOAT(KRC))
      ENDIF
C
      GMP=EXP(XPLOG/FLOAT(KRC))
      SUMA=0.0
      SUMB=0.0
      DO 34 K=1,NS
      XXX(K)=ZDERVX(K)/FLOAT(KRC)
      XXX(K+NS)=DDERVX(K)/FLOAT(KRC)
      SUMA=SUMA+XXX(K)**2
      SUMB=SUMB+XXX(K+NS)**2
  34  CONTINUE
C
C
C  CALCULATE STEPSIZE IN .01 UNITS
C
      STEPZ=.01/SQRT(SUMA)
      STEPD=.01/SQRT(SUMB)
C      
C
C      IF(IICONG.EQ.45.AND.NEQ.EQ.20)THEN
C      IPHASE=1
C      ENDIF
C
C
C  SEARCH ALONG VECTOR IN BEST DIRECTION
C
C
      XINCZ=0.0
      XINCD=0.0
C
      DO 213 KK=1,NINC
      SUM=0.0
      DO 35 K=1,NS
C
C  *****
C   CHECK FOR SIGN OF DERIVATIVE FOR GRADIENT!!!!!!!!
C  *****
C
      OLDZ(K)=DZSAVE(K)-XINCZ*XXX(K)
C
      SUM=SUM+OLDZ(K)**2
  35  CONTINUE
C
C  CHECK TO SEE IF ROLL CALL MIDPOINT OUTSIDE UNIT HYPERSPHERE
C
      IF(SUM.GT.1.0)THEN
C
C  RESET LEGISLATOR POINT TO SURFACE OF UNIT HYPERSPHERE AND
C    CALCULATE DERIVATIVES AND LOG-LIKELIHOODS.  THEN EXIT
C    SEARCH LOOP
C
         DO 36 K=1,NS
         OLDZ(K)=OLDZ(K)/SQRT(SUM)
  36     CONTINUE
         DO 37 K=1,NS
         DDERVX(K)=0.0
         ZDERVX(K)=0.0
  37     CONTINUE
         CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
         GMP=EXP(XPLOG/FLOAT(KRC))
C
         YGMP(KK)=GMP
         LLL(KK)=KK
         DO 38 K=1,NS
         YGAMMA(KK,K)=OLDZ(K)
         YGAMMA(KK,K+NS)=OLDD(K)
         XXXSAVE(KK,K)=XXX(K)
         XXXSAVE(KK,K+NS)=XXX(K+NS)
  38     CONTINUE
         YLOG(KK)=XPLOG
         GO TO 2113
      ENDIF
      DO 39 K=1,NS
      DDERVX(K)=0.0
      ZDERVX(K)=0.0
  39  CONTINUE
C
C  GET DERIVATIVES
C
      CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
      GMP=EXP(XPLOG/FLOAT(KRC))
C
      YGMP(KK)=GMP
      LLL(KK)=KK
      DO 41 K=1,NS
      YGAMMA(KK,K)=OLDZ(K)
      YGAMMA(KK,K+NS)=OLDD(K)
      XXXSAVE(KK,K)=XXX(K)
      XXXSAVE(KK,K+NS)=XXX(K+NS)
  41  CONTINUE
      YLOG(KK)=XPLOG
C
C      IF(IICONG.EQ.45.AND.NEQ.EQ.20)THEN
C      IPHASE=1
C      ENDIF
C
      XINCZ=XINCZ+STEPZ
  213 CONTINUE
C
      NNINC=NINC
      GO TO 2115
 2113 NNINC=KK
 2115 CONTINUE
C
C  FIND MAXIMUM ON BEST DIRECTION THROUGH THE SPACE
C
      CALL RSORT(YGMP,NNINC,LLL)
C      IF(IICONG.EQ.45.AND.NEQ.EQ.20)THEN
C      IPHASE=1
C      ENDIF
      DO 42 K=1,NS
      OLDZ(K)=YGAMMA(LLL(NNINC),K)
      OLDD(K)=YGAMMA(LLL(NNINC),K+NS)
      DZSAVE(K)=OLDZ(K)
      DZSAVE(K+NS)=OLDD(K)
  42  CONTINUE
C
C  STORE GMP
C
      YYGMP(IIII)=YGMP(NNINC)
C
C  EXIT THE SEARCH IF NO IMPROVEMENT AFTER 3rd ITERATION
C
      IF(IIII.GE.3)THEN
         STOPPER=YYGMP(IIII)-YYGMP(IIII-1)
         IF(STOPPER.LE..0001)GO TO 909
      ENDIF
  98  CONTINUE
  909 CONTINUE
C
C
      CALL PROLLC2(IICONG,NEQ,NPC,KRC,KTOTP,KTOTQ,
     C               XPLOG,OLDZ,OLDD,DDERVX,ZDERVX,
     C               ZDF,NDEVIT,XDEVIT)
C
      GMPNOW=EXP(XPLOG/FLOAT(KRC))
      IF((SAVEGMP-GMPNOW).GT..00001)THEN
C          WRITE(*,1003)IICONG,NEQ
C          STOP
      ENDIF
C
 9999 CONTINUE
      RETURN
      END
C
C
C  ************************************************************************
C    SORT SUBROUTINE--SORTS A VECTOR 'A' OF REAL ELEMENTS INTO ASCENDING
C    ORDER.  'LA' IS THE NUMBER OF ELEMENTS TO BE SORTED AND 'IR' IS A
C    VECTOR OF INTEGERS THAT RECORDS THE PERMUTATIONS--USUALLY SET TO
C    1,2,3,4,...
C  ************************************************************************
C
C
      SUBROUTINE RSORT(A,LA,IR)
      DIMENSION A(LA),IU(21),IL(21),IR(LA)
c$$$      if (LA.gt.size(A)) write(*,*)'LA:',LA,size(A)
      IF (LA.LE.0) RETURN
      M = 1
      I = 1
      J = LA
      R = .375
    5 IF (I.EQ.J) GO TO 45
      IF (R.GT..5898437) GO TO 10
      R = R+3.90625E-2
      GO TO 15 
   10 R = R-.21875 
   15 K = I 
C
C SELECT A CENTRAL ELEMENT OF THE  
C ARRAY AND SAVE IT IN LOCATION T  
C
      IJ = I+(J-I)*R
c$$$      if (IJ.ge.LA) write(*,*)'IJ:',IJ,LA
      T = A(IJ)   
      IT = IR(IJ) 
C
C FIRST ELEMENT OF ARRAY IS GREATER
C THAN T, INTERCHANGE WITH T       
C
c$$$      if (I.ge.size(A)) write(*,*)'I:',I,size(A),LA
      IF (A(I).LE.T) GO TO 20  
      A(IJ) = A(I) 
      A(I) = T     
      T = A(IJ) 
      IR(IJ) = IR(I)  
      IR(I) = IT
      IT = IR(IJ) 
   20 L = J
C
C IF LAST ELEMENT OF ARRAY IS LESS THAN
C T, INTERCHANGE WITH T
C
      IF (A(J).GE.T) GO TO 30
      A(IJ) = A(J)
      A(J) = T
      T = A(IJ)
      IR(IJ) = IR(J)
      IR(J) = IT
      IT = IR(IJ)
C
C IF FIRST ELEMENT OF ARRAY IS GREATER
C THAN T, INTERCHANGE WITH T
C
      IF (A(I).LE.T) GO TO 30
      A(IJ) = A(I)
      A(I) = T
      T = A(IJ)
      IR(IJ) = IR(I)
      IR(I) = IT
      IT = IR(IJ)
      GO TO 30
   25 IF (A(L).EQ.A(K)) GO TO 30
      TT = A(L)
      A(L) = A(K)
      A(K) = TT
      ITT = IR(L)
      IR(L) = IR(K)
      IR(K) = ITT
C
C FIND AN ELEMENT IN THE SECOND HALF OF
C THE ARRAY WHICH IS SMALLER THAN T
C
   30 L = L-1
      IF (A(L).GT.T) GO TO 30
C
C FIND AN ELEMENT IN THE FIRST HALF OF
C THE ARRAY WHICH IS GREATER THAN T
C
   35 K = K+1
      IF (A(K).LT.T) GO TO 35
C
C INTERCHANGE THESE ELEMENTS
C
      IF (K.LE.L) GO TO 25
C
C SAVE UPPER AND LOWER SUBSCRIPTS OF
C THE ARRAY YET TO BE SORTED
C
      IF (L-I.LE.J-K) GO TO 40
      IL(M) = I
      IU(M) = L
      I = K
      M = M+1
      GO TO 50
   40 IL(M) = K
      IU(M) = J
      J = L
      M = M+1
      GO TO 50
C
C BEGIN AGAIN ON ANOTHER PORTION OF
C THE UNSORTED ARRAY
C
   45 M = M-1
      IF (M.EQ.0) RETURN
      I = IL(M)
      J = IU(M)
c$$$      if (M.ge.size(IL)) write(*,*)'M(IL):',M,size(IL),21
c$$$      if (M.ge.size(IU)) write(*,*)'M(IU):',M,size(IU),21
   50 IF (J-I.GE.11) GO TO 15
      IF (I.EQ.1) GO TO 5
      I = I-1
   55 I = I+1
      IF (I.EQ.J) GO TO 45
      T = A(I+1)
      IT = IR(I+1)
      IF (A(I).LE.T) GO TO 55
      K = I
 60   A(K+1) = A(K)
c$$$      if (K.ge.size(IR)) write(*,*)'K(IR):',K,size(IR),LA
      IR(K+1) = IR(K)
      K = K-1
c$$$      if (K.ge.size(A)) write(*,*)'K(A):',K,size(A),LA
      IF (T.LT.A(K)) GO TO 60
      A(K+1) = T
c$$$      if (K+1.ge.size(IR)) write(*,*)'K+1(IR):',K+1,size(IR),LA
      IR(K+1) = IT
      GO TO 55
      END
C
C
C *********************************************************************
C   SUBROUTINE CUTPLANE -- FINDS CUTTING LINE USING THE CUTTING 
C                            PLANE PROCEDURE
C *********************************************************************
C
C
      SUBROUTINE CUTPLANE(JJJ,NP,NRCALL,NS,XMAT,ZVEC,WS,
     C                      MCUTS,LERROR,IFIXX,KTT,KT,LDATA)
      DIMENSION XMAT(1001,25),ZVEC(2901,25),XPROJ(1001,2901),XXY(2901),
     C          LLL(2901),XXX(2901),MM(2901),MVOTE(2901),WS(5802),
     C          LLV(2901),LLVB(2901),LLE(2901),LLEB(2901),
     C          LERROR(1001,2901),ZS(2901),MCUTS(2901,2),
     C          XJCH(25),XJEH(25),XJCL(25),XJEL(25),
     C          LDATA(1001,2901)
  100 FORMAT(5I5)      
 1093 FORMAT(' CLASSIFICATION CHECK  ',I3,I8)
 1094 FORMAT(' RC  CLASSIFICATION ERROR  ',2I3,2I8,2F10.5)
C
C   ESTIMATE PROJECTION VECTORS
C  
      KT=0
      KTT=0
      KTTSAVE=0
      KTSAVE=0
      KCHECK=0
      DO 93 JX=1,NRCALL
C
C  GET YES AND NO COUNTS
C
      KYES=0
      KNO=0
      DO 92 I=1,NP
      IF(LDATA(I,JX).EQ.1)KYES=KYES+1
      IF(LDATA(I,JX).EQ.6)KNO=KNO+1
  92  CONTINUE
C
C
      DO 89 I=1,NP
      SUM=0.0
      DO 90 K=1,NS
      SUM=SUM+XMAT(I,K)*ZVEC(JX,K)
  90  CONTINUE
C
C  SAVE PROJECTION VECTORS -- LEGISLATOR BY ROLL CALL MATRIX
C
      XPROJ(I,JX)=SUM
      XXY(I)=SUM
      LLL(I)=I
      XXX(I)=SUM
      MM(I)=LDATA(I,JX)
      IF(LDATA(I,JX).EQ.0)MM(I)=9
  89  CONTINUE
C
C  SORT PROJECTION VECTOR (Y-HAT)
C
      CALL RSORT(XXX,NP,LLL)
      DO 114 I=1,NP
      MVOTE(I)=MM(LLL(I))
  114 CONTINUE
C
C  
C  CALCULATE CLASSIFICATION ERRORS OF PROJECTION ONTO NORMAL VECTOR
C  
C
      JCH=0
      JEH=0
      JCL=0
      JEL=0
      IROTC=0
      CALL JAN1PT(NP,NRCALL,NS,JX,XMAT,XXX,MVOTE,WS,
     C            LLV,LLVB,LLE,LLEB,LERROR,
     C            ZS,JCH,JEH,JCL,JEL,IROTC,KCUT,LCUT,LLL,
     C            XJCH,XJEH,XJCL,XJEL)
C
      IF(JEH+JEL.EQ.0)THEN
         KT=KT+JCH+JEH+JCL+JEL
         KTSAVE=KTSAVE+JCH+JEH+JCL+JEL
         IJUST=0
         GO TO 9377
      ENDIF
      IF(IFIXX.EQ.0)THEN
         KT=KT+JCH+JEH+JCL+JEL
         KTSAVE=KTSAVE+JCH+JEH+JCL+JEL
         KTTSAVE=KTTSAVE+JEH+JEL
         GO TO 9377
      ENDIF
C
C  SET-UP FOR GRID SEARCH FOR BEST CUTTING LINE 
C
      NCUT=25
      CALL SEARCH(IIII,JX,NCUT,NS,NP,NRCALL,KCUT,LCUT,KTT,KT,
     C            XMAT,ZVEC,XPROJ,WS,XXY,
     C            KITTY1,KITTY2,KYES,KNO,LDATA)
C
C
      KTTSAVE=KTTSAVE+KITTY1
      KTSAVE=KTSAVE+KITTY2
C
 9377 CONTINUE
C
C  STORE DIRECTIONALITY OF ROLL CALL
C
      MCUTS(JX,1)=KCUT
      MCUTS(JX,2)=LCUT
C
C
C  LOCATE ERRORS -- WS(.) CONTAINS THE OPTIMAL CUTTING POINT ON THE
C                   PROJECTION VECTOR -- IT CAN BE USED TO CALCULATE THE
C                   CLASSIFICATION ERRORS
C
      KSUM=0
      KSUMYES=0
      KSUMNO=0
      SUMYES=0.0
      SUMNO=0.0
      DO 108 I=1,NP
      LERROR(I,JX)=0
      XXX(I)=XXY(I)
      LLL(I)=I
      IF(LDATA(I,JX).EQ.0)GO TO 108
      IF(LDATA(I,JX).EQ.1)THEN
         SUMYES=SUMYES+XXY(I)
         KSUMYES=KSUMYES+1
      ENDIF
      IF(LDATA(I,JX).EQ.6)THEN
         SUMNO=SUMNO+XXY(I)
         KSUMNO=KSUMNO+1
      ENDIF
      IF(XXY(I).LT.WS(JX))THEN
         IF(LDATA(I,JX).NE.KCUT)THEN
            LERROR(I,JX)=1
            KCHECK=KCHECK+1
            KSUM=KSUM+1
         ENDIF
      ENDIF
      IF(XXY(I).GT.WS(JX))THEN
         IF(LDATA(I,JX).NE.LCUT)THEN
            LERROR(I,JX)=1
            KCHECK=KCHECK+1
            KSUM=KSUM+1
         ENDIF
      ENDIF
  108 CONTINUE
      SUMYES=SUMYES/FLOAT(KSUMYES)
      SUMNO=SUMNO/FLOAT(KSUMNO)
      WS(2)=SUMYES
      WS(3)=SUMNO
C
  93  CONTINUE
C
      KT=KTSAVE
      KTT=KTTSAVE
      KCHECK2=0
      DO 281 JX=1,NRCALL
      DO 282 I=1,NP
      KCHECK2=KCHECK2+LERROR(I,JX)
  282 CONTINUE
  281 CONTINUE
      IF(KT.GT.0)THEN
        XERROR=FLOAT(KTT)/FLOAT(KT)
        YERROR=1.0-XERROR
      ENDIF
C      WRITE(* ,1094)JJJ,NS,KTT,KT,XERROR,YERROR
      RETURN
      END
C
C  ************************************************************************
C    SUBROUTINE SEARCH
C  ************************************************************************
C
      SUBROUTINE SEARCH(IIII,JX,NCUT,NS,NP,NRCALL,KCUT,LCUT,KTT,KT,
     C                  XMAT,ZVEC,XPROJ,WS,XXY,
     C                  KITTY1,KITTY2,KYES,KNO,LDATA)
      DIMENSION XMAT(1001,25),ZVEC(2901,25),
     C          XPROJ(1001,2901),WS(5802),XXY(1001),XXX(2901),MM(1001),
     C          LLL(2901),MVOTE(2901),LLM(1001),LLN(1001),
     C          KKKCUT(1001),LLLCUT(1001),LLV(2901),
     C          LLVB(2901),LLE(2901),LLEB(2901),LERROR(1001,2901),
     C          XJCH(25),XJEH(25),XJCL(25),XJEL(25),
     C          ZS(2901),UUU(1001,25),Y16MIDP(1001,25),
     C          FV1(1001),FV2(1001),SUMX(1001),X16MIDP(1001,25),
     C          YHAT(61913),LWRONG(1001),
     C          VVV(25,25),LDATA(1001,2901),WORK(3*NS*NS+1001)
  210 FORMAT(I5,10F12.3)
 1091 FORMAT(' INVERSE MATRIX ERROR',I4,I5,I8,2F10.4)
 1099 FORMAT(I3,I5,I3,2I4)
 1103 FORMAT(' MIDPOINT DECOMPOSITION',5I6)
 1212 FORMAT(I3,I5,7I4)
 3909 FORMAT(I5,I3,6I4,2I8,5I5)
C
      DO 1 I=1,50
      SUMX(I)=0.0
  1   CONTINUE
C
C  PHASE 2
C
C      NCUT2=20
C
      DO 999 IJL=1,NCUT
C
C  SET-UP FOR PHASE 2
C
C
      DO 388 K=1,NS
      UUU(IJL,K)=ZVEC(JX,K)
  388 CONTINUE
      DO 389 I=1,NP
      SUM=0.0
      DO 390 K=1,NS
c$$$         if (K.ge.size(IR)) write(*,*)'K(IR):',K,size(IR),LA
      SUM=SUM+XMAT(I,K)*ZVEC(JX,K)
  390 CONTINUE
C
C  SAVE PROJECTION VECTORS -- LEGISLATOR BY ROLL CALL MATRIX
C
      XPROJ(I,JX)=SUM
      XXY(I)=SUM
      LLL(I)=I
      XXX(I)=SUM
      MM(I)=LDATA(I,JX)
      IF(LDATA(I,JX).EQ.0)MM(I)=9
  389 CONTINUE
C
C  SORT PROJECTION VECTOR (Y-HAT)
C
C
      CALL RSORT(XXX,NP,LLL)
      DO 314 I=1,NP
      MVOTE(I)=MM(LLL(I))
  314 CONTINUE
C
C  
C  CALCULATE CLASSIFICATION ERRORS FOR BEST SOLUTION FROM PHASE 1
C  
C
      JCH=0
      JEH=0
      JCL=0
      JEL=0
      IROTC=0
      CALL JAN1PT(NP,NRCALL,NS,JX,XMAT,XXX,MVOTE,WS,
     C            LLV,LLVB,LLE,LLEB,LERROR,
     C            ZS,JCH,JEH,JCL,JEL,IROTC,KCUT,LCUT,LLL,
     C            XJCH,XJEH,XJCL,XJEL)
C
      LLM(IJL)=IJL
      LLN(IJL)=JEH+JEL
      FV1(IJL)=FLOAT(JEH+JEL)
      FV2(IJL)=WS(JX)
      KKKCUT(IJL)=KCUT
      LLLCUT(IJL)=LCUT
C
      IF(JEH+JEL.EQ.0)THEN
         KT=KT+JCH+JCL+JEH+JEL
         KITTY1=0
         KITTY2=JCH+JCL+JEH+JEL
         IJUST=2
         RETURN
      ENDIF
C
C
      KASTRO=4*(JEH+JEL)
      IF(KASTRO.GT.NP)KASTRO=NP
      IF(KASTRO.LT.4*NS)KASTRO=4*NS
C
      DO 108 I=1,NP
      LWRONG(I)=0
      DB2B1=WS(JX)-XXY(I)
      IF(XXY(I).LT.WS(JX))THEN
C
C  IF CORRECT PLACE LEGISLATOR POINT ON THE CURRENT CUTTING PLANE
C
         IF(LDATA(I,JX).EQ.KCUT)THEN
            DO 109 K=1,NS
            Y16MIDP(I,K)=XMAT(I,K)+DB2B1*ZVEC(JX,K)
  109       CONTINUE
         ENDIF
C
C  IF INCORRECT PUT ACTUAL POINT INTO THE CUTTING CLOUD
C
         IF(LDATA(I,JX).EQ.LCUT)THEN
            LWRONG(I)=1
            DO 110 K=1,NS
            Y16MIDP(I,K)=XMAT(I,K)
  110       CONTINUE
         ENDIF
C
C  IF NOT-VOTING PUT LEGISLATOR POINT ON THE CURRRENT CUTTING PLANE
C
         IF(LDATA(I,JX).EQ.0)THEN
            DO 111 K=1,NS
            Y16MIDP(I,K)=XMAT(I,K)+DB2B1*ZVEC(JX,K)
  111       CONTINUE
         ENDIF
      ENDIF
      IF(XXY(I).GT.WS(JX))THEN
C
C  IF CORRECT PLACE LEGISLATOR POINT ON THE CURRENT CUTTING PLANE
C
         IF(LDATA(I,JX).EQ.LCUT)THEN
            DO 112 K=1,NS
            Y16MIDP(I,K)=XMAT(I,K)+DB2B1*ZVEC(JX,K)
  112       CONTINUE
         ENDIF
C
C  IF INCORRECT PUT ACTUAL POINT INTO THE CUTTING CLOUD
C
         IF(LDATA(I,JX).EQ.KCUT)THEN
            LWRONG(I)=1
            DO 113 K=1,NS
            Y16MIDP(I,K)=XMAT(I,K)
  113       CONTINUE
         ENDIF
C
C  IF NOT-VOTING PUT LEGISLATOR POINT ON THE CURRRENT CUTTING PLANE
C
         IF(LDATA(I,JX).EQ.0)THEN
            DO 214 K=1,NS
            Y16MIDP(I,K)=XMAT(I,K)+DB2B1*ZVEC(JX,K)
  214       CONTINUE
         ENDIF
      ENDIF
C
  108 CONTINUE
C
C  MASS CENTER THE CUTTING PLANE MATRIX (Y16MIDP(,) HAS ALL POINTS)
C
      DO 215 K=1,NS
      SUM=0.0
      DO 216 I=1,NP
      SUM=SUM+Y16MIDP(I,K)
  216 CONTINUE
      DO 217 I=1,NP
      Y16MIDP(I,K)=Y16MIDP(I,K)-SUM/FLOAT(NP)
      SUMX(K)=SUMX(K)+Y16MIDP(I,K)**2
  217 CONTINUE
      SUMX(K)=SUMX(K)/FLOAT(NP)
  215 CONTINUE
C
C  CONSTRUCT PARTIAL CUTTING PLANE MATRIX (X16MIDP(,))
C
      KK=0
      KHIT=0
C
      DO 316 I=1,NP
      IF(LWRONG(I).EQ.1)THEN
         KK=KK+1
         DO 317 K=1,NS
         X16MIDP(KK,K)=Y16MIDP(I,K)
  317    CONTINUE
      ENDIF
  316 CONTINUE
      DO 201 I=1,NP
      IF(LWRONG(I).EQ.0)THEN
         KK=KK+1
         DO 219 K=1,NS
         X16MIDP(KK,K)=Y16MIDP(I,K)
  219    CONTINUE
         IF(KK.EQ.KASTRO)GO TO 203
      ENDIF
  201 CONTINUE
  203 CONTINUE
C
C  MASS CENTER THE PARTIAL CUTTING PLANE MATRIX
C
      DO 815 K=1,NS
      SUM=0.0
      DO 816 I=1,KASTRO
      SUM=SUM+X16MIDP(I,K)
  816 CONTINUE
      DO 817 I=1,KASTRO
      X16MIDP(I,K)=X16MIDP(I,K)-SUM/FLOAT(KASTRO)
      SUMX(K+NS)=SUMX(K+NS)+X16MIDP(I,K)**2
  817 CONTINUE
      SUMX(K+NS)=SUMX(K+NS)/FLOAT(KASTRO)
  815 CONTINUE
C
C  RUN REGRESSION TO ELIMINATE DIMENSION WITH LEAST VARIANCE
C
C
C  CALL SINGULAR VALUE DECOMPOSITION ROUTINE
C
      LWORK = 3*NS*NS+1001
      CALL SGESVD('S', 'A', NP, NS, Y16MIDP, 1001, YHAT, Y16MIDP,
     $     1001, VVV, 25, WORK, LWORK, INFO)
C
      DO 115 K=1,NS
      SUMX(K)=SUMX(K+NS)
      ZVEC(JX,K)=VVV(NS,K)
  115 CONTINUE
C
 3908 FORMAT(I5,I3,10F7.3)
C      ENDIF
C
C
C  RUN REGRESSION TO ELIMINATE DIMENSION WITH LEAST VARIANCE
C
      CALL SGESVD('S', 'A', KASTRO, NS, X16MIDP, 1001, YHAT, X16MIDP
     $     , 1001, VVV, 25, WORK, LWORK, INFO)
C
      IF(IJL.GT.25)THEN
         DO 114 K=1,NS
         ZVEC(JX,K)=VVV(NS,K)
  114    CONTINUE
      ENDIF
C
C
  999 CONTINUE
C
      CALL RSORT(FV1,NCUT,LLM)
C
      DO 281 JJ=1,NCUT
      IF(FV1(1).LT.FV1(JJ))GO TO 282
  281 CONTINUE
  282 KIN=JJ-1
      LLM(1)=LLM(KIN)
C
      DO 387 K=1,NS
      ZVEC(JX,K)=UUU(LLM(1),K)
  387 CONTINUE
      WS(JX)=FV2(LLM(1))
      KCUT=KKKCUT(LLM(1))
      LCUT=LLLCUT(LLM(1))
      DO 137 I=1,NP
      SUM=0.0
      DO 138 K=1,NS
      SUM=SUM+XMAT(I,K)*ZVEC(JX,K)
  138 CONTINUE
      XPROJ(I,JX)=SUM
      XXY(I)=SUM
  137 CONTINUE      
      KTT=KTT+LLN(LLM(1))
      KITTY1=LLN(LLM(1))
      IJUST=3
      KT=KT+JCH+JCL+JEH+JEL
      KITTY2=JCH+JCL+JEH+JEL
      RETURN
      END
C
C  **************************************************************************
C    SUBROUTINE JAN1PT -- FINDS OPTIMAL CUTTING POINT FOR ONE DIMENSION
C  **************************************************************************
C
      SUBROUTINE JAN1PT(NP,NRCALL,NS,IVOT,XMAT,YSS,KA,WS,LLV,LLVB,
     C                  LLE,LLEB,
     C                  LERROR,ZS,JCH,JEH,JCL,JEL,IROTC,KCCUT,LCCUT,
     C                  LLL,XJCH,XJEH,XJCL,XJEL)
      DIMENSION YSS(2901),KA(2901),WS(5802),LV(2901),LVB(2901),
     C          LEB(2901),Z(2901),Y(2901),LLV(2901),LLE(2901),
     C          LLVB(2901),LE(2901),LERROR(1001,2901),LJEP(2901),
     C          LLEB(2901),ZS(2901),LLL(2901),XMAT(1001,25),
     C          XJCH(25),XJEH(25),XJCL(25),XJEL(25),AAJEP(101),
     C          ABJEP(101),LAJEP(101),LBJEP(101),LCJEP(101),
     C          LDJEP(101),ABABJEP(101),MJEP(101)
C
      JROTC=1
      IF(IROTC.EQ.2)THEN
         JROTC=0
         IROTC=1
      ENDIF
      NPN=NP+1
      NPP=NP-1
      KCUT=1
      LCUT=6
      NOTE=2
      IF(IROTC.EQ.1)THEN
         NOTE=1
      ENDIF
      AA1=0.0
      AB1=0.0
      LA1=0
      LB1=0
      LC1=0
      LD1=0
      AA2=999.0
      AB2=0.0
      LA2=0
      LB2=0
      LC2=0
      LD2=0
      DO 999 III=1,NOTE
      IF(III.EQ.2)THEN
         KCUT=6
         LCUT=1
      ENDIF
C
C  CHECK ALL POSSIBLE INTERIOR CUT POINTS  --  THE NP INPUT POINTS
C      ARE HELD FIXED.  THERE ARE NP POSSIBLE CUT-POINTS BEGINNING
C      WITH CUT-POINT 1 WHICH IS .001 UNITS TO THE LEFT OF POINT 1.
C      CUT-POINT 2 IS BETWEEN POINTS 1 AND 2, ETC.
C
C     1   2   3   4   5   6   7   8   9   10   11 ...... NP-1   NP
C    *  *   *   *   *   *   *   *   *   *    *                *
C    1  2   3   4   5   6   7   8   9  10   11  ...........  NP
C
C  IF KCUT=1 AND LCUT=6, THE FOLLOWING NP PATTERNS ARE TESTED
C
C PATTERN
C   1         6666666666666666666666
C   2         1666666666666666666666
C   3         1166666666666666666666
C   4         1116666666666666666666
C   5         1111666666666666666666
C   6         1111166666666666666666
C   7         1111116666666666666666
C   .           .....
C   .           .....
C   .           .....
C  NP-1       1111111111111111111166
C   NP        1111111111111111111116
C
C  BECAUSE THE PROGRAM TRIES BOTH KCUT=1/LCUT=6 AND KCUT=6/LCUT=1, THIS
C  WILL ALSO TEST THE ONE MISSING PATTERN ABOVE, VIZ., ALL "1"s.
C
C
      KSE=0
      KSV=0
      LSV=0
      LSE=0
      KMARK=1
      I=0
  10  I=I+1
      IF(I-NP-1)61,12,12
  61  Z(I)=999.0
      IF(I.EQ.1)THEN
         Y(I)=YSS(1)-.001
      ENDIF
      IF(I.GT.1)THEN
         Y(I)=(YSS(I)+YSS(I-1))/2.0
      ENDIF
C      IF(KA(I).EQ.9)GO TO 10
      IF(KMARK.EQ.1)THEN
         DO 3 J=I,NP
         IF(KA(J).EQ.9)GO TO 3
         IF(LCUT-KA(J))33,5,33
  33     IF(KCUT-KA(J))3,6,3
  5      LSV=LSV+1
         GO TO 3
  6      LSE=LSE+1
  3      CONTINUE
         KMARK=0
         GO TO 31
      ENDIF
      IF(KA(I-1).EQ.KCUT)THEN
         KSV=KSV+1
         LSE=LSE-1
      ENDIF
      IF(KA(I-1).EQ.LCUT)THEN
         KSE=KSE+1
         LSV=LSV-1
      ENDIF
C
  31  CONTINUE
      LJEP(I)=I
      LV(I)=KSV
      LVB(I)=LSV
      LE(I)=KSE
      LEB(I)=LSE
      KT=LV(I)+LE(I)+LVB(I)+LEB(I)
      Z(I)=FLOAT(LE(I)+LEB(I))/FLOAT(KT)
C
      IF(JROTC.EQ.0)THEN
         ZS(I)=Y(I)
         LLV(I)=LV(I)
         LLE(I)=LE(I)
         LLVB(I)=LVB(I)
         LLEB(I)=LEB(I)
      ENDIF
      GO TO 10
  12  CONTINUE
C
C  FIND BEST CUT POINT
C
      CALL RSORT(Z,NP,LJEP)
      KIN=1
      MJEP(1)=1
      AAJEP(KIN)=Z(1)
      ABJEP(KIN)=Y(LJEP(1))
      ABABJEP(KIN)=ABS(ABJEP(KIN))
      LAJEP(KIN)=LV(LJEP(1))
      LBJEP(KIN)=LE(LJEP(1))
      LCJEP(KIN)=LVB(LJEP(1))
      LDJEP(KIN)=LEB(LJEP(1))
C
C  CHECK IF THERE ARE MULTIPLE CUT-POINTS WITH SAME CLASSIFICATION AND
C    SELECT THAT CUT-POINT CLOSEST TO THE INTERIOR OF THE SPACE
C
      DO 63 I=2,NP
      IF(ABS(Z(1)-Z(I)).LE..00001)THEN
         KIN=KIN+1
         MJEP(KIN)=KIN
         AAJEP(KIN)=Z(I)
         ABJEP(KIN)=Y(LJEP(I))
         ABABJEP(KIN)=ABS(ABJEP(KIN))
         LAJEP(KIN)=LV(LJEP(I))
         LBJEP(KIN)=LE(LJEP(I))
         LCJEP(KIN)=LVB(LJEP(I))
         LDJEP(KIN)=LEB(LJEP(I))
         IF(KIN.GT.100)GO TO 633
         GO TO 63
      ENDIF
      IF(Z(1).LT.Z(I))GO TO 633
  63  CONTINUE
  633 CONTINUE
      IF(KIN.EQ.1)THEN
         AA=AAJEP(1)
         AB=ABJEP(1)
         LA=LAJEP(1)
         LB=LBJEP(1)
         LC=LCJEP(1)
         LD=LDJEP(1)
      ENDIF
      IF(KIN.GT.1)THEN
         CALL RSORT(ABABJEP,KIN,MJEP)
         AA=AAJEP(MJEP(1))
         AB=ABJEP(MJEP(1))
         LA=LAJEP(MJEP(1))
         LB=LBJEP(MJEP(1))
         LC=LCJEP(MJEP(1))
         LD=LDJEP(MJEP(1))
      ENDIF
C
      IF(III.EQ.1)THEN
         AA1=AA
         AB1=AB
         LA1=LA
         LB1=LB
         LC1=LC
         LD1=LD
      ENDIF
      IF(III.EQ.2)THEN
         AA2=AA
         AB2=AB
         LA2=LA
         LB2=LB
         LC2=LC
         LD2=LD
      ENDIF
C
  999 CONTINUE
C
      IF(AA1.LE.AA2)THEN
         KCCUT=1
         LCCUT=6
         AA=AA1
         AB=AB1
         LA=LA1
         LB=LB1
         LC=LC1
         LD=LD1
      ENDIF
      IF(AA1.GT.AA2)THEN
         KCCUT=6
         LCCUT=1
         AA=AA2
         AB=AB2
         LA=LA2
         LB=LB2
         LC=LC2
         LD=LD2
      ENDIF
      IF(IROTC.EQ.1)THEN
         KCCUT=1
         LCCUT=6
         AA=AA1
         AB=AB1
         LA=LA1
         LB=LB1
         LC=LC1
         LD=LD1
      ENDIF
      WS(IVOT)=AB
      IF(IROTC.EQ.1)WS(IVOT+NRCALL)=AB
      IF(JROTC.EQ.1)THEN
         ZS(IVOT)=AA
         LLV(IVOT)=LA
         LLE(IVOT)=LB
         LLVB(IVOT)=LC
         LLEB(IVOT)=LD
      ENDIF
      JCL=LA
      JEL=LB
      JCH=LC
      JEH=LD
C
      IF(IROTC.EQ.0)THEN
         DO 71 K=1,NS
         XJCH(K)=0.0
         XJEH(K)=0.0
         XJCL(K)=0.0
         XJEL(K)=0.0
  71     CONTINUE
         DO 64 I=1,NP
         IF(KA(I).EQ.9)GO TO 64
         IF(YSS(I).LT.AB)THEN
            IF(KA(I).EQ.KCCUT)THEN
               LERROR(LLL(I),IVOT)=0
               DO 70 K=1,NS
               XJCL(K)=XJCL(K)+XMAT(LLL(I),K)
   70          CONTINUE
            ENDIF
            IF(KA(I).EQ.LCCUT)THEN
               LERROR(LLL(I),IVOT)=1
               DO 72 K=1,NS
               XJEL(K)=XJEL(K)+XMAT(LLL(I),K)
   72          CONTINUE
            ENDIF
         ENDIF
         IF(YSS(I).GT.AB)THEN
            IF(KA(I).EQ.LCCUT)THEN
               LERROR(LLL(I),IVOT)=0
               DO 73 K=1,NS
               XJCH(K)=XJCH(K)+XMAT(LLL(I),K)
   73          CONTINUE
            ENDIF
            IF(KA(I).EQ.KCCUT)THEN
               LERROR(LLL(I),IVOT)=1
               DO 74 K=1,NS
               XJEH(K)=XJEH(K)+XMAT(LLL(I),K)
   74          CONTINUE
            ENDIF
         ENDIF
  64     CONTINUE
         DO 75 K=1,NS
         IF(JCL.GT.0)XJCL(K)=XJCL(K)/FLOAT(JCL)
         IF(JEL.GT.0)XJEL(K)=XJEL(K)/FLOAT(JEL)
         IF(JCH.GT.0)XJCH(K)=XJCH(K)/FLOAT(JCH)
         IF(JEH.GT.0)XJEH(K)=XJEH(K)/FLOAT(JEH)
  75     CONTINUE
      ENDIF
      IF(IROTC.EQ.1)THEN
         DO 65 I=1,NP
         IF(KA(I).EQ.9)GO TO 65
         IF(YSS(I).LT.AB)THEN
            IF(KA(I).EQ.KCCUT)LERROR(IVOT,LLL(I))=0
            IF(KA(I).EQ.LCCUT)LERROR(IVOT,LLL(I))=1
         ENDIF
         IF(YSS(I).GT.AB)THEN
            IF(KA(I).EQ.LCCUT)LERROR(IVOT,LLL(I))=0
            IF(KA(I).EQ.KCCUT)LERROR(IVOT,LLL(I))=1
         ENDIF
  65     CONTINUE
      ENDIF
      RETURN
      END
