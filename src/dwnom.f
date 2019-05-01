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
      
      SUBROUTINE dwnom(NOMSTARTIN, WEIGHTSIN, NBILLS, ICONGIN,
     C     DYNIN, ZMIDIN, MCONGIN,
     C     NROWRCT, NCOLRCT, RCVOTET1IN, RCVOTET9IN,
     C     NLEGS, NCONGIN, ID1IN,
     C     XDATAIN, NROWRC, NCOLRC,
     C     RCVOTE1IN, RCVOTE9IN,
     C     XDATAOUT, SDX1OUT, SDX2OUT, VARX1OUT, VARX2OUT,
     C     XBIGLOGOUT, KBIGLOGOUT,
     C     GMPAOUT, GMPBOUT, DYNOUT, ZMIDOUT)
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
     C     DYNIN(NBILLS, NOMSTARTIN(1)),
     C     ZMIDIN(NBILLS, NOMSTARTIN(1)),
     C     XDATAIN(NLEGS, NOMSTARTIN(1)),
     C     XDATAOUT(NLEGS, NOMSTARTIN(1)),
     C     SDX1OUT(NLEGS), SDX2OUT(NLEGS),
     C     VARX1OUT(NLEGS), VARX2OUT(NLEGS),
     C     XBIGLOGOUT(NLEGS, 2),
     C     GMPAOUT(NLEGS), GMPBOUT(NLEGS),
     C     DYNOUT(NBILLS, NOMSTARTIN(1)),
     C     ZMIDOUT(NBILLS, NOMSTARTIN(1))
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
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
      CHARACTER*64 FTITLE
      INTEGER*2 ITIM1,ITIM2,ITIM3,ITIM4,JTIM1,JTIM2,JTIM3,JTIM4
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
 1001 format(' ELAPSED TIME OF JOB  ',4(i2.2,'.'))
C     This function can't be compiled!:
C     call gettim(itim1,itim2,itim3,itim4)
      
C
C  READ TITLE OF RUN
C
      FTITLE = 'NOMINAL DYNAMIC-WEIGHTED MULTIDIMENSIONAL UNFOLDING '
      WRITE(*,102)FTITLE
      NS = NOMSTARTIN(1)
      NMODEL = NOMSTARTIN(2)
      NFIRST = NOMSTARTIN(3)
      NLAST = NOMSTARTIN(4)
      IHAPPY1 = NOMSTARTIN(5)
      IHAPPY2 = NOMSTARTIN(6)
      WRITE(*,103)NS,NMODEL,NFIRST,NLAST,IHAPPY1,IHAPPY2
      WEIGHT = WEIGHTSIN
      WRITE(*,104)WEIGHT(NS+1),(WEIGHT(K),K=2,NS)
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
      MCONG = MCONGIN
  425 WRITE(*,302)I
C
C  READ ROLL CALL STARTS -- HC01108.DAT
C
      ICONG = ICONGIN
      DYN = DYNIN
      ZMID = ZMIDIN
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
 475  WRITE(*,300)I
      NQTOT=I
C
C  READ PSEUDO-DYNAMIC STARTS FROM BLACK BOX -- HL01108.SRT
C
      NCONG = NCONGIN
      ID1 = ID1IN
      XDATA = XDATAIN
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
 450  WRITE(*,301)I
      WRITE(*,321)
      NPTOT=I
C
      DO 9999 IHAPPY=IHAPPY1,IHAPPY2
C
C  ************************************
C     DIMENSION WEIGHT PHASE
C  ************************************
      IF(NS.GE.2)THEN
         WRITE(*,320)'dimension weights'
         CALL WINT(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      ENDIF
C
C  ************************************
C     BETA (1/SIGMA) PHASE
C  ************************************
      WRITE(*,320)'beta'
      CALL SIGMAS(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
C
C  ************************************
C     ROLL CALL PHASE
C  ************************************
C    
C
      WRITE(*,320)'roll call vectors'
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
         WRITE(*,305)II,NQ,NQC
         STOP
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
         WRITE(*,306)II,J,I
         STOP
      ENDIF
      IF(RCVOTE1(I+KTOTP,J).NEQV.RCVOTET1(J+KTOTQ,I))THEN
         WRITE(*,307)II,J,I
         STOP
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
      WRITE(*,320)'legislator coordinates'
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
C     This function can't be compiled!:
C     call gettim(itim1,itim2,itim3,itim4)
      
c$$$      write(*,1000)itim1,itim2,itim3,itim4
c$$$      write(*,1001)jtim1,jtim2,jtim3,jtim4
C     stop
      WRITE(*,321)
      end
C
C  ***************************************************************************
C    SUBROUTINE SIGMAS--FINDS BEST STARTING ESTIMATE OF SIGMA-SQUARED VIA
C                       GRID SEARCH
C  ***************************************************************************
C
      SUBROUTINE SIGMAS(XPLOG,WDERV,NFIRST,NLAST,ZDF,NDEVIT,XDEVIT)
      DIMENSION WDERV(99),ZDF(150000,4)
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
C
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
      DIMENSION WDERV(99),ZDF(150000,4)
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
C
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
      DIMENSION DYES(99),DNO(99),DCC(99),DBB(99),
     C          WDERV(99),ZDF(150000,4),WDERV2(99)
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
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
      DIMENSION DYES(99),DNO(99),DYES1(99),DNO1(99),
     C          DCC(99),DBB(99),DCC1(99),DBB1(99),XDERV(99),
     C          XDERV1(99),XDERV2(99),XDERV3(99),
     C          ATIME(152,127),MARK(99,3),XMARK(99,3),
     C          XBETA(5,5),ZDF(150000,4),OUTX0(99,99),AADERV(99),
     C          OUTX1(99,99),OUTX2(99,99),OUTX3(99,99)
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
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
      DIMENSION ATIME(152,127),ZDF(150000,4),
     C            YYY(152),VVV(152),ID1(54001),XMARK(99,3),
     C            XDERV(99),XDERV1(99),XDERV2(99),XDERV3(99),
     C            XXX(152),ZMAT(99,99),FV1(199),
     C            FV2(199),WVEC(199),DERVISH(99,99),
     C            LLL(152),YLOG(152),YGAMMA(152,99),
     C            XXXSAVE(152,99),XBETA(5,5),XBETASV(5,5),
     C            YGMP(152),YYGMP(152),OUTX0(99,99),
     C            OUTX1(99,99),OUTX2(99,99),OUTX3(99,99)
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
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
          WRITE(*,1003)NEP
C          STOP
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
             WRITE(*,1003)NEP
             STOP
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
             WRITE(*,1003)NEP
             STOP
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
             WRITE(*,1003)NEP
             STOP
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
      call rs(99,NS,OUTX0,wvec,1,ZMAT,fv1,fv2,ier)
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
         call rs(99,2*NS,OUTX1,wvec,1,ZMAT,fv1,fv2,ier)
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
         call rs(99,3*NS,OUTX2,wvec,1,ZMAT,fv1,fv2,ier)
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
         call rs(99,4*NS,OUTX3,wvec,1,ZMAT,fv1,fv2,ier)
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
     CBB(127,152),ZMAT(127,127),FV1(127),FV2(127),WVEC(127)
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
      call rs(127,nf,B,wvec,1,ZMAT,fv1,fv2,ier)
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
      DIMENSION ZDF(150000,4),DYES(99),DNO(99),DYES1(99),DNO1(99),
     C          DCC(99),DBB(99),DCC1(99),DBB1(99),ZDERV(99),
     C          DDERV(99),OLDZ(99),OLDD(99)
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
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
      DIMENSION ZDF(150000,4),DDERVX(99),ZDERVX(99),OLDZ(99),
     C          OLDD(99),DZSAVE(66),XXX(66),YGAMMA(50,66),
     C          XXXSAVE(50,66),YLOG(50),LLL(50),YGMP(50),
     C          YYGMP(50)
      LOGICAL*1 RCVOTE1,RCVOTE9,RCVOTET1,RCVOTET9,RCBAD,LWHERE
      COMMON /XXCOM/ ZMID(99001,2),XDATA(54001,9),DYN(99001,2),
     C               RCVOTE1(54001,2901),RCVOTE9(54001,2901),
     C               RCVOTET1(99001,660),RCVOTET9(99001,660),
     C               RCBAD(99001),LWHERE(99999,200),
     C               NUMCONG(200),MCONG(200,3),ICONG(99001),
     C               WEIGHT(99),NUMCONGT(200),
     C               NCONG(54001),KWHERE(99999,200),
     C               XBIGLOG(54001,2),KBIGLOG(54001,4)
      COMMON /MINE/ NS,NQTOT,NPTOT,KLASS,
     C              KLASSYY,KLASSNY,KLASSYN,KLASSNN
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
C  **************************************************************************
C    EIGENVECTOR/EIGENVALUE DECOMPOSITION SUBROUTINES FOR A SYMMETRIC MATRIX
C    SUBROUTINES ARE FROM EISPACK
C  **************************************************************************
C
      SUBROUTINE RS(NM,N,A,W,MATZ,Z,FV1,FV2,IERR)
C
      INTEGER N,NM,IERR,MATZ
      REAL A(NM,N),W(N),Z(NM,N),FV1(N),FV2(N)
C
C     THIS SUBROUTINE CALLS THE RECOMMENDED SEQUENCE OF
C     SUBROUTINES FROM THE EIGENSYSTEM SUBROUTINE PACKAGE (EISPACK)
C     TO FIND THE EIGENVALUES AND EIGENVECTORS (IF DESIRED)
C     OF A REAL SYMMETRIC MATRIX.
C
C     ON INPUT
C
C        NM  MUST BE SET TO THE ROW DIMENSION OF THE TWO-DIMENSIONAL
C        ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C        DIMENSION STATEMENT.
C
C        N  IS THE ORDER OF THE MATRIX  A.
C
C        A  CONTAINS THE REAL SYMMETRIC MATRIX.
C
C        MATZ  IS AN INTEGER VARIABLE SET EQUAL TO ZERO IF
C        ONLY EIGENVALUES ARE DESIRED.  OTHERWISE IT IS SET TO
C        ANY NON-ZERO INTEGER FOR BOTH EIGENVALUES AND EIGENVECTORS.
C
C     ON OUTPUT
C
C        W  CONTAINS THE EIGENVALUES IN ASCENDING ORDER.
C
C        Z  CONTAINS THE EIGENVECTORS IF MATZ IS NOT ZERO.
C
C        IERR  IS AN INTEGER OUTPUT VARIABLE SET EQUAL TO AN ERROR
C           COMPLETION CODE DESCRIBED IN THE DOCUMENTATION FOR TQLRAT
C           AND TQL2.  THE NORMAL COMPLETION CODE IS ZERO.
C
C        FV1  AND  FV2  ARE TEMPORARY STORAGE ARRAYS.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IF (N .LE. NM) GO TO 10
      IERR = 10 * N
      GO TO 50
C
   10 IF (MATZ .NE. 0) GO TO 20
C     .......... FIND EIGENVALUES ONLY ..........
      CALL  TRED1(NM,N,A,W,FV1,FV2)
      CALL  TQLRAT(N,W,FV2,IERR)
      GO TO 50
C     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS ..........
   20 CALL  TRED2(NM,N,A,W,FV1,Z)
      CALL  TQL2(NM,N,W,FV1,Z,IERR)
   50 RETURN
      END
      SUBROUTINE TRED1(NM,N,A,D,E,E2)
C
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),E2(N)
      REAL F,G,H,SCALE
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED1,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX
C     TO A SYMMETRIC TRIDIAGONAL MATRIX USING
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C     ON OUTPUT
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS-
C          FORMATIONS USED IN THE REDUCTION IN ITS STRICT LOWER
C          TRIANGLE.  THE FULL UPPER TRIANGLE OF A IS UNALTERED.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
C
C        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
C          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, N
         D(I) = A(N,I)
         A(N,I) = A(I,I)
  100 CONTINUE
C     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 1) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(D(K))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
C
         DO 125 J = 1, L
            D(J) = A(L,J)
            A(L,J) = A(I,J)
            A(I,J) = 0.0E0
  125    CONTINUE
C
  130    E(I) = 0.0E0
         E2(I) = 0.0E0
         GO TO 300
C
  140    DO 150 K = 1, L
            D(K) = D(K) / SCALE
            H = H + D(K) * D(K)
  150    CONTINUE
C
         E2(I) = SCALE * SCALE * H
         F = D(L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         D(L) = F - G
         IF (L .EQ. 1) GO TO 285
C     .......... FORM A*U ..........
         DO 170 J = 1, L
  170    E(J) = 0.0E0
C
         DO 240 J = 1, L
            F = D(J)
            G = E(J) + A(J,J) * F
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
               G = G + A(K,J) * D(K)
               E(K) = E(K) + A(K,J) * F
  200       CONTINUE
C
  220       E(J) = G
  240    CONTINUE
C     .......... FORM P ..........
         F = 0.0E0
C
         DO 245 J = 1, L
            E(J) = E(J) / H
            F = F + E(J) * D(J)
  245    CONTINUE
C
         H = F / (H + H)
C     .......... FORM Q ..........
         DO 250 J = 1, L
  250    E(J) = E(J) - H * D(J)
C     .......... FORM REDUCED A ..........
         DO 280 J = 1, L
            F = D(J)
            G = E(J)
C
            DO 260 K = J, L
  260       A(K,J) = A(K,J) - F * E(K) - G * D(K)
C
  280    CONTINUE
C
  285    DO 290 J = 1, L
            F = D(J)
            D(J) = A(L,J)
            A(L,J) = A(I,J)
            A(I,J) = F * SCALE
  290    CONTINUE
C
  300 CONTINUE
C
      RETURN
      END
      SUBROUTINE TRED2(NM,N,A,D,E,Z)
C
      INTEGER I,J,K,L,N,II,NM,JP1
      REAL A(NM,N),D(N),E(N),Z(NM,N)
      REAL F,G,H,HH,SCALE
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TRED2,
C     NUM. MATH. 11, 181-195(1968) BY MARTIN, REINSCH, AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
C
C     THIS SUBROUTINE REDUCES A REAL SYMMETRIC MATRIX TO A
C     SYMMETRIC TRIDIAGONAL MATRIX USING AND ACCUMULATING
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        A CONTAINS THE REAL SYMMETRIC INPUT MATRIX.  ONLY THE
C          LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
C
C     ON OUTPUT
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE TRIDIAGONAL MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
C          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
C
C        Z CONTAINS THE ORTHOGONAL TRANSFORMATION MATRIX
C          PRODUCED IN THE REDUCTION.
C
C        A AND Z MAY COINCIDE.  IF DISTINCT, A IS UNALTERED.
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      DO 100 I = 1, N
C
         DO 80 J = I, N
   80    Z(J,I) = A(J,I)
C
         D(I) = A(N,I)
  100 CONTINUE
C
      IF (N .EQ. 1) GO TO 510
C     .......... FOR I=N STEP -1 UNTIL 2 DO -- ..........
      DO 300 II = 2, N
         I = N + 2 - II
         L = I - 1
         H = 0.0E0
         SCALE = 0.0E0
         IF (L .LT. 2) GO TO 130
C     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO 120 K = 1, L
  120    SCALE = SCALE + ABS(D(K))
C
         IF (SCALE .NE. 0.0E0) GO TO 140
  130    E(I) = D(L)
C
         DO 135 J = 1, L
            D(J) = Z(L,J)
            Z(I,J) = 0.0E0
            Z(J,I) = 0.0E0
  135    CONTINUE
C
         GO TO 290
C
  140    DO 150 K = 1, L
            D(K) = D(K) / SCALE
            H = H + D(K) * D(K)
  150    CONTINUE
C
         F = D(L)
         G = -SIGN(SQRT(H),F)
         E(I) = SCALE * G
         H = H - F * G
         D(L) = F - G
C     .......... FORM A*U ..........
         DO 170 J = 1, L
  170    E(J) = 0.0E0
C
         DO 240 J = 1, L
            F = D(J)
            Z(J,I) = F
            G = E(J) + Z(J,J) * F
            JP1 = J + 1
            IF (L .LT. JP1) GO TO 220
C
            DO 200 K = JP1, L
               G = G + Z(K,J) * D(K)
               E(K) = E(K) + Z(K,J) * F
  200       CONTINUE
C
  220       E(J) = G
  240    CONTINUE
C     .......... FORM P ..........
         F = 0.0E0
C
         DO 245 J = 1, L
            E(J) = E(J) / H
            F = F + E(J) * D(J)
  245    CONTINUE
C
         HH = F / (H + H)
C     .......... FORM Q ..........
         DO 250 J = 1, L
  250    E(J) = E(J) - HH * D(J)
C     .......... FORM REDUCED A ..........
         DO 280 J = 1, L
            F = D(J)
            G = E(J)
C
            DO 260 K = J, L
  260       Z(K,J) = Z(K,J) - F * E(K) - G * D(K)
C
            D(J) = Z(L,J)
            Z(I,J) = 0.0E0
  280    CONTINUE
C
  290    D(I) = H
  300 CONTINUE
C     .......... ACCUMULATION OF TRANSFORMATION MATRICES ..........
      DO 500 I = 2, N
         L = I - 1
         Z(N,L) = Z(L,L)
         Z(L,L) = 1.0E0
         H = D(I)
         IF (H .EQ. 0.0E0) GO TO 380
C
         DO 330 K = 1, L
  330    D(K) = Z(K,I) / H
C
         DO 360 J = 1, L
            G = 0.0E0
C
            DO 340 K = 1, L
  340       G = G + Z(K,I) * Z(K,J)
C
            DO 360 K = 1, L
               Z(K,J) = Z(K,J) - G * D(K)
  360    CONTINUE
C
  380    DO 400 K = 1, L
  400    Z(K,I) = 0.0E0
C
  500 CONTINUE
C
  510 DO 520 I = 1, N
         D(I) = Z(N,I)
         Z(N,I) = 0.0E0
  520 CONTINUE
C
      Z(N,N) = 1.0E0
      E(1) = 0.0E0
      RETURN
      END
      SUBROUTINE TQL2(NM,N,D,E,Z,IERR)
C
      INTEGER I,J,K,L,M,N,II,L1,L2,NM,MML,IERR
      REAL D(N),E(N),Z(NM,N)
      REAL C,C2,C3,DL1,EL1,F,G,H,P,R,S,S2,TST1,TST2,PYTHAG
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQL2,
C     NUM. MATH. 11, 293-306(1968) BY BOWDLER, MARTIN, REINSCH, AND
C     WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 227-240(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE QL METHOD.
C     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
C     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
C     FULL MATRIX TO TRIDIAGONAL FORM.
C
C     ON INPUT
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT.
C
C        N IS THE ORDER OF THE MATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
C          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
C          THE IDENTITY MATRIX.
C
C      ON OUTPUT
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
C          UNORDERED FOR INDICES 1,2,...,IERR-1.
C
C        E HAS BEEN DESTROYED.
C
C        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
C          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
C          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
C          EIGENVALUES.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     CALLS PYTHAG FOR  SQRT(A*A + B*B) .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E(I-1) = E(I)
C
      F = 0.0E0
      TST1 = 0.0E0
      E(N) = 0.0E0
C
      DO 240 L = 1, N
         J = 0
         H = ABS(D(L)) + ABS(E(L))
         IF (TST1 .LT. H) TST1 = H
C     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
         DO 110 M = L, N
            TST2 = TST1 + ABS(E(M))
            IF (TST2 .EQ. TST1) GO TO 120
C     .......... E(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 220
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         L1 = L + 1
         L2 = L1 + 1
         G = D(L)
         P = (D(L1) - G) / (2.0E0 * E(L))
         R = PYTHAG(P,1.0E0)
         D(L) = E(L) / (P + SIGN(R,P))
         D(L1) = E(L) * (P + SIGN(R,P))
         DL1 = D(L1)
         H = G - D(L)
         IF (L2 .GT. N) GO TO 145
C
         DO 140 I = L2, N
  140    D(I) = D(I) - H
C
  145    F = F + H
C     .......... QL TRANSFORMATION ..........
         P = D(M)
         C = 1.0E0
         C2 = C
         EL1 = E(L1)
         S = 0.0E0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            C3 = C2
            C2 = C
            S2 = S
            I = M - II
            G = C * E(I)
            H = C * P
            R = PYTHAG(P,E(I))
            E(I+1) = S * R
            S = E(I) / R
            C = P / R
            P = C * D(I) - S * G
            D(I+1) = H + S * (C * G + S * D(I))
C     .......... FORM VECTOR ..........
            DO 180 K = 1, N
               H = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * H
               Z(K,I) = C * Z(K,I) - S * H
  180       CONTINUE
C
  200    CONTINUE
C
         P = -S * S2 * C3 * EL1 * E(L) / DL1
         E(L) = S * P
         D(L) = C * P
         TST2 = TST1 + ABS(E(L))
         IF (TST2 .GT. TST1) GO TO 130
  220    D(L) = D(L) + F
  240 CONTINUE
C     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO 300 II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
         DO 260 J = II, N
            IF (D(J) .GE. P) GO TO 260
            K = J
            P = D(J)
  260    CONTINUE
C
         IF (K .EQ. I) GO TO 300
         D(K) = D(I)
         D(I) = P
C
         DO 280 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  280    CONTINUE
C
  300 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
      SUBROUTINE TQLRAT(N,D,E2,IERR)
C
      INTEGER I,J,L,M,N,II,L1,MML,IERR
      REAL D(N),E2(N)
      REAL B,C,F,G,H,P,R,S,T,EPSLON,PYTHAG
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE TQLRAT,
C     ALGORITHM 464, COMM. ACM 16, 689(1973) BY REINSCH.
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
C     TRIDIAGONAL MATRIX BY THE RATIONAL QL METHOD.
C
C     ON INPUT
C
C        N IS THE ORDER OF THE MATRIX.
C
C        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
C
C        E2 CONTAINS THE SQUARES OF THE SUBDIAGONAL ELEMENTS OF THE
C          INPUT MATRIX IN ITS LAST N-1 POSITIONS.  E2(1) IS ARBITRARY.
C
C      ON OUTPUT
C
C        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
C          ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
C          THE SMALLEST EIGENVALUES.
C
C        E2 HAS BEEN DESTROYED.
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     CALLS PYTHAG FOR  SQRT(A*A + B*B) .
C
C     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
C     MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
C
C     THIS VERSION DATED AUGUST 1983.
C
C     ------------------------------------------------------------------
C
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E2(I-1) = E2(I)
C
      F = 0.0E0
      T = 0.0E0
      E2(N) = 0.0E0
C
      DO 290 L = 1, N
         J = 0
         H = ABS(D(L)) + SQRT(E2(L))
         IF (T .GT. H) GO TO 105
         T = H
         B = EPSLON(T)
         C = B * B
C     .......... LOOK FOR SMALL SQUARED SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (E2(M) .LE. C) GO TO 120
C     .......... E2(N) IS ALWAYS ZERO, SO THERE IS NO EXIT
C                THROUGH THE BOTTOM OF THE LOOP ..........
  110    CONTINUE
C
  120    IF (M .EQ. L) GO TO 210
  130    IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         L1 = L + 1
         S = SQRT(E2(L))
         G = D(L)
         P = (D(L1) - G) / (2.0E0 * S)
         R = PYTHAG(P,1.0E0)
         D(L) = S / (P + SIGN(R,P))
         H = G - D(L)
C
         DO 140 I = L1, N
  140    D(I) = D(I) - H
C
         F = F + H
C     .......... RATIONAL QL TRANSFORMATION ..........
         G = D(M)
         IF (G .EQ. 0.0E0) G = B
         H = G
         S = 0.0E0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            P = G * H
            R = P + E2(I)
            E2(I+1) = S * R
            S = E2(I) / R
            D(I+1) = H + S * (H + D(I))
            G = D(I) - E2(I) / G
            IF (G .EQ. 0.0E0) G = B
            H = G * P / R
  200    CONTINUE
C
         E2(L) = S * G
         D(L) = H
C     .......... GUARD AGAINST UNDERFLOW IN CONVERGENCE TEST ..........
         IF (H .EQ. 0.0E0) GO TO 210
         IF (ABS(E2(L)) .LE. ABS(C/H)) GO TO 210
         E2(L) = H * E2(L)
         IF (E2(L) .NE. 0.0E0) GO TO 130
  210    P = D(L) + F
C     .......... ORDER EIGENVALUES ..........
         IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
      REAL FUNCTION PYTHAG(A,B)
      REAL A,B
C
C     FINDS SQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW
C
      REAL P,R,S,T,U
      P = AMAX1(ABS(A),ABS(B))
      IF (P .EQ. 0.0E0) GO TO 20
      R = (AMIN1(ABS(A),ABS(B))/P)**2
   10 CONTINUE
         T = 4.0E0 + R
         IF (T .EQ. 4.0E0) GO TO 20
         S = R/T
         U = 1.0E0 + 2.0E0*S
         P = U*P
         R = (S/U)**2 * R
      GO TO 10
   20 PYTHAG = P
      RETURN
      END
c
      REAL FUNCTION EPSLON (X)
      REAL X
      REAL A,B,C,EPS
      A = 4.0E0/3.0E0
  10  B = A - 1.0E0
      C = B + B + B
      EPS = ABS(C-1.0E0)
      IF(EPS .EQ. 0.0E0)GO TO 10
      EPSLON = EPS*ABS(X)
      RETURN
      END
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
     C          VVV(25,25),LDATA(1001,2901)
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
      XTOL=.001
      CALL LSVRR(NP,NS,Y16MIDP,1001,21,XTOL,IRANK,YHAT,Y16MIDP,
     C           1001,VVV,25)
C
      DO 115 K=1,NS
      SUMX(K)=SUMX(K+NS)
      ZVEC(JX,K)=VVV(K,NS)
  115 CONTINUE
C
 3908 FORMAT(I5,I3,10F7.3)
C      ENDIF
C
C
C  RUN REGRESSION TO ELIMINATE DIMENSION WITH LEAST VARIANCE
C
      CALL LSVRR(KASTRO,NS,X16MIDP,1001,21,XTOL,IRANK,YHAT,X16MIDP,
     C           1001,VVV,25)
C
      IF(IJL.GT.25)THEN
         DO 114 K=1,NS
         ZVEC(JX,K)=VVV(K,NS)
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
C-----------------------------------------------------------------------
C  IMSL Name:  LSVRR/DLSVRR (Single/Double precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    January 1, 1985
C
C  Purpose:    Compute the singular value decomposition of a real
C              matrix.
C
C  Usage:      CALL LSVRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK,
C                          S, U, LDU, V, LDV)
C
C  Arguments:
C     NRA    - Number of rows of A.  (Input)
C     NCA    - Number of columns of A.  (Input)
C     A      - NRA by NCA matrix whose singular value decomposition
C              is to be computed.  (Input)
C     LDA    - Leading dimension of A exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     IPATH  - Flag used to control the computation of the singular
C              vectors.  (Input)
C              IPATH has the decimal expansion IJ such that:
C                 I = 0 means do not compute the left singular vectors,
C                 I = 1 means return the NCA left singular vectors in U,
C                 I = 2 means return only the MIN(NRA,NCA) left singular
C                       vectors in U,
C                 J = 0 means do not compute the right singular vectors,
C                 J = 1 means return the right singular vectors in V.
C              For example, IPATH = 20 means I = 2 and J = 0.
C     TOL    - Scalar containing the tolerance used to determine when a
C              singular value is negligible.  (Input)
C              If TOL is positive then a singular value SI is considered
C                 negligible if SI .LE. TOL.
C              If TOL is negative then a singular value SI is considered
C                 negligible if SI .LE. ABS(TOL)*(Infinity norm of A).
C                 In this case ABS(TOL) should generally contain an
C                 estimate of the level of relative error in the data.
C     IRANK  - Scalar containing an estimate of the rank of A.  (Output)
C     S      - Vector of length MIN(NRA+1,NCA) containing the singular
C              values of A in descending order of magnitude in the first
C              MIN(NRA,NCA) positions.  (Output)
C     U      - NRA by NCU matrix containing the left singular vectors of
C              A.  (Output)
C              NCU must be equal to NRA if I is equal to 1.
C              NCU must be equal to MIN(NRA,NCA) if I is equal to 2.
C              U will not be referenced if I is equal to zero.  If NRA
C              is less than or equal to NCU, then U can share the same
C              storage locations as A.  See Remarks.
C     LDU    - Leading dimension of U exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     V      - NCA by NCA matrix containing the right singular vectors
C              of A.  (Output)
C              V will not be referenced if J is equal to zero.  V can
C              share the same storage location as A; however U and V
C              cannot both coincide with A simultaneously.
C     LDV    - Leading dimension of V exactly as specified in the
C              dimension statement of the calling program.  (Input)
C
C  Remarks:
C  1. Automatic workspace usage is
C              LSVRR    NRA*NCA + NRA + NCA + MAX(NRA,NCA) - 1 units, or
C              DLSVRR   2*(NRA*NCA + NRA + NCA + MAX(NRA,NCA) - 1)
C                       units.
C     Workspace may be explicitly provided, if desired, by use of
C     L2VRR/DL2VRR.  The reference is
C              CALL L2VRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK,
C                          S, U, LDU, V, LDV, ACOPY, WK)
C     The additional arguments are as follows:
C     ACOPY  - Work vector of length NRA*NCA for the matrix A.  If
C              A is not needed then A and ACOPY may share the same
C              storage locations.
C     WK     - Work vector of length NRA + NCA + MAX(NRA,NCA) - 1.
C
C  2. Informational error
C     Type Code
C       4   1  Convergence cannot be achieved for all the singular
C              values and their corresponding singular vectors.
C
C  3. When NRA is much greater than NCA, it might not be reasonable to
C     store the whole matrix U.  In this case IPATH with I = 2 allows
C     a singular value factorization of A to be computed in which only
C     the first NCA columns of U are computed, and in many applications
C     those are all that are needed.
C
C  Keywords:   Least squares; Complete orthogonal decomposition;
C              Rank estimation
C
C  GAMS:       D6
C
C  Chapters:   MATH/LIBRARY Linear Systems
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE LSVRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK, S, U,
     &                  LDU, V, LDV)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NRA, NCA, LDA, IPATH, IRANK, LDU, LDV
      REAL       TOL, A(LDA,*), S(*), U(LDU,*), V(LDV,*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    INDA, INDW
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(61913)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ RWKSP
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MAX0
      INTRINSIC  MAX0
      INTEGER    MAX0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, L2VRR
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KGT, N1RCD
      INTEGER    I1KGT, N1RCD
C
      CALL E1PSH ('LSVRR ')
C
      IF (NRA.LE.0 .OR. NCA.LE.0) THEN
         CALL E1STI (1, NRA)
         CALL E1STI (2, NCA)
         CALL E1MES (5, 1, 'Both the number of rows and the '//
     &               'number of columns of the input matrix have to '//
     &               'be positive while NRA = %(I1) and NCA = %(I2) '//
     &               'are given.')
      ELSE
         INDA = I1KGT(NRA*NCA,3)
         INDW = I1KGT(NRA+NCA+MAX0(NCA,NRA)-1,3)
         IF (N1RCD(0) .NE. 0) THEN
            CALL E1MES (5, 2, ' ')
            CALL E1STI (1, NRA)
            CALL E1STI (2, NCA)
            CALL E1MES (5, 2, 'The workspace is based on NRA and NCA '//
     &                  'where NRA = %(I1) and NCA = %(I2) are '//
     &                  'given.')
         ELSE
            CALL L2VRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK, S, U,
     &                  LDU, V, LDV, RDWKSP(INDA), RDWKSP(INDW))
         END IF
      END IF
C
      CALL E1POP ('LSVRR ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  L2VRR/DL2VRR (Single/Double precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    January 1, 1985
C
C  Purpose:    Compute the singular value decomposition of a real
C              matrix.
C
C  Usage:      CALL L2VRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK,
C                          S, U, LDU, V, LDV, WKA, WK)
C
C  Arguments:  See LSVRR/DLSVRR.
C
C  Remarks:    See LSVRR/DLSVRR.
C
C  Chapter:    MATH/LIBRARY Linear Systems
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE L2VRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK, S, U,
     &                  LDU, V, LDV, WKA, WK)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NRA, NCA, LDA, IPATH, IRANK, LDU, LDV
      REAL       TOL, A(LDA,*), S(*), U(*), V(*), WKA(*), WK(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, INDE, INDW, JOBU, JOBV
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, SCOPY, L3VRR
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   N1RCD
      INTEGER    N1RCD
C
      CALL E1PSH ('L2VRR   ')
C
      IF (NRA.LE.0 .OR. NCA.LE.0) THEN
         CALL E1STI (1, NRA)
         CALL E1STI (2, NCA)
         CALL E1MES (5, 1, 'Both the number of rows and the '//
     &               'number of columns of the input matrix have to '//
     &               'be positive while NRA = %(I1) and NCA = %(I2) '//
     &               'are given.')
      ELSE
         IF (NRA .GT. LDA) THEN
            CALL E1STI (1, NRA)
            CALL E1STI (2, LDA)
            CALL E1MES (5, 2, 'The number of rows of A must be '//
     &                  'less than or equal to its leading dimension '//
     &                  'while NRA = %(I1) and LDA = %(I2) are '//
     &                  'given.')
         END IF
C
         IF (LDU .LE. 0) THEN
            CALL E1STI (1, LDU)
            CALL E1MES (5, 3, 'The leading dimension of U must '//
     &                  'be greater than zero.  LDU = %(I1) is '//
     &                  'given.')
         END IF
C
         IF (LDV .LE. 0) THEN
            CALL E1STI (1, LDV)
            CALL E1MES (5, 4, 'The leading dimension of V must '//
     &                  'be greater than zero.  LDV = %(I1) is '//
     &                  'given.')
         END IF
C
         IF (N1RCD(0) .EQ. 0) THEN
            JOBU = MOD(IPATH,100)/10
            JOBV = MOD(IPATH,10)
            IF ((JOBU.NE.0.AND.JOBU.NE.1.AND.JOBU.NE.2) .OR.
     &          (JOBV.NE.0.AND.JOBV.NE.1)) THEN
               CALL E1STI (1, JOBU)
               CALL E1STI (2, JOBV)
               CALL E1MES (5, 7, 'Error in computation control flag. '//
     &                     'The IJ decimal expansion of IPATH is I = '//
     &                     '%(I1) and J = %(I2).  I must be either '//
     &                     '0, 1 or 2 and J must be either 0 or 1.')
            ELSE
C                                  MAKE A COPY OF A IN WKA AND WORK
C                                  WITH WKA ONLY.
               DO 10  I=1, NCA
                  CALL SCOPY (NRA, A(1,I), 1, WKA((I-1)*NRA+1), 1)
   10          CONTINUE
C
               INDE = 1
               INDW = NCA + 1
               CALL L3VRR (NRA, NCA, WKA, NRA, IPATH, TOL, IRANK, S,
     &                     U, LDU, V, LDV, WK(INDE), WK(INDW))
            END IF
         END IF
      END IF
C
      CALL E1POP ('L2VRR   ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  L3VRR/DL3VRR (Single/Double precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    January 1, 1985
C
C  Purpose:    Compute the singular value decomposition of a real
C              matrix.
C
C  Usage:      CALL L3VRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK,
C                          S, U, LDU, V, LDV, E, WORK)
C
C  Arguments:  See LSVRR/DLSVRR
C
C  Remarks:    See LSVRR/DLSVRR.
C
C  Chapter:    MATH/LIBRARY Linear Systems
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE L3VRR (NRA, NCA, A, LDA, IPATH, TOL, IRANK, S, U,
     &                  LDU, V, LDV, E, WORK)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NRA, NCA, LDA, IPATH, IRANK, LDU, LDV
      REAL       TOL, A(LDA,*), S(*), U(LDU,*), V(LDV,*), E(*), WORK(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IEND, INFO, ISTART, ITER, J, JOBU, K, KASE, KK, L,
     &           LL, LLS, LM1, LP1, LS, LU, M, MAXIT, MINMN, MM, MM1,
     &           MP1, NCT, NCTP1, NCU, NRT, NRTP1
      REAL       ANORM, B, C, CS, EL, EMM1, F, G, SCALE, SHIFT, SL,
     &           SM, SMM1, SN, STOL, T, T1, TEST, ZTEST
      LOGICAL    WANTU, WANTV
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  ABS,AMAX1,MAX0,MIN0,MOD,SIGN,SQRT
      INTRINSIC  ABS, AMAX1, MAX0, MIN0, MOD, SIGN, SQRT
      INTEGER    MAX0, MIN0, MOD
      REAL       ABS, AMAX1, SIGN, SQRT
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, SCOPY, SGEMV, SGER, SROT,
     &           SROTG, SSCAL, SSET, SSWAP
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   SASUM, SNRM2
      REAL       SASUM, SNRM2
C
      CALL E1PSH ('L3VRR   ')
C                                  IF TOL.LT.0, COMPUTE THE INFINITY
C                                  NORM OF A AND SINGULAR VALUE
C                                  TOLERANCE FOR RANK ESTIMATION
      IF (TOL .LT. 0.0E0) THEN
         ANORM = 0.0E0
         DO 10  I=1, NRA
            ANORM = AMAX1(ANORM,SASUM(NCA,A(I,1),LDA))
   10    CONTINUE
         STOL = ABS(TOL)*ANORM
      ELSE
         STOL = TOL
      END IF
C                                  SET THE MAXIMUM NUMBER OF
C                                  ITERATIONS.
      MAXIT = 30
C                                  DETERMINE WHAT IS TO BE COMPUTED.
      WANTU = .FALSE.
      WANTV = .FALSE.
      JOBU = MOD(IPATH,100)/10
      NCU = NRA
      IF (JOBU .EQ. 2) NCU = MIN0(NRA,NCA)
      IF (JOBU .NE. 0) WANTU = .TRUE.
      IF (MOD(IPATH,10) .NE. 0) WANTV = .TRUE.
C                                  REDUCE A TO BIDIAGONAL FORM, STORING
C                                  THE DIAGONAL ELEMENTS IN S AND THE
C                                  SUPER-DIAGONAL ELEMENTS IN E.
      INFO = 0
      NCT = MIN0(NRA-1,NCA)
      NRT = MAX0(0,MIN0(NCA-2,NRA))
      LU = MAX0(NCT,NRT)
      IF (LU .GE. 1) THEN
         DO 20  L=1, LU
            LP1 = L + 1
            IF (L .LE. NCT) THEN
C                                  COMPUTE THE TRANSFORMATION FOR THE
C                                  L-TH NCA AND PLACE THE L-TH
C                                  DIAGONAL IN S(L).
               S(L) = SNRM2(NRA-L+1,A(L,L),1)
               IF (S(L) .NE. 0.0E0) THEN
                  IF (A(L,L) .NE. 0.0E0) S(L) = SIGN(S(L),A(L,L))
                  CALL SSCAL (NRA-L+1, 1.0E0/S(L), A(L,L), 1)
                  A(L,L) = 1.0E0 + A(L,L)
               END IF
               S(L) = -S(L)
            END IF
C
            IF (NCA .GE. LP1) THEN
               IF (L.LE.NCT .AND. S(L).NE.0.0E0) THEN
C                                  APPLY THE TRANSFORMATION.
                  CALL SGEMV ('T', NRA-L+1, NCA-LP1+1, -1.0E0/A(L,L),
     &                        A(L,LP1), LDA, A(L,L), 1, 0.0E0,
     &                        WORK(NRA+1), 1)
                  CALL SGER (NRA-L+1, NCA-LP1+1, 1.0E0, A(L,L), 1,
     &                       WORK(NRA+1), 1, A(L,LP1), LDA)
               END IF
C                                  PLACE THE L-TH ROW OF A INTO E FOR
C                                  THE SUBSEQUENT CALCULATION OF THE
C                                  ROW TRANSFORMATION.
               CALL SCOPY (NCA-LP1+1, A(L,LP1), LDA, E(LP1), 1)
            END IF
C                                  PLACE THE TRANSFORMATION IN U FOR
C                                  SUBSEQUENT BACK MULTIPLICATION.
            IF (WANTU .AND. L.LE.NCT) CALL SCOPY (NRA-L+1, A(L,L), 1,
     &          U(L,L), 1)
            IF (L .LE. NRT) THEN
C                                  COMPUTE THE L-TH ROW TRANSFORMATION
C                                  AND PLACE THE L-TH SUPER-DIAGONAL
C                                  IN E(L).
               E(L) = SNRM2(NCA-L,E(LP1),1)
               IF (E(L) .NE. 0.0E0) THEN
                  IF (E(LP1) .NE. 0.0E0) E(L) = SIGN(E(L),E(LP1))
                  CALL SSCAL (NCA-L, 1.0E0/E(L), E(LP1), 1)
                  E(LP1) = 1.0E0 + E(LP1)
               END IF
               E(L) = -E(L)
               IF (LP1.LE.NRA .AND. E(L).NE.0.0E0) THEN
C                                  APPLY THE TRANSFORMATION.
                  CALL SGEMV ('N', NRA-L, NCA-LP1+1, 1.0E0,
     &                        A(LP1,LP1), LDA, E(LP1), 1, 0.0E0,
     &                        WORK(LP1), 1)
                  CALL SGER (NRA-L, NCA-LP1+1, -1.0E0/E(LP1),
     &                       WORK(LP1), 1, E(LP1), 1, A(LP1,LP1), LDA)
               END IF
C                                  PLACE THE TRANSFORMATION IN V FOR
C                                  SUBSEQUENT BACK MULTIPLICATION.
               IF (WANTV) CALL SCOPY (NCA-LP1+1, E(LP1), 1, V(LP1,L),
     &             1)
            END IF
   20    CONTINUE
      END IF
C                                  SET UP THE FINAL BIDIAGONAL MATRIX
C                                  OR ORDER M.
      M = MIN0(NCA,NRA+1)
      NCTP1 = NCT + 1
      NRTP1 = NRT + 1
      IF (NCT .LT. NCA) S(NCTP1) = A(NCTP1,NCTP1)
      IF (NRA .LT. M) S(M) = 0.0E0
      IF (NRTP1 .LT. M) E(NRTP1) = A(NRTP1,M)
      E(M) = 0.0E0
C                                  IF REQUIRED, GENERATE U.
      IF (WANTU) THEN
         IF (NCU .GE. NCTP1) THEN
            DO 30  J=NCTP1, NCU
               CALL SSET (NRA, 0.0E0, U(1,J), 1)
               U(J,J) = 1.0E0
   30       CONTINUE
         END IF
         IF (NCT .GE. 1) THEN
            DO 40  LL=1, NCT
               L = NCT - LL + 1
               IF (S(L) .NE. 0.0E0) THEN
                  LP1 = L + 1
                  IF (NCU .GE. LP1) THEN
                     CALL SGEMV ('T', NRA-L+1, NCU-LP1+1,
     &                           -1.0E0/U(L,L), U(L,LP1), LDU, U(L,L),
     &                           1, 0.0E0, WORK(NRA+1), 1)
                     CALL SGER (NRA-L+1, NCU-LP1+1, 1.0E0, U(L,L), 1,
     &                          WORK(NRA+1), 1, U(L,LP1), LDU)
                  END IF
                  CALL SSCAL (NRA-L+1, -1.0E0, U(L,L), 1)
                  U(L,L) = 1.0E0 + U(L,L)
                  LM1 = L - 1
                  IF (LM1 .GE. 1) CALL SSET (LM1, 0.0E0, U(1,L), 1)
               ELSE
                  CALL SSET (NRA, 0.0E0, U(1,L), 1)
                  U(L,L) = 1.0E0
               END IF
   40       CONTINUE
         END IF
      END IF
C                                  IF IT IS REQUIRED, GENERATE V.
      IF (WANTV) THEN
         DO 50  LL=1, NCA
            L = NCA - LL + 1
            LP1 = L + 1
            IF (L.LE.NRT .AND. E(L).NE.0.0E0) THEN
               CALL SGEMV ('T', NCA-L, NCA-LP1+1, -1.0E0/V(LP1,L),
     &                     V(LP1,LP1), LDV, V(LP1,L), 1, 0.0E0,
     &                     WORK(NRA+1), 1)
               CALL SGER (NCA-L, NCA-LP1+1, 1.0E0, V(LP1,L), 1,
     &                    WORK(NRA+1), 1, V(LP1,LP1), LDV)
            END IF
            CALL SSET (NCA, 0.0E0, V(1,L), 1)
            V(L,L) = 1.0E0
   50    CONTINUE
      END IF
C                                  MAIN ITERATION LOOP FOR THE SINGULAR
C                                  VALUES.
      MM = M
      ITER = 0
   60 CONTINUE
C                                  QUIT IF ALL THE SINGULAR VALUES HAVE
C                                  BEEN FOUND.
      IF (M.NE.0 .AND. ITER.LE.MAXIT) THEN
C                                  THIS SECTION OF THE PROGRAM INSPECTS
C                                  FOR NEGLIGIBLE ELEMENTS IN THE S AND
C                                  E ARRAYS. ON COMPLETION THE VARIABLES
C                                  KASE AND L ARE SET AS FOLLOWS.
C
C                                  KASE = 1     IF S(M) AND E(L-1) ARE
C                                  NEGLIGIBLE AND L.LT.M
C
C                                  KASE = 2     IF S(L) IS NEGLIGIBLE
C                                  AND L.LT.M
C
C                                  KASE = 3     IF E(L-1) IS NEGLIGIBLE,
C                                  L.LT.M, AND S(L), ..., S(M) ARE NOT
C                                  NEGLIGIBLE (QR STEP).
C
C                                  KASE = 4     IF E(M-1) IS NEGLIGIBLE
C                                  (CONVERGENCE).
         DO 70  LL=1, M
            L = M - LL
            IF (L .EQ. 0) GO TO 80
            TEST = ABS(S(L)) + ABS(S(L+1))
            ZTEST = TEST + ABS(E(L))
            IF (ZTEST .EQ. TEST) THEN
               E(L) = 0.0E0
               GO TO 80
            END IF
   70    CONTINUE
   80    CONTINUE
         IF (L .EQ. M-1) THEN
            KASE = 4
         ELSE
            LP1 = L + 1
            MP1 = M + 1
            DO 90  LLS=LP1, MP1
               LS = M - LLS + LP1
               IF (LS .EQ. L) GO TO 100
               TEST = 0.0E0
               IF (LS .NE. M) TEST = TEST + ABS(E(LS))
               IF (LS .NE. L+1) TEST = TEST + ABS(E(LS-1))
               ZTEST = TEST + ABS(S(LS))
               IF (ZTEST .EQ. TEST) THEN
                  S(LS) = 0.0E0
                  GO TO 100
               END IF
   90       CONTINUE
  100       CONTINUE
            IF (LS .EQ. L) THEN
               KASE = 3
            ELSE IF (LS .EQ. M) THEN
               KASE = 1
            ELSE
               KASE = 2
               L = LS
            END IF
         END IF
         L = L + 1
C                                  PERFORM THE TASK INDICATED BY KASE.
         IF (KASE .EQ. 1) THEN
C                                  DEFLATE NEGLIGIBLE S(M).
            MM1 = M - 1
            F = E(M-1)
            E(M-1) = 0.0E0
            DO 110  KK=L, MM1
               K = MM1 - KK + L
               T1 = S(K)
               CALL SROTG (T1, F, CS, SN)
               S(K) = T1
               IF (K .NE. L) THEN
                  F = -SN*E(K-1)
                  E(K-1) = CS*E(K-1)
               END IF
               IF (WANTV) CALL SROT (NCA, V(1,K), 1, V(1,M), 1, CS, SN)
  110       CONTINUE
C                                  SPLIT AT NEGLIGIBLE S(L).
         ELSE IF (KASE .EQ. 2) THEN
            F = E(L-1)
            E(L-1) = 0.0E0
            DO 120  K=L, M
               T1 = S(K)
               CALL SROTG (T1, F, CS, SN)
               S(K) = T1
               F = -SN*E(K)
               E(K) = CS*E(K)
               IF (WANTU) CALL SROT (NRA, U(1,K), 1, U(1,L-1), 1, CS,
     &             SN)
  120       CONTINUE
C                                  PERFORM ONE QR STEP.
         ELSE IF (KASE .EQ. 3) THEN
C                                  CALCULATE THE SHIFT.
            SCALE = AMAX1(ABS(S(M)),ABS(S(M-1)),ABS(E(M-1)),ABS(S(L)),
     &              ABS(E(L)))
            SM = S(M)/SCALE
            SMM1 = S(M-1)/SCALE
            EMM1 = E(M-1)/SCALE
            SL = S(L)/SCALE
            EL = E(L)/SCALE
            B = ((SMM1+SM)*(SMM1-SM)+EMM1**2)/2.0E0
            C = (SM*EMM1)**2
            SHIFT = 0.0E0
            IF (B.NE.0.0E0 .OR. C.NE.0.0E0) THEN
               SHIFT = SQRT(B**2+C)
               IF (B .LT. 0.0E0) SHIFT = -SHIFT
               SHIFT = C/(B+SHIFT)
            END IF
            F = (SL+SM)*(SL-SM) - SHIFT
            G = SL*EL
C                                  CHASE ZEROS.
            MM1 = M - 1
            DO 130  K=L, MM1
               CALL SROTG (F, G, CS, SN)
               IF (K .NE. L) E(K-1) = F
               F = CS*S(K) + SN*E(K)
               E(K) = CS*E(K) - SN*S(K)
               G = SN*S(K+1)
               S(K+1) = CS*S(K+1)
               IF (WANTV) CALL SROT (NCA, V(1,K), 1, V(1,K+1), 1, CS,
     &             SN)
               CALL SROTG (F, G, CS, SN)
               S(K) = F
               F = CS*E(K) + SN*S(K+1)
               S(K+1) = -SN*E(K) + CS*S(K+1)
               G = SN*E(K+1)
               E(K+1) = CS*E(K+1)
               IF (WANTU .AND. K.LT.NRA) CALL SROT (NRA, U(1,K), 1,
     &             U(1,K+1), 1, CS, SN)
  130       CONTINUE
            E(M-1) = F
            ITER = ITER + 1
C                                  CONVERGENCE.
         ELSE IF (KASE .EQ. 4) THEN
C                                  MAKE THE SINGULAR VALUE POSITIVE.
            IF (S(L) .LT. 0.0E0) THEN
               S(L) = -S(L)
               IF (WANTV) CALL SSCAL (NCA, -1.0E0, V(1,L), 1)
            END IF
C                                  ORDER THE SINGULAR VALUE.
  140       IF (L .EQ. MM) GO TO 150
            IF (S(L) .GE. S(L+1)) GO TO 150
            T = S(L)
            S(L) = S(L+1)
            S(L+1) = T
            IF (WANTV .AND. L.LT.NCA) CALL SSWAP (NCA, V(1,L), 1, V(1,
     &          L+1), 1)
            IF (WANTU .AND. L.LT.NRA) CALL SSWAP (NRA, U(1,L), 1, U(1,
     &          L+1), 1)
            L = L + 1
            GO TO 140
  150       CONTINUE
            ITER = 0
            M = M - 1
         END IF
         GO TO 60
      END IF
      IF (ITER .GT. MAXIT) INFO = M
      IF (INFO .EQ. 0) THEN
C                                  ESTIMATE THE RANK OF A
         LS = MIN0(NCA+1,NRA)
         MINMN = MIN0(NCA,NRA)
         IRANK = MINMN
         DO 160  I=1, MINMN
            IF (ABS(S(I)) .LE. STOL) THEN
               IRANK = I - 1
               GO TO 170
            END IF
  160    CONTINUE
  170    CONTINUE
      ELSE
         ISTART = INFO + 1
         IEND = MIN0(NRA,NCA)
         CALL E1STI (1, ISTART)
         CALL E1STI (2, IEND)
C         CALL E1MES (4, 1, 'Convergence can only be obtained '//
C     &               'for the %(I1), ... , %(I2) singular values '//
C     &               'and their corresponding singular vectors.')
      END IF
      CALL E1POP ('L3VRR ')
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1MES
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    Set an error state for the current level in the stack.
C              The message is printed immediately if the error type is
C              5, 6, or 7 and the print attribute for that type is YES.
C
C  Usage:      CALL E1MES(IERTYP,IERCOD,MSGPKD)
C
C  Arguments:
C     IERTYP - Integer specifying the error type.  (Input)
C                IERTYP=1,  informational/note
C                IERTYP=2,  informational/alert
C                IERTYP=3,  informational/warning
C                IERTYP=4,  informational/fatal
C                IERTYP=5,  terminal
C                IERTYP=6,  PROTRAN/warning
C                IERTYP=7,  PROTRAN/fatal
C     IERCOD - Integer specifying the error code.  (Input)
C     MSGPKD - A character string containing the message.
C              (Input)  Within the message, any of following may appear
C                %(A1),%(A2),...,%(A9) for character arrays
C                %(C1),%(C2),...,%(C9) for complex numbers
C                %(D1),%(D2),...,%(D9) for double precision numbers
C                %(I1),%(I2),...,%(I9) for integer numbers
C                %(K1),%(K2),...,%(K9) for keywords
C                %(L1),%(L2),...,%(L9) for literals (strings)
C                %(R1),%(R2),...,%(R9) for real numbers
C                %(Z1),%(Z2),...,%(Z9) for double complex numbers
C              This provides a way to insert character arrays, strings,
C              numbers, and keywords into the message.  See remarks
C              below.
C
C  Remarks:
C     The number of characters in the message after the insertion of
C     the corresponding strings, etc. should not exceed 255.  If the
C     limit is exceeded, only the first 255 characters will be used.
C     The appropriate strings, etc. need to have been previously stored
C     in common via calls to E1STA, E1STD, etc.  Line breaks may be
C     specified by inserting the two characters '%/' into the message
C     at the desired locations.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1MES (IERTYP, IERCOD, MSGPKD)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IERTYP, IERCOD
      CHARACTER  MSGPKD*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ERTYP2, I, IER, IPLEN, ISUB, LAST, LEN2, LOC, M, MS,
     &           NLOC, NUM, PBEG
      CHARACTER  MSGTMP(255)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT, NFORMS
      CHARACTER  BLNK, DBB(3), FIND(4), FORMS(9), INREF(25), LPAR,
     &           NCHECK(3), PERCNT, RPAR
      SAVE       BLNK, DBB, FIND, FORMS, IFINIT, INREF, LPAR, NCHECK,
     &           NFORMS, PERCNT, RPAR
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  LEN,MIN0
      INTRINSIC  LEN, MIN0
      INTEGER    LEN, MIN0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TCI, E1INIT, E1PRT, E1UCS, M1VE, M1VECH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1DX
      INTEGER    I1DX
C
      DATA FORMS/'A', 'C', 'D', 'I', 'K', 'L', 'R', 'S', 'Z'/,
     &     NFORMS/9/
      DATA PERCNT/'%'/, LPAR/'('/, RPAR/')'/, BLNK/' '/
      DATA INREF/' ', 'i', 'n', ' ', 'r', 'e', 'f', 'e', 'r',
     &     'e', 'n', 'c', 'e', ' ', 't', 'o', ' ', 'k', 'e',
     &     'y', 'w', 'o', 'r', 'd', ' '/
      DATA NCHECK/'N', '1', '*'/, DBB/'.', ' ', ' '/
      DATA FIND/'*', ' ', ' ', '*'/
      DATA IFINIT/0/
C                                  INITIALIZE ERROR TABLE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
C                                  CHECK AND SET ERROR TYPE IF NECESSARY
      IF (IERTYP .NE. -1) THEN
         ERTYPE(CALLVL) = IERTYP
      ELSE IF (IERTYP.LT.-1 .OR. IERTYP.GT.7) THEN
         MSGLEN = 51
         CALL M1VECH ('.  Error from E1MES.  Illegal error type'//
     &                ' specified. ', MSGLEN, MSGSAV, MSGLEN)
         CALL E1PRT
         STOP
      END IF
C
      ERTYP2 = ERTYPE(CALLVL)
C                                  SET ERROR CODE IF NECESSARY
      IF (IERCOD .GT. -1) ERCODE(CALLVL) = IERCOD
      LEN2 = LEN(MSGPKD)
C
      IF (IERTYP.EQ.0 .OR. IERCOD.EQ.0) THEN
C                                  REMOVE THE ERROR STATE
         MSGLEN = 0
      ELSE IF (LEN2.EQ.0 .OR. (LEN2.EQ.1.AND.MSGPKD(1:1).EQ.BLNK)) THEN
         IF (ERTYP2 .EQ. 6) IFERR6 = 1
         IF (ERTYP2 .EQ. 7) IFERR7 = 1
C                                  UPDATE CHECKSUM PARAMETER ERCKSM
         CALL E1UCS
C                                  PRINT MESSAGE IF NECESSARY
         IF (ERTYP2.GE.5 .AND. PRINTB(ERTYP2).EQ.1) CALL E1PRT
      ELSE
C                                  FILL UP MSGSAV WITH EXPANDED MESSAGE
         LEN2 = MIN0(LEN2,255)
         DO 10  I=1, LEN2
            MSGTMP(I) = MSGPKD(I:I)
   10    CONTINUE
         MS = 0
         M = 0
C                                  CHECK PLIST FOR KEYWORD NAME
         NLOC = I1DX(PLIST,PLEN,NCHECK,3)
         IF (NLOC.GT.0 .AND. HDRFMT(ERTYP2).EQ.3) THEN
C                                  M1VE INREF INTO MSGSAV
            CALL M1VE (INREF, 1, 25, 25, MSGSAV, 1, 25, 25, IER)
C                                  GET LENGTH OF KEYWORD NAME
            CALL C1TCI (PLIST(NLOC+3), 3, IPLEN, IER)
            PBEG = NLOC + 3 + IER
C                                  M1VE KEYWORD NAME INTO MSGSAV
            CALL M1VE (PLIST, PBEG, PBEG+IPLEN-1, PLEN, MSGSAV, 26,
     &                 IPLEN+25, 255, IER)
C                                  UPDATE POINTER
            MS = IPLEN + 25
         END IF
C                                  INSERT DOT, BLANK, BLANK
         CALL M1VE (DBB, 1, 3, 3, MSGSAV, MS+1, MS+3, 255, IER)
         MS = MS + 3
C                                  LOOK AT NEXT CHARACTER
   20    M = M + 1
         ISUB = 0
         IF (M .GT. LEN2-4) THEN
            LAST = LEN2 - M + 1
            DO 30  I=1, LAST
   30       MSGSAV(MS+I) = MSGTMP(M+I-1)
            MSGLEN = MS + LAST
            GO TO 40
         ELSE IF (MSGTMP(M).EQ.PERCNT .AND. MSGTMP(M+1).EQ.LPAR .AND.
     &           MSGTMP(M+4).EQ.RPAR) THEN
            CALL C1TCI (MSGTMP(M+3), 1, NUM, IER)
            IF (IER.EQ.0 .AND. NUM.NE.0 .AND. I1DX(FORMS,NFORMS,
     &          MSGTMP(M+2),1).NE.0) THEN
C                                  LOCATE THE ITEM IN THE PARAMETER LIST
               CALL M1VE (MSGTMP(M+2), 1, 2, 2, FIND, 2, 3, 4, IER)
               LOC = I1DX(PLIST,PLEN,FIND,4)
               IF (LOC .GT. 0) THEN
C                                  SET IPLEN = LENGTH OF STRING
                  CALL C1TCI (PLIST(LOC+4), 4, IPLEN, IER)
                  PBEG = LOC + 4 + IER
C                                  ADJUST IPLEN IF IT IS TOO BIG
                  IPLEN = MIN0(IPLEN,255-MS)
C                                  M1VE STRING FROM PLIST INTO MSGSAV
                  CALL M1VE (PLIST, PBEG, PBEG+IPLEN-1, PLEN, MSGSAV,
     &                       MS+1, MS+IPLEN, 255, IER)
                  IF (IER.GE.0 .AND. IER.LT.IPLEN) THEN
C                                  UPDATE POINTERS
                     M = M + 4
                     MS = MS + IPLEN - IER
C                                  BAIL OUT IF NO MORE ROOM
                     IF (MS .GE. 255) THEN
                        MSGLEN = 255
                        GO TO 40
                     END IF
C                                  SET FLAG TO SHOW SUBSTITION WAS MADE
                     ISUB = 1
                  END IF
               END IF
            END IF
         END IF
         IF (ISUB .EQ. 0) THEN
            MS = MS + 1
            MSGSAV(MS) = MSGTMP(M)
         END IF
         GO TO 20
   40    ERTYP2 = ERTYPE(CALLVL)
         IF (ERTYP2 .EQ. 6) IFERR6 = 1
         IF (ERTYP2 .EQ. 7) IFERR7 = 1
C                                  UPDATE CHECKSUM PARAMETER ERCKSM
         CALL E1UCS
C                                  PRINT MESSAGE IF NECESSARY
         IF (ERTYP2.GE.5 .AND. PRINTB(ERTYP2).EQ.1) CALL E1PRT
      END IF
C                                  CLEAR PARAMETER LIST
      PLEN = 1
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1POP
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 13, 1984
C
C  Purpose:    To pop a subroutine name from the error control stack.
C
C  Usage:      CALL E1POP(NAME)
C
C  Arguments:
C     NAME   - A character string of length six specifying the name
C              of the subroutine.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1POP (NAME)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  NAME*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IERTYP, IR
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1PRT, E1PSH, E1STI, E1STL, I1KRL
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KST
      INTEGER    I1KST
C
      IF (CALLVL .LE. 1) THEN
         CALL E1PSH ('E1POP ')
         CALL E1STL (1, NAME)
         CALL E1MES (5, 1, 'Error condition in E1POP.  Cannot pop '//
     &               'from %(L1) because stack is empty.')
         STOP
      ELSE IF (NAME .NE. RNAME(CALLVL)) THEN
         CALL E1STL (1, NAME)
         CALL E1STL (2, RNAME(CALLVL))
         CALL E1MES (5, 2, 'Error condition in E1POP.  %(L1) does '//
     &               'not match the name %(L2) in the stack.')
         STOP
      ELSE
         IERTYP = ERTYPE(CALLVL)
         IF (IERTYP .NE. 0) THEN
C                                  M1VE ERROR TYPE AND ERROR CODE TO
C                                    PREVIOUS LEVEL FOR ERROR TYPES 2-7
            IF (IERTYP.GE.2 .AND. IERTYP.LE.7) THEN
               ERTYPE(CALLVL-1) = ERTYPE(CALLVL)
               ERCODE(CALLVL-1) = ERCODE(CALLVL)
            END IF
C                                  CHECK PRINT TABLE TO DETERMINE
C                                    WHETHER TO PRINT STORED MESSAGE
            IF (IERTYP .LE. 4) THEN
               IF (ISUSER(CALLVL-1) .AND. PRINTB(IERTYP).EQ.1)
     &             CALL E1PRT
            ELSE
               IF (PRINTB(IERTYP) .EQ. 1) CALL E1PRT
            END IF
C                                  CHECK STOP TABLE AND ERROR TYPE TO
C                                    DETERMINE WHETHER TO STOP
            IF (IERTYP .LE. 4) THEN
               IF (ISUSER(CALLVL-1) .AND. STOPTB(IERTYP).EQ.1) THEN
                  STOP
               END IF
            ELSE IF (IERTYP .EQ. 5) THEN
               IF (STOPTB(IERTYP) .EQ. 1) THEN
                  STOP
               END IF
            ELSE IF (HDRFMT(IERTYP) .EQ. 1) THEN
               IF (ISUSER(CALLVL-1)) THEN
                  IF (N1RGB(0) .NE. 0) THEN
                     STOP
                  END IF
               END IF
            END IF
         END IF
C                                  SET ERROR TYPE AND CODE
         IF (CALLVL .LT. MAXLEV) THEN
            ERTYPE(CALLVL+1) = -1
            ERCODE(CALLVL+1) = -1
         END IF
C                                  SET IR = AMOUNT OF WORKSPACE
C                                  ALLOCATED AT THIS LEVEL
         IR = I1KST(1) - IALLOC(CALLVL-1)
         IF (IR .GT. 0) THEN
C                                  RELEASE WORKSPACE
            CALL I1KRL (IR)
            IALLOC(CALLVL) = 0
         ELSE IF (IR .LT. 0) THEN
            CALL E1STI (1, CALLVL)
            CALL E1STI (2, IALLOC(CALLVL-1))
            CALL E1STI (3, I1KST(1))
            CALL E1MES (5, 3, 'Error condition in E1POP. '//
     &                  ' The number of workspace allocations at '//
     &                  'level %(I1) is %(I2).  However, the total '//
     &                  'number of workspace allocations is %(I3).')
            STOP
         END IF
C                                  DECREASE THE STACK POINTER BY ONE
         CALLVL = CALLVL - 1
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1PSH
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    To push a subroutine name onto the error control stack.
C
C  Usage:      CALL E1PSH(NAME)
C
C  Arguments:
C     NAME   - A character string of length six specifing the name of
C              the subroutine.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1PSH (NAME)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  NAME*(*)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      SAVE       IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1MES, E1STI
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KST
      INTEGER    I1KST
C
      DATA IFINIT/0/
C                                  INITIALIZE ERROR TABLE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      IF (CALLVL .GE. MAXLEV) THEN
         CALL E1STI (1, MAXLEV)
         CALL E1MES (5, 1, 'Error condition in E1PSH.  Push would '//
     &               'cause stack level to exceed %(I1). ')
         STOP
      ELSE
C                                  STORE ALLOCATION LEVEL
         IALLOC(CALLVL) = I1KST(1)
C                                  INCREMENT THE STACK POINTER BY ONE
         CALLVL = CALLVL + 1
C                                  PUT SUBROUTINE NAME INTO STACK
         RNAME(CALLVL) = NAME
C                                  SET ERROR TYPE AND ERROR CODE
         ERTYPE(CALLVL) = 0
         ERCODE(CALLVL) = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1STI
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 6, 1984
C
C  Purpose:    To store an integer for subsequent use within an error
C              message.
C
C  Usage:      CALL E1STI(II, IVALUE)
C
C  Arguments:
C     II     - Integer specifying the substitution index.  II must be
C              between 1 and 9.  (Input)
C     IVALUE - The integer to be stored.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1STI (II, IVALUE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    II, IVALUE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IBEG, IER, ILEN
      CHARACTER  ARRAY(14)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      CHARACTER  BLANK(1)
      SAVE       BLANK, IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TIC, E1INIT, E1INPL
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1ERIF
      INTEGER    I1ERIF
C
      DATA BLANK/' '/, IFINIT/0/
C                                  INITIALIZE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      CALL C1TIC (IVALUE, ARRAY, 14, IER)
      IBEG = I1ERIF(ARRAY,14,BLANK,1)
      IF (II.GE.1 .AND. II.LE.9 .AND. IER.EQ.0) THEN
         ILEN = 15 - IBEG
         CALL E1INPL ('I', II, ILEN, ARRAY(IBEG))
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KGT
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    January 17, 1984
C
C  Purpose:    Allocate numerical workspace.
C
C  Usage:      I1KGT(NELMTS,ITYPE)
C
C  Arguments:
C     NELMTS - Number of elements of data type ITYPE to be
C              allocated.  (Input)
C     ITYPE  - Data type of array to be allocated.  (Input)
C                 1 - logical
C                 2 - integer
C                 3 - real
C                 4 - double precision
C                 5 - complex
C                 6 - double complex
C     I1KGT  - Integer function.  (Output)  Returns the index of the
C              first element in the current allocation.
C
C  Remarks:
C  1. On return, the array will occupy
C     WKSP(I1KGT), WKSP(I1KGT+1), ..., WKSP(I1KGT+NELMTS-1) where
C     WKSP is an array of data type ITYPE equivalenced to RWKSP.
C
C  2. If I1KGT is negative, the absolute value of I1KGT is the
C     additional workspace needed for the current allocation.
C
C  3. The allocator reserves the first sixteen integer locations of
C     the stack for its own internal bookkeeping.  These are initialized
C     by the function IWKIN upon the first call to the allocation
C     package.
C
C  4. The use of the first ten integer locations is as follows:
C      WKSP( 1) - LOUT    The number of current allocations
C      WKSP( 2) - LNOW    The current active length of the stack
C      WKSP( 3) - LUSED   The maximum value of WKSP(2) achieved
C                         thus far
C      WKSP( 4) - LBND    The lower bound of permanent storage which
C                         is one numeric storage unit more than the
C                         maximum allowed length of the stack.
C      WKSP( 5) - LMAX    The maximum length of the storage array
C      WKSP( 6) - LALC    The total number of allocations handled by
C                         I1KGT
C      WKSP( 7) - LNEED   The number of numeric storage units by which
C                         the array size must be increased for all past
C                         allocations to succeed
C      WKSP( 8) - LBOOK   The number of numeric storage units used for
C                         bookkeeping
C      WKSP( 9) - LCHAR   The pointer to the portion of the permanent
C                         stack which contains the bookkeeping and
C                         pointers for the character workspace
C                         allocation.
C      WKSP(10) - LLCHAR  The length of the array beginning at LCHAR
C                         set aside for character workspace bookkeeping
C                         and pointers.
C                 NOTE -  If character workspace is not being used,
C                         LCHAR and LLCHAR can be ignored.
C  5. The next six integer locations contain values describing the
C     amount of storage allocated by the allocation system to the
C     various data types.
C      WKSP(11) - Numeric storage units allocated to LOGICAL
C      WKSP(12) - Numeric storage units allocated to INTEGER
C      WKSP(13) - Numeric storage units allocated to REAL
C      WKSP(14) - Numeric storage units allocated to DOUBLE PRECISION
C      WKSP(15) - Numeric storage units allocated to COMPLEX
C      WKSP(16) - Numeric storage units allocated to DOUBLE COMPLEX
C
C  Copyright:  1984 by IMSL, Inc. All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1KGT (NELMTS, ITYPE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NELMTS, ITYPE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IDUMAL, IGAP, ILEFT, IPA, IPA7, ISA, ISA7,
     &           ISIZE(6), JTYPE, LALC, LBND, LBOOK, LMAX, LNEED,
     &           LNEED1, LNOW, LOUT, LUSED
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM8/
      INTEGER    PROLVL, XXLINE(10), XXPLEN(10), ICALOC(10), INALOC(10)
      COMMON     /ERCOM8/ PROLVL, XXLINE, XXPLEN, ICALOC, INALOC
      SAVE       /ERCOM8/
C                              SPECIFICATIONS FOR COMMON /ERCOM9/
      CHARACTER  XXPROC(10)*31
      COMMON     /ERCOM9/ XXPROC
      SAVE       /ERCOM9/
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(61913)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ RWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
      EQUIVALENCE (ISIZE(1), IWKSP(11))
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS,MAX0,MOD
      INTRINSIC  IABS, MAX0, MOD
      INTEGER    IABS, MAX0, MOD
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1POS, E1PSH, E1STI, IWKIN
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KQU
      INTEGER    I1KQU
C
      DATA FIRST/.TRUE./
C
      CALL E1PSH ('I1KGT ')
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C                                  NUMBER OF ELEMENTS LESS THAN 0
      IF (NELMTS .LT. 0) THEN
         CALL E1STI (1, NELMTS)
         CALL E1MES (5, 2, 'Number of elements is not positive.%/'//
     &               'NELMTS = %(I1).')
         CALL E1POP ('I1KGT ')
         GO TO 9000
      END IF
C                                  ILLEGAL DATA TYPE REQUESTED
      IF (ITYPE.EQ.0 .OR. IABS(ITYPE).GE.7) THEN
         CALL E1MES (5, 3, 'Illegal data type requested.')
         CALL E1POP ('I1KGT ')
         GO TO 9000
      END IF
C                                  BOOKKEEPING OVERWRITTEN
      IF (LNOW.LT.LBOOK .OR. LNOW.GT.LUSED .OR. LUSED.GT.LMAX .OR.
     &    LNOW.GE.LBND .OR. LOUT.GT.LALC) THEN
         CALL E1MES (5, 4, 'One or more of the first eight '//
     &               'bookkeeping locations in IWKSP have been '//
     &               'overwritten.')
         CALL E1POP ('I1KGT ')
         GO TO 9000
      END IF
C
      CALL E1POP ('I1KGT ')
C                                  DETERMINE NUMBER OF LOCATIONS STILL
C                                  AVAILABLE FOR DATA TYPE ITYPE
C                                  NOTE: I1KQU ALLOWS FOR 2 INTEGER
C                                        POINTERS WHICH MUST BE HANDLED
C                                        ARTIFICIALLY IF ILEFT = 0.
      ILEFT = I1KQU(IABS(ITYPE))
C
      IF (ITYPE .GT. 0) THEN
C                                  RELEASABLE STORAGE
         IF (ILEFT .GE. NELMTS) THEN
            I1KGT = (LNOW*ISIZE(2)-1)/ISIZE(ITYPE) + 2
            I = ((I1KGT-1+NELMTS)*ISIZE(ITYPE)-1)/ISIZE(2) + 3
C                                  IWKSP(I-1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION. IWKSP(I) CONTAINS
C                                  LNOW FOR THE PREVIOUS ALLOCATION.
            IWKSP(I-1) = ITYPE
            IWKSP(I) = LNOW
            LOUT = LOUT + 1
            LALC = LALC + 1
            LNOW = I
            LUSED = MAX0(LUSED,LNOW)
            LNEED = 0
         ELSE
C                                  RELEASABLE STORAGE WAS REQUESTED
C                                  BUT THE STACK WOULD OVERFLOW.
C                                  THEREFORE, ALLOCATE RELEASABLE
C                                  SPACE THROUGH THE END OF THE STACK
            IF (LNEED .EQ. 0) THEN
               IDUMAL = (LNOW*ISIZE(2)-1)/ISIZE(ITYPE) + 2
               I = ((IDUMAL-1+ILEFT)*ISIZE(ITYPE)-1)/ISIZE(2) + 3
C                                  ADVANCE COUNTERS AND STORE POINTERS
C                                  IF THERE IS ROOM TO DO SO
               IF (I .LT. LBND) THEN
C                                  IWKSP(I-1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION. IWKSP(I) CONTAINS
C                                  LNOW FOR THE PREVIOUS ALLOCATION.
                  IWKSP(I-1) = ITYPE
                  IWKSP(I) = LNOW
                  LOUT = LOUT + 1
                  LALC = LALC + 1
                  LNOW = I
                  LUSED = MAX0(LUSED,LNOW)
               END IF
            END IF
C                                  CALCULATE AMOUNT NEEDED TO ACCOMODATE
C                                  THIS ALLOCATION REQUEST
            LNEED1 = (NELMTS-ILEFT)*ISIZE(ITYPE)
            IF (ILEFT .EQ. 0) THEN
               IGAP = ISIZE(ITYPE) - MOD(LNOW+LNEED,ISIZE(ITYPE))
               IF (IGAP .EQ. ISIZE(ITYPE)) IGAP = 0
               LNEED1 = LNEED1 + 2*ISIZE(2) + IGAP
            END IF
C                                  MODIFY LNEED ACCORDING TO THE SIZE
C                                  OF THE BASE BEING USED (D.P. HERE)
            LNEED = LNEED + ((LNEED1+ISIZE(3)-1)/ISIZE(3))
C                                  SINCE CURRENT ALLOCATION IS ILLEGAL,
C                                  RETURN THE NEGATIVE OF THE ADDITIONAL
C                                  AMOUNT NEEDED TO MAKE IT LEGAL
            I1KGT = -LNEED
         END IF
      ELSE
C                                  PERMANENT STORAGE
         IF (ILEFT .GE. NELMTS) THEN
            JTYPE = -ITYPE
            I1KGT = (LBND*ISIZE(2)-1)/ISIZE(JTYPE) + 1 - NELMTS
            I = ((I1KGT-1)*ISIZE(JTYPE))/ISIZE(2) - 1
C                                  IWKSP(I) CONTAINS LBND FOR PREVIOUS
C                                  PERMANENT STORAGE ALLOCATION.
C                                  IWKSP(I+1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION.
            IWKSP(I) = LBND
            IWKSP(I+1) = JTYPE
            LALC = LALC + 1
            LBND = I
            LNEED = 0
         ELSE
C                                  PERMANENT STORAGE WAS REQUESTED
C                                  BUT THE STACK WOULD OVERFLOW,
C                                  THEREFORE, ALLOCATE RELEASABLE
C                                  SPACE THROUGH THE END OF THE STACK
            IF (LNEED .EQ. 0) THEN
               JTYPE = -ITYPE
               IDUMAL = (LNOW*ISIZE(2)-1)/ISIZE(JTYPE) + 2
               I = ((IDUMAL-1+ILEFT)*ISIZE(JTYPE)-1)/ISIZE(2) + 3
C                                  ADVANCE COUNTERS AND STORE POINTERS
C                                  IF THERE IS ROOM TO DO SO
               IF (I .LT. LBND) THEN
C                                  IWKSP(I-1) CONTAINS THE DATA TYPE FOR
C                                  THIS ALLOCATION. IWKSP(I) CONTAINS
C                                  LNOW FOR THE PREVIOUS ALLOCATION.
                  IWKSP(I-1) = JTYPE
                  IWKSP(I) = LNOW
                  LOUT = LOUT + 1
                  LALC = LALC + 1
                  LNOW = I
                  LUSED = MAX0(LUSED,LNOW)
               END IF
            END IF
C                                  CALCULATE AMOUNT NEEDED TO ACCOMODATE
C                                  THIS ALLOCATION REQUEST
            LNEED1 = (NELMTS-ILEFT)*ISIZE(-ITYPE)
            IF (ILEFT .EQ. 0) THEN
               IGAP = ISIZE(-ITYPE) - MOD(LNOW+LNEED,ISIZE(-ITYPE))
               IF (IGAP .EQ. ISIZE(-ITYPE)) IGAP = 0
               LNEED1 = LNEED1 + 2*ISIZE(2) + IGAP
            END IF
C                                  MODIFY LNEED ACCORDING TO THE SIZE
C                                  OF THE BASE BEING USED (D.P. HERE)
            LNEED = LNEED + ((LNEED1+ISIZE(3)-1)/ISIZE(3))
C                                  SINCE CURRENT ALLOCATION IS ILLEGAL,
C                                  RETURN THE NEGATIVE OF THE ADDITIONAL
C                                  AMOUNT NEEDED TO MAKE IT LEGAL
            I1KGT = -LNEED
         END IF
      END IF
C                                  STACK OVERFLOW - UNRECOVERABLE ERROR
 9000 IF (LNEED .GT. 0) THEN
         CALL E1POS (-5, IPA, ISA)
         CALL E1POS (5, 0, 0)
         CALL E1POS (-7, IPA7, ISA7)
         CALL E1POS (7, 0, 0)
         CALL E1PSH ('I1KGT ')
         CALL E1STI (1, LNEED+(LMAX/ISIZE(3)))
         IF (XXLINE(PROLVL).GE.1 .AND. XXLINE(PROLVL).LE.999) THEN
            CALL E1MES (7, 1, 'Insufficient workspace for current '//
     &                  'allocation(s).  Correct by inserting the '//
     &                  'following PROTRAN line: $OPTIONS;WORKSPACE=%'//
     &                  '(I1)')
         ELSE
            CALL E1MES (5, 5, 'Insufficient workspace for current '//
     &                  'allocation(s). Correct by calling IWKIN '//
     &                  'from main program with the three following '//
     &                  'statements:  (REGARDLESS OF PRECISION)%/'//
     &                  '      COMMON /WORKSP/  RWKSP%/      REAL '//
     &                  'RWKSP(%(I1))%/      CALL IWKIN(%(I1))')
         END IF
         CALL E1POP ('I1KGT ')
         CALL E1POS (5, IPA, ISA)
         CALL E1POS (7, IPA7, ISA7)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  N1RCD
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 6, 1984
C
C  Purpose:    Retrieve an error code.
C
C  Usage:      N1RCD(IOPT)
C
C  Arguments:
C     IOPT   - Integer specifying the level.  (Input)
C              If IOPT=0 the error code for the current level is
C              returned.  If IOPT=1 the error code for the most
C              recently called routine (last pop) is returned.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION N1RCD (IOPT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IOPT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1PRT, M1VECH
C
      IF (IOPT.NE.0 .AND. IOPT.NE.1) THEN
         ERTYPE(CALLVL) = 5
         ERCODE(CALLVL) = 1
         MSGLEN = 47
         CALL M1VECH ('.  The argument passed to N1RCD must be 0 or '//
     &                '1. ', MSGLEN, MSGSAV, MSGLEN)
         CALL E1PRT
         STOP
      ELSE
         N1RCD = ERCODE(CALLVL+IOPT)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SASUM (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Sum the absolute values of the components of a
C              single precision vector.
C
C  Usage:      SASUM(N, SX, INCX)
C
C  Arguments:
C     N      - Length of vectors X.  (Input)
C     SX     - Real vector of length N*INCX.  (Input)
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be SX(1+(I-1)*INCX). INCX must be
C              greater than 0.
C     SASUM  - Single precision sum from I=1 to N of ABS(X(I)).
C              (Output)
C              X(I) refers to a specific element of SX.
C
C  GAMS:       D1a
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      REAL FUNCTION SASUM (N, SX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      REAL       SX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, M, MP1, NINCX
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  ABS
      INTRINSIC  ABS
      REAL       ABS
C
      SASUM = 0.0E0
      IF (N .GT. 0) THEN
         IF (INCX .NE. 1) THEN
C                                  CODE FOR INCREMENT NOT EQUAL TO 1
            NINCX = N*INCX
            DO 10  I=1, NINCX, INCX
               SASUM = SASUM + ABS(SX(I))
   10       CONTINUE
         ELSE
C                                  CODE FOR INCREMENT EQUAL TO 1
            M = MOD(N,6)
C                                  CLEAN-UP LOOP
            DO 30  I=1, M
               SASUM = SASUM + ABS(SX(I))
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 6
               SASUM = SASUM + ABS(SX(I)) + ABS(SX(I+1)) +
     &                 ABS(SX(I+2)) + ABS(SX(I+3)) + ABS(SX(I+4)) +
     &                 ABS(SX(I+5))
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SCOPY (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Copy a vector X to a vector Y, both single precision.
C
C  Usage:      CALL SCOPY (N, SX, INCX, SY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     SX     - Real vector of length MAX(N*IABS(INCX),1).  (Input)
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be.. SX(1+(I-1)*INCX) if INCX .GE. 0
C              or SX(1+(I-N)*INCX) if INCX .LT. 0.
C     SY     - Real vector of length MAX(N*IABS(INCY),1).  (Output)
C              SCOPY copies X(I) to Y(I) for I=1,...,N. X(I) and Y(I)
C              refer to specific elements of SX and SY, respectively.
C              See INCX and INCY argument descriptions.
C     INCY   - Displacement between elements of SY.  (Input)
C              Y(I) is defined to be.. SY(1+(I-1)*INCY) if INCY .GE. 0
C              or SY(1+(I-N)*INCY) if INCY .LT. 0.
C
C  GAMS:       D1a
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SCOPY (N, SX, INCX, SY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      REAL       SX(*), SY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               SY(IY) = SX(IX)
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
            M = MOD(N,7)
C                                  CLEAN-UP LOOP
            DO 30  I=1, M
               SY(I) = SX(I)
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 7
               SY(I) = SX(I)
               SY(I+1) = SX(I+1)
               SY(I+2) = SX(I+2)
               SY(I+3) = SX(I+3)
               SY(I+4) = SX(I+4)
               SY(I+5) = SX(I+5)
               SY(I+6) = SX(I+6)
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SGEMV  (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    July 15, 1986
C
C  Purpose:    Perform the matrix-vector multiplication
C              y = alpha*A*x + beta*y or y = alpha*A'*x + beta*y,
C              all single precision.
C
C  Usage:      CALL SGEMV (TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y,
C                          INCY)
C
C  Arguments:
C     TRANS  - Character specifing the operation to be performed.
C              (Input)
C                 TRANS               Operation
C              'N' or 'n'      y = alpha*A*x + beta*y
C              'T' or 't'      y = alpha*A'*x + beta*y
C              'C' or 'c'      y = alpha*A'*x + beta*y
C     M      - Number of rows in A.  (Input)
C     N      - Number of columns in A.  (Input)
C     ALPHA  - Scalar.  (Input)
C     A      - Array of size M by N.  (Input)
C     LDA    - Leading dimension of A exactly as specified in the
C              calling routine.  (Input)
C     X      - Vector of length (N-1)*IABS(INCX)+1 when TRANS is
C              'N' or 'n' and of length (M-1)*IABS(INCX)+1 otherwise.
C              (Input)
C     INCX   - Displacement between elements of X.  (Input)
C     BETA   - Scalar.  (Input)
C              When BETA is zero, Y is not referenced.
C     Y      - Vector of length (N-1)*IABS(INCY)+1 when TRANS is
C              'M' or 'm' and of length (M-1)*IABS(INCY)+1 otherwise.
C              (Input/Output)
C     INCY   - Displacement between elements of Y.  (Input)
C
C  GAMS:       D1b
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SGEMV (TRANS, M, N, ALPHA, A, LDA, X, INCX, BETA, Y,
     &                  INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    M, N, LDA, INCX, INCY
      REAL       ALPHA, BETA, X(*), Y(*)
      CHARACTER  TRANS*1
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, KY, LENX, LENY
C                                  SPECIFICATIONS FOR SPECIAL CASES
      REAL       A(*)
      EXTERNAL   SAXPY, SDOT
      INTEGER    KX
      REAL       SDOT
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   SSCAL, SSET
C                                  Quick return if possible
      IF (M.EQ.0 .OR. N.EQ.0 .OR. (ALPHA.EQ.0.0) .AND. (BETA.EQ.1.0))
     &    GO TO 9000
C
      IF (TRANS.EQ.'N' .OR. TRANS.EQ.'n') THEN
         LENX = N
         LENY = M
      ELSE
         LENX = M
         LENY = N
      END IF
C
      IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-LENX+1)*INCX + 1
      IF (INCY .LT. 0) IY = (-LENY+1)*INCY + 1
C
      IF (BETA .EQ. 1) THEN
      ELSE IF (INCY .EQ. 0) THEN
         IF (BETA .EQ. 0.0) THEN
            Y(1) = 0.0
         ELSE
            Y(1) = BETA**LENY*Y(1)
         END IF
      ELSE IF (BETA .EQ. 0.0) THEN
         CALL SSET (LENY, 0.0, Y, IABS(INCY))
      ELSE
         CALL SSCAL (LENY, BETA, Y, IABS(INCY))
      END IF
C
      IF (ALPHA .EQ. 0.0) GO TO 9000
C                                  Not transpose
      IF (TRANS.EQ.'N' .OR. TRANS.EQ.'n') THEN
         KX = IX
         DO 10  I=1, N
            CALL SAXPY (M, ALPHA*X(KX), A(LDA*(I-1)+1), 1, Y, INCY)
            KX = KX + INCX
   10    CONTINUE
      ELSE
C                                  Transpose
         KY = IY
         DO 20  I=1, N
            Y(KY) = Y(KY) + ALPHA*SDOT(M,A(LDA*(I-1)+1),1,X,INCX)
            KY = KY + INCY
   20    CONTINUE
      END IF
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SGER  (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    July 15, 1986
C
C  Purpose:    Perform the rank-one matrix update A = alpha*x*y' + A,
C              all single precision.
C
C  Usage:      CALL SGER (M, N, ALPHA, X, INCX, Y, INCY, A, LDA)
C
C  Arguments:
C     M      - Number of rows in A.  (Input)
C     N      - Number of columns in A.  (Input)
C     ALPHA  - Real scalar.  (Input)
C     X      - Real vector of length (M-1)*IABS(INCX)+1.  (Input)
C     INCX   - Displacement between elements of X.  (Input)
C     Y      - Real vector of length (N-1)*IABS(INCY)+1.  (Input)
C     INCY   - Displacement between elements of Y.  (Input)
C     A      - Array of size M by N.  (Input/Output)
C              On input, A contains the matrix to be updated.
C              On output, A contains the updated matrix.
C     LDA    - Leading dimension of A exactly as specified in the
C              calling routine.  (Input)
C
C  GAMS:       D1b
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SGER (M, N, ALPHA, X, INCX, Y, INCY, A, LDA)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    M, N, INCX, INCY, LDA
      REAL       ALPHA, X(*), Y(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IY, J
C                                  SPECIFICATIONS FOR SPECIAL CASES
      REAL       A(*)
      INTEGER    I1X
      EXTERNAL   SAXPY
C                                  Quick return if possible
      IF (M.EQ.0 .OR. N.EQ.0 .OR. ALPHA.EQ.0.0) GO TO 9000
C
      IY = 1
      IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
C
      I1X = 1
      DO 10  J=1, N
         CALL SAXPY (M, ALPHA*Y(IY), X, INCX, A(I1X), 1)
         IY = IY + INCY
         I1X = I1X + LDA
   10 CONTINUE
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SNRM2 (Single precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Compute the Euclidean length or L2 norm of a
C              single-precision vector.
C
C  Usage:      SNRM2(N, SX, INCX)
C
C  Arguments:
C     N      - Length of vector X.  (Input)
C     SX     - Real vector of length N*INCX.  (Input)
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be SX(1+(I-1)*INCX). INCX must be
C              greater than zero.
C     SNRM2  - Square root of the sum from I=1 to N of X(I)**2.
C              (Output)
C              X(I) refers to a specific element of SX. See INCX
C              argument description.
C
C  GAMS:       D1a3b
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      REAL FUNCTION SNRM2 (N, SX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      REAL       SX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, J, NEXT, NN
      REAL       HITEST, SUM, XMAX
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      REAL       CUTHI, CUTLO, ONE, ZERO
      SAVE       CUTHI, CUTLO, ONE, ZERO
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  ABS,SQRT
      INTRINSIC  ABS, SQRT
      REAL       ABS, SQRT
C
      DATA ZERO/0.0E0/, ONE/1.0E0/
      DATA CUTLO/4.441E-16/, CUTHI/1.304E19/
C
      IF (N .GT. 0) GO TO 10
      SNRM2 = ZERO
      GO TO 140
C
   10 ASSIGN 30 TO NEXT
      SUM = ZERO
      NN = N*INCX
C                                  BEGIN MAIN LOOP
      I = 1
   20 GO TO NEXT, (30, 40, 70, 80)
   30 IF (ABS(SX(I)) .GT. CUTLO) GO TO 110
      ASSIGN 40 TO NEXT
      XMAX = ZERO
C                                  PHASE 1. SUM IS ZERO
   40 IF (SX(I) .EQ. ZERO) GO TO 130
      IF (ABS(SX(I)) .GT. CUTLO) GO TO 110
C                                  PREPARE FOR PHASE 2.
      ASSIGN 70 TO NEXT
      GO TO 60
C                                  PREPARE FOR PHASE 4.
   50 I = J
      ASSIGN 80 TO NEXT
      SUM = (SUM/SX(I))/SX(I)
   60 XMAX = ABS(SX(I))
      GO TO 90
C                                  PHASE 2. SUM IS SMALL. SCALE TO
C                                  AVOID DESTRUCTIVE UNDERFLOW.
   70 IF (ABS(SX(I)) .GT. CUTLO) GO TO 100
C                                  COMMON CODE FOR PHASES 2 AND 4. IN
C                                  PHASE 4 SUM IS LARGE. SCALE TO
C                                  AVOID OVERFLOW.
   80 IF (ABS(SX(I)) .LE. XMAX) GO TO 90
      SUM = ONE + SUM*(XMAX/SX(I))**2
      XMAX = ABS(SX(I))
      GO TO 130
C
   90 SUM = SUM + (SX(I)/XMAX)**2
      GO TO 130
C                                  PREPARE FOR PHASE 3.
  100 SUM = (SUM*XMAX)*XMAX
C                                  FOR REAL OR D.P. SET HITEST =
C                                  CUTHI/N FOR COMPLEX SET HITEST =
C                                  CUTHI/(2*N)
  110 HITEST = CUTHI/N
C                                  PHASE 3. SUM IS MID-RANGE. NO
C                                  SCALING.
      DO 120  J=I, NN, INCX
         IF (ABS(SX(J)) .GE. HITEST) GO TO 50
  120 SUM = SUM + SX(J)*SX(J)
      SNRM2 = SQRT(SUM)
      GO TO 140
C
  130 CONTINUE
      I = I + INCX
      IF (I .LE. NN) GO TO 20
C                                  END OF MAIN LOOP. COMPUTE SQUARE
C                                  ROOT AND ADJUST FOR SCALING.
      SNRM2 = XMAX*SQRT(SUM)
  140 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SROT (Single precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Apply a Givens plane rotation in single precision.
C
C  Usage:      CALL SROT (N, SX, INCX, SY, INCY, C, S)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     SX     - Real vector of length MAX(N*IABS(INCX),1).
C              (Input/Output)
C              SROT replaces X(I) with SC*X(I) + SS*Y(I) for I=1,...,N.
C              X(I) and Y(I) refer to specific elements of SX and SY.
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be
C                 SX(1+(I-1)*INCX) if INCX.GE.0  or
C                 SX(1+(I-N)*INCX) if INCX.LT.0.
C     SY     - Real vector of length MAX(N*IABS(INCY),1).
C              (Input/Output)
C              SROT replaces Y(I) with -SS*X(I) + SC*Y(I) for I=1,...,N.
C              X(I) and Y(I) refer to specific elements of SX and SY.
C     INCY   - Displacement between elements of SY.  (Input)
C              Y(I) is defined to be
C                 SY(1+(I-1)*INCY) if INCY.GE.0  or
C                 SY(1+(I-N)*INCY) if INCY.LT.0.
C     C      - Real scalar containing elements of the rotation matrix.
C              (Input)
C     S      - Real scalar containing elements of the rotation matrix.
C              (Input)
C
C  Remark:
C                  ( SC SS )    (X(1) ... X(N))
C     SROT applies (       ) to (             )
C                  (-SS SC )    (Y(1) ... Y(N))
C
C  GAMS:       D1a8
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SROT (N, SX, INCX, SY, INCY, C, S)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      REAL       C, S, SX(*), SY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY
      REAL       STEMP
C
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS OR EQUAL
C                                    INCREMENTS NOT EQUAL TO 1
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               STEMP = C*SX(IX) + S*SY(IY)
               SY(IY) = C*SY(IY) - S*SX(IX)
               SX(IX) = STEMP
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
            DO 20  I=1, N
               STEMP = C*SX(I) + S*SY(I)
               SY(I) = C*SY(I) - S*SX(I)
               SX(I) = STEMP
   20       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SROTG (Single precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Construct a Givens plane rotation in single precision.
C
C  Usage:      CALL SROTG (SA, SB, SC, SS)
C
C  Arguments:
C     SA     - First element of vector.  (Input/Output)
C              On output, R = (+/-)SQRT(SA**2 + SB**2) overwrites SA.
C     SB     - Second element of vector.  (Input/Output)
C              On output, Z overwrites SB where Z is defined to be
C                SS        if ABS(SA) .GT. ABS(SB)
C                1.0/SC    if ABS(SB) .GE. ABS(SA) and SC .NE. 0.0
C                1.0       if SC .EQ. 0.0.
C     SC     - Real scalar containing elements of the rotation matrix.
C              (Output)
C     SS     - Real scalar containing elements of the rotation matrix.
C              (Output)
C
C  Remark:
C     SROTG constructs the Givens rotation
C                         ( SC  SS )
C                     G = (        ) , SC**2 + SS**2 = 1
C                         (-SS  SC )
C     which zeros the second element of (SA SB)(transpose).
C
C  GAMS:       D1a8
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SROTG (SA, SB, SC, SS)
C                                  SPECIFICATIONS FOR ARGUMENTS
      REAL       SA, SB, SC, SS
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      REAL       R, U, V
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  ABS,SQRT
      INTRINSIC  ABS, SQRT
      REAL       ABS, SQRT
C
      IF (ABS(SA) .GT. ABS(SB)) THEN
C                                  HERE ABS(SA) .GT. ABS(SB)
         U = SA + SA
         V = SB/U
C                                  NOTE THAT U AND R HAVE THE SIGN OF
C                                    SA
         R = SQRT(.25+V**2)*U
C                                  NOTE THAT SC IS POSITIVE
         SC = SA/R
         SS = V*(SC+SC)
         SB = SS
         SA = R
      ELSE
C                                  HERE ABS(SA) .LE. ABS(SB)
         IF (SB .NE. 0.0) THEN
            U = SB + SB
            V = SA/U
C                                  NOTE THAT U AND R HAVE THE SIGN OF
C                                    SB (R IS IMMEDIATELY STORED IN SA)
            SA = SQRT(.25+V**2)*U
C                                  NOTE THAT SS IS POSITIVE
            SS = SB/SA
            SC = V*(SS+SS)
            IF (SC .NE. 0.0) THEN
               SB = 1.0/SC
            ELSE
               SB = 1.0
            END IF
C                                  HERE SA = SB = 0.
         ELSE
            SC = 1.0
            SS = 0.0
            SA = 0.0
            SB = 0.0
         END IF
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SSCAL (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Multiply a vector by a scalar, y = ay, both single
C              precision.
C
C  Usage:      CALL SSCAL (N, SA, SX, INCX)
C
C  Arguments:
C     N      - Length of vector X.  (Input)
C     SA     - Real scalar.  (Input)
C     SX     - Real vector of length N*INCX.  (Input/Output)
C              SSCAL replaces X(I) with SA*X(I) for I=1,...,N. X(I)
C              refers to a specific element of SX. See INCX argument
C              description.
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be SX(1+(I-1)*INCX). INCX must be
C              greater than zero.
C
C  GAMS:       D1a6
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SSCAL (N, SA, SX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      REAL       SA, SX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, M, MP1, NS
C
      IF (N .GT. 0) THEN
         IF (INCX .NE. 1) THEN
C                                  CODE FOR INCREMENTS NOT EQUAL TO 1.
            NS = N*INCX
            DO 10  I=1, NS, INCX
               SX(I) = SA*SX(I)
   10       CONTINUE
         ELSE
C                                  CODE FOR INCREMENTS EQUAL TO 1.
C                                  CLEAN-UP LOOP SO REMAINING VECTOR
C                                  LENGTH IS A MULTIPLE OF 5.
            M = N - (N/5)*5
            DO 30  I=1, M
               SX(I) = SA*SX(I)
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 5
               SX(I) = SA*SX(I)
               SX(I+1) = SA*SX(I+1)
               SX(I+2) = SA*SX(I+2)
               SX(I+3) = SA*SX(I+3)
               SX(I+4) = SA*SX(I+4)
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SSET (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Set the components of a vector to a scalar, all
C              single precision.
C
C  Usage:      CALL SSET (N, SA, SX, INCX)
C
C  Arguments:
C     N      - Length of vector X.  (Input)
C     SA     - Real scalar.  (Input)
C     SX     - Real vector of length N*INCX.  (Input/Output)
C              SSET replaces X(I) with SA for I=1,...,N. X(I) refers to
C              a specific element of SX. See INCX argument description.
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be SX(1+(I-1)*INCX). INCX must be
C              greater than zero.
C
C  GAMS:       D1a1
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SSET (N, SA, SX, INCX)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX
      REAL       SA, SX(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, M, MP1, NINCX
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (INCX .NE. 1) THEN
C                                  CODE FOR INCREMENT NOT EQUAL TO 1
            NINCX = N*INCX
            DO 10  I=1, NINCX, INCX
               SX(I) = SA
   10       CONTINUE
         ELSE
C                                  CODE FOR INCREMENT EQUAL TO 1
            M = MOD(N,8)
C                                  CLEAN-UP LOOP
            DO 30  I=1, M
               SX(I) = SA
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 8
               SX(I) = SA
               SX(I+1) = SA
               SX(I+2) = SA
               SX(I+3) = SA
               SX(I+4) = SA
               SX(I+5) = SA
               SX(I+6) = SA
               SX(I+7) = SA
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SSWAP (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Interchange vectors X and Y, both single precision.
C
C  Usage:      CALL SSWAP (N, SX, INCX, SY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     SX     - Real vector of length MAX(N*IABS(INCX),1).
C              (Input/Output)
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be
C                 SX(1+(I-1)*INCX) if INCX.GE.0  or
C                 SX(1+(I-N)*INCX) if INCX.LT.0.
C     SY     - Real vector of length MAX(N*IABS(INCY),1).
C              (Input/Output)
C     INCY   - Displacement between elements of SY.  (Input)
C              Y(I) is defined to be
C                 SY(1+(I-1)*INCY) if INCY.GE.0  or
C                 SY(1+(I-N)*INCY) if INCY.LT.0.
C
C  GAMS:       D1a5
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SSWAP (N, SX, INCX, SY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      REAL       SX(*), SY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
      REAL       STEMP
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS OR EQUAL
C                                    INCREMENTS NOT EQUAL TO 1
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               STEMP = SX(IX)
               SX(IX) = SY(IY)
               SY(IY) = STEMP
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
            M = MOD(N,3)
C                                  CLEAN-UP LOOP
            DO 30  I=1, M
               STEMP = SX(I)
               SX(I) = SY(I)
               SY(I) = STEMP
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 3
               STEMP = SX(I)
               SX(I) = SY(I)
               SY(I) = STEMP
               STEMP = SX(I+1)
               SX(I+1) = SY(I+1)
               SY(I+1) = STEMP
               STEMP = SX(I+2)
               SX(I+2) = SY(I+2)
               SY(I+2) = STEMP
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  C1TCI
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 13, 1984
C
C  Purpose:    Convert character string into corresponding integer
C              form.
C
C  Usage:      CALL C1TCI (CHRSTR, SLEN, NUM, IER)
C
C  Arguments:
C   CHRSTR  - Character array that contains the number description.
C             (Input)
C   SLEN    - Length of the character array.  (Input)
C   NUM     - The answer.  (Output)
C   IER     - Completion code.  (Output)  Where
C                IER =-2  indicates that the number is too large to
C                         be converted;
C                IER =-1  indicates that SLEN <= 0;
C                IER = 0  indicates normal completion;
C                IER > 0  indicates that the input string contains a
C                         nonnumeric character.  IER is the index of
C                         the first nonnumeric character in CHRSTR.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE C1TCI (CHRSTR, SLEN, NUM, IER)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    SLEN, NUM, IER
      CHARACTER  CHRSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    COUNT, I, IMACH5, J, N, S, SIGN
      CHARACTER  ZERO
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK, DIGIT*10, MINUS, PLUS
      SAVE       BLANK, DIGIT, MINUS, PLUS
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (DIGIT, ZERO)
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  INDEX
      INTRINSIC  INDEX
      INTEGER    INDEX
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   IMACH
      INTEGER    IMACH
C
      DATA DIGIT/'0123456789'/
      DATA BLANK/' '/, MINUS/'-'/, PLUS/'+'/
C
C                                  CHECK SLEN
      NUM = 0
      IF (SLEN .LE. 0) THEN
         IER = -1
         GO TO 50
      END IF
C                                  HANDLE LEADING BLANKS
      SIGN = 1
      I = 1
   10 IF (I .LE. SLEN) THEN
         IF (CHRSTR(I) .EQ. BLANK) THEN
            I = I + 1
            GO TO 10
         END IF
      ELSE
         IER = 1
         GO TO 50
      END IF
C                                  CHECK FOR SIGN, IF ANY
      S = I
      IF (CHRSTR(I) .EQ. MINUS) THEN
         SIGN = -1
         I = I + 1
      ELSE IF (CHRSTR(I) .EQ. PLUS) THEN
         I = I + 1
      END IF
   20 IF (I .LE. SLEN) THEN
         IF (CHRSTR(I) .EQ. BLANK) THEN
            I = I + 1
            GO TO 20
         END IF
      ELSE
         IER = S
         GO TO 50
      END IF
C                                  SKIP LEADING ZERO
      J = I
   30 IF (I .LE. SLEN) THEN
         IF (CHRSTR(I) .EQ. ZERO) THEN
            I = I + 1
            GO TO 30
         END IF
      ELSE
         IER = 0
         GO TO 50
      END IF
C                                  CHECK FIRST NONBLANK CHARACTER
      COUNT = 0
C                                  CHECK NUMERIC CHARACTERS
      IMACH5 = IMACH(5)
   40 N = INDEX(DIGIT,CHRSTR(I))
      IF (N .NE. 0) THEN
         COUNT = COUNT + 1
         IF (NUM .GT. ((IMACH5-N)+1)/10) THEN
            IER = -2
            GO TO 50
         ELSE
            NUM = NUM*10 - 1 + N
            I = I + 1
            IF (I .LE. SLEN) GO TO 40
         END IF
      END IF
C
      IF (COUNT .EQ. 0) THEN
         IF (I .GT. J) THEN
            IER = I
         ELSE
            IER = S
         END IF
      ELSE IF (I .GT. SLEN) THEN
         NUM = SIGN*NUM
         IER = 0
      ELSE
         NUM = SIGN*NUM
         IER = I
      END IF
C
   50 CONTINUE
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1INIT
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 13, 1984
C
C  Purpose:    Initialization.
C
C  Usage:      CALL E1INIT
C
C  Arguments:  None
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1INIT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    L
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    ISINIT
      SAVE       ISINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                              SPECIFICATIONS FOR COMMON /ERCOM8/
      INTEGER    PROLVL, XXLINE(10), XXPLEN(10), ICALOC(10), INALOC(10)
      COMMON     /ERCOM8/ PROLVL, XXLINE, XXPLEN, ICALOC, INALOC
      SAVE       /ERCOM8/
C                              SPECIFICATIONS FOR COMMON /ERCOM9/
      CHARACTER  XXPROC(10)*31
      COMMON     /ERCOM9/ XXPROC
      SAVE       /ERCOM9/
C
      DATA ISINIT/0/
C
      IF (ISINIT .EQ. 0) THEN
C                                  INITIALIZE
         CALLVL = 1
         ERCODE(1) = 0
         ERTYPE(1) = 0
         IALLOC(1) = 0
         ISUSER(1) = .TRUE.
         IFERR6 = 0
         IFERR7 = 0
         PLEN = 1
         MAXLEV = 50
         DO 10  L=2, 51
            ERTYPE(L) = -1
            ERCODE(L) = -1
            IALLOC(L) = 0
            ISUSER(L) = .FALSE.
   10    CONTINUE
         DO 20  L=1, 7
            HDRFMT(L) = 1
            TRACON(L) = 1
   20    CONTINUE
         PROLVL = 1
         DO 30  L=1, 10
   30    ICALOC(L) = 0
         XXLINE(1) = 0
         XXPLEN(1) = 1
         XXPROC(1) = '?'
         RNAME(1) = 'USER'
         PRINTB(1) = 0
         PRINTB(2) = 0
         DO 40  L=3, 7
   40    PRINTB(L) = 1
         STOPTB(1) = 0
         STOPTB(2) = 0
         STOPTB(3) = 0
         STOPTB(4) = 1
         STOPTB(5) = 1
         STOPTB(6) = 0
         STOPTB(7) = 1
         ERCKSM = 0.0D0
C                                  SET FLAG TO INDICATE THAT
C                                    INITIALIZATION HAS OCCURRED
         ISINIT = 1
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1PRT
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 14, 1984
C
C  Purpose:    To print an error message.
C
C  Usage:      CALL E1PRT
C
C  Arguments:  None
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1PRT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ALL, I, IBEG, IBLOC, IBLOC2, IEND, IER, IHDR, J,
     &           LERTYP, LOC, LOCM1, LOCX, MAXLOC, MAXTMP, MLOC, MOD,
     &           NCBEG, NLOC, NOUT
      CHARACTER  MSGTMP(70), STRING(10)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  ATLINE(9), BLANK(1), DBB(3), FROM(6), MSGTYP(8,7),
     &           PERSLA(2), QMARK, UNKNOW(8)
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                              SPECIFICATIONS FOR COMMON /ERCOM8/
      INTEGER    PROLVL, XXLINE(10), XXPLEN(10), ICALOC(10), INALOC(10)
      COMMON     /ERCOM8/ PROLVL, XXLINE, XXPLEN, ICALOC, INALOC
      SAVE       /ERCOM8/
C                              SPECIFICATIONS FOR COMMON /ERCOM9/
      CHARACTER  XXPROC(10)*31
      COMMON     /ERCOM9/ XXPROC
      SAVE       /ERCOM9/
      SAVE       ATLINE, BLANK, DBB, FROM, MSGTYP, PERSLA, QMARK,
     &           UNKNOW
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MIN0
      INTRINSIC  MIN0
      INTEGER    MIN0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TIC, M1VE, UMACH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1DX, I1ERIF
      INTEGER    I1DX, I1ERIF
C
      DATA MSGTYP/'N', 'O', 'T', 'E', ' ', ' ', ' ', ' ', 'A',
     &     'L', 'E', 'R', 'T', ' ', ' ', ' ', 'W', 'A', 'R',
     &     'N', 'I', 'N', 'G', ' ', 'F', 'A', 'T', 'A', 'L',
     &     ' ', ' ', ' ', 'T', 'E', 'R', 'M', 'I', 'N', 'A',
     &     'L', 'W', 'A', 'R', 'N', 'I', 'N', 'G', ' ', 'F',
     &     'A', 'T', 'A', 'L', ' ', ' ', ' '/
      DATA UNKNOW/'U', 'N', 'K', 'N', 'O', 'W', 'N', ' '/
      DATA ATLINE/' ', 'a', 't', ' ', 'l', 'i', 'n', 'e', ' '/
      DATA BLANK/' '/, FROM/' ', 'f', 'r', 'o', 'm', ' '/
      DATA DBB/'.', ' ', ' '/, PERSLA/'%', '/'/
      DATA QMARK/'?'/
C
      IF (MSGLEN .LE. 0) RETURN
      CALL UMACH (2, NOUT)
      MAXTMP = 70
      MOD = 0
      LERTYP = ERTYPE(CALLVL)
      IHDR = HDRFMT(LERTYP)
      IF (IHDR .EQ. 3) THEN
         IF (XXPROC(PROLVL)(1:1).EQ.QMARK .AND. XXLINE(PROLVL).EQ.0)
     &       THEN
            IHDR = 1
         END IF
      END IF
      IEND = 0
      IF (IHDR.EQ.1 .AND. ERTYPE(CALLVL).LE.4) THEN
         MSGTMP(1) = BLANK(1)
         IEND = 1
C                                  CONVERT ERROR CODE INTO CHAR STRING
         CALL C1TIC (ERCODE(CALLVL), STRING, 10, IER)
C                                  LOCATE START OF NON-BLANK CHARACTERS
         IBEG = I1ERIF(STRING,10,BLANK,1)
C                                  M1VE IT TO MSGTMP
         CALL M1VE (STRING, IBEG, 10, 10, MSGTMP, IEND+1,
     &              IEND+11-IBEG, MAXTMP, IER)
         IEND = IEND + 11 - IBEG
      END IF
      IF (IHDR .NE. 2) THEN
         CALL M1VE (FROM, 1, 6, 6, MSGTMP, IEND+1, IEND+6, MAXTMP, IER)
         IEND = IEND + 6
      END IF
      IF (IHDR .EQ. 3) THEN
C                                  THIS IS A PROTRAN RUN TIME ERROR MSG.
C                                  RETRIEVE THE PROCEDURE NAME
         CALL M1VE (XXPROC(PROLVL), 1, XXPLEN(PROLVL), 31, MSGTMP,
     &              IEND+1, IEND+XXPLEN(PROLVL), MAXTMP, IER)
         MLOC = IEND + XXPLEN(PROLVL) + 1
         MSGTMP(MLOC) = BLANK(1)
         IEND = IEND + I1DX(MSGTMP(IEND+1),XXPLEN(PROLVL)+1,BLANK,1) -
     &          1
         IF (XXLINE(PROLVL) .GT. 0) THEN
C                                  INSERT ATLINE
            CALL M1VE (ATLINE, 1, 9, 9, MSGTMP, IEND+1, IEND+9,
     &                 MAXTMP, IER)
            IEND = IEND + 9
C                                  CONVERT PROTRAN GLOBAL LINE NUMBER
            CALL C1TIC (XXLINE(PROLVL), STRING, 10, IER)
C                                  LOCATE START OF NON-BLANK CHARACTERS
            IBEG = I1ERIF(STRING,10,BLANK,1)
C                                  M1VE GLOBAL LINE NUMBER TO MSGTMP
            CALL M1VE (STRING, IBEG, 10, 10, MSGTMP, IEND+1,
     &                 IEND+11-IBEG, MAXTMP, IER)
            IEND = IEND + 11 - IBEG
         END IF
      ELSE
C                                  THIS IS EITHER A LIBRARY ERROR MSG
C                                  OR A PROTRAN PREPROCESSOR ERROR MSG
         IF (IHDR .EQ. 1) THEN
C                                  THIS IS A LIBRARY ERROR MESSAGE.
C                                  RETRIEVE ROUTINE NAME
            CALL M1VE (RNAME(CALLVL), 1, 6, 6, MSGTMP, IEND+1, IEND+6,
     &                 MAXTMP, IER)
            MSGTMP(IEND+7) = BLANK(1)
            IEND = IEND + I1DX(MSGTMP(IEND+1),7,BLANK,1) - 1
         END IF
C                                  ADD DOT, BLANK, BLANK IF NEEDED
         IF (I1DX(MSGSAV,3,DBB,3) .NE. 1) THEN
            CALL M1VE (DBB, 1, 3, 3, MSGTMP, IEND+1, IEND+3, MAXTMP,
     &                 IER)
            IEND = IEND + 3
            MOD = 3
         END IF
      END IF
C                                  MSGTMP AND MSGSAV NOW CONTAIN THE
C                                   ERROR MESSAGE IN FINAL FORM.
      NCBEG = 59 - IEND - MOD
      ALL = 0
      IBLOC = I1DX(MSGSAV,MSGLEN,PERSLA,2)
      IF (IBLOC.NE.0 .AND. IBLOC.LT.NCBEG) THEN
         LOCM1 = IBLOC - 1
         LOC = IBLOC + 1
      ELSE IF (MSGLEN .LE. NCBEG) THEN
         LOCM1 = MSGLEN
         ALL = 1
      ELSE
         LOC = NCBEG
C                                  CHECK FOR APPROPRIATE PLACE TO SPLIT
   10    CONTINUE
         IF (MSGSAV(LOC) .NE. BLANK(1)) THEN
            LOC = LOC - 1
            IF (LOC .GT. 1) GO TO 10
            LOC = NCBEG + 1
         END IF
         LOCM1 = LOC - 1
      END IF
C                                  NO BLANKS FOUND IN FIRST NCBEG CHARS
      IF (LERTYP.GE.1 .AND. LERTYP.LE.7) THEN
         WRITE (NOUT,99995) (MSGTYP(I,LERTYP),I=1,8),
     &                     (MSGTMP(I),I=1,IEND), (MSGSAV(I),I=1,LOCM1)
      ELSE
         WRITE (NOUT,99995) (UNKNOW(I),I=1,8), (MSGTMP(I),I=1,IEND),
     &                     (MSGSAV(I),I=1,LOCM1)
      END IF
      IF (ALL .EQ. 0) THEN
C                                  PREPARE TO WRITE CONTINUATION OF
C                                    MESSAGE
C
C                                  FIND WHERE TO BREAK MESSAGE
C                                    LOC = NUMBER OF CHARACTERS OF
C                                          MESSAGE WRITTEN SO FAR
   20    LOCX = LOC + 64
         NLOC = LOC + 1
         IBLOC2 = IBLOC
         MAXLOC = MIN0(MSGLEN-LOC,64)
         IBLOC = I1DX(MSGSAV(NLOC),MAXLOC,PERSLA,2)
         IF (MSGSAV(NLOC).EQ.BLANK(1) .AND. IBLOC2.EQ.0) NLOC = NLOC +
     &       1
         IF (IBLOC .GT. 0) THEN
C                                  PAGE BREAK FOUND AT IBLOC
            LOCX = NLOC + IBLOC - 2
            WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
            LOC = NLOC + IBLOC
            GO TO 20
C                                  DON'T BOTHER LOOKING FOR BLANK TO
C                                    BREAK AT IF LOCX .GE. MSGLEN
         ELSE IF (LOCX .LT. MSGLEN) THEN
C                                  CHECK FOR BLANK TO BREAK THE LINE
   30       CONTINUE
            IF (MSGSAV(LOCX) .EQ. BLANK(1)) THEN
C                                  BLANK FOUND AT LOCX
               WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
               LOC = LOCX
               GO TO 20
            END IF
            LOCX = LOCX - 1
            IF (LOCX .GT. NLOC) GO TO 30
            LOCX = LOC + 64
C                                  NO BLANKS FOUND IN NEXT 64 CHARS
            WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
            LOC = LOCX
            GO TO 20
         ELSE
C                                  ALL THE REST WILL FIT ON 1 LINE
            LOCX = MSGLEN
            WRITE (NOUT,99996) (MSGSAV(I),I=NLOC,LOCX)
         END IF
      END IF
C                                  SET LENGTH OF MSGSAV AND PLEN
C                                    TO SHOW THAT MESSAGE HAS
C                                    ALREADY BEEN PRINTED
 9000 MSGLEN = 0
      PLEN = 1
      IF (TRACON(LERTYP).EQ.1 .AND. CALLVL.GT.2) THEN
C                                  INITIATE TRACEBACK
         WRITE (NOUT,99997)
         DO 9005  J=CALLVL, 1, -1
            IF (J .GT. 1) THEN
               IF (ISUSER(J-1)) THEN
                  WRITE (NOUT,99998) RNAME(J), ERTYPE(J), ERCODE(J)
               ELSE
                  WRITE (NOUT,99999) RNAME(J), ERTYPE(J), ERCODE(J)
               END IF
            ELSE
               WRITE (NOUT,99998) RNAME(J), ERTYPE(J), ERCODE(J)
            END IF
 9005    CONTINUE
      END IF
C
      RETURN
99995 FORMAT (/, ' *** ', 8A1, ' ERROR', 59A1)
99996 FORMAT (' *** ', 9X, 64A1)
99997 FORMAT (14X, 'Here is a traceback of subprogram calls',
     &       ' in reverse order:', /, 14X, '      Routine    Error ',
     &       'type    Error code', /, 14X, '      -------    ',
     &       '----------    ----------')
99998 FORMAT (20X, A6, 5X, I6, 8X, I6)
99999 FORMAT (20X, A6, 5X, I6, 8X, I6, 4X, '(Called internally)')
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1UCS
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 8, 1984
C
C  Purpose:    To update the checksum number for error messages.
C
C  Usage:      CALL E1UCS
C
C  Arguments:  None
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1UCS
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IBEG, IBEG2, IEND, ILOC, IPOS, JLOC, NCODE, NLEN
      DOUBLE PRECISION DNUM
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION DMAX
      CHARACTER  BLANK(1), COMMA(1), EQUAL(1), LPAR(1)
      SAVE       BLANK, COMMA, DMAX, EQUAL, LPAR
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DMOD
      INTRINSIC  DMOD
      DOUBLE PRECISION DMOD
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   S1ANUM
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   ICASE, I1X
      INTEGER    ICASE, I1X
C
      DATA BLANK(1)/' '/, COMMA(1)/','/, LPAR(1)/'('/
      DATA EQUAL(1)/'='/, DMAX/1.0D+9/
C
      IF (MSGLEN .GT. 1) THEN
         IPOS = 0
         IBEG2 = 1
   10    IBEG = IBEG2
         IEND = MSGLEN
C                                  LOOK FOR BLANK, COMMA, LEFT PAREN.,
C                                  OR EQUAL SIGN
         ILOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,BLANK,1)
         JLOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,COMMA,1)
         IF (ILOC.EQ.0 .OR. (JLOC.GT.0.AND.JLOC.LT.ILOC)) ILOC = JLOC
         JLOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,LPAR,1)
         IF (ILOC.EQ.0 .OR. (JLOC.GT.0.AND.JLOC.LT.ILOC)) ILOC = JLOC
         JLOC = I1X(MSGSAV(IBEG),IEND-IBEG+1,EQUAL,1)
         IF (ILOC.EQ.0 .OR. (JLOC.GT.0.AND.JLOC.LT.ILOC)) ILOC = JLOC
         IF (ILOC .GE. 1) THEN
            CALL S1ANUM (MSGSAV(IBEG+ILOC), IEND-IBEG-ILOC+1, NCODE,
     &                   NLEN)
            IF (NCODE.EQ.2 .OR. NCODE.EQ.3) THEN
C                                  FLOATING POINT NUMBER FOUND.
C                                  SET POINTERS TO SKIP OVER IT
               IBEG2 = IBEG + ILOC + NLEN
               IF (IBEG2 .LE. MSGLEN) THEN
                  CALL S1ANUM (MSGSAV(IBEG2), IEND-IBEG2+1, NCODE,
     &                         NLEN)
                  IF ((MSGSAV(IBEG2).EQ.'+'.OR.MSGSAV(IBEG2).EQ.
     &                '-') .AND. NCODE.EQ.1) THEN
C                                  INTEGER IMMEDIATELY FOLLOWS A REAL AS
C                                  WITH SOME CDC NOS. LIKE 1.2345678+123
C                                  SET POINTERS TO SKIP OVER IT
                     IBEG2 = IBEG2 + NLEN
                  END IF
               END IF
            ELSE
               IBEG2 = IBEG + ILOC
            END IF
            IEND = IBEG + ILOC - 1
         END IF
C                                  UPDATE CKSUM USING PART OF MESSAGE
         DO 20  I=IBEG, IEND
            IPOS = IPOS + 1
            DNUM = ICASE(MSGSAV(I))
            ERCKSM = DMOD(ERCKSM+DNUM*IPOS,DMAX)
   20    CONTINUE
C                                  GO BACK FOR MORE IF NEEDED
         IF (IEND.LT.MSGLEN .AND. IBEG2.LT.MSGLEN) GO TO 10
C                                  UPDATE CKSUM USING ERROR TYPE
         DNUM = ERTYPE(CALLVL)
         ERCKSM = DMOD(ERCKSM+DNUM*(IPOS+1),DMAX)
C                                  UPDATE CKSUM USING ERROR CODE
         DNUM = ERCODE(CALLVL)
         ERCKSM = DMOD(ERCKSM+DNUM*(IPOS+2),DMAX)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  M1VE
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 5, 1984
C
C  Purpose:    Move a subset of one character array to another.
C
C  Usage:      CALL M1VE(INSTR, INBEG, INEND, INLEN, OUTSTR, OUTBEG,
C                         OUTEND, OUTLEN, IER)
C
C  Arguments:
C     INSTR  - Source character array.  (Input)
C     INBEG  - First element of INSTR to be moved.  (Input)
C     INEND  - Last element of INSTR to be moved.  (Input)
C              The source subset is INSTR(INBEG),...,INSTR(INEND).
C     INLEN  - Length of INSTR.  (Input)
C     OUTSTR - Destination character array.  (Output)
C     IUTBEG - First element of OUTSTR destination.  (Input)
C     IUTEND - Last element of OUTSTR  destination.  (Input)
C              The destination subset is OUTSRT(IUTBEG),...,
C              OUTSTR(IUTEND).
C     IUTLEN - Length of OUTSTR.  (Input)
C     IER    - Completion code.  (Output)
C              IER = -2  indicates that the input parameters, INBEG,
C                        INEND, INLEN, IUTBEG, IUTEND are not
C                        consistent.  One of the conditions
C                        INBEG.GT.0, INEND.GE.INBEG, INLEN.GE.INEND,
C                        IUTBEG.GT.0, or IUTEND.GE.IUTBEG is not
C                        satisfied.
C              IER = -1  indicates that the length of OUTSTR is
C                        insufficient to hold the subset of INSTR.
C                        That is, IUTLEN is less than IUTEND.
C              IER =  0  indicates normal completion
C              IER >  0  indicates that the specified subset of OUTSTR,
C                        OUTSTR(IUTBEG),...,OUTSTR(IUTEND) is not long
C                        enough to hold the subset INSTR(INBEG),...,
C                        INSTR(INEND) of INSTR.  IER is set to the
C                        number of characters that were not moved.
C
C  Remarks:
C  1. If the subset of OUTSTR is longer than the subset of INSTR,
C     trailing blanks are moved to OUTSTR.
C  2. If the subset of INSTR is longer than the subset of OUTSTR,
C     the shorter subset is moved to OUTSTR and IER is set to the number
C     of characters that were not moved to OUTSTR.
C  3. If the length of OUTSTR is insufficient to hold the subset,
C     IER is set to -2 and nothing is moved.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE M1VE (INSTR, INBEG, INEND, INLEN, OUTSTR, IUTBEG,
     &                 IUTEND, IUTLEN, IER)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    INBEG, INEND, INLEN, IUTBEG, IUTEND, IUTLEN, IER
      CHARACTER  INSTR(*), OUTSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IUTLAS, KI, KO
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK
      SAVE       BLANK
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MIN0
      INTRINSIC  MIN0
      INTEGER    MIN0
C
      DATA BLANK/' '/
C                                  CHECK INBEG, INEND, INLEN, IUTBEG,
C                                  AND IUTEND
C
      IF (INBEG.LE.0 .OR. INEND.LT.INBEG .OR. INLEN.LT.INEND .OR.
     &    IUTBEG.LE.0 .OR. IUTEND.LT.IUTBEG) THEN
         IER = -2
         RETURN
      ELSE IF (IUTLEN .LT. IUTEND) THEN
         IER = -1
         RETURN
      END IF
C                                  DETERMINE LAST CHARACTER TO M1VE
      IUTLAS = IUTBEG + MIN0(INEND-INBEG,IUTEND-IUTBEG)
C                                  M1VE CHARACTERS
      KI = INBEG
      DO 10  KO=IUTBEG, IUTLAS
         OUTSTR(KO) = INSTR(KI)
         KI = KI + 1
   10 CONTINUE
C                                   SET IER TO NUMBER OF CHARACTERS THAT
C                                   WHERE NOT MOVED
      IER = KI - INEND - 1
C                                   APPEND BLANKS IF NECESSARY
      DO 20  KO=IUTLAS + 1, IUTEND
         OUTSTR(KO) = BLANK
   20 CONTINUE
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  M1VECH
C
C  Computer:   DGC/SINGLE
C
C  Revised:    December 31, 1984
C
C  Purpose:    Character substring assignment.
C
C  Usage:      CALL M1VECH (STR1, LEN1, STR2, LEN2)
C
C  Arguments:
C     STR1   - Source substring.  (Input)
C              The source substring is STR1(1:LEN1).
C     LEN1   - Length of STR1.  (Input)
C     STR2   - Destination substring.  (Output)
C              The destination substring is STR2(1:LEN2).
C     LEN2   - Length of STR2.  (Input)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE M1VECH (STR1, LEN1, STR2, LEN2)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LEN1, LEN2
      CHARACTER  STR1(*), STR2(*)
C
      STR2(1:LEN2) = STR1(1:LEN1)
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1DX (Single precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    September 9, 1985
C
C  Purpose:    Determine the array subscript indicating the starting
C              element at which a key character sequence begins.
C              (Case-insensitive version)
C
C  Usage:      I1DX(CHRSTR, I1LEN, KEY, KLEN)
C
C  Arguments:
C     CHRSTR - Character array to be searched.  (Input)
C     I1LEN  - Length of CHRSTR.  (Input)
C     KEY    - Character array that contains the key sequence.  (Input)
C     KLEN   - Length of KEY.  (Input)
C     I1DX   - Integer function.  (Output)
C
C  Remarks:
C  1. Returns zero when there is no match.
C
C  2. Returns zero if KLEN is longer than ISLEN.
C
C  3. Returns zero when any of the character arrays has a negative or
C     zero length.
C
C  GAMS:       N5c
C
C  Chapter:    MATH/LIBRARY Utilities
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1DX (CHRSTR, I1LEN, KEY, KLEN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    I1LEN, KLEN
      CHARACTER  CHRSTR(*), KEY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, II, J
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   ICASE, I1CSTR
      INTEGER    ICASE, I1CSTR
C
      I1DX = 0
      IF (KLEN.LE.0 .OR. I1LEN.LE.0) GO TO 9000
      IF (KLEN .GT. I1LEN) GO TO 9000
C
      I = 1
      II = I1LEN - KLEN + 1
   10 IF (I .LE. II) THEN
         IF (ICASE(CHRSTR(I)) .EQ. ICASE(KEY(1))) THEN
            IF (KLEN .NE. 1) THEN
               J = KLEN - 1
               IF (I1CSTR(CHRSTR(I+1),J,KEY(2),J) .EQ. 0) THEN
                  I1DX = I
                  GO TO 9000
               END IF
            ELSE
               I1DX = I
               GO TO 9000
            END IF
         END IF
         I = I + 1
         GO TO 10
      END IF
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KRL
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1983
C
C  Purpose:    Deallocate the last N allocations made in the workspace.
C              stack by I1KGT
C
C  Usage:      CALL I1KRL(N)
C
C  Arguments:
C     N      - Number of allocations to be released top down (Input)
C
C  Copyright:  1983 by IMSL, Inc.  All Rights Reserved
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE I1KRL (N)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IN, LALC, LBND, LBOOK, LMAX, LNEED, LNOW, LOUT,
     &           LUSED, NDX, NEXT
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(61913)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ RWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1STI, IWKIN
C
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C                                  CALLING I1KRL(0) WILL CONFIRM
C                                  INTEGRITY OF SYSTEM AND RETURN
      IF (N .LT. 0) THEN
         CALL E1MES (5, 10, 'Error from subroutine I1KRL:  Attempt'//
     &               ' to release a negative number of workspace'//
     &               ' allocations. ')
         GO TO 9000
      END IF
C                                  BOOKKEEPING OVERWRITTEN
      IF (LNOW.LT.LBOOK .OR. LNOW.GT.LUSED .OR. LUSED.GT.LMAX .OR.
     &    LNOW.GE.LBND .OR. LOUT.GT.LALC) THEN
         CALL E1MES (5, 11, 'Error from subroutine I1KRL:  One or '//
     &               'more of the first eight bookkeeping locations '//
     &               'in IWKSP have been overwritten.  ')
         GO TO 9000
      END IF
C                                  CHECK ALL THE POINTERS IN THE
C                                  PERMANENT STORAGE AREA.  THEY MUST
C                                  BE MONOTONE INCREASING AND LESS THAN
C                                  OR EQUAL TO LMAX, AND THE INDEX OF
C                                  THE LAST POINTER MUST BE LMAX+1.
      NDX = LBND
      IF (NDX .NE. LMAX+1) THEN
         DO 10  I=1, LALC
            NEXT = IWKSP(NDX)
            IF (NEXT .EQ. LMAX+1) GO TO 20
C
            IF (NEXT.LE.NDX .OR. NEXT.GT.LMAX) THEN
               CALL E1MES (5, 12, 'Error from subroutine I1KRL:  '//
     &                     'A pointer in permanent storage has been '//
     &                     ' overwritten. ')
               GO TO 9000
            END IF
            NDX = NEXT
   10    CONTINUE
         CALL E1MES (5, 13, 'Error from subroutine I1KRL:  A '//
     &               'pointer in permanent storage has been '//
     &               'overwritten. ')
         GO TO 9000
      END IF
   20 IF (N .GT. 0) THEN
         DO 30  IN=1, N
            IF (LNOW .LE. LBOOK) THEN
               CALL E1MES (5, 14, 'Error from subroutine I1KRL:  '//
     &                     'Attempt to release a nonexistant '//
     &                     'workspace  allocation. ')
               GO TO 9000
            ELSE IF (IWKSP(LNOW).LT.LBOOK .OR. IWKSP(LNOW).GE.LNOW-1)
     &              THEN
C                                  CHECK TO MAKE SURE THE BACK POINTERS
C                                  ARE MONOTONE.
               CALL E1STI (1, LNOW)
               CALL E1MES (5, 15, 'Error from subroutine I1KRL:  '//
     &                     'The pointer at IWKSP(%(I1)) has been '//
     &                     'overwritten.  ')
               GO TO 9000
            ELSE
               LOUT = LOUT - 1
               LNOW = IWKSP(LNOW)
            END IF
   30    CONTINUE
      END IF
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KST
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1983
C
C  Purpose:    Return control information about the workspace stack.
C
C  Usage:      I1KST(NFACT)
C
C  Arguments:
C     NFACT  - Integer value between 1 and 6 inclusive returns the
C                 following information: (Input)
C                   NFACT = 1 - LOUT: number of current allocations
C                               excluding permanent storage. At the
C                               end of a run, there should be no
C                               active allocations.
C                   NFACT = 2 - LNOW: current active length
C                   NFACT = 3 - LTOTAL: total storage used thus far
C                   NFACT = 4 - LMAX: maximum storage allowed
C                   NFACT = 5 - LALC: total number of allocations made
C                               by I1KGT thus far
C                   NFACT = 6 - LNEED: number of numeric storage units
C                               by which the stack size must be
C                               increased for all past allocations
C                               to succeed
C     I1KST  - Integer function. (Output) Returns a workspace stack
C              statistic according to value of NFACT.
C
C  Copyright:  1983 by IMSL, Inc.  All Rights Reserved
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1KST (NFACT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NFACT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ISTATS(7)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(61913)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ RWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (ISTATS(1), IWKSP(1))
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, IWKIN
C
      DATA FIRST/.TRUE./
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C
      IF (NFACT.LE.0 .OR. NFACT.GE.7) THEN
         CALL E1MES (5, 9, 'Error from subroutine I1KST:  Argument'//
     &               ' for I1KST must be between 1 and 6 inclusive.')
      ELSE IF (NFACT .EQ. 1) THEN
C                                  LOUT
         I1KST = ISTATS(1)
      ELSE IF (NFACT .EQ. 2) THEN
C                                  LNOW + PERMANENT
         I1KST = ISTATS(2) + (ISTATS(5)-ISTATS(4)+1)
      ELSE IF (NFACT .EQ. 3) THEN
C                                  LUSED + PERMANENT
         I1KST = ISTATS(3) + (ISTATS(5)-ISTATS(4)+1)
      ELSE IF (NFACT .EQ. 4) THEN
C                                  LMAX
         I1KST = ISTATS(5)
      ELSE IF (NFACT .EQ. 5) THEN
C                                  LALC
         I1KST = ISTATS(6)
      ELSE IF (NFACT .EQ. 6) THEN
C                                  LNEED
         I1KST = ISTATS(7)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1KQU
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    January 17, 1984
C
C  Purpose:    Return number of elements of data type ITYPE that
C              remain to be allocated in one request.
C
C  Usage:      I1KQU(ITYPE)
C
C  Arguments:
C     ITYPE  - Type of storage to be checked (Input)
C                 1 - logical
C                 2 - integer
C                 3 - real
C                 4 - double precision
C                 5 - complex
C                 6 - double complex
C     I1KQU  - Integer function. (Output) Returns number of elements
C              of data type ITYPE remaining in the stack.
C
C  Copyright:  1983 by IMSL, Inc.  All Rights Reserved
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1KQU (ITYPE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    ITYPE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ISIZE(6), LALC, LBND, LBOOK, LMAX, LNEED, LNOW, LOUT,
     &           LUSED
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(61913)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ RWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
      EQUIVALENCE (ISIZE(1), IWKSP(11))
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MAX0
      INTRINSIC  MAX0
      INTEGER    MAX0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, IWKIN
C
      DATA FIRST/.TRUE./
C
      CALL E1PSH ('I1KQU ')
C
      IF (FIRST) THEN
C                                  INITIALIZE WORKSPACE IF NEEDED
         FIRST = .FALSE.
         CALL IWKIN (0)
      END IF
C                                  BOOKKEEPING OVERWRITTEN
      IF (LNOW.LT.LBOOK .OR. LNOW.GT.LUSED .OR. LUSED.GT.LMAX .OR.
     &    LNOW.GE.LBND .OR. LOUT.GT.LALC) THEN
         CALL E1MES (5, 7, 'One or more of the first eight '//
     &               'bookkeeping locations in IWKSP have been '//
     &               'overwritten.')
      ELSE IF (ITYPE.LE.0 .OR. ITYPE.GE.7) THEN
C                                  ILLEGAL DATA TYPE REQUESTED
         CALL E1MES (5, 8, 'Illegal data type requested.')
      ELSE
C                                  THIS CALCULATION ALLOWS FOR THE
C                                  TWO POINTER LOCATIONS IN THE STACK
C                                  WHICH ARE ASSIGNED TO EACH ALLOCATION
         I1KQU = MAX0(((LBND-3)*ISIZE(2))/ISIZE(ITYPE)-(LNOW*ISIZE(2)-
     &           1)/ISIZE(ITYPE)-1,0)
      END IF
C
      CALL E1POP ('I1KQU ')
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  IWKIN (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    January 17, 1984
C
C  Purpose:    Initialize bookkeeping locations describing the
C              workspace stack.
C
C  Usage:      CALL IWKIN (NSU)
C
C  Argument:
C     NSU    - Number of numeric storage units to which the workspace
C              stack is to be initialized
C
C  GAMS:       N4
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IWKIN (NSU)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NSU
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    ISIZE(6), LALC, LBND, LBOOK, LMAX, LNEED, LNOW, LOUT,
     &           LUSED, MELMTS, MTYPE
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      LOGICAL    FIRST
      SAVE       FIRST
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(61913)
      REAL       RDWKSP(5000)
      DOUBLE PRECISION DWKSP(2500)
      COMPLEX    CWKSP(2500)
      COMPLEX    CZWKSP(2500)
      COMPLEX    *16 ZWKSP(1250)
      INTEGER    IWKSP(5000)
      LOGICAL    LWKSP(5000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ RWKSP
C                                  SPECIFICATIONS FOR EQUIVALENCE
      EQUIVALENCE (LOUT, IWKSP(1))
      EQUIVALENCE (LNOW, IWKSP(2))
      EQUIVALENCE (LUSED, IWKSP(3))
      EQUIVALENCE (LBND, IWKSP(4))
      EQUIVALENCE (LMAX, IWKSP(5))
      EQUIVALENCE (LALC, IWKSP(6))
      EQUIVALENCE (LNEED, IWKSP(7))
      EQUIVALENCE (LBOOK, IWKSP(8))
      EQUIVALENCE (ISIZE(1), IWKSP(11))
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  MAX0
      INTRINSIC  MAX0
      INTEGER    MAX0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1STI
C
      DATA FIRST/.TRUE./
C
      IF (.NOT.FIRST) THEN
         IF (NSU .NE. 0) THEN
            CALL E1STI (1, LMAX)
            CALL E1MES (5, 100, 'Error from subroutine IWKIN:  '//
     &                  'Workspace stack has previously been '//
     &                  'initialized to %(I1). Correct by making the '//
     &                  'call to IWKIN the first executable '//
     &                  'statement in the main program.  ')
C
            STOP
C
         ELSE
            RETURN
         END IF
      END IF
C
      IF (NSU .EQ. 0) THEN
C                                  IF NSU=0 USE DEFAULT SIZE 5000
         MELMTS = 5000
      ELSE
         MELMTS = NSU
      END IF
C                                  NUMBER OF ITEMS .LT. 0
      IF (MELMTS .LE. 0) THEN
         CALL E1STI (1, MELMTS)
         CALL E1MES (5, 1, 'Error from subroutine IWKIN:  Number '//
     &               'of numeric storage units is not positive. NSU '//
     &               '= %(I1) ')
      ELSE
C
         FIRST = .FALSE.
C                                  HERE TO INITIALIZE
C
C                                  SET DATA SIZES APPROPRIATE FOR A
C                                  STANDARD CONFORMING FORTRAN SYSTEM
C                                  USING THE FORTRAN
C                                  *NUMERIC STORAGE UNIT* AS THE
C                                  MEASURE OF SIZE.
C
C                                  TYPE IS REAL
         MTYPE = 3
C                                  LOGICAL
         ISIZE(1) = 1
C                                  INTEGER
         ISIZE(2) = 1
C                                  REAL
         ISIZE(3) = 1
C                                  DOUBLE PRECISION
         ISIZE(4) = 2
C                                  COMPLEX
         ISIZE(5) = 2
C                                  DOUBLE COMPLEX
         ISIZE(6) = 4
C                                  NUMBER OF WORDS USED FOR BOOKKEEPING
         LBOOK = 16
C                                  CURRENT ACTIVE LENGTH OF THE STACK
         LNOW = LBOOK
C                                  MAXIMUM VALUE OF LNOW ACHIEVED THUS
C                                  FAR
         LUSED = LBOOK
C                                  MAXIMUM LENGTH OF THE STORAGE ARRAY
         LMAX = MAX0(MELMTS,((LBOOK+2)*ISIZE(2)+ISIZE(3)-1)/ISIZE(3))
C                                  LOWER BOUND OF THE PERMANENT STORAGE
C                                  WHICH IS ONE WORD MORE THAN THE
C                                  MAXIMUM ALLOWED LENGTH OF THE STACK
         LBND = LMAX + 1
C                                  NUMBER OF CURRENT ALLOCATIONS
         LOUT = 0
C                                  TOTAL NUMBER OF ALLOCATIONS MADE
         LALC = 0
C                                  NUMBER OF WORDS BY WHICH THE ARRAY
C                                  SIZE MUST BE INCREASED FOR ALL PAST
C                                  ALLOCATIONS TO SUCCEED
         LNEED = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1X (Single precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    August 30, 1985
C
C  Purpose:    Determine the array subscript indicating the starting
C              element at which a key character sequence begins.
C              (Case-sensitive version)
C
C  Usage:      I1X(CHRSTR, I1LEN, KEY, KLEN)
C
C  Arguments:
C     CHRSTR - Character array to be searched.  (Input)
C     I1LEN  - Length of CHRSTR.  (Input)
C     KEY    - Character array that contains the key sequence.  (Input)
C     KLEN   - Length of KEY.  (Input)
C     I1X    - Integer function.  (Output)
C
C  Remarks:
C  1. Returns zero when there is no match.
C
C  2. Returns zero if KLEN is longer than ISLEN.
C
C  3. Returns zero when any of the character arrays has a negative or
C     zero length.
C
C  GAMS:       N5c
C
C  Chapter:    MATH/LIBRARY Utilities
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1X (CHRSTR, I1LEN, KEY, KLEN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    I1LEN, KLEN
      CHARACTER  CHRSTR(*), KEY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, II, J
C
      I1X = 0
      IF (KLEN.LE.0 .OR. I1LEN.LE.0) GO TO 9000
      IF (KLEN .GT. I1LEN) GO TO 9000
C
      I = 1
      II = I1LEN - KLEN + 1
   10 IF (I .LE. II) THEN
         IF (CHRSTR(I) .EQ. KEY(1)) THEN
            DO 20  J=2, KLEN
               IF (CHRSTR(I+J-1) .NE. KEY(J)) GO TO 30
   20       CONTINUE
            I1X = I
            GO TO 9000
   30       CONTINUE
         END IF
         I = I + 1
         GO TO 10
      END IF
C
 9000 RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  C1TIC
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 9, 1984
C
C  Purpose:    Convert an integer to its corresponding character form.
C              (Right justified)
C
C  Usage:      CALL C1TIC(NUM, CHRSTR, SLEN, IER)
C
C  Arguments:
C     NUM    - Integer number.  (Input)
C     CHRSTR - Character array that receives the result.  (Output)
C     SLEN   - Length of the character array.  (Input)
C     IER    - Completion code.  (Output) Where
C                 IER < 0  indicates that SLEN <= 0,
C                 IER = 0  indicates normal completion,
C                 IER > 0  indicates that the character array is too
C                       small to hold the complete number.  IER
C                       indicates how many significant digits are
C                       being truncated.
C
C  Remarks:
C  1. The character array is filled in a right justified manner.
C  2. Leading zeros are replaced by blanks.
C  3. Sign is inserted only for negative number.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE C1TIC (NUM, CHRSTR, SLEN, IER)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NUM, SLEN, IER
      CHARACTER  CHRSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, J, K, L
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK(1), DIGIT(10), MINUS(1)
      SAVE       BLANK, DIGIT, MINUS
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   M1VE
C
      DATA DIGIT/'0', '1', '2', '3', '4', '5', '6', '7', '8',
     &     '9'/
      DATA BLANK/' '/, MINUS/'-'/
C                                  CHECK SLEN
      IF (SLEN .LE. 0) THEN
         IER = -1
         RETURN
      END IF
C                                  THE NUMBER IS ZERO
      IF (NUM .EQ. 0) THEN
         CALL M1VE (BLANK, 1, 1, 1, CHRSTR, 1, SLEN-1, SLEN, I)
         CHRSTR(SLEN) = DIGIT(1)
         IER = 0
         RETURN
      END IF
C                                  CONVERT NUMBER DIGIT BY DIGIT TO
C                                  CHARACTER FORM
      J = SLEN
      K = IABS(NUM)
   10 IF (K.GT.0 .AND. J.GE.1) THEN
         L = K
         K = K/10
         L = L - K*10
         CHRSTR(J) = DIGIT(L+1)
         J = J - 1
         GO TO 10
      END IF
C
   20 IF (K .EQ. 0) THEN
         IF (NUM .LT. 0) THEN
            CALL M1VE (MINUS, 1, 1, 1, CHRSTR, J, J, SLEN, I)
            IF (I .NE. 0) THEN
               IER = 1
               RETURN
            END IF
            J = J - 1
         END IF
         IER = 0
         CALL M1VE (BLANK, 1, 1, 1, CHRSTR, 1, J, SLEN, I)
         RETURN
      END IF
C                                  DETERMINE THE NUMBER OF SIGNIFICANT
C                                  DIGITS BEING TRUNCATED
      I = 0
   30 IF (K .GT. 0) THEN
         K = K/10
         I = I + 1
         GO TO 30
      END IF
C
      IF (NUM .LT. 0) I = I + 1
      IER = I
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  N1RGB
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    Return a positive number as a flag to indicated that a
C              stop should occur due to one or more global errors.
C
C  Usage:      N1RGB(IDUMMY)
C
C  Arguments:
C     IDUMMY - Integer scalar dummy argument.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION N1RGB (IDUMMY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IDUMMY
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  INITIALIZE FUNCTION
      N1RGB = 0
C                                  CHECK FOR GLOBAL ERROR TYPE 6
      IF (IFERR6 .GT. 0) THEN
         N1RGB = STOPTB(6)
         IFERR6 = 0
      END IF
C                                  CHECK FOR GLOBAL ERROR TYPE 7
      IF (IFERR7 .GT. 0) THEN
         N1RGB = STOPTB(7)
         IFERR7 = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SDOT (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Compute the single-precision dot product x*y.
C
C  Usage:      SDOT(N, SX, INCX, SY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     SX     - Real vector of length MAX(N*IABS(INCX),1).  (Input)
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be.. SX(1+(I-1)*INCX) if INCX .GE. 0
C              or SX(1+(I-N)*INCX) if INCX .LT. 0.
C     SY     - Real vector of length MAX(N*IABS(INCY),1).  (Input)
C     INCY   - Displacement between elements of SY.  (Input)
C              Y(I) is defined to be.. SY(1+(I-1)*INCY) if INCY .GE. 0
C              or SY(1+(I-N)*INCY) if INCY .LT. 0.
C     SDOT   - Sum from I=1 to N of X(I)*Y(I).  (Output)
C              X(I) and Y(I) refer to specific elements of SX and SY,
C              respectively.  See INCX and INCY argument descriptions.
C
C  GAMS:       D1a4
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      REAL FUNCTION SDOT (N, SX, INCX, SY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      REAL       SX(*), SY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      SDOT = 0.0E0
      IF (N .GT. 0) THEN
         IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS
            IX = 1
            IY = 1
            IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
            IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
            DO 10  I=1, N
               SDOT = SDOT + SX(IX)*SY(IY)
               IX = IX + INCX
               IY = IY + INCY
   10       CONTINUE
         ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
            M = MOD(N,5)
C                                  CLEAN-UP LOOP SO REMAINING VECTOR
            DO 30  I=1, M
               SDOT = SDOT + SX(I)*SY(I)
   30       CONTINUE
            MP1 = M + 1
            DO 40  I=MP1, N, 5
               SDOT = SDOT + SX(I)*SY(I) + SX(I+1)*SY(I+1) +
     &                SX(I+2)*SY(I+2) + SX(I+3)*SY(I+3) +
     &                SX(I+4)*SY(I+4)
   40       CONTINUE
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1POS
C
C  Computer:   DGC/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    Set or retrieve print and stop attributes.
C
C  Usage:      CALL E1POS(IERTYP,IPATT,ISATT)
C
C  Arguments:
C     IERTYP - Integer specifying the error type for which print and
C              stop attributes are to be set or retrieved.  (Input)  If
C              IERTYP is 0 then the settings apply to all error types.
C              If IERTYP is between 1 and 7, then the settings only
C              apply to that specified error type.  If IERTYP is
C              negative then the current print and stop attributes will
C              be returned in IPATT and ISATT.
C     IPATT  - If IERTYP is positive, IPATT is an integer specifying the
C              desired print attribute as follows: -1 means no change,
C              0 means NO, 1 means YES, and 2 means assign the default
C              setting.  (Input)  If IERTYP is negative, IPATT is
C              returned as 1 if print is YES or 0 if print is NO for
C              error type IABS(IERTYP).  (Output)
C     ISATT  - If IERTYP is positive, ISATT is an integer specifying the
C              desired stop attribute as follows: -1 means no change,
C              0 means NO, 1 means YES, and 2 means assign the default
C              setting.  (Input)  If IERTYP is negative, ISATT is
C              returned as 1 if print is YES or 0 if print is NO for
C              error type IABS(IERTYP).  (Output)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1POS (IERTYP, IPATT, ISATT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IERTYP, IPATT, ISATT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IER
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    DEFLTP(7), DEFLTS(7), IFINIT
      SAVE       DEFLTP, DEFLTS, IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1MES, E1STI
C
      DATA IFINIT/0/
      DATA DEFLTP/0, 0, 1, 1, 1, 1, 1/, DEFLTS/0, 0, 0, 1, 1, 0, 1/
C                                  INITIALIZE ERROR TABLE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      IER = 0
      IF (IERTYP .GE. 0) THEN
         IF (IPATT.LT.-1 .OR. IPATT.GT.2) THEN
            CALL E1STI (1, IPATT)
            CALL E1MES (5, 1, 'Invalid value specified for print '//
     &                  'table attribute.  IPATT must be -1, 0, 1, '//
     &                  'or 2.  IPATT = %(I1)')
            IER = 1
         END IF
         IF (ISATT.LT.-1 .OR. ISATT.GT.2) THEN
            CALL E1STI (1, ISATT)
            CALL E1MES (5, 1, 'Invalid value specified for stop '//
     &                  'table attribute.  ISATT must be -1, 0, 1, '//
     &                  'or 2.  ISATT = %(I1)')
            IER = 1
         END IF
      END IF
      IF (IER .EQ. 0) THEN
         IF (IERTYP .EQ. 0) THEN
            IF (IPATT.EQ.0 .OR. IPATT.EQ.1) THEN
               DO 10  I=1, 7
   10          PRINTB(I) = IPATT
            ELSE IF (IPATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTINGS
               DO 20  I=1, 7
   20          PRINTB(I) = DEFLTP(I)
            END IF
            IF (ISATT.EQ.0 .OR. ISATT.EQ.1) THEN
               DO 30  I=1, 7
   30          STOPTB(I) = ISATT
            ELSE IF (ISATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTINGS
               DO 40  I=1, 7
   40          STOPTB(I) = DEFLTS(I)
            END IF
         ELSE IF (IERTYP.GE.1 .AND. IERTYP.LE.7) THEN
            IF (IPATT.EQ.0 .OR. IPATT.EQ.1) THEN
               PRINTB(IERTYP) = IPATT
            ELSE IF (IPATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTING
               PRINTB(IERTYP) = DEFLTP(IERTYP)
            END IF
            IF (ISATT.EQ.0 .OR. ISATT.EQ.1) THEN
               STOPTB(IERTYP) = ISATT
            ELSE IF (ISATT .EQ. 2) THEN
C                                  ASSIGN DEFAULT SETTING
               STOPTB(IERTYP) = DEFLTS(IERTYP)
            END IF
         ELSE IF (IERTYP.LE.-1 .AND. IERTYP.GE.-7) THEN
            I = IABS(IERTYP)
            IPATT = PRINTB(I)
            ISATT = STOPTB(I)
         END IF
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1STL
C
C  Computer:   DGC/SINGLE
C
C  Revised:    November 8, 1985
C
C  Purpose:    To store a string for subsequent use within an error
C              message.
C
C  Usage:      CALL E1STL(IL,STRING)
C
C  Arguments:
C     IL     - Integer specifying the substitution index.  IL must be
C              between 1 and 9.  (Input)
C     STRING - A character string.  (Input)
C
C  Copyright:  1985 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1STL (IL, STRING)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IL
      CHARACTER  STRING*(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, LEN2
      CHARACTER  STRGUP(255)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IFINIT
      SAVE       IFINIT
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS,LEN,MIN0
      INTRINSIC  IABS, LEN, MIN0
      INTEGER    IABS, LEN, MIN0
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1INIT, E1INPL
C
      DATA IFINIT/0/
C                                  INITIALIZE IF NECESSARY
      IF (IFINIT .EQ. 0) THEN
         CALL E1INIT
         IFINIT = 1
      END IF
      LEN2 = LEN(STRING)
      LEN2 = MIN0(LEN2,255)
      DO 10  I=1, LEN2
         STRGUP(I) = STRING(I:I)
   10 CONTINUE
      IF (IABS(IL).GE.1 .AND. IABS(IL).LE.9) THEN
         CALL E1INPL ('L', IL, LEN2, STRGUP)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  SAXPY (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    August 9, 1986
C
C  Purpose:    Compute the scalar times a vector plus a vector,
C              y = ax + y, all single precision.
C
C  Usage:      CALL SAXPY (N, SA, SX, INCX, SY, INCY)
C
C  Arguments:
C     N      - Length of vectors X and Y.  (Input)
C     SA     - Real scalar.  (Input)
C     SX     - Real vector of length MAX(N*IABS(INCX),1).  (Input)
C     INCX   - Displacement between elements of SX.  (Input)
C              X(I) is defined to be
C                 SX(1+(I-1)*INCX) if INCX.GE.0  or
C                 SX(1+(I-N)*INCX) if INCX.LT.0.
C     SY     - Real vector of length MAX(N*IABS(INCY),1).
C              (Input/Output)
C              SAXPY replaces Y(I) with SA*X(I) + Y(I) for I=1,...,N.
C              X(I) and Y(I) refer to specific elements of SX and SY.
C     INCY   - Displacement between elements of SY.  (Input)
C              Y(I) is defined to be
C                 SY(1+(I-1)*INCY) if INCY.GE.0  or
C                 SY(1+(I-N)*INCY) if INCY.LT.0.
C
C  GAMS:       D1a7
C
C  Chapters:   MATH/LIBRARY Basic Matrix/Vector Operations
C              STAT/LIBRARY Mathematical Support
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SAXPY (N, SA, SX, INCX, SY, INCY)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, INCX, INCY
      REAL       SA, SX(*), SY(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IX, IY, M, MP1
C                                  SPECIFICATIONS FOR SPECIAL CASES
C     INTRINSIC  MOD
      INTRINSIC  MOD
      INTEGER    MOD
C
      IF (N .GT. 0) THEN
         IF (SA .NE. 0.0) THEN
            IF (INCX.NE.1 .OR. INCY.NE.1) THEN
C                                  CODE FOR UNEQUAL INCREMENTS OR EQUAL
C                                  INCREMENTS NOT EQUAL TO 1
               IX = 1
               IY = 1
               IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
               IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
               DO 10  I=1, N
                  SY(IY) = SY(IY) + SA*SX(IX)
                  IX = IX + INCX
                  IY = IY + INCY
   10          CONTINUE
            ELSE
C                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
               M = MOD(N,4)
C                                  CLEAN-UP LOOP
               DO 30  I=1, M
                  SY(I) = SY(I) + SA*SX(I)
   30          CONTINUE
               MP1 = M + 1
               DO 40  I=MP1, N, 4
                  SY(I) = SY(I) + SA*SX(I)
                  SY(I+1) = SY(I+1) + SA*SX(I+1)
                  SY(I+2) = SY(I+2) + SA*SX(I+2)
                  SY(I+3) = SY(I+3) + SA*SX(I+3)
   40          CONTINUE
            END IF
         END IF
      END IF
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1ERIF
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 13, 1984
C
C  Purpose:    Return the position of the first element of a given
C              character array which is not an element of another
C              character array.
C
C  Usage:      I1ERIF(STR1, LEN1, STR2, LEN2)
C
C  Arguments:
C     STR1   - Character array to be searched.  (Input)
C     LEN1   - Length of STR1.  (Input)
C     STR2   - Character array to be searched for.  (Input)
C     LEN2   - Length of STR2.  (Input)
C     I1ERIF - Integer function.  (Output)
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1ERIF (STR1, LEN1, STR2, LEN2)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LEN1, LEN2
      CHARACTER  STR1(*), STR2(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1X
      INTEGER    I1X
C                              FIRST EXECUTABLE STATEMENT
      IF (LEN1.LE.0 .OR. LEN2.LE.0) THEN
         I1ERIF = 1
      ELSE
         DO 10  I=1, LEN1
            IF (I1X(STR2,LEN2,STR1(I),1) .EQ. 0) THEN
               I1ERIF = I
               RETURN
            END IF
   10    CONTINUE
         I1ERIF = 0
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  ICASE (Single precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    September 9, 1985
C
C  Purpose:    Convert from character to the integer ASCII value without
C              regard to case.
C
C  Usage:      ICASE(CH)
C
C  Arguments:
C     CH     - Character to be converted.  (Input)
C     ICASE  - Integer ASCII value for CH without regard to the case
C              of CH.  (Output)
C              ICASE returns the same value as IMSL routine IACHAR for
C              all but lowercase letters.  For these, it returns the
C              IACHAR value for the corresponding uppercase letter.
C
C  GAMS:       N3
C
C  Chapter:    MATH/LIBRARY Utilities
C              STAT/LIBRARY Utilities
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION ICASE (CH)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  CH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   IACHAR
      INTEGER    IACHAR
C
      ICASE = IACHAR(CH)
      IF (ICASE.GE.97 .AND. ICASE.LE.122) ICASE = ICASE - 32
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  S1ANUM
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 28, 1984
C
C  Purpose:    Scan a token and identify it as follows: integer, real
C              number (single/double), FORTRAN relational operator,
C              FORTRAN logical operator, or FORTRAN logical constant.
C
C  Usage:      CALL S1ANUM(INSTR, SLEN, CODE, OLEN)
C
C  Arguments:
C     INSTR  - Character string to be scanned.  (Input)
C     SLEN   - Length of INSTR.  (Input)
C     CODE   - Token code.  (Output)  Where
C                 CODE =  0  indicates an unknown token,
C                 CODE =  1  indicates an integer number,
C                 CODE =  2  indicates a (single precision) real number,
C                 CODE =  3  indicates a (double precision) real number,
C                 CODE =  4  indicates a logical constant (.TRUE. or
C                               .FALSE.),
C                 CODE =  5  indicates the relational operator .EQ.,
C                 CODE =  6  indicates the relational operator .NE.,
C                 CODE =  7  indicates the relational operator .LT.,
C                 CODE =  8  indicates the relational operator .LE.,
C                 CODE =  9  indicates the relational operator .GT.,
C                 CODE = 10  indicates the relational operator .GE.,
C                 CODE = 11  indicates the logical operator .AND.,
C                 CODE = 12  indicates the logical operator .OR.,
C                 CODE = 13  indicates the logical operator .EQV.,
C                 CODE = 14  indicates the logical operator .NEQV.,
C                 CODE = 15  indicates the logical operator .NOT..
C     OLEN   - Length of the token as counted from the first character
C              in INSTR.  (Output)  OLEN returns a zero for an unknown
C              token (CODE = 0).
C
C  Remarks:
C  1. Blanks are considered significant.
C  2. Lower and upper case letters are not significant.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE S1ANUM (INSTR, SLEN, CODE, OLEN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    SLEN, CODE, OLEN
      CHARACTER  INSTR(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, IBEG, IIBEG, J
      LOGICAL    FLAG
      CHARACTER  CHRSTR(6)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    TABPTR(16), TDCNST, TICNST, TOKEN(13), TRCNST, TZERR
      CHARACTER  DIGIT(10), LETTER(52), MINUS, PERIOD, PLUS, TABLE(38)
      SAVE       DIGIT, LETTER, MINUS, PERIOD, PLUS, TABLE, TABPTR,
     &           TDCNST, TICNST, TOKEN, TRCNST, TZERR
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1X, I1CSTR
      INTEGER    I1X, I1CSTR
C
      DATA TOKEN/5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 4, 4/
      DATA TABLE/'D', 'E', 'E', 'Q', 'N', 'E', 'L', 'T', 'L',
     &     'E', 'G', 'T', 'G', 'E', 'A', 'N', 'D', 'O', 'R',
     &     'E', 'Q', 'V', 'N', 'E', 'Q', 'V', 'N', 'O', 'T',
     &     'T', 'R', 'U', 'E', 'F', 'A', 'L', 'S', 'E'/
      DATA TABPTR/1, 2, 3, 5, 7, 9, 11, 13, 15, 18, 20, 23, 27, 30,
     &     34, 39/
      DATA DIGIT/'0', '1', '2', '3', '4', '5', '6', '7', '8',
     &     '9'/
      DATA LETTER/'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
     &     'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
     &     'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'a', 'b', 'c',
     &     'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
     &     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w',
     &     'x', 'y', 'z'/
      DATA PERIOD/'.'/, PLUS/'+'/, MINUS/'-'/
      DATA TZERR/0/, TICNST/1/
      DATA TRCNST/2/, TDCNST/3/
C
      IF (SLEN .LE. 0) THEN
         CODE = 0
         OLEN = 0
         RETURN
      END IF
C                                  STATE 0 - ASSUME ERROR TOKEN
      IBEG = 1
      CODE = TZERR
C                                  CHECK SIGN
      IF (INSTR(IBEG).EQ.MINUS .OR. INSTR(IBEG).EQ.PLUS) THEN
         FLAG = .TRUE.
         IIBEG = IBEG
         IBEG = IBEG + 1
      ELSE
         FLAG = .FALSE.
      END IF
C                                  STATE 1 - ASSUME INTEGER CONSTANT
      IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
         CODE = TICNST
         IIBEG = IBEG
         IBEG = IBEG + 1
C
   10    IF (IBEG .LE. SLEN) THEN
C
            IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
               IIBEG = IBEG
               IBEG = IBEG + 1
               GO TO 10
C
            END IF
C
         ELSE
            GO TO 80
C
         END IF
C
         IF (INSTR(IBEG) .NE. PERIOD) GO TO 80
      END IF
C                                  STATE 2 - ASSUME REAL CONSTANT
      IF (CODE .EQ. TICNST) THEN
         CODE = TRCNST
         IIBEG = IBEG
         IBEG = IBEG + 1
         IF (IBEG .GT. SLEN) GO TO 80
      ELSE IF (INSTR(IBEG).EQ.PERIOD .AND. SLEN.GE.2) THEN
         IF (I1X(DIGIT,10,INSTR(IBEG+1),1) .NE. 0) THEN
            CODE = TRCNST
            IIBEG = IBEG + 1
            IBEG = IBEG + 2
            IF (IBEG .GT. SLEN) GO TO 80
         END IF
      END IF
C
      IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
         CODE = TRCNST
         IIBEG = IBEG
         IBEG = IBEG + 1
C
   20    IF (IBEG .LE. SLEN) THEN
C
            IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
               IIBEG = IBEG
               IBEG = IBEG + 1
               GO TO 20
C
            END IF
C
         ELSE
            GO TO 80
C
         END IF
C
      END IF
C
      IF (CODE .EQ. TZERR) THEN
         IF (INSTR(IBEG) .NE. PERIOD) GO TO 80
         IBEG = IBEG + 1
         IF (IBEG .GT. SLEN) GO TO 80
      END IF
C
      IF (I1X(LETTER,52,INSTR(IBEG),1) .EQ. 0) GO TO 80
      CHRSTR(1) = INSTR(IBEG)
C
      DO 30  I=2, 6
         IBEG = IBEG + 1
         IF (IBEG .GT. SLEN) GO TO 80
         IF (I1X(LETTER,52,INSTR(IBEG),1) .EQ. 0) GO TO 40
         CHRSTR(I) = INSTR(IBEG)
   30 CONTINUE
C
      GO TO 80
C
   40 CONTINUE
C
      DO 50  J=1, 15
         IF (I1CSTR(CHRSTR,I-1,TABLE(TABPTR(J)),TABPTR(J+1)-TABPTR(J))
     &        .EQ. 0) GO TO 60
   50 CONTINUE
C
      GO TO 80
C                                  STATE 4 - LOGICAL OPERATOR
   60 IF (J .GT. 2) THEN
C
         IF (CODE .EQ. TRCNST) THEN
C
            IF (INSTR(IBEG) .EQ. PERIOD) THEN
               CODE = TICNST
               IIBEG = IIBEG - 1
            END IF
C
            GO TO 80
C
         ELSE IF (INSTR(IBEG) .NE. PERIOD) THEN
            GO TO 80
C
         ELSE IF (FLAG) THEN
            GO TO 80
C
         ELSE
            CODE = TOKEN(J-2)
            IIBEG = IBEG
            GO TO 80
C
         END IF
C
      END IF
C                                  STATE 5 - DOUBLE PRECISION CONSTANT
      IF (CODE .NE. TRCNST) GO TO 80
      IF (INSTR(IBEG).EQ.MINUS .OR. INSTR(IBEG).EQ.PLUS) IBEG = IBEG +
     &    1
      IF (IBEG .GT. SLEN) GO TO 80
C
      IF (I1X(DIGIT,10,INSTR(IBEG),1) .EQ. 0) THEN
         GO TO 80
C
      ELSE
         IIBEG = IBEG
         IBEG = IBEG + 1
C
   70    IF (IBEG .LE. SLEN) THEN
C
            IF (I1X(DIGIT,10,INSTR(IBEG),1) .NE. 0) THEN
               IIBEG = IBEG
               IBEG = IBEG + 1
               GO TO 70
C
            END IF
C
         END IF
C
      END IF
C
      IF (J .EQ. 1) CODE = TDCNST
C
   80 CONTINUE
C
      IF (CODE .EQ. TZERR) THEN
         OLEN = 0
C
      ELSE
         OLEN = IIBEG
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  IMACH (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 26, 1984
C
C  Purpose:    Retrieve integer machine constants.
C
C  Usage:      IMACH(N)
C
C  Arguments:
C     N      - Index of desired constant.  (Input)
C     IMACH  - Machine constant.  (Output)
C
C  Remark:
C     Following is a description of the assorted integer machine
C     constants.
C
C     Words
C
C        IMACH( 1) = Number of bits per integer storage unit.
C        IMACH( 2) = Number of characters per integer storage unit.
C
C     Integers
C
C        Assume integers are represented in the S-DIGIT, BASE-A form
C        SIGN ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
C        where 0 .LE. X(I) .LT. A for I=0,...,S-1.  Then
C
C        IMACH( 3) = A, the base.
C        IMACH( 4) = S, number of BASE-A digits.
C        IMACH( 5) = A**S - 1, largest magnitude.
C
C     Floating-point numbers
C
C        Assume floating-point numbers are represented in the T-DIGIT,
C        BASE-B form SIGN (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
C        where 0 .LE. X(I) .LT. B for I=1,...,T,
C        0 .LT. X(1), and EMIN .LE. E .LE. EMAX.  Then
C
C        IMACH( 6) = B, the base.
C
C        Single precision
C
C           IMACH( 7) = T, number of BASE-B digits.
C           IMACH( 8) = EMIN, smallest exponent E.
C           IMACH( 9) = EMAX, largest exponent E.
C
C        Double precision
C
C           IMACH(10) = T, number of BASE-B digits.
C           IMACH(11) = EMIN, smallest exponent E.
C           IMACH(12) = EMAX, largest exponent E.
C
C  GAMS:       R1
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C              SFUN/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IMACH (N)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    NOUT
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IMACHV(12)
      SAVE       IMACHV
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   UMACH
C                                  DEFINE CONSTANTS
      DATA IMACHV(1)/32/
      DATA IMACHV(2)/4/
      DATA IMACHV(3)/2/
      DATA IMACHV(4)/31/
      DATA IMACHV(5)/2147483647/
      DATA IMACHV(6)/2/
      DATA IMACHV(7)/24/
      DATA IMACHV(8)/-125/
      DATA IMACHV(9)/128/
      DATA IMACHV(10)/53/
      DATA IMACHV(11)/-1021/
      DATA IMACHV(12)/1024/
C
      IF (N.LT.1 .OR. N.GT.12) THEN
C                                  ERROR.  INVALID RANGE FOR N.
         CALL UMACH (2, NOUT)
         WRITE (NOUT,99999) N
99999    FORMAT (/, ' *** TERMINAL ERROR 5 from IMACH.  The argument',
     &          /, ' ***          must be between 1 and 12 inclusive.'
     &          , /, ' ***          N = ', I6, '.', /)
         IMACH = 0
         STOP
C
      ELSE
         IMACH = IMACHV(N)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  E1INPL
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 2, 1984
C
C  Purpose:    To store a character string in the parameter list PLIST
C              for use by the error message handler.
C
C  Usage:      CALL E1INPL(FORM,NUM,SLEN,STRUP)
C
C  Arguments:
C     FORM   - A character string of length one to be inserted into
C              PLIST which specifies the form of the string.  (Input)
C              For example, 'L' for string, 'A' for character array,
C              'I' for integer, 'K' for keyword (PROTRAN only).  An
C              asterisk is inserted into PLIST preceding FORM.
C     NUM    - Integer to be inserted as a character into PLIST
C              immediately following FORM.  (Input)  NUM must be between
C              1 and 9.
C     SLEN   - The number of characters in STRUP.  (Input)  LEN must be
C              less than or equal to 255.  The character representation
C              of SLEN is inserted into PLIST after NUM and an asterisk.
C     STRUP  - A character string of length LEN which is to be inserted
C              into PLIST.  (Input)  Trailing blanks are ignored.
C
C  Copyright:  1984 by IMSL, Inc.  All rights reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE E1INPL (FORM, NUM, SLEN, STRUP)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    NUM, SLEN
      CHARACTER  FORM, STRUP(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IER, L, LEN2, LENCK, LOC, NLEN, NNUM
      CHARACTER  STRNCH(3)
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      CHARACTER  BLANK, PRCNT(1), TEMP(4)
      SAVE       BLANK, PRCNT, TEMP
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                              SPECIFICATIONS FOR COMMON /ERCOM1/
      INTEGER    CALLVL, MAXLEV, MSGLEN, ERTYPE(51), ERCODE(51),
     &           PRINTB(7), STOPTB(7), PLEN, IFERR6, IFERR7,
     &           IALLOC(51), HDRFMT(7), TRACON(7)
      COMMON     /ERCOM1/ CALLVL, MAXLEV, MSGLEN, ERTYPE, ERCODE,
     &           PRINTB, STOPTB, PLEN, IFERR6, IFERR7, IALLOC, HDRFMT,
     &           TRACON
      SAVE       /ERCOM1/
C                              SPECIFICATIONS FOR COMMON /ERCOM2/
      CHARACTER  MSGSAV(255), PLIST(300), RNAME(51)*6
      COMMON     /ERCOM2/ MSGSAV, PLIST, RNAME
      SAVE       /ERCOM2/
C                              SPECIFICATIONS FOR COMMON /ERCOM3/
      DOUBLE PRECISION ERCKSM
      COMMON     /ERCOM3/ ERCKSM
      SAVE       /ERCOM3/
C                              SPECIFICATIONS FOR COMMON /ERCOM4/
      LOGICAL    ISUSER(51)
      COMMON     /ERCOM4/ ISUSER
      SAVE       /ERCOM4/
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   C1TIC, M1VE
C
      DATA TEMP/'*', ' ', ' ', '*'/, PRCNT/'%'/, BLANK/' '/
C
      NNUM = IABS(NUM)
      LENCK = PLEN + SLEN + 8
      IF (NNUM.GE.1 .AND. NNUM.LE.9 .AND. LENCK.LE.300) THEN
         TEMP(2) = FORM
         CALL C1TIC (NNUM, TEMP(3), 1, IER)
         LOC = PLEN + 1
         IF (LOC .EQ. 2) LOC = 1
         CALL M1VE (TEMP, 1, 4, 4, PLIST(LOC), 1, 4, 262, IER)
         LOC = LOC + 4
         IF (NUM .LT. 0) THEN
            LEN2 = SLEN
         ELSE
            DO 10  L=1, SLEN
               LEN2 = SLEN - L + 1
               IF (STRUP(LEN2) .NE. BLANK) GO TO 20
   10       CONTINUE
            LEN2 = 1
   20       CONTINUE
         END IF
         NLEN = 1
         IF (LEN2 .GE. 10) NLEN = 2
         IF (LEN2 .GE. 100) NLEN = 3
         CALL C1TIC (LEN2, STRNCH, NLEN, IER)
         CALL M1VE (STRNCH, 1, NLEN, 3, PLIST(LOC), 1, NLEN, 262, IER)
         LOC = LOC + NLEN
         CALL M1VE (PRCNT, 1, 1, 1, PLIST(LOC), 1, 1, 262, IER)
         LOC = LOC + 1
         CALL M1VE (STRUP, 1, LEN2, LEN2, PLIST(LOC), 1, LEN2, 262,
     &              IER)
         PLEN = LOC + LEN2 - 1
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  UMACH (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    March 21, 1984
C
C  Purpose:    Set or retrieve input or output device unit numbers.
C
C  Usage:      CALL UMACH (N, NUNIT)
C
C  Arguments:
C     N      - Index of desired unit.  (Input)
C              The values of N are defined as follows:
C              N = 1, corresponds to the standard input unit.
C              N = 2, corresponds to the standard output unit.
C     NUNIT  - I/O unit.  (Input or Output)
C              If the value of N is negative, the unit corresponding
C              to the index is reset to the value given in NUNIT.
C              Otherwise, the value corresponding to the index is
C              returned in NUNIT.
C
C  GAMS:       R1
C
C  Chapters:   MATH/LIBRARY Reference Material
C              STAT/LIBRARY Reference Material
C              SFUN/LIBRARY Reference Material
C
C  Copyright:  1984 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE UMACH (N, NUNIT)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, NUNIT
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    NN, NOUT
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    UNIT(2)
      SAVE       UNIT
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  IABS
      INTRINSIC  IABS
      INTEGER    IABS
C
      DATA UNIT(1)/5/
      DATA UNIT(2)/6/
C
      NN = IABS(N)
      IF (NN.NE.1 .AND. NN.NE.2) THEN
C                                  ERROR.  INVALID RANGE FOR N.
         NOUT = UNIT(2)
         WRITE (NOUT,99999) NN
99999    FORMAT (/, ' *** TERMINAL ERROR 5 from UMACH.  The absolute',
     &          /, ' ***          value of the index variable must be'
     &          , /, ' ***          1 or 2.  IABS(N) = ', I6,
     &          '.', /)
         STOP
C                                  CHECK FOR RESET OR RETRIEVAL
      ELSE IF (N .LT. 0) THEN
C                                  RESET
         UNIT(NN) = NUNIT
      ELSE
C                                  RETRIEVE
         NUNIT = UNIT(N)
      END IF
C
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  IACHAR (Single precision version)
C
C  Computer:   PCDSMS/SINGLE
C
C  Revised:    September 9, 1985
C
C  Purpose:    Return the integer ASCII value of a character argument.
C
C  Usage:      IACHAR(CH)
C
C  Arguments:
C     CH     - Character argument for which the integer ASCII value
C              is desired.  (Input)
C     IACHAR - Integer ASCII value for CH.  (Output)
C              The character CH is in the IACHAR-th position of the
C              ASCII collating sequence.
C
C  GAMS:       N3
C
C  Chapter:    MATH/LIBRARY Utilities
C              STAT/LIBRARY Utilities
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION IACHAR (CH)
C                                  SPECIFICATIONS FOR ARGUMENTS
      CHARACTER  CH
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      IACHAR = ICHAR(CH)
      RETURN
      END
C-----------------------------------------------------------------------
C  IMSL Name:  I1CSTR (Single precision version)
C
C  Computer:   DGC/SINGLE
C
C  Revised:    September 10, 1985
C
C  Purpose:    Case insensitive comparison of two character arrays.
C
C  Usage:      I1CSTR(STR1, LEN1, STR2, LEN2)
C
C  Arguments:
C     STR1   - First character array.  (Input)
C     LEN1   - Length of STR1.  (Input)
C     STR2   - Second character array.  (Input)
C     LEN2   - Length of STR2.  (Input)
C     I1CSTR - Integer function.  (Output) Where
C              I1CSTR = -1  if STR1 .LT. STR2,
C              I1CSTR =  0  if STR1 .EQ. STR2,
C              I1CSTR =  1  if STR1 .GT. STR2.
C
C  Remarks:
C  1. If the two arrays, STR1 and STR2,  are of unequal length, the
C     shorter array is considered as if it were extended with blanks
C     to the length of the longer array.
C
C  2. If one or both lengths are zero or negative the I1CSTR output is
C     based on comparison of the lengths.
C
C  GAMS:       N5c
C
C  Chapter:    MATH/LIBRARY Utilities
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION I1CSTR (STR1, LEN1, STR2, LEN2)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    LEN1, LEN2
      CHARACTER  STR1(LEN1), STR2(LEN2)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    IC1, IC2, ICB, IS, L, LENM
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  ISIGN,MIN0
      INTRINSIC  ISIGN, MIN0
      INTEGER    ISIGN, MIN0
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   ICASE
      INTEGER    ICASE
C
      IF (LEN1.GT.0 .AND. LEN2.GT.0) THEN
C                                  COMPARE FIRST LENM CHARACTERS
         LENM = MIN0(LEN1,LEN2)
         DO 10  L=1, LENM
            IC1 = ICASE(STR1(L))
            IC2 = ICASE(STR2(L))
            IF (IC1 .NE. IC2) THEN
               I1CSTR = ISIGN(1,IC1-IC2)
               RETURN
            END IF
   10    CONTINUE
      END IF
C                                  COMPARISON BASED ON LENGTH OR
C                                  TRAILING BLANKS
      IS = LEN1 - LEN2
      IF (IS .EQ. 0) THEN
         I1CSTR = 0
      ELSE
         IF (LEN1.LE.0 .OR. LEN2.LE.0) THEN
C                                  COMPARISON BASED ON LENGTH
            I1CSTR = ISIGN(1,IS)
         ELSE
C                                  COMPARISON BASED ON TRAILING BLANKS
C                                  TO EXTEND SHORTER ARRAY
            LENM = LENM + 1
            ICB = ICASE(' ')
            IF (IS .GT. 0) THEN
C                                  EXTEND STR2 WITH BLANKS
               DO 20  L=LENM, LEN1
                  IC1 = ICASE(STR1(L))
                  IF (IC1 .NE. ICB) THEN
                     I1CSTR = ISIGN(1,IC1-ICB)
                     RETURN
                  END IF
   20          CONTINUE
            ELSE
C                                  EXTEND STR1 WITH BLANKS
               DO 30  L=LENM, LEN2
                  IC2 = ICASE(STR2(L))
                  IF (ICB .NE. IC2) THEN
                     I1CSTR = ISIGN(1,ICB-IC2)
                     RETURN
                  END IF
   30          CONTINUE
            END IF
C
            I1CSTR = 0
         END IF
      END IF
C
      RETURN
      END
