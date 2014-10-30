
!***********************************************************************************************************************************
!**                                            S U B R O U T I N E   W I T H D R A W A L                                          **
!***********************************************************************************************************************************

SUBROUTINE WITHDRAWAL
  USE GLOBAL; USE GEOMC; USE TVDC; USE SELWC; USE LOGICC; USE MAIN, ONLY: DERIVED_CALC, CDN,tdgon, jsg,  nnsg, ndo, jwd   ! cb 1/16/13
  
  IMPLICIT NONE
  REAL :: HSWT,HSWB,ELR,WSEL,ELSTR,COEF,RATIO,HT,RHOFT,DLRHOT,HB,RHOFB,DLRHOB,VSUM,DLRHOMAX,HWDT,HWDB,ELWD,TEMPEST,ESTRTEST,QSUMJS
  REAL :: FRACV,QSUMWD
  INTEGER :: K,JS,KSTR,KTOP,KBOT,KWD,JJWD     ! jwd

RETURN

!***********************************************************************************************************************************
!**                                             D O W N S T R E A M   W I T H D R A W A L                                         **
!***********************************************************************************************************************************

ENTRY DOWNSTREAM_WITHDRAWAL (JS)

! Variable initialization

  HSWT = 0.0; HSWB = 0.0; VNORM = 0.0; QNEW = 0.0

! Water surface elevation

  ELR  = SINA(JB)*DLX(ID)*0.5
  WSEL = ELWS(ID)-ELR                   !EL(KT,ID)-Z(ID)*COSA(JB)

! Structure layer

  DO K=KT,KB(ID)
    IF (EL(K,ID)-ELR < ESTR(JS,JB)) EXIT
  END DO
  KSTR = MAX(K-1,KT)
  KSTR = MIN(KSTR,KB(ID))

! Initial withdrawal limits

  KTOP = MAX(KTSW(JS,JB),KT)
  IF (KSTR < KTOP) KTOP = KSTR
  KBOT = MIN(KBSW(JS,JB),KB(ID))
  IF (KBOT <= KT .AND. KBOT /= KB(ID)) KBOT = KT+1
  IF (KBOT > KB(ID)) KBOT = KB(ID)
  ELSTR = ESTR(JS,JB)
  IF (ESTR(JS,JB) <= EL(KB(ID)+1,ID+1)-ELR) THEN
    KSTR  = KB(ID)
    ELSTR = EL(KB(ID),ID)-ELR
  END IF
  IF (ESTR(JS,JB) > EL(KT,ID)-ELR) ELSTR = WSEL
  IF (KBSW(JS,JB) < KSTR) THEN
    KSTR  = KT
    ELSTR = WSEL
  END IF

! Boundary interference

  COEF = 1.0
  IF ((WSEL-EL(KBOT,ID)-ELR) /= 0.0) THEN
    RATIO = (ELSTR-(EL(KBOT,ID)-ELR))/(WSEL-(EL(KBOT,ID)-ELR))
    IF (RATIO < 0.1 .OR. RATIO > 0.9) COEF = 2.0
  END IF

! Withdrawal zone above structure

  DO K=KSTR-1,KTOP,-1

!** Density frequency

    HT    = (EL(K,ID)-ELR)-ELSTR
    RHOFT = MAX(SQRT((ABS(RHO(K,ID)-RHO(KSTR,ID)))/(HT*RHO(KSTR,ID)+NONZERO)*G),NONZERO)

!** Thickness

    IF (POINT_SINK(JS,JB)) THEN
      HSWT = (COEF*QSTR(JS,JB)/RHOFT)**0.333333
    ELSE
      HSWT = SQRT(2.0*COEF*QSTR(JS,JB)/(WSTR(JS,JB)*RHOFT))
    END IF
    IF (HT >= HSWT) THEN
      KTOP = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELSTR+HSWT) < WSEL) THEN
    DLRHOT = ABS(RHO(KSTR,ID)-RHO(KTOP,ID))
  ELSE IF (WSEL == ELSTR) THEN
    DLRHOT = NONZERO
  ELSE
    DLRHOT = ABS(RHO(KSTR,ID)-RHO(KT,ID))*HSWT/(WSEL-ELSTR)
  END IF
  DLRHOT = MAX(DLRHOT,NONZERO)

! Withdrawal zone below structure

  DO K=KSTR+1,KBOT

!** Density frequency

    HB    = ELSTR-(EL(K,ID)-ELR)
    RHOFB = MAX(SQRT((ABS(RHO(K,ID)-RHO(KSTR,ID)))/(HB*RHO(KSTR,ID)+NONZERO)*G),NONZERO)

!** Thickness

    IF (POINT_SINK(JS,JB)) THEN
      HSWB = (COEF*QSTR(JS,JB)/RHOFB)**0.333333
    ELSE
      HSWB = SQRT(2.0*COEF*QSTR(JS,JB)/(WSTR(JS,JB)*RHOFB))
    END IF
    IF (HB >= HSWB) THEN
      KBOT = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELSTR-HSWB) > EL(KBOT+1,ID)) THEN
    DLRHOB = ABS(RHO(KSTR,ID)-RHO(KBOT,ID))
  ELSE
    DLRHOB = ABS(RHO(KSTR,ID)-RHO(KBOT,ID))*HSWB/(ELSTR-(EL(KBOT+1,ID)-ELR))
  END IF
  DLRHOB = MAX(DLRHOB,NONZERO)

! Velocity profile

  VSUM     = 0.0
!  DLRHOMAX = MAX(DLRHOT,DLRHOB,1.0E-10)                      ! GH 1/31/08
  DO K=KTOP,KBOT
!    VNORM(K) = ABS(1.0-((RHO(K,ID)-RHO(KSTR,ID))/DLRHOMAX)**2)*BHR2(K,ID)
 	   IF(K.GT.KSTR)THEN
       DLRHOMAX = MAX(DLRHOB,1.0E-10)                          !GH 1/31/08
       ELSE
       DLRHOMAX = MAX(DLRHOT,1.0E-10)                          !GH 1/31/08
       ENDIF
     VNORM(K) = 1.0-((RHO(K,ID)-RHO(KSTR,ID))/DLRHOMAX)**2
 	 IF(VNORM(K).GT.1.0) VNORM(K)=1.0                         !GH 1/31/08
	 IF(VNORM(K).LT.0.0) VNORM(K)=0.0                         !GH 1/31/08
	 VNORM(K)=VNORM(K)*BHR2(K,ID)
     VSUM     = VSUM+VNORM(K)
  END DO

! OUTFLOWS
  QSUMJS=0.0                                                  ! SW 7/30/09
  TAVG(JS,JB)=0.0                                                    ! CB 5/12/10
  IF(CONSTITUENTS)CAVG(JS,JB,CN(1:NAC))=0.0
  IF(DERIVED_CALC)CDAVG(JS,JB,CDN(1:NACD(JW),JW))=0.0
  DO K=KTOP,KBOT
    QNEW(K)    = (VNORM(K)/VSUM)*QSTR(JS,JB)
    QOUT(K,JB) =  QOUT(K,JB)+QNEW(K)
    TAVG(JS,JB)=TAVG(JS,JB)+QNEW(K)*T2(K,ID)                  ! SW 7/30/09
    IF(CONSTITUENTS)CAVG(JS,JB,CN(1:NAC))=CAVG(JS,JB,CN(1:NAC))+QNEW(K)*C2(K,ID,CN(1:NAC))  
    IF(DERIVED_CALC)CDAVG(JS,JB,CDN(1:NACD(JW),JW))=CDAVG(JS,JB,CDN(1:NACD(JW),JW))+QNEW(K)*CD(K,ID,CDN(1:NACD(JW),JW))
    QSUMJS=QSUMJS+QNEW(K)
  END DO
IF(QSUMJS.GT.0.0)THEN
  TAVG(JS,JB)=TAVG(JS,JB)/QSUMJS
  IF(CONSTITUENTS)then                    ! cb 1/16/13
    CAVG(JS,JB,CN(1:NAC))=CAVG(JS,JB,CN(1:NAC))/QSUMJS
    if(tdgon)then
      call total_dissolved_gas (palt(id),nnsg,jsg,tavg(js,jb),cavg(js,jb,ndo))        
    end if
  end if
  IF(DERIVED_CALC)then                    ! cb 1/16/13
    CDAVG(JS,JB,CDN(1:NACD(JW),JW))=CDAVG(JS,JB,CDN(1:NACD(JW),JW))/QSUMJS
    if(tdgon)then
      cdavg(js,jb,16)  = (cavg(js,jb,ndo)/exp(7.7117-1.31403*(log(tavg(js,jb)+45.93)))*palt(id))*100.0        
    end if
  end if
ELSE
  TAVG(JS,JB)=-99.0
  IF(CONSTITUENTS)CAVG(JS,JB,CN(1:NAC))=-99.0
  IF(DERIVED_CALC)CDAVG(JS,JB,CDN(1:NACD(JW),JW))=-99.0
END IF

! Inactive layers and total outflow

  IF (JS == NST) THEN
    WHERE (QOUT(:,JB) == 0.0) U(:,ID) = 0.0
  END IF
RETURN
!***********************************************************************************************************************************
!**                                             D O W N S T R E A M   W I T H D R A W A L  ESTIMATE                               **
!***********************************************************************************************************************************

ENTRY DOWNSTREAM_WITHDRAWAL_ESTIMATE(JS,TEMPEST,ESTRTEST)

! VARIABLE INITIALIZATION

  HSWT = 0.0; HSWB = 0.0; VNORM = 0.0; QNEW = 0.0

! Water surface elevation

  ELR  = SINA(JB)*DLX(ID)*0.5
  WSEL = EL(KT,ID)-Z(ID)*COSA(JB)-ELR

! Structure layer

  DO K=KT,KB(ID)
    IF (EL(K,ID)-ELR < estrtest) EXIT
  END DO
  KSTR = MAX(K-1,KT)
  KSTR = MIN(KSTR,KB(ID))

! Initial withdrawal limits

  KTOP = MAX(KTSW(JS,JB),KT)
  IF (KSTR < KTOP) KTOP = KSTR
  KBOT = MIN(KBSW(JS,JB),KB(ID))
  IF (KBOT <= KT .AND. KBOT /= KB(ID)) KBOT = KT+1
  IF (KBOT > KB(ID)) KBOT = KB(ID)                                                                                     !SW 06/03/02
  ELSTR = ESTRTEST
  IF (ESTRTEST <= EL(KB(ID)+1,ID+1)-ELR) THEN                                                                       !SW 10/17/01
    KSTR  = KB(ID)
    ELSTR = EL(KB(ID),ID)-ELR                                                                                          !SW 10/17/01
  END IF
  IF (ESTRTEST > EL(KT,ID)-ELR) ELSTR = WSEL
  IF (KBSW(JS,JB) < KSTR) THEN
    KSTR  = KT
    ELSTR = WSEL                                                                                                       !SW 10/05/00
  END IF

! Boundary interference

  COEF = 1.0
  IF ((WSEL-EL(KBOT,ID)-ELR) /= 0.0) THEN
    RATIO = (ELSTR-(EL(KBOT,ID)-ELR))/(WSEL-(EL(KBOT,ID)-ELR))                                                         !SW 10/17/01
    IF (RATIO < 0.1 .OR. RATIO > 0.9) COEF = 2.0
  END IF

! Withdrawal zone above structure

  DO K=KSTR-1,KTOP,-1

!** Density frequency

    HT    = (EL(K,ID)-ELR)-ELSTR
    RHOFT = MAX(SQRT((ABS(RHO(K,ID)-RHO(KSTR,ID)))/(HT*RHO(KSTR,ID)+NONZERO)*G),NONZERO)

!** Thickness

    IF (POINT_SINK(JS,JB)) THEN
      HSWT = (COEF*QSTR(JS,JB)/RHOFT)**0.333333
    ELSE
      HSWT = SQRT(2.0*COEF*QSTR(JS,JB)/(WSTR(JS,JB)*RHOFT))
    END IF
    IF (HT >= HSWT) THEN
      KTOP = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELSTR+HSWT) < WSEL) THEN
    DLRHOT = ABS(RHO(KSTR,ID)-RHO(KTOP,ID))
  ELSE IF (WSEL == ELSTR) THEN
    DLRHOT = NONZERO
  ELSE
    DLRHOT = ABS(RHO(KSTR,ID)-RHO(KT,ID))*HSWT/(WSEL-ELSTR)
  END IF
  DLRHOT = MAX(DLRHOT,NONZERO)

! Withdrawal zone below structure

  DO K=KSTR+1,KBOT

!** Density frequency

    HB    = ELSTR-(EL(K,ID)-ELR)                                                                                       !SW 10/17/01
    RHOFB = MAX(SQRT((ABS(RHO(K,ID)-RHO(KSTR,ID)))/(HB*RHO(KSTR,ID)+NONZERO)*G),NONZERO)

!** Thickness

    IF (POINT_SINK(JS,JB)) THEN
      HSWB = (COEF*QSTR(JS,JB)/RHOFB)**0.333333
    ELSE
      HSWB = SQRT(2.0*COEF*QSTR(JS,JB)/(WSTR(JS,JB)*RHOFB))
    END IF
    IF (HB >= HSWB) THEN
      KBOT = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELSTR-HSWB) > EL(KBOT+1,ID)) THEN
    DLRHOB = ABS(RHO(KSTR,ID)-RHO(KBOT,ID))
  ELSE
    DLRHOB = ABS(RHO(KSTR,ID)-RHO(KBOT,ID))*HSWB/(ELSTR-(EL(KBOT+1,ID)-ELR))                                           !SW 10/17/01
  END IF
  DLRHOB = MAX(DLRHOB,NONZERO)

! Velocity profile

  VSUM     = 0.0
!  DLRHOMAX = MAX(DLRHOT,DLRHOB,1.0E-10)
  DO K=KTOP,KBOT
  IF(K.GT.KSTR)THEN
       DLRHOMAX = MAX(DLRHOB,1.0E-10)                          !GH 1/31/08
       ELSE
       DLRHOMAX = MAX(DLRHOT,1.0E-10)                          !GH 1/31/08
       ENDIF
       VNORM(K) = 1.0-((RHO(K,ID)-RHO(KSTR,ID))/DLRHOMAX)**2
     IF(VNORM(K).GT.1.0) VNORM(K)=1.0                          !GH 1/31/08
	 IF(VNORM(K).LT.0.0) VNORM(K)=0.0                          !GH 1/31/08
	 VNORM(K)=VNORM(K)*BHR2(K,ID)
     VSUM= VSUM+VNORM(K)
  END DO

! Outflows

  tempest=0.0
  DO K=KTOP,KBOT
    tempest=tempest+t2(k,id)*(VNORM(K)/VSUM)*QSTR(JS,JB)
  END DO

  if(qstr(js,jb).gt.0.0)tempest=tempest/qstr(js,jb)

RETURN
!***********************************************************************************************************************************
!**                                                L A T E R A L   W I T H D R A W A L                                            **
!***********************************************************************************************************************************

ENTRY LATERAL_WITHDRAWAL

! Variable initialization

  VNORM = 0.0; QSW(:,JWD) = 0.0; HWDT = 0.0; HWDB = 0.0

! Structure layer

  K = KT
  DO K=KT,KB(I)
    IF (EL(K,I) < EWD(JWD)) EXIT
  END DO
  KWD = MAX(K-1,KT)
  KWD = MIN(KWD,KB(I))

! Initial withdrawal limits

  KTOP = MAX(KTWD(JWD),KT)
  IF (KWD < KTOP) KTOP = KWD
  KBOT = MIN(KBWD(JWD),KB(I))
  IF (KBOT <= KT .AND. KB(I) /= KBOT) KBOT = KT+1
  IF (KBOT > KB(I)) KBOT = KB(I)
  ELWD = EWD(JWD)
  IF (EWD(JWD) <= EL(KB(I)+1,I)) THEN
    KWD  = KB(I)
    ELWD = EL(KB(I),I)
  END IF
  IF (EWD(JWD) > EL(KT,I)) ELWD = EL(KT,I)
  IF (KBWD(JWD) < KWD) THEN
    KWD  = KT
    ELWD = EL(KT,I)
  END IF

! Boundary interference

  COEF = 1.0
  IF (KT /= KBOT) THEN
    RATIO = (ELWD-EL(KBOT,I))/(EL(KT,I)-EL(KBOT,I))
    IF (RATIO < 0.1 .OR. RATIO > 0.9) COEF = 2.0
  END IF

! Withdrawal zone above structure

  DO K=KWD-1,KTOP,-1

!** Density frequency

    HT    = EL(K,I)-ELWD
    RHOFT = MAX(SQRT((ABS(RHO(K,I)-RHO(KWD,I)))/(HT*RHO(KWD,I)+NONZERO)*G),NONZERO)

!** Thickness

    HWDT = (COEF*QWD(JWD)/RHOFT)**0.333333
    IF (HT >= HWDT) THEN
      KTOP = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELWD+HWDT) < EL(KT,I)) THEN
    DLRHOT = ABS(RHO(KWD,I)-RHO(KTOP,I))
  ELSE IF (EL(KT,I) == ELWD) THEN
    DLRHOT = NONZERO
  ELSE
    DLRHOT = ABS(RHO(KWD,I)-RHO(KT,I))*HWDT/(EL(KT,I)-ELWD)
  END IF
  DLRHOT = MAX(DLRHOT,NONZERO)

! Withdrawal zone below structure

  DO K=KWD+1,KBOT

!** Density frequency

    HB    = ELWD-EL(K,I)
    RHOFB = MAX(SQRT((ABS(RHO(K,I)-RHO(KWD,I)))/(HB*RHO(KWD,I)+NONZERO)*G),NONZERO)

!** Thickness

    HWDB = (COEF*QWD(JWD)/RHOFB)**0.333333
    IF (HB >= HWDB) THEN
      KBOT = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELWD-HWDB) > EL(KBOT+1,I)) THEN
    DLRHOB = ABS(RHO(KWD,I)-RHO(KBOT,I))
  ELSE
    DLRHOB = ABS(RHO(KWD,I)-RHO(KBOT,I))*HWDB/(ELWD-EL(KBOT+1,I))
  END IF
  DLRHOB = MAX(DLRHOB,NONZERO)

! Velocity profile

  VSUM     = 0.0
!  DLRHOMAX = MAX(DLRHOT,DLRHOB,1.0E-10)                                                                             ! SW 1/24/05
  DO K=KTOP,KBOT
!    VNORM(K) = ABS(1.0-((RHO(K,I)-RHO(KWD,I))/DLRHOMAX)**2)*BHR2(K,I)
 	   IF(K.GT.KWD)THEN
       DLRHOMAX = MAX(DLRHOB,1.0E-10)                          !GH 1/31/08
       ELSE
       DLRHOMAX = MAX(DLRHOT,1.0E-10)                          !GH 1/31/08
       ENDIF
     VNORM(K) = 1.0-((RHO(K,I)-RHO(KWD,I))/DLRHOMAX)**2
 	 IF(VNORM(K).GT.1.0) VNORM(K)=1.0                         !GH 1/31/08
	 IF(VNORM(K).LT.0.0) VNORM(K)=0.0                         !GH 1/31/08
	 VNORM(K)=VNORM(K)*BHR2(K,I)
     VSUM     = VSUM+VNORM(K)
  END DO

! Outflows
  QSUMWD=0.0                                                  ! SW 7/30/09
  TAVGW(JWD)=0.0
  IF(CONSTITUENTS)CAVGW(JWD,CN(1:NAC))=0.0
  IF(DERIVED_CALC)CDAVGW(JWD,CDN(1:NACD(JW),JW))=0.0

  DO K=KTOP,KBOT
    FRACV=(VNORM(K)/VSUM)
    QSW(K,JWD) = QSW(K,JWD)+FRACV*QWD(JWD)
    TAVGW(JWD)=TAVGW(JWD)+FRACV*QWD(JWD)*T2(K,I)                  ! SW 7/30/09
    IF(CONSTITUENTS)CAVGW(JWD,CN(1:NAC))=CAVGW(JWD,CN(1:NAC))+FRACV*QWD(JWD)*C2(K,I,CN(1:NAC))  
    IF(DERIVED_CALC)CDAVGW(JWD,CDN(1:NACD(JW),JW))=CDAVGW(JWD,CDN(1:NACD(JW),JW))+FRACV*QWD(JWD)*CD(K,I,CDN(1:NACD(JW),JW))
    QSUMWD=QSUMWD+FRACV*QWD(JWD)
  END DO
  IF(QSUMWD.GT.0.0)THEN
    TAVGW(JWD)=TAVGW(JWD)/QSUMWD               ! SW 7/30/09
    IF(CONSTITUENTS)then                       ! cb 1/16/13
      CAVGW(JWD,CN(1:NAC))=CAVGW(JWD,CN(1:NAC))/QSUMWD  
      if(tdgon)then
        call total_dissolved_gas (palt(i),nnsg,jsg,tavgw(jwd),cavgw(jwd,ndo))  
      end if
    end if
    IF(DERIVED_CALC)then
      CDAVGW(JWD,CDN(1:NACD(JW),JW))=CDAVGW(JWD,CDN(1:NACD(JW),JW))/QSUMWD
      if(tdgon)then
        cdavgw(jwd,16)  = (cavgw(jwd,ndo)/exp(7.7117-1.31403*(log(tavgw(jwd)+45.93)))*palt(id))*100.0        
      end if
    end if
  ELSE
    TAVGW(JWD)=-99.0
    IF(CONSTITUENTS)CAVGW(JWD,CN(1:NAC))=-99.0 
    IF(DERIVED_CALC)CDAVGW(JWD,CDN(1:NACD(JW),JW))=-99.0
  ENDIF
  KTW(JWD) = KTOP
  KBW(JWD) = KBOT
  RETURN
!***********************************************************************************************************************************
!**                                                L A T E R A L   W I T H D R A W A L ESTIMATE                                   **
!***********************************************************************************************************************************

  ENTRY LATERAL_WITHDRAWAL_ESTIMATE (JJWD,TEMPEST,ESTRTEST)

! VARIABLE INITIALIZATION

  VNORM = 0.0; QSW(:,JJWD) = 0.0; HWDT = 0.0; HWDB = 0.0

! Structure layer

  K = KT
  DO K=KT,KB(I)
    IF (EL(K,I) < estrtest) EXIT
  END DO
  KWD = MAX(K-1,KT)
  KWD = MIN(KWD,KB(I))

! Initial withdrawal limits

  KTOP = MAX(KTWD(JJWD),KT)
  IF (KWD < KTOP) KTOP = KWD
  KBOT = MIN(KBWD(JJWD),KB(I))
  IF (KBOT <= KT .AND. KB(I) /= KBOT) KBOT = KT+1
  IF (KBOT > KB(I)) KBOT = KB(I)
  ELWD = ESTRTEST
  IF (ESTRTEST <= EL(KB(I)+1,I)) THEN
    KWD  = KB(I)
    ELWD = EL(KB(I),I)
  END IF
  IF (ESTRTEST > EL(KT,I)) ELWD = EL(KT,I)
  IF (KBWD(JJWD) < KWD) THEN
    KWD  = KT
    ELWD = EL(KT,I)
  END IF

! Boundary interference

  COEF = 1.0
  IF (KT /= KBOT) THEN
    RATIO = (ELWD-EL(KBOT,I))/(EL(KT,I)-EL(KBOT,I))
    IF (RATIO < 0.1 .OR. RATIO > 0.9) COEF = 2.0
  END IF

! Withdrawal zone above structure

  DO K=KWD-1,KTOP,-1

!** Density frequency

    HT    = EL(K,I)-ELWD
    RHOFT = MAX(SQRT((ABS(RHO(K,I)-RHO(KWD,I)))/(HT*RHO(KWD,I)+NONZERO)*G),NONZERO)

!** Thickness

    HWDT = (COEF*QWD(JJWD)/RHOFT)**0.333333
    IF (HT >= HWDT) THEN
      KTOP = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELWD+HWDT) < EL(KT,I)) THEN
    DLRHOT = ABS(RHO(KWD,I)-RHO(KTOP,I))
  ELSE IF (EL(KT,I) == ELWD) THEN
    DLRHOT = NONZERO
  ELSE
    DLRHOT = ABS(RHO(KWD,I)-RHO(KT,I))*HWDT/(EL(KT,I)-ELWD)
  END IF
  DLRHOT = MAX(DLRHOT,NONZERO)

! Withdrawal zone below structure

  DO K=KWD+1,KBOT

!** Density frequency

    HB    = ELWD-EL(K,I)
    RHOFB = MAX(SQRT((ABS(RHO(K,I)-RHO(KWD,I)))/(HB*RHO(KWD,I)+NONZERO)*G),NONZERO)

!** Thickness

    HWDB = (COEF*QWD(JJWD)/RHOFB)**0.333333
    IF (HB >= HWDB) THEN
      KBOT = K; EXIT
    END IF
  END DO

! Reference density

  IF ((ELWD-HWDB) > EL(KBOT+1,I)) THEN
    DLRHOB = ABS(RHO(KWD,I)-RHO(KBOT,I))
  ELSE
    DLRHOB = ABS(RHO(KWD,I)-RHO(KBOT,I))*HWDB/(ELWD-EL(KBOT+1,I))
  END IF
  DLRHOB = MAX(DLRHOB,NONZERO)

! Velocity profile

  VSUM     = 0.0
!  DLRHOMAX = MAX(DLRHOT,DLRHOB,1.0E-10)                                                                             ! SW 1/24/05
  DO K=KTOP,KBOT
!    VNORM(K) = ABS(1.0-((RHO(K,I)-RHO(KWD,I))/DLRHOMAX)**2)*BHR2(K,I)
 	   IF(K.GT.KWD)THEN
       DLRHOMAX = MAX(DLRHOB,1.0E-10)                          !GH 1/31/08
       ELSE
       DLRHOMAX = MAX(DLRHOT,1.0E-10)                          !GH 1/31/08
       ENDIF
     VNORM(K) = 1.0-((RHO(K,I)-RHO(KWD,I))/DLRHOMAX)**2
 	 IF(VNORM(K).GT.1.0) VNORM(K)=1.0                          !GH 1/31/08
	 IF(VNORM(K).LT.0.0) VNORM(K)=0.0                          !GH 1/31/08
	 VNORM(K)=VNORM(K)*BHR2(K,I)
     VSUM = VSUM+VNORM(K)
  END DO

! Outflows

  DO K=KTOP,KBOT
    tempest=tempest+t2(k,i)*(VNORM(K)/VSUM)*QWD(JJWD)
  END DO
  if(qwd(Jjwd).gt.0.0)tempest=tempest/qwd(Jjwd)
  KTW(JJWD) = KTOP
  KBW(JJWD) = KBOT
  return


END SUBROUTINE WITHDRAWAL



MODULE SELECTIVE1 
 REAL                                          :: NXTSTR, NXTTCD, NXTSPLIT,TCDFREQ,TFRQTMP
  CHARACTER(8)                                 :: TEMPC,TSPLTC
  CHARACTER(8), ALLOCATABLE, DIMENSION(:)      :: TCELEVCON,TCYEARLY,TCNTR,TSPLTCNTR,MONCTR,TSYEARLY,DYNSEL,ELCONTSPL
  INTEGER                                      :: NUMTEMPC,NUMTSPLT, TEMPN,NSTT    
  INTEGER, ALLOCATABLE, DIMENSION(:)           :: TCNELEV,TCJB,TCJS,TCISEG,TSPLTJB,NOUTS,KSTRSPLT, JBMON, JSMON, NCOUNTCW,SELD
  REAL,          ALLOCATABLE, DIMENSION(:,:)   :: TCELEV, TEMPCRIT,QSTRFRAC
  REAL,          ALLOCATABLE, DIMENSION(:)     :: TCTEMP,TCTEND,TCTSRT,TCKLAY,TSPLTT,VOLM,QWDFRAC,TSTEND,TSTSRT,NXSEL,TEMP2
  INTEGER, ALLOCATABLE, DIMENSION(:,:)         :: JSTSPLT, NCOUNTC, JSTSPLTT
   REAL,          ALLOCATABLE, DIMENSION(:,:)  :: VOLMC 

END MODULE SELECTIVE1

SUBROUTINE SELECTIVEINIT

USE SELECTIVE1;   USE MAIN
  USE GLOBAL;     USE NAMESC; USE GEOMC;  USE LOGICC; USE PREC;  USE SURFHE;  USE KINETIC; USE SHADEC; USE EDDY
  USE STRUCTURES; USE TRANS;  USE TVDC;   USE SELWC;  USE GDAYC; USE SCREENC; USE TDGAS;   USE RSTART

  IMPLICIT NONE
  
  INTEGER N, IFILE
  REAL DAYTEST

!**                                                   Task 2: Calculations                                                        **
!***********************************************************************************************************************************
      
      IFILE=1949
      TAVG=0.0
      TAVGW=0.0
      DO JB=1,NBR
        IF(NSTR(JB) > 0)THEN
        IFILE=IFILE+1
        WRITE (SEGNUM,'(I0)') JB
        SEGNUM = ADJUSTL(SEGNUM)
        L      = LEN_TRIM(SEGNUM)

        IF(RESTART_IN)THEN
                OPEN  (IFILE,FILE='str_br'//segnum(1:l)//'.opt',POSITION='APPEND')
                JDAY1=0.0
                REWIND (IFILE)
                READ   (IFILE,'(/)',END=13)
                DO WHILE (JDAY1 < JDAY)
                READ (IFILE,'(F10.0)',END=13) JDAY1
                END DO
                BACKSPACE (IFILE)
                13     JDAY1=0.0    
        ELSE
                OPEN  (IFILE,FILE='str_br'//segnum(1:l)//'.opt',status='unknown')
                WRITE(IFILE,*)'Branch:',jb,' # of structures:',nstr(jb),' outlet temperatures'
                WRITE(IFILE,'("      JDAY",<nstr(jb)>(6x,"T(C)"),<nstr(jb)>(3x,"Q(m3/s)"),<nstr(jb)>(4x,"ELEVCL"))')
        ENDIF
        ENDIF
      END DO
 
      IF(NWD > 0)THEN
       IFILE=IFILE+1  
       IF(RESTART_IN)THEN
                OPEN  (IFILE,FILE='wd_out.opt',POSITION='APPEND')
                JDAY1=0.0
                REWIND (IFILE)
                READ   (IFILE,'(/)',END=14)
                DO WHILE (JDAY1 < JDAY)
                READ (IFILE,'(F10.0)',END=14) JDAY1
                END DO
                BACKSPACE (IFILE)
                14    JDAY1=0.0   
       ELSE
        OPEN  (IFILE,FILE='wd_out.opt',STATUS='unknown')
        WRITE(IFILE,*)'Withdrawals: # of withdrawals:',nwd,' outlet temperatures'
        WRITE(IFILE,'("      JDAY",<nwd>(6x,"T(C)"),<nwd>(3x,"Q(m3/s)"),<nwd>(4x,"ELEVCL"))')
       ENDIF
      end if
 
      
      OPEN(1010,FILE='w2_selective.npt',STATUS='old')
      DO J=1,3
      READ(1010,*)
      END DO
      READ(1010,'(8X,F8.0)')TFRQTMP
      NXTSTR=TMSTRT
      DO J=1,2
      READ(1010,*)
      END DO
      READ(1010,'(8X,A8,I8,F8.0)')TEMPC,NUMTEMPC,TCDFREQ
      NXTTCD=TMSTRT
      NXTSPLIT=TMSTRT
      NSTT=NUMTEMPC
      
  ALLOCATE (TCNELEV(NSTT),TCJB(NSTT),TCJS(NSTT), TCELEV(NSTT,11),TCTEMP(NSTT),TCTEND(NSTT),TCTSRT(NSTT),NCOUNTC(NST,NBR),TCISEG(NSTT),TCKLAY(NSTT),TCELEVCON(NSTT)) 
  ALLOCATE (TCYEARLY(NSTT), JBMON(NSTT),JSMON(NSTT),TCNTR(NSTT)) 
  ALLOCATE (VOLM(NWB),MONCTR(NSTT),NCOUNTCW(NWD),QWDFRAC(NWD),QSTRFRAC(NST,NBR),DYNSEL(NSTT),SELD(NSTT),NXSEL(NSTT),TEMP2(NSTT))       
      
      DO J=1,2
      READ(1010,*)
      END DO
      NCOUNTC=0
      DO J=1,NUMTEMPC
      READ(1010,'(8X,A8,I8,I8,A8,F8.0,F8.0,F8.0,I8,10(F8.0))')TCNTR(J),TCJB(J),TCJS(J),TCYEARLY(J),TCTSRT(J),TCTEND(J),TCTEMP(J),TCNELEV(J),(TCELEV(J,N),N=1,TCNELEV(J))
        IF(TCNTR(J)=='      ST')THEN      
        TCELEV(J,TCNELEV(J)+1)=ESTR(TCJS(J),TCJB(J))   ! ALWAYS PUT THE ORIGINAL ELEVATION AS THE LAST ELEVATION
        ELSE
        TCELEV(J,TCNELEV(J)+1)=EWD(TCJS(J))   ! ALWAYS PUT THE ORIGINAL ELEVATION AS THE LAST ELEVATION
        ENDIF
      END DO
      DO J=1,2
      READ(1010,*)
      END DO
      DO J=1,NUMTEMPC
      READ(1010,'(8X,I8,F8.0,A8)')TCISEG(J),TCKLAY(J),DYNSEL(J)      
      END DO
      DO J=1,2
      READ(1010,*)
      END DO
      DO J=1,NUMTEMPC
      READ(1010,'(8X,A8)')TCELEVCON(J) 
      END DO
      DO J=1,2
      READ(1010,*)
      END DO
      READ(1010,'(8X,A8,I8)')TSPLTC,NUMTSPLT
      
      ALLOCATE(TSYEARLY(NUMTSPLT),TSTSRT(NUMTSPLT),TSTEND(NUMTSPLT),TSPLTJB(NUMTSPLT),TSPLTT(NUMTSPLT),NOUTS(NUMTSPLT),JSTSPLT(NUMTSPLT,10),KSTRSPLT(NUMTSPLT),TSPLTCNTR(NUMTSPLT))
      ALLOCATE(JSTSPLTT(NUMTSPLT,10),ELCONTSPL(NUMTSPLT))
      
      DO J=1,2
      READ(1010,*)
      END DO
      DO J=1,NUMTSPLT
      !READ(1010,'(8X,A8,I8,A8,F8.0,F8.0,F8.0,I8,10I8)')TSPLTCNTR(J),TSPLTJB(J),TSYEARLY(J),TSTSRT(J),TSTEND(J),TSPLTT(J),NOUTS(J),(JSTSPLTT(J,N),N=1,NOUTS(J))
      READ(1010,'(8X,A8,I8,A8,F8.0,F8.0,F8.0,I8,2I8,A8)')TSPLTCNTR(J),TSPLTJB(J),TSYEARLY(J),TSTSRT(J),TSTEND(J),TSPLTT(J),NOUTS(J),(JSTSPLTT(J,N),N=1,2),ELCONTSPL(J)
      NOUTS(J)=2                ! NUMBER OF OUTLETS FOR EACH SPLIT FLOW PERIOD LIMITED TO 2
      !IF(NOUTS(J).GT.2)WRITE(*,*)'TCD NOUTS > 2 - ONLY FIRST 2 WILL BE USED'
      ENDDO
      JSTSPLT=JSTSPLTT                                                                                             ! CB 10/14/11 START
      DO J=1,NUMTSPLT  !REODERING OUTLETS SO THAT HIGHEST ELEVATION STRUCTURE ON TOP (ASSUMING 2 SPLIT OUTLETS) 
!        IF(TCNTR(J) == '      ST')THEN
        IF(TSPLTCNTR(J) == '      ST')THEN                                                                        ! cb 11/11/12
          IF(ESTR(JSTSPLTT(J,1),TSPLTJB(J)) < ESTR(JSTSPLTT(J,2),TSPLTJB(J)))THEN                               
            JSTSPLT(J,1)=JSTSPLTT(J,2)                                                                          
            JSTSPLT(J,2)=JSTSPLTT(J,1)                                                                          
          END IF                                                                                                
!        ELSE IF(TCNTR(J) == '      WD')THEN
        ELSE IF(TSPLTCNTR(J) == '      WD')THEN                                                                        ! cb 11/11/12
          IF(EWD(JSTSPLTT(J,1)) < EWD(JSTSPLTT(J,2)))THEN                                    
            JSTSPLT(J,1)=JSTSPLTT(J,2)                                                                          
            JSTSPLT(J,2)=JSTSPLTT(J,1)                                                                          
          END IF                                                                                                
        END IF
      END DO                                                                                                       ! CB 10/14/11 END
      DO J=1,2
      READ(1010,*)
      END DO
      READ(1010,'(8X,I8)')TEMPN
      DO J=1,2
      READ(1010,*)
      END DO
      ALLOCATE(TEMPCRIT(NWB,TEMPN),VOLMC(NWB,TEMPN))
      DO J=1,TEMPN
        READ(1010,'(8X,10F8.0)')(TEMPCRIT(JW,J),JW=1,NWB)   ! NOTE MAX OF 10 WATERBODIES
      END DO
      CLOSE(1010)

      
      DO JW=1,NWB
        IFILE=IFILE+1
        WRITE (SEGNUM,'(I0)') JW
        SEGNUM = ADJUSTL(SEGNUM)
        L      = LEN_TRIM(SEGNUM)
         IF(RESTART_IN)THEN
                OPEN  (IFILE,FILE='VOLUME_WB'//SEGNUM(1:L)//'.OPT',POSITION='APPEND')
                JDAY1=0.0
                REWIND (IFILE)
                READ   (IFILE,'(/)',END=15)
                DO WHILE (JDAY1 < JDAY)
                READ (IFILE,'(F10.0)',END=15) JDAY1
                END DO
                BACKSPACE (IFILE)
                15    JDAY1=0.0   
       ELSE
        OPEN  (IFILE,FILE='VOLUME_WB'//SEGNUM(1:L)//'.OPT',STATUS='UNKNOWN')
        WRITE(IFILE,4315)
       ENDIF
      ENDDO
      
4315  FORMAT("JDAY    VOLUME    ",<TEMPN>("VOLCRIT      "))


! INITIALIZING STRUCTURE ELEVATION IF STRUCTURE
IF(TEMPC=='      ON')THEN     
  DO JW=1,NWB
   DO JB=BS(JW),BE(JW)
    DO JS=1,NST
     DO J=1,NUMTEMPC        
       IF(TCJB(J) == JB .AND. TCJS(J) == JS .AND. TCNTR(J) == '      ST')THEN
           IF(TCYEARLY(J) == '     OFF')THEN
             DAYTEST=JDAY
           ELSE
             DAYTEST=REAL(JDAYG)+JDAY-INT(JDAY)
           END IF
           IF(DAYTEST >= TCTSRT(J) .AND. DAYTEST < TCTEND(J))THEN               
               ! MAKING SURE THAT STRUCTURE IS BELOW WATER SURFACE
             DO NN=1,TCNELEV(J)
               IF(TCELEV(J,NN) < ELWS(DS(JB)))THEN
                 NCOUNTC(JS,JB)=NN
                 ESTR(JS,JB)=TCELEV(J,NCOUNTC(JS,JB))
                 EXIT
               END IF                 
             END DO
		   END IF
	   END IF
	 END DO
	END DO
   END DO
  END DO
   
   
   ! INITIALIZING STRUCTURE ELEVATION IF WITHDRAWAL

  DO JWD=1,NWD
   
     DO J=1,NUMTEMPC        
       IF(TCJS(J) == JWD .AND. TCNTR(J) == '      WD')THEN
           IF(TCYEARLY(J) == '     OFF')THEN
             DAYTEST=JDAY
           ELSE
             DAYTEST=REAL(JDAYG)+JDAY-INT(JDAY)
           END IF
           IF(DAYTEST >= TCTSRT(J) .AND. DAYTEST < TCTEND(J))THEN               
               ! MAKING SURE THAT STRUCTURE IS BELOW WATER SURFACE
             DO NN=1,TCNELEV(J)
               IF(TCELEV(J,NN) < ELWS(IWD(JWD)))THEN
                 NCOUNTCW(JWD)=NN
                 EWD(JWD)=TCELEV(J,NCOUNTCW(JWD))
                 EXIT
               END IF                 
             END DO
		   END IF
	   END IF
	 END DO

  END DO
END IF
  
  ! OPEN DYNAMIC SELECTIVE WITHDRAWAL FILES
  
  DO J=1,numtempc
     if(DYNSEL(J) == '      ON')then
     WRITE (SEGNUM,'(I0)') J     
     SEGNUM = ADJUSTL(SEGNUM)
     L      = LEN_TRIM(SEGNUM)   
     SELD(J) = 1009+J  
     OPEN (SELD(J),FILE='dynselective'//SEGNUM(1:L)//'.npt',STATUS='OLD') 
      READ (SELD(J),'(///1000F8.0)') NXSEL(J),TEMP2(J)
        tctemp(J)=TEMP2(J)
      READ (SELD(J),'(1000F8.0)') NXSEL(J),TEMP2(J)
     END IF
    ENDDO 
  
 RETURN

END SUBROUTINE SELECTIVEINIT

SUBROUTINE SELECTIVE
 USE SELECTIVE1
  USE MAIN
  USE GLOBAL;     USE NAMESC; USE GEOMC;  USE LOGICC; USE PREC;  USE SURFHE;  USE KINETIC; USE SHADEC; USE EDDY
  USE STRUCTURES; USE TRANS;  USE TVDC;   USE SELWC;  USE GDAYC; USE SCREENC; USE TDGAS;   USE RSTART
  
  IMPLICIT NONE
  !** Timestep violation entry point  210 CONTINUE                
  INTEGER JJ, JJW, KK, KS, IFILE, KSTR
  REAL DAYTEST, ELR, QALL, TCOMP, TEMPBOT, TEMPEST, TEMPTOP, TMOD, WSEL

  IF(TSPLTC=='      ON')THEN    
    DO J=1,NUMTSPLT
      IF(TSYEARLY(J) == '     OFF')THEN
        DAYTEST=JDAY
      ELSE
        DAYTEST=REAL(JDAYG)+JDAY-INT(JDAY)
      END IF
      IF(NXTSPLIT > TSTSRT(J) .AND. DAYTEST <= TSTSRT(J))THEN
        NXTSPLIT=TSTSRT(J)
      END IF
    END DO
  END IF

 IF(TSPLTC=='      ON'.AND.JDAY.GE.NXTSPLIT)THEN  
 
  DO J=1,NUMTSPLT
        IF(TSYEARLY(J) == '     OFF')THEN
            DAYTEST=JDAY
          ELSE
        DAYTEST=REAL(JDAYG)+JDAY-INT(JDAY)
        END IF
   IF(DAYTEST >= TSTSRT(J) .AND. DAYTEST < TSTEND(J))THEN 
    ! DO STRUCTURES FIRST
    DO JW=1,NWB
        DO JB=BS(JW),BE(JW)
            IF(TSPLTJB(J) == JB .AND. TSPLTCNTR(J) == '      ST')THEN
                QALL=0.0
                DO JJ=1,NOUTS(J)
                QALL=QALL+QSTR(JSTSPLT(J,JJ),TSPLTJB(J))   ! SUM UP ALL THE FLOWS
                ELR  = SINA(JB)*DLX(DS(JB))*0.5
                    DO K=KTWB(JW),KB(DS(JB))
                    IF (EL(K,DS(JB))-ELR < ESTR(JSTSPLT(J,JJ),TSPLTJB(J))) EXIT                                                                               !SW 10/17/01
                    END DO
                KSTR = K-1
                KSTRSPLT(JJ) = MIN(KSTR,KB(DS(JB)))
                ENDDO               
              DO JJ=1,NOUTS(J)               ! cb 11/11/12 dividing total flow between outlets for temperature test - if no flow there is no temperature test
                  QSTR(JSTSPLT(J,JJ),TSPLTJB(J)) = qall/real(nouts(j))
              ENDDO               
              ID=DS(JB)
              ELR  = SINA(JB)*DLX(ID)*0.5          ! CB 10/14/11
              WSEL = ELWS(ID)-ELR                  ! CB 10/14/11
              CALL DOWNSTREAM_WITHDRAWAL_ESTIMATE(JSTSPLT(J,1),TEMPTOP,ESTR(JSTSPLT(J,1),TSPLTJB(J)))
              CALL DOWNSTREAM_WITHDRAWAL_ESTIMATE(JSTSPLT(J,2),TEMPBOT,ESTR(JSTSPLT(J,2),TSPLTJB(J)))
             IF(ESTR(JSTSPLT(J,1),TSPLTJB(J)) > WSEL .AND. ELCONTSPL(J) =='     OFF') THEN   ! NO FLOWS THROUG THIS OUTLET IF WSEL BELOW LEVEL OF OUTLET  ! CB 10/14/11
               QSTR(JSTSPLT(J,1),TSPLTJB(J))=0.0
               QSTRFRAC(JSTSPLT(J,1),TSPLTJB(J))=0.0
              
            ELSE IF(TEMPTOP > TSPLTT(J)  .AND.  TEMPBOT > TSPLTT(J) ) THEN   ! NO FLOWS THROUG THIS OUTLET IF T1 AND T2 > TCRITERIA
               QSTR(JSTSPLT(J,1),TSPLTJB(J))=0.0
               QSTRFRAC(JSTSPLT(J,1),TSPLTJB(J))=0.0

              !ELSEIF(T2(KSTRSPLT(1),DS(JB)) < TSPLTT(J)) THEN   ! ALL FLOWS FROM TOP IF TCRITERIA < TOUTLET
              ELSEIF(TEMPTOP < TSPLTT(J)) THEN   ! ALL FLOWS FROM TOP IF TCRITERIA < TOUTLET
               QSTR(JSTSPLT(J,1),TSPLTJB(J))=QALL
               QSTRFRAC(JSTSPLT(J,1),TSPLTJB(J))=1.0

              ELSE
                !QSTR(JSTSPLT(J,1),TSPLTJB(J))=QALL*(TSPLTT(J)-T2(KSTRSPLT(2),DS(JB)))/(T2(KSTRSPLT(1),DS(JB))-T2(KSTRSPLT(2),DS(JB)))
                IF(ABS(TEMPTOP-TEMPBOT) < 0.0001)THEN
                  QSTR(JSTSPLT(J,1),TSPLTJB(J))=QALL
                  QSTRFRAC(JSTSPLT(J,1),TSPLTJB(J))=1.0
                ELSE
                  QSTR(JSTSPLT(J,1),TSPLTJB(J))=QALL*(TSPLTT(J)-TEMPBOT)/(TEMPTOP-TEMPBOT)
                  QSTRFRAC(JSTSPLT(J,1),TSPLTJB(J))=QSTR(JSTSPLT(J,1),TSPLTJB(J))/QALL
                END IF
              ENDIF

              QSTR(JSTSPLT(J,2),TSPLTJB(J))=QALL-QSTR(JSTSPLT(J,1),TSPLTJB(J))
              QSTRFRAC(JSTSPLT(J,2),TSPLTJB(J))=QSTR(JSTSPLT(J,2),TSPLTJB(J))/QALL
             EXIT
             END IF
         END DO
       END DO
    ! DO WITHDRAWALS NEXT
      DO JWD=1,NWD
            IF(TSPLTCNTR(J) == '      WD')THEN
                QALL=0.0
               DO JJB=1,NBR
                 IF (IWD(JWD) >= US(JJB) .AND. IWD(JWD) <= DS(JJB)) EXIT
               END DO
               DO JJW=1,NWB
                 IF (JJB >= BS(JJW) .AND. JJB <= BE(JJW)) EXIT
               END DO
               DO JJ=1,NOUTS(J)
                QALL=QALL+QWD(JSTSPLT(J,JJ))   ! SUM UP ALL THE FLOWS
                ELR  = SINA(JJB)*DLX(IWD(JWD))*0.5
                    DO K=KTWB(JJW),KB(IWD(JWD))
                    IF (EL(K,IWD(JWD))-ELR < EWD(JSTSPLT(J,JJ))) EXIT                                                                               !SW 10/17/01
                    END DO
                KSTR = K-1
                KSTRSPLT(JJ) = MIN(KSTR,KB(IWD(JWD)))
               ENDDO
               JJ=1               ! ASSIGN FLOW TO FIRST OUTLET               
               WSEL = ELWS(IWD(JWD))-ELR                  ! CB 10/14/11
               I=IWD(JWD)
               CALL LATERAL_WITHDRAWAL_ESTIMATE(JSTSPLT(J,1),TEMPTOP,EWD(JSTSPLT(J,1)))
               CALL LATERAL_WITHDRAWAL_ESTIMATE(JSTSPLT(J,2),TEMPBOT,EWD(JSTSPLT(J,2)))              
              IF(EWD(JSTSPLT(J,1)) > WSEL .AND. TCELEVCON(J) =='     OFF') THEN
                QWD(JSTSPLT(J,1))=0.0
                QWDFRAC(JSTSPLT(J,1))=0.0             
             ELSE IF(TEMPTOP > TSPLTT(J)  .AND.  TEMPBOT > TSPLTT(J) ) THEN   ! NO FLOWS THROUG THIS OUTLET IF T1 AND T2 > TCRITERIA
               QWD(JSTSPLT(J,1))=0.0
               QWDFRAC(JSTSPLT(J,1))=0.0

              ELSEIF(TEMPTOP < TSPLTT(J)) THEN   ! ALL FLOWS FROM TOP IF TCRITERIA < TOUTLET
               QWD(JSTSPLT(J,1))=QALL
               QWDFRAC(JSTSPLT(J,1))=1.0

              ELSE
                !QWD(JSTSPLT(J,1))=QALL*(TSPLTT(J)-T2(KSTRSPLT(2),IWD(JWD)))/(T2(KSTRSPLT(1),IWD(JWD))-T2(KSTRSPLT(2),IWD(JWD)))
                IF(ABS(TEMPTOP-TEMPBOT) < 0.0001)THEN
                  QWD(JSTSPLT(J,1))=QALL
                  QWDFRAC(JSTSPLT(J,1))=1.0
                ELSE
                  QWD(JSTSPLT(J,1))=QALL*(TSPLTT(J)-TEMPBOT)/(TEMPTOP-TEMPBOT)
                  QWDFRAC(JSTSPLT(J,1))=QWD(JSTSPLT(J,1))/QALL
                END IF
              ENDIF

              QWD(JSTSPLT(J,2))=QALL-QWD(JSTSPLT(J,1))
              QWDFRAC(JSTSPLT(J,2))=QWD(JSTSPLT(J,2))/QALL
             EXIT
             END IF
       END DO
     ENDIF
     ENDDO
  
   NXTSPLIT=NXTSPLIT+TCDFREQ
  END IF
  IF(TSPLTC=='      ON')THEN

    DO J=1,NUMTSPLT
    IF(TSYEARLY(J) == '     OFF')THEN
            DAYTEST=JDAY
          ELSE
        DAYTEST=REAL(JDAYG)+JDAY-INT(JDAY)
        END IF
    IF(DAYTEST >= TSTSRT(J) .AND. DAYTEST < TSTEND(J))THEN 
    ! DO STRUCTURES FIRST
      DO JW=1,NWB
        DO JB=BS(JW),BE(JW)
            IF(TSPLTJB(J) == JB .AND. TSPLTCNTR(J) == '      ST')THEN
                QALL=0.0
                DO JJ=1,NOUTS(J)
                QALL=QALL+QSTR(JSTSPLT(J,JJ),TSPLTJB(J))   ! SUM UP ALL THE FLOWS
                ELR  = SINA(JB)*DLX(DS(JB))*0.5
                    DO K=KTWB(JW),KB(DS(JB))
                    IF (EL(K,DS(JB))-ELR < ESTR(JSTSPLT(J,JJ),TSPLTJB(J))) EXIT                                                                               !SW 10/17/01
                    END DO
                KSTR = K-1
                KSTRSPLT(JJ) = MIN(KSTR,KB(DS(JB)))
                ENDDO               
              QSTR(JSTSPLT(J,1),TSPLTJB(J))=QSTRFRAC(JSTSPLT(J,1),TSPLTJB(J))*QALL
              QSTR(JSTSPLT(J,2),TSPLTJB(J))=QSTRFRAC(JSTSPLT(J,2),TSPLTJB(J))*QALL
             EXIT
             END IF
        END DO
      END DO
    ! DO WITHDRAWALS NEXT
      DO JWD=1,NWD
            IF(TSPLTCNTR(J) == '      WD')THEN
                QALL=0.0
                DO JJB=1,NBR
                  IF (IWD(JWD) >= US(JJB) .AND. IWD(JWD) <= DS(JJB)) EXIT
                END DO
                DO JJW=1,NWB
                  IF (JJB >= BS(JJW) .AND. JJB <= BE(JJW)) EXIT
                END DO
               DO JJ=1,NOUTS(J)
                QALL=QALL+QWD(JSTSPLT(J,JJ))   ! SUM UP ALL THE FLOWS
                ELR  = SINA(JJB)*DLX(IWD(JWD))*0.5
                    DO K=KTWB(JJW),KB(IWD(JWD))
                    IF (EL(K,IWD(JWD))-ELR < EWD(JSTSPLT(J,JJ))) EXIT                                                                               !SW 10/17/01
                    END DO
                KSTR = K-1
                KSTRSPLT(JJ) = MIN(KSTR,KB(IWD(JWD)))
               ENDDO               
               QWD(JSTSPLT(J,1))=  QWDFRAC(JSTSPLT(J,1))*QALL
               QWD(JSTSPLT(J,2))=  QWDFRAC(JSTSPLT(J,2))*QALL
             EXIT
             END IF
      END DO
      ENDIF
    ENDDO
   
  ENDIF   
 

      IF (JDAY.GE.NXTSTR) THEN
        NXTSTR = NXTSTR+TFRQTMP   
        IFILE=1949
        DO JB=1,NBR
            IF(NSTR(JB) > 0)THEN
            IFILE=IFILE+1
            WRITE (IFILE,'(F10.4,<NSTR(JB)>F10.2,<NSTR(JB)>F10.2,<NSTR(JB)>F10.2)') JDAY,(TAVG(I,JB),I=1,NSTR(JB)),(QSTR(I,JB),I=1,NSTR(JB)),(ESTR(I,JB),I=1,NSTR(JB))
            END IF
         ENDDO          
          IF(NWD > 0)THEN
            IFILE=IFILE+1
            WRITE (IFILE,'(F10.4,<NWD>F10.2,<NWD>F10.2,<NWD>F10.2)') JDAY,(TAVGW(I),I=1,NWD),(QWD(I),I=1,NWD),(EWD(I),I=1,NWD)
          END IF
         ! TEMPERATURE CONTROL LOGIC 

         ! COMPUTING RESERVOIR VOLUME AND VOLUME BELOW 'TEMPCRIT'        
        VOLMC=0.0
        VOLM=0.0
        DO JW=1,NWB
         KT = KTWB(JW)
           DO JB=BS(JW),BE(JW)           
             DO I=CUS(JB),DS(JB)
               VOLM(JW) = VOLM(JW) +BH2(KT,I)*DLX(I)               
               DO K=KT+1,KB(I)
                 VOLM(JW) = VOLM(JW)+BH(K,I)*DLX(I)               
               END DO
               DO KK=1,TEMPN                                         
                 IF(T2(KT,I).LE.TEMPCRIT(JW,KK))VOLMC(JW,KK) = VOLMC(JW,KK)+BH2(KT,I)*DLX(I)                                                 
                 DO K=KT+1,KB(I)                 
                   IF(T2(K,I).LE.TEMPCRIT(JW,KK))VOLMC(JW,KK) = VOLMC(JW,KK)+BH(K,I)*DLX(I)
                 END DO
               END DO               
             END DO         
           END DO
     
         IFILE=IFILE+1
         WRITE(IFILE,5315)JDAY,VOLM(JW),(VOLMC(JW,KK), KK=1,TEMPN)
5315     FORMAT(F8.2,100(G12.4,G12.4))
       ENDDO
         
      ENDIF



IF(TEMPC=='      ON'.AND.JDAY.GE.NXTTCD)THEN  

! IF DYNAMIC SELECTIVE CHANGE TEMPERATURE 

 DO J=1,NUMTEMPC
  IF(DYNSEL(J) == '      ON')THEN
     DO WHILE (JDAY >= NXSEL(J))
        TCTEMP(J)=TEMP2(J)
      READ (SELD(J),'(1000F8.0)') NXSEL(J),TEMP2(J)
    END DO
   ENDIF
  ENDDO

  
! STRUCTURES  
  
  DO JW=1,NWB
   DO JB=BS(JW),BE(JW)
    DO JS=1,NST
     DO J=1,NUMTEMPC      

          
      IF(TCJB(J) == JB .AND. TCJS(J) == JS .AND.  TCNTR(J) == '      ST')THEN
          IF(TCISEG(J).EQ.0)THEN
            TCOMP=TAVG(TCJS(J),TCJB(J))   !CB 9/8/06   TAVG(JSMON(J),JBMON(J))
          ELSEIF(TCISEG(J) < 0)THEN
            TCOMP=TWDO(ABS(TCISEG(J)))      ! SW 11/26/10       
          ELSE

! CHECKING TO SEE IF THE MONITORING SEGMENT TCISEG IS IN THE SAME BRANCH AND WATER BODY AS THE STRUCTURE
            DO JJB=1,NBR
              IF (TCISEG(J) >= US(JJB) .AND. TCISEG(J) <= DS(JJB)) EXIT
            END DO
            DO JJW=1,NWB
              IF (JJB >= BS(JJW) .AND. JJB <= BE(JJW)) EXIT
            END DO

            IF (TCKLAY(J)< 0) THEN                                                                                       
              K = INT(ABS(TCKLAY(J)))
            ELSE
              DO K=KTWB(JJW),KB(TCISEG(J))
                IF (DEPTHB(K,TCISEG(J)) > TCKLAY(J)) EXIT                                                                      
              END DO
              K = MIN(K,KB(TCISEG(J)))                                                                                         
            END IF
            TCOMP=T2(K,TCISEG(J))
          ENDIF
          IF(TCYEARLY(J) == '     OFF')THEN
            DAYTEST=JDAY
          ELSE
            DAYTEST=REAL(JDAYG)+JDAY-INT(JDAY)
          END IF          
          IF(DAYTEST >= TCTSRT(J) .AND. DAYTEST < TCTEND(J))THEN
               IF(TCOMP > TCTEMP(J) .AND. TCNELEV(J) > NCOUNTC(JS,JB))THEN
               ! MAKING SURE THAT THE NEXT LOWER STRUCTURE FOR A PARTICULAR 'J' IS FOUND
                 DO NN=NCOUNTC(JS,JB)+1,TCNELEV(J)                 
                   IF(TCELEV(J,NN) < ESTR(JS,JB))THEN
                      NCOUNTC(JS,JB)=NN
                      ESTR(JS,JB)=TCELEV(J,NCOUNTC(JS,JB))
                      EXIT
                   END IF                 
                 END DO                                               
               ELSEIF(TCOMP < TCTEMP(J) .AND.  NCOUNTC(JS,JB).GT. 1)THEN
                 ! TO PREVENT THIS HAPPENING AT EACH TIME IT CHECKS IT AND HENCE OSCIALLTING BACK AND FORTH - CHECK THE TEMP AT THE UPPER OUTLET ALSO
                 IF(TCISEG(J) > 0)THEN  
                    IF(JB.EQ.JJB)THEN
                      DO KS=KTWB(JW),KB(DS(JB))
                        IF (DEPTHB(KS,TCISEG(J)) > TCELEV(J,NCOUNTC(JS,JB)-1)) EXIT                                                                          !TC 01/03/02
                      END DO
                      KS = MIN(KS,KB(TCISEG(J)))
                      TMOD= T2(KS,DS(JB))
                    ELSE
                      TMOD=T2(K,TCISEG(J))                      
                    END IF
                    IF(TMOD < TCTEMP(J) .AND. TCELEV(J,NCOUNTC(JS,JB)-1) < ELWS(DS(JB)))THEN                      
                      ! MAKING SURE THAT THE NEXT UPPER STRUCTURE FOR A PARTICULAR 'J' IS FOUND
                      DO NN=NCOUNTC(JS,JB)-1,1,-1
                        IF(TCELEV(J,NN) > ESTR(JS,JB))THEN
                          NCOUNTC(JS,JB)=NN
                          ESTR(JS,JB)=TCELEV(J,NCOUNTC(JS,JB))
                          EXIT
                        END IF                 
                      END DO
                    ENDIF
                 END IF  ! CB 9/8/06
                 IF(TCISEG(J).EQ.0)THEN
! CALCULATE THE ESTIMATED OUTFLOW TEMPERATURE AT HIGHER PORTS WHEN TCOMP<TCTEMP(J), AND MOVE UP IF HIGHER PORT STILL MEETS TO CRITERIA - THIS DOESN'T HAPPEN WHEN TCISEG < 0
                   DO NN=1,NCOUNTC(JS,JB)-1
                     ID=DS(JB)
                     KT=KTWB(JW)                     
                     CALL DOWNSTREAM_WITHDRAWAL_ESTIMATE(JS,TEMPEST,TCELEV(J,NN))
                     IF(TEMPEST < TCTEMP(J) .AND. TCELEV(J,NN) < ELWS(DS(JB)))THEN
                       NCOUNTC(JS,JB)=NN
                       ESTR(JS,JB)=TCELEV(J,NCOUNTC(JS,JB))
                       EXIT
                     END IF
                   END DO
                 END IF
               ENDIF
               IF(TCELEVCON(J) =='      ON' .AND. TCNELEV(J) > NCOUNTC(JS,JB).AND. ESTR(JS,JB) > ELWS(DS(JB)))THEN  
                 NCOUNTC(JS,JB)=NCOUNTC(JS,JB)+1
                 ESTR(JS,JB)=TCELEV(J,NCOUNTC(JS,JB))
               END IF
          ENDIF          
      ENDIF            
     END DO
    END DO
   END DO
  ENDDO
  
 ! Withdrawals 
  

  DO JWD=1,NWD
     DO J=1,NUMTEMPC
      IF(TCJS(J) == JWD .AND.  TCNTR(J) == '      WD')THEN
          IF(TCISEG(J).EQ.0)THEN
!           TCOMP=TOUT(JB)
            TCOMP=TAVGW(TCJS(J))   !CB 9/8/06   TAVGW(JSMON(J))
          ELSEIF(TCISEG(J) < 0)THEN  
            TCOMP=TWDO(ABS(TCISEG(J)))
          ELSE

! CHECKING TO SEE IF THE MONITORING SEGMENT TCISEG IS IN THE SAME BRANCH AND WATER BODY AS THE WITHDRAWAL
            DO JJB=1,NBR
              IF (TCISEG(J) >= US(JJB) .AND. TCISEG(J) <= DS(JJB)) EXIT
            END DO
            DO JJW=1,NWB
              IF (JJB >= BS(JJW) .AND. JJB <= BE(JJW)) EXIT
            END DO

            IF (TCKLAY(J)< 0) THEN                                                                                       
              K = INT(ABS(TCKLAY(J)))
            ELSE
              DO K=KTWB(JJW),KB(TCISEG(J))
                IF (DEPTHB(K,TCISEG(J)) > TCKLAY(J)) EXIT                                                                      
              END DO
              K = MIN(K,KB(TCISEG(J)))                                                                                         
            END IF
            TCOMP=T2(K,TCISEG(J))
          ENDIF
          IF(TCYEARLY(J) == '     OFF')THEN
            DAYTEST=JDAY
          ELSE
            DAYTEST=REAL(JDAYG)+JDAY-INT(JDAY)
          END IF
          IF(DAYTEST >= TCTSRT(J) .AND. DAYTEST < TCTEND(J))THEN
               IF(TCOMP > TCTEMP(J) .AND. TCNELEV(J) > NCOUNTCW(JWD))THEN
               ! MAKING SURE THAT THE NEXT LOWER STRUCTURE FOR A PARTICULAR 'J' IS FOUND
                 DO NN=NCOUNTCW(JWD)+1,TCNELEV(J)                 
                   IF(TCELEV(J,NN) < EWD(JWD))THEN
                      NCOUNTCW(JWD)=NN
                      EWD(JWD)=TCELEV(J,NCOUNTCW(JWD))
                      EXIT
                   END IF                 
                 END DO                                               
               ELSEIF(TCOMP < TCTEMP(J) .AND.  NCOUNTCW(JWD).GT. 1)THEN
                 ! TO PREVENT THIS HAPPENING AT EACH TIME IT CHECKS IT AND HENCE OSCIALLTING BACK AND FORTH - CHECK THE TEMP AT THE UPPER OUTLET ALSO
                 IF(TCISEG(J) >  0)THEN  
                      TMOD=T2(K,TCISEG(J))                      
                    IF(TMOD < TCTEMP(J) .AND. TCELEV(J,NCOUNTCW(JWD)-1) < ELWS(IWD(JWD)))THEN                      
                      ! MAKING SURE THAT THE NEXT UPPER STRUCTURE FOR A PARTICULAR 'J' IS FOUND
                      DO NN=NCOUNTCW(JWD)-1,1,-1
                        IF(TCELEV(J,NN) > EWD(JWD))THEN
                          NCOUNTCW(JWD)=NN
                          EWD(JWD)=TCELEV(J,NCOUNTCW(JWD))
                          EXIT
                        END IF                 
                      END DO
                    ENDIF
                 END IF  ! CB 9/8/06
                 IF(TCISEG(J) == 0)THEN
! CALCULATE THE ESTIMATED OUTFLOW TEMPERATURE AT HIGHER PORTS WHEN TCOMP<TCTEMP(J), AND MOVE UP IF HIGHER PORT STILL MEETS TO CRITERIA
                   I         = MAX(CUS(JBWD(JWD)),IWD(JWD))
                   DO NN=1,NCOUNTCW(JWD)-1                     
                     CALL LATERAL_WITHDRAWAL_ESTIMATE(JWD,TEMPEST,TCELEV(J,NN))
                     IF(TEMPEST < TCTEMP(J) .AND. TCELEV(J,NN) < ELWS(IWD(JWD)))THEN
                       NCOUNTCW(JWD)=NN
                       EWD(JWD)=TCELEV(J,NCOUNTCW(JWD))
                       EXIT
                     END IF
                   END DO
                 END IF
               ENDIF
               IF(TCELEVCON(J) =='      ON' .AND. TCNELEV(J) > NCOUNTCW(JWD).AND. EWD(JWD) > ELWS(IWD(JWD)))THEN  
                 NCOUNTCW(JWD)=NCOUNTCW(JWD)+1
                 EWD(JWD)=TCELEV(J,NCOUNTCW(JWD))
               END IF
          ENDIF          
      ENDIF            
     END DO
  ENDDO
  
  NXTTCD = NXTTCD+TCDFREQ    
ENDIF  
  
RETURN
ENTRY DEALLOCATE_SELECTIVE
  DEALLOCATE (TCNELEV,TCJB,TCJS, TCELEV,TCTEMP,TCTEND,TCTSRT,NCOUNTC,TCISEG,TCKLAY,TCELEVCON,ELCONTSPL) 
  DEALLOCATE (TSPLTJB,TSPLTT,NOUTS,JSTSPLT,KSTRSPLT,TCYEARLY, JBMON,JSMON,TCNTR,TSPLTCNTR,JSTSPLTT) 
  DEALLOCATE (VOLM,MONCTR,NCOUNTCW,QWDFRAC,QSTRFRAC)    
  DEALLOCATE(TEMPCRIT,VOLMC,DYNSEL,SELD,NXSEL,TEMP2,TSYEARLY,TSTEND,TSTSRT)
RETURN   

END SUBROUTINE SELECTIVE

		  
		  
		  