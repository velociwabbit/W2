SUBROUTINE WQCONSTITUENTS

USE MAIN
USE GLOBAL;     USE NAMESC; USE GEOMC;  USE LOGICC; USE PREC;  USE SURFHE;  USE KINETIC; USE SHADEC; USE EDDY
  USE STRUCTURES; USE TRANS;  USE TVDC;   USE SELWC;  USE GDAYC; USE SCREENC; USE TDGAS;   USE RSTART
  USE MACROPHYTEC; USE POROSITYC; USE ZOOPLANKTONC;USE TRIDIAG_V
  
  IMPLICIT NONE
  EXTERNAL RESTART_OUTPUT
 ! REAL(R8) :: BTA1(1000),GMA1(1000)   ! places a limit of 1000 vertical layers

      IF(MACROPHYTE_ON.AND.UPDATE_KINETICS)CALL POROSITY 
      DO JW=1,NWB
        KT = KTWB(JW)
        DO JB=BS(JW),BE(JW)
          IU = CUS(JB)
          ID = DS(JB)

!******** Kinetic sources/sinks

          IF (SEDIMENT_CALC(JW))then
            CALL SEDIMENT
            CALL SEDIMENTP
            CALL SEDIMENTN
            CALL SEDIMENTC
            IF(DYNSEDK(JW) == '      ON')CALL SEDIMENT_DECAY_RATE
          END IF
          DO M=1,NMC
            IF (MACROPHYTE_CALC(JW,M))THEN
              CALL MACROPHYTE(M)
            END IF
          END DO

          IF (UPDATE_KINETICS) THEN
            IF (UPDATE_RATES) THEN
              CALL TEMPERATURE_RATES
              CALL KINETIC_RATES
            END IF
            DO JAC=1,NAC
              JC = CN(JAC)
              IF (JC == NPO4)                    CALL PHOSPHORUS
              IF (JC == NNH4)                    CALL AMMONIUM
              IF (JC == NNO3)                    CALL NITRATE
              IF (JC == NDSI)                    CALL DISSOLVED_SILICA
              IF (JC == NPSI)                    CALL PARTICULATE_SILICA
              IF (JC == NFE)                     CALL IRON
              IF (JC == NLDOM)                   CALL LABILE_DOM
              IF (JC == NRDOM)                   CALL REFRACTORY_DOM
              IF (JC == NLPOM)                   CALL LABILE_POM
              IF (JC == NRPOM)                   CALL REFRACTORY_POM
              IF (JC == NDO)                     CALL DISSOLVED_OXYGEN
              IF (JC >= NGCS  .AND. JC <= NGCE)  CALL GENERIC_CONST(JC-NGCS+1)
              IF (JC >= NSSS  .AND. JC <= NSSE)  CALL SUSPENDED_SOLIDS(JC-NSSS+1)
              IF (JC >= NAS   .AND. JC <= NAE)THEN
                IF(ALG_CALC(JC-NAS+1))CALL ALGAE(JC-NAS+1)
              ENDIF
              IF (JC >= NBODS .AND. JC <= NBODE)THEN
                DO JCB=1,NBOD       ! VARIABLE STOICHIOMETRY FOR CBOD, CB 6/6/10
                  IF(BOD_CALC(JCB))THEN
                    IF(JC == NBODC(JCB))CALL BIOCHEMICAL_O2_DEMAND(JCB)
                    IF(JC == NBODP(JCB) .AND. BOD_CALCP(JCB))CALL BIOCHEMICAL_O2_DEMAND_P(JCB)         ! CB 5/19/2011
                    IF(JC == NBODN(JCB) .AND. BOD_CALCN(JCB))CALL BIOCHEMICAL_O2_DEMAND_N(JCB)         ! CB 5/19/2011
                  END IF
                END DO       
              ENDIF
              IF (JC >= NZOOS  .AND. JC <= NZOOE .AND.ZOOPLANKTON_CALC)CALL ZOOPLANKTON  		
              IF (JC == NLDOMP)                CALL LABILE_DOM_P
              IF (JC == NRDOMP)                CALL REFRACTORY_DOM_P
              IF (JC == NLPOMP)                CALL LABILE_POM_P
              IF (JC == NRPOMP)                CALL REFRACTORY_POM_P
              IF (JC == NLDOMN)                CALL LABILE_DOM_N
              IF (JC == NRDOMN)                CALL REFRACTORY_DOM_N
              IF (JC == NLPOMN)                CALL LABILE_POM_N
              IF (JC == NRPOMN)                CALL REFRACTORY_POM_N
            END DO
            IF (PH_CALC(JW)) CALL INORGANIC_CARBON
            IF (PH_CALC(JW)) CALL PH_CO2
          END IF
          DO JE=1,NEP   ! sw 5/16/06
            IF (EPIPHYTON_CALC(JW,JE)) CALL EPIPHYTON(JE)
          END DO

!******** External sources/sinks

            IF(AERATEC == "      ON")CALL AERATEMASS

          DO JAC=1,NAC
            JC = CN(JAC)
            IF (TRIBUTARIES) THEN
              DO JT=1,JTT
                IF (JB == JBTR(JT)) THEN
                  I = ITR(JT)
                  IF (I < CUS(JB)) I = CUS(JB)
                  DO K=KTTR(JT),KBTR(JT)
                    IF (QTR(JT) < 0.0) THEN
                      CSSB(K,I,JC) = CSSB(K,I,JC)+C1(K,I,JC)*QTR(JT)*QTRF(K,JT)
                    ELSE
                      CSSB(K,I,JC) = CSSB(K,I,JC)+CTR(JC,JT)*QTR(JT)*QTRF(K,JT)
                    END IF
                  END DO
                END IF
              END DO
            END IF
            IF (DIST_TRIBS(JB)) THEN
              DO I=IU,ID
                IF (QDT(I) < 0.0) THEN
                  CSSB(KT,I,JC) = CSSB(KT,I,JC)+C1(KT,I,JC)*QDT(I)
                ELSE
                  CSSB(KT,I,JC) = CSSB(KT,I,JC)+CDTR(JC,JB)*QDT(I)
                END IF
              END DO
            END IF
            IF (WITHDRAWALS) THEN
              DO JWD=1,JWW
                IF (QWD(JWD) /= 0.0) THEN
                  IF (JB == JBWD(JWD)) THEN
                    I = MAX(CUS(JBWD(JWD)),IWD(JWD))
                    DO K=KTW(JWD),KBW(JWD)      !CONCURRENT(K=KTW(JWD):KBW(JWD))                 ! FORALL
                      CSSB(K,I,JC) = CSSB(K,I,JC)-C1S(K,I,JC)*QSW(K,JWD)
                    END DO
                  END IF
                END IF
              END DO
            END IF
            IF (PRECIPITATION(JW)) THEN
              DO I=IU,ID    !CONCURRENT (I=IU:ID)                                  !FORALL
                CSSB(KT,I,JC) = CSSB(KT,I,JC)+CPR(JC,JB)*QPR(I)
              END DO
            END IF
            IF (UP_FLOW(JB)) THEN
              DO K=KT,KB(IU)
                IF (.NOT. HEAD_FLOW(JB)) THEN
                  CSSB(K,IU,JC) = CSSB(K,IU,JC)+QINF(K,JB)*QIN(JB)*CIN(JC,JB)
                ELSE
                  IF (U(K,IU-1) >= 0.0) THEN
                    CSSB(K,IU,JC) = CSSB(K,IU,JC)+U(K,IU-1)*BHR1(K,IU-1)*C1S(K,IU-1,JC)
                  ELSE
                    CSSB(K,IU,JC) = CSSB(K,IU,JC)+U(K,IU-1)*BHR1(K,IU-1)*C1S(K,IU,JC)
                  END IF
                END IF
              END DO
            END IF
            IF (DN_FLOW(JB)) CSSB(KT:KB(ID),ID,JC) = CSSB(KT:KB(ID),ID,JC)-QOUT(KT:KB(ID),JB)*C1S(KT:KB(ID),ID,JC)
            IF (UP_HEAD(JB)) THEN
                DO K=KT,KB(IU)
                IUT = IU
                IF (QUH1(K,JB) >= 0.0) IUT = IU-1
                CSSUH1(K,JC,JB) = C1S(K,IUT,JC)*QUH1(K,JB)
                CSSB(K,IU,JC)   = CSSB(K,IU,JC)+CSSUH1(K,JC,JB)
              END DO
              IF (UH_INTERNAL(JB)) THEN
                IF (UHS(JB) /= DS(JBUH(JB)) .OR. DHS(JBUH(JB)) /= US(JB)) THEN
                  IF (JBUH(JB) >= BS(JW) .AND. JBUH(JB) <= BE(JW)) THEN
                    I = UHS(JB)
                    !dir$ ivdep
                    DO K=KT,KB(IU)
                      CSSB(K,I,JC) = CSSB(K,I,JC)-CSSUH2(K,JC,JB)/DLT
                    END DO
                  ELSE
                    CALL UPSTREAM_CONSTITUENT(C2(:,:,JC),CSSB(:,:,JC))
                  END IF
                END IF
              END IF
            END IF
            IF (DN_HEAD(JB)) THEN
              DO K=KT,KB(ID+1)
                IDT = ID+1
                IF (QDH1(K,JB) >= 0.0) IDT = ID
                CSSDH1(K,JC,JB) = C1S(K,IDT,JC)*QDH1(K,JB)
                CSSB(K,ID,JC)   = CSSB(K,ID,JC)-CSSDH1(K,JC,JB)
              END DO
              IF (DH_INTERNAL(JB)) THEN
                IF (DHS(JB) /= US(JBDH(JB)) .OR. UHS(JBDH(JB)) /= DS(JB)) THEN
                  IF (JBDH(JB) >= BS(JW) .AND. JBDH(JB) <= BE(JW)) THEN
                    I = DHS(JB)
                    DO K=KT,KB(ID+1)
                      CSSB(K,I,JC) = CSSB(K,I,JC)+CSSDH2(K,JC,JB)/DLT
                    END DO
                  ELSE
                    CALL DOWNSTREAM_CONSTITUENT(C2(:,:,JC),CSSB(:,:,JC))
                  END IF
                END IF
              END IF
            END IF
          END DO
        END DO
      END DO

!**** Kinetic fluxes

      DO JW=1,NWB
        IF (FLUX(JW)) CALL KINETIC_FLUXES
      END DO

!**** Constituent transport

!!$OMP PARALLEL DO !!PRIVATE(I,JC,KT,JB,JW,DT)    !I,JC,KT,JW,JB,CNEW,SSB,SSK,COLD,AT,VT,CT,DT) 
    DO JAC=1,NAC    !CONCURRENT(JAC=1:NAC)              !JAC=1,NAC
    JC   =  CN(JAC)
      DO JW=1,NWB
        KT = KTWB(JW)
        DO JB=BS(JW),BE(JW)
          IU = CUS(JB)
          ID = DS(JB)
      !    DO JAC=1,NAC
      !      JC   =  CN(JAC)
            COLD => C1S(:,:,JC)
            CALL HORIZONTAL_MULTIPLIERS       
            CALL VERTICAL_MULTIPLIERS           
            
     !       CNEW => C1(:,:,JC)
     !       SSB  => CSSB(:,:,JC)
     !       SSK  => CSSK(:,:,JC)
     !       CALL HORIZONTAL_TRANSPORT
            DO I=IU,ID
              DO K=KT,KB(I)      
              DT(K,I) = (C1S(K,I,JC)*BH2(K,I)/DLT+(ADX(K,I)*BHR1(K,I)-ADX(K,I-1)*BHR1(K,I-1))/DLX(I)+(1.0D0-THETA(JW))                     &
                    *(ADZ(K,I)*BB(K,I)-ADZ(K-1,I)*BB(K-1,I))+CSSB(K,I,JC)/DLX(I))*DLT/BH1(K,I)+CSSK(K,I,JC)*DLT
              END DO                                                    
            END DO
            DO I=IU,ID
        !      CALL TRIDIAG(AT(:,I),VT(:,I),CT(:,I),DT(:,I),KT,KB(I),KMX,CNEW(:,I))
                    BTA1(KT)=VT(KT,I)
                    GMA1(KT)=DT(KT,I)
                  DO K=KT+1,KB(I)
                           BTA1(K) = VT(K,I)-AT(K,I)/BTA1(K-1)*CT(K-1,I)
                    GMA1(K) = DT(K,I)-AT(K,I)/BTA1(K-1)*GMA1(K-1)
                  END DO
                  C1(KB(I),I,JC) = GMA1(KB(I))/BTA1(KB(I))
                  DO K=KB(I)-1,KT,-1
                    C1(K,I,JC) = (GMA1(K)-CT(K,I)*C1(K+1,I,JC))/BTA1(K)
                  END DO
            END DO
          END DO
        END DO
    END DO
!!$OMP END PARALLEL DO
      IF (DERIVED_CALC) CALL DERIVED_CONSTITUENTS

    END SUBROUTINE WQCONSTITUENTS
