!***********************************************************************************************************************************
!**                                                                                                                               **
!**                                                         CE-QUAL-W2                                                            **
!**                                            A Two-dimensional, Laterally Averaged,                                             **
!**                                             Hydrodynamic and Water Quality Model                                              **
!**                                                            for                                                                **
!**                                           Rivers, Lakes, Reservoirs, and Estuaries                                            **
!**                                                                                                                               **
!**                                                        Version 3.7                                                            **
!**                                                                                                                               **
!**                                                  Thomas M. Cole, Retired                                                      **
!**                                                Water Quality Modeling Group                                                   **
!**                                                U.S. Army Corps of Engineers                                                   **
!**                                                Waterways Experiment Station                                                   **
!**                                                Vicksburg, Mississippi 39180                                                   **
!**                                                                                                                               **
!**                                                        Scott A. Wells                                                         **
!**                                       Department of Civil and Environmental Engineering                                       **
!**                                                  Portland State University                                                    **
!**                                                         PO Box 751                                                            **
!**                                                 Portland, Oregon  97207-0751                                                  **
!**                                                 phone number: (503) 725-4276                                                  **
!**                                                 fax   number: (503) 725-5950                                                  **
!**                                                 e-mail: scott@cecs.pdx.edu                                                    **
!**                                                                                                                               **
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!**                                                                                                                               **
!**                  The long arm of the lawyers has found its way into the water quality modeling arena, so:                     **
!**                                                                                                                               **
!**  This model was developed and is maintained by the U.S. Army Engineer Waterways Experiment Station, Vicksburg, MS.  The US    **
!**  government and its components are not responsible for any damages, including incidental or consequential damages, arising    **
!**  from use or misuse of this model, or from results achieved or conclusions drawn by others.  Distribution of this model is    **
!**  restricted by the Export Administration Act of 1969,  50 app. USC subsections 2401-2420, as amended, and other applicable    **
!**  laws or regulations.                                                                                                         **
!**                                                                                                                               **
!***********************************************************************************************************************************

!***********************************************************************************************************************************
!**                                                      Module Declaration                                                       **
!***********************************************************************************************************************************
MODULE MSCLIB
  INTEGER :: HTHREAD
  LOGICAL :: STOP_PUSHED, STOPPED, RESTART_PUSHED, RESTART_EXISTS
  INCLUDE "RESOURCE.FD"
  INTERFACE
    FUNCTION $BEGINTHREADEX (SECURITY,STACK_SIZE,START_ADDRESS,ARGLIST,INITFLAG,THRDADDR)
      USE DFWINTY, RENAMED => DLT
      !DEC$ ATTRIBUTES C,ALIAS : "__BEGINTHREADEX" :: $BEGINTHREADEX
      !DEC$ ATTRIBUTES REFERENCE,ALLOW_NULL        :: SECURITY
      !DEC$ ATTRIBUTES REFERENCE,IGNORE_LOC        :: THRDADDR
      INTEGER(UINT)                                :: $BEGINTHREADEX
      INTEGER(UINT),               INTENT(IN)      :: STACK_SIZE, INITFLAG
      INTEGER(PVOID),              INTENT(IN)      :: START_ADDRESS, ARGLIST
      INTEGER(UINT),               INTENT(OUT)     :: THRDADDR
      TYPE(T_SECURITY_ATTRIBUTES), INTENT(IN)      :: SECURITY
    END FUNCTION $BEGINTHREADEX
  END INTERFACE
  INTERFACE
    SUBROUTINE $ENDTHREADEX (RETVAL)
      USE DFWINTY, RENAMED => DLT
      !DEC$ ATTRIBUTES C, ALIAS : "__ENDTHREADEX" :: $ENDTHREADEX
      INTEGER(UINT), INTENT(IN) :: RETVAL
    END SUBROUTINE $ENDTHREADEX
  END INTERFACE
END MODULE MSCLIB
MODULE PREC
  INTEGER, PARAMETER :: I2=SELECTED_INT_KIND (3)
  INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(15)
END MODULE PREC
MODULE RSTART
  USE PREC
  REAL(R8)                                           :: DLTS,   CURMAX, DLTFF, DLTMAXX    ! SW 7/13/2010
  INTEGER                                            :: RSODP,  DLTDP,  TSRDP,  WDODP,  CUF,    RSO=31
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: SNPDP,  VPLDP,  CPLDP,  PRFDP,  SCRDP,  SPRDP,  FLXDP, NSPRF
  REAL                                               :: NXTMRS, NXTMWD, NXTMTS
  REAL,              ALLOCATABLE, DIMENSION(:)       :: NXTMSN, NXTMPR, NXTMSP, NXTMCP, NXTMVP, NXTMSC, NXTMFL
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: SBKT,   ELTMF
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: TSSUH2, TSSDH2, SAVH2,  SAVHR,  SU,     SW,     SAZ
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:,:)   :: CSSUH2, CSSDH2
  REAL(R8)                                           :: ELTM
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: VOLIN,  VOLOUT, VOLUH,  VOLDH,  VOLPR,  VOLTRB, VOLDT, VOLWD,  VOLEV
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: VOLSBR, VOLTBR, VOLSR,  VOLTR
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: TSSEV,  TSSPR,  TSSTR,  TSSDT,  TSSWD,  TSSUH,  TSSDH, TSSIN,  TSSOUT
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: TSSS,   TSSB,   TSSICE
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: ESBR,   ETBR,   EBRI,   SZ
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: CMBRT
END MODULE RSTART
MODULE GLOBAL
  USE PREC
  REAL*4                                             :: W2VER=3.71
  REAL(R8),   PARAMETER                              :: DAY=86400.0D0,  NONZERO=1.0D-20, REFL=0.94D0, FRAZDZ=0.14D0, DZMIN=1.4D-7
  REAL(R8),   PARAMETER                              :: AZMIN=1.4D-6, DZMAX=1.0D3,     RHOW=1000.0D0
  REAL(R8)                                           :: DLT,    DLTMIN, DLTTVD
  REAL(R8)                                           :: BETABR, START,  HMAX2,  CURRENT
  REAL(R8),   POINTER,            DIMENSION(:,:)     :: U,      W,      T2,     AZ,     RHO,    ST,     SB
  REAL(R8),   POINTER,            DIMENSION(:,:)     :: DLTLIM, VSH,    ADMX,   DM,     ADMZ,   HDG,    HPG,    GRAV
  REAL(R8),   TARGET,ALLOCATABLE, DIMENSION(:,:)     :: T1,     TSS
  REAL(R8),   TARGET,ALLOCATABLE, DIMENSION(:,:,:)   :: C1,     C2,     C1S,    CSSB,   CSSK
  REAL,      TARGET,ALLOCATABLE, DIMENSION(:,:,:)    :: KF,     CD
  REAL(R8),   TARGET,ALLOCATABLE, DIMENSION(:,:,:)   :: HYD
  REAL,       TARGET,ALLOCATABLE, DIMENSION(:,:,:,:) :: AF,     EF
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: ICETH,  ELKT,   HMULT,  CMULT,  CDMULT, WIND2,  AZMAX,  PALT, Z0
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: QSS,    VOLUH2, VOLDH2, QUH1,   QDH1,   UXBR,   UYBR,   VOL
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: ALLIM,  APLIM,  ANLIM,  ASLIM,  KFS
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: ELLIM,  EPLIM,  ENLIM,  ESLIM
  INTEGER                                            :: W2ERR,  WRN
  INTEGER                                            :: IMX,    KMX,    NBR,    NTR,    NWD,    NWB,    NCT,    NBOD
  INTEGER                                            :: NST,    NSP,    NGT,    NPI,    NPU,    NWDO,   NIKTSR, NUNIT
  INTEGER                                            :: JW,     JB,     JC,     IU,     ID,     KT,     I,      JJB
  INTEGER                                            :: NOD,    NDC,    NAL,    NSS,    NHY,    NFL,    NEP,    NEPT
  INTEGER                                            :: NZP,    NZPT, JZ,     NZOOS,  NZOOE,  NMC,   NMCT  ! number of zooplankton groups, CONSTIUENT NUMBER FOR ZOOPLANKTON, START AND END
  INTEGER, POINTER,               DIMENSION(:)       :: SNP,    PRF,    VPL,    CPL,    SPR,    FLX,    FLX2
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: BS,     BE,     US,     CUS,    DS,     JBDN
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: KB,     KTI,    SKTI,   KTWB,   KBMIN,  CDHS
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: UHS,    DHS,    UQB,    DQB
  INTEGER, TARGET,   ALLOCATABLE, DIMENSION(:,:)     :: OPT
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: NBODC, NBODN, NBODP        ! CB 6/6/10
  LOGICAL,           ALLOCATABLE, DIMENSION(:)       :: ICE,    ICE_CALC,LAYERCHANGE
  CHARACTER(10)                                      :: CCTIME
  CHARACTER(12)                                      :: CDATE
  CHARACTER(72)                                      :: RSIFN
  CHARACTER(180)                                     :: MODDIR                     ! CURRENT WORKING DIRECTORY
  REAL(R8),     SAVE, ALLOCATABLE, DIMENSION(:,:)    :: RATZ,   CURZ1,  CURZ2,  CURZ3    ! SW 5/15/06
  REAL(R8)                                           :: G,PI
  REAL(R8)                                           :: DENSITY
  DATA                                        NDC /23/, NHY /15/, NFL /121/      ! cb 6/6/10
  DATA                                        G /9.81D0/, PI/3.14159265359D0/
  DATA                                        WRN /32/, W2ERR /33/
  EXTERNAL DENSITY
END MODULE GLOBAL
MODULE GEOMC
  USE PREC
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: JBUH,   JBDH,   JWUH,   JWDH
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: ALPHA,  SINA,   COSA,   SLOPE,  BKT,    DLX,    DLXR, SLOPEC, SINAC
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: H,      H1,     H2,     BH1,    BH2,    BHR1,    BHR2,   AVHR
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: B,      BI,     BB,     BH,     BHR,    BR,      EL,     AVH1,  AVH2, BNEW ! SW 1/23/06
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: DEPTHB, DEPTHM, FETCHU, FETCHD
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: Z, ELWS
END MODULE GEOMC
MODULE NAMESC
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: LNAME
  CHARACTER(6),      ALLOCATABLE, DIMENSION(:)       :: CUNIT,  CUNIT2
  CHARACTER(8),      ALLOCATABLE, DIMENSION(:)       :: CNAME2, CDNAME2
  CHARACTER(9),      ALLOCATABLE, DIMENSION(:)       :: FMTH,   FMTC,   FMTCD
  CHARACTER(19),     ALLOCATABLE, DIMENSION(:)       :: CNAME1
  CHARACTER(43),     ALLOCATABLE, DIMENSION(:)       :: CNAME,  CNAME3, CDNAME, CDNAME3, HNAME
  CHARACTER(72),     ALLOCATABLE, DIMENSION(:)       :: TITLE
  CHARACTER(10),     ALLOCATABLE, DIMENSION(:,:)     :: CONV
END MODULE NAMESC
MODULE STRUCTURES
  USE PREC
  REAL(R8)                                           :: DIA,    FMAN,   CLEN,   CLOSS,  UPIE,   DNIE
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: QOLD,   QOLDS,  VMAX,   DTP,    DTPS
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: EGT,    A1GT,   B1GT,   G1GT,   A2GT,   B2GT,   G2GT, EGT2
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: QGT,    GTA1,   GTB1,   GTA2,   GTB2,   BGT
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: QSP,    A1SP,   B1SP,   A2SP,   B2SP,   ESP
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: EUPI,   EDPI,   WPI,    DLXPI,  FPI,    FMINPI, QPI, BP
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: YS,     VS,     YSS,    VSS,    YST,    VST,    YSTS,   VSTS
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: IUPI,   IDPI,   JWUPI,  JWDPI,  JBDPI,  JBUPI
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: IUSP,   IDSP,   JWUSP,  JWDSP,  JBUSP,  JBDSP
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: IUGT,   IDGT,   JWUGT,  JWDGT,  JBUGT,  JBDGT
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: IWR,    KTWR,   KBWR
  LOGICAL,           ALLOCATABLE, DIMENSION(:)       :: LATERAL_SPILLWAY, LATERAL_PIPE, LATERAL_GATE, LATERAL_PUMP, BEGIN, WLFLAG
  CHARACTER(8),      ALLOCATABLE, DIMENSION(:)       :: LATGTC, LATSPC, LATPIC, LATPUC, DYNGTC, DYNPIPE, DYNPUMP                         ! SW 5/10/10
  CHARACTER(8)                                       :: GT2CHAR 
  REAL(R8),      ALLOCATABLE, DIMENSION(:)           :: EPU,    STRTPU, ENDPU,  EONPU,  EOFFPU, QPU
  INTEGER,       ALLOCATABLE, DIMENSION(:)           :: IUPU,   IDPU,   KTPU,   KBPU,   JWUPU,  JWDPU,  JBUPU,  JBDPU
  REAL(R8)                                           :: THR, OMEGA, EPS2
  INTEGER                                            :: NN, NNPIPE, NC
  REAL,              ALLOCATABLE, DIMENSION(:)       :: EGTO,BGTO       ! CB/8/13/ 2010
  CHARACTER(8),      ALLOCATABLE, DIMENSION(:)       :: GTIC            ! cb/8/13/ 2010
  DATA                                             THR/0.01D0/, OMEGA/0.8D0/, EPS2/0.0001D0/
  DATA                                          NN/19/ ,   NNPIPE /19/, NC/7/
END MODULE STRUCTURES
MODULE TRANS
  USE PREC
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: THETA
  REAL(R8),POINTER,               DIMENSION(:,:)     :: COLD,   CNEW,   SSB,    SSK
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: DX,     DZ,     DZQ
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: ADX,    ADZ,    AT,     VT,     CT,     DT
END MODULE TRANS
MODULE SURFHE
USE PREC
  REAL(R8)                                           :: RHOWCP, PHISET
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: ET,     CSHE,   LAT,    LONGIT, SHADE,  RB,     RE,     RC
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: WIND,   WINDH,  WSC,    AFW,    BFW,    CFW,    PHI0
  LOGICAL,       ALLOCATABLE, DIMENSION(:)       :: RH_EVAP
  INTEGER,       ALLOCATABLE, DIMENSION(:)       :: IWIND  !MLM 08/12/05
END MODULE SURFHE
MODULE TVDC
  USE PREC
  REAL(R8),              ALLOCATABLE, DIMENSION(:)       :: QIN,    QTR,    QDTR,   PR,     ELUH,   ELDH,   QWD,    QSUM
  REAL(R8),              ALLOCATABLE, DIMENSION(:)       :: TIN,    TTR,    TDTR,   TPR,    TOUT,   TWDO,   TIND,   QIND
  REAL(R8),              ALLOCATABLE, DIMENSION(:)       :: TAIR,   TDEW,   CLOUD,  PHI,    SRON
  REAL(R8),              ALLOCATABLE, DIMENSION(:,:)     :: CIN,    CTR,    CDTR,   CPR,    CIND,   TUH,    TDH,    QOUT
  REAL(R8),              ALLOCATABLE, DIMENSION(:,:,:)   :: CUH,    CDH
  INTEGER                                            :: NAC,    NOPEN
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: NACPR,  NACIN,  NACDT,  NACTR,  NACD,   CN
  INTEGER,           ALLOCATABLE, DIMENSION(:,:)     :: TRCN,   INCN,   DTCN,   PRCN
  LOGICAL                                            :: CONSTITUENTS
  CHARACTER(72)                                      :: QGTFN,  QWDFN,  WSCFN,  SHDFN
  CHARACTER(72),     ALLOCATABLE, DIMENSION(:)       :: METFN,  QOTFN,  QINFN,  TINFN,  CINFN,  QTRFN,  TTRFN,  CTRFN,  QDTFN
  CHARACTER(72),     ALLOCATABLE, DIMENSION(:)       :: TDTFN,  CDTFN,  PREFN,  TPRFN,  CPRFN,  EUHFN,  TUHFN,  CUHFN,  EDHFN
  CHARACTER(72),     ALLOCATABLE, DIMENSION(:)       :: EXTFN,  CDHFN,  TDHFN
END MODULE TVDC
MODULE KINETIC
  USE PREC
  REAL                                               :: KDO                        
  REAL(R8),    POINTER,           DIMENSION(:,:)     :: TDS,    COL,    NH4,    NO3,    PO4,    FE,     DSI,    PSI,    LDOM
  REAL(R8),    POINTER,           DIMENSION(:,:)     :: RDOM,   LPOM,   RPOM,   O2,     TIC,    ALK
  REAL(R8),    POINTER,           DIMENSION(:,:)     :: COLSS,  NH4SS,  NO3SS,  PO4SS,  FESS,   DSISS,  PSISS,  LDOMSS
  REAL(R8),    POINTER,           DIMENSION(:,:)     :: RDOMSS, LPOMSS, RPOMSS, DOSS,   TICSS,  CASS
  REAL,    POINTER,               DIMENSION(:,:)     :: PH,     CO2,    HCO3,   CO3
  REAL,    POINTER,               DIMENSION(:,:)     :: TN,     TP,     TKN
  REAL,    POINTER,               DIMENSION(:,:)     :: DON,    DOP,    DOC
  REAL,    POINTER,               DIMENSION(:,:)     :: PON,    POP,    POC
  REAL,    POINTER,               DIMENSION(:,:)     :: TON,    TOP,    TOC
  REAL,    POINTER,               DIMENSION(:,:)     :: APR,    CHLA,   ATOT
  REAL,    POINTER,               DIMENSION(:,:)     :: O2DG
  REAL,    POINTER,               DIMENSION(:,:)     :: SSSI,   SSSO,   TISS,   TOTSS
  REAL,    POINTER,               DIMENSION(:,:)     :: PO4AR,  PO4AG,  PO4AP,  PO4SD,  PO4SR,  PO4NS,  PO4POM, PO4DOM, PO4OM
  REAL,    POINTER,               DIMENSION(:,:)     :: PO4ER,  PO4EG,  PO4EP,  TICEP,  DOEP,   DOER
  REAL,    POINTER,               DIMENSION(:,:)     :: NH4ER,  NH4EG,  NH4EP,  NO3EG,  DSIEG,  LDOMEP, LPOMEP
  REAL,    POINTER,               DIMENSION(:,:)     :: NH4AR,  NH4AG,  NH4AP,  NH4SD,  NH4SR,  NH4D,   NH4POM, NH4DOM, NH4OM
  REAL,    POINTER,               DIMENSION(:,:)     :: NO3AG,  NO3D,   NO3SED
  REAL,    POINTER,               DIMENSION(:,:)     :: DSIAG,  DSID,   DSISD,  DSISR,  DSIS
  REAL,    POINTER,               DIMENSION(:,:)     :: PSIAM,  PSID,   PSINS
  REAL,    POINTER,               DIMENSION(:,:)     :: FENS,   FESR
  REAL,    POINTER,               DIMENSION(:,:)     :: LDOMAP, LDOMD,  LRDOMD, RDOMD
  REAL,    POINTER,               DIMENSION(:,:)     :: LPOMAP, LPOMD,  LRPOMD, RPOMD,  LPOMNS, RPOMNS
  REAL,    POINTER,               DIMENSION(:,:)     :: DOAP,   DOAR,   DODOM,  DOPOM,  DOOM,   DONIT
  REAL,    POINTER,               DIMENSION(:,:)     :: DOSED,  DOSOD,  DOBOD,  DOAE
  REAL,    POINTER,               DIMENSION(:,:)     :: CBODU,  CBODDK, TICAP
  REAL,    POINTER,               DIMENSION(:,:)     :: SEDD,   SODD,   SEDAS,  SEDOMS, SEDNS
  REAL(R8),POINTER,               DIMENSION(:,:,:)   :: SS,     ALG,    CBOD,   CG
  REAL(R8),POINTER,               DIMENSION(:,:,:)   :: SSSS,   ASS,    CBODSS, CGSS
  REAL,    POINTER,               DIMENSION(:,:,:)   :: AGR,    ARR,    AER,    AMR,    ASR
  REAL,    POINTER,               DIMENSION(:,:,:)   :: EGR,    ERR,    EER,    EMR,    EBR
  REAL(R8),POINTER,               DIMENSION(:,:)     :: LDOMP,  RDOMP,  LPOMP,  RPOMP,  LDOMN,  RDOMN,  LPOMN,  RPOMN
  REAL(R8),POINTER,               DIMENSION(:,:)     :: LDOMPSS,  RDOMPSS, LPOMPSS, RPOMPSS, LDOMNSS, RDOMNSS
  REAL(R8),POINTER,               DIMENSION(:,:)     :: LPOMNSS,  RPOMNSS
  REAL,    POINTER,               DIMENSION(:,:)     :: LDOMPAP,  LDOMPEP, LPOMPAP, LPOMPNS, RPOMPNS
  REAL,    POINTER,               DIMENSION(:,:)     :: LDOMNAP,  LDOMNEP, LPOMNAP, LPOMNNS, RPOMNNS
  REAL,    POINTER,               DIMENSION(:,:)     :: SEDDP,    SEDASP,  SEDOMSP, SEDNSP,  LPOMEPP
  REAL,    POINTER,               DIMENSION(:,:)     :: SEDDN,    SEDASN,  SEDOMSN, SEDNSN,  LPOMEPN, SEDNO3
  REAL,    POINTER,               DIMENSION(:,:)     :: SEDDC,    SEDASC,  SEDOMSC, SEDNSC,  LPOMEPC
  REAL,    POINTER,               DIMENSION(:,:)     :: CBODNS,   SEDCB,   SEDCBP,  SEDCBN,  SEDCBC
  REAL,    POINTER,               DIMENSION(:,:)     :: SEDBR,    SEDBRP,  SEDBRC,  SEDBRN, CO2REAER        !CB 11/30/06
  REAL(R8),POINTER,               DIMENSION(:,:,:)   :: CBODP,    CBODN       ! CB 6/6/10
  REAL(R8),POINTER,               DIMENSION(:,:,:)   :: CBODPSS,    CBODNSS       ! CB 6/6/10
  REAL,    POINTER,               DIMENSION(:,:)     :: CBODNSP,  CBODNSN          ! cb 6/6/10
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: EPM,    EPD,    EPC
  REAL,              ALLOCATABLE, DIMENSION(:)       :: CGQ10,  CG0DK,  CG1DK,  CGS
  REAL,              ALLOCATABLE, DIMENSION(:)       :: SOD,    SDK,    LPOMDK, RPOMDK, LDOMDK, RDOMDK, LRDDK,  LRPDK
  REAL,              ALLOCATABLE, DIMENSION(:)       :: SSS,    TAUCR,  POMS,   FES, seds, sedb  !cb 11/27/06
  REAL,              ALLOCATABLE, DIMENSION(:)       :: AG,     AR,     AE,     AM,     AS,     AHSN,   AHSP,   AHSSI,  ASAT
  REAL,              ALLOCATABLE, DIMENSION(:)       :: AP,     AN,     AC,     ASI,    ACHLA,  APOM,   ANPR
  REAL,              ALLOCATABLE, DIMENSION(:)       :: EG,     ER,     EE,     EM,     EB
  REAL,              ALLOCATABLE, DIMENSION(:)       :: EHSN,   EHSP,   EHSSI,  ESAT,   EHS,    ENPR
  REAL,              ALLOCATABLE, DIMENSION(:)       :: EP,     EN,     EC,     ESI,    ECHLA,  EPOM
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: BETA,   EXH2O,  EXSS,   EXOM,   EXA
  REAL,              ALLOCATABLE, DIMENSION(:)       :: DSIR,   PSIS,   PSIDK,  PARTSI
  REAL,              ALLOCATABLE, DIMENSION(:)       :: ORGP,   ORGN,   ORGC,   ORGSI
  REAL,              ALLOCATABLE, DIMENSION(:)       :: BODP,   BODN,   BODC
  REAL,              ALLOCATABLE, DIMENSION(:)       :: PO4R,   PARTP
  REAL,              ALLOCATABLE, DIMENSION(:)       :: NH4DK,  NH4R,   NO3DK,  NO3S, FNO3SED
  REAL,              ALLOCATABLE, DIMENSION(:)       :: O2AG,   O2AR,   O2OM,   O2NH4
  REAL,              ALLOCATABLE, DIMENSION(:)       :: O2EG,   O2ER
  REAL,              ALLOCATABLE, DIMENSION(:)       :: CO2R,   FER
  REAL,              ALLOCATABLE, DIMENSION(:)       :: KBOD,   TBOD,   RBOD
  REAL,              ALLOCATABLE, DIMENSION(:)       :: CAQ10,  CADK,   CAS
  REAL,              ALLOCATABLE, DIMENSION(:)       :: OMT1,   OMT2,   SODT1,  SODT2,  NH4T1,  NH4T2,  NO3T1,  NO3T2
  REAL,              ALLOCATABLE, DIMENSION(:)       :: OMK1,   OMK2,   SODK1,  SODK2,  NH4K1,  NH4K2,  NO3K1,  NO3K2
  REAL,              ALLOCATABLE, DIMENSION(:)       :: AT1,    AT2,    AT3,    AT4
  REAL,              ALLOCATABLE, DIMENSION(:)       :: AK1,    AK2,    AK3,    AK4
  REAL,              ALLOCATABLE, DIMENSION(:)       :: ET1,    ET2,    ET3,    ET4
  REAL,              ALLOCATABLE, DIMENSION(:)       :: EK1,    EK2,    EK3,    EK4
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: WIND10, CZ,     QC,     QERR
  REAL,              ALLOCATABLE, DIMENSION(:)       :: REAER,  RCOEF1, RCOEF2, RCOEF3, RCOEF4
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: DO1,    DO2,    DO3,    GAMMA
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: SED,    FPSS,   FPFE
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: CBODD
  REAL,              ALLOCATABLE, DIMENSION(:)       :: CBODS
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: ORGPLD,  ORGPRD,   ORGPLP,    ORGPRP,  ORGNLD,  ORGNRD, ORGNLP, ORGNRP
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: LDOMPMP, LDOMNMP,  LPOMPMP,   LPOMNMP, RPOMPMP, RPOMNMP
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: LPZOOINP,LPZOOINN, LPZOOOUTP, LPZOOOUTN
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: SEDC,    SEDN, SEDP
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: SEDVPC,  SEDVPP, SEDVPN
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: SDKV,    SEDDKTOT
  INTEGER                                            :: NLDOMP,NRDOMP,NLPOMP,NRPOMP,NLDOMN,NRDOMN,NLPOMN,NRPOMN
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: NAF,    NEQN,   ANEQN,  ENEQN
  INTEGER,           ALLOCATABLE, DIMENSION(:,:)     :: KFCN
  LOGICAL,           ALLOCATABLE, DIMENSION(:)       :: SEDIMENT_RESUSPENSION
  CHARACTER(8),      ALLOCATABLE, DIMENSION(:)       :: CAC,    REAERC
  CHARACTER(10),     ALLOCATABLE, DIMENSION(:,:)     :: LFPR
  CONTAINS
  REAL    FUNCTION SATO (T,SAL,P,SALT_WATER)
      REAL(R8) :: T,SAL
      REAL(R8) :: P
      LOGICAL  :: SALT_WATER
      SATO = EXP(7.7117-1.31403*(LOG(T+45.93)))*P
      IF (SALT_WATER) SATO = EXP(LOG(SATO)-SAL*(1.7674E-2-1.0754E1/(T+273.15)+2.1407E3/(T+273.15)**2))
    END FUNCTION SATO
  REAL  FUNCTION FR (TT,TT1,TT2,SK1,SK2)
      REAL(R8) :: TT
      REAL     :: TT1,TT2,SK1,SK2
      FR = SK1*EXP(LOG(SK2*(1.0-SK1)/(SK1*(1.0-SK2)))/(TT2-TT1)*(TT-TT1))
    END FUNCTION FR
  REAL  FUNCTION FF (TT,TT3,TT4,SK3,SK4)
      REAL     :: TT3,TT4,SK3,SK4
      REAL(R8) :: TT
      FF = SK4*EXP(LOG(SK3*(1.0-SK4)/(SK4*(1.0-SK3)))/(TT4-TT3)*(TT4-TT))
    END FUNCTION FF
END MODULE KINETIC
MODULE SELWC
  USE PREC
  REAL(R8),              ALLOCATABLE, DIMENSION(:)    :: VNORM,  QNEW
  !REAL,                  ALLOCATABLE, DIMENSION(:)    :: EWD, TAVGW                   ! cb 1/16/13
  real,                  allocatable, dimension(:)    :: ewd
  REAL(R8),              ALLOCATABLE, DIMENSION(:,:)  :: QSTR,   QSW
  !REAL,                  ALLOCATABLE, DIMENSION(:,:)  :: ESTR,   WSTR, TAVG            ! SW Selective 7/30/09
  real,                  allocatable, dimension(:,:)  :: estr,   wstr                  ! cb 1/16/13
  real(r8),              allocatable, dimension(:,:)  :: tavg                  ! cb 1/16/13
  real(r8),              allocatable, dimension(:)    :: tavgw                  ! cb 1/16/13
  !REAL,                  ALLOCATABLE, DIMENSION(:,:)  :: CAVGW,  CDAVGW
  !REAL,                  ALLOCATABLE, DIMENSION(:,:,:):: CAVG,   CDAVG
  real(r8),              allocatable, dimension(:,:)  :: cavgw,  cdavgw                  ! cb 1/16/13
  real(r8),              allocatable, dimension(:,:,:):: cavg,   cdavg                  ! cb 1/16/13
  INTEGER,               ALLOCATABLE, DIMENSION(:)    :: NSTR,   NOUT,   KTWD,   KBWD,   KTW,   KBW
  INTEGER,               ALLOCATABLE, DIMENSION(:,:)  :: KTSW,   KBSW,   KOUT
  CHARACTER(8),          ALLOCATABLE, DIMENSION(:)    :: DYNSTRUC
END MODULE SELWC
MODULE GDAYC
  REAL                                           :: DAYM,   EQTNEW
  INTEGER                                        :: JDAYG,  M,      YEAR,   GDAY
  LOGICAL                                        :: LEAP_YEAR
  CHARACTER(9)                                   :: MONTH
END MODULE GDAYC
MODULE SCREENC
  USE PREC
  REAL                                           :: JDAY,   DLTS1,  JDMIN,  MINDLT, DLTAV,  ELTMJD
  REAL,              ALLOCATABLE, DIMENSION(:)   :: ZMIN,   CMIN,   CMAX,   HYMIN,  HYMAX,  CDMIN,  CDMAX
  INTEGER                                        :: ILOC,   KLOC,   IMIN,   KMIN,   NIT,    NV,     JTT,     JWW
  INTEGER,           ALLOCATABLE, DIMENSION(:)   :: IZMIN
  CHARACTER(8),      ALLOCATABLE, DIMENSION(:)   :: ACPRC,  AHPRC,  ACDPRC
END MODULE SCREENC
MODULE TRIDIAG_V
USE PREC
!INTEGER,                             INTENT(IN)  :: S, E, N
!  REAL(R8),              DIMENSION(:), INTENT(IN)  :: A(E),V(E),C(E),D(E)
!  REAL(R8),              DIMENSION(:), INTENT(OUT) :: U(N)
  REAL(R8), ALLOCATABLE, DIMENSION(:)              :: BTA1, GMA1
  !REAL(R8), DIMENSION(1000)              :: BTA, GMA
!  INTEGER                                          :: I
END MODULE TRIDIAG_V
MODULE TDGAS
  REAL,              ALLOCATABLE, DIMENSION(:)   :: AGASSP, BGASSP, CGASSP, AGASGT, BGASGT, CGASGT
  INTEGER,           ALLOCATABLE, DIMENSION(:)   :: EQSP,   EQGT
END MODULE TDGAS
MODULE LOGICC
  LOGICAL                                        :: SUSP_SOLIDS,        OXYGEN_DEMAND,    UPDATE_GRAPH,     INITIALIZE_GRAPH
  LOGICAL                                        :: WITHDRAWALS,        TRIBUTARIES,      GATES, PIPES
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: NO_WIND,            NO_INFLOW,        NO_OUTFLOW,       NO_HEAT
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: UPWIND,             ULTIMATE,         FRESH_WATER,      SALT_WATER
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: LIMITING_DLT,       TERM_BY_TERM,     MANNINGS_N,       PH_CALC
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: ONE_LAYER,          DIST_TRIBS,       PRECIPITATION
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: PRINT_SEDIMENT,     LIMITING_FACTOR,  READ_EXTINCTION,  READ_RADIATION
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: UH_INTERNAL,        DH_INTERNAL,      UH_EXTERNAL,      DH_EXTERNAL
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: UQ_INTERNAL,        DQ_INTERNAL,      UQ_EXTERNAL,      DQ_EXTERNAL
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: UP_FLOW,            DN_FLOW,          INTERNAL_FLOW
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: DAM_INFLOW,         DAM_OUTFLOW                                    !TC 08/03/04
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: INTERP_METEOROLOGY, INTERP_INFLOW,    INTERP_DTRIBS,    INTERP_TRIBS
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: INTERP_WITHDRAWAL,  INTERP_HEAD,      INTERP_EXTINCTION
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: VISCOSITY_LIMIT,    CELERITY_LIMIT,   IMPLICIT_AZ,      TRAPEZOIDAL !SW 07/16/04
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: HYDRO_PLOT,         CONSTITUENT_PLOT, DERIVED_PLOT
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: INTERP_GATE     ! cb 8/13/2010
  LOGICAL,           ALLOCATABLE, DIMENSION(:,:) :: PRINT_DERIVED,      PRINT_HYDRO,      PRINT_CONST,      PRINT_EPIPHYTON
  LOGICAL,           ALLOCATABLE, DIMENSION(:,:) :: POINT_SINK,         INTERNAL_WEIR,    INTERP_OUTFLOW
END MODULE LOGICC
MODULE SHADEC
  integer, PARAMETER :: IANG=18
  REAL,PARAMETER                                 :: GAMA=(3.1415926*2.)/REAL(IANG)                         ! SW 10/17/05
  REAL,                           DIMENSION(IANG):: ANG                                                    ! SW 10/17/05
  REAL,              ALLOCATABLE, DIMENSION(:)   :: A00,    DECL,   HH,     TTLB,   TTRB,   CLLB,   CLRB   ! SW 10/17/05
  REAL,              ALLOCATABLE, DIMENSION(:)   :: SRLB1,  SRRB1,  SRLB2,  SRRB2,  SRFJD1, SRFJD2, SHADEI
  REAL,              ALLOCATABLE, DIMENSION(:,:) :: TOPO
  LOGICAL,           ALLOCATABLE, DIMENSION(:)   :: DYNAMIC_SHADE
  DATA ANG  /0.00000, 0.34907, 0.69813, 1.04720, 1.39626, 1.74533, 2.09440, 2.44346, &
            2.79253, 3.14159, 3.49066, 3.83972, 4.18879, 4.53786, 4.88692, 5.23599, 5.58505, 5.93412/      ! SW 10/17/05
END MODULE SHADEC
MODULE EDDY
USE PREC
  CHARACTER(8),      ALLOCATABLE, DIMENSION(:)      :: AZC,IMPTKE
  REAL(R8),          ALLOCATABLE, DIMENSION(:)      :: WSHY,   FRIC
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)    :: FRICBR, DECAY
  REAL(R8),          ALLOCATABLE, DIMENSION (:,:,:) :: TKE
  REAL(R8),          ALLOCATABLE, DIMENSION (:,:)   :: AZT, DZT
  REAL(R8),          ALLOCATABLE, DIMENSION(:)      :: USTARBTKE, E
  REAL(R8),          ALLOCATABLE, DIMENSION(:)      :: EROUGH, ARODI, TKELATPRDCONST, STRICK
  INTEGER,           ALLOCATABLE, DIMENSION(:)      :: FIRSTI, LASTI, WALLPNT, TKEBC
  LOGICAL,           ALLOCATABLE, DIMENSION(:)      :: STRICKON, TKELATPRD
END MODULE EDDY
MODULE MACROPHYTEC
  REAL,    POINTER,               DIMENSION(:,:)     :: NH4MR,  NH4MG,  LDOMMAC, RPOMMAC, LPOMMAC, DOMP, DOMR, TICMC
  REAL,    POINTER,               DIMENSION(:,:)     :: PO4MR,  PO4MG
  REAL,              ALLOCATABLE, DIMENSION(:)       :: MG,     MR,     MM, MMAX,   MBMP
  REAL,              ALLOCATABLE, DIMENSION(:)       :: MT1,    MT2,    MT3,    MT4,    MK1,    MK2,    MK3,    MK4
  REAL,              ALLOCATABLE, DIMENSION(:)       :: MP,     MN,     MC
  REAL,              ALLOCATABLE, DIMENSION(:)       :: PSED,   NSED,   MHSP,   MHSN,   MHSC,   MSAT,   EXM
  REAL,              ALLOCATABLE, DIMENSION(:)       :: CDDRAG, DWV,    DWSA,  ANORM
  REAL,              ALLOCATABLE, DIMENSION(:)       :: ARMAC
  REAL,              ALLOCATABLE, DIMENSION(:)       :: O2MG,   O2MR,   LRPMAC,  MPOM
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: MACMBRS,MACMBRT,SSMACMB
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: CW,     BIC, MACWBCI
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: MACTRMR,MACTRMF,MACTRM
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: MMR,    MRR
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: MAC,    MACT
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: MPLIM,  MNLIM, MCLIM
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: SMAC,   SMACT
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: GAMMAJ
  REAL,              ALLOCATABLE, DIMENSION(:,:,:,:) :: MGR
  REAL,              ALLOCATABLE, DIMENSION(:,:,:,:) :: MACRC,  MACRM
  REAL,              ALLOCATABLE, DIMENSION(:,:,:,:) :: MLLIM
  REAL,              ALLOCATABLE, DIMENSION(:,:,:,:) :: MACSS
  REAL,              ALLOCATABLE, DIMENSION(:,:,:,:) :: SMACRC, SMACRM
  LOGICAL,           ALLOCATABLE, DIMENSION(:)       :: KTICOL
  LOGICAL,           ALLOCATABLE, DIMENSION(:,:)     :: PRINT_MACROPHYTE, MACROPHYTE_CALC
  LOGICAL                                            :: MACROPHYTE_ON
  CHARACTER(3),      ALLOCATABLE, DIMENSION(:,:)     :: MPRWBC, MACWBC
  CHARACTER(10),      ALLOCATABLE, DIMENSION(:,:)    :: CONV2
  CHARACTER(10),     ALLOCATABLE, DIMENSION(:,:,:,:) :: MLFPR
END MODULE MACROPHYTEC
MODULE POROSITYC
    REAL,              ALLOCATABLE, DIMENSION(:)     :: SAREA, VOLKTI
    REAL,              ALLOCATABLE, DIMENSION(:,:)   :: POR,   VOLI,   VSTEMKT
    REAL,              ALLOCATABLE, DIMENSION(:,:,:) :: VSTEM
    LOGICAL,       ALLOCATABLE, DIMENSION(:)         :: HEAD_FLOW
    LOGICAL,       ALLOCATABLE, DIMENSION(:)         :: UP_HEAD
END MODULE POROSITYC
MODULE ZOOPLANKTONC
  USE PREC
  LOGICAL                                            :: ZOOPLANKTON_CALC
  REAL,              ALLOCATABLE, DIMENSION(:)       :: ZG,ZM,ZEFF,PREFP,ZR,ZOOMIN,ZS2P,EXZ
  REAL,              ALLOCATABLE, DIMENSION(:)       :: ZT1,ZT2,ZT3,ZT4,ZK1,ZK2,ZK3,ZK4
  REAL,              ALLOCATABLE, DIMENSION(:)       :: ZP,ZN,ZC,O2ZR
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: PREFA, PREFZ ! OMNIVOROUS ZOOPLANKTON
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: PO4ZR,NH4ZR,DOZR,TICZR,LPZOOOUT,LPZOOIN
  REAL(R8),POINTER,               DIMENSION(:,:,:)   :: ZOO, ZOOSS
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: ZMU,TGRAZE,ZRT,ZMT
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: ZOORM,ZOORMR,ZOORMF
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: AGZT
  REAL,              ALLOCATABLE, DIMENSION(:,:,:,:) :: AGZ, ZGZ ! OMNIVOROUS ZOOPLANKTON
END MODULE ZOOPLANKTONC
MODULE INITIALVELOCITY
  USE PREC
  LOGICAL                                            :: INIT_VEL, ONCE_THROUGH
  REAL(R8),          ALLOCATABLE, DIMENSION(:)       :: QSSI,ELWSS,UAVG
  REAL(R8),          ALLOCATABLE, DIMENSION(:,:)     :: BSAVE
  LOGICAL,           ALLOCATABLE, DIMENSION(:)       :: LOOP_BRANCH
END MODULE INITIALVELOCITY
MODULE ENVIRPMOD
CHARACTER*3, SAVE, ALLOCATABLE, DIMENSION (:) :: CC_E,CD_E
CHARACTER*3, SAVE :: VEL_VPR,TEMP_VPR,SELECTIVEC,DEPTH_VPR
REAL, SAVE :: TEMP_INT,TEMP_TOP,VEL_INT,VEL_TOP,SUMVOLT,TIMLAST,DLTT,V_CNT,V_SUM,V_TOT,VOLGL,T_CRIT,V_CRIT,C_CRIT,CD_CRIT,TEMP_C,V_AVG,VEL_C,D_AVG,D_C
REAL, SAVE :: T_CNT,T_SUM,T_TOT,T_AVG,D_CNT,D_SUM,D_TOT,D_CRIT,D_INT,D_TOP,SJDAY1,SJDAY2
REAL,ALLOCATABLE, SAVE, DIMENSION (:) :: C_CNT,CD_CNT,C_TOT,CD_TOT,T_CLASS,V_CLASS,C_SUM,CD_SUM,C_AVG,CD_AVG,D_CLASS
REAL,ALLOCATABLE, SAVE, DIMENSION (:,:) :: C_CLASS,CD_CLASS,CONC_C,CONC_CD
REAL, ALLOCATABLE, SAVE, DIMENSION (:) :: C_INT,C_TOP,CD_INT,CD_TOP,CN_E,CDN_E
INTEGER CONE,NUMCLASS,IOPENFISH,NAC_E,NACD_E,JJ,JACD,ISTART,IEND
DATA CONE/1500/
END MODULE ENVIRPMOD
Module MAIN
USE PREC
! Variable declaration
  INTEGER       :: J,NIW,NGC,NGCS,NTDS,NCCS,NGCE,NSSS,NSSE,NPO4,NNH4
  INTEGER       :: NNO3,NDSI,NPSI,NFE,NLDOM,NRDOM,NLPOM,NRPOM,NBODS
  INTEGER       :: NBODE, NAS, NAE, NDO, NTIC, NALK, NTRT, NWDT
  INTEGER       :: NDT, JS, JP, JG,JT, JH, NTSR, NIWDO, NRSO,JD
  INTEGER       :: NBODCS, NBODCE, NBODPS, NBODPE, NBODNS, NBODNE, IBOD, JCB       ! cb 6/6/10
  INTEGER       :: JF,JA,JM,JE,JJZ,K,L3,L1,L2,NTAC,NDSP,NTACMX,NTACMN,JFILE
  INTEGER       :: KBP,JWR,JJJB,JDAYNX,NIT1,JWD,L,IUT,IDT,KTIP
  INTEGER       :: INCRIS,IE,II,NDLT,NRS,INCR,IS,JAC
  REAL          :: JDAYTS, JDAY1, TMSTRT, TMEND,HMAX, DLXMAX,CELRTY,NXTVD,TTIME
  REAL(R8)      :: DLMR, TICE                        ! SW 4/19/10
  REAL(R8)      :: TAU1,TAU2, ELTMS, EPI,HMIN,DLXMIN, RHOICP
  REAL(R8)      :: HRAD
  REAL(R8)      :: ZB,WWT,DFC,GC2,UDR,UDL,AB, EFFRIC
  REAL(R8)      :: DEPKTI,COLB,COLDEP,SSTOT,RHOIN,VQIN,VQINI
  REAL(R8)      :: QINFR, ELT,RHOIRL1,V1,BHSUM,BHRSUM,WT1,WT2
  REAL(R8)      :: ICETHU, ICETH1, ICETH2, ICE_TOL,DEL,HICE            ! SW 4/19/10
  REAL(R8)      :: DLTCAL,HEATEX,SROOUT,SROSED,SROIN,SRONET,TFLUX,HIA  
  REAL(R8)      :: TAIRV,EA,ES,DTV
  REAL(R8)      :: T2R4
  INTEGER       :: CON,    RSI,    GRF,  NDG=16         
  INTEGER       :: VSF,    SIF
  INTEGER       :: jsg,    nnsg                         ! 1/16/13
  LOGICAL       :: ADD_LAYER,      SUB_LAYER,          WARNING_OPEN,    ERROR_OPEN,      VOLUME_WARNING, SURFACE_WARNING
  LOGICAL       :: END_RUN,        BRANCH_FOUND,       NEW_PAGE,        UPDATE_KINETICS, UPDATE_RATES
  LOGICAL       :: WEIR_CALC,      DERIVED_CALC,       RESTART_IN,      RESTART_OUT
  LOGICAL       :: SPILLWAY,       PUMPS
  LOGICAL       :: TIME_SERIES,    DOWNSTREAM_OUTFLOW, ICE_COMPUTATION
  LOGICAL       :: DSI_CALC, PO4_CALC, N_CALC             ! cb 10/12/11
  logical       :: tdgon                                 ! cb 1/16/13
  CHARACTER(1)  :: ESC
  CHARACTER(2)  :: DEG
  CHARACTER(3)  :: GDCH
  CHARACTER(8)  :: RSOC,   RSIC,   CCC,   LIMC,   WDOC,   TSRC,   EXT, SELECTC, CLOSEC, HABTATC,ENVIRPC, AERATEC, INITUWL, DLTINTER      ! SW 7/31/09; 8/24/09
  CHARACTER(10) :: BLANK,  BLANK1, SEDCH,   SEDPCH,   SEDNCH,   SEDCCH 
  CHARACTER(72) :: WDOFN,  RSOFN,  TSRFN, SEGNUM, LINE, SEGNUM2
  LOGICAL       :: RETLOG

! Allocatable array declarations

  REAL,          ALLOCATABLE, DIMENSION(:)     :: ETUGT,  EBUGT,  ETDGT,  EBDGT
  REAL,          ALLOCATABLE, DIMENSION(:)     :: ETUSP,  EBUSP,  ETDSP,  EBDSP
  REAL,          ALLOCATABLE, DIMENSION(:)     :: ETUPI,  EBUPI,  ETDPI,  EBDPI,  ETPU,   EBPU,   TSEDF
  REAL,          ALLOCATABLE, DIMENSION(:)     :: CSUM,   CDSUM,  X1
  REAL,          ALLOCATABLE, DIMENSION(:)     :: RSOD,   RSOF,   DLTD,   DLTF,   DLTMAX
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: QWDO
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: ICETHI, ALBEDO, HWI,    BETAI,  GAMMAI, ICEMIN, ICET2,  CBHE,   TSED
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: FI,     SEDCI,  FSOD,   FSED,   AX,     RANLW,    T2I,    ELBOT,  DXI
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: QINT,   QOUTT,  WSHX
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: SROSH,  EV, RS, RN
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: QDT,    QPR,    ICESW
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: XBR,    QPRBR,  EVBR,   TPB
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: DLXRHO, Q,      QSSUM
  REAL,          ALLOCATABLE, DIMENSION(:)     :: ELTRT,  ELTRB
  REAL,          ALLOCATABLE, DIMENSION(:)     :: TSRD,   TSRF,   WDOD,   WDOF
  REAL,          ALLOCATABLE, DIMENSION(:)     :: QOAVR,  QIMXR,  QOMXR,  QTAVB,  QTMXB
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: FETCH,  ETSR
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: QINSUM, TINSUM
  REAL,          ALLOCATABLE, DIMENSION(:)     :: CDTOT
  REAL,          ALLOCATABLE, DIMENSION(:)     :: SEDCIP, SEDCIN, SEDCIC, SEDCIS
  REAL,          ALLOCATABLE, DIMENSION(:,:)   :: ESTRT,  WSTRT,  CINSUM, HAB
  REAL(R8),      ALLOCATABLE, DIMENSION(:,:)   :: P,      HSEG,   QTOT
  REAL,          ALLOCATABLE, DIMENSION(:,:)   :: CPB,    COUT,   CWDO,   CDWDO, KFJW
  REAL(R8),      ALLOCATABLE, DIMENSION(:,:)   :: C2I,    EPICI
  REAL(R8),      ALLOCATABLE, DIMENSION(:,:)   :: QTRF
  REAL,          ALLOCATABLE, DIMENSION(:,:)   :: SNPD,   SCRD,   PRFD,   SPRD,   CPLD,   VPLD,   FLXD
  REAL,          ALLOCATABLE, DIMENSION(:,:)   :: SNPF,   SCRF,   PRFF,   SPRF,   CPLF,   VPLF,   FLXF
  REAL,          ALLOCATABLE, DIMENSION(:,:)   :: TVP,    SEDVP,  QINF
  REAL(R8),      ALLOCATABLE, DIMENSION(:,:)   :: TSSUH1, TSSDH1
  REAL(R8),      ALLOCATABLE, DIMENSION(:,:,:) :: CSSUH1, CSSDH1
  REAL,          ALLOCATABLE, DIMENSION(:,:,:) :: EPIVP,  CVP
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: VOLB
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: DLVOL,  VOLG
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: A,      C,      D,      F,      V,      BTA,    GMA,    BHRHO
  REAL(R8),      ALLOCATABLE, DIMENSION(:)     :: DLVR,   ESR,    ETR
  REAL,          ALLOCATABLE, DIMENSION(:,:)   :: CMBRS
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: KTUGT,  KBUGT,  KTDGT,  KBDGT
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: KTUSP,  KBUSP,  KTDSP,  KBDSP
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: KTUPI,  KBUPI,  KTDPI,  KBDPI
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: NSNP,   NSCR,   NSPR,   NVPL,   NFLX,   NCPL,   BTH
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: VPR,    LPR,    NIPRF,  NISPR,  NPRF
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: NISNP
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: NBL,    KBMAX,  KBI
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: KBR,    IBPR
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: TSR
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: NPOINT, NL,     KTQIN,  KBQIN, ilayer    ! SW 1/23/06
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: ITR,    KTTR,   KBTR,   JBTR
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: IWD,    KWD,    JBWD
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: IWDO,   ITSR
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: ILAT,   JBDAM,  JSS
  INTEGER,       ALLOCATABLE, DIMENSION(:)     :: ICPL                                      ! cb 1/26/09
  INTEGER,       ALLOCATABLE, DIMENSION(:,:)   :: KTSWT,  KBSWT
  INTEGER,       ALLOCATABLE, DIMENSION(:,:)   :: IPRF,   ISPR,   ISNP,   BL,     WDO,    CDN, WDO2
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: ALLOW_ICE,           PUMPON,        FETCH_CALC  ! ICE_IN,     RC/SW 4/28/11
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: DN_HEAD,        HEAD_BOUNDARY
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: PLACE_QIN,      PLACE_QTR,      SPECIFY_QTR
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: OPEN_VPR,       OPEN_LPR
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: ISO_TEMP,       VERT_TEMP,      LONG_TEMP,     VERT_PROFILE,  LONG_PROFILE
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: SEDIMENT_CALC,  DETAILED_ICE,   IMPLICIT_VISC, SNAPSHOT,      PROFILE
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: VECTOR,         CONTOUR,        SPREADSHEET,   SCREEN_OUTPUT
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: FLUX,           EVAPORATION,    ZERO_SLOPE
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: ISO_SEDIMENT,   VERT_SEDIMENT,  LONG_SEDIMENT
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: VOLUME_BALANCE, ENERGY_BALANCE, MASS_BALANCE, BOD_CALC, ALG_CALC
  LOGICAL,       ALLOCATABLE, DIMENSION(:)     :: BOD_CALCP,BOD_CALCN                                                  ! cb 5/19/2011
  LOGICAL,       ALLOCATABLE, DIMENSION(:,:)   :: ISO_EPIPHYTON,  VERT_EPIPHYTON, LONG_EPIPHYTON, EPIPHYTON_CALC
  LOGICAL,       ALLOCATABLE, DIMENSION(:,:)   :: ISO_CONC,       VERT_CONC,      LONG_CONC,      TDG_SPILLWAY,   TDG_GATE
  CHARACTER(4),  ALLOCATABLE, DIMENSION(:)     :: CUNIT1
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: SEG,    SEDRC,  TECPLOT
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: HPLTC,  CPLTC,  CDPLTC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: EXC,    EXIC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: GASGTC, GASSPC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: CWDOC,  CDWDOC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: ICEC,   SEDCc,  SEDPRC, SNPC,   SCRC,   SPRC,   PRFC,DYNSEDK
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: RHEVC,  VPLC,   CPLC,   AZSLC,  FETCHC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: DTRC,   SROC,   KFAC,   CDAC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: INCAC,  TRCAC,  DTCAC,  PRCAC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: WTYPEC, GRIDC                                                        !SW 07/16/04
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: PUSPC,  PDSPC,  PUGTC,  PDGTC,  PDPIC,  PUPIC,  PPUC,   TRC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: SLICEC, FLXC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: VBC,    MBC,    EBC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: PQC,    EVC,    PRC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: QINC,   QOUTC,  WINDC,  HEATC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: VISC,   CELC, DLTADD
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: SLTRC,  SLHTC,  FRICC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:)     :: QINIC,  TRIC,   DTRIC,  WDIC,   HDIC,   METIC, KFNAME2
  CHARACTER(10), ALLOCATABLE, DIMENSION(:)     :: C2CH,   CDCH,   EPCH,   macch, KFCH
  CHARACTER(45), ALLOCATABLE, DIMENSION(:)     :: KFNAME
  CHARACTER(72), ALLOCATABLE, DIMENSION(:)     :: SNPFN,  PRFFN,  VPLFN,  CPLFN,  SPRFN,  FLXFN,  FLXFN2, BTHFN,  VPRFN,  LPRFN
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:,:)   :: SINKC,  SINKCT
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:,:)   :: CPRBRC, CDTBRC, CPRWBC, CINBRC, CTRTRC, HPRWBC, STRIC,  CDWBC,  KFWBC
  CHARACTER(8),  ALLOCATABLE, DIMENSION(:,:)   :: EPIC,   EPIPRC
  CHARACTER(10), ALLOCATABLE, DIMENSION(:,:)   :: CONV1
  CHARACTER(72), PARAMETER                     :: CONFN='w2_con.npt'
  CHARACTER(72)                                :: TEXT

! Data declarations
  REAL(R8)  :: RK1, RL1, RIMT, RHOA, RHOI, VTOL, CP, THRKTI
  DATA RK1   /2.12D0/,         RL1    /333507.0D0/, RIMT /0.0D0/, RHOA /1.25D0/, RHOI /916.0D0/, VTOL /1.0D3/, CP /4186.0D0/
  DATA ICE_TOL /0.0050D0/  !0.005
  DATA BLANK /'          '/, BLANK1 /'    -99.00'/
  DATA CON   /10/,  RSI /11/
  DATA THRKTI /0.10D0/  

END MODULE MAIN
MODULE BIOENERGETICS            
  INTEGER,           ALLOCATABLE, DIMENSION(:)       :: IBIO,BIODP,BIOEXPFN,WEIGHTNUM
  REAL,              ALLOCATABLE, DIMENSION(:)       :: BIOD, BIOF,VOLROOS
  REAL,              ALLOCATABLE, DIMENSION(:,:)     :: C2W
  REAL,              ALLOCATABLE, DIMENSION(:,:,:)   :: C2ZOO
  CHARACTER*8 BIOC      
  LOGICAL BIOEXP,FISHBIO   
  INTEGER NBIO,NIBIO,KLIM
  REAL*8 NXBIO,NXTBIO,GAMMAB
  CHARACTER*72 BIOFN,WEIGHTFN
  CHARACTER*8 BHEAD(20)
END MODULE BIOENERGETICS