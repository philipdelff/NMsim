$PROBLEM    018 with 2 comp
$INPUT ROW ID TIME EVID CMT AMT DV MDV BBW DOSE

$DATA ./NMsimData_xgxr021_sd1_NMreadSim.csv
IGN=@

$SUBROUTINE ADVAN4 TRANS4
$PK
TVKA=THETA(1)
TVV2=THETA(2)
TVCL=THETA(3)
TVV3=THETA(4)
TVQ=THETA(5)
                   
KA=TVKA*EXP(ETA(1))
V2=TVV2*EXP(ETA(2))
CL=TVCL*EXP(ETA(3))
V3=TVV3*EXP(ETA(4))
Q=TVQ*EXP(ETA(5))
S2=V2

$ERROR
  IPRED=F
  IRES=DV-IPRED

  IF (IPRED.GT.1) THEN
    W = SQRT(IPRED**2*SIGMA(1,1) + SIGMA(2,2))
  ELSE
    W=1
  ENDIF

  IWRES=IRES/W
  Y=F+F*ERR(1)+ERR(2)

;-----------------------INITIAL ESTIMATES---------------------------------
$THETA  (0,2.16656) ; POPKA
$THETA  (0,75.729) ; POPV2
$THETA  (0,13.9777) ; POPCL
$THETA  (0,150.059) ; POPV3
$THETA  (0,8.4865) ; TVQ
$OMEGA  0  FIX
$OMEGA  0.178666
$OMEGA  0.249778
$OMEGA  0  FIX
$OMEGA  0  FIX
$SIGMA  0.0822435
$SIGMA  0  FIX




$SIMULATION ONLYSIM (2342) 

$TABLE      ROW TVKA TVV2 TVV3 TVCL KA V2 V3 CL Q PRED IPRED Y NOPRINT
            FILE=NMsim_xgxr021_sd1_NMreadSim.tab
