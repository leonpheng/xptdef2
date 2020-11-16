

$PROBLEM Example NM-TRAN Control Stream for a 2-cmt Model
$DATA  ../ODON_POPPK_31OCT20.csv IGNORE=#
$INPUT  ID DV LNDV AMT EVID MDV TIME FORM FAST AGE SEX RACE COUNTRY BHT BWT BBSA BBMI BALB ALB BALP ALP 
		BALT ALT BAST AST BBILI BILI BCREAT CREAT BCRCL CRCL BEGFR EGFR TULOC TUTYPE ECOG 
		PPI CYPINH CYPIND BHEPIMP HEPIMP BRIMPC RIMPC BRIMPE RIMPE  
		
$SUBROUTINES ADVAN12 TRANS4
$PK

TVCL = THETA(1)
CL = TVCL * EXP(ETA(1))
TVV = THETA(2)
V2 = TVV * EXP(ETA(2))
TVQ3 = THETA(3)
Q3 = TVQ3 * EXP(ETA(3))
TVV3 = THETA(4)
V3 = TVV3 * EXP(ETA(4))
TVQ4 = THETA(5)
Q4 = TVQ4 * EXP(ETA(5))
TVV4 = THETA(6)
V4 = TVV4 * EXP(ETA(6))



TVKA = THETA(7)
KA = TVKA* EXP(ETA(7))


; scale predictions based on dose (mg) and Cp (ng/mL)
S2 = V2/1000


$ERROR


Y = F + F * EPS(1) + EPS(2) 


$THETA    (0,10)     ; Clearance (L/h)
          (0,100)    ; V2 (L)
		  (0,20)     ; Q3 (L/h)
          (0,100)    ; V3 (L)
		  (0,200)     ; Q4 (L/h)
          (0,100)    ; V4 (L)
          (0,0.01)   ; Absorption Rate (1/h)


$OMEGA    (0.1)    ; IIV on CL 
          (0.1)    ; IIV on V2 
		  (0.1)    ; IIV on Q3 
          (0.1)    ; IIV on V3 
		  (0.1)    ; IIV on Q4 
          (0.1)    ; IIV on V4 
		   (0.1)    ; IIV on KA 
$SIGMA    (0.4)    ; EPS1
(0.4)    ; EPS2 

; Use conditional estimation with interaction
$ESTIMATION METHOD=1 INTER MAXEVAL=9999 PRINT=1 NOABORT SIG=6 MSFO=cpt3.msf


$TABLE ID AMT ONEHEADER NOPRINT FILE=cpt3.tbl