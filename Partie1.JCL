//API3DBC JOB (ACCT#),'COMPDBT',MSGCLASS=H,REGION=4M,
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//    COND=(4,LT),TIME=(0,30)
//*
//*------------------------------------------------------*
//* ===> CHANGER XX PAR N¢ DU GROUPE   (XX 01 @ 15)      *
//*      CHANGER     API9DB$ PAR LE NOM DU PROGRAMME     *
//*------------------------------------------------------*
//*
//*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
//*   CETTE PROCEDURE CONTIENT 5 STEPS :                             *
//*       ======> SI RE-EXECUTION FAIRE RESTART AU "STEPRUN"         *
//*                                                                  *
//*         1/  PRECOMPILE  DB2                                      *
//*         2/  COMPILE COBOL II                                     *
//*         3/  LINKEDIT  (DANS FORM.CICS.LOAD)                      *
//*         4/  BIND PLAN PARTIR DE API9.SOURCE.DBRMLIB              *
//*         5/  EXECUTE DU PROGRAMME                                 *
//*  LES   PROCEDURES  SE TROUVENT DANS SDJ.FORM.PROCLIB             *
//*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
//PROCLIB  JCLLIB ORDER=SDJ.FORM.PROCLIB
//*
//         SET SYSUID=API3,
//             NOMPGM=PROJET1
//*
//APPROC   EXEC COMPDB2
//STEPDB2.SYSLIB  DD DSN=&SYSUID..SOURCE.DCLGEN,DISP=SHR
//STEPDB2.SYSIN   DD DSN=&SYSUID..SOURCE.DB2(&NOMPGM),DISP=SHR
//STEPDB2.DBRMLIB DD DSN=&SYSUID..COB.DBRM(&NOMPGM),DISP=SHR
//*STEPCOB.SYSLIB  DD DSN=&SYSUID..COB.CPY.,DISP=SHR
//*                DD DSN=API3.COB.CPY,DISP=SHR
//STEPLNK.SYSLMOD DD DSN=&SYSUID..COB.LOAD(&NOMPGM),DISP=SHR
//*
//*--- ETAPE DE BIND --------------------------------------
//*
//BIND     EXEC PGM=IKJEFT01,COND=(4,LT)
//*DBRMLIB  DD  DSN=&SYSUID..DB2.DBRMLIB,DISP=SHR
//DBRMLIB  DD  DSN=&SYSUID..COB.DBRM,DISP=SHR
//SYSTSPRT DD  SYSOUT=*,OUTLIM=25000
//SYSTSIN  DD  *
  DSN SYSTEM (DSN1)
  BIND PLAN      (PROJET1) -
       QUALIFIER (API3)     -
       ACTION    (REPLACE) -
       MEMBER    (PROJET1) -
       VALIDATE  (BIND)    -
       ISOLATION (CS)      -
       ACQUIRE   (USE)     -
       RELEASE   (COMMIT)  -
       EXPLAIN   (NO)
/*
//STEPRUN  EXEC PGM=IKJEFT01,COND=(4,LT)
//STEPLIB  DD  DSN=&SYSUID..COB.LOAD,DISP=SHR
//SYSOUT   DD  SYSOUT=*,OUTLIM=1000
//SYSTSPRT DD  SYSOUT=*,OUTLIM=2500
//SYSTSIN  DD  *
  DSN SYSTEM (DSN1)
  RUN PROGRAM(PROJET1) PLAN (PROJET1)
/*
//NPDATA DD DSN=API3.PROJET.NEWPRODS.DATA,DISP=SHR
//ERRLOG DD DSN=API3.PROJET.NEWPRODS.ERR,DISP=SHR
//CHNGFILE DD DSN=API3.PROJET.NEWPRODS.CHNG.KSDS,DISP=SHR
