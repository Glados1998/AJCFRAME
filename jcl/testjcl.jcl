//API4DB2 JOB (ACCT#),'COMPDB2',MSGCLASS=H,REGION=4M,
//        CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//        COND=(4,LT),TIME=(0,5)
//*
//*------------------------------------------------------*
//* ===> CHANGER XX PAR N° DU GROUPE   (XX 01 @ 15)      *
//*      CHANGER     API15DB$ PAR LE NOM DU PROGRAMME    *
//*------------------------------------------------------*
//*
//*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
//*   CETTE PROCEDURE CONTIENT 5 STEPS :                             *
//*       ======> SI RE-EXECUTION FAIRE RESTART AU "STEPRUN"         *
//*                                                                  *
//*         1/  PRECOMPILE  DB2                                      *
//*         2/  COMPILE COBOL II                                     *
//*         3/  LINKEDIT  (DANS FORM.CICS.LOAD)                      *
//*         4/  BIND PLAN PARTIR DE API15.SOURCE.DBRMLIB             *
//*         5/  EXECUTE DU PROGRAMME                                 *
//*  LES   PROCEDURES  SE TROUVENT DANS SDJ.FORM.PROCLIB             *
//*=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-*
// JCLLIB ORDER=SDJ.FORM.PROCLIB
//*        MAINDRV = cobol driver/switchesboard for the subprograms
//         SET USERID=API4,NOMPGM=MAINDRV
//*
//APPROCDRV EXEC COMPDB2
//STEPDB2.SYSIN    DD DSN=&USERID..PROJET.COBOL(&NOMPGM),DISP=SHR
//STEPLNK.SYSLMOD  DD DSN=&USERID..PROJET.LOAD(&NOMPGM),DISP=SHR
//*-- Compile called subprogram 1 (change name as needed) ----------*
//*        SUBPROCA = subprogram A of MAINDRV
//         SET NOMPGM=SUBPROCA
//APPROCA  EXEC COMPDB2
//STEPDB2.SYSLIB   DD DSN=&USERID..PROJET.DCLGEN,DISP=SHR
//STEPDB2.SYSIN    DD DSN=&USERID..PROJET.DB2(&NOMPGM),DISP=SHR
//STEPDB2.DBRMLIB  DD DSN=&USERID..PROJET.DBRMLIB(&NOMPGM),DISP=SHR
//STEPLNK.SYSLMOD  DD DSN=&USERID..PROJET.LOAD(&NOMPGM),DISP=SHR
//*-- Compile called subprogram 2 (change name as needed) ----------*
//*        SUBPROCB = subprogram B of MAINDRV
//         SET NOMPGM=SUBPROCB
//APPROCB  EXEC COMPDB2
//STEPDB2.SYSLIB   DD DSN=&USERID..PROJET.DCLGEN,DISP=SHR
//STEPDB2.SYSIN    DD DSN=&USERID..PROJET.DB2(&NOMPGM),DISP=SHR
//STEPDB2.DBRMLIB  DD DSN=&USERID..PROJET.DBRMLIB(&NOMPGM),DISP=SHR
//STEPLNK.SYSLMOD  DD DSN=&USERID..PROJET.LOAD(&NOMPGM),DISP=SHR
//*-- Compile called subprogram 3 (change name as needed) ----------*
//*        SUBPROCC = subprogram C of MAINDRV                 
//         SET NOMPGM=SUBPROCC
//APPROCC  EXEC COMPDB2
//STEPDB2.SYSLIB   DD DSN=&USERID..PROJET.DCLGEN,DISP=SHR
//STEPDB2.SYSIN    DD DSN=&USERID..PROJET.DB2(&NOMPGM),DISP=SHR
//STEPDB2.DBRMLIB  DD DSN=&USERID..PROJET.DBRMLIB(&NOMPGM),DISP=SHR
//STEPLNK.SYSLMOD  DD DSN=&USERID..PROJET.LOAD(&NOMPGM),DISP=SHR
//*--- BIND PLAN INCLUDING ALL DBRM MEMBERS WITH SQL ---------------*
//BIND     EXEC PGM=IKJEFT01,COND=(4,LT)
//DBRMLIB  DD  DSN=&USERID..PROJET.DBRMLIB,DISP=SHR
//SYSTSPRT DD  SYSOUT=*,OUTLIM=25000
//SYSTSIN  DD  *
  DSN SYSTEM (DSN1)
  BIND PLAN      (MAINDRV) -
       QUALIFIER (API4)    -
       ACTION    (REPLACE) -
       MEMBER    (SUBPROCA) -
       VALIDATE  (BIND)    -
       ISOLATION (CS)      -
       ACQUIRE   (USE)     -
       RELEASE   (COMMIT)  -
       EXPLAIN   (NO)
/*
//*--- RUN DRIVER; SELECT SUBPROC VIA PARM -------------------------*
//STEPRUN  EXEC PGM=IKJEFT01,COND=(4,LT)
//STEPLIB  DD  DSN=&USERID..PROJET.LOAD,DISP=SHR
//SYSOUT   DD  SYSOUT=*,OUTLIM=1000
//SYSTSPRT DD  SYSOUT=*,OUTLIM=2500
//DDOUT1   DD  DSN=API4.PROJET.STATS.DATA,DISP=MOD
//DDOUT2   DD  DSN=API4.PROJET.JOURNALISATION.DATA,DISP=MOD
//SYSTSIN  DD  *
  DSN SYSTEM (DSN1)
  RUN PROGRAM(MAINDRV) PLAN(MAINDRV) PARM('SUBPROCA')
/*
