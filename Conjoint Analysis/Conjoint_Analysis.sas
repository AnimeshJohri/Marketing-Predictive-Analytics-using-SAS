/*-------------------------------- CONJOINT ANALYSIS ----------*/
LIBNAME cnjt 'H:\Homework3';
/***** DATA IMPORT *****/
TITLE 'Importing Data';
DATA cnjt.HW3;
input brand $ scent $ soft $ oz pr s1 s2 s3 s4 s5;
cards;
complete	fresh	n	48	4.99	1	3	3	2	2
complete	fresh	y	32	2.99	1	3	3	5	5
complete	lemon	n	32	2.99	1	2	7	5	1
complete	lemon	y	64	3.99	1	9	5	8	1
complete	U	n	64	3.99	1	9	7	8	7
complete	U	y	48	4.99	1	3	3	2	3
Smile	fresh	n	64	2.99	1	9	9	9	6
Smile	fresh	y	48	3.99	1	7	7	6	5
Smile	lemon	n	48	3.99	1	7	7	6	1
Smile	lemon	y	32	4.99	1	1	1	1	1
Smile	U	n	32	4.99	1	1	3	1	2
Smile	U	y	64	2.99	1	9	3	9	9
Wave	fresh	n	32	3.99	7	1	7	4	5
Wave	fresh	y	64	4.99	5	5	3	3	2
Wave	lemon	n	64	4.99	5	5	5	3	1
Wave	lemon	y	48	2.99	9	9	5	7	1
Wave	U	n	48	2.99	9	9	5	7	7
Wave	U	y	32	3.99	7	1	5	4	5
Wave	lemon	n	64	2.99	8	9	6	9	3
Smile	lemon	n	32	4.99	2	1	3	2	1
Smile	fresh	y	48	2.99	2	8	4	5	5
complete	U	y	32	2.99	2	4	2	5	6
complete	lemon	y	48	3.99	2	6	6	6	1
;RUN;

PROC PRINT DATA=cnjt.HW3;RUN;

/* Print Contents of the dataset */
PROC CONTENTS DATA=cnjt.HW3; RUN;

/* Define Dummy Variables */
DATA dtrgt; SET cnjt.HW3;
IF brand="complete" THEN cbrand=1 ; ELSE cbrand=0;
IF brand="Smile" THEN sbrand=1 ; ELSE sbrand=0;
IF scent="fresh" THEN fscent=1 ; ELSE fscent=0;
IF scent="lemon" THEN lscent=1 ; ELSE lscent=0;
IF soft="y" THEN nsoft=1 ; ELSE nsoft=0;
IF oz=48 THEN moz=1 ; ELSE moz=0;
IF oz=64 THEN hoz=1 ; ELSE hoz=0;
IF pr=3.99 THEN mprice=1 ; ELSE mprice=0;
IF pr=4.99 THEN hprice=1 ; ELSE hprice=0;
RUN;

PROC PRINT DATA=dtrgt;RUN;

/* Loop Regression for Five Respondents */
%MACRO DTRGT;
%DO i=1 %TO 5;
PROC REG DATA=dtrgt OUTEST=REGOUT&i;
MODEL s&i = cbrand sbrand fscent lscent nsoft moz hoz mprice hprice /STB;
RUN;
%END;
%MEND DTRGT;

/*ods excel file="H:\Homework3\HW3Q3_output.xlsx" options(embedded_titles="yes");*/
%DTRGT;

/* Print the Output - REGOUT */
%MACRO PRINT2;
%DO i=1 %TO 5;
DATA INP&i;SET REGOUT&i;
PROC PRINT DATA=INP&i;
RUN;
%END;
%MEND PRINT2;

%PRINT2;

/* Computation Loop to Achieve Utilities and Part-Worths */
%MACRO Compute;
%DO i=1 %TO 5;
     data COMP&i;
     set INP&i;
wave=-(cbrand+sbrand)/3;
smile=((2*sbrand)-cbrand)/3;
compl=((2*cbrand)-sbrand)/3;
unscented=-(fscent+lscent)/3;
fresh=((2*fscent)-lscent)/3;
lemon=((2*lscent)-fscent)/3;
y=nsoft/2;
n=-nsoft/2;
ls=-(moz+hoz)/3;
ms=((2*moz)-hoz)/3;
hs=((2*hoz)-moz)/3;
lp=-(mprice+hprice)/3;
mp=((2*mprice)-hprice)/3;
hp=((2*hprice)-mprice)/3;
brmaxmin=max(wave,smile,compl)-min(wave,smile,compl);
scmaxmin=max(unscented,fresh,lemon)-min(unscented,fresh,lemon);
somaxmin=max(y,n)-min(y,n);
simaxmin=max(ls,ms,hs)-min(ls,ms,hs);
prmaxmin=max(lp,mp,hp)-min(lp,mp,hp);
total=scmaxmin+somaxmin+simaxmin+prmaxmin+brmaxmin;
ribrand=brmaxmin/total;
riscent=scmaxmin/total;
risoft=somaxmin/total;
risize=simaxmin/total;
riprice=prmaxmin/total;
RUN;
%END;	 
%MEND Compute;

%Compute;

/* Merge Utilities and Part-worths of each respondent into a single dataset - INTERIM */
DATA INTERIM(KEEP=_DEPVAR_ wave smile compl unscented fresh lemon y n ls ms hs lp mp hp brmaxmin scmaxmin somaxmin simaxmin prmaxmin total ribrand riscent risoft risize riprice);
MERGE COMP1 COMP2 COMP3 COMP4 COMP5;
BY _DEPVAR_;RUN;

PROC PRINT DATA=INTERIM;RUN;

/* Transpose INTERIM Dataset */
PROC TRANSPOSE DATA=INTERIM OUT=INTERIM2 NAME=VARIABLES PREFIX=RESP;RUN;

PROC PRINT DATA=INTERIM2;RUN;

/* Prediction Loop for given dataset */
%MACRO Predict;
%DO i=1 %TO 5;
     data RESP&i(KEEP=_DEPVAR_ A B C D E PR_A PR_B PR_C PR_D PR_E);
     set COMP&i;
A=compl+lemon+y+hs+lp;
B=smile+fresh+y+ms+lp;
C=smile+unscented+y+ms+mp;
D=wave+unscented+y+ms+lp;
E=smile+unscented+n+ms+lp;
ETOTAL=exp(A)+exp(B)+exp(C)+exp(D)+exp(E);
PR_A=exp(A)/ETOTAL;
PR_B=exp(B)/ETOTAL;
PR_C=exp(C)/ETOTAL;
PR_D=exp(D)/ETOTAL;
PR_E=exp(E)/ETOTAL;
RUN;
%END;
%MEND Predict;

%Predict;

/* Print output of Prediction Dataset */
%MACRO PRPRINT;
%DO i=1 %TO 5;
PROC PRINT DATA=RESP&i;
RUN;
%END;
%MEND PRPRINT;

%PRPRINT;

/* Transpose Rows to Columns */
%MACRO R2C;
%DO j=1 %TO 5;
PROC TRANSPOSE DATA=RESP&j OUT=TRESP&j NAME=UTILITY PREFIX=RESP&j; 
RUN;
%END;
%MEND R2C;

%R2C;
/* Sort the Dataset By Utility */
%MACRO SORDTR;
%DO j=1 %TO 5;
PROC SORT DATA=TRESP&j OUT=FIN&j;
BY UTILITY;
RUN;
%END;
%MEND SORDTR;

%SORDTR;

/* Merge the all five files into single file */
DATA FINAL;
MERGE FIN1 FIN2 FIN3 FIN4 FIN5;
BY UTILITY;RUN;

PROC PRINT DATA=FINAL;RUN;

/* Print Predicted Values for each Respondent */
DATA FINAL2; SET FINAL(FIRSTOBS=1 OBS=5);
PROC PRINT DATA=FINAL2;RUN;

/* Predict Market Share  */
DATA PREDICT; SET FINAL(firstobs=6);
MS=(RESP11+RESP21+RESP31+RESP41+RESP51)/5; RUN;

PROC PRINT DATA=PREDICT;RUN;

/* TRANSREG Procedure - To check for Respondent 1 */
proc transreg data=dtrgt utilities short outtest=Utils separators='  ';
      ods select FitStatistics Utilities;
      title2 'Individual Conjoint Analyses';
      model identity(s1) =
            class(brand scent soft oz pr / zero=sum);
   run;
/* TRANSREG Procedure - To check for Respondent 2 */
   proc transreg data=dtrgt utilities short outtest=Utils separators='  ';
      ods select FitStatistics Utilities;
      title2 'Individual Conjoint Analyses';
      model identity(s2) =
            class(brand scent soft oz pr / zero=sum);
   run;
/* TRANSREG Procedure - To check for Respondent 3 */
   proc transreg data=dtrgt utilities short outtest=Utils separators='  ';
      ods select FitStatistics Utilities;
      title2 'Individual Conjoint Analyses';
      model identity(s3) =
            class(brand scent soft oz pr / zero=sum);
   run;
/* TRANSREG Procedure - To check for Respondent 4 */
   proc transreg data=dtrgt utilities short outtest=Utils separators='  ';
      ods select FitStatistics Utilities;
      title2 'Individual Conjoint Analyses';
      model identity(s4) =
            class(brand scent soft oz pr / zero=sum);
   run;
/* TRANSREG Procedure - To check for Respondent 5 */
   proc transreg data=dtrgt utilities short outtest=Utils separators='  ';
      ods select FitStatistics Utilities;
      title2 'Individual Conjoint Analyses';
      model identity(s5) =
            class(brand scent soft oz pr / zero=sum);
   run;
/* TRANSREG Procedure - To check for complete dataset */
   proc transreg data=dtrgt utilities short outtest=Utils separators='  ';
      ods select FitStatistics Utilities;
      title2 'Individual Conjoint Analyses';
      model identity(s1-s5) =
            class(brand scent soft oz pr / zero=sum);
   run;
