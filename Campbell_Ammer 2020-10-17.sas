proc datasets lib=work kill nolist memtype=data;
quit;

libname RA 'C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3'; run;
libname RA1 'C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-1'; run;






proc datasets lib=work kill nolist memtype=data;
quit;

libname CF 'C:\Users\KUTHURU\Downloads\Laptop\Semester 3\Corporate Finance Slava\Homework-3'; run;



%let wrds=wrds-cloud.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username=adarshkp password="{SAS002}17CB392C3171814A0F4275CF33F4601116D0FB3207889EC521ABAA62";              
                                         
libname rwork slibref = work server = wrds; run;

rsubmit;
options nocenter nodate nonumber ls=max ps=max msglevel=i; 
libname mf '/wrds/crsp/sasdata/q_mutualfunds'; run; *refers to crsp;
libname crspa '/wrds/crsp/sasdata/a_stock'; run; *refers to crsp;
libname ff '/wrds/ff/sasdata'; run;
libname s12 '/wrds/tfn/sasdata/s12'; run; *refers to tfn;
libname mfl '/wrds/mfl/sasdata'; run;
libname a_ccm '/wrds/crsp/sasdata/a_ccm'; run; *refers to crsp;
libname compa '/wrds/comp/sasdata/naa'; run; *refers to compa;
libname compg '/wrds/comp/sasdata/d_global'; run; *refers to compg;
libname pn '/wrds/comp/sasdata/naa/pension'; run; *compustat pension;
libname temp '/scratch/bc/Adarsh'; run;

endrsubmit;


































































options mcompilenote=ALL;
options SYMBOLGEN MPRINT MLOGIC;

%macro doit1;
%do j=1994 %to 2010;

data sample;
	set comp_extract2;
	if calyear=&j+1;
run;

%ps_matching(data=sample,y=TobinQ1,w=target,x=tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1,M=1,Link=logit,L=2,Lt=1);

data closestjmt;
	set closestjmt;
	calyear=&j+1;
run;

data closestjmc;
	set closestjmc;
	calyear=&j+1;
run;

proc append data=closestjmt base=treated_final force; run;
proc append data=closestjmc base=control_final force; run;

%end;
%mend doit1;
%doit1

%let _edtm=%sysfunc(datetime());
%let _runtm=%sysfunc(putn(&_edtm - &_sdtm, 12.4));
%put It took &_runtm second to run the program;



/* Step 2.6. Include fundamentals data in matched data */

proc sql;
	create table treated_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1t, b.tobinq_2 as tobinq_2t, b.tobinq_3 as tobinq_3t,
	b.tobinq_4 as tobinq_4t, b.tobinq_5 as tobinq_5t, b.tobinq1 as tobinq1t, b.tobinq2 as tobinq2t, 
	b.tobinq3 as tobinq3t, b.tobinq4 as tobinq4t, b.tobinq5 as tobinq5t, b.Lnmarketvalue_1 as nmarketvalue_1t,
	b.Leverage_1 as Leverage_1t, b.roa_1 as roa_1t, b.Lnsize_1 as Lnsize_1t, b.capx_1 as capx_1t,
	b.rd_1 as rd_1t, b.Intangibility_1 as Intangibility_1t
	from treated_final as a left join comp_extract2 as b
	on a.gvkeyt=b.gvkey and a.calyear=b.calyear;
quit;

proc sql;
	create table treated_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1c, b.tobinq_2 as tobinq_2c, b.tobinq_3 as tobinq_3c,
	b.tobinq_4 as tobinq_4c, b.tobinq_5 as tobinq_5c, b.tobinq1 as tobinq1c, b.tobinq2 as tobinq2c, 
	b.tobinq3 as tobinq3c, b.tobinq4 as tobinq4c, b.tobinq5 as tobinq5c, b.Lnmarketvalue_1 as nmarketvalue_1c,
	b.Leverage_1 as Leverage_1c, b.roa_1 as roa_1c, b.Lnsize_1 as Lnsize_1c, b.capx_1 as capx_1c,
	b.rd_1 as rd_1c, b.Intangibility_1 as Intangibility_1c
	from treated_final1 as a left join comp_extract2 as b
	on a.gvkeyc=b.gvkey and a.calyear=b.calyear;
quit;


proc sql;
	create table control_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1t, b.tobinq_2 as tobinq_2t, b.tobinq_3 as tobinq_3t,
	b.tobinq_4 as tobinq_4t, b.tobinq_5 as tobinq_5t, b.tobinq1 as tobinq1t, b.tobinq2 as tobinq2t, 
	b.tobinq3 as tobinq3t, b.tobinq4 as tobinq4t, b.tobinq5 as tobinq5t, b.Lnmarketvalue_1 as nmarketvalue_1t,
	b.Leverage_1 as Leverage_1t, b.roa_1 as roa_1t, b.Lnsize_1 as Lnsize_1t, b.capx_1 as capx_1t,
	b.rd_1 as rd_1t, b.Intangibility_1 as Intangibility_1t
	from control_final as a left join comp_extract2 as b
	on a.gvkeyt=b.gvkey and a.calyear=b.calyear;
quit;

proc sql;
	create table control_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1c, b.tobinq_2 as tobinq_2c, b.tobinq_3 as tobinq_3c,
	b.tobinq_4 as tobinq_4c, b.tobinq_5 as tobinq_5c, b.tobinq1 as tobinq1c, b.tobinq2 as tobinq2c, 
	b.tobinq3 as tobinq3c, b.tobinq4 as tobinq4c, b.tobinq5 as tobinq5c, b.Lnmarketvalue_1 as nmarketvalue_1c,
	b.Leverage_1 as Leverage_1c, b.roa_1 as roa_1c, b.Lnsize_1 as Lnsize_1c, b.capx_1 as capx_1c,
	b.rd_1 as rd_1c, b.Intangibility_1 as Intangibility_1c
	from control_final1 as a left join comp_extract2 as b
	on a.gvkeyc=b.gvkey and a.calyear=b.calyear;
quit;


/* Step 2.7. Table4A results */

data treated_final1;
	set treated_final1;
	tobinq_1=sum(tobinq_1t,-tobinq_1c);
	tobinq_2=sum(tobinq_2t,-tobinq_2c);
	tobinq_3=sum(tobinq_3t,-tobinq_3c);
	tobinq_4=sum(tobinq_4t,-tobinq_4c);
	tobinq_5=sum(tobinq_5t,-tobinq_5c);
	Lnmarketvalue_1=sum(nmarketvalue_1t,-nmarketvalue_1c);
	Leverage_1=sum(Leverage_1t,-Leverage_1c);
	roa_1=sum(roa_1t,-roa_1c);
	Lnsize_1=sum(Lnsize_1t,-Lnsize_1c);
	capx_1=sum(capx_1t,-capx_1c);
	rd_1=sum(rd_1t,-rd_1c);
	Intangibility_1=sum(Intangibility_1t,-Intangibility_1c);
run;

*means;
proc means data=treated_final1 noprint;
  by target;
  var tobinq_1t tobinq_2t tobinq_3t tobinq_4t tobinq_5t nmarketvalue_1t Leverage_1t roa_1t Lnsize_1t capx_1t rd_1t Intangibility_1t
tobinq_1c tobinq_2c tobinq_3c tobinq_4c tobinq_5c nmarketvalue_1c Leverage_1c roa_1c Lnsize_1c capx_1c rd_1c Intangibility_1c
tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1;
  output out=table4a mean=tobinq_1t tobinq_2t tobinq_3t tobinq_4t tobinq_5t nmarketvalue_1t Leverage_1t roa_1t Lnsize_1t capx_1t rd_1t Intangibility_1t
tobinq_1c tobinq_2c tobinq_3c tobinq_4c tobinq_5c nmarketvalue_1c Leverage_1c roa_1c Lnsize_1c capx_1c rd_1c Intangibility_1c
tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1;
quit;

*p-value;
proc means data=treated_final1 noprint;
  by target;
  var tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1;
  output out=table4b prt=tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1;
quit;

*Export csv;
proc export data=table4a outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\Corporate Finance Slava\Homework-3\Table4a.csv" 
dbms=csv replace;
run;

proc export data=table4b outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\Corporate Finance Slava\Homework-3\Table4b.csv" 
dbms=csv replace;
run;

proc export data=treated_final1 outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\Corporate Finance Slava\Homework-3\fig1.csv" 
dbms=csv replace;
run;


/*********************************************************/
/*Step 3. Extension: improve sales to fend off activists */
/*********************************************************/
*here, sales variables enters covariates in matching;


%let _sdtm=%sysfunc(datetime());

options mcompilenote=ALL;
options SYMBOLGEN MPRINT MLOGIC;

%macro doit1;
%do j=1994 %to 2010;

data sample;
	set comp_extract2;
	if calyear=&j+1;
run;

%ps_matching(data=sample,y=TobinQ1,w=target,x=tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 Lnsale_1 capx_1 rd_1 Intangibility_1,M=1,Link=logit,L=2,Lt=1);

data closestjmt;
	set closestjmt;
	calyear=&j+1;
run;

data closestjmc;
	set closestjmc;
	calyear=&j+1;
run;

proc append data=closestjmt base=treated_final force; run;
proc append data=closestjmc base=control_final force; run;

%end;
%mend doit1;
%doit1

%let _edtm=%sysfunc(datetime());
%let _runtm=%sysfunc(putn(&_edtm - &_sdtm, 12.4));
%put It took &_runtm second to run the program;



/* Step 3.1. Include fundamentals data in matched data */

proc sql;
	create table treated_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1t, b.tobinq_2 as tobinq_2t, b.tobinq_3 as tobinq_3t,
	b.tobinq_4 as tobinq_4t, b.tobinq_5 as tobinq_5t, b.tobinq1 as tobinq1t, b.tobinq2 as tobinq2t, 
	b.tobinq3 as tobinq3t, b.tobinq4 as tobinq4t, b.tobinq5 as tobinq5t, b.Lnmarketvalue_1 as Lnmarketvalue_1t,
	b.Leverage_1 as Leverage_1t, b.roa_1 as roa_1t, b.Lnsize_1 as Lnsize_1t, b.capx_1 as capx_1t,
	b.rd_1 as rd_1t, b.Intangibility_1 as Intangibility_1t, b.Lnsale_1 as Lnsale_1t
	from treated_final as a left join comp_extract2 as b
	on a.gvkeyt=b.gvkey and a.calyear=b.calyear;
quit;

proc sql;
	create table treated_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1c, b.tobinq_2 as tobinq_2c, b.tobinq_3 as tobinq_3c,
	b.tobinq_4 as tobinq_4c, b.tobinq_5 as tobinq_5c, b.tobinq1 as tobinq1c, b.tobinq2 as tobinq2c, 
	b.tobinq3 as tobinq3c, b.tobinq4 as tobinq4c, b.tobinq5 as tobinq5c, b.Lnmarketvalue_1 as Lnmarketvalue_1c,
	b.Leverage_1 as Leverage_1c, b.roa_1 as roa_1c, b.Lnsize_1 as Lnsize_1c, b.capx_1 as capx_1c,
	b.rd_1 as rd_1c, b.Intangibility_1 as Intangibility_1c, b.Lnsale_1 as Lnsale_1c
	from treated_final1 as a left join comp_extract2 as b
	on a.gvkeyc=b.gvkey and a.calyear=b.calyear;
quit;


proc sql;
	create table control_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1t, b.tobinq_2 as tobinq_2t, b.tobinq_3 as tobinq_3t,
	b.tobinq_4 as tobinq_4t, b.tobinq_5 as tobinq_5t, b.tobinq1 as tobinq1t, b.tobinq2 as tobinq2t, 
	b.tobinq3 as tobinq3t, b.tobinq4 as tobinq4t, b.tobinq5 as tobinq5t, b.Lnmarketvalue_1 as Lnmarketvalue_1t,
	b.Leverage_1 as Leverage_1t, b.roa_1 as roa_1t, b.Lnsize_1 as Lnsize_1t, b.capx_1 as capx_1t,
	b.rd_1 as rd_1t, b.Intangibility_1 as Intangibility_1t, b.Lnsale_1 as Lnsale_1t
	from control_final as a left join comp_extract2 as b
	on a.gvkeyt=b.gvkey and a.calyear=b.calyear;
quit;

proc sql;
	create table control_final1 as
	select distinct a.*, b.tobinq_1 as tobinq_1c, b.tobinq_2 as tobinq_2c, b.tobinq_3 as tobinq_3c,
	b.tobinq_4 as tobinq_4c, b.tobinq_5 as tobinq_5c, b.tobinq1 as tobinq1c, b.tobinq2 as tobinq2c, 
	b.tobinq3 as tobinq3c, b.tobinq4 as tobinq4c, b.tobinq5 as tobinq5c, b.Lnmarketvalue_1 as Lnmarketvalue_1c,
	b.Leverage_1 as Leverage_1c, b.roa_1 as roa_1c, b.Lnsize_1 as Lnsize_1c, b.capx_1 as capx_1c,
	b.rd_1 as rd_1c, b.Intangibility_1 as Intangibility_1c, b.Lnsale_1 as Lnsale_1c
	from control_final1 as a left join comp_extract2 as b
	on a.gvkeyc=b.gvkey and a.calyear=b.calyear;
quit;


/* Step 3.2. Extension table results */

data treated_final1;
	set treated_final1;
	tobinq_1=sum(tobinq_1t,-tobinq_1c);
	tobinq_2=sum(tobinq_2t,-tobinq_2c);
	tobinq_3=sum(tobinq_3t,-tobinq_3c);
	tobinq_4=sum(tobinq_4t,-tobinq_4c);
	tobinq_5=sum(tobinq_5t,-tobinq_5c);
	Lnmarketvalue_1=sum(Lnmarketvalue_1t,-Lnmarketvalue_1c);
	Leverage_1=sum(Leverage_1t,-Leverage_1c);
	roa_1=sum(roa_1t,-roa_1c);
	Lnsize_1=sum(Lnsize_1t,-Lnsize_1c);
	capx_1=sum(capx_1t,-capx_1c);
	rd_1=sum(rd_1t,-rd_1c);
	Intangibility_1=sum(Intangibility_1t,-Intangibility_1c);
	Lnsale_1=sum(Lnsale_1t,-Lnsale_1c);
run;

*means;
proc means data=treated_final1 noprint;
  by target;
  var tobinq_1t tobinq_2t tobinq_3t tobinq_4t tobinq_5t Lnmarketvalue_1t Leverage_1t roa_1t Lnsize_1t capx_1t rd_1t Intangibility_1t Lnsale_1t
tobinq_1c tobinq_2c tobinq_3c tobinq_4c tobinq_5c Lnmarketvalue_1c Leverage_1c roa_1c Lnsize_1c capx_1c rd_1c Intangibility_1c Lnsale_1c
tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1 Lnsale_1;
  output out=table4a mean=tobinq_1t tobinq_2t tobinq_3t tobinq_4t tobinq_5t Lnmarketvalue_1t Leverage_1t roa_1t Lnsize_1t capx_1t rd_1t Intangibility_1t Lnsale_1t
tobinq_1c tobinq_2c tobinq_3c tobinq_4c tobinq_5c Lnmarketvalue_1c Leverage_1c roa_1c Lnsize_1c capx_1c rd_1c Intangibility_1c Lnsale_1c
tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1 Lnsale_1;
quit;

*p-value;
proc means data=treated_final1 noprint;
  by target;
  var tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1 Lnsale_1;
  output out=table4b prt=tobinq_1 tobinq_2 tobinq_3 tobinq_4 tobinq_5 Lnmarketvalue_1 Leverage_1 roa_1 Lnsize_1 capx_1 rd_1 Intangibility_1 Lnsale_1;
quit;

*Export csv;
proc export data=table4a outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\Corporate Finance Slava\Homework-3\Table4a.csv" 
dbms=csv replace;
run;

proc export data=table4b outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\Corporate Finance Slava\Homework-3\Table4b.csv" 
dbms=csv replace;
run;


proc export data=treated_final1 outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\Corporate Finance Slava\Homework-3\fig2.csv" 
dbms=csv replace;
run;














/*************************************************************************************/
/*                  Step 1. select all SEC filings from ra.filings                   */
/*************************************************************************************/


proc sort data=ra1.filings; by date coname formtype; run;
*1,919,271 obs;

*include only 10-k related filings;
proc sql;
    create table a_10k as
    select distinct *
    from ra1.filings 
    where formtype IN ("10-K", "10-K/A", "10-K405", "10-K405/A", 
        "10-KSB", "10-KSB/A", "10-KT", "10-KT/A", "10KSB", 
        "10KSB/A", "10KSB40", "10KSB40/A", "10KT405", 
        "10KT405/A");
quit;
*310405 obs;

*prepend the html path;
DATA a_10k;
        SET a_10k;
		path='https'||':'||'//'||'www'||'.'||'sec'||'.'||'gov'||'/'||'Archives'||'/'||TRIM(filename);
		filename1=SCAN(filename,-1,'/');
        n=_N_;;
RUN;
DATA a_10k;
        SET a_10k;
		path=TRIM(path);
		filename2='C'||':'||'/'||'Users'||'/'||'KUTHURU'||'/Downloads'||'/'||'Laptop'||'/'||'Semester 3'||'/'||'RA work'||'/'||'Iteration-3'||'/'||'Downloaded Files'||'/'||TRIM(filename1);
RUN;

*Group number of firms by year;
proc sql;
	create table year as
	select distinct year(date) as year, count(distinct cik) as nfirm
	from a_10k
	group by year(date);
quit;

**Generate macdonald_header3 file with code from Iteration-2 folder and merge with a_10k;
proc sql;
	create table a_10k1 as
	select distinct a.*, b.*
	from a_10k as a, macdonald_header3 as b
	where a.cik=b.cik and a.date=b.newdate;
quit;
*11398 obs;
proc sort data=a_10k1 nodupkey;by cik date form_type; run;
*11193 obs;


*********************************************************************************
		Create random sample of 500 obs and download files
*********************************************************************************;
*create a random sample which contains 500 of the firms from full sample; 
PROC SURVEYSELECT DATA=a_10k1 noprint OUT=random METHOD=SRS
SAMPSIZE=500 SEED=11193;
RUN;
*500 obs;

Data random;
        Set random;
		n=_N_;
run;

/* Store the number of files in a macro variable "num" */
proc sql noprint;
        select count(*) into :num from random;
quit;

**instead of printing log, it saves log file at mentioned location;
proc printto log="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\log.log";
run;

/* Create a macro to iterate over the filenames and download txt files from EDGAR.*/
options mcompilenote=ALL;
options SYMBOLGEN MPRINT MLOGIC;
%LET k=1;
%macro doit;
    %do j=1 %to &num;

        proc sql noprint;
            select path into :path from random where n=&j;
        quit;

		proc sql noprint;
            select filename1 into :filename1 from random where n=&j;
        quit;

**Downloads text file from web;
filename out "C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\Downloaded Files\&filename1";
 
proc http
 url="%trim(&path)" /*trims trailing blanks after url link */
 method="get" out=out;
run;

%end;
%mend doit;
%doit

*re-enabling the log;
PROC PRINTTO PRINT=PRINT LOG=LOG ;
RUN;


*Export csv;
proc export data=random outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\Data.csv" 
dbms=csv replace;
run;


*********************************************************************************
		Importing verified sample from R and checking for false positives
*********************************************************************************;
***Import xlsx;
PROC IMPORT OUT= sample
            DATAFILE= "C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\sample.xlsx" 
            DBMS=EXCEL REPLACE;
sheet="sample";
GETNAMES=YES;
MIXED=NO;
SCANTEXT=YES;
USEDATE=YES;
SCANTIME=YES;
RUN;

**Import csv with random sample data generated earlier;
PROC IMPORT OUT= data
            DATAFILE= "C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\Data.csv" 
            DBMS=CSV REPLACE;
RUN;

*Merge the above datasets;
proc sql;
	create table data as
	select distinct a.*,b.*
	from data as a left join sample as b
	on a.cik=b.cik and a.date=b.Date_of_Filing;
quit;

*Merge above dataset with a_10k1;
proc sql;
	create table data as
	select distinct a.*,b.*
	from data as a left join a_10k1 as b
	on a.cik=b.cik and a.date=b.date;
quit;

*Include lag filename in above dataset;
proc sql;
	create table data as
	select distinct a.*,b.filename as filename_1, b.filename1 as filename1_1, b.filename2 as filename2_1
	from data as a left join a_10k as b
	on a.cik=b.cik and a.lag_date=b.date;
quit;
proc sort data=data nodupkey; by cik date; run;

*prepend the html path;
DATA data;
        SET data;
		path_1='https'||':'||'//'||'www'||'.'||'sec'||'.'||'gov'||'/'||'Archives'||'/'||TRIM(filename_1);
		filename1=SCAN(filename,-1,'/');
        n=_N_;;
RUN;
DATA a_10k;
        SET a_10k;
		path_1=TRIM(path_1);
		filename2_1='C'||':'||'/'||'Users'||'/'||'KUTHURU'||'/Downloads'||'/'||'Laptop'||'/'||'Semester 3'||'/'||'RA work'||'/'||'Iteration-3'||'/'||'Downloaded Files'||'/'||TRIM(filename1_1);
RUN;
**Download lag filenames from EDGAR;
Data data;
        Set data;
		n=_N_;
run;

/* Store the number of files in a macro variable "num" */
proc sql noprint;
        select count(*) into :num from data;
quit;

**instead of printing log, it saves log file at mentioned location;
proc printto log="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\log.log";
run;

/* Create a macro to iterate over the filenames and download txt files from EDGAR.*/
options mcompilenote=ALL;
options SYMBOLGEN MPRINT MLOGIC;
%LET k=1;
%macro doit;
    %do j=1 %to &num;

        proc sql noprint;
            select path_1 into :path from data where n=&j;
        quit;

		proc sql noprint;
            select filename1_1 into :filename1 from data where n=&j;
        quit;

**Downloads text file from web;
filename out "C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\Downloaded Files\&filename1";
 
proc http
 url="%trim(&path)" /*trims trailing blanks after url link */
 method="get" out=out;
run;

%end;
%mend doit;
%doit

*re-enabling the log;
PROC PRINTTO PRINT=PRINT LOG=LOG ;
RUN;

*Export csv;
proc export data=data outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\sample_final.csv" 
dbms=csv replace;
run;


*********************************************************************************
		Download files for all 11193 companies;
*********************************************************************************;

Data a_10k1;
        Set a_10k1;
		n=_N_;
run;

/* Store the number of files in a macro variable "num" */
proc sql noprint;
        select count(*) into :num from a_10k1;
quit;

**instead of printing log, it saves log file at mentioned location;
proc printto log="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\log.log";
run;

/* Create a macro to iterate over the filenames and download txt files from EDGAR.*/
options mcompilenote=ALL;
options SYMBOLGEN MPRINT MLOGIC;
%LET k=1;
%macro doit;
    %do j=1 %to &num;

        proc sql noprint;
            select path into :path from a_10k1 where n=&j;
        quit;

		proc sql noprint;
            select filename1 into :filename1 from a_10k1 where n=&j;
        quit;

**Downloads text file from web;
filename out "C:\Users\KUTHURU\Desktop\Downloaded files\&filename1";
 
proc http
 url="%trim(&path)" /*trims trailing blanks after url link */
 method="get" out=out;
run;

%end;
%mend doit;
%doit

*re-enabling the log;
PROC PRINTTO PRINT=PRINT LOG=LOG ;
RUN;

*change directory path;
DATA a_10k1;
        SET a_10k1;
		filename2='C'||':'||'/'||'Users'||'/'||'KUTHURU'||'/Desktop'||'/'||'Downloaded Files'||'/'||TRIM(filename1);
RUN;

*Export csv;
proc export data=a_10k1 outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\data1.csv" 
dbms=csv replace;
run;

*********************************************************************************
		Download files for all 300000 companies that filed 10-k;
*********************************************************************************;

*change directory path;
DATA a_10k;
        SET a_10k;
		filename2='C'||':'||'/'||'Users'||'/'||'KUTHURU'||'/Desktop'||'/'||'Downloaded Files'||'/'||'Full'||'/'||TRIM(filename1);
		n=_N_;
RUN;

/* Store the number of files in a macro variable "num" */
proc sql noprint;
        select count(*) into :num from a_10k;
quit;

**instead of printing log, it saves log file at mentioned location;
proc printto log="C:\Users\KUTHURU\Desktop\Downloaded Files\log.log";
run;

/* Create a macro to iterate over the filenames and download txt files from EDGAR.*/
options mcompilenote=ALL;
options SYMBOLGEN MPRINT MLOGIC;
%LET k=1;
%macro doit;
    %do j=1 %to 20000; *&num;

        proc sql noprint;
            select path into :path from a_10k where n=&j;
        quit;

		proc sql noprint;
            select filename1 into :filename1 from a_10k where n=&j;
        quit;

**Downloads text file from web;
filename out "C:\Users\KUTHURU\Desktop\Downloaded Files\Full\&filename1";
 
proc http
 url="%trim(&path)" /*trims trailing blanks after url link */
 method="get" out=out;
run;

%end;
%mend doit;
%doit

*re-enabling the log;
PROC PRINTTO PRINT=PRINT LOG=LOG ;
RUN;


*Export csv;
proc export data=a_10k outfile="C:\Users\KUTHURU\Downloads\Laptop\Semester 3\RA work\Iteration-3\data2.csv" 
dbms=csv replace;
run;
