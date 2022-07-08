
******************************************************************************;
******************************************************************************;
*** Create table of variables to label mappings.
*** This is necessary for constructing the labels in the output tables.
******************************************************************************;
******************************************************************************;


/* Read in Marcos*/
******************************************************************************;
******************************************************************************;
*** Macro to generate one line of the table for a CATEGORICAL variable.
*** Variant that uses formats to obtain a nice label for each level within
*** a categorical variable.
***
*** REQUIRED INPUT ARGUMENTS.
***
*** ROWVAR : The CATEGORICAL row variable
***
***
*** OPTIONAL INPUT ARGUMENTS.
***
*** VARFIELDLEN: Maximum allowed length of the VARIABLE column in the output.
*** FREQMEANLEN: Maximum allowed length of the MEAN(SD)/N(PCT) columns in the output.  
*** ROUNDVAL:    Value used to round floating point numbers, an argument to the ROUND function.                
***
*** P-values are computed using the chi-squared test. Note that in some
*** situations, some may argue that Fishers Exact Test should be used
*** instead.
***
*** Version 9 (6/3/2014):
***     Modify the macro for summarizing categorical variables to handle
***     the case where the categorical ROW variable actually has only one level,
***     as in the case of the mutually exclusive insurance variables.
***     In such a case, compute the p-value across the categories of the
***     categorical COLUMN variable (a 1-way ANOVA).
***
*** Version 10 (6/13/2014): Add a row showing number (%) missing.
******************************************************************************;
******************************************************************************;

%macro CategoricalRowVar2(rowVar=,varFieldLen = 128.,freqMeanLen = 48.,roundVal=1);

options nomprint nonotes;

%put Processing &rowVar....;

%local NUMROWLEVELS ROWVARLEVEL NUMCOLLEVELS COLVARLEVEL i j level dsid numObs rc;

***********************************************************;
*** Determine number of levels in ROWVAR.;
***********************************************************;

*** Uniquify group levels.;
proc sort data=&INPUT_DATA out=RowVarLevels_dat NODUPKEY;
    by &rowVar;
run;

*** Remove any missing values.;
data RowVarLevels_dat;
    set RowVarLevels_dat;
    if ( &rowVar ^= " " );
    keep &rowVar;
run;

ODS SELECT NONE; * Turn displayed output off.;
proc contents data=RowVarLevels_dat;
    ods output Contents.DataSet.Attributes = dataset_attributes;
run;
ODS SELECT ALL; * Turn displayed output back on.;

data dataset_attributes;
    set dataset_attributes;
    if ( Label2 = "Observations" ) then CALL SYMPUT("NUMROWLEVELS",nValue2);
    else delete;
    keep nValue2;
run;

***********************************************************;
*** Determine number of levels in COLVAR.;
***********************************************************;

*** Uniquify group levels.;
proc sort data=&INPUT_DATA out=COLVARLevels_dat NODUPKEY;
    by &COLVAR;
run;

*** Remove any missing values.;
data COLVARLevels_dat;
    set COLVARLevels_dat;
    if ( &COLVAR ^= " " );
    keep &COLVAR;
run;

ODS SELECT NONE; * Turn displayed output off.;
proc contents data=COLVARLevels_dat;
    ods output Contents.DataSet.Attributes = dataset_attributes;
run;
ODS SELECT ALL; * Turn displayed output back on.;

data dataset_attributes;
    set dataset_attributes;
    if ( Label2 = "Observations" ) then CALL SYMPUT("NUMCOLLEVELS",nValue2);
    else delete;
    keep nValue2;
run;

***********************************************************;
*** Compute frequencies stratified by the column variable.;
***********************************************************;

ODS SELECT NONE;
proc freq data=&INPUT_DATA;
    tables &COLVAR;
    ods output OneWayFreqs = group_sizes;
run;
ODS SELECT ALL;

ODS SELECT NONE; * Turn displayed output off.;
proc freq data=&INPUT_DATA;
    tables &rowVar * &COLVAR / chisq;
    ods output Freq.Table1.CrossTabFreqs = groups_freqs;
    ods output Freq.Table1.ChiSq         = groups_pval;
run;
ODS SELECT ALL; * Turn displayed output back on.;

*** Determine size of overall group.
*** Include frequency missing - if the overall sample size is smaller than the
*** sum of the subgroups, it is an indication that some subjects had a missing
*** GROUP value.;
data total_num_dat;
    set groups_freqs;
    if ( ( &rowVar = " " ) AND ( &COLVAR = " " ) );
    total_num = Frequency + Missing;
    overall_label = cat("Total (Overall) (N = ",total_num,")");
    keep total_num overall_label;
run;
data _NULL_;
    set total_num_dat;
    CALL SYMPUT("OVERALL_LABEL",overall_label);
run;

***********************************************************;
*** Version 9 (6/3/2014):
***     Modify the macro for summarizing categorical variables to handle
***     the case where the categorical ROW variable actually has only one level,
***     as in the case of the mutually exclusive insurance variables.
***     In such a case, compute the p-value across the categories of the
***     categorical COLUMN variable (a 1-way ANOVA).
***********************************************************;

%if ( &NUMROWLEVELS = 1 ) %then %do;
ODS SELECT NONE; * Turn displayed output off.;
proc anova data=&INPUT_DATA;
    class &COLVAR;
    model &rowVar = &COLVAR;
    ods output ANOVA.ANOVA.&rowVar..ModelANOVA = &ODSOUTPUT;
run;
ODS SELECT ALL; * Turn displayed output back on.;

data groups_pval;
    set &ODSOUTPUT;
    Statistic  = "Chi-Square";
    format ProbF PVALUE6.4;
    label ProbF = "P-value";
    rename ProbF = Prob;
    keep Statistic ProbF;
run;
%end;

***********************************************************;
*** Version 5 (3/7/2013): If the data set GROUPS_PVAL does
*** not yet exist, intitialize it with a missing P-value.
*** This can happen if a row or column sum is zero in the
*** chi-squared table.;
***********************************************************;

%if NOT %sysfunc(exist(groups_pval)) %then %do;
data groups_pval;
    Statistic  = "Chi-Square";
    Prob       = .;
    label Prob = "P-value";
run;
%end;

***********************************************************;
*** If the output data set does not yet exist, intitialize it.;
***********************************************************;

%if NOT %sysfunc(exist(&OUTPUT_DATA)) %then %do;
data &OUTPUT_DATA;
    set _NULL_;
run;
%end;

***********************************************************;
*** Reformat the PROC FREQ results into rows, one per level of the ROW variable.;
***********************************************************;

*** Loop over levels of the row variable.;
%do i = 1 %to &NUMROWLEVELS;

    *** Store current ROW level into a macro variable.;
    data _NULL_;
        set RowVarLevels_dat;
        if ( _N_ = &i ) then CALL SYMPUTX("ROWVARLEVEL",&rowVar);
    run;

    *******************************************************;
    *** Version 6 (5/14/2014): If available, obtain a nice label for the current
    *** row variable level from the format.;
    *** First obtain name of format of the current variable from VARTABLE if possible.;

    data formatTmpDat;
        set VarTable;
        if ( Variable = "&rowVar" );
        keep Format;
    run;

    *** Add the name of the format of the current variable to the table of format levels.;
    data formatTmpDat2;
        set fmtdata;
        if ( _N_ = 1 ) then set formatTmpDat;
    run;

    *** Extract the relevant rows for the format of the current variable.;
    data formatTmpDat3;
        set formatTmpDat2;
        if ( index(Format,strip(FMTNAME)) > 0 );
        keep FMTNAME START END LABEL;
    run;

    *** If FORMATTMPDAT3 is NON-empty, obtain the label of the current level.;
    %let dsid   = %sysfunc(open(formatTmpDat3));
    %let numObs = %sysfunc(attrn(&dsid,nobs));
    %let rc     = %sysfunc(close(&dsid));
    %if ( &numObs > 0 ) %then %do;
        data _NULL_;
            set formatTmpDat3;
            if ( ( START <= &ROWVARLEVEL )
             AND ( END   >= &ROWVARLEVEL ) )
                then CALL SYMPUTX("ROWVARLEVELLABEL",LABEL);
        run;
    %end;
    %else %do;
        %let ROWVARLEVELLABEL = &ROWVARLEVEL;
    %end;

    *** Row Name.;
    data row_name;
        set &INPUT_DATA(keep=&rowVar);
        length Variable tmp $ &varFieldLen;
        tmp = strip(vlabel(&rowVar));
        Variable = catt(tmp," : &ROWVARLEVELLABEL : N (%)");
        if ( _N_ = 1 );
        keep Variable;
    run;

    *** Delete temporary data set(s).;
    proc datasets nolist;
        delete formatTmpDat formatTmpDat2 formatTmpDat3;
    run;
    quit;

    *******************************************************;
    *******************************************************;

    *** Extract total ACROSS the column variable.;
    data total_across_group;
        retain TotNum freqpct_meansd;
        length freqpct_meansd $ &freqMeanLen;
        set groups_freqs;
        by Table;
        if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = " " ) ) then TotNum = Frequency;

        if ( last.Table ) then do;
            Denominator    = Frequency;
            Percent        = 100 * TotNum / Frequency;
            Percent        = round(Percent,&roundVal);
            freqpct_meansd = cat(TotNum," (",strip(put(Percent,8.0)),")");
            label freqpct_meansd = "&OVERALL_LABEL";
            keep freqpct_meansd;
            output;
        end;
    run;

    *** Loop over COLVAR levels.;
    *** Fill in columns within the current row.;
    %do j = 1 %to &NUMCOLLEVELS;

        *** Store current COLUMN level into a macro variable.;
        data _NULL_;
            set COLVARLevels_dat;
            if ( _N_ = &j ) then CALL SYMPUTX("COLVARLEVEL",&COLVAR);
        run;

        *** Determine group size.;
        data label_&j;
            set group_sizes;
            if ( strip(F_&COLVAR) = "&COLVARLEVEL" ) then group_label = cat("&COLVAR = &COLVARLEVEL (N = ",Frequency,")");
            else delete;
            keep group_label;
        run;
        data _NULL_;
            set label_&j;
            CALL SYMPUT("GROUP_LABEL",group_label);
        run;

        *** Create cell for current group level / row variable level.
        ***
        *** Version 11 (6/17/2014): If COLPERCENT is missing, set PCT to 0.;
        data cell_G&j;
            length freqpct_meansd_g&j $ &freqMeanLen;
            set groups_freqs;
            if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = "&COLVARLEVEL" ) );
            if ( ColPercent ^= . ) then pct = round(ColPercent,&roundVal);
            else                        pct = 0;
            freqpct_meansd_g&j = cat(Frequency," (",strip(put(pct,8.0)),")");
            label freqpct_meansd_g&j = "&GROUP_LABEL";
            keep freqpct_meansd_g&j;
        run;

    %end;

    *** p-value.;
    data cell_pval;
        set groups_pval;
        if ( Statistic = "Chi-Square" );
*        format Prob BEST12.; * The formatting is PVALUE6.4 by default.;
        label Prob = "P-value";

        *** If this is NOT the first level of the row variable, set probability to missing.;
        *** This is just for aesthetic purposes.;
        if ( &i ^= 1 ) then Prob = .;
        keep Prob;
    run;

    *** Make table row.;
    *** Reference: http://www.stattutorials.com/SAS/TUTORIAL-PROC-MEANS-OUTPUT.htm;
    data table_row&i;
        set row_name;
        if ( _N_ = 1 ) then do;
            set total_across_group;
            %do j = 1 %to &NUMCOLLEVELS;
                set cell_G&j; 
            %end;
            set cell_pval;
        end;
    run;

    *** Append table row to cumulative SAS data set.;
    data &OUTPUT_DATA;
        set &OUTPUT_DATA table_row&i;
    run;
    proc datasets nolist;
        delete table_row&i;
    run;
    quit;

%end;

***********************************************************;
*** Delete temporary data sets.
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist;
    delete groups_freqs group_sizes groups_pval table_row row_name total_across_group
        COLVARLevels_dat RowVarLevels_dat dataset_attributes
        %do j = 1 %to &NUMCOLLEVELS;
            cell_G&j label_&j
        %end;
    cell_pval total_num_dat;
run;
quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes;

***********************************************************;
*** Version 10 (6/13/2014): Add a row showing number (%) missing.
***********************************************************;

* %CheckMissingness(vrbl=&rowVar);

%mend CategoricalRowVar2;

******************************************************************************;
******************************************************************************;
*** Macro to generate one line of the table for a CONTINUOUS variable
*** showing MEAN and STANDARD DEVIATION.
***
*** REQUIRED INPUT ARGUMENTS.
***
*** ROWVAR : The CATEGORICAL row variable
***
***
*** OPTIONAL INPUT ARGUMENTS.
***
*** VARFIELDLEN: Maximum allowed length of the VARIABLE column in the output.
*** FREQMEANLEN: Maximum allowed length of the MEAN(SD)/N(PCT) columns in the output.
*** ROUNDVAL:    Value used to round floating point numbers, an argument to the ROUND function.
*** PVALMETHOD:  If set to NONPARAMETRIC (the default), PROC NPAR1WAY will be used to compute
***              the p-value. Otherwise, PROC ANOVA will be used.
***
*** P-values are computed using the MEDIAN TEST as implemented in PROC NPAR1WAY.
*** This was thought preferable to using PROC ANOVA, which assumes cells of
*** equal size.
***
*** Version 10 (6/13/2014): Add a row showing number (%) missing.
******************************************************************************;
******************************************************************************;

%macro ContinuousRowVar2(rowVar=,
                         varFieldLen = 128.,
                         freqMeanLen = 48.,
                         roundVal=0.1,
                         pValMethod=NONPARAMETRIC);

options nomprint nonotes;

%put Processing &rowVar...;

%local NUMCOLLEVELS COLVARLEVEL dsid vnum rc j level;

***********************************************************;
*** Determine number of levels in COLVAR.;
***********************************************************;

*** Uniquify group levels.;
proc sort data=&INPUT_DATA out=COLVARLevels_dat NODUPKEY;
    by &COLVAR;
run;

*** Remove any missing values.;
data COLVARLevels_dat;
    set COLVARLevels_dat;
    if ( &COLVAR ^= " " );
    keep &COLVAR;
run;

ODS SELECT NONE; * Turn displayed output off.;
proc contents data=COLVARLevels_dat;
    ods output Contents.DataSet.Attributes = dataset_attributes;
run;
ODS SELECT ALL; * Turn displayed output back on.;

data dataset_attributes;
    set dataset_attributes;
    if ( Label2 = "Observations" ) then CALL SYMPUT("NUMCOLLEVELS",nValue2);
    else delete;
    keep nValue2;
run;

***********************************************************;
*** Determine row name.;
***********************************************************;

data row_name;
    set &INPUT_DATA(keep=&rowVar);
    length Variable tmp $ &varFieldLen;
    tmp = strip(vlabel(&rowVar));
    Variable = catt(tmp," : Mean (SD) [N]");
    if ( _N_ = 1 );
    keep Variable;
run;

***********************************************************;
*** Version 9 (3/15/2013): Count overall sample size as
*** well as sample size of each group.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc contents data=&INPUT_DATA;
    ods output Contents.DataSet.Attributes = overall_attributes;
run;
ODS SELECT ALL; * Turn displayed output back on.;

*** Generate label for overall column.;
data overall_attributes;
    set overall_attributes;
    if ( Label2 = "Observations" );
    overall_label = cat("Total (Overall) (N = ",nValue2,")");
    keep overall_label;
run;
data _NULL_;
    set overall_attributes;
    CALL SYMPUT("OVERALL_LABEL",overall_label);
run;

*** Run PROC FREQ to obtain samples sizes for groups.;
ODS SELECT NONE; * Turn displayed output off.;
proc freq data=&INPUT_DATA;
    tables &COLVAR;
    ods output Freq.Table1.OneWayFreqs = groups_freqs;
run;
ods trace off;

ODS SELECT NONE;
proc freq data=&INPUT_DATA;
    tables &COLVAR;
    ods output OneWayFreqs = group_sizes;
run;
ODS SELECT ALL;

***********************************************************;
*** Subset the data to guarantee that the column variable
*** does not have missing values. This kludge seems to be
*** the cleanest way to handle the case where the column
*** variable is missing.
*** Otherwise, the computation of means by level of the
*** column variable will output the means for subjects with
*** the column variable missing as Means.ByGroup1.Summary.
*** That would require complicated code, involving a test
*** for the case of missing values in the column variable
*** anyway.
*** 1/16/2013: Need to handle the case where COLVAR is
*** NUMERIC rather than CHARACTER. Use the OPEN, VARNUM,
*** and VARTYPE functions.
***********************************************************;

data nonmissing_COLVAR;
    set &INPUT_DATA;
    %let dsid = %sysfunc(open(&INPUT_DATA));
    %let vnum = %sysfunc(varnum(&dsid,&COLVAR));
    %if ( %sysfunc(vartype(&dsid,&vnum)) = C ) %then %do;
    if ( &COLVAR ^= " " ); * COLVAR is of CHARACTER type.;
    %end;
    %else %do;
    if ( &COLVAR ^= . ); * COLVAR is of NUMERIC type.;
    %end;
    %let rc = %sysfunc(close(&dsid));
run;

***********************************************************;
*** Compute means of TOTAL GROUP.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc means data=nonmissing_COLVAR;
    var &rowVar;
    ods output Means.Summary = meansd_total;
run;
ODS SELECT ALL; * Turn displayed output back on.;

data cell_TOT;
    length freqpct_meansd $ &freqMeanLen;
    set meansd_total;
    m              = round(&rowVar._Mean,&roundVal);
    s              = round(&rowVar._StdDev,&roundVal);
    freqpct_meansd = cat(put(m,8.1)," (",strip(put(s,8.1)),") [",&rowVar._N,"]");
    label freqpct_meansd = "&OVERALL_LABEL";
    keep freqpct_meansd;
run;

***********************************************************;
*** Compute means by levels of the column variable.;
***********************************************************;

proc sort data=nonmissing_COLVAR; by &COLVAR; run;
ODS SELECT NONE; * Turn displayed output off.;
proc means data=nonmissing_COLVAR;
    by &COLVAR;
    var &rowVar;

    %do j = 1 %to &NUMCOLLEVELS;
        ods output Means.ByGroup&j..Summary = meansd_level_&j;
    %end;
run;
ODS SELECT ALL; * Turn displayed output back on.;

***********************************************************;
*** Compute p-value.
*** NONPARAMETRIC CASE:
***     If NUMCOLLEVELS = 2, use WILCOXON.
***     Otherwise, use KRUSKALWALLIS.
***********************************************************;

%if ( &pValMethod = NONPARAMETRIC ) %then %do;
    %if ( &NUMCOLLEVELS = 2 ) %then %do;
        %let ODSOUTPUT = WilcoxonTest;
        %let PVARNAME  = PT2_WIL;
    %end;
    %else %do;
        %let ODSOUTPUT = KruskalWallisTest;
        %let PVARNAME  = P_KW;
    %end;

    ODS SELECT NONE; * Turn displayed output off.;
    proc npar1way data=nonmissing_COLVAR wilcoxon;
        class &COLVAR;
        var &rowVar;
        ods output &ODSOUTPUT = &ODSOUTPUT;
    run;
    ODS SELECT ALL; * Turn displayed output back on.;

    data cell_pval;
        set &ODSOUTPUT;
        if ( Name1 = "&PVARNAME" );
        format nValue1 PVALUE6.4;
        label nValue1 = "P-value";
        rename nValue1 = Prob;
        keep nValue1;
    run;
%end;
%else %do;
%put ************************************;
%put ************************************;
%put USING PROC ANOVA RATHER THAN PROC NPAR1WAY...;
%put ************************************;
%put ************************************;
    %let ODSOUTPUT = anova_out;
    ODS SELECT NONE; * Turn displayed output off.;
    proc anova data=nonmissing_COLVAR;
        class &COLVAR;
        model &rowVar = &COLVAR;
        ods output ANOVA.ANOVA.&rowVar..ModelANOVA = &ODSOUTPUT;
    run;
    ODS SELECT ALL; * Turn displayed output back on.;

    data cell_pval;
        set &ODSOUTPUT;
        format ProbF PVALUE6.4;
        label ProbF = "P-value";
        rename ProbF = Prob;
        keep ProbF;
    run;
%end;

***********************************************************;
*** Parse output cells, one per level of the column variable.;
***********************************************************;

%do j = 1 %to &NUMCOLLEVELS;

    *** Store current COLUMN level into a macro variable.;
    %local level;
    data _NULL_;
        set COLVARLevels_dat;
        if ( _N_ = &j ) then CALL SYMPUTX("COLVARLEVEL",&COLVAR);
    run;

    *** Determine group size.;
    data label_&j;
        set group_sizes;
        if ( &COLVAR = "&COLVARLEVEL" ) then group_label = cat("&COLVAR = &COLVARLEVEL (N = ",Frequency,")");
        else delete;
        keep group_label;
    run;
    data _NULL_;
        set label_&j;
        CALL SYMPUT("GROUP_LABEL",group_label);
    run;

    *** If the LABEL_J data set is NOT empty, then generate the output cell.
    *** If the LABEL_J data set is empty, then create a dummy entry.;
    %let dsid = %sysfunc(open(label_&j));
    %let num  = %sysfunc(attrn(&dsid,nobs));
    %let rc   = %sysfunc(close(&dsid));
    %if ( &num > 0 ) %then %do;
        data cell_G&j;
            length freqpct_meansd_g&j $ &freqMeanLen;
*            length m s          $ 16.;
            set meansd_level_&j;
*            m            = putn(round(&rowVar._Mean,&roundVal),5.3);
*            s            = putn(round(&rowVar._StdDev,&roundVal),5.3);
            m                  = round(&rowVar._Mean,&roundVal);
            s                  = round(&rowVar._StdDev,&roundVal);
            freqpct_meansd_g&j = cat(put(m,8.1)," (",strip(put(s,8.1)),") [",&rowVar._N,"]");
            label freqpct_meansd_g&j = "&GROUP_LABEL";
            keep freqpct_meansd_g&j;
        run;
    %end;
    %else %do;
        data cell_G&j;
            length freqpct_meansd_g&j $ &freqMeanLen;
            freqpct_meansd_g&j = " ";
        run;
    %end;
%end;

***********************************************************;
*** Make table row.;
*** Fill in columns within the current row.;
*** Reference: http://www.stattutorials.com/SAS/TUTORIAL-PROC-MEANS-OUTPUT.htm;
***********************************************************;

data table_row_MeanSD;
    set row_name;
    if ( _N_ = 1 ) then do;
        set cell_TOT;
        %do j = 1 %to &NUMCOLLEVELS;
            set cell_G&j;
        %end;
        set cell_pval;
    end;
run;

***********************************************************;
*** If the output data set does not yet exist, intitialize it.;
***********************************************************;

%if NOT %sysfunc(exist(&OUTPUT_DATA)) %then %do;
data &OUTPUT_DATA;
    set _NULL_;
run;
%end;

***********************************************************;
*** Append table row to cumulative SAS data set.;
***********************************************************;

data &OUTPUT_DATA;
    set &OUTPUT_DATA table_row_MeanSD;
run;

***********************************************************;
*** Delete temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist;
    delete nonmissing_COLVAR &ODSOUTPUT
        COLVARLevels_dat dataset_attributes
        %do j = 1 %to &NUMCOLLEVELS;
            cell_G&j meansd_level_&j label_&j
        %end;
        table_row_MeanSD row_name cell_TOT
    cell_pval meansd_total groups_freqs group_sizes overall_attributes;
run;
quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes;

***********************************************************;
*** Version 10 (6/13/2014): Add a row showing number (%) missing.
***********************************************************;

* %CheckMissingness(vrbl=&rowVar);



%mend ContinuousRowVar2;
