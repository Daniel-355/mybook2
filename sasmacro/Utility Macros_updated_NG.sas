******************************************************************************;
******************************************************************************;
*** UTILITY MACROS
***
*** Set of SAS macros to help with basic tasks.
***
*** Author: Jose M. Maisog
*** Version: 43
*** Date: 11/28/2014
***
*** Revision: Nicole M.Geralnc
*** Version: 44
*** Date: 08/15/2017
***
*** Version 44: Better Means (%better_means) and trylock (%trylock) macros added.
***
*** Revision: Nicole M. Gerlanc
*** Date: 10/06/17
*** Version 45: Getvars and Table_summary macros added. (%Table_summary) calls both
*** macros.
*** 
*** Revision: Nicole M. Gerlanc
*** Date 10/11/17
*** Version 46:  Pairwise Pearson Chi-Square (%PAIRWISE_CHISQ) and Adjusted Chi-Square (%CHISQ_MC)
*** 
*** Report bugs to the author at jose.maisog@nih.gov
***
*** Known bugs:
*** (1) When running the macro CATEGORICALROWVAR, if one of the
*** groups in the column variable COLVAR is completely empty, a row
*** for the row variable will NOT be generated. At this time (4/1/2014),
*** it is planned to fix this sometime in the near future.
***
*** (2) This is not really a bug, but the macro GEOMEAN_CI95 manually computes
*** a geometric mean. Version 9.4 of SAS now makes a GEOMEAN function available.
*** Reference:
*** http://support.sas.com/documentation/cdl/en/lefunctionsref/67239/HTML/default/viewer.htm#p0ywq67uqarnnen135hhs9gcsuv0.htm
*** At this time (4/1/2014), it is planned to modify the GEOMEAN_CI95 macro to
*** use the new GEOMEAN function sometime in the near future.
***
*** (3) The CLS macro creates HTML files on disk. At this time, I do not know how
*** to suppress this side-effect.
***
*** (4) The input TEXT argument to the ADDTEXT macro must NOT contain commas.
******************************************************************************;
******************************************************************************;

*** Set option to make a MERGE with no BY an ERROR.
*** Reference:
*** http://support.sas.com/documentation/cdl/en/lesysoptsref/64892/HTML/default/viewer.htm#n0hlv62ubu3fa6n1co02ezotijyo.htm;
options mergenoby = error;

******************************************************************************;
******************************************************************************;
*** Macro to widen a data set with repeats over only one repetition variable.
***
*** INPUTDATA  : Input SAS data set.
*** IDVAR      : The variable indicating the observational unit (e.g., spid, SUBJECT_ID).
*** REPVAR     : The variable indicating repetitions (e.g., SEQ_NBR, VISIT_NBR).
***              Can be either numeric or character, but if it is character it MUST
***              be easily convertible to a numeric value using the INPUT function.
*** WIDEPREFIX : A prefix (a single character is probably best, can be empty)
***              appended to new wide variable names. If it is desired to insert the
***              input data set name into the names of the new wide variables, include
***              the input data set name as part of the WIDEPREFIX input argument.
***
******************************************************************************;
******************************************************************************;

%macro WidenDataSet(inputData=,idVar=,repVar=,widePrefix=,outputData=);

%local repVar_NUM ERR_DETECTED NUMREPS
      i j k typeLIST varLIST vrbl MINREPS MAXREPS ADJREPS lenRepVarFmtDat;

options nomprint nonotes;

*** Load data.;
data temp;
    set &inputData;
run;

*** Extract types, labels, and formats of variables.;
ods select none;
proc contents data=temp varnum;
    ods output Position = type_label_format_dat;
run;
ods select all;

*** Make a list of the types.;
proc sql noprint;
    select Type into :typeLIST separated by " " from type_label_format_dat;
quit;

*** We will need a numeric version of the repetition variable for indexing during widening.
*** If the repetition variable is character, attempt to make a numeric version of it.;
data _NULL_;
    set type_label_format_dat;
    if ( Variable = "&repVar" ) then do;
        call symput("repVarTYPE",Type);
    end;
run;
%if ( &repVarTYPE = Char ) %then %do;
    %let repVar_NUM  = &repVar._NUM;
    %let ERR_DETECTED = 0; * Initialize to 0.;
    data temp;
        set temp;
        &repVar_NUM = input(&repVar,BEST12.);
        if ( _ERROR_ = 1 ) then call symput("ERR_DETECTED",_ERROR_);
    run;
    *** If an error was detected, do not proceed, give an error message.;
    %if ( &ERR_DETECTED ) %then %do;

        *** Delete temporary data sets.;
        proc datasets nolist;
            delete type_label_format_dat temp;
        run;
        quit;
        %put **********************************************;
        %put **********************************************;
        %put MACRO WIDENDATASET TERMINATED.;
        %put An attempt to convert the repetition variable &repVar to numeric failed.;
        %put The input data set was &inputData.;
        %put **********************************************;
        %put **********************************************;
        %abort;
    %end;
%end;
%else %let repVar_NUM  = &repVar;

*** Make a list of the variables.;
proc sql noprint;
    select Variable into :varLIST separated by " " from type_label_format_dat;
quit;

*** Make an array of the formats. This is necessary because the formats of character variables have dollar signs in them.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    data _NULL_;
        set type_label_format_dat;
        if ( Variable = "&vrbl" ) then do;
            call symput("format&i",Format);
        end;
    run;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Version 25 (2/25/2014): If a format FORMAT&i is missing, it means that SAS is using a default format.
*** In this case, set FORMAT&i to some reasonable default format.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    %if ( &&format&i = . ) %then %do;
        *** Open the data set descriptor.;
        %let dsid = %sysfunc(open(temp));
        %let vnum = %sysfunc(varnum(&dsid,&vrbl));

        *** Determine whether the variable is CHARACTER or NUMERIC.;
        %if ( %sysfunc(vartype(&dsid,&vnum)) = C ) %then %do;
            *** If the variable is CHARACTER, set the format to $X., where X is the LENGTH.;
            %let vrblLen  = %sysfunc(varlen(&dsid,&vnum));
            %let format&i = $&vrblLen..;
        %end;
        %else %do;
            *** If the variable is NUMERIC, set the format to BEST12.;
            %let format&i = BEST12.;
        %end;

        ** Close the data set descriptor.;
        %let rc = %sysfunc(close(&dsid));
    %end;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Version 28 (4/3/2014): At this point, if a variable is Character type but the
*** first character of its format is NOT a dollar sign, insert a leading dollar sign.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    *** Open the data set descriptor.;
    %let dsid = %sysfunc(open(temp));
    %let vnum = %sysfunc(varnum(&dsid,&vrbl));

    *** Determine whether the variable is CHARACTER or NUMERIC.;
    %if ( %sysfunc(vartype(&dsid,&vnum)) = C ) %then %do;
        *** If the variable is CHARACTER, check the format and fix if necessary.;
        %if ( &&format&i = ) %then %do;
            %let vlen = %sysfunc(varlen(&dsid,&vnum));
            %let format&i = $&vlen..;
        %end;
        %else %do;
            %let firstChar = %substr(&&format&i,1,1);
            %if ( &firstChar ^= $ ) %then %let format&i = $&&format&i;
        %end;
    %end;

    *** Close the data set descriptor.;
    %let rc = %sysfunc(close(&dsid));

    *** Get next variable.;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Make an array of the labels. This is necessary because the labels have spaces in them.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    data _NULL_;
        set type_label_format_dat;
        if ( Variable = "&vrbl" ) then do;
            call symput("label&i",strip(Label));
        end;
    run;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Version 28 (4/3/2014): If a label is missing, set it to the original variable name.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    %if ( %bquote(&&label&i) = . ) OR ( %bquote(&&label&i) = ) %then %let label&i = &vrbl;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Determine the maximum number of repetitions from the maximum value of
*** the numeric form of the repetition variable.;
ods select none;
proc means data=temp MIN MAX;
    var &repVar_NUM;
    ods output Summary = max_dat;
run;
ods select all;
data _NULL_;
    set max_dat;
    CALL SYMPUT("MAXREPS",strip(&repVar_NUM._Max));
    CALL SYMPUT("MINREPS",strip(&repVar_NUM._Min));
run;

*** If possible, obtain formatted labels for each value of the repetition variable.;
proc format library=work.formats cntlout = fmtdata; run; * Obtain format labels.;
data repvar_format;                                      * Isolate format of the repetitition variable.;
    length Format $ 32.;
    set type_label_format_dat;
    if ( Variable = "&repVar" );
    strLength = length(Format);
    if ( substr(Format,1,1) = '$' ) then Format = substr(Format,2,strLength-1); * Remove leading $.;
    strLength = length(Format);
    if ( substr(Format,strLength,1) = '.' ) then Format = substr(Format,1,strLength-1); * Removing trailing dot.;
    rename Format = FMTNAME;
run;
proc sort data=repvar_format; by FMTNAME; run;
proc sort data=fmtdata;       by FMTNAME; run;
data repvar_format;
    merge repvar_format(keep= Variable FMTNAME in=in1) fmtdata(in=in2);
    by FMTNAME;
    if ( in1 AND in2 );
run;
%let dsid = %sysfunc(open(repvar_format));
%let lenRepVarFmtDat = %sysfunc(attrn(&dsid,nobs));
%let rc   = %sysfunc(close(&dsid));
%do i = 1 %to &lenRepVarFmtDat;
    data _NULL_;
        set repvar_format;
        if ( _N_ = &i ) then do;
            call symput("repValStart&i",strip(START));
            call symput("repValEnd&i",strip(END));
            call symput("repValLabel&i",strip(Label));
        end;
    run;
%end;

*** Need to account for the possibility that the repetition number starts at zero.
*** Define an adjustment to be applied to the numeric version of the repetition number,
*** and then adjust the numeric version of the repetition number accordingly.;
%let ADJREPS = %eval( 1 - &MINREPS );

*** Widen variables over the repetition variable.
*** Do not widen &idVar or the repetition variable itself.;
proc sort data=temp; by &idVar &repVar_NUM; run;
data &outputData;
    set temp;
    by &idVar;
    keep &idVar;

    *** Loop over all variables.;
    %let i = 1;
    %let vrbl = %scan(&varLIST,&i);
    %do %until(NOT %length(&vrbl));

        *** If the current variable is NOT &idVar and is NOT the repetition variable, widen it.;
        %if ( ( %lowcase(&vrbl) ^= %lowcase(&idVar)      )
          AND ( %lowcase(&vrbl) ^= %lowcase(&repVar)     )
          AND ( %lowcase(&vrbl) ^= %lowcase(&repVar_NUM) ) ) %then %do;

            *** First apply format of original variable to the new wide variables.
            *** Need to do this before the ARRAY or RETAIN statements to make sure
            *** that character variables are initialized to character, not numeric.;
            %let ORIG_FORMAT = &&format&i;
            format &vrbl.&widePrefix.&MINREPS - &vrbl.&widePrefix&MAXREPS &ORIG_FORMAT;

            *** Map arrays to new wide variables;
            array X&i[*] &vrbl.&widePrefix.&MINREPS - &vrbl.&widePrefix&MAXREPS;

            *** RETAIN new wide variables across observations in the input data set.;
            retain &vrbl.&widePrefix.&MINREPS - &vrbl.&widePrefix&MAXREPS;

            *** Initialize array to missing values.;
            %let ORIG_TYPE = %scan(&typeLIST,&i);
            if ( first.&idVar ) then do;
                do i=1 to DIM(X&i);
                %if ( &ORIG_TYPE = Char ) %then %do;
                    X&i[i] = " ";
                %end;
                %else %do;
                    X&i[i] = .;
                %end;
                end;
            end;

            *** Fill in new wide variables via mapped array.
            *** Use the numeric version of the repetition variable as the array index.;
            if ( &repVar_NUM ^= . ) then X&i[ &repVar_NUM + &ADJREPS ] = &vrbl;

            *** Insert informative labels;
            %do j = &MINREPS %to &MAXREPS;
                *** If the repetition variable is numeric and has a formatting,
                *** attempt to use the formatting labels, if any.;
                %if ( &lenRepVarFmtDat > 0 ) %then %do;
                    %let repValue = &j;
                    %do k = 1 %to &lenRepVarFmtDat;
                        %if ( &&repValStart&k <= &j ) AND ( &j <= &&repValEnd&k ) %then %let repValue = &&repValLabel&k;
                    %end;
                %end;
                %else %do;
                    %let repValue = &j;
                %end;
                label &vrbl.&widePrefix.&j = "&&label&i : &repVar = &repValue";
            %end;
        %end;

        %let i = %eval( &i + 1 );
        %let vrbl = %scan(&varLIST,&i);
    %end;

    *** If this is the last observation for this subject, output the line.;
    if ( last.&idVar ) then output;

    *** Keep only &idVar and the widened variables.;
    keep &idVar
    %let i = 1;
    %let vrbl = %scan(&varLIST,&i);
    %do %until(NOT %length(&vrbl));
        %if ( ( &vrbl ^= &idVar  )
          AND ( &vrbl ^= &repVar     )
          AND ( &vrbl ^= &repVar_NUM ) ) %then %do;
            &vrbl.&widePrefix.&MINREPS - &vrbl.&widePrefix&MAXREPS
        %end;
        %let i = %eval( &i + 1 );
        %let vrbl = %scan(&varLIST,&i);
    %end;
    ;            
run;

*** Delete temporary data sets.;
proc datasets nolist;
    delete type_label_format_dat max_dat temp fmtdata repvar_format;
run;
quit;

%mend WidenDataSet;

******************************************************************************;
******************************************************************************;
*** Macro to widen a data set with repeats over TWO repetition variables.
***
*** INPUTDATA   : Input SAS data set.
*** IDVAR       : The variable indicating the observational unit (e.g., spid, SUBJECT_ID).
*** REPVAR1     : The first variable indicating repetitions (e.g., SEQ_NBR, VISIT_NBR).
***               Can be either numeric or character, but if it is character it MUST
***               be easily convertible to a numeric value using the INPUT 
.
*** WIDEPREFIX1 : A prefix (a single character is probably best, can be empty)
***               appended to newwide variable names, for the first repetition variable.
***               If it is desired to insert the input data set name into the names
***               of the new wide variables, include the input data set name as part of
***               the WIDEPREFIX1 input argument.
*** REPVAR2     : The second variable indicating repetitions (e.g., SEQ_NBR, VISIT_NBR).
***               Can be either numeric or character, but if it is character it MUST
***               be easily convertible to a numeric value using the INPUT function.
*** WIDEPREFIX2 : A prefix (a single character is probably best, can be empty)
***               appended to newwide variable names, for the second repetition variable.
***
******************************************************************************;
******************************************************************************;

%macro WidenDataSetTwoVars(inputData=,idVar=,repVar1=,repVar2=,widePrefix1=,widePrefix2=,outputData=);

%local repVar1_NUM repVar2_NUM ERR_DETECTED NUMREPS1 NUMREPS2 NUMREPS
      i j k typeLIST varLIST vrbl MINREPS1 MAXREPS1 MINREPS2 MAXREPS2 ADJREPS1 ADJREPS2;

options nomprint nonotes;

*** Load data.;
data temp;
    set &inputData;
run;

*** Extract types, labels, and formats of variables.;
ods select none;
proc contents data=temp varnum;
    ods output Position = type_label_format_dat;
run;
ods select all;

*** Make a list of the types.;
proc sql noprint;
    select Type into :typeLIST separated by " " from type_label_format_dat;
quit;

*** We will need numeric versions of the repetition variables for indexing during widening.
*** If a repetition variable is character, attempt to make a numeric version of it.
*** REPETITION VARIABLE #1.;
data _NULL_;
    set type_label_format_dat;
    if ( Variable = "&repVar1" ) then do;
        call symput("repVar1TYPE",Type);
    end;
run;
%if ( &repVar1TYPE = Char ) %then %do;
    %let repVar1_NUM  = &repVar1._NUM;
    %let ERR_DETECTED = 0; * Initialize to 0.;
    data temp;
        set temp;
        &repVar1_NUM = input(&repVar1,BEST12.);
        if ( _ERROR_ = 1 ) then call symput("ERR_DETECTED",_ERROR_);
    run;
    *** If an error was detected, do not proceed, give an error message.;
    %if ( &ERR_DETECTED ) %then %do;

        *** Delete temporary data sets.;
        proc datasets nolist;
            delete type_label_format_dat temp;
        run;
        quit;
        %put **********************************************;
        %put **********************************************;
        %put MACRO WIDENDATASET TERMINATED.;
        %put An attempt to convert the repetition variable &repVar1 to numeric failed.;
        %put The input data set was &inputData.;
        %put **********************************************;
        %put **********************************************;
        %abort;
    %end;
%end;
%else %let repVar1_NUM  = &repVar1;

*** We will need numeric versions of the repetition variables for indexing during widening.
*** If a repetition variable is character, attempt to make a numeric version of it.
*** REPETITION VARIABLE #2.;
data _NULL_;
    set type_label_format_dat;
    if ( Variable = "&repVar2" ) then do;
        call symput("repVar2TYPE",Type);
    end;
run;
%if ( &repVar2TYPE = Char ) %then %do;
    %let repVar2_NUM  = &repVar2._NUM;
    %let ERR_DETECTED = 0; * Initialize to 0.;
    data temp;
        set temp;
        &repVar2_NUM = input(&repVar2,BEST12.);
        if ( _ERROR_ = 1 ) then call symput("ERR_DETECTED",_ERROR_);
    run;
    *** If an error was detected, do not proceed, give an error message.;
    %if ( &ERR_DETECTED ) %then %do;

        *** Delete temporary data sets.;
        proc datasets nolist;
            delete type_label_format_dat temp;
        run;
        quit;
        %put **********************************************;
        %put **********************************************;
        %put MACRO WIDENDATASET TERMINATED.;
        %put An attempt to convert the repetition variable &repVar2 to numeric failed.;
        %put The input data set was &inputData.;
        %put **********************************************;
        %put **********************************************;
        %abort;
    %end;
%end;
%else %let repVar2_NUM  = &repVar2;

*** Make a list of the variables.;
proc sql noprint;
    select Variable into :varLIST separated by " " from type_label_format_dat;
quit;

*** Make an array of the formats. This is necessary because the formats of character variables have dollar signs in them.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    data _NULL_;
        set type_label_format_dat;
        if ( Variable = "&vrbl" ) then do;
            call symput("format&i",strip(Format));
        end;
    run;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Version 28 (4/3/2014): At this point, if a variable is Character type but the
*** first character of its format is NOT a dollar sign, insert a leading dollar sign.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    *** Open the data set descriptor.;
    %let dsid = %sysfunc(open(temp));
    %let vnum = %sysfunc(varnum(&dsid,&vrbl));

    *** Determine whether the variable is CHARACTER or NUMERIC.;
    %if ( %sysfunc(vartype(&dsid,&vnum)) = C ) %then %do;
        *** If the variable is CHARACTER, check the format and fix if necessary.;
        %let firstChar = %substr(&&format&i,1,1);
        %if ( &firstChar ^= $ ) %then %let format&i = $&&format&i;
    %end;

    *** Close the data set descriptor.;
    %let rc = %sysfunc(close(&dsid));

    *** Get next variable.;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Make an array of the labels. This is necessary because the labels have spaces in them.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    data _NULL_;
        set type_label_format_dat;
        if ( Variable = "&vrbl" ) then do;
            call symput("label&i",Label);
        end;
    run;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Version 28 (4/3/2014): If a label is missing, set it to the original variable name.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(NOT %length(&vrbl));
    %if ( %bquote(&&label&i) = . ) %then %let label&i = &vrbl;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Determine the minimum and maximum values of repetition variable #1.;
ods select none;
proc means data=temp MIN MAX;
    var &repVar1_NUM;
    ods output Summary = max_dat1;
run;
ods select all;
data _NULL_;
    set max_dat1;
    CALL SYMPUT("MAXREPS1",strip(&repVar1_NUM._Max));
    CALL SYMPUT("MINREPS1",strip(&repVar1_NUM._Min));
run;
%let NUMREPS1 = %eval( &MAXREPS1 - &MINREPS1 + 1 );

*** Determine the minimum and maximum values of repetition variable #2.;
ods select none;
proc means data=temp MIN MAX;
    var &repvar2_NUM;
    ods output Summary = max_dat1;
run;
ods select all;
data _NULL_;
    set max_dat1;
    CALL SYMPUT("MAXREPS2",strip(&repvar2_NUM._Max));
    CALL SYMPUT("MINREPS2",strip(&repVar2_NUM._Min));
run;
%let NUMREPS2 = %eval( &MAXREPS2 - &MINREPS2 + 1 );

*** Compute total number of repetitions over both repetition variables.;
%let NUMREPS = %eval( &NUMREPS1 * &NUMREPS2 );

*** Need to account for the possibility that the repetition number starts at zero.
*** Define an adjustment to be applied to the numeric version of the repetition number,
*** and then adjust the numeric version of the repetition number accordingly.;
%let ADJREPS1 = %eval( 1 - &MINREPS1 );
%let ADJREPS2 = %eval( 1 - &MINREPS2 );

*** Widen variables over the repetition variable.
*** Do not widen &idVar or the repetition variable itself.;
proc sort data=temp; by &idVar &repVar1_NUM; run;
data &outputData;
    set temp;
    by &idVar;
    keep &idVar;

    *** Compute index into arrays, given the two repetition variables.;
    tmpIndex = ( ( &REPVAR1 + &ADJREPS1 - 1 ) * &NUMREPS2 ) + ( &REPVAR2 + &ADJREPS2 );

    *** Loop over all variables.;
    %let i = 1;
    %let vrbl = %scan(&varLIST,&i);
    %do %until(NOT %length(&vrbl));

        *** If the current variable is NOT &idVar and is NOT the repetition variable, widen it.;
        %if ( ( &vrbl ^= &idVar       )
          AND ( &vrbl ^= &repVar1     )
          AND ( &vrbl ^= &repVar1_NUM )
          AND ( &vrbl ^= &repVar2     )
          AND ( &vrbl ^= &repVar2_NUM ) ) %then %do;

            *** First apply format of original variable to the new wide variables.
            *** Need to do this before the ARRAY or RETAIN statements to make sure
            *** that character variables are initialized to character, not numeric.;
            %let ORIG_FORMAT = &&format&i;
            format
            %do j = &MINREPS1 %to &MAXREPS1;
                %do k = &MINREPS2 %to &MAXREPS2;
                &vrbl.&widePrefix1&j.&widePrefix2&k
                %end;
            %end;
                &ORIG_FORMAT;

            *** Map arrays to new wide variables;
            array X&i[*]
            %do j = &MINREPS1 %to &MAXREPS1;
                %do k = &MINREPS2 %to &MAXREPS2;
                &vrbl.&widePrefix1&j.&widePrefix2&k
                %end;
            %end;
            ;

            *** RETAIN new wide variables across observations in the input data set.;
            retain
            %do j = &MINREPS1 %to &MAXREPS1;
                %do k = &MINREPS2 %to &MAXREPS2;
                &vrbl.&widePrefix1&j.&widePrefix2&k
                %end;
            %end;
            ;

            *** Initialize array to missing values.;
            %let ORIG_TYPE = %scan(&typeLIST,&i);
            if ( first.&idVar ) then do;
                do i=1 to DIM(X&i);
                %if ( &ORIG_TYPE = Char ) %then %do;
                    X&i[i] = " ";
                %end;
                %else %do;
                    X&i[i] = .;
                %end;
                end;
            end;

            *** Fill in new wide variables via mapped array.
            *** Use the numeric version of the repetition variable as the array index.;
            if ( tmpIndex ^= . ) then X&i[tmpIndex] = &vrbl;

            *** Insert informative labels;
            %do j = &MINREPS1 %to &MAXREPS1;
                %do k = &MINREPS2 %to &MAXREPS2;
                    label &vrbl.&widePrefix1&j.&widePrefix2&k = "&&label&i : &repVar1 = &j : &repVar2 = &k";
                %end;
            %end;
        %end;

        %let i = %eval( &i + 1 );
        %let vrbl = %scan(&varLIST,&i);
    %end;

    *** If this is the last observation for this subject, output the line.;
    if ( last.&idVar ) then output;

    *** Keep only &idVar and the widened variables.;
    keep &idVar
    %let i = 1;
    %let vrbl = %scan(&varLIST,&i);
    %do %until(NOT %length(&vrbl));
        %if ( ( &vrbl ^= &idVar       )
          AND ( &vrbl ^= &repVar1     )
          AND ( &vrbl ^= &repVar1_NUM )
          AND ( &vrbl ^= &repVar2     )
          AND ( &vrbl ^= &repVar2_NUM ) ) %then %do;
            %do j = &MINREPS1 %to &MAXREPS1;
                %do k = &MINREPS2 %to &MAXREPS2;
                &vrbl.&widePrefix1&j.&widePrefix2&k
                %end;
            %end;
        %end;
        %let i = %eval( &i + 1 );
        %let vrbl = %scan(&varLIST,&i);
    %end;
    ;            
run;

*** Delete temporary data sets.;
proc datasets nolist;
    delete type_label_format_dat max_dat1 max_dat2 temp;
run;
quit;

%mend WidenDataSetTwoVars;

******************************************************************************;
******************************************************************************;
*** MACRO TO MERGE A NON-REPEATED DATA SET INTO THE OUTPUT DATA SET.
*** THIS IS FOR THE FETAL GROWTH STUDY, BUT MAY BE USEFUL IN OTHER CONTEXTS.
******************************************************************************;
******************************************************************************;

%macro MergeNonrepeatedDataIntoOutput(inputData=,initData=,outputData=);

proc format library=work.formats cntlin=fmtlib.varfmt;

%local dotIndex strLength startIndex finalLength FILENAME i vrbl;

options nomprint nonotes;

*** Load data into a temporary data set.;
%let dotIndex = %index(&inputData,.);
%if ( &dotIndex = 0 ) %then %do;
    data temp;
        set &inputData;
    run;
    %let FILENAME = &inputData;
%end;
%else %do;
    data temp;
        set &inputData;
    run;
    %let strLength   = %length(&inputData);
    %let startIndex  = %eval( &dotIndex + 1 );
    %let finalLength = %eval( &strLength - &dotIndex );
    %let FILENAME    = %substr(&inputData,&startIndex,&finalLength);
%end;

***  Version 18 (11/18/2013): Extract types, labels, and formats of variables.;
ods select none;
proc contents data=temp varnum;
    ods output Position = type_label_format_dat;
run;
ods select all;

*** Version 18 (11/18/2013): Make a list of the variables.;
proc sql noprint;
    select Variable into :varLIST separated by " " from type_label_format_dat;
quit;

*** Version 18 (11/18/2013): Loop over all variables, and if the variable
*** is NOT SUBJECT_ID, append the data set name to the variable name.;
data temp;
    set temp;
   %let i = 1;
   %let vrbl = %scan(&varLIST,&i);
   %do %until(NOT %length(&vrbl));
       %let i = %eval( &i + 1 );
      %if ( &vrbl ^= SUBJECT_ID ) %then %do;
      rename &vrbl = &vrbl._&FILENAME;
      %end;
       %let vrbl = %scan(&varLIST,&i);
   %end;
run;

*** If INITDATA is given, initialize a SAS data set named INITDATA to TEMP.
*** Otherwise, MERGE the TEMP data set into OUTPUTDATA.;
*** MERGE with output data set.;
%if ( %length(&initData) ) %then %do;
data &initData;
    set temp;
run;
proc sort data=&initData; by SUBJECT_ID; run;
%end;
%else %do;
/* The output data set should already be sorted at this point.
proc sort data=&outputData; by SUBJECT_ID; run;
*/
proc sort data=temp; by SUBJECT_ID; run;
data &outputData;
    merge &outputData temp;
    by SUBJECT_ID;
run;
%end;

*** Invoke CODEBOOKNONREPEATEDDATA to update the code book.;
%CodeBookNonrepeatedData(inputData=temp,outputData=codebook);

*** Delete temporary data set.;
proc datasets nolist;
    delete temp type_label_format_dat;
run;
quit;

%mend MergeNonrepeatedDataIntoOutput;

******************************************************************************;
******************************************************************************;
*** MACRO TO WIDEN DATA AND MERGE INTO THE OUTPUT DATA SET.
*** THIS MACRO CAN HANDLE DATA WITH EITHER ONE OR TWO REPETITION VARIABLES.
*** THIS IS FOR THE FETAL GROWTH STUDY, BUT MAY BE USEFUL IN OTHER CONTEXTS.
*** NOTE THAT IT HAS A CONDITIONAL THAT INVOLVES VARIABLES SPECIFIC TO THE
*** FETAL GROWTH STUDY, THOUGH.
******************************************************************************;
******************************************************************************;

%macro WidenDataSetAndMergeIntoOutput(inputData=,idVar=,repVar1=,repVar2=,widePrefix1=,widePrefix2=,initData=,outputData=);

*** Load data into a temporary data set.
***
*** Version 17 (11/6/2014): To avoid problems with the ampersand interfering with the widening macro,
*** relabel L&D variables in data set FM024i.;
%let dotIndex = %index(&inputData,.);
%if ( &dotIndex = 0 ) %then %do;
    data temp;
        set &inputData;
    run;
    %let FILENAME = &inputData;
%end;
%else %do;
    data temp;
        set &inputData;
        %if ( &inputData = WORKING.fm024i ) %then %do;
        label DDPROM   = "F024I H7 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - PROM"
              DDPTLABR = "F024I H8 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Preterm labor"
              DDCERVIN = "F024I H9 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Cervical incompetence"
              DDPREECL = "F024I H10 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Gest. hypertension"
              DDPLACPR = "F024I H11 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Placenta previa"
              DDPLACAB = "F024I H12 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Placental abruption"
              DDPYELON = "F024I H13 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Pyelonephritis"
              DDAPPEND = "F024I H14 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Appendicitis"
              DDCHOLE  = "F024I H15 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Cholecystitis/cholelithiasis"
              DDOTR    = "F024I H16 Hospital Admission/Labor and Delivery Triage/ER visits Diagnosis - Other";
        %end;
    run;
    %let strLength   = %length(&inputData);
    %let startIndex  = %eval( &dotIndex + 1 );
    %let finalLength = %eval( &strLength - &dotIndex );
    %let FILENAME    = %substr(&inputData,&startIndex,&finalLength);
%end;

*** Widen data. The SAS macro to call depends on whether we have two or only one repetition variables.
*** (Data set FM013a has two repetition variables.0;
%let idVar = SUBJECT_ID;
%if ( ( &REPVAR1 ^= )
  AND ( &REPVAR2 ^= ) ) %then %do;
    %WidenDataSetTwoVars(inputData=temp,idVar=&idVar,repVar1=&REPVAR1,repVar2=&repVar2,widePrefix1=_&FILENAME._&widePrefix1,widePrefix2=_&widePrefix2,outputData=output_dat);

    *** Invoke CODEBOOKREPEATEDDATAONEREPVAR to update the code book.;
    %CodeBookRepeatedDataTwoRepVars(inputData=&inputData,repVar1=&REPVAR1,repVar2=&REPVAR2,widePrefix1=&widePrefix1,widePrefix2=&widePrefix2,outputData=codebook);
%end;
%else %if ( ( &REPVAR1 ^= )
        AND ( &REPVAR2  = ) ) %then %do;
    %WidenDataSet(inputData=temp,idVar=&idVar,repVar=&REPVAR1,widePrefix=_&FILENAME._&widePrefix1,outputData=output_dat);

    *** Invoke CODEBOOKREPEATEDDATAONEREPVAR to update the code book.;
    %CodeBookRepeatedDataOneRepVar(inputData=&inputData,repVar=&REPVAR1,widePrefix=&widePrefix1,outputData=codebook);
%end;
%else %do;
    %put **********************************************;
    %put **********************************************;
    %put MACRO WIDENDATASETANDMERGEINTOOUTPUT TERMINATED.;
    %put Need to provide at least REPVAR1.;
    %put **********************************************;
    %put **********************************************;
    %abort;
%end;

*** If INITDATA is given, initialize a SAS data set named INITDATA to OUTPUT_DAT.
*** Otherwise, MERGE the OUTPUT_DAT data set into OUTPUTDATA.;
*** MERGE with output data set.;
%if ( %length(&initData) ) %then %do;
data &initData;
    set output_dat;
run;
proc sort data=&initData; by SUBJECT_ID; run;
%end;
%else %do;
/* The output data set should already be sorted at this point.
proc sort data=&outputData; by SUBJECT_ID; run;
*/
proc sort data=output_dat; by SUBJECT_ID; run;
data &outputData;
    merge &outputData output_dat;
    by SUBJECT_ID;
run;
%end;

*** Delete temporary data set.;
proc datasets nolist;
    delete output_dat temp;
run;
quit;

%mend WidenDataSetAndMergeIntoOutput;

******************************************************************************;
*** MACRO TO CREATE ROW(S) FOR ONE VARIABLE.
*** Creates row(s) that include information on format labels, if any.
******************************************************************************;

%macro AddInfoOnFormatLabelsIfAny(inputData=,vrbl=);

    *** Create a column FMTNAME matching the variable of that name
    *** in the data set containing format information, fmtdata_codebook.;
    data &inputData;
        set &inputData;
        length FMTNAME $ 32.;

        if ( Variable = "&vrbl" ); * Get the current variable;

        *** Obtain first and last last character of FORMAT.;
        strLength = length(Format);
		firstChar = substr(Format,1,1);
        lastChar  = substr(Format,strLength);

        *** Set FMTNAME equal to FORMAT. If the last character
        *** of FORMAT is a dot/period, do not include it.
		*** If the first character is a dollar sign, do not include it;
        if ( lastChar = '.' ) then do;
		    if ( firstChar = '$' ) then FMTNAME = substr(Format,2,strLength-2);
			else                        FMTNAME = substr(Format,1,strLength-1);
		end;
        else                       FMTNAME = Format;
        drop strLength lastChar firstChar;
    run;

    *** Merge information on levels within format with the
    *** table of types, labels, and formats of variables.
    *** Drop FMTNAME, since it is redundant.;
    proc sort data=&inputData; by FMTNAME; run;
    proc sort data=fmtdata_codebook;    by FMTNAME; run;
    data &inputData;
        merge &inputData(in=in1) fmtdata_codebook(keep=FMTNAME FormatValueLabel);
        by FMTNAME;
        if ( in1 );
        drop FMTNAME;
    run;

    *** Remove repeat entries. Also drop FMTNAME, since it is redundant.;
    data &inputData;
        retain lastMember lastVariable lastType lastFormat lastInformat lastLabel;
        length Member   lastMember   $ 256.;
        length Variable lastVariable $ 32.;
        length Type     lastType     $ 4.;
        length Format   lastFormat   $ 15.;
        length Informat lastInformat $ 11.;
        length Label    lastLabel    $ 256.;
        set var_dat;
        array var[*]  Member     Variable     Type     Format     Informat     Label;
        array last[*] lastMember lastVariable lastType lastFormat lastInformat lastLabel;

        *** Remove repeat entries.;
        if ( ( _N_ = 1 ) OR ( Variable ^= lastVariable ) ) then do i = 1 to dim(var);
            lastVariable  = Variable; * Initialize.;
        end;
        else if ( Variable = lastVariable ) then do;
            Len = .;
            Num = .;
            do i = 1 to dim(last);
            var[i] = " ";
            end;
        end;
        format Num BEST4.;
        drop i lastMember lastVariable lastType lastFormat lastInformat lastLabel;
    run;

%mend AddInfoOnFormatLabelsIfAny;

******************************************************************************;
*** MACRO TO CREATE CODEBOOK INFORMATION FOR A NON-REPEATED DATA SET.
*** Loosely adapted on the macro MERGENONREPEATEDDATAINTOOUTPUT
*** in the SAS program
*** Create ONE Merged Data Set From C-TASC Data Sets - v10.070214.sas
******************************************************************************;

%macro CodeBookNonrepeatedData(inputData=,outputData=);

options nomprint nonotes;

%local i vrbl varLIST dotIndex FILENAME strLength startIndex finalLength;

%local idVar;
%let idVar = SUBJECT_ID;

*** Parse out name of input data set, as was done in the macro
*** MERGENONREPEATEDDATAINTOOUTPUT.;
%let dotIndex = %index(&inputData,.);
%if ( &dotIndex = 0 ) %then %let FILENAME = &inputData;
%else %do;
    %let strLength   = %length(&inputData);
    %let startIndex  = %eval( &dotIndex + 1 );
    %let finalLength = %eval( &strLength - &dotIndex );
    %let FILENAME    = %substr(&inputData,&startIndex,&finalLength);
%end;

*** If output data set does not yet exist,
*** initialize it so it can be appended to.;
%if NOT %sysfunc(exist(&outputData)) %then %do;
data &outputData;
    set _NULL_;
run;
%end;

***  Extract types, labels, and formats of variables in the current data set.;
ods select none;
proc contents data=&inputData varnum;
    ods output Position = type_label_format_dat;
run;
ods select all;

*** Remove the subject ID from the table.;
*data type_label_format_dat;
*    set type_label_format_dat;
*    if ( Variable = "&idVar" ) then delete;
*run;

*** Rename variables as was done in the macro
*** MERGENONREPEATEDDATAINTOOUTPUT.;
***data type_label_format_dat;
***    set type_label_format_dat;
***    Variable = catt(Variable,"_","&FILENAME");
***run;

*** Refresh list of variables, now that we have appended the file name to the variable names.;
proc sql noprint;
    select Variable into :varLIST separated by " " from type_label_format_dat;
quit;

*** Process variables one-by-one. Yes, tedious and slow, but I
*** think this is the best way to go for clarity and debugging.
*** Do not include the subject ID variable.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until( NOT %length(&vrbl));

    %put &inputData : &i : &vrbl;

    *** Obtain entry for the current variable.;
    data var_dat;
        length Label $ 256.;
        set type_label_format_dat;
        if ( Variable = "&vrbl" );
    run;

    *** Add information on format labels, if any.;
    %AddInfoOnFormatLabelsIfAny(inputData=var_dat,vrbl=&vrbl);

    *** Append to output data set.;
    data &outputData;
        set &outputData var_dat;
    run;

    *** Delete temporary data set(s).;
    proc datasets nolist;
    delete var_dat;
    run;
    quit;

    *** Get next variable.;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Delete temporary data sets.;
proc datasets nolist;
    delete type_label_format_dat;
run;
quit;

*** Re-order columns in output data set.
*** It would be VERY convenient to have the label and
*** format value labels right next to the variable names.;
data &outputData;
    retain Member Num Variable Label Format FormatValueLabel;
    set &outputData;
run;

%mend CodeBookNonrepeatedData;

*** TEST.;
/*
%CodeBookNonrepeatedData(inputData=nfgs.fm002,outputdata=test_dat);
*/

******************************************************************************;
*** MACRO TO PROCESS REPEATED MEASURES DATA SET, ONE REPETITION VARIABLE.
*** Very loosely adapted on the macro WIDENDATASET
*** referenced in the SAS program
*** Create ONE Merged Data Set From C-TASC Data Sets - v10.070214.sas
*** but defined in the SAS program
*** Utility Macros - v32.061214.sas
******************************************************************************;

%macro CodeBookRepeatedDataOneRepVar(inputData=,repVar=,widePrefix=,outputData=);

options nomprint nonotes;

*** Load data.;
data temp;
    set &inputData;
run;

%local i vrbl varLIST dotIndex FILENAME strLength startIndex finalLength;
%local repVarTYPE ERR_DETECTED MAXREPS MINREPS;

%local idVar;
%let idVar = SUBJECT_ID;

*** Parse out name of input data set, as was done in the macro
*** MERGENONREPEATEDDATAINTOOUTPUT.;
%let dotIndex = %index(&inputData,.);
%if ( &dotIndex = 0 ) %then %let FILENAME = &inputData;
%else %do;
    %let strLength   = %length(&inputData);
    %let startIndex  = %eval( &dotIndex + 1 );
    %let finalLength = %eval( &strLength - &dotIndex );
    %let FILENAME    = %substr(&inputData,&startIndex,&finalLength);
%end;

*** If output data set does not yet exist,
*** initialize it so it can be appended to.;
%if NOT %sysfunc(exist(&outputData)) %then %do;
data &outputData;
    set _NULL_;
run;
%end;

***  Extract types, labels, and formats of variables in the current data set.;
ods select none;
proc contents data=&inputData varnum;
    ods output Position = type_label_format_dat;
run;
ods select all;

*** We will need a numeric version of the repetition variable for indexing.
*** If the repetition variable is character, attempt to make a numeric version of it.;
data _NULL_;
    set type_label_format_dat;
    if ( Variable = "&repVar" ) then do;
    call symput("repVarTYPE",Type);
    end;
run;
%if ( &repVarTYPE = Char ) %then %do;
    %let repVar_NUM  = &repVar._NUM;
    %let ERR_DETECTED = 0; * Initialize to 0.;
    data temp;
    set temp;
    &repVar_NUM = input(&repVar,BEST12.);
    if ( _ERROR_ = 1 ) then call symput("ERR_DETECTED",_ERROR_);
    run;
    *** If an error was detected, do not proceed, give an error message.;
    %if ( &ERR_DETECTED ) %then %do;

    *** Delete temporary data sets.;
    proc datasets nolist;
        delete type_label_format_dat temp;
    run;
    quit;
    %put **********************************************;
    %put **********************************************;
    %put MACRO CODEBOOKREPEATEDDATA TERMINATED.;
    %put An attempt to convert the repetition variable &repVar to numeric failed.;
    %put The input data set was &inputData.;
    %put **********************************************;
    %put **********************************************;
    %abort;
    %end;
%end;
%else %let repVar_NUM  = &repVar;

*** Remove the subject ID and the repetition variable from the table.;
data type_label_format_dat;
    set type_label_format_dat;
    if ( ( Variable = "&idVar" )
      OR ( Variable = "&repVar" ) ) then delete;
run;

*** Make a list of the variables.;
proc sql noprint;
    select Variable into :varLIST separated by " " from type_label_format_dat;
quit;

*** Determine the maximum number of repetitions from the maximum value of
*** the numeric form of the repetition variable.;
ods select none;
proc means data=temp MIN MAX;
    var &repVar_NUM;
    ods output Summary = minmax_dat;
run;
ods select all;
data _NULL_;
    set minmax_dat;
    CALL SYMPUT("MAXREPS",strip(&repVar_NUM._Max));
    CALL SYMPUT("MINREPS",strip(&repVar_NUM._Min));
run;

*** Loop over variables in TYPE_LABEL_FORMAT_DAT.
*** Create table rows for each variable in which the variable names
*** have been replaced by their widened versions
*** (exclude the repetition variable itself).
*** Add information on format labels, if any.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until( NOT %length(&vrbl));

    %put &inputData : &i : &vrbl;

    *** Obtain entry for the current variable.;
    data var_dat;
        length Label $ 256.;
        set type_label_format_dat;
        if ( Variable = "&vrbl" );
    run;

    *** Create widened variable name.;
    data var_dat;
        set var_dat;
        Variable = "&vrbl._&FILENAME._&widePrefix.[&MINREPS-&MAXREPS]";
        Label    = catt(Label," : &repVar = [&MINREPS-&MAXREPS]");
    run;

    *** Add information on format labels, if any.;
    %AddInfoOnFormatLabelsIfAny(inputData=var_dat,vrbl=&vrbl._&FILENAME._&widePrefix.[&MINREPS-&MAXREPS]);

    *** Append to output data set.;
    data &outputData;
        set &outputData var_dat;
    run;

    *** Delete temporary data set(s).;
    proc datasets nolist;
        delete var_dat;
        run;
    quit;

    *** Get next variable.;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Delete temporary data sets.;
proc datasets nolist;
    delete temp type_label_format_dat minmax_dat;
run;
quit;

*** Re-order columns in output data set.
*** It would be VERY convenient to have the label and
*** format value labels right next to the variable names.;
data &outputData;
    retain Member Num Variable Label Format FormatValueLabel;
    set &outputData;
run;

%mend CodeBookRepeatedDataOneRepVar;

*** TEST.;
/*
%CodeBookRepeatedDataOneRepVar(inputData=nfgs.fm003,repVar=VISIT_NBR,widePrefix=V,outputData=test_dat);
*/

******************************************************************************;
*** MACRO TO PROCESS REPEATED MEASURES DATA SET, TWO REPETITION VARIABLES.
*** Heavily adapted from the preceding macro CODEBOOKREPEATEDDATAONEREPVAR.
******************************************************************************;

%macro CodeBookRepeatedDataTwoRepVars(inputData=,repVar1=,widePrefix1=,repVar2=,widePrefix2=,outputData=);

options nomprint nonotes;

*** Load data.;
data temp;
    set &inputData;
run;

%local i vrbl varLIST dotIndex FILENAME strLength startIndex finalLength;
%local repVarTYPE ERR_DETECTED MAXREPS MINREPS;

%local idVar;
%let idVar = SUBJECT_ID;

*** Parse out name of input data set, as was done in the macro
*** MERGENONREPEATEDDATAINTOOUTPUT.;
%let dotIndex = %index(&inputData,.);
%if ( &dotIndex = 0 ) %then %let FILENAME = &inputData;
%else %do;
    %let strLength   = %length(&inputData);
    %let startIndex  = %eval( &dotIndex + 1 );
    %let finalLength = %eval( &strLength - &dotIndex );
    %let FILENAME    = %substr(&inputData,&startIndex,&finalLength);
%end;

*** If output data set does not yet exist,
*** initialize it so it can be appended to.;
%if NOT %sysfunc(exist(&outputData)) %then %do;
data &outputData;
    set _NULL_;
run;
%end;

***  Extract types, labels, and formats of variables in the current data set.;
ods select none;
proc contents data=&inputData varnum;
    ods output Position = type_label_format_dat;
run;
ods select all;

*** We will need numeric versions of the repetition variables for indexing during widening.
*** If a repetition variable is character, attempt to make a numeric version of it.
*** REPETITION VARIABLE #1.;
data _NULL_;
    set type_label_format_dat;
    if ( Variable = "&repVar1" ) then do;
    call symput("repVar1TYPE",Type);
    end;
run;
%if ( &repVar1TYPE = Char ) %then %do;
    %let repVar1_NUM  = &repVar1._NUM;
    %let ERR_DETECTED = 0; * Initialize to 0.;
    data temp;
    set temp;
    &repVar1_NUM = input(&repVar1,BEST12.);
    if ( _ERROR_ = 1 ) then call symput("ERR_DETECTED",_ERROR_);
    run;
    *** If an error was detected, do not proceed, give an error message.;
    %if ( &ERR_DETECTED ) %then %do;

    *** Delete temporary data sets.;
    proc datasets nolist;
        delete type_label_format_dat temp;
    run;
    quit;
    %put **********************************************;
    %put **********************************************;
    %put MACRO WIDENDATASET TERMINATED.;
    %put An attempt to convert the repetition variable &repVar1 to numeric failed.;
    %put The input data set was &inputData.;
    %put **********************************************;
    %put **********************************************;
    %abort;
    %end;
%end;
%else %let repVar1_NUM  = &repVar1;

*** We will need numeric versions of the repetition variables for indexing during widening.
*** If a repetition variable is character, attempt to make a numeric version of it.
*** REPETITION VARIABLE #2.;
data _NULL_;
    set type_label_format_dat;
    if ( Variable = "&repVar2" ) then do;
    call symput("repVar2TYPE",Type);
    end;
run;
%if ( &repVar2TYPE = Char ) %then %do;
    %let repVar2_NUM  = &repVar2._NUM;
    %let ERR_DETECTED = 0; * Initialize to 0.;
    data temp;
    set temp;
    &repVar2_NUM = input(&repVar2,BEST12.);
    if ( _ERROR_ = 1 ) then call symput("ERR_DETECTED",_ERROR_);
    run;
    *** If an error was detected, do not proceed, give an error message.;
    %if ( &ERR_DETECTED ) %then %do;

    *** Delete temporary data sets.;
    proc datasets nolist;
        delete type_label_format_dat temp;
    run;
    quit;
    %put **********************************************;
    %put **********************************************;
    %put MACRO WIDENDATASET TERMINATED.;
    %put An attempt to convert the repetition variable &repVar2 to numeric failed.;
    %put The input data set was &inputData.;
    %put **********************************************;
    %put **********************************************;
    %abort;
    %end;
%end;
%else %let repVar2_NUM  = &repVar2;

*** Remove the subject ID and the repetition variables from the table.;
data type_label_format_dat;
    set type_label_format_dat;
    if ( ( Variable = "&idVar" )
      OR ( Variable = "&repVar1" )
      OR ( Variable = "&repVar2" ) ) then delete;
run;

*** Make a list of the variables.;
proc sql noprint;
    select Variable into :varLIST separated by " " from type_label_format_dat;
quit;

*** Determine the minimum and maximum values of repetition variable #1.;
ods select none;
proc means data=temp MIN MAX;
    var &repVar1_NUM;
    ods output Summary = max_dat1;
run;
ods select all;
data _NULL_;
    set max_dat1;
    CALL SYMPUT("MAXREPS1",strip(&repVar1_NUM._Max));
    CALL SYMPUT("MINREPS1",strip(&repVar1_NUM._Min));
run;
%let NUMREPS1 = %eval( &MAXREPS1 - &MINREPS1 + 1 );

*** Determine the minimum and maximum values of repetition variable #2.;
ods select none;
proc means data=temp MIN MAX;
    var &repvar2_NUM;
    ods output Summary = max_dat1;
run;
ods select all;
data _NULL_;
    set max_dat1;
    CALL SYMPUT("MAXREPS2",strip(&repvar2_NUM._Max));
    CALL SYMPUT("MINREPS2",strip(&repVar2_NUM._Min));
run;
%let NUMREPS2 = %eval( &MAXREPS2 - &MINREPS2 + 1 );
run;

*** Loop over variables in TYPE_LABEL_FORMAT_DAT.
*** Create table rows for each variable in which the variable names
*** have been replaced by their widened versions (do not modify the name
*** of the repetition variables).
*** Add information on format labels, if any.;
%let i = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until( NOT %length(&vrbl));

    %put &inputData : &i : &vrbl;

    *** Obtain entry for the current variable.;
    data var_dat;
        length Label $ 256.;
        set type_label_format_dat;
        if ( Variable = "&vrbl" );
    run;

    *** Create widened variable name.;
    data var_dat;
        set var_dat;
        Variable = "&vrbl._&FILENAME._&widePrefix1.[&MINREPS1-&MAXREPS1]_&widePrefix2.[&MINREPS2-&MAXREPS2]";
        Label    = catt(Label," : &repVar1 = [&MINREPS1-&MAXREPS1] : &repVar2 = [&MINREPS2-&MAXREPS2]");
    run;

    *** Add information on format labels, if any.;
    %AddInfoOnFormatLabelsIfAny(inputData=var_dat,vrbl=&vrbl._&FILENAME._&widePrefix1.[&MINREPS1-&MAXREPS1]_&widePrefix2.[&MINREPS2-&MAXREPS2]);

    *** Append to output data set.;
    data &outputData;
        set &outputData var_dat;
    run;

    *** Delete temporary data set(s).;
    proc datasets nolist;
        delete var_dat;
        run;
    quit;

    *** Get next variable.;
    %let i = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Delete temporary data sets.;
proc datasets nolist;
    delete temp type_label_format_dat minmax_dat max_dat1;
run;
quit;

*** Re-order columns in output data set.
*** It would be VERY convenient to have the label and
*** format value labels right next to the variable names.;
data &outputData;
    retain Member Num Variable Label Format FormatValueLabel;
    set &outputData;
run;

%mend CodeBookRepeatedDataTwoRepVars;

******************************************************************************;
******************************************************************************;
*** MACRO TO FIX PRESUMED CONVERSION ERRORS IN FORM 016.
******************************************************************************;
******************************************************************************;

%macro FixConversionErrors();

options mprint notes;

*** Determine number of observations in input data set.;
%let dsid = %sysfunc(open(unit_conversion_fixes));
%let num  = %sysfunc(attrn(&dsid,nobs));
%let rc   = %sysfunc(close(&dsid));

*** Loop over observations.;
%do obsNum = 1 %to &num;

    *** Extract OBSNUM-th row from the UNIT_CONVERSION_FIXES data set.;
    data unit_conversion_fixes_subset;
        length cSUBJECT_ID $ 7.;
        set unit_conversion_fixes;
        if ( _N_ = &obsNum );
        cSUBJECT_ID = STRIP(PUT(SUBJECT_ID, z7.));
        drop SUBJECT_ID;
    run;

	*** Convert SUBJECT_ID to character.;
    data unit_conversion_fixes_subset;
        set unit_conversion_fixes_subset;
        rename cSUBJECT_ID = SUBJECT_ID;
    run;


    *** Determine variable name.;
	%let VRBL =;
	data _NULL_;
	    set unit_conversion_fixes_subset;
		call symput("VRBL",Variable);
	run;
	%put &VRBL;

	*** Parse variable name to determine names of measurements #1-#3.;
	%let baseName = %substr(&VRBL,5);
	%put &baseName;

	*** Extract current value for given subject and variable from FM016 data set.;
	data fm016_subset;
	    set fm016(keep=SUBJECT_ID &baseName.1-&baseName.3);
	run;

	*** Constrain the FM016_SUBSET data to the current subject.;
	proc sort data=fm016_subset; by SUBJECT_ID; run;
	data fm016_subset;
	    merge fm016_subset unit_conversion_fixes_subset(in=in2);
		by SUBJECT_ID;
		if ( in2 );
	run;

	*** Determine whether the variable is in CM or MM from the Label.;
	data fm016_subset;
	    set fm016_subset;
		if      ( index(lowcase(Label),"cm") > 0 ) then cmOrMm = "CM";
		else if ( index(lowcase(Label),"mm") > 0 ) then cmOrMm = "MM";
		else                                            cmOrMm = "XX";
	run;

	*** Attempt conversion fix as was done in the MS Excel file.;
	data fm016_subset;
	    set fm016_subset;
		if ( cmOrMm = "CM" ) then do;
		    if ( OutlierStatus = "Too Low" ) then do;
                rescaledVal1 = &baseName.1 * 2.54;
                rescaledVal2 = &baseName.2 * 2.54;
                rescaledVal3 = &baseName.3 * 2.54;
			end;
		    if ( OutlierStatus = "Too High" ) then do;
                rescaledVal1 = &baseName.1 / 2.54;
                rescaledVal2 = &baseName.2 / 2.54;
                rescaledVal3 = &baseName.3 / 2.54;
			end;
		end;
		else if ( cmOrMm = "MM" ) then do;
		    if ( OutlierStatus = "Too Low" ) then do;
                rescaledVal1 = &baseName.1 * 25;
                rescaledVal2 = &baseName.2 * 25;
                rescaledVal3 = &baseName.3 * 25;
			end;
		    if ( OutlierStatus = "Too High" ) then do;
                rescaledVal1 = &baseName.1 / 25;
                rescaledVal2 = &baseName.2 / 25;
                rescaledVal3 = &baseName.3 / 25;
			end;
		end;
	run;

	*** Compute average of the rescaled values, and if this is between
	*** the lower and upper extremes set a flag FIXCONFIRMED to 1. Again,
	*** this is re-doing what was done manually in the MS Excel file.;
	data fm016_subset;
	    set fm016_subset;
		rescaledAvg = mean(rescaledVal1,rescaledVal2,rescaledVal3);
		if ( LowerExtreme <= rescaledAvg <= UpperExtreme ) then fixConfirmed = 1;
		else                                                    fixConfirmed = 0;
		call symput("FIXCONFIRMED",fixConfirmed);
	run;
	
    *** If the fix is confirmed, replace measurements #1-#3 with
	*** the rescaled values.;
	%if ( &FIXCONFIRMED = 1 ) %then %do;
	proc sort data=fm016; by SUBJECT_ID; run;
	data fm016;
	    merge fm016 fm016_subset(keep=SUBJECT_ID rescaledVal1-rescaledVal3 in=in2);
		by SUBJECT_ID;
		if ( in2 ) then do;
		    &baseName.1 = rescaledVal1;
		    &baseName.2 = rescaledVal2;
		    &baseName.3 = rescaledVal3;
		end;
		drop rescaledVal1-rescaledVal3;
	run;
	%end;

	*** Delete temporary data set(s).;
	proc datasets nolist;
	    delete unit_conversion_fixes_subset fm016_subset;
	run;
	quit;
%end;

%mend FixConversionErrors;

******************************************************************************;
******************************************************************************;
*** MACRO TO CREATE AVERAGED VARIABLES FOR TRIPLICATE ANTHROPOMETRIC VARIABLES.
*** ASSUMES THAT THE DATA SET list_of_tolerances HAS ALREADY BEEN READ INTO
*** SAS.
******************************************************************************;
******************************************************************************;

%macro ComputeAverageOfTriplicates(inputData=,theForm=,tripLIST=);

options nomprint nonotes;

*** Make temporary list of variables with their labels.;
ods select none;
proc contents data=&inputData varnum;
    ods output Position = vartab;
run;
ods select all;

*** Loop over triplicate variables.;
%let i = 1;
%let TRIPVAR = %scan(&tripLIST,&i);
%do %until( NOT %length(&TRIPVAR) );

    *** Get label for current triplicate variable.
    *** From this, parse together the label for the AVG variable.;
    %let theLabel =;
    data _NULL_;
        length beforeText afterText avgLabel $ 128.;
        set vartab(keep=Variable Label);
        Label = strip(Label);
        if ( Variable = "&TRIPVAR.3" );
        threeIndex = index(Label,' 3');
        strLength  = length(Label);
        beforeText = substr(Label,1,threeIndex-1);
        afterText  = substr(Label,threeIndex+3,strLength-threeIndex-2);
        avgLabel   = cat(strip(beforeText),' Avg ',strip(afterText));
        call symput("theLabel",avgLabel);
        call symput("lenLabel",length(avgLabel));
    run;
    %put "&i : &TRIPVAR : &theLabel";

    *** Get the tolerance for the current triplicate variable.
    *** This tolerance MAY be specific to the Study Form number.;
    %let TOL =;
    data _NULL_;
        set list_of_tolerances;
        if ( ( FormNum = "&theForm" ) AND ( varBaseName = "&TRIPVAR" ) ) then call symput("TOL",Tolerance);
    run;

    *** Make sure TOL is non-blank.;
    %if ( &TOL = ) %then %do;
        %put "*********************************************";
        %put "*********************************************";
        %put "*********************************************";
        %put "ERROR: Unable to find tolerance for &TRIPVAR";
        %put "*********************************************";
        %put "*********************************************";
        %put "*********************************************";
        %abort cancel;
    %end;

    data &inputData;
        set &inputData;

        *** Compute pairwise differences.;
        absDiff21 = abs(&TRIPVAR.2-&TRIPVAR.1);
        absDiff31 = abs(&TRIPVAR.3-&TRIPVAR.1);
        absDiff32 = abs(&TRIPVAR.3-&TRIPVAR.2);

        *** If there are three values and they are equidistant,
        *** or if all three pairwise differences are greater than tolerance,
        *** average all three values and flag for further inspection.;
        &TRIPVAR._EQUIDIST3 = .; * Initialize.;
        &TRIPVAR._BIGDIFFS3 = .; * Initialize.;
        if ( ( ( absDiff21 ^= . ) AND ( absDiff31 ^= . ) AND ( absDiff32 ^= . ) AND ( absDiff21 = absDiff31 ) AND ( absDiff32 = 2*absDiff21 ) )
          OR ( ( absDiff21 ^= . ) AND ( absDiff31 ^= . ) AND ( absDiff32 ^= . ) AND ( absDiff21 = absDiff32 ) AND ( absDiff31 = 2*absDiff21 ) )
          OR ( ( absDiff21 ^= . ) AND ( absDiff31 ^= . ) AND ( absDiff32 ^= . ) AND ( absDiff31 = absDiff32 ) AND ( absDiff21 = 2*absDiff31 ) ) ) then &TRIPVAR._EQUIDIST3 = 1;
        if ( ( absDiff21 > &TOL ) AND ( absDiff31 > &TOL ) AND ( absDiff32 > &TOL ) ) then &TRIPVAR._BIGDIFFS3 = 1;

        *** Compute Avg_&TRIPVAR as the mean of the two closest measurements.
        *** If the three measurements are equidistant, average all three.;
        if ( &TRIPVAR._EQUIDIST3 = 1 )                                        then Avg_&TRIPVAR = mean(&TRIPVAR.1,&TRIPVAR.2,&TRIPVAR.3);
        else if ( ( absDiff21 <= absDiff31 ) AND ( absDiff21 <= absDiff32 ) ) then Avg_&TRIPVAR = mean(&TRIPVAR.1,&TRIPVAR.2);
        else if ( ( absDiff31 <= absDiff21 ) AND ( absDiff31 <= absDiff32 ) ) then Avg_&TRIPVAR = mean(&TRIPVAR.1,&TRIPVAR.3);
        else                                                                       Avg_&TRIPVAR = mean(&TRIPVAR.2,&TRIPVAR.3);

        *** Apply label to the AVG variable.;
        %if ( &lenLabel > 0 ) %then %do;
        label Avg_&TRIPVAR = "&theLabel";
        %end;

        *** Apply label to the &TRIPVAR._EQUIDIST3 and &TRIPVAR._BIGDIFFS3 variables.;
        label &TRIPVAR._EQUIDIST3 = "&TRIPVAR - Triplicates are equidistant?"
              &TRIPVAR._BIGDIFFS3 = "&TRIPVAR - All three pairwise differences are greater than tolerance?";
        drop absDiff21 absDiff31 absDiff32 &TRIPVAR._EQUIDIST3 &TRIPVAR._BIGDIFFS3;
    run;

    *** Get next triplicate variable.;
    %let i = %eval( &i + 1 );
    %let TRIPVAR = %scan(&tripLIST,&i);
%end; * End DO loop over triplicate variables.;

*** Delete temporary data set(s).;
proc datasets nolist;
    delete vartab;
run;
quit;

options notes;

%mend ComputeAverageOfTriplicates;

******************************************************************************;
******************************************************************************;
*** Macro to zoom in on (focus in on, cone down on, blow up) a portion of a
*** Kaplan Meier Plot.
***
*** XMIN : The X axis lower bound of the cone down area.
*** XMAX : The X axis upper bound of the cone down area.
*** YMIN : The Y axis lower bound of the cone down area.
*** YMAX : The Y axis upper bound of the cone down area.
***
*** Example of usage:
***
***     %ZoomSurvivalPlot(xmin=1000,xmax=2000,ymin=0.5,ymax=0.6);
***
***     proc lifetest data=sashelp.BMT plots=survival(atrisk=0 to 2500 by 500);
***        ods select SurvivalPlot;
***        time T * Status(0);
***        strata Group;
***     run;
***
***
*** Reference:
*** http://support.sas.com/documentation/cdl/en/statug/65328/HTML/default/viewer.htm#statug_templt_sect021.htm
***
*** To return plotting to normal, use:
***
***     proc template;
***        delete Stat.Lifetest.Graphics.ProductLimitSurvival / store=sasuser.templat;
***     run;
***
***     proc lifetest data=sashelp.BMT plots=survival(atrisk=0 to 2500 by 500);
***        ods select SurvivalPlot;
***        time T * Status(0);
***        strata Group;
***     run;
***
*** See the bottom of this web page ("Modifying Graph Titles and Axis Labels"):
*** http://support.sas.com/documentation/cdl/en/statug/65328/HTML/default/viewer.htm#statug_templt_sect012.htm
*** See also:
*** http://support.sas.com/documentation/cdl/en/statug/65328/HTML/default/viewer.htm#statug_templt_sect008.htm
******************************************************************************;
******************************************************************************;

%macro ZoomSurvivalPlot(xmin=,xmax=,ymin=,ymax=);

proc template;
define statgraph Stat.Lifetest.Graphics.ProductLimitSurvival;
   dynamic NStrata xName plotAtRisk plotCensored plotCL plotHW plotEP labelCL labelHW labelEP
      maxTime xtickVals xtickValFitPol method StratumID classAtRisk plotBand plotTest GroupName
      yMin Transparency SecondTitle TestName pValue;
   BeginGraph;
      if (NSTRATA=1)
         if (EXISTS(STRATUMID))
            entrytitle METHOD " " "Survival Estimate" " for " STRATUMID;
         else
            entrytitle METHOD " " "Survival Estimate";
         endif;
         if (PLOTATRISK)
            entrytitle "with Number of Subjects at Risk" / textattrs=GRAPHVALUETEXT;
         endif;
         layout overlay / xaxisopts=(shortlabel=XNAME offsetmin=.05 linearopts=(viewmin=&xmin viewmax=&xmax
            tickvaluelist=XTICKVALS tickvaluefitpolicy=XTICKVALFITPOL)) yaxisopts=(label=
            "Survival Probability" shortlabel="Survival" linearopts=(viewmin=&ymin viewmax=&ymax
            tickvaluelist=(0 .2 .4 .6 .8 1.0)));
            if (PLOTHW=1 AND PLOTEP=0)
               bandplot LimitUpper=HW_UCL LimitLower=HW_LCL x=TIME / modelname="Survival"
                  fillattrs=GRAPHCONFIDENCE name="HW" legendlabel=LABELHW;
            endif;
            if (PLOTHW=0 AND PLOTEP=1)
               bandplot LimitUpper=EP_UCL LimitLower=EP_LCL x=TIME / modelname="Survival"
                  fillattrs=GRAPHCONFIDENCE name="EP" legendlabel=LABELEP;
            endif;
            if (PLOTHW=1 AND PLOTEP=1)
               bandplot LimitUpper=HW_UCL LimitLower=HW_LCL x=TIME / modelname="Survival"
                  fillattrs=GRAPHDATA1 datatransparency=.55 name="HW" legendlabel=LABELHW;
               bandplot LimitUpper=EP_UCL LimitLower=EP_LCL x=TIME / modelname="Survival"
                  fillattrs=GRAPHDATA2 datatransparency=.55 name="EP" legendlabel=LABELEP;
            endif;
            if (PLOTCL=1)
               if (PLOTHW=1 OR PLOTEP=1)
                  bandplot LimitUpper=SDF_UCL LimitLower=SDF_LCL x=TIME / modelname="Survival"
                     display=(outline) outlineattrs=GRAPHPREDICTIONLIMITS name="CL" legendlabel
                     =LABELCL;
               else
                  bandplot LimitUpper=SDF_UCL LimitLower=SDF_LCL x=TIME / modelname="Survival"
                     fillattrs=GRAPHCONFIDENCE name="CL" legendlabel=LABELCL;
               endif;
            endif;
            stepplot y=SURVIVAL x=TIME / name="Survival" rolename=(_tip1=ATRISK _tip2=EVENT)
               tip=(y x Time _tip1 _tip2) legendlabel="Survival";
            if (PLOTCENSORED=1)
               scatterplot y=CENSORED x=TIME / markerattrs=(symbol=plus) name="Censored"
                  legendlabel="Censored";
            endif;
            if (PLOTCL=1 OR PLOTHW=1 OR PLOTEP=1)
               discretelegend "Censored" "CL" "HW" "EP" / location=outside halign=center;
            else
               if (PLOTCENSORED=1)
                  discretelegend "Censored" / location=inside autoalign=(topright bottomleft);
               endif;
            endif;
            if (PLOTATRISK=1)
               innermargin / align=bottom;
                  blockplot x=TATRISK block=ATRISK / repeatedvalues=true display=(values)
                     valuehalign=start valuefitpolicy=truncate labelposition=left labelattrs=
                     GRAPHVALUETEXT valueattrs=GRAPHDATATEXT (size=7pt) includemissingclass=
                     false;
               endinnermargin;
            endif;
         endlayout;
      else
         entrytitle METHOD " " "Survival Estimates";
         if (EXISTS(SECONDTITLE))
            entrytitle SECONDTITLE / textattrs=GRAPHVALUETEXT;
         endif;
         layout overlay / xaxisopts=(shortlabel=XNAME offsetmin=.05 linearopts=(viewmin=&xmin viewmax=&xmax
            tickvaluelist=XTICKVALS tickvaluefitpolicy=XTICKVALFITPOL)) yaxisopts=(label=
            "Survival Probability" shortlabel="Survival" linearopts=(viewmin=&ymin viewmax=&ymax
            tickvaluelist=(0 .2 .4 .6 .8 1.0)));
            if (PLOTHW)
               bandplot LimitUpper=HW_UCL LimitLower=HW_LCL x=TIME / group=STRATUM index=
                  STRATUMNUM modelname="Survival" datatransparency=Transparency;
            endif;
            if (PLOTEP)
               bandplot LimitUpper=EP_UCL LimitLower=EP_LCL x=TIME / group=STRATUM index=
                  STRATUMNUM modelname="Survival" datatransparency=Transparency;
            endif;
            if (PLOTCL)
               if (PLOTBAND)
                  bandplot LimitUpper=SDF_UCL LimitLower=SDF_LCL x=TIME / group=STRATUM index=
                     STRATUMNUM modelname="Survival" display=(outline);
               else
                  bandplot LimitUpper=SDF_UCL LimitLower=SDF_LCL x=TIME / group=STRATUM index=
                     STRATUMNUM modelname="Survival" datatransparency=Transparency;
               endif;
            endif;
            stepplot y=SURVIVAL x=TIME / group=STRATUM index=STRATUMNUM name="Survival"
               rolename=(_tip1=ATRISK _tip2=EVENT) tip=(y x Time _tip1 _tip2);
            if (PLOTCENSORED)
               scatterplot y=CENSORED x=TIME / group=STRATUM index=STRATUMNUM markerattrs=(
                  symbol=plus);
            endif;
            if (PLOTATRISK)
               innermargin / align=bottom;
                  blockplot x=TATRISK block=ATRISK / class=CLASSATRISK repeatedvalues=true
                     display=(label values) valuehalign=start valuefitpolicy=truncate
                     labelposition=left labelattrs=GRAPHVALUETEXT valueattrs=GRAPHDATATEXT (
                     size=7pt) includemissingclass=false;
               endinnermargin;
            endif;
            DiscreteLegend "Survival" / title=GROUPNAME location=outside;
            if (PLOTCENSORED)
               if (PLOTTEST)
                  layout gridded / rows=2 autoalign=(TOPRIGHT BOTTOMLEFT TOP BOTTOM) border=
                     true BackgroundColor=GraphWalls:Color Opaque=true;
                     entry "+ Censored";
                     if (PVALUE < .0001)
                        entry TESTNAME " p " eval (PUT(PVALUE, PVALUE6.4));
                     else
                        entry TESTNAME " p=" eval (PUT(PVALUE, PVALUE6.4));
                     endif;
                  endlayout;
               else
                  layout gridded / rows=1 autoalign=(TOPRIGHT BOTTOMLEFT TOP BOTTOM) border=
                     true BackgroundColor=GraphWalls:Color Opaque=true;
                     entry "+ Censored";
                  endlayout;
               endif;
            else
               if (PLOTTEST)
                  layout gridded / rows=1 autoalign=(TOPRIGHT BOTTOMLEFT TOP BOTTOM) border=
                     true BackgroundColor=GraphWalls:Color Opaque=true;
                     if (PVALUE < .0001)
                        entry TESTNAME " p " eval (PUT(PVALUE, PVALUE6.4));
                     else
                        entry TESTNAME " p=" eval (PUT(PVALUE, PVALUE6.4));
                     endif;
                  endlayout;
               endif;
            endif;
         endlayout;
      endif;
   EndGraph;
end;
run;

%mend ZoomSurvivalPlot;

******************************************************************************;
******************************************************************************;
*** Macro to log-transform and standardize a set of variables.
***
*** INPUTDATA : The input SAS data set.
*** VARLIST   : Desired number of subjects. Default value is 100.
***
*** This macro will NOT overwrite the original values. Instead, new variables
*** will be added to the input data set. They will have
*** the same name as the original variable, except that they will have the
*** following extension to the original name:
***
***     _LOGSTD  : The log-transformed and standardized value.
***
*** The logarithm of the original value is computed, after adding a suitable
*** offset. If the minimum value across observations of a variable is greater
*** than -1, then the offset is the usual +1. Otherwise, the offset is computed
*** by negating the minimum and adding 0.001, so that after adding the offset
*** the minimum value would be 0.001. This prevents taking the log of a
*** non-positive value.
***
*** Then the variance is standardized to unity.
***
*** Note that SAS variables can have names that are up to 32 characters long.
*** This means that in order to use this macro, the SAS variables pointed to
*** by VARLIST must have names with a length of 25 or less.
***
*** Version 16 (8/6/2013): Do not drop the SD, as this may be useful for imputed data.;
******************************************************************************;
******************************************************************************;

%macro LogTransformAndStandardize(inputData=,varLIST=);

%local i vrbl;

%let i    = 1;
%let vrbl = %scan(&varLIST,&i);
%do %until(not %length(&vrbl));

    *** Find the minimum value of the current chemical.;
    ODS SELECT NONE; * Turn displayed output off.;
    proc univariate data=&inputData;
        var &vrbl;
        output out=min_dat min=theMin;
    run;
    ODS SELECT ALL; * Turn displayed output back on.;

    *** If the minimum was greater than -1, then the offset is the usual +1.
    *** Otherwise, the offset is computed by negating the minimum and adding
    *** 0.001, so that after adding the offset the minimum value would be 0.001.;
    data min_dat;
        set min_dat;
        if ( theMin > -1 ) then theOffset = 1;
        else theOffset = (-theMin) + 0.001;
    run;

    *** Take the log of the chemical values after adding the offset.;
    data &inputData;
        set &inputData;
        if ( _N_ = 1 ) then set min_dat;
        &vrbl._logstd = log( &vrbl + theOffset );
        drop theOffset theMin;
    run;

    %let i    = %eval( &i + 1 );
    %let vrbl = %scan(&varLIST,&i);
%end;

*** Standardize.;
ODS SELECT NONE; * Turn displayed output off.;
proc univariate data=&inputData noprint;
    var
    %let i    = 1;
    %let vrbl = %scan(&varLIST,&i);
    %do %until(not %length(&vrbl));
        &vrbl._logstd
        %let i    = %eval( &i + 1 );
        %let vrbl = %scan(&varLIST,&i);
    %end;
    ;
    output out=stndrdz_cf std =
    %let i    = 1;
    %let vrbl = %scan(&varLIST,&i);
    %do %until(not %length(&vrbl));

        &vrbl._std
        %let i    = %eval( &i + 1 );
        %let vrbl = %scan(&varLIST,&i);
    %end;
    ;
run;
ODS SELECT ALL; * Turn displayed output back on.;

*** Version 16 (8/6/2013): Do not drop the SD, as this may be useful for imputed data.;

data &inputData;
    set &inputData;
    if ( _N_ = 1 ) then set stndrdz_cf;
    %let i    = 1;
    %let vrbl = %scan(&varLIST,&i);
    %do %until(not %length(&vrbl));

        &vrbl._logstd = &vrbl._logstd / &vrbl._std;
        %let i    = %eval( &i + 1 );
        %let vrbl = %scan(&varLIST,&i);
    %end;
run;

*** Delete temporary data sets.;
proc datasets nolist;
    delete stndrdz_cf min_dat;
run;
quit;

%mend LogTransformAndStandardize;

******************************************************************************;
******************************************************************************;
*** Macro to create a non-repeated measures data set, i.e., one observation
*** per subject.
***
*** OUTPUTDATA     : Desired name for the output SAS data set.
*** NUMBSUBJECTS   : Desired number of subjects. Default value is 100.
*** RANDOMSEED     : Random seed for the random number generator.
***                  Set this to some integer for replicability.
*** MISSINGNESSPCT : A continuous number between 0 and 1. If set to a non-zero
***                  number, a proportion of observations set by MISSINGNESSPCT
***                  will be missing in GROUP, HEIGHT, and COLOR.
***                  The default value is 0 (i.e., no missing values).
***
*** The output data set will contain three categorical variables, GROUP, COLOR,
*** and SIZE, and two continuous variables, WEIGHT and HEIGHT.
*** It will also contain a variable ID which could be considered a subject ID.
*** The GROUP variable could be considered to be an exposure variable.
*** 
*** Reference: http://blogs.sas.com/content/iml/2011/08/24/how-to-generate-random-numbers-in-sas/;
******************************************************************************;
******************************************************************************;

%macro MakeSyntheticNonrepeatedData(outputData=,numSubjects=100,randomSeed=NULL,missingnessPct=0);

data &outputData;
    retain id Group Height Weight Color Size Group;
    length Color Size $  8.;
    length Group      $ 13.;

    %if ( &randomSeed ^= NULL ) %then %do;
    call streaminit(&randomSeed); /* set random number seed */
    %end;

    do id = 1 to &numSubjects;
        * Generate two continuous random variables HEIGHT and WEIGHT.;
        Height = 60  + rand("Uniform")*10; /* Height ~ U[0,1] */
        Weight = 100 + rand("Uniform")*50; /* Weight ~ U[0,1] */

        * Generate categorical random variable COLOR.;
        ran1 = rand("Uniform"); /* ran1 ~ U[0,1] */
        if      ( ran1 < 1/3 ) then Color = "A: Red";
        else if ( ran1 < 2/3 ) then Color = "B: Green";
        else                        Color = "C: Blue";

        * Generate categorical random variable SIZE.;
        ran2 = rand("Uniform"); /* ran2 ~ U[0,1] */
        if ( ran2 < 0.5 ) then Size = "A: Small";
        else                   Size = "B: Big";

        * Generate categorical random variable GROUP.;
        ran3 = rand("Uniform"); /* ran3 ~ U[0,1] */
        if ( ran3 < 0.5 ) then Group = "A: NonExposed";
        else                   Group = "B: Exposed";

        *** Induce missing values in GROUP, HEIGHT, and COLOR.;
        ran4 = rand("Uniform"); /* ran4 ~ U[0,1] */
        if ( ran4 < &missingnessPct ) then Group = " ";
        ran5 = rand("Uniform"); /* ran5 ~ U[0,1] */
        if ( ran5 < &missingnessPct ) then Height = .;
        ran6 = rand("Uniform"); /* ran6 ~ U[0,1] */
        if ( ran6 < &missingnessPct ) then Color = " ";

        output;
    end;
    keep id Height Weight Color Size Group;
run;

%mend MakeSyntheticNonrepeatedData;

******************************************************************************;
******************************************************************************;
*** Macro to create a synthetic data set WITH REPEATED MEASURES, i.e., several
*** observations per subject.
***
*** OUTPUTDATA      : Desired name for the output SAS data set.
*** NUMBSUBJECTS    : Desired number of subjects. Default value is 100.
*** RANDOMSEED      : Random seed for the random number generator.
***                   Set this to some integer for replicability.
*** NUMBEROFREPEATS : Desired number of repeats per subject.
***                   Default value is 1.
*** MISSINGNESSPCT  : A continuous number between 0 and 1. If set to a non-zero
***                   number, a proportion of observations set by MISSINGNESSPCT
***                   will be missing in GROUP, HEIGHT, and COLOR.
***                   The default value is 0 (i.e., no missing values).
***
*** The output data set will contain three categorical variables, GROUP, COLOR,
*** and SIZE, and two continuous variables, WEIGHT and HEIGHT.
*** It will also contain a variable ID which could be considered a subject ID.
*** Finally, it will contain a variable REPEAT, to indicate the repetition
*** number with subject. This variable will run from 1 to NUMBEROFREPEATS.
*** The GROUP variable could be considered to be an exposure variable.
*** 
*** Reference: http://blogs.sas.com/content/iml/2011/08/24/how-to-generate-random-numbers-in-sas/;
******************************************************************************;
******************************************************************************;

%macro MakeSyntheticRepeatedData(outputData=,numSubjects=100,numberOfRepeats=1,randomSeed=NULL,missingnessPct=0);

*** First make a temporary non-repeated measures data set with NUMSUBJECTS * NUMBEROFREPEATS number of
*** observations.;
%let totNumObs = %eval( &numSubjects * &numberOfRepeats );
%MakeSyntheticNonrepeatedData(outputData=&outputData,numSubjects=&totNumObs,randomSeed=&randomSeed,missingnessPct=&missingnessPct);

*** Insert a ROWNUM variable showing the observation number.;
data &outputData;
    set &outputData;
    rowNum = _N_;
run;

*** Create a data set that has the desired IDs and REPEAT variable.
*** This will be merged into the temporary non-repeated measures data set just created.;
data ids_and_repeat_dat;
    do id_tmp = 1 to &numSubjects;
        do repeat = 1 to &numberOfRepeats;
            rowNum = ( ( id_tmp - 1 ) * &numberOfRepeats ) + repeat;
            output;
        end;
    end;
run;

*** Merge the desired IDs and REPEAT variable into the non-repeated measures data set.;
data &outputData;
    retain id_tmp  repeat;
    merge &outputData ids_and_repeat_dat;
    by rowNum;
    drop id rowNum;
run;

*** Rename ID_TMP;
data &outputData;
    set &outputData;
    rename id_tmp = id;
run;

*** Delete temporary data sets.;
proc datasets nolist;
    delete ids_and_repeat_dat;
run;
quit;

%mend MakeSyntheticRepeatedData;

******************************************************************************;
******************************************************************************;
*** Macro to clear RESULTS and LOG.
***
*** Reference: http://listserv.uga.edu/cgi-bin/wa?A2=ind0310c&L=sas-l&P=35375;
*** http://support.sas.com/kb/4/159.html;
***
*** Note: has side effect of creating an HTML file in the folder in which
*** SAS was invoked. Have not yet figured out how to refresh the output window
*** without causing this.
******************************************************************************;
******************************************************************************;

%macro cls();
    dm  'clear output';
    dm  'clear log';
    ods html close; 
    ods preferences;
    dm 'odsresults; clear';
    ods html newfile=none; * Reference: https://communities.sas.com/thread/30747?start=0&tstart=0.;
%mend cls;

******************************************************************************;
******************************************************************************;
*** Macro to count number of unique occurrences of a given variable.
*** Result is displayed in the SAS log.
***
*** INPUTDATA   : The input SAS data set.
*** VRBL        : Name of a variable, either CHARACTER or NUMERIC.
***
*** The number of unique occurences of a given variable is displayed in the
*** SAS LOG. Also displayed are the maximum number of repeated values of the
*** variable.
******************************************************************************;
******************************************************************************;

%macro CountUniqueVals(inputData=,vrbl=);

%local num_obs num_var MaxNumObs;

options nonotes nomprint;

***********************************************************;
*** Remove missing values of &vrbl.;
***********************************************************;

data CountUniqueVals_subset;
    set &inputData;

    *** Keep only non-missing values.;
    %let dsid = %sysfunc(open(&inputData));
    %let vnum = %sysfunc(varnum(&dsid,&vrbl));
    %if ( %sysfunc(vartype(&dsid,&vnum)) = C ) %then %do;
    if ( &vrbl ^= " " );   * VAR is of CHARACTER type.;
        if ( &vrbl ^= " " );
    %end;
    %else %do;
        if ( &vrbl ^= . ); * VAR is of NUMERIC type.;
    %end;
    %let rc = %sysfunc(close(&dsid));
    keep &vrbl;
run;

***********************************************************;
*** Use PROC SQL to obtain a data set containing only unique
*** values.;
***********************************************************;

proc sql noprint;
    create table uniquevar as
    select distinct a.&vrbl
    from CountUniqueVals_subset a;
quit;

***********************************************************;
*** 1/31/2013: Use PROC CONTENTS to count the number of observations.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc contents data=&inputData;
    ods output Contents.DataSet.Attributes = Attributes;
run;
ODS SELECT ALL; * Turn displayed output back on.;

data var_tmp;
    set Attributes;
    if ( Label2 = "Observations" );
    rename nValue2 = n;
    keep nValue2;
run;
data _NULL_;
    set var_tmp;
    CALL SYMPUT("num_obs",n);
run;

***********************************************************;
*** Use NOBS= option to count the number of unique occurences
*** of the variable.;
***********************************************************;

data var_tmp;
    set uniquevar nobs=nobs;
    n = nobs;
run;
data _NULL_;
    set var_tmp;
    CALL SYMPUT("num_var",n);
run;

***********************************************************;
*** Reference: http://www.teebark.com/index.php/mainframe/frequency-count-with-sas/;
***********************************************************;

proc freq data=CountUniqueVals_subset noprint;
    table &vrbl / out=outfreq;
run;

***********************************************************;
*** Reference: http://www.sascommunity.org/wiki/Fun_with_PROC_SQL_Summary_Functions;
***********************************************************;

PROC SQL;
   create table maxcount as
   SELECT max(COUNT) AS maximum
           Label="Maximum Count"
   FROM outfreq;
QUIT;
data _null_;
    set maxcount;
    CALL SYMPUT("MaxNumObs",maximum);
run;

***********************************************************;
*** Delete temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist; delete CountUniqueVals_subset var_tmp uniquevar outfreq maxcount Attributes; run; quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes mprint;

***********************************************************;
*** Display counts in the SAS log.;
***********************************************************;

%put *** SAS Data Set &inputData;
%put *** Number of observations               = %kleft(&num_obs);
%put *** Number of unique occurences of &vrbl = %kleft(&num_var);
%put *** Maximum number of repeats of &vrbl   = %kleft(&MaxNumObs);

%mend CountUniqueVals;

******************************************************************************;
******************************************************************************;
*** Macro to make a macro variable array containing a list of NON-MISSING
*** values of a variable in a given data set.
***
*** INPUTDATA   : The input SAS data set.
*** VRBL        : Name of a variable, either CHARACTER or NUMERIC.
*** OUTLISTNAME : Desired name for output macro array. This will contain
***               a sorted list of the unique values of VRBL.
***
*** Note: the list in OUTLISTNAME will be delimited by (single) spaces.
*** This means that if VRBL is a CHARACTER variable that sometimes contains
*** words separated by spaces, the internal spaces may be confused with
*** the spaces used for delimiting. Consider using underscores rather than
*** spaces in such cases.
***
******************************************************************************;
******************************************************************************;
%macro MakeMacroVarArray(inputData=,vrbl=,outListName=);

%local dsid vnum rc;

***********************************************************;
*** Remove missing values of &vrbl.;
***********************************************************;

data MakeMacroVarArray_subset;
    set &inputData;

    *** Keep only non-missing values.;
    %let dsid = %sysfunc(open(&inputData));
    %let vnum = %sysfunc(varnum(&dsid,&vrbl));
    %if ( %sysfunc(vartype(&dsid,&vnum)) = C ) %then %do;
    if ( &vrbl ^= " " );   * VAR is of CHARACTER type.;
        if ( &vrbl ^= " " );
    %end;
    %else %do;
        if ( &vrbl ^= . ); * VAR is of NUMERIC type.;
    %end;
    %let rc = %sysfunc(close(&dsid));
    keep &vrbl;
run;

***********************************************************;
*** Uniquify.  I.e., make temporary data set containing only
*** one observation per &var.;
***********************************************************;

proc sort data=MakeMacroVarArray_subset NODUPKEY;
    by &vrbl;
run;

***********************************************************;
*** Make GLOBAL macro variable array containing list of &vars.;
*** Reference for GLOBAL macro variable array:
*** http://support.sas.com/documentation/cdl/en/mcrolref/61885/HTML/default/viewer.htm#a001072111.htm#a001072130;
***********************************************************;

%global &outListName;
proc sql noprint;
    select &vrbl into :&outListName separated by " "
        from MakeMacroVarArray_subset;
quit;

***********************************************************;
*** Delete temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist; delete MakeMacroVarArray_subset; run; quit;
ODS SELECT ALL; * Turn displayed output back on.;

%mend MakeMacroVarArray;

******************************************************************************;
******************************************************************************;
*** Macro to compute tertiles.
***
*** INPUTDATA : The input SAS data set.
*** VRBL      : Name of a CONTINUOUS variable, from which tertiles will be
***             computed.
***
*** Seven new variables will be added to the input data set. They will have
*** the same name as the original variable, except that they will have the
*** following extensions to the original name:
***
***     _N  : The number of observations used to compute cutoffs.
***     _P1 : The first threshold cutoff (first tertile).
***     _P2 : The second threshold cutoff (second tertile).
***     _T  : NUMERIC categorical variable indicating the tertile.
***     _T1 : Indicator variable showing whether an observation is in the 1st third.
***     _T2 : Indicator variable showing whether an observation is in the 2nd third.
***     _T3 : Indicator variable showing whether an observation is in the 3rd third.
***
*** Note that SAS variables can have names that are up to 32 characters long.
*** This means that in order to use this macro, the SAS variable pointed to
*** by VRBL must have a name with a length of 29 or less.
******************************************************************************;
******************************************************************************;

%macro ComputeTertiles(inputData=,vrbl=);

***********************************************************;
*** Compute tertile cutoffs.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc univariate data=&inputData;
    var &vrbl;
    output out=tertiles_temp pctlpts=0 33 67 100 pctlpre=p; * For cutoffs.;
    ods output Univariate.&vrbl..Moments = Moments;   * For N.;
run;
ODS SELECT ALL; * Turn displayed output back on.;

***********************************************************;
*** Reformat the number of observations used to compute tertiles.;
***********************************************************;

data Moments;
    set Moments;
    if ( Label1 = "N" );
    rename nValue2 = &vrbl._N;
    keep nValue2;
run;

***********************************************************;
*** Copy tertile cutoffs into input data set.;
***********************************************************;

data &inputData;
    set &inputData;
    if ( _N_ = 1 ) then set Moments;
run;

***********************************************************;
*** Define new tertile variables in input data set.;
***********************************************************;

data &inputData;
    set &inputData;
    if ( _N_ = 1 ) then set tertiles_temp;

    &vrbl._T  = .;
    &vrbl._T1 = .;
    &vrbl._T2 = .;
    &vrbl._T3 = .;
    if ( &vrbl NE . ) then do;
        &vrbl._T1 = 0;
        &vrbl._T2 = 0;
        &vrbl._T3 = 0;
        if ( &vrbl <  p33 ) then do;
            &vrbl._T1 = 1;
            &vrbl._T  = 1;
        end;
        if ( ( &vrbl >= p33 ) AND ( &vrbl < p67 ) ) then do;
            &vrbl._T2 = 1;
            &vrbl._T  = 2;
        end;
        if ( &vrbl >= p67 ) then do;
            &vrbl._T3 = 1;
            &vrbl._T  = 3;
        end;
    end;
    rename p0   = &vrbl._Mn
           p33 = &vrbl._P1
           p67 = &vrbl._P2
           p100 = &vrbl._Mx;
run;

***********************************************************;
*** Remove temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets; delete Moments tertiles_temp; run; quit;
ODS SELECT ALL; * Turn displayed output back on.;

%mend ComputeTertiles;

******************************************************************************;
******************************************************************************;
*** Macro to compute quartiles.;
***
*** INPUTDATA : The input SAS data set.
*** VRBL      : Name of a CONTINUOUS variable, from which quartiles will be
***             computed.
***
*** Nine new variables will be added to the input data set. They will have
*** the same name as the original variable, except that they will have the
*** following extensions to the original name:
***
***     _N  : The number of observations used to compute cutoffs.
***     _P1 : The first threshold cutoff (first quartile).
***     _P2 : The second threshold cutoff (second quartile).
***     _P2 : The third threshold cutoff (third quartile).
***     _Q  : NUMERIC categorical variable indicating the quartile.
***     _Q1 : Indicator variable showing whether an observation is in the 1st quarter.
***     _Q2 : Indicator variable showing whether an observation is in the 2nd quarter.
***     _Q3 : Indicator variable showing whether an observation is in the 3rd quarter.
***     _Q4 : Indicator variable showing whether an observation is in the 4th quarter.
***
*** Note that SAS variables can have names that are up to 32 characters long.
*** This means that in order to use this macro, the SAS variable pointed to
*** by VRBL must have a name with a length of 29 or less.
******************************************************************************;
******************************************************************************;

%macro ComputeQuartiles(inputData=,vrbl=);

***********************************************************;
*** Compute quartile cutoffs.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc univariate data=&inputData;
    var &vrbl;
    output out=quartiles_temp pctlpts=0 25 50 75 100 pctlpre=p; * For cutoffs.;
    ods output Univariate.&vrbl..Moments = Moments;       * For N.;
run;
ODS SELECT ALL; * Turn displayed output back on.;

***********************************************************;
*** Reformat the number of observations used to compute quartiles.;
***********************************************************;

data Moments;
    set Moments;
    if ( Label1 = "N" );
    rename nValue2 = &vrbl._N;
    keep nValue2;
run;

***********************************************************;
*** Copy quartile cutoffs into input data set.;
***********************************************************;

data &inputData;
    set &inputData;
    if ( _N_ = 1 ) then set Moments;
run;

***********************************************************;
*** Define new quartile variables in input data set.;
***********************************************************;

data &inputData;
    set &inputData;
    if ( _N_ = 1 ) then set quartiles_temp;

    &vrbl._Q  = .;
    &vrbl._Q1 = .;
    &vrbl._Q2 = .;
    &vrbl._Q3 = .;
    &vrbl._Q4 = .;
    if ( &vrbl NE . ) then do;
        &vrbl._Q1 = 0;
        &vrbl._Q2 = 0;
        &vrbl._Q3 = 0;
        &vrbl._Q4 = 0;
        if ( &vrbl <  p25 ) then do;
            &vrbl._Q1 = 1;
            &vrbl._Q  = 1;
        end;
        if ( ( &vrbl >= p25 ) AND ( &vrbl < p50 ) ) then do;
            &vrbl._Q2 = 1;
            &vrbl._Q  = 2;
        end;
        if ( ( &vrbl >= p50 ) AND ( &vrbl < p75 ) ) then do;
            &vrbl._Q3 = 1;
            &vrbl._Q  = 3;
        end;
        if ( &vrbl >= p75 ) then do;
            &vrbl._Q4 = 1;
            &vrbl._Q  = 4;
        end;
    end;
    rename p0   = &vrbl._Mn
           p25  = &vrbl._P1
           p50  = &vrbl._P2
           p75  = &vrbl._P3
           p100 = &vrbl._Mx;
run;

***********************************************************;
*** Remove temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets; delete Moments quartiles_temp; run; quit;
ODS SELECT ALL; * Turn displayed output back on.;

%mend ComputeQuartiles;

******************************************************************************;
******************************************************************************;
*** Macro to compute quartiles using quartile thresholds computed from some
*** other variable.
***
*** INPUTDATA : The input SAS data set.
*** VRBL1     : Name of a CONTINUOUS variable, from which quartiles will be
***             computed.
*** VRBL2     : Name of a CONTINUOUS variable, whose quartile thresholds will
***             be used to categorize VRBL1 into quartiles. The quartile
***             thresholds of VRBl2 must have already been pre-computed with
***             a call to the original macro COMPUTEQUARTILES.
***
*** FIVE new variables will be added to the input data set. They will have
*** the same name as the original variable, except that they will have the
*** following extensions to the original name:
***
***     _Q  : NUMERIC categorical variable indicating the quartile.
***     _Q1 : Indicator variable showing whether an observation is in the 1st quarter.
***     _Q2 : Indicator variable showing whether an observation is in the 2nd quarter.
***     _Q3 : Indicator variable showing whether an observation is in the 3rd quarter.
***     _Q4 : Indicator variable showing whether an observation is in the 4th quarter.
***
*** Note that SAS variables can have names that are up to 32 characters long.
*** This means that in order to use this macro, the SAS variable pointed to
*** by VRBL must have a name with a length of 29 or less.
******************************************************************************;
******************************************************************************;

%macro ComputeQuartiles2(inputData=,vrbl1=,vrbl2=);

***********************************************************;
*** Define new quartile variables in input data set.;
***********************************************************;

data &inputData;
    set &inputData;

    &vrbl1._Q  = .;
    &vrbl1._Q1 = .;
    &vrbl1._Q2 = .;
    &vrbl1._Q3 = .;
    &vrbl1._Q4 = .;
    if ( &vrbl1 NE . ) then do;
        &vrbl1._Q1 = 0;
        &vrbl1._Q2 = 0;
        &vrbl1._Q3 = 0;
        &vrbl1._Q4 = 0;
        if ( &vrbl1 <  &vrbl2._P1 ) then do;
            &vrbl1._Q1 = 1;
            &vrbl1._Q  = 1;
        end;
        if ( ( &vrbl1 >= &vrbl2._P1 ) AND ( &vrbl1 < &vrbl2._P2 ) ) then do;
            &vrbl1._Q2 = 1;
            &vrbl1._Q  = 2;
        end;
        if ( ( &vrbl1 >= &vrbl2._P2 ) AND ( &vrbl1 < &vrbl2._P3 ) ) then do;
            &vrbl1._Q3 = 1;
            &vrbl1._Q  = 3;
        end;
        if ( &vrbl1 >= &vrbl2._P3 ) then do;
            &vrbl1._Q4 = 1;
            &vrbl1._Q  = 4;
        end;
    end;
run;

%mend ComputeQuartiles2;

******************************************************************************;
******************************************************************************;
*** Macro to compute quintiles.;
***
*** INPUTDATA : The input SAS data set.
*** VRBL      : Name of a CONTINUOUS variable, from which quintiles will be
***             computed.
***
*** Eleven new variables will be added to the input data set. They will have
*** the same name as the original variable, except that they will have the
*** following extensions to the original name:
***
***     _N  : The number of observations used to compute cutoffs.
***     _P1 : The first threshold cutoff (first quintile).
***     _P2 : The second threshold cutoff (second quintile).
***     _P3 : The third threshold cutoff (third quintile).
***     _P4 : The third threshold cutoff (fourth quintile).
***     _K  : NUMERIC categorical variable indicating the quintile.
***     _K1 : Indicator variable showing whether an observation is in the 1st quint.
***     _K2 : Indicator variable showing whether an observation is in the 2nd quint.
***     _K3 : Indicator variable showing whether an observation is in the 3rd quint.
***     _K4 : Indicator variable showing whether an observation is in the 4th quint.
***     _K5 : Indicator variable showing whether an observation is in the 4th quint.
***
*** Note that SAS variables can have names that are up to 32 characters long.
*** This means that in order to use this macro, the SAS variable pointed to
*** by VRBL must have a name with a length of 29 or less.
******************************************************************************;
******************************************************************************;

%macro ComputeQuintiles(inputData=,vrbl=);

***********************************************************;
*** Compute quintile cutoffs.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc univariate data=&inputData;
    var &vrbl;
    output out=quintiles_temp pctlpts=0 20 40 60 80 100 pctlpre=p; * For cutoffs.;
    ods output Univariate.&vrbl..Moments = Moments;          * For N.;
run;
ODS SELECT ALL; * Turn displayed output back on.;

***********************************************************;
*** Reformat the number of observations used to compute quintiles.;
***********************************************************;

data Moments;
    set Moments;
    if ( Label1 = "N" );
    rename nValue2 = &vrbl._N;
    keep nValue2;
run;

***********************************************************;
*** Copy quintile cutoffs into input data set.;
***********************************************************;

data &inputData;
    set &inputData;
    if ( _N_ = 1 ) then set Moments;
run;

***********************************************************;
*** Define new quintile variables in input data set.;
***********************************************************;

data &inputData;
    set &inputData;
    if ( _N_ = 1 ) then set quintiles_temp;

    &vrbl._K  = .;
    &vrbl._K1 = .;
    &vrbl._K2 = .;
    &vrbl._K3 = .;
    &vrbl._K4 = .;
    &vrbl._K5 = .;
    if ( &vrbl NE . ) then do;
        &vrbl._K1 = 0;
        &vrbl._K2 = 0;
        &vrbl._K3 = 0;
        &vrbl._K4 = 0;
        &vrbl._K5 = 0;
        if ( &vrbl <  p20 ) then do;
            &vrbl._K1 = 1;
            &vrbl._K  = 1;
        end;
        if ( ( &vrbl >= p20 ) AND ( &vrbl < p40 ) ) then do;
            &vrbl._K2 = 1;
            &vrbl._K  = 2;
        end;
        if ( ( &vrbl >= p40 ) AND ( &vrbl < p60 ) ) then do;
            &vrbl._K3 = 1;
            &vrbl._K  = 3;
        end;
        if ( ( &vrbl >= p60 ) AND ( &vrbl < p80 ) ) then do;
            &vrbl._K4 = 1;
            &vrbl._K  = 4;
        end;
        if ( &vrbl >= p80 ) then do;
            &vrbl._K5 = 1;
            &vrbl._K  = 5;
        end;
    end;
    rename p0   = &vrbl._Mn
           p20 = &vrbl._P1
           p40 = &vrbl._P2
           p60 = &vrbl._P3
           p80 = &vrbl._P4
           p100 = &vrbl._Mx;
run;

***********************************************************;
*** Remove temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets; delete Moments quintiles_temp; run; quit;
ODS SELECT ALL; * Turn displayed output back on.;

%mend ComputeQuintiles;

******************************************************************************;
******************************************************************************;
*** Macro to retain observations which have non-missing data for all variables
*** in a given list.
***
*** INPUTDATA  : The input SAS data set
*** OUTPUTDATA : The output SAS data set
*** VARLIST    : Space-delimited list of variables. Variables can be either
***              numeric or character. 
***
*** The input data set is filtered to create the output data set.
*** If an observation has a missing value for a variable that is listed in
*** VARLIST, that observation is excluded from the output data set.
*** If OUTPUTDATA is set to the same thing as INPUTDATA, then INPUTDATA will
*** be overwritten. Otherwise, INPUTDATA will be left unchanged, and a new
*** SAS data set named OUTPUTDATA will be created.;
******************************************************************************;
******************************************************************************;

%macro RetainNonmissingObservations(inputData=,outputData=,varList=);

***********************************************************;
*** Filter the data.;
***********************************************************;

data &outputData;
    set &inputData;

    *** If VARLIST is non-empty then filter.;
    %if ( %length(&varList) ) %then %do; 
        *** Loop over variables in VARLIST. Remove observations that contain
        *** missing values for any variable in VARLIST. We need to handle
        *** numeric and character variables differently.;
        %local i vrbl dsid vnum rc;
        if ( 0
            %let i    = 1;
            %let vrbl = %scan(&varList,&i);
            %do %until(not %length(&vrbl));
                %let dsid = %sysfunc(open(&inputData));
                %let vnum = %sysfunc(varnum(&dsid,&vrbl));
                %if ( %sysfunc(vartype(&dsid,&vnum)) = C ) %then %do;
                OR ( &vrbl = " " )
                %end;
                %else %do;
                OR ( &vrbl = . )
                %end;
                %let rc   = %sysfunc(close(&dsid));
                %let i    = %eval( &i + 1 );
                %let vrbl = %scan(&varList,&i);
            %end;
        ) then delete;
    %end;
run;

%mend RetainNonmissingObservations;

* Example call.;
* %RetainNonmissingObservations(inputData=bmi_analytic_dat,outputData=test,varList=BmiCat_m CotCat_m age_m age_diff BmiCat2_f BmiCat3_f BmiCat4_f);

******************************************************************************;
******************************************************************************;
*** Macro to compute geometric means and 95% CIs for ONE VARIABLE, ONE CELL.
*** Invoked by the macro CONTINUOUSROWVAR_GEOMETRICMEAN, defined below.
***
*** REQUIRED INPUT ARGUMENTS.
***
*** INPUTDATA  : Name of an input SAS data set.
*** VRBL       : Name of the variable on which to compute the geometric mean.
*** OUTVAR     : Desired name of the output text variable.
*** GROUP      : A word or phrase indicating the group, used to label the OUTVAR.
*** OUTPUTDATA : Desired name of the output SAS data set.
*** ROUNDVAL:    Value used to round floating point numbers, an argument to the ROUND function.
***
***
*** OPTIONAL INPUT ARGUMENTS.
***
*** VARFIELDLEN: Maximum allowed length of the VARIABLE column in the output.
******************************************************************************;
******************************************************************************;

%macro geomean_ci95(inputData=,
                    vrbl=,
                    outvar=,
                    group=,
                    outputData=,
                    varFieldLen = 128.,
                    freqMeanLen = 48.,
                    roundVal=0.01);

options nomprint nonotes;

*** Log-transform the values.;
data tmp_logamt;
    set &inputData;
    if ( &vrbl LE 0 ) then &vrbl = .;
    logvar = log ( &vrbl );
    keep logvar;
run;

*** Compute mean and 95% CI OF THE LOG-TRANSFORMED VALUES.
*** The data has been log-transformed at this point and
*** will need to be exponentiated later.;
ODS SELECT NONE; * Turn displayed output off.;
proc means data=tmp_logamt N NMISS mean clm;
    var logvar;
    ods output summary=logresults;
run;
ODS SELECT ALL; * Turn displayed output back on.;

*** Determine row name.;
data row_name_gm;
    set &inputData(keep=&vrbl);
    length Variable tmp $ &varFieldLen;
    tmp = strip(vlabel(&vrbl));
    Variable = catt(tmp," : GeoMean (95% C.I.) [N]");
    if ( _N_ = 1 );
    keep Variable;
run;

*** Exponentiate the means of THE LOG-TRANSFORMED VALUES to recover the original units.;
*** Format output character string.;
data &outputData;
    retain Variable &outvar;
    length Variable $ &varFieldLen;
    length &outvar  $ &freqMeanLen;
    set logresults;
    if ( _N_ = 1 ) then set row_name_gm;
    geo_mean           = exp(logvar_Mean);      * Recover original scaling.;
    geo_lowb           = exp(logvar_LCLM);      * Recover original scaling.;
    geo_upb            = exp(logvar_UCLM);      * Recover original scaling.;
    geo_mean           = round(geo_mean,&roundVal);
    geo_lowb           = round(geo_lowb,&roundVal);
    geo_upb            = round(geo_upb,&roundVal); 
    &outvar = cat(put(geo_mean,8.2)," (",strip(put(geo_lowb,8.2)),"-",strip(put(geo_upb,8.2)),") [",logvar_N,"]");
    label &outvar = "Geometric Mean (95% C.I.) - &group";
    keep Variable &outvar;
run;

*** Delete temporary data sets.;
proc datasets nolist;
    delete logresults tmp_logamt row_name_gm;
run;
quit;

options notes;

%mend geomean_ci95;

******************************************************************************;
******************************************************************************;
*** The next four SAS macros are designed to help generate a descriptive
*** Table ("Table 1").
***
*** CATEGORICALROWVAR: generates row for a categorical variable
*** CONTINUOUSROWVAR:  generates row for a continuous variable
*** ADDTEXT:           adds a (possibly blank) text row
*** CONTINUOUSROWVAR_GEOMETRICMEAN: similar to CONTINUOUSROWVAR, but computes
***                    GEOMETRIC rather than ARITHMETIC MEANS.
***
*** The following macro variables are assumed to be set BEFORE invoking
*** any of the three macros.
***     INPUT_DATA      : The input data set
***     OUTPUT_DATA     : The output data set (the table)
***     COLVAR          : The categorical column variable
***
*** Special note regarding levels in categorical variables.
*** Levels of categorical variables will appear in the output table in
*** ALPHABETICAL ORDER. Sometimes some NON-alphabetical order is desired.
*** For example, if the levels of SIZE are "Small", "Medium", and
*** "Large", then when they are in alphabetical order we would have:
*** "Large", "Medium", then "Small". However, we might prefer the order to be
*** "Small", "Medium", and "Large" instead, which is not alphabetical.
*** A work-around is to insert prefixes in the level labels to force the
*** desired ordering, e.g. "A: Small", "B: Medium", and "C: Large".
*** The same holds true for the categorical variable for the columns.;
******************************************************************************;
******************************************************************************;

******************************************************************************;
******************************************************************************;
*** Macro to generate one line of the table for a CATEGORICAL variable.
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
******************************************************************************;
******************************************************************************;

%macro CategoricalRowVar(rowVar=,varFieldLen = 128.,freqMeanLen = 48.,roundVal=0.1);

options nomprint nonotes;

%put Processing &rowVar...;

%local NUMROWLEVELS ROWVARLEVEL NUMCOLLEVELS COLVARLEVEL i j level;

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

    *** Row Name.;
    data row_name;
        set &INPUT_DATA(keep=&rowVar);
        length Variable tmp $ &varFieldLen;
        tmp = strip(vlabel(&rowVar));
        Variable = catt(tmp," : &ROWVARLEVEL : N (%)");
        if ( _N_ = 1 );
        keep Variable;
    run;

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
            freqpct_meansd = cat(TotNum," (",strip(put(percent,8.1)),"%)");
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

        *** Create cell for current group level / row variable level.;
        data cell_G&j;
            length freqpct_meansd_g&j $ &freqMeanLen;
            set groups_freqs;
            if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = "&COLVARLEVEL" ) );
            pct          = round(ColPercent,&roundVal);
            freqpct_meansd_g&j = cat(Frequency," (",strip(put(percent,8.1)),"%)");
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
*** Delete temporary data sets.;
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

%mend CategoricalRowVar;

******************************************************************************;
******************************************************************************;
*** Macro to generate one line of the table for a CATEGORICAL variable.
*** This variant shows percent AND 95% CI in parentheses.
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
******************************************************************************;
******************************************************************************;

%macro CategoricalRowVar95CI(rowVar=,varFieldLen = 128.,freqMeanLen = 48.,roundVal=0.001,ci95=NormalApproximation);

options nomprint nonotes;

%put Processing &rowVar...;

%local NUMROWLEVELS ROWVARLEVEL NUMCOLLEVELS COLVARLEVEL i j level dsid num rc;

***********************************************************;
*** Determine number of levels in ROWVAR.;
***********************************************************;

*** Uniquify group levels.;
proc sort data=&CATVAR_DATA out=RowVarLevels_dat NODUPKEY;
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
proc sort data=&CATVAR_DATA out=COLVARLevels_dat NODUPKEY;
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

*** Determine size of overall group, not including missing.
*** Include frequency missing - if the overall sample size is smaller than the
*** sum of the subgroups, it is an indication that some subjects had a missing
*** GROUP value.;
data total_nonmissing_dat;
    set groups_freqs;
    if ( ( &rowVar = " " ) AND ( &COLVAR = " " ) );
    total_nonmissing = Frequency;
    keep total_nonmissing;
run;
data _NULL_;
    set total_num_dat;
    CALL SYMPUT("OVERALL_LABEL",overall_label);
run;

*** Determine size of overall group, including missing.
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

    *** Row Name.;
    data row_name;
        set &INPUT_DATA(keep=&rowVar);
        length Variable tmp $ &varFieldLen;
        tmp = strip(vlabel(&rowVar));
        Variable = catt(tmp," : &ROWVARLEVEL : N (% (95% C.I.))");
        if ( _N_ = 1 );
        keep Variable;
    run;

    *** Check whether this row is empty. If so, hand-create the data.;
    data check_row;
        set groups_freqs;
        if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = " " ) );
    run;
    %let dsid = %sysfunc(open(check_row));
    %let num  = %sysfunc(attrn(&dsid,nobs));
    %let rc   = %sysfunc(close(&dsid));

    *** Extract total ACROSS the column variable.;
    data total_across_group;
        retain TotNum freqpct_meansd;
        length freqpct_meansd $ &freqMeanLen;

        %if ( &num = 0 ) %then %do;
        set total_nonmissing_dat;
        TotNum    = 0;
        Frequency = total_nonmissing;
        %end;
        %else %do;
        set groups_freqs;
        by Table;
        if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = " " ) ) then TotNum = Frequency;
        if ( last.Table ) then do;
        %end;

            *** Compute percent.;
            Denominator    = Frequency;
            Percent        = 100 * TotNum / Frequency;
            Percent        = round(Percent,0.1);

            *** NORMAL APPROXIMATION.
            *** Reference: http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval;
            pObserved      = TotNum / Frequency;
            varianceX      = Frequency * pObserved * ( 1 - pObserved );
            varianceP      = pObserved * ( 1 - pObserved ) / Frequency;
            stddevP        = sqrt(varianceP);
            p              = probit(0.975);
            lcl            = pObserved - ( p * stddevP );
            ucl            = pObserved + ( p * stddevP );
            lcl            = round(100*lcl,0.1);
            ucl            = round(100*ucl,0.1);

            *** WILSON SCORE INTERVAL.
            *** Reference: http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval;
            sqrt2ndTerm    = (p*p)/(4*Frequency*Frequency);
            denominator    = 1+((p*p)/Frequency);
            nmrtr2ndTerm   = (p*p)/(2*Frequency);
            centerOfWilson = (pObserved+nmrtr2ndTerm)/denominator;
            lcl_wilson     = (pObserved+nmrtr2ndTerm-(p*sqrt(varianceP+sqrt2ndTerm)))/denominator;
            ucl_wilson     = (pObserved+nmrtr2ndTerm+(p*sqrt(varianceP+sqrt2ndTerm)))/denominator;
            lcl_wilson     = round(100*lcl_wilson,0.1);
            ucl_wilson     = round(100*ucl_wilson,0.1);

            *** Build output string. Choose between the NORMAL APPROXIMATION and the WILSON SCORE INTERVAL.;
            %if ( &ci95 = NormalApproximation ) %then %do;
            freqpct_meansd = cat(TotNum," (",strip(put(Percent,12.1)),"% ","(",strip(put(lcl,12.1)),"%-",strip(put(ucl,12.1)),"%)",")");
            %end;
            %else %do;
            freqpct_meansd = cat(TotNum," (",strip(put(Percent,12.1)),"% ","(",strip(put(lcl_wilson,12.1)),"%-",strip(put(ucl_wilson,12.1)),"%)",")");
            %end;
            label freqpct_meansd = "&OVERALL_LABEL";
            keep freqpct_meansd;
        %if ( &num ^= 0 ) %then %do;
            output;
        end;
        %end;
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

        *** Obtain total count for column.;
        data column_total;
            set groups_freqs;
            if ( ( &rowVar = " " ) AND ( &COLVAR = "&COLVARLEVEL" ) );
            rename Frequency = N;
            label Frequency = "N";
            keep Frequency;
        run;

        *** Check whether this row is empty. If so, hand-create the data.;
        data check_row;
            set groups_freqs;
            if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = "&COLVARLEVEL" ) );
        run;
        %let dsid = %sysfunc(open(check_row));
        %let num  = %sysfunc(attrn(&dsid,nobs));
        %let rc   = %sysfunc(close(&dsid));

        *** Create cell for current group level / row variable level.;
        data cell_G&j;
            length freqpct_meansd_g&j $ &freqMeanLen;

            %if ( &num = 0 ) %then %do;
            set column_total; * Obtain column total.;
            pct       = 0;
            Frequency = 0;
            %end;
            %else %do;
            set groups_freqs;
            if ( _N_ = 1 ) then set column_total; * Obtain column total.;
            if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = "&COLVARLEVEL" ) );
            pct = round(ColPercent,0.1);
            %end;

            *** NORMAL APPROXIMATION.
            *** Reference: http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval;
            pObserved      = Frequency / N;
            varianceX      = N * pObserved * ( 1 - pObserved );
            varianceP      = pObserved * ( 1 - pObserved ) / N;
            stddevP        = sqrt(varianceP);
            p              = probit(0.975);
            lcl            = pObserved - ( p * stddevP );
            ucl            = pObserved + ( p * stddevP );
            lcl            = round(100*lcl,0.1);
            ucl            = round(100*ucl,0.1);

            *** WILSON SCORE INTERVAL.
            *** Reference: http://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval;
            sqrt2ndTerm    = (p*p)/(4*N*N);
            denominator    = 1+((p*p)/N);
            nmrtr2ndTerm   = (p*p)/(2*N);
            centerOfWilson = (pObserved+nmrtr2ndTerm)/denominator;
            lcl_wilson     = (pObserved+nmrtr2ndTerm-(p*sqrt(varianceP+sqrt2ndTerm)))/denominator;
            ucl_wilson     = (pObserved+nmrtr2ndTerm+(p*sqrt(varianceP+sqrt2ndTerm)))/denominator;
            lcl_wilson     = round(100*lcl_wilson,0.1);
            ucl_wilson     = round(100*ucl_wilson,0.1);

            *** Build output string. Choose between the NORMAL APPROXIMATION and the WILSON SCORE INTERVAL.;
            %if ( &ci95 = NormalApproximation ) %then %do;
            freqpct_meansd_g&j = cat(Frequency," (",strip(put(percent,12.1)),"% ","(",strip(put(lcl,12.1)),"%-",strip(put(ucl,12.1)),"%)",")");
            %end;
            %else %do;
            freqpct_meansd_g&j = cat(Frequency," (",strip(put(percent,12.1)),"% ","(",strip(put(lcl_wilson,12.1)),"%-",strip(put(ucl_wilson,12.1)),"%)",")");
            %end;
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
*** Delete temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist;
    delete groups_freqs group_sizes groups_pval table_row row_name total_across_group
        COLVARLevels_dat RowVarLevels_dat dataset_attributes
        %do j = 1 %to &NUMCOLLEVELS;
            cell_G&j label_&j
        %end;
    cell_pval total_num_dat check_row total_nonmissing_dat;
run;
quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes;

%mend CategoricalRowVar95CI;

******************************************************************************;
******************************************************************************;
***
*** BORROWING MACRO FROM THE SAS PROGRAM
*** Make Descriptive Tables for Reference and for Standard - v7.052814.sas
***
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
******************************************************************************;
******************************************************************************;

%macro CategoricalRowVar2(rowVar=,varFieldLen = 128.,freqMeanLen = 48.,roundVal=0.1);

options nomprint nonotes;

*** Extract formats into a SAS data set.
*** This is necessary for the macro CATEGORICALROWVAR2.;
proc format library=work.formats cntlout = work.catrowvar2_format_dat;
run;

*** Make table of variables.
*** This is necessary for the macro CATEGORICALROWVAR2.;
ods select none;
proc contents data=&INPUT_DATA varnum;
    ods output Position = catrowvar2_vartable_dat;
run;
ods select all;

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

*** Obtain FORMATTED list of levels in COLVAR.;
ods select none;
proc freq data=&INPUT_DATA;
    tables &COLVAR / NOCOL NOFREQ NOPERCENT NOROW;
    ods output OneWayFreqs = COLVARLevels_dat2;
run;
ods select all;
data COLVARLevels_dat2;
    set COLVARLevels_dat2(keep=F_&COLVAR);
    rename F_&COLVAR = &COLVAR;
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
    *** First obtain name of format of the current variable from catrowvar2_vartable_dat if possible.;

    data formatTmpDat;
        set catrowvar2_vartable_dat;
        if ( Variable = "&rowVar" );
        keep Format;
    run;

    *** Add the name of the format of the current variable to the table of format levels.;
    data formatTmpDat2;
        set catrowvar2_format_dat;
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
            freqpct_meansd = cat(TotNum," (",strip(put(Percent,8.1)),"%)");
            label freqpct_meansd = "&OVERALL_LABEL";
            keep freqpct_meansd;
            output;
        end;
    run;

    *** Loop over COLVAR levels.;
    *** Fill in columns within the current row.;
    %do j = 1 %to &NUMCOLLEVELS;

        *** Store current COLUMN level into a macro variable, unformatted and formatted.;
        data _NULL_;
            set COLVARLevels_dat;
            if ( _N_ = &j ) then CALL SYMPUTX("COLVARLEVEL",&COLVAR);
        run;
        data _NULL_;
            set COLVARLevels_dat2;
            if ( _N_ = &j ) then CALL SYMPUTX("COLVARLEVEL2",&COLVAR);
        run;

        *** Determine group label.;
        data label_&j;
            set group_sizes;
            if ( strip(F_&COLVAR) = "&COLVARLEVEL2" ) then group_label = cat("&COLVAR = &COLVARLEVEL2 (N = ",Frequency,")");
            else delete;
            keep group_label;
        run;
        data _NULL_;
            set label_&j;
            CALL SYMPUT("GROUP_LABEL",group_label);
        run;

        *** Create cell for current group level / row variable level.;
        data cell_G&j;
            length freqpct_meansd_g&j $ &freqMeanLen;
            set groups_freqs;
            if ( ( &rowVar = "&ROWVARLEVEL" ) AND ( &COLVAR = "&COLVARLEVEL" ) );
            pct          = round(ColPercent,&roundVal);
            freqpct_meansd_g&j = cat(Frequency," (",strip(put(percent,8.1)),"%)");
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
*** Delete temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist;
    delete groups_freqs group_sizes groups_pval table_row row_name total_across_group
        COLVARLevels_dat RowVarLevels_dat dataset_attributes
        %do j = 1 %to &NUMCOLLEVELS;
            cell_G&j label_&j
        %end;
    cell_pval total_num_dat
    COLVARLevels_dat2 catrowvar2_format_dat catrowvar2_vartable_dat;
run;
quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes;

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
******************************************************************************;
******************************************************************************;

%macro ContinuousRowVar(rowVar=,
                        varFieldLen = 128.,
                        freqMeanLen = 48.,
                        roundVal=0.01,
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
    freqpct_meansd = cat(put(m,8.2)," (",strip(put(s,8.2)),") [",&rowVar._N,"]");
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
%else %if ( &NUMCOLLEVELS = 2 ) %then %do;
%put ************************************;
%put ************************************;
%put USING PROC TTEST (with Satterthwaite) RATHER THAN PROC NPAR1WAY OR PROC ANOVA...;
%put ************************************;
%put ************************************;
    %let ODSOUTPUT = ttest_out;
    ODS SELECT NONE; * Turn displayed output off.;
    proc ttest data=nonmissing_COLVAR;
        class &COLVAR;
        var &rowVar;
        ods output TTests = &ODSOUTPUT;
    run;
    ODS SELECT ALL; * Turn displayed output back on.;

    data cell_pval;
        set &ODSOUTPUT;
        format Probt PVALUE6.4;
        if ( Method = "Satterthwaite" );
        label Probt = "P-value";
        rename Probt = Prob;
        keep Probt;
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
            freqpct_meansd_g&j = cat(put(m,8.2)," (",strip(put(s,8.2)),") [",&rowVar._N,"]");
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

%mend ContinuousRowVar;

******************************************************************************;
******************************************************************************;
*** Macro to generate one line of the table for a CONTINUOUS variable.
*** This variant shows 95% CI rather than SD in parentheses.
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
*** P-values are computed using the MEDIAN TEST as implemented in PROC NPAR1WAY.
*** This was thought preferable to using PROC ANOVA, which assumes cells of
*** equal size.
******************************************************************************;
******************************************************************************;

%macro ContinuousRowVar95CI(rowVar=,
                        varFieldLen = 128.,
                        freqMeanLen = 48.,
                        roundVal=0.001);

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
    Variable = catt(tmp," : Mean (95% C.I.) [N]");
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
ods select all;

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
    p              = probit(0.975);
    lcl            = round(&rowVar._Mean - ( p * &rowVar._StdDev ),&roundVal);
    ucl            = round(&rowVar._Mean + ( p * &rowVar._StdDev ),&roundVal);
    freqpct_meansd = cat(m," (",lcl,"-",ucl,") [",&rowVar._N,"]");
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

***********************************************************;
*** Compute p-value.
*** If NUMCOLLEVELS = 2, use WILCOXON.
*** Otherwise, use KRUSKALWALLIS.
***********************************************************;

%if ( &NUMCOLLEVELS = 2 ) %then %do;
    %let ODSOUTPUT = WilcoxonTest;
    %let PVARNAME  = PT2_WIL;
%end;
%else %do;
    %let ODSOUTPUT = KruskalWallisTest;
    %let PVARNAME  = P_KW;
%end;

*proc anova data=nonmissing_COLVAR;
*    class &COLVAR;
*    model &rowVar = &COLVAR;
*    ods output ANOVA.ANOVA.&rowVar..ModelANOVA = anova_out;
*run;
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
        set groups_freqs;
        if ( strip(F_&COLVAR) = "&COLVARLEVEL" ) then group_label = cat("&COLVAR = &COLVARLEVEL (N = ",Frequency,")");
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
            set meansd_level_&j;
            p                  = probit(0.975);
            lcl                = round(&rowVar._Mean - ( p * &rowVar._StdDev ),&roundVal);
            ucl                = round(&rowVar._Mean + ( p * &rowVar._StdDev ),&roundVal);
            freqpct_meansd_g&j = cat(strip(put(&rowVar._Mean,12.3))," (",strip(put(lcl,12.3)),"-",strip(put(ucl,12.3)),") [",&rowVar._N,"]");
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
    cell_pval meansd_total groups_freqs overall_attributes;
run;
quit;
ODS SELECT ALL; * Turn displayed output back on.;

%mend ContinuousRowVar95CI;

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
*** FREQMEANLEN: Maximum allowed length of the MEDIAN(IQR) columns in the output.
*** ROUNDVAL:    Value used to round floating point numbers, an argument to the ROUND function.
*** PVALMETHOD:  If set to NONPARAMETRIC (the default), PROC NPAR1WAY will be used to compute
***              the p-value. Otherwise, PROC ANOVA will be used.
***
*** P-values are computed using the WILCOXON TEST as implemented in PROC NPAR1WAY
*** if the number of levels in the column variable is exactly 2, but using the
*** KRUSKAL-WALLIS test otherwise.  This was thought preferable to using PROC ANOVA,
*** which assumes cells of equal size.
******************************************************************************;
******************************************************************************;

%macro ContinuousRowVarMedianIQR(rowVar=,
                        varFieldLen = 128.,
                        freqMeanLen = 48.,
                        roundVal=0.01,
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
    Variable = catt(tmp," : Median (IQR) [N]");
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

*** Determine data type of COLVAR.;
%let dsid = %sysfunc(open(&INPUT_DATA));
%let vnum = %sysfunc(varnum(&dsid,&COLVAR));
%let COLVARTYPE = %sysfunc(vartype(&dsid,&vnum));
%let rc = %sysfunc(close(&dsid));

data nonmissing_COLVAR;
    set &INPUT_DATA;
    %if ( &COLVARTYPE = C ) %then %do;
    if ( &COLVAR ^= " " ); * COLVAR is of CHARACTER type.;
    %end;
    %else %do;
    if ( &COLVAR ^= . ); * COLVAR is of NUMERIC type.;
    %end;
run;

***********************************************************;
*** Compute MEDIANS of TOTAL GROUP.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc univariate data=nonmissing_COLVAR;
    var &rowVar;
    ods output Quantiles = medianiqr_total
               Moments   = moments_total;
run;
ODS SELECT ALL; * Turn displayed output back on.;

*** Select out the total non-missing N.;
data moments_total;
    set moments_total(keep=Label1 nValue1);
    if ( Label1 = "N" );
    rename nValue1 = N;
    keep nValue1;
run;

data cell_TOT;
    retain p25 p50 p75;
    length freqpct_meansd $ &freqMeanLen;
    set medianiqr_total end=lastobs;
    if ( _N_ = 1 ) then set moments_total;
    if ( Quantile = "25% Q1" )     then p25 = Estimate;
    if ( Quantile = "50% Median" ) then p50 = Estimate;
    if ( Quantile = "75% Q3" )     then p75 = Estimate;
    if ( lastobs ) then do;
        p25round       = round(p25,&roundVal);
        p50round       = round(p50,&roundVal);
        p75round       = round(p75,&roundVal);
        freqpct_meansd = cat(put(p50round,8.2)," (",strip(put(p25round,8.2)),",",strip(put(p75round,8.2)),") [",N,"]");
        output;
    end;
    label freqpct_meansd = "&OVERALL_LABEL";
    keep freqpct_meansd;
run;

***********************************************************;
*** Compute MEDIANS by levels of the column variable.;
***********************************************************;

proc sort data=nonmissing_COLVAR; by &COLVAR; run;
ODS SELECT NONE; * Turn displayed output off.;
proc sort data=nonmissing_COLVAR; by &COLVAR; run;
ODS select none;
proc univariate data=nonmissing_COLVAR;
    by &COLVAR;
    var &rowVar;
    ods output Moments   = moments_groups
               Quantiles = medianiqr_groups;
run;
ODS select all;

*** Select out the for each group non-missing N.;
data moments_groups;
    set moments_groups(keep=&COLVAR Label1 nValue1);
    if ( Label1 = "N" );
    rename nValue1 = N;
    keep &COLVAR nValue1;
run;

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
%else %if ( &NUMCOLLEVELS = 2 ) %then %do;
%put ************************************;
%put ************************************;
%put USING PROC TTEST (with Satterthwaite) RATHER THAN PROC NPAR1WAY OR PROC ANOVA...;
%put ************************************;
%put ************************************;
    %let ODSOUTPUT = ttest_out;
    ODS SELECT NONE; * Turn displayed output off.;
    proc ttest data=nonmissing_COLVAR;
        class &COLVAR;
        var &rowVar;
        ods output TTests = &ODSOUTPUT;
    run;
    ODS SELECT ALL; * Turn displayed output back on.;

    data cell_pval;
        set &ODSOUTPUT;
        format Probt PVALUE6.4;
        if ( Method = "Satterthwaite" );
        label Probt = "P-value";
        rename Probt = Prob;
        keep Probt;
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

    *** Store current COLUMN level into a temporary SAS data set.
    *** Store current COLUMN level into a macro variable.;
    %local level;
    data current_level;
        set COLVARLevels_dat;
        if ( _N_ = &j ) then CALL SYMPUTX("COLVARLEVEL",&COLVAR);
        else delete;
        rename &COLVAR = currentLevel;
    run;

    *** Determine group label.;
    data label_&j;
        set group_sizes;
        if ( _N_ = 1 ) then set current_level;
        if ( &COLVAR = currentLevel ) then do;
            group_label = cat("&COLVAR = ",strip(currentLevel)," (N = ",Frequency,")");
            CALL SYMPUT("GROUP_LABEL",group_label);
        end;
        else delete;
        keep group_label;
    run;

    *** Determine group sample size.;
    data group_sample_size;
        set moments_groups;
        if ( _N_ = 1 ) then set current_level;
        if ( &COLVAR = currentLevel );
        keep N;
    run;

    *** If the LABEL_J data set is NOT empty, then generate the output cell.
    *** If the LABEL_J data set is empty, then create a dummy entry.;
    %let dsid = %sysfunc(open(label_&j));
    %let num  = %sysfunc(attrn(&dsid,nobs));
    %let rc   = %sysfunc(close(&dsid));
    %if ( &num > 0 ) %then %do;
        data cell_G&j;
            retain p25 p50 p75;
            length freqpct_meansd_g&j $ &freqMeanLen;
            set medianiqr_groups end=lastobs;
            if ( _N_ = 1 ) then set group_sample_size;
            %if ( &COLVARTYPE = C ) %then %do;
            if ( ( &COLVAR = "&COLVARLEVEL" ) AND ( Quantile = "25% Q1" ) )     then p25 = Estimate;
            if ( ( &COLVAR = "&COLVARLEVEL" ) AND ( Quantile = "50% Median" ) ) then p50 = Estimate;
            if ( ( &COLVAR = "&COLVARLEVEL" ) AND ( Quantile = "75% Q3" ) )     then p75 = Estimate;
            %end;
            %else %do;
            if ( ( &COLVAR = &COLVARLEVEL ) AND ( Quantile = "25% Q1" ) )     then p25 = Estimate;
            if ( ( &COLVAR = &COLVARLEVEL ) AND ( Quantile = "50% Median" ) ) then p50 = Estimate;
            if ( ( &COLVAR = &COLVARLEVEL ) AND ( Quantile = "75% Q3" ) )     then p75 = Estimate;
            %end;
            if ( lastobs ) then do;
                p25round           = round(p25,&roundVal);
                p50round           = round(p50,&roundVal);
                p75round           = round(p75,&roundVal);
                freqpct_meansd_g&j = cat(put(p50round,8.2)," (",strip(put(p25round,8.2)),",",strip(put(p75round,8.2)),") [",N,"]");
                output;
            end;
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

    *** Delete temporary data set.;
    proc datasets nolist;
        delete current_level label_&j group_sample_size;
    run;
    quit;
%end;

***********************************************************;
*** Make table row.;
*** Fill in columns within the current row.;
*** Reference: http://www.stattutorials.com/SAS/TUTORIAL-PROC-MEANS-OUTPUT.htm;
***********************************************************;

data table_row_MedianIQR;
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
    set &OUTPUT_DATA table_row_MedianIQR;
run;

***********************************************************;
*** Delete temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist;
    delete nonmissing_COLVAR &ODSOUTPUT
        COLVARLevels_dat dataset_attributes
        %do j = 1 %to &NUMCOLLEVELS;
            cell_G&j
        %end;
        table_row_MedianIQR row_name cell_TOT
    cell_pval  group_sizes overall_attributes;
run;
quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes;

%mend ContinuousRowVarMedianIQR;

******************************************************************************;
******************************************************************************;
*** Macro to generate one line of the table for a CONTINUOUS variable, but
*** computing GEOMETRIC MEANS rather than ARITHMETIC MEANS.
***
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
*** P-values are computed using the MEDIAN TEST as implemented in PROC NPAR1WAY.
*** This was thought preferable to using PROC ANOVA, which assumes cells of equal size.
***
*** Note that at this time (3/7/2013) this macro does not generate p-values
*** if there are more than two levels in the categorical column variable.
******************************************************************************;
******************************************************************************;

%macro ContinuousRowVar_GeometricMean(rowVar=,
                                      varFieldLen = 128.,
                                      freqMeanLen = 48.,
                                      roundVal=0.01);

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
    Variable = catt(tmp," : GeoMean (95% C.I.) [N]");
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
ods select all;

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
*** Compute GEOMETRIC MEAN of TOTAL GROUP.;
***********************************************************;

%geomean_ci95(inputData=nonmissing_COLVAR,vrbl=&rowVar,outvar=freqpct_meansd,
              group=Total (Overall),outputData=cell_TOT,
              varFieldLen=&varFieldLen,freqMeanLen=&freqMeanLen);

*** Relabel.;
data cell_TOT;
    set cell_TOT;
    label freqpct_meansd = "&OVERALL_LABEL";
run;

***********************************************************;
*** Loop over levels of the column variable, compute geometric means.;
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
        if ( strip(F_&COLVAR) = "&COLVARLEVEL" ) then group_label = cat("&COLVAR = &COLVARLEVEL (N = ",Frequency,")");
        else delete;
        keep group_label;
    run;
    data _NULL_;
        set label_&j;
        CALL SYMPUT("GROUP_LABEL",group_label);
    run;

    *** Subset the data.;
    data subset_for_geomean;
        set nonmissing_COLVAR;
        if ( &COLVAR = "&COLVARLEVEL" );
    run;

    *** Compute geometric means for the subsetted data.;
    %geomean_ci95(inputData=subset_for_geomean,vrbl=&rowVar,outvar=freqpct_meansd_g&j,
                  group=&COLVARLEVEL,outputData=cell_G&j,
                  varFieldLen=&varFieldLen,freqMeanLen=&freqMeanLen);

    *** Relabel.;
    data cell_G&j;
        set cell_G&j;
        label freqpct_meansd_g&j = "&GROUP_LABEL";
    run;
%end;

***********************************************************;
*** Compute p-value if possible.
*** Note that these p-values are being computed on values
*** that have NOT been log-transformed. Log transformation
*** was done within the sub-macro GEOMEAN_CI95.
*** If NUMCOLLEVELS = 2, use WILCOXON.
*** Otherwise, use KRUSKALWALLIS.
***********************************************************;

%if ( &NUMCOLLEVELS = 2 ) %then %do;
    %let ODSOUTPUT = WilcoxonTest;
    %let PVARNAME  = PT2_WIL;
%end;
%else %do;
    %let ODSOUTPUT = KruskalWallisTest;
    %let PVARNAME  = P_KW;
%end;

*** Compute p-values.;
proc sort data=nonmissing_COLVAR; by &COLVAR; run;
ODS SELECT NONE; * Turn displayed output off.;
PROC NPAR1WAY data=nonmissing_COLVAR wilcoxon;
    class &COLVAR;
    var &rowVar;
    ods output &ODSOUTPUT = cell_pval;
run;
ODS SELECT ALL; * Turn displayed output back on.;

*** If PROC NPAR1WAY does not generate a p-value for whatever reason,
*** give a missing value for the p-value.;
%if NOT %sysfunc(exist(cell_pval)) %then %do;
data cell_pval;
    Prob       = .;
    label Prob = "P-value";
run;
%end;
%else %do;
*** Keep only the Wilcoxon-Kruskal-Wallis statistic p-value.;
data cell_pval;
    set cell_pval;
    if ( Name1 = "&PVARNAME" );
    label nValue1 = "P-value";
    rename nValue1 = Prob;
    keep nValue1;
run;
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
            cell_G&j label_&j
        %end;
        table_row_MeanSD row_name cell_TOT
        cell_pval meansd_total subset_for_geomean groups_freqs group_sizes overall_attributes;
run;
quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes;

%mend ContinuousRowVar_GeometricMean;

******************************************************************************;
******************************************************************************;
*** Macro to add a dummy row.;
***
*** REQUIRED INPUT ARGUMENTS.
***
*** TEXT : Text to appear in the first column.
***        For a blank row, leave this unspecified.
***        Text must NOT contain commas.
***
***
*** OPTIONAL INPUT ARGUMENTS.
***
*** VARFIELDLEN: Maximum allowed length of the VARIABLE column in the output.
***
******************************************************************************;
******************************************************************************;

%macro AddText(text=,varFieldLen = 128.);

options nomprint nonotes;

***********************************************************;
*** If the output data set does not yet exist, intitialize it.;
***********************************************************;

%if NOT %sysfunc(exist(&OUTPUT_DATA)) %then %do;
data &OUTPUT_DATA;
    set _NULL_;
run;
%end;

***********************************************************;
*** Set row to contain desired text.;
***********************************************************;

data row_name;
    length Variable $ &varFieldLen;
    Variable = "&text";
run;

***********************************************************;
*** Append dummy row to cumulative SAS data set.;
***********************************************************;

data &OUTPUT_DATA;
    set &OUTPUT_DATA row_name;
run;

***********************************************************;
*** Delete temporary data sets.;
***********************************************************;

ODS SELECT NONE; * Turn displayed output off.;
proc datasets nolist; delete row_name; run; quit;
ODS SELECT ALL; * Turn displayed output back on.;

options notes;

%mend AddText;

******************************************************************************;
******************************************************************************;
*** Macro to save a data set to a CSV file. This is intended for use with
*** the four macros for creating a descriptive table (CATEGORICALROWVAR,
*** CONTINUOUSROWVAR, ADDTEXT, and CONTINUOUSROWVAR_GEOMETRICMEAN), so it
*** assumes that the macro variable INPUTDATA has already been defined.
***
*** RTFFILE : Path name for the output RTF file..
***
*** OPTIONAL INPUT ARGUMENTS.
***
******************************************************************************;
******************************************************************************;

%macro WriteDescriptiveTableToCsvFile(outFile=);

proc export data=&OUTPUT_DATA replace label
    outfile="&outFile"
    dbms=csv;
run;

%mend WriteDescriptiveTableToCsvFile;

******************************************************************************;
******************************************************************************;
*** Macro to save a data set to an RTF file. This is intended for use with
*** the four macros for creating a descriptive table (CATEGORICALROWVAR,
*** CONTINUOUSROWVAR, ADDTEXT, and CONTINUOUSROWVAR_GEOMETRICMEAN), so it
*** assumes that the macro variable INPUTDATA has already been defined.
***
*** RTFFILE : Path name for the output RTF file..
***
*** OPTIONAL INPUT ARGUMENTS.
***
******************************************************************************;
******************************************************************************;

%macro WriteDescriptiveTableToRtfFile(outFile=);

ods graphics on;
ods rtf file="&outFile";
ods rtf select Print.Print;

proc print data=&OUTPUT_DATA label noobs;
run;

ods rtf close;
ods graphics off;

%mend WriteDescriptiveTableToRtfFile;

/****************************************************************************************/
/* PROGRAM: better_means */
/* AUTHORS: Myra A. Oltsik and Peter Crawford */
/* ORIGINAL DATE: 12/20/05 */
/* PURPOSE: Create a dataset with PROC MEANS statistics, with each record being */
/* one variable. Print stats if needed, too. Fixes ODS problems. */
/* */
/* NOTE: This macro has special handling for N, SUMWGT, KURT and SKEW. */
/* Also: STDEV, Q1, MEDIAN, Q3 are referred as STD, P25, P50, P75. */
/****************************************************************************************/
/****************************************************************************************/
/* MACRO PARAMETERS: */
/* required: none */
/* optional: print -- whether or not to print results to output */
/* data -- dataset name to be analysed */
/* sort -- sort order choice of the file of MEANS, by VARNUM or NAME */
/* stts -- indicate which statistics should included in the output */
/* varlst -- list of variables for means if not all numeric vars in file */
/* clss -- variable(s) for a class statement */
/* wghts -- variable for a weight statement */
/* defaults: */
/* data -- &syslast (most recently created data set) */
/* print -- Y */
/* sort -- VARNUM */
/* stts -- _ALL_ */
/* varlst -- _ALL_ */
/* */
/* Created Macro Variables: */
/* locals -- see inline comments at %local statement */
/* Creates Data Sets */
/* results are written to &data._means */
/* many data sets are created in the work library all prefixed _better_ */
/* but unless the testing option is set, the work data stes are deleted */
/* */
/* SAMPLES: */
/* %better_means(data=test); print all default statistics in a dataset */
/* %better_means(data=sashelp.class,stts=MEAN SUM); print only MEAN and SUM stats */
/* %better_means(data=sashelp.gnp,print=N,sort=NAME,stts=MIN MAX,varlst=INVEST */
/* EXPORTS); suppress list printing, limit output statistics and variables, and */
/* sort on NAME */
/* %better_means(data=sasuser.shoes,clss=PRODUCT); run all stats by PRODUCT field */
/* %better_means(data=sasuser.weighted,wghts=WGT); run all stats weighted on WGT */
/****************************************************************************************/
%macro
better_means(
data = &syslast ,
print = Y,
sort = VARNUM,
stts = _ALL_,
varlst = _ALL_,
clss = ,
wghts = ,
testing= no, /* any other value will preserve the _better_: data sets */
/****************************************************************************************/
/* PROVIDE THE COMPLETE PROC MEANS STATISTIC LIST (FROM ONLINE-DOC) IF NONE STATED. */
/****************************************************************************************/
_stts = N MEAN STD MIN MAX CSS CV LCLM NMISS
P1 P5 P10 P25 P50 P75 P90 P95 P99 QRANGE RANGE
PROBT STDERR SUM SUMWGT KURT SKEW T UCLM USS VAR
);
%local
vLexist /* EXISTENCE OF LABELS ON INPUT DATASET */
s /* POINTER TO STATISTIC IN THE STATISTICS LIST */
stato /* HOLDER OF AN INDIVIDUAL STATISTIC NAME :-
USED IN STATISTIC TABLE NAME, AND
USED IN THE IN= VARIABLE DATASET OPTION */
full /* INDICATOR IN OUTPUT LABEL WHEN ALL STATS USED.*/
;
/****************************************************************************************/
/* PUT STATS AND VAR PARAMETER LIST INTO UPPER CASE. */
/****************************************************************************************/
%let varlst = %upcase(&varlst);
%let stts = %upcase(&stts);
%let data = &data ; /* RESOLVE &syslast, WHEN DEFAULTED */
/****************************************************************************************/
/* GET THE NAMES/NUMBERS OF ALL VARIABLES INTO A LOOKUP FORMAT IF SORT ORDER = VARNUM. */
/****************************************************************************************/
%if &sort eq VARNUM %then %do;
proc contents data= &data out= _better_cols noprint;
run;
data _better_cntl;
retain
FMTNAME '_bm_VN'
TYPE 'I'
HLO 'U'
;
set _better_cols( keep= NAME VARNUM rename=( VARNUM=LABEL ));
START = upcase( NAME) ;
run;
proc format cntlin= _better_cntl;
run;
%end;

/****************************************************************************************/
/* PROCESS STATISTICS CONDITIONS / COMBINATIONS */
/****************************************************************************************/
%if &stts = _ALL_ or %length(&stts) = 0 %then %do;
%let stts = &_stts ;
%let full = FULL STATS;
%end;
%if %length(&wghts) %then %do;
%* remove KURT and Skew when weights are present;
%let stts = %sysfunc( tranwrd( &stts, KURT, %str( ) ));
%let stts = %sysfunc( tranwrd( &stts, SKEW, %str( ) ));
%let full = STATS ;
%end;
%else %do;
%* remove SUMWGT when no weights present ;
%let stts = %sysfunc( tranwrd( &stts, SUMWGT, %str( ) ));
%let full = STATS ;
%end;
/****************************************************************************************/
/* RUN PROC MEANS ON VARIABLES WITH OUTPUT FILE FOR EACH STATISTIC REQUESTED. MERGE */
/* DATASET OF LIST OF NUMERIC VARIABLES AND THEIR VARNUM. */
/****************************************************************************************/
proc means data= &data noprint missing;
%if &varlst ne _ALL_ & %length(&varlst) %then %do;
var &varlst;
%end;
%if %length(&clss) %then %do;
class &clss;
%end;
%if %length(&wghts) %then %do;
weight &wghts;
%end;
%let s = 1 ;
%let stato = %scan( &stts, 1 );
%do %while( %length(&stato) > 0 ); /* USING %LENGTH() FOR &STATO WORDS SIGNIFICANT TO %IF/%WHILE */
output out= _better_&stato &stato= ;
%let s = %eval( &s +1 );
%let stato = %scan( &stts, &s );
%end;
run;
data _better_means1;
length
_BETTER_ $32. /* STATS IDENTITY */
;
set /* ALL THOSE OUTPUT DATASETS FROM PROC MEANS */
%let stato = %scan( &stts, 1 );
%let s = 1 ;
%do %while( %length(&stato) gt 0 );
_better_&stato( in= _in_&stato ) /* NEED IN= VARIABLE TO IDENTIFY INPUT DATA */
%let s = %eval( &s +1 );
%let stato = %scan( &stts, &s );
%end;
;
by _TYPE_ &clss;
%let stato = %scan( &stts, 1 ); /* GENERATE _BETTER_ TO IDENTIFY EACH ROW OF RESULTS */
%let s = 1 ;
%do %while( %length(&stato) > 0 );
if _in_&stato then _BETTER_ = "%upcase( &stato )" ; else
%let s = %eval( &s +1 );
%let stato = %scan( &stts, &s );
%end;
;
run;
proc transpose data=_better_means1 out=_better_means2;
by _TYPE_ &clss ;
id _BETTER_ ;
run;
/****************************************************************************************/
/* FROM SAS FAQ # 1806: MACRO TO CHECK IF THE VARIABLE EXISTS IN A DATASET. */
/****************************************************************************************/
%macro varcheck(varname,dsname);
%local dsid vindex rc;
%let dsid = %sysfunc(open(&dsname,is));
%if &dsid EQ 0 %then %do;
%put ERROR: (varcheck) The data set "&dsname" could not be found;
%end;
%else %do;
%let vindex = %sysfunc(varnum(&dsid,&varname));
%end;
%let rc = %sysfunc(close(&dsid));
&vindex
%mend varcheck;
%let vLexist = %varcheck(_LABEL_,_better_means2);
/****************************************************************************************/
/* CREATE BASIS FOR OUTPUT DATASET BASED ON DIFFERENT CONDITIONS AND PARAMETER CHOICES. */
/****************************************************************************************/
%macro inL( list, seek )/ des= "Return TRUE, if &seek in &list, blank delimited";
%sysfunc( indexw( &list, &seek ))
%mend inL ;

%macro now( fmt= datetime21.2 ) / des= "Timestamp";
%sysfunc( datetime(), &fmt )
%mend now;

data _better_means_out;
length
_TYPE_ 3. ;
retain /* TO FIX ORDER OF THE FIRST FEW */
&clss
%if &sort eq VARNUM %then %do;
VARNUM
%end;
NAME
%if &vLexist ne 0 %then %do; /* ADD IF TRANSPOSED DATASET CONTAINS THE LABEL VARIABLE */
LABEL
%end;
%if %inL(&stts,N) %then %do; /* ADD % NOT MISSING IF STATISTIC "N" REQUESTED */
N
PCT_POP
PCT_DEN
%end;
;
set _better_means2(rename=(
_NAME_ = NAME
%if &vLexist ne 0 %then %do;
_LABEL_ = LABEL
%end;
));
%if %inL(&stts,N) %then %do;
format
PCT_POP percent.4
;
if NAME = "_FREQ_" then do;
PCT_DEN = N ;
delete;
end;
else do;
if PCT_DEN then PCT_POP = N / pct_den ;
end;
drop
PCT_DEN
;
%end;
%else %do;
if NAME = "_FREQ_" then delete;
%end;
%if &sort eq VARNUM %then %do;
VARNUM = input(NAME,_bm_VN.);
%end;
NAMEU = upcase(NAME) ;
run;
/****************************************************************************************/
/* CREATE FINAL DATASET WITH ALL STATISTICS, SORTED AS REQUESTED ON INVOCATION. */
/****************************************************************************************/
proc sort data= _better_means_out
out= &data._means(label= "&FULL FOR &data %NOW" drop= NAMEU
%if %length(&clss) = 0 %then %do;
_TYPE_
%end;
);
by _TYPE_ &clss &sort;
run;
/****************************************************************************************/
/* IF PRINTED OUTPUT IS REQUESTED, DO SO HERE. */
/****************************************************************************************/
%if &print = Y %then %do;
proc print data=&data._means;
title3 "MEANS FOR &data";
%if %length(&clss) > 0 %then %do;
by _TYPE_;
%end;
run;
%end;
%if &testing = no %then %do;
/****************************************************************************************/
/* CLEAN UP REMAINING TEMPORARY DATASETS. */
/****************************************************************************************/
proc datasets lib= work nolist;
delete _better_: ;
run; quit;
%end;
%mend better_means;


/****************************************************************************************/
/* PROGRAM: trylock 
/* AUTHORS: John Leveille, d-Wise Technologies, Inc., Raleigh, NC
/*          Jimmy Hunnings, d-Wise Technologies, Inc., Raleigh, NC  
/* ORIGINAL DATE: 2005 
/* PURPOSE: Resolves file locked error when the same file is used in multiple steps by using 
/* the LOCK statement in base SAS® to cause your program to wait until the tables you need 
/* are available.
/* 
/****************************************************************************************/
/****************************************************************************************/
/* MACRO PARAMETERS: */
/* member = dataset to lock
/* timeout = number of seconds to keep trying to get the lock
/* retry = This is the amount of time that the macro should wait in between each attempt 
/* to lock the table
/****************************************************************************************/

%macro trylock(member=,timeout=10,retry=1);
 %local starttime;
 %let starttime = %sysfunc(datetime());
 %do %until(&syslckrc <= 0
 or %sysevalf(%sysfunc(datetime()) > (&starttime + &timeout)));
 %put trying open ...;
 data _null_;
 dsid = 0;
 do until (dsid > 0 or datetime() > (&starttime + &timeout));
 dsid = open("&member");
 if (dsid = 0) then rc = sleep(&retry);
 end;
 if (dsid > 0) then rc = close(dsid);
 run;
 %put trying lock ...;
 lock &member;
 %put syslckrc=&syslckrc;
 %end;
 %mend trylock; 


***********************************************************;
*** END OF FILE.;
***********************************************************;

/****************************************************************************
*****************************************************************************
*** Author: Yuanchao Zheng, M.S., Stanford University
*** Purpose: Create descriptive tables
*** OS: Windows 10 Pro 64-bit
*** Software: SAS 9.4
*** Note:
*** There are two SAS macros: Getvars and Table_summary. Getvars computes
*** summary statistics for both categorical and continuous variables.
*** Table_summary calls Getvars and outputs a summary table.
***
*** 10/18/17 - the missing option was removed to keep missing out of the calculations
***
*** Copyright (C) <2017> <Yuanchao Zheng>
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program. If not, see <http://www.gnu.org/licenses/>.
*****************************************************************************
****************************************************************************/
/****************************************************************************
*** Marco 1: Getvars;
****************************************************************************/
%macro Getvars(dat=&yourdata.,datout=&output_data.);

proc datasets; delete &datout.;run; quit;

*Get summary statistics for categorical variables;
***********************************************************;
%if &varlist_cat.^= %then %do;

*count number of categorical variables;
%let nvarlist_cat=%sysfunc(countw(&varlist_cat.));

*for each categorical variable, compute counts and percents;
%do i=1 %to &nvarlist_cat.;
%let var&i.=%scan(&varlist_cat.,&i.);

proc freq data=&dat. noprint; tables &&var&i../ out=dat_cat_&i.; run; * deleated "missing" before out statement want to remove missing from proc freq calculations;

data dat_cat_&i._2(drop=&&var&i.. COUNT PERCENT);
set dat_cat_&i.;

length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

output_suborder=5;
xx_variable="%trim(%upcase(&&var&i..))";
xx_var_label=upcase(VLABEL(&&var&i..));
xx_description=strip(vvalue(&&var&i..));
xx_type="count and percent";
xx_var=strip(put(COUNT,16.))||" ("||strip(put(PERCENT, 16.&decimal_max.))||"%"||")";
xx_variable_original="%trim(%upcase(&&var&i..))";

label xx_variable="Variable" xx_var_label="Variable label"
xx_description="Description" xx_type="Type of statistic"
xx_var="Statistic";
format percent 8.1;
run;

proc append base=&datout. data=dat_cat_&i._2; run;

%end;

proc datasets library=work; delete dat_cat:; run;

%end;

*Get summary statistics for continuous variables;
***********************************************************;
*part 1: mean and std values;

%if &varlist_cont.^= %then %do;

%let nvarlist_cont=%sysfunc(countw(&varlist_cont.));

%do j=1 %to &nvarlist_cont.;
%let var&j.=%scan(&varlist_cont.,&j.);

proc means data=&dat. noprint; var &&var&j..;
output out=dat_cont1_&j.(drop=_TYPE_ _FREQ_) mean=mean std=std;
run;

data dat_cont1_&j._2(drop=mean std);
set dat_cont1_&j.;
length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

output_suborder=1;
xx_variable="%trim(%upcase(&&var&j..))";
xx_var_label=strip(upcase(VLABEL(mean)));
xx_description="Continuous";
xx_type="mean and std";
xx_var=strip(put(mean,16.&decimal_max.))||" ("||strip(put(std,
16.&decimal_max.))||")";
xx_variable_original="%trim(%upcase(&&var&j..))";
if xx_var_label="MEAN" then xx_var_label=xx_variable;

label xx_variable="Variable" xx_var_label="Variable label"
xx_description="Description" xx_type="Type of statistic"
xx_var="Statistic";
run;

proc append base=&datout. data=dat_cont1_&j._2; run;

*part 2: median and IQR, min max;

proc means data=&dat. noprint; var &&var&j..;
output out=dat_cont2_&j.(drop=_TYPE_ _FREQ_)
p50=p50 p25=p25 p75=p75 min=min max=max;
run;

data dat_cont2_&j._2(drop=p50 p25 p75);
set dat_cont2_&j.(drop=min max);
length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

output_suborder=2;
xx_variable="%trim(%upcase(&&var&j..))";
xx_var_label=strip(upcase(VLABEL(p50)));
xx_description="Continuous";
xx_type="median and IQR";
xx_var=strip(put(p50,16.&decimal_max.))||" ("||strip(put(p25,
16.&decimal_max.))||", "||strip(put(p75, 16.&decimal_max.))||")";
xx_variable_original="%trim(%upcase(&&var&j..))";
if xx_var_label="P50" then xx_var_label=xx_variable;

label xx_variable="Variable" xx_var_label="Variable label"
xx_description="Description" xx_type="Type of statistic"
xx_var="Statistic";
run;

data dat_cont2_&j._3(drop=min max);
set dat_cont2_&j.(drop=p50 p25 p75);
length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;

output_suborder=3;
xx_variable="%trim(%upcase(&&var&j..))";
xx_var_label=strip(upcase(VLABEL(min)));
xx_description="Continuous";
xx_type="min and max";
xx_var="("||strip(put(min, 16.&decimal_max.))||", "||strip(put(max, 16.&decimal_max.))||")";
xx_variable_original="%trim(%upcase(&&var&j..))";
if xx_var_label="MIN" then xx_var_label=xx_variable;

label xx_variable="Variable" xx_var_label="Variable label"
xx_description="Description" xx_type="Type of statistic"
xx_var="Statistic";
run;

proc append base=&datout. data=dat_cont2_&j._2; run;
proc append base=&datout. data=dat_cont2_&j._3; run;

*part 3: check missingness;
proc format;
value $mchar
" "="Missing"
other="Not Missing"
;
value mnum
. ="Missing"
other="Not Missing"
;
run;

data dat_cont_check_&j.;
set &dat.(keep=&&var&j..);
format _CHAR_ $mchar. _NUMERIC_ mnum.;
run;

proc freq data=dat_cont_check_&j. noprint;
tables &&var&j../out=dat_cont_check_&j._2; * removing missing here too;
run;

data dat_cont_check_&j._3(drop=&&var&j.. COUNT PERCENT);
set dat_cont_check_&j._2;
length xx_variable xx_var_label xx_description xx_type xx_var xx_variable_original $ 100;
if vvalue(&&var&j..)="Missing";

output_suborder=4;
xx_variable="%trim(%upcase(&&var&j..))";
xx_var_label=upcase(VLABEL(&&var&j..));
xx_description="Continuous: missing";
xx_type="count and percent";
xx_var=strip(put(COUNT,16.))||" ("||strip(put(PERCENT, 16.&decimal_max.))||"%"||")";
xx_variable_original="%trim(%upcase(&&var&j..))";

label xx_variable="Variable" xx_var_label="Variable label"
xx_description="Description" xx_type="Type of statistic"
xx_var="Statistic";
format percent 8.1;
run;

proc append base=&datout. data=dat_cont_check_&j._3; run;
%end;

proc datasets library=work; delete dat_cont:; run;
%end;

%mend;

/****************************************************************************
*** Marco 2: Table_summary;
****************************************************************************/

%macro Table_summary(byvar=&group_by.);

/***************************SAS settings***************************/
options missing="" PAGENO=1 label nofmterr formdlim=" ";

*Set SAS formats if there is any external one;
%if &formatsfolder.^= %then %do; libname library &formatsfolder.;%end;

*Set where data to be summarized is stored. Default is SAS work library.;
%if &yourfolder.^= %then %do;
libname datain &yourfolder.;
data &yourdata.; set datain.&yourdata.; run;
%end;
%else %do; data &yourdata.; set work.&yourdata.; run; %end;

*If none of variable is specified as categorical or continuous.;
%if &varlist_cat.= and &varlist_cont.= %then %do;
%put ERROR: no variable is specified.; %return;
%end;

*If no variable output order is specifed, then use the default order.;
%if &output_order.= %then %do;
%let output_order=%sysfunc(catx(%str( ),&varlist_cont., &varlist_cat.));
%end;

/**********************If group_by option is NOT used*******************/

%if &byvar.= %then %do;

*Count the total number of rows;
proc sql; select count(*) into: n_population from &yourdata.; quit;
%let n_population = &n_population.;

*Call the macro %getvars to get all statistics;
%getvars(dat=&yourdata., datout=&output_data.)

data &output_data.;
set &output_data.(rename=(xx_var=xx_var_all));
label xx_var_all="All (n=%trim(&n_population.))";
run;

*Create a dataset that contains variable output orders.;
proc datasets; delete table_orders; run;
%let noutput_order=%sysfunc(countw(&output_order.));

data table_orders;
length xx_variable_original $ 100 output_order 8;
xx_variable_original=" "; output_order=0;
output;
run;

%do tableorder=1 %to &noutput_order.;
%let tableorder_var&tableorder.=%scan(&output_order.,&tableorder.);
data table_orders;
set table_orders end=eof;
length xx_variable_original $ 100 output_order 8; output;
if eof then do;
xx_variable_original="%trim(%upcase(&&tableorder_var&tableorder..))";

output_order=&tableorder.;
output;
end;
run;
%end;

data table_orders; set table_orders(where=(output_order^=0)); run;

*Add variable output orders to dataset with all statistics;
*Sort data by specified orders;
proc sort data=&output_data.; by xx_variable_original output_suborder;
proc sort data=table_orders nodupkey; by xx_variable_original; run;
data &output_data.;
merge &output_data.(in=a) table_orders;
by xx_variable_original;
if a;
run;
proc datasets; delete table_orders; run;

proc sort data=&output_data.; by output_order output_suborder; run;
data &output_data.;
set &output_data.(drop=output_order output_suborder xx_variable_original);
if indexc(xx_var_all, '0123456789')=0 then xx_var_all="";
if missing(xx_description) then xx_description="Missing";
if xx_variable=xx_var_label then xx_var_label=" ";
run;

*Output the final summary table;
ods listing close;
options missing="" orientation=landscape number date;
ods rtf bodytitle style=statistical file="&output_data..rtf";
footnote;
title "Descriptive Table";

proc print data=&output_data. label;
var xx_variable xx_description xx_type xx_var_all;
run;

ods rtf close;
title; footnote;

%end;

/**********************If group_by option is used*******************/

%else %do;

*Create a character variable to represent original group-by variable;
*Formatted values will be used if the original variable contains formats.;
data yourdata2;
set &yourdata.;
length &byvar._temp $ 50;

*Create a character variable to represent original group-by variable;
&byvar._temp=strip(vvalue(&byvar.));

*Whether or not delete unspeficied missing values in group-by variable;

%if &group_by_missing.=0 %then %do;
if missing(&byvar._temp) then delete;
%end;
%else %if &group_by_missing.=1 %then %do;
if missing(&byvar._temp) then &byvar._temp="Unspecified Missing";
%end;
run;

%let byvar_old=%trim(&byvar.);
%let byvar=&byvar._temp;

*compute the number of different subpopulations in group-by variable;
proc freq data=yourdata2(keep=&byvar.) noprint;
tables &byvar./out=temp_byvar;
run;
proc sql; select count(*) into: n_byvar from temp_byvar; quit;
%let n_byvar = &n_byvar.;

*for each subpopulation, compute the number of observations;
data _NULL_;
length byvar_formatted obs $ 100;
set temp_byvar;
byvar_formatted=vvalue(&byvar.);
obs="value"||strip(_N_);
obs2="counts"||strip(_N_);
call symput(obs,trim(byvar_formatted)); call symput(obs2,trim(COUNT));
run;

*for each subpopulation, compute all the statistics;
proc datasets; delete table_suborders; run;

%do m=1 %to &n_byvar.;

data temp_data_sub&m.;
set yourdata2;
if strip(upcase(vvalue(&byvar.)))=strip(upcase("&&value&m.."));
run;
%getvars(dat=temp_data_sub&m., datout=temp_Cohort_demo_sub&m.)

data temp_Cohort_demo_sub&m.;
set temp_Cohort_demo_sub&m.(rename=(xx_var=xx_var&m.));
if indexc(xx_var&m., '0123456789')=0 then xx_var&m.="";
label xx_var&m.="%qupcase(&&value&m..) (n=%trim(&&counts&m..))";
run;

proc sort data=temp_Cohort_demo_sub&m.;
by xx_variable_original xx_variable xx_var_label xx_description xx_type;
run;

proc append base=table_suborders
data=temp_Cohort_demo_sub&m.(keep=xx_variable_original xx_variable xx_var_label xx_description xx_type output_suborder);
run;
%end;

*compute the number of total observations, and statistics for all;

proc sql;select count(&byvar.) into: n_population from yourdata2; quit;
%let n_population = &n_population.;

%getvars(dat=yourdata2, datout=temp_cohort_demo_all)

data temp_cohort_demo_all(drop=output_suborder rename=(xx_var=xx_var_all));
set temp_cohort_demo_all;
if indexc(xx_var, '0123456789')=0 then xx_var="";
label xx_var="ALL (n=%trim(&n_population.))";
run;

*Create a dataset that contains variable output orders.;
proc datasets; delete table_orders; run;
%let noutput_order=%sysfunc(countw(&output_order.));
data table_orders;
length xx_variable_original $ 100 output_order 8;
xx_variable_original=" "; output_order=0; output;
run;

%do tableorder=1 %to &noutput_order.;
%let tableorder_var&tableorder.=%scan(&output_order.,&tableorder.);
data table_orders;
set table_orders end=eof;
length xx_variable_original $ 100 output_order 8;
output;
if eof then do;
xx_variable_original="%trim(%upcase(&&tableorder_var&tableorder..))";
output_order=&tableorder.;output;
end;
run;
%end;
data table_orders; set table_orders(where=(output_order^=0)); run;

proc sort data=table_orders nodupkey; by xx_variable_original; run;
proc sort data=table_suborders nodup;
by xx_variable_original xx_variable xx_var_label xx_description xx_type; run;

data orders;
merge table_orders(in=a) table_suborders;
by xx_variable_original;
if a;
run;

*Add variable output orders to dataset with all statistics;
proc sort data=orders nodup;
by xx_variable_original xx_variable xx_var_label xx_description xx_type; run;

proc sort data=temp_cohort_demo_all;
by xx_variable_original xx_variable xx_var_label xx_description xx_type; run;

data &output_data.;

%if n_byvar.>1 %then %do;
merge orders(in=a) temp_cohort_demo_all

temp_Cohort_demo_sub1 - temp_Cohort_demo_sub&n_byvar.;
%end;
%else %if n_byvar.=1 %then %do;
merge orders(in=a) temp_cohort_demo_all temp_Cohort_demo_sub1;
%end;

by xx_variable_original xx_variable xx_var_label xx_description xx_type;
if a;
run;

proc sort data=&output_data.; by output_order output_suborder; run;
data &output_data.(drop=xx_variable_original output_order output_suborder);
set &output_data.;
if missing(xx_description) then xx_description="Missing";
if xx_variable=xx_var_label then xx_var_label=" ";
run;

*Check whether there is any missing value in the group-by variable.;
proc sql;
select count(*) into: nmiss1 from &yourdata.;
select count(*) into: nmiss2 from yourdata2;
quit;
%let nmiss=%eval(&nmiss1.-&nmiss2.);
%let nmiss=%trim(&nmiss.);

*Delete unnecessary datasets;
proc datasets library=work;
delete orders table_orders table_suborders yourdata2 temp:;
run;

*Output the final summary table;
ods listing close;
options missing="" orientation=landscape number date;
ods rtf bodytitle style=statistical file="&output_data..rtf";

title "Descriptive Table by Variable &byvar_old.";
%if &group_by_missing.=0 %then %do;
footnote "NOTE: there are &nmiss. observations deleted due to missing values in the variable &byvar_old.";
%end;

proc print data=&output_data. label;
%if n_byvar.>1 %then %do;
var xx_variable xx_description xx_type xx_var_all xx_var1-xx_var&n_byvar.;
%end;
%else %if n_byvar.=1 %then %do;
var xx_variable xx_description xx_type xx_var_all xx_var1;
%end;
run;

ods rtf close;
title;footnote;
%end;
%mend;

/***********************************************************************/
/* Marco: Implementing Multiple Comparisons on Pearson Chi-Square Test
/* for an RxC Contingency Table in SAS
/* 
/* %Pairwise_Chisq(data=, group=, outcome=, count=)
/* %Chisq_MC (data=, group=, outcome=, count=, num_perm=, seed=)
/*
/* By: Man Jin and Binhuan Wang 2014
/* SAS Techinical Paper 1544-2014
/* 
/* Notes: 10/18/17
/* Multiple perutations requires more memory than available on laptop
/* and will require Biowulf to run.
/*
/* Merge statement updated as issue with running merge wo a by statement
/* in current version of SAS (9.4). Will not be able to confirm fix 
/* until multiple permuations are run.
/* 
/***********************************************************************/

%macro Pairwise_Chisq(data=, group=, outcome=, count=);
proc freq data=&data noprint;
table &group/out=out_group;
run;
data groupID;
set out_group;
GID=_n_;
run;
data _null_;
set groupID nobs=nobs;
call symput('num_group', compress(put(nobs, 11.)));
stop;
run;
data ChiTest; *store pairwise Chisquare statistics to this;
stop;
run;
%Do group1=1 %to &num_group-1; *for each pair obtain chisquare stat;
%Do group2=&group1+1 %to &num_group;
data group_pair;
set groupID;
if GID EQ &group1 then call symput('name_group1', type);
if GID EQ &group2 then call symput('name_group2', type);
run;
proc freq data=&data noprint; output out=CTout PCHI;
tables &group*&outcome /chisq cellchi2 nopercent;
where &group in ("&name_group1", "&name_group2");
weight &count;
run;

data CTout1;
set CTout;
PAIR_COMPARED="&name_group1"||" vs "||"&name_group2";
run;
data ChiTest;
set ChiTest CTout1;
run;
%End;
%End;
data ChiTest; *rename variables for final results;
set ChiTest;
CHI_STAT=_PCHI_; DF=DF_PCHI; P_VALUE=P_PCHI;
keep pair_compared Chi_stat DF p_value;
run;
%mend Pairwise_Chisq;

%macro Chisq_MC (data=, group=, outcome=, count=, num_perm=, seed=);
options nosource nonotes;
data data_ungrouped; *convert grouped data to ungrouped data; 
set &data;
keep &group &outcome; 
do i=1 to &count;
output; 
end;
run;

*** excute submacro pairwise_chisq on the original data;
%pairwise_chisq(data=&data, group=&group, outcome=&outcome, count=&count); 
data ChiTest_original; *save the pairwise chisquare test on original data;
set ChiTest;
run;

data ChiMax_all; *store max chisq stats from all permutations stop;
run;

*** repeat permutations;
%Do perm=1 %to &num_perm;
data outcome_var; *start to ermute the group assignment; 
set data_ungrouped;
keep &outcome;
run;

data group_var;
set data_ungrouped; 
random_number=ranuni(&seed); 
call symput('seed', &seed+1); 
keep &group random_number;
run;

proc sort data=group_var; *randomly permute group assignment; 
by random_number;
run;

*Alt join;
proc sql;
  create table data_permuted as
  select * from group_var as t1,outcome_var as t2
  ;
quit;

*data data_permuted; *obtain a randomly permuted data; 
*merge group_var outcome_var;
*run;

proc freq data=data_permuted noprint; *group ungrouped permuted data; 
table &group*&outcome/out=data_permuted_grouped;
run;

*execute the submacro pairwise_chisq on permuted data;

%pairwise_chisq(data=data_permuted_grouped, group=&group, outcome=&outcome, count=count);
Data ChiTest_perm; *save the pairwise chisq stat on a permuted data; 
set ChiTest;
run;

proc iml; *calculate the max of pairwise chisq on a permuted data; 
use ChiTest_perm; read all;
Chi_max=max(Chi_stat);
create ChiMax_perm var {Chi_max &perm}; append;
quit;

data ChiMax_all;
set ChiMax_all ChiMax_perm;
run;

%End; *end of all permutations;

proc iml;
use ChiTest_original; read all;
use ChiMax_all; read all;
n_pair=nrow(p_value);*number of pairs compared;

*initinals for Bonferroni and Tukey ajusted p-values;

p_Bonferroni=p_value; p_Tukey=p_value;
*for each pair calculate adjusted p-values;

do pair=1 to n_pair;
p_Bonferroni[pair]=min(p_value[pair]*n_pair,1);
p_Tukey[pair]=sum(Chi_max>Chi_stat[pair])/nrow(Chi_max);
end;

create p_adjusted var{p_Bonferroni p_Tukey}; append;
quit;

data output_final; *THIS IS FINAL OUTPUT;
merge ChiTest_original p_adjusted;
run;

proc print data=output_final; *PRINT FINAL OUTPUT;
title "RESULTS of MACRO Chisq_MC";
run;

options source notes;

%mend Chisq_MC;
quit; 

