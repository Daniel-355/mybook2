OPTIONS NONUMBER NODATE PAGESIZE = MAX FORMCHAR = '|----|+|---+=|-/<>*' FORMDLIM=' ';title;
ods noproctitle;
ods listing close;
ods graphics /imagename=' example1 ' ;
ods html gpath='  ' file=' sas3ef8ce46c91.html ' (no_top_matter no_bottom_matter) style=journal;
proc means data=sashelp.class (keep = age);
run;
