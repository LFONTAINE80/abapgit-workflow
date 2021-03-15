FUNCTION Y_BC_CONSTANTS_GET_DATA.
*"--------------------------------------------------------------------
*"*"Interface locale :
*"  IMPORTING
*"     REFERENCE(IW_OBJNM) TYPE  EU_I_NAME
*"     REFERENCE(IW_CONST) TYPE  YCONSNAME OPTIONAL
*"  EXPORTING
*"     REFERENCE(EWT_VALUES) TYPE  YRSRANGE_TT
*"     REFERENCE(EWT_ALL_VALUES) TYPE  YBCT_CONSTANTS_TT
*"  EXCEPTIONS
*"      OBJNM_NOT_FILLED
*"      NO_ENTRY_FOUND
*"--------------------------------------------------------------------
************************************************************************
*                          DATA DECLARATION
************************************************************************
  DATA : lt_ybct_constants TYPE TABLE OF ybct_constants,
         ls_ybct_constants TYPE ybct_constants,
         ls_values         TYPE rsrange.

************************************************************************
*                            MAIN PROGRAM
************************************************************************
  REFRESH : lt_ybct_constants.
  CLEAR : ls_ybct_constants.
* Check that Workbench object name and constant name have been filled
  IF iw_objnm IS INITIAL.
    MESSAGE e002(ytrm_msg) RAISING objnm_not_filled.
  ENDIF.

* Get data from table YCAT06
  IF iw_const IS INITIAL.
    SELECT *
           INTO TABLE lt_ybct_constants
           FROM ybct_constants
           WHERE objname = iw_objnm.
  ELSE.
    SELECT *
           INTO TABLE lt_ybct_constants
           FROM ybct_constants
           WHERE objname = iw_objnm
             AND const_name = iw_const.
  ENDIF.

* Transfer data
  IF lt_ybct_constants[] IS INITIAL.
    MESSAGE e003(ytrm_msg) RAISING no_entry_found.
  ELSE.
    SORT lt_ybct_constants BY const_name counter.
    LOOP AT lt_ybct_constants INTO ls_ybct_constants.
      CLEAR ls_values.
      MOVE : ls_ybct_constants-sign TO ls_values-sign,
             ls_ybct_constants-opti TO ls_values-option,
             ls_ybct_constants-low  TO ls_values-low,
             ls_ybct_constants-high TO ls_values-high.
      APPEND ls_values TO ewt_values.
    ENDLOOP.
    ewt_all_values[] = lt_ybct_constants[].
  ENDIF.





ENDFUNCTION.
