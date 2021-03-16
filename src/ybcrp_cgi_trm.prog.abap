*&---------------------------------------------------------------------*
*& Module Pool       YBCRP_CGI_TRM
*&
*&---------------------------------------------------------------------*

REPORT  ybcrp_cgi_trm.

* Include : Data declaration
INCLUDE ybcrp_cgi_trm_top.
* Include : Selection screen
INCLUDE ybcrp_cgi_trm_sel.
* Include : PBO modules
INCLUDE ybcrp_cgi_trm_pbo.
* Include : PAI modules
INCLUDE ybcrp_cgi_trm_pai.
* Include : Routines
INCLUDE ybcrp_cgi_trm_form.
* Include : Routines Exit
INCLUDE ybcrp_cgi_trm_form_exit.

START-OF-SELECTION.
  REFRESH wt_seq_type.
* SR5 - Get relationship between objetc type and sequence order number
* Use to sequence TR list
  SELECT *
  FROM ybct_seq_type
  INTO TABLE wt_seq_type.

  IF sy-subrc = 0.
    SORT wt_seq_type BY object_type.
  ENDIF.


* SRx - Get Object type aliases in order to access to object version
  REFRESH wt_ybct_objtyp_vers.
* SR5
  SELECT *
  FROM ybct_objtyp_vers
  INTO TABLE wt_ybct_objtyp_vers.

  IF sy-subrc = 0.
    SORT wt_ybct_objtyp_vers BY objtyp_src.
  ENDIF.

* SRx - Get Excel fiel generation data
  REFRESH wt_ybct_xls_custom.
  SELECT * FROM ybct_xls_custom
           INTO TABLE wt_ybct_xls_custom.

  IF sy-subrc = 0.
    SORT wt_ybct_xls_custom BY fieldpos fieldname.
  ENDIF.

  REFRESH r_systlist.
  PERFORM f000_get_constants_data USING 'CGI_TRM'
                                        'SYSTLIST'
                                  CHANGING r_systlist.

  CLEAR w_date_limit.
  CALL FUNCTION 'CALCULATE_DATE'
    EXPORTING
*     DAYS        = '0'
      months      = '-36'
      start_date  = sy-datum
    IMPORTING
      result_date = w_date_limit.

* Call General screen of CGI Transport Request Manager
  CALL SCREEN 100.
