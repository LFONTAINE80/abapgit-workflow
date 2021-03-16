*&---------------------------------------------------------------------*
*&  Include           YBCRP_CGI_TRM_FORM_EXIT
*&---------------------------------------------------------------------*
* We handle the icon status for the different status
* devl,qual,inte,prod

FORM f100_icon_status_management USING fuw_column_name TYPE flag
                                       fuw_fieldname TYPE lvc_fname
                              CHANGING fcw_fieldvalue TYPE string.

  DATA : ls_sysname TYPE rsrange,
         l_const TYPE yconsname,
         lr_sysname TYPE yrsrange_tt.

  CLEAR : w_var1, w_var2.
  SPLIT fuw_fieldname AT '_' INTO w_var1 w_var2.

  REFRESH lr_sysname.
  CLEAR l_const.
  CONCATENATE 'SYSTEM' w_var2 INTO l_const.
  PERFORM f000_get_constants_data USING 'CGI_TRM'
                                        l_const
                                  CHANGING lr_sysname.

  CLEAR ls_sysname.
*   Only one entry could be retrieved
  READ TABLE lr_sysname INTO ls_sysname INDEX 1.
  IF sy-subrc <> 0.
    CLEAR ls_sysname.
  ENDIF.

  IF fuw_column_name IS INITIAL.
    CASE fcw_fieldvalue.
      WHEN '@EB@'.
        fcw_fieldvalue = text-v01.
* We handle color grey for not release
        PERFORM f200_fill_backcol USING '15'.
      WHEN '@08@'.
        fcw_fieldvalue = text-v02.
* green, or release without problem
        PERFORM f200_fill_backcol USING '10'.
      WHEN '@09@'.
        fcw_fieldvalue = text-v04.
* Yellow for alert messages
        PERFORM f200_fill_backcol USING '6'.
      WHEN '@0A@'.
        fcw_fieldvalue = text-v03.
* red for error messages
        PERFORM f200_fill_backcol USING '3'.
      WHEN OTHERS.
        CLEAR fcw_fieldvalue.

    ENDCASE.
  ELSE.
    CONCATENATE text-044 ls_sysname-low INTO fcw_fieldvalue
                                          SEPARATED BY space.
  ENDIF.
ENDFORM.                    "f100_icon_status_management

*&---------------------------------------------------------------------*
*&      Form  F100_ICON_TEXT_MANAGEMENT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f100_icon_text_management  USING fuw_column_name TYPE flag
                                      fuw_fieldname TYPE lvc_fname
                             CHANGING fcw_fieldvalue TYPE string.

  DATA : l_name TYPE thead-tdname,
         l_text TYPE string.

  IF fuw_column_name IS INITIAL.
    IF fcw_fieldvalue IS NOT INITIAL.
      CLEAR l_name.
      l_name = ws_ybcs_trs-trkorr.
      REFRESH wt_tline.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'YTRM'
          language                = sy-langu
          name                    = l_name
          object                  = 'YTRM'
        TABLES
          lines                   = wt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        CLEAR ws_tline.
        CLEAR l_text.
        LOOP AT wt_tline INTO ws_tline.
          IF l_text IS INITIAL.
            l_text = ws_tline-tdline.
          ELSE.
            CONCATENATE l_text
                        cl_abap_char_utilities=>newline
                        ws_tline-tdline
                        INTO l_text.
          ENDIF.
        ENDLOOP.
        fcw_fieldvalue = l_text.
      ENDIF.

    ENDIF.
  ELSE.
    fcw_fieldvalue = text-t07.
  ENDIF.
ENDFORM.                    "f100_icon_text_management

*&---------------------------------------------------------------------*
*&      Form  F100_ICON_SHERLOCK_MANAGEMENT
*&---------------------------------------------------------------------*
*       We handle the display excel for the icon sherlock.
*----------------------------------------------------------------------*
FORM f100_icon_sherlock_management USING fuw_column_name TYPE flag
                                         fuw_fieldname TYPE lvc_fname
                                CHANGING fcw_fieldvalue TYPE string.

  IF fuw_column_name IS INITIAL.
    CASE fcw_fieldvalue.
      WHEN '@38@'.
        fcw_fieldvalue = text-v05.
        PERFORM f200_fill_backcol USING '10'.
      WHEN '@02@'.
        fcw_fieldvalue = text-v06.
        PERFORM f200_fill_backcol USING '3'.
      WHEN OTHERS.
        fcw_fieldvalue = text-v07.
        PERFORM f200_fill_backcol USING '15'.
    ENDCASE.
  ELSE.
    fcw_fieldvalue = text-t08.
  ENDIF.
ENDFORM.                    "F100_ICON_SHERLOCK_MANAGEMENT

*&---------------------------------------------------------------------*
*&      Form  F100_PROJECT_FIELD_MANAGEMENt
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FUW_COLUMN_NAME  text
*      -->FUW_FIELDNAME    text
*      -->FCW_FIELDVALUE   text
*----------------------------------------------------------------------*
FORM f100_project_field_management USING fuw_column_name TYPE flag
                                         fuw_fieldname TYPE lvc_fname
                                CHANGING fcw_fieldvalue TYPE string.

*  DATA : lr_project TYPE TABLE OF rsrange,
*         ls_project TYPE rsrange,
*         l_answer TYPE c,
*         lt_spopli TYPE TABLE OF spopli,
*         ls_spopli TYPE spopli.
*
*  IF fuw_column_name IS INITIAL.
*    IF w_project IS INITIAL.
*      REFRESH lt_spopli.
*      CLEAR l_answer.
*      REFRESH lr_project.
*      PERFORM f000_get_constants_data USING 'CGI_TRM'
*                                            'PROJECT'
*                                      CHANGING lr_project.
*
*      CLEAR ls_project.
*      LOOP AT lr_project INTO ls_project.
*        CLEAR ls_spopli.
*        ls_spopli-varoption = ls_project-low.
*        APPEND ls_spopli TO lt_spopli.
*      ENDLOOP.
** A pop-up in order to choose the project which the ot
** selected are referenced
*      CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
*        EXPORTING
*          start_col          = 5
*          start_row          = 5
*          textline1          = text-p01
*          textline2          = text-p02
*          titel              = text-p03
*        IMPORTING
*          answer             = l_answer
*        TABLES
*          t_spopli           = lt_spopli
*        EXCEPTIONS
*          not_enough_answers = 1
*          too_much_answers   = 2
*          too_much_marks     = 3
*          OTHERS             = 4.
*      IF sy-subrc = 0 AND l_answer NE 'A'.
*        CLEAR ls_spopli.
*        READ TABLE lt_spopli INTO ls_spopli INDEX l_answer.
*        IF sy-subrc = 0.
*          fcw_fieldvalue =  ls_spopli-varoption.
*          w_project = ls_spopli-varoption.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      fcw_fieldvalue = w_project.
*    ENDIF.
*
*  ELSE.
*    fcw_fieldvalue = text-t09.
*    CLEAR w_project.
*  ENDIF.
ENDFORM.                    "F100_PROJECT_FIELD_MANAGEMENt

*&---------------------------------------------------------------------*
*&      Form  F100_SRC_CLT_FIELD_MANAGEMENT
*&---------------------------------------------------------------------*
* We handle the column source (src) for the second spreadsheet.
*----------------------------------------------------------------------*
FORM f100_src_clt_field_management  USING fuw_column_name TYPE flag
                                          fuw_fieldname TYPE lvc_fname
                                 CHANGING fcw_fieldvalue TYPE string.

  IF fuw_column_name IS INITIAL.
    CLEAR ws_e070c.
    READ TABLE wt_e070c INTO ws_e070c
                        WITH KEY trkorr = ws_ybcs_trs-trkorr
                        BINARY SEARCH.
    IF sy-subrc = 0.
      CONCATENATE sy-sysid ws_e070c-client INTO fcw_fieldvalue.
    ENDIF.
  ELSE.
    fcw_fieldvalue = text-t10.
  ENDIF.

ENDFORM.                    "F100_SRC_CLT_FIELD_MANAGEMENT

*&---------------------------------------------------------------------*
*&      Form  f100_sequence_management
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FUW_COLUMN_NAME  text
*      -->FUW_FIELDNAME    text
*      -->FCW_FIELDVALUE   text
*----------------------------------------------------------------------*
FORM f100_sequence_management USING fuw_column_name TYPE flag
                                    fuw_fieldname TYPE lvc_fname
                           CHANGING fcw_fieldvalue TYPE string.

  IF fuw_column_name IS INITIAL.
    CASE w_alias.
      WHEN 'QUAL'.
        IF ws_ybcs_trs-status_devl = icon_light_out.
          fcw_fieldvalue = w_sequence.
        ELSE.
          MESSAGE i029(ytrm_msg) INTO fcw_fieldvalue.
        ENDIF.
      WHEN 'INTE'.
        IF ws_ybcs_trs-status_qual IS NOT INITIAL
          AND ws_ybcs_trs-status_inte IS INITIAL.
          fcw_fieldvalue = w_sequence.
        ELSE.
          IF ws_ybcs_trs-status_qual IS INITIAL.
            MESSAGE i030(ytrm_msg) INTO fcw_fieldvalue.
          ELSE.
            MESSAGE i029(ytrm_msg) INTO fcw_fieldvalue.
          ENDIF.
        ENDIF.
      WHEN 'PROD'.
        IF ws_ybcs_trs-status_inte IS NOT INITIAL
          AND ws_ybcs_trs-status_prod IS INITIAL.
          fcw_fieldvalue = w_sequence.
        ELSE.
          IF ws_ybcs_trs-status_inte IS INITIAL.
            MESSAGE i030(ytrm_msg) INTO fcw_fieldvalue.
          ELSE.
            MESSAGE i029(ytrm_msg) INTO fcw_fieldvalue.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        fcw_fieldvalue = w_sequence.
    ENDCASE.
  ELSE.
    fcw_fieldvalue = text-t11.
  ENDIF.
ENDFORM.                    "f100_sequence_management
