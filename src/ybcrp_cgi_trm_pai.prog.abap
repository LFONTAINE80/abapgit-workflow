*&---------------------------------------------------------------------*
*&  Include           YBCRP_CGI_TRM_PAI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       Module to manage Exit command of TRM (Screen 0100)
*----------------------------------------------------------------------*
MODULE exit_command INPUT.
* Check SY-UCOMM value in order to manage the program exit
  CASE sy-ucomm.
    WHEN 'EXIT' OR 'RETURN' OR 'BACK'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       Module to manage User actions (Program Execution and Toolbar
*       actions)
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
* Check SY-UCOMM in order to get the user actions in screen 0100
  CASE sy-ucomm.

    WHEN 'EXEC'.

*     When we have the data of transport request object analysis which
*     are displaying, we have to clear the second alv when we are
*     raising a new selection of data
      IF o_alv_troa IS NOT INITIAL.
        CLEAR : o_alv_troa,w_first_display_troa.
        CALL METHOD o_custom_container_troa->free.
        CLEAR o_custom_container_troa.
      ENDIF.

*     Data selection according selection criteria
      PERFORM f100_data_selection_trs.
      IF w_sel_crit_error IS INITIAL.
        PERFORM f100_fill_alv_structure_trs.
        IF wt_ybcs_trs[] IS INITIAL.
          MESSAGE i001(ytrm_msg) DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
        PERFORM f100_display_alv USING o_custom_container_trs
                                       'TRS_ALV_CC'
                                       'YBCS_TRS'
                                       'X'
                                       o_alv_trs
                                       o_event_receiver_trs
                                       wt_ybcs_trs
                                       w_first_display_trs
                                CHANGING wt_trs_fieldcat.
      ELSE.
        IF o_alv_trs IS NOT INITIAL.
          CLEAR o_alv_trs.
          CLEAR : o_split, o_top_container, o_bottom_container,
                  o_html_control.
          CALL METHOD o_custom_container_trs->free.
          CLEAR w_first_display_trs.
          CLEAR o_custom_container_trs.
          CLEAR o_event_receiver_trs.
        ENDIF.
      ENDIF.
* We handle the button Application Help
    WHEN 'HELP'.
      PERFORM f100_display_help.
*      We handle the button check TR Syntax
    WHEN 'CHECK'.
*     Get the selected rows
      PERFORM f100_get_selected_rows.
*     Realize the action of the check TR syntax button
      PERFORM f100_do_action_check.
*      We handle the button MERGE
    WHEN 'MERGE'.
*     Get the selected rows
      PERFORM f100_get_selected_rows.
*     Realize the action of the merge button
      PERFORM f100_do_action_merge.
*     Data selection according selection criteria
      PERFORM f100_data_selection_trs.
      IF w_sel_crit_error IS INITIAL.
        PERFORM f100_fill_alv_structure_trs.
        IF wt_ybcs_trs[] IS INITIAL.
          MESSAGE i001(ytrm_msg) DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
        PERFORM f100_display_alv USING o_custom_container_trs
                                       'TRS_ALV_CC'
                                       'YBCS_TRS'
                                       'X'
                                       o_alv_trs
                                       o_event_receiver_trs
                                       wt_ybcs_trs
                                       w_first_display_trs
                                CHANGING wt_trs_fieldcat.
      ELSE.
        IF o_alv_trs IS NOT INITIAL.
          CLEAR o_alv_trs.
          CLEAR : o_split, o_top_container, o_bottom_container,
                  o_html_control.
          CALL METHOD o_custom_container_trs->free.
          CLEAR w_first_display_trs.
          CLEAR o_custom_container_trs.
          CLEAR o_event_receiver_trs.
        ENDIF.
      ENDIF.
*      We handle the button CHANGE TR DESCRIPTION
*    WHEN 'CHANGEDES'.
**     Get the selected rows
*      PERFORM f100_get_selected_rows.
**     Data selection according selection criteria
*      PERFORM f100_data_selection_trs.
**     Data selection according selection criteria
*      PERFORM f100_data_selection_trs.
*      IF w_sel_crit_error IS INITIAL.
*        PERFORM f100_fill_alv_structure_trs.
*        IF wt_ybcs_trs[] IS INITIAL.
*          MESSAGE i001(ytrm_msg) DISPLAY LIKE 'E'.
*          EXIT.
*        ENDIF.
*        PERFORM f100_display_alv USING o_custom_container_trs
*                                       'TRS_ALV_CC'
*                                       'YBCS_TRS'
*                                       'X'
*                                       o_alv_trs
*                                       o_event_receiver_trs
*                                       wt_ybcs_trs
*                                       w_first_display_trs
*                                CHANGING wt_trs_fieldcat.
*      ELSE.
*        IF o_alv_trs IS NOT INITIAL.
*          CLEAR o_alv_trs.
*          CLEAR : o_split, o_top_container, o_bottom_container,
*                  o_html_control.
*          CALL METHOD o_custom_container_trs->free.
*          CLEAR w_first_display_trs.
*          CLEAR o_custom_container_trs.
*          CLEAR o_event_receiver_trs.
*        ENDIF.
*      ENDIF.

*      We handle the button CHANGE TR OWNER
    WHEN 'CHANGETR'.
*     Get the selected rows
      PERFORM f100_get_selected_rows.
*      We handle the button ADD TEXT
      PERFORM f100_do_change_owner.
*     Data selection according selection criteria
      PERFORM f100_data_selection_trs.
      IF w_sel_crit_error IS INITIAL.
        PERFORM f100_fill_alv_structure_trs.
        IF wt_ybcs_trs[] IS INITIAL.
          MESSAGE i001(ytrm_msg) DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
        PERFORM f100_display_alv USING o_custom_container_trs
                                       'TRS_ALV_CC'
                                       'YBCS_TRS'
                                       'X'
                                       o_alv_trs
                                       o_event_receiver_trs
                                       wt_ybcs_trs
                                       w_first_display_trs
                                CHANGING wt_trs_fieldcat.
      ELSE.
        IF o_alv_trs IS NOT INITIAL.
          CLEAR o_alv_trs.
          CLEAR : o_split, o_top_container, o_bottom_container,
                  o_html_control.
          CALL METHOD o_custom_container_trs->free.
          CLEAR w_first_display_trs.
          CLEAR o_custom_container_trs.
          CLEAR o_event_receiver_trs.
        ENDIF.
      ENDIF.
    WHEN 'ADDTEXT'.
*     Get the selected rows
      PERFORM f100_get_selected_rows.
      PERFORM f100_do_action_addtext.
*      We handle the button EXPORT TO EXCEL
    WHEN 'EXPORT'.
      PERFORM f100_do_export_excel.
    WHEN 'SEQUENCE'.
*     Get the selected rows
*      PERFORM f100_get_selected_rows. " not necessary because we do
*      the filter on all fields.
      PERFORM f100_do_action_sequence.
*      We handle the button analyse (display of the second ALV)
    WHEN 'ANALYZE'.
*     Get the selected rows
      PERFORM f100_get_selected_rows.
      refresh wt_obj_analyzed.

* If we have the conditions
      IF w_num_row_sel > 0.
        PERFORM f100_data_selection_troa.
        IF w_sel_crit_error_troa IS INITIAL.
          IF wt_object_list[] IS INITIAL.
            MESSAGE i013(ytrm_msg).
            EXIT.
          ENDIF.
          REFRESH wt_ybcs_troa.
          PERFORM f100_fill_alv_structure_troa.
*      Handle ALV TROA
          PERFORM f100_display_alv USING o_custom_container_troa
                                         'TROA_ALV_CC'
                                         'YBCS_TROA'
                                         space
                                         o_alv_troa
                                         o_event_receiver_troa
                                         wt_ybcs_troa
                                         w_first_display_troa
                                  CHANGING wt_troa_fieldcat.
        ENDIF.
      ELSE.
        MESSAGE i010(ytrm_msg) DISPLAY LIKE 'E'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT
