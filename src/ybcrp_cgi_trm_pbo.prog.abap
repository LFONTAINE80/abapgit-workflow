*&---------------------------------------------------------------------*
*&  Include           YBCRP_CGI_TRM_PBO
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       Module to manage GUI status and title (Screen 0100)
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
* Table to store excluding button
  DATA wt_fcode TYPE TABLE OF sy-ucomm.
  DATA : ws_screen TYPE screen.

*  Check the displaying of ALV Transport request Statutes
*  If ALV is hidden, don't activate the Toolbar buttons
  REFRESH : wt_fcode.
  IF w_first_display_trs IS INITIAL.
    APPEND 'CHECK' TO wt_fcode.
    APPEND 'MERGE' TO wt_fcode.
    APPEND 'CHANGEDES' TO wt_fcode.
    APPEND 'CHANGETR' TO wt_fcode.
    APPEND 'ADDTEXT' TO wt_fcode.
    APPEND 'EXPORT' TO wt_fcode.
    APPEND 'SEQUENCE' TO wt_fcode.
    APPEND 'ANALYZE' TO wt_fcode.
  ELSE.
    APPEND 'CHANGEDES' TO wt_fcode.
  ENDIF.

* Set GUI STATUS
  SET PF-STATUS 'Y_STATUS_0100' EXCLUDING wt_fcode.
* Set GUI Title
  SET TITLEBAR 'Y_TITLEBAR_0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
