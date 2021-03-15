*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_YBCT_XLS_CUSTOM
*   generation date: 03.11.2017 at 14:36:30 by user GLION
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_YBCT_XLS_CUSTOM    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
