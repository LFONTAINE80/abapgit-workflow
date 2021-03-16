*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.11.2017 at 14:36:34 by user GLION
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YBCT_XLS_CUSTOM.................................*
DATA:  BEGIN OF STATUS_YBCT_XLS_CUSTOM               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YBCT_XLS_CUSTOM               .
CONTROLS: TCTRL_YBCT_XLS_CUSTOM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YBCT_XLS_CUSTOM               .
TABLES: YBCT_XLS_CUSTOM                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
