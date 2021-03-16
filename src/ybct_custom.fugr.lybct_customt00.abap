*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.11.2017 at 14:44:33 by user GLION
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YBCT_CUSTOM.....................................*
DATA:  BEGIN OF STATUS_YBCT_CUSTOM                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YBCT_CUSTOM                   .
CONTROLS: TCTRL_YBCT_CUSTOM
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YBCT_CUSTOM                   .
TABLES: YBCT_CUSTOM                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
