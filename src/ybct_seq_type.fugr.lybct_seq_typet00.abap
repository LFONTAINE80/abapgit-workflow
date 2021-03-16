*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.11.2017 at 14:38:22 by user GLION
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YBCT_SEQ_TYPE...................................*
DATA:  BEGIN OF STATUS_YBCT_SEQ_TYPE                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YBCT_SEQ_TYPE                 .
CONTROLS: TCTRL_YBCT_SEQ_TYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YBCT_SEQ_TYPE                 .
TABLES: YBCT_SEQ_TYPE                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
