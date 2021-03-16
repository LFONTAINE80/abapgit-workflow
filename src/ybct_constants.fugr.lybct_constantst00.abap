*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.11.2017 at 14:39:57 by user GLION
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YBCT_CONSTANTS..................................*
DATA:  BEGIN OF STATUS_YBCT_CONSTANTS                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YBCT_CONSTANTS                .
CONTROLS: TCTRL_YBCT_CONSTANTS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YBCT_CONSTANTS                .
TABLES: YBCT_CONSTANTS                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
