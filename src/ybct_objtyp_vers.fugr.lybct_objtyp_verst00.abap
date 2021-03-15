*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 03.11.2017 at 14:45:31 by user GLION
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: YBCT_OBJTYP_VERS................................*
DATA:  BEGIN OF STATUS_YBCT_OBJTYP_VERS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_YBCT_OBJTYP_VERS              .
CONTROLS: TCTRL_YBCT_OBJTYP_VERS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *YBCT_OBJTYP_VERS              .
TABLES: YBCT_OBJTYP_VERS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
