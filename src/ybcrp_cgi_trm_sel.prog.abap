*&---------------------------------------------------------------------*
*&  Include           YBCRP_CGI_TRM_SEL
*&---------------------------------------------------------------------*
* The selection screen is encapsulated into a subscreen of CGI
* Transport request Manager central screen.
SELECTION-SCREEN BEGIN OF SCREEN 110 AS SUBSCREEN.
* Block 1 : Selection criteria
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-t01.
* First line : Selection per TR number
SELECTION-SCREEN : BEGIN OF LINE.
* Radiobutton for selection per TR number
PARAMETERS :  p_trnum RADIOBUTTON GROUP grp1 USER-COMMAND sele
                       DEFAULT 'X'.
SELECTION-SCREEN: COMMENT 3(26) text-s01,
                  POSITION 30.
* Select-options : TR number
SELECT-OPTIONS : s_trnum FOR e070-trkorr.
SELECTION-SCREEN END OF LINE.

* Second line : Selection per TR description
SELECTION-SCREEN : BEGIN OF LINE.
* Radiobutton for selection per TR description
PARAMETERS : p_trdesc RADIOBUTTON GROUP grp1.
SELECTION-SCREEN: COMMENT 3(29) text-s02,
                  POSITION 30.
* TR Description
SELECT-OPTIONS : s_trdes FOR e07t-as4text NO INTERVALS.
SELECTION-SCREEN END OF LINE.

* Third line : Selection per TR description
SELECTION-SCREEN : BEGIN OF LINE.
* Radiobutton for selection per TR project
*PARAMETERS : p_trpro RADIOBUTTON GROUP grp1.
*SELECTION-SCREEN: COMMENT 3(29) text-s03 ,
*                  POSITION 33.
* TR Project
PARAMETERS : p_proj TYPE tr_extpid NO-DISPLAY.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b01.


* Block 2 : Additional Criteria
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-t02.
* Select-options : owner name
SELECT-OPTIONS : s_trown FOR e070-as4user.

* Select-options : TR number
SELECT-OPTIONS : s_trobj FOR e071-obj_name.

SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN PUSHBUTTON 41(19) text-s04  USER-COMMAND exec.
* code bouton croix : @0W@

SELECTION-SCREEN END OF SCREEN 110.
