*&---------------------------------------------------------------------*
*&  Include           YBCRP_CGI_TRM_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                       CLASSES                                        *
*----------------------------------------------------------------------*
* Definition of a specific class to manage the ALV
* For each method, we give the name of the standard event in the class
* according to which the ALV is defined
* In front of "IMPORTING", we give the standard parameters of the event
DATA : o_alv_trs_top          TYPE REF TO cl_dd_document,
       o_html_control         TYPE REF TO cl_gui_html_viewer.


*----------------------------------------------------------------------*
*       CLASS ylc_spec_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ylc_spec_alv  DEFINITION.
  PUBLIC  SECTION.

*   - Event "Reception of the modified data in the grid"
*    METHODS    modif_data
*    FOR EVENT  data_changed
*    OF         cl_gui_alv_grid
*    IMPORTING  er_data_changed.

*   - Event "Simple clic"
    METHODS    simple_clic
    FOR EVENT  hotspot_click
    OF         cl_gui_alv_grid
    IMPORTING  e_row_id
               e_column_id
               es_row_no
               sender.

*   - Event corresponding to the use of one of the specific buttons
*   created in the status bar
*    METHODS    handle_user_command
*    FOR EVENT  user_command
*    OF         cl_gui_alv_grid
*    IMPORTING  e_ucomm.

*   - Event "Double clic"
*    METHODS    double_clic
*    FOR EVENT  double_click
*    OF         cl_gui_alv_grid
*    IMPORTING  e_row
*               e_column.

*   - Event "Top of page"
    METHODS   handle_top_of_page
    FOR EVENT top_of_page
    OF        cl_gui_alv_grid
    IMPORTING e_dyndoc_id.

ENDCLASS.                    "ylc_spec_alv DEFINITION


* Implementation of the specific class
* For each method, we define the treatment that will be launched for the
* corresponding event
CLASS ylc_spec_alv IMPLEMENTATION.

* - Event "Reception of the modified data in the grid"
*  METHOD modif_data.
**    PERFORM m1_modif_data USING er_data_changed.
*  ENDMETHOD.                    "modif_data

* - Event "Simple clic"
  METHOD simple_clic.
    PERFORM f100_simple_clic USING e_row_id
                                   e_column_id
                                   sender.

  ENDMETHOD.                    "simple_clic


* - Event corresponding to the use of one of the specific buttons
* created in the status bar
*  METHOD handle_user_command.
*    PERFORM f100_handle_user_command USING e_ucomm.
*  ENDMETHOD.                    "handle_user_command

* - Event "Double clic"
*  METHOD double_clic.
**    PERFORM m4_double_clic USING e_row
**                                 e_column.
*  ENDMETHOD.                    "double_clic

*   - Event "Top of page"
  METHOD handle_top_of_page.
    PERFORM f300_top_of_page USING o_alv_trs_top.
  ENDMETHOD.                    "handle_top_of_page

ENDCLASS.                    "cl_spec_alv IMPLEMENTATION


*----------------------------------------------------------------------*
*                       POOLS                                          *
*----------------------------------------------------------------------*
TYPE-POOLS : ctslg.
TYPE-POOLS : icon.
TYPE-POOLS : sinfo.
TYPE-POOLS : ole2.

************************************************************************
*             TABLES
************************************************************************
TABLES : e070,
         e071,
         e07t.


*----------------------------------------------------------------------*
*                       CONSTANTS                                      *
*----------------------------------------------------------------------*

CONSTANTS c_x         TYPE c VALUE 'X'.
CONSTANTS c_h         TYPE c VALUE 'H'.
************************************************************************
*             TYPES
************************************************************************
TYPES : BEGIN OF t_e070,
        trkorr     TYPE e070-trkorr,
        trfunction TYPE e070-trfunction,
        trstatus   TYPE e070-trstatus,
        as4user    TYPE e070-as4user,
        as4date    TYPE e070-as4date,
        as4time    TYPE e070-as4time,
        END OF t_e070.

TYPES : BEGIN OF t_e07t,
        trkorr  TYPE e07t-trkorr,
        as4text TYPE e07t-as4text,
        END OF t_e07t.

TYPES : BEGIN OF t_e070_task,
        trkorr     TYPE e070-trkorr,
        trfunction TYPE e070-trfunction,
        as4date TYPE e070-as4date,
        as4time TYPE e070-as4time,
        strkorr    TYPE e070-strkorr,
        END OF t_e070_task.

TYPES : BEGIN OF t_e071,
        trkorr   TYPE e071-trkorr,
        as4pos   TYPE e071-as4pos,
        pgmid    TYPE e071-pgmid,
        object   TYPE e071-object,
        obj_name TYPE e071-obj_name,
        objfunc  TYPE e071-objfunc,
        lockflag TYPE e071-lockflag,
        gennum   TYPE e071-gennum,
        lang     TYPE e071-lang,
        activity TYPE e071-activity,
        END OF t_e071.

TYPES : BEGIN OF t_tr_text,
          trkorr TYPE trkorr,
          text_lines TYPE tline_t,
        END OF t_tr_text.

TYPES: BEGIN OF t_info,
             flag       TYPE char1,
             line       TYPE so_text255,
         END OF t_info.

TYPES : BEGIN OF t_object_list,
        trkorr    TYPE trkorr,
        task      TYPE trkorr,
        obj_name  TYPE trobj_name,
        obj_type  TYPE trobjtype, "trobj_type obj_type
        obj_delete TYPE flag,
        END OF t_object_list.

TYPES : BEGIN OF t_novers_obj,
        obj_name  TYPE trobj_name,
        obj_type  TYPE trobjtype, "trobj_type obj_type
        END OF t_novers_obj.

TYPES : BEGIN OF t_tr_list,
        trkorr TYPE trkorr,
        trfunction TYPE ytrfunction,
        END OF t_tr_list.

TYPES : BEGIN OF t_e070c,
          trkorr TYPE e070c-trkorr,
          client TYPE e070c-client,
        END OF t_e070c.
*
*TYPES : BEGIN OF t_sherlock,
*          trkorr TYPE zzcit_appro_tr-trkorr,
*          check_status TYPE zzcit_appro_tr-check_status,
*        END OF t_sherlock.

TYPES : BEGIN OF t_obj_analyzed,
          sign TYPE rssign,
          option TYPE rsoption,
          low TYPE trobj_name,
          high TYPE trobj_name,
        END OF t_obj_analyzed.

************************************************************************
*             RANGES
************************************************************************
* ybct_constants-objname for SR1.
DATA : r_trfunction      TYPE yrsrange_tt,
* ybct constants-objname for SR2
       r_trfunction_task TYPE yrsrange_tt,
* ybct constants-objname for SR2
       r_pgmid           TYPE yrsrange_tt,
* ybct constants-objname for field 03 / BR02
       r_funcname        TYPE yrsrange_tt,
       r_systlist        TYPE yrsrange_tt.

************************************************************************
*             INTERNAL TABLES
************************************************************************
DATA : wt_e07t          TYPE TABLE OF t_e07t,
       wt_e07t_troa     TYPE TABLE OF t_e07t,
       wt_tr_list       TYPE TABLE OF t_tr_list,
       wt_e070          TYPE TABLE OF t_e070,
       wt_e070_task     TYPE TABLE OF t_e070_task,
       wt_e070_novers   TYPE TABLE OF t_e070_task,
       wt_e070c         TYPE TABLE OF t_e070c,
       wt_e071          TYPE TABLE OF t_e071,
       wt_e071_novers   TYPE TABLE OF t_e071,
       wt_syst          TYPE TABLE OF tmscsyst,
       wt_bapiret2      TYPE TABLE OF bapiret2,
       wt_ybcs_trs      TYPE TABLE OF ybcs_trs,
       wt_ybcs_troa     TYPE TABLE OF ybcs_troa,
       wt_tline         TYPE TABLE OF tline,
       wt_trs_fieldcat  TYPE lvc_t_fcat,
*       wt_sherlock      TYPE TABLE OF t_sherlock,
       wt_troa_fieldcat TYPE lvc_t_fcat,
       wt_seq_type      TYPE TABLE OF ybct_seq_type,
       wt_ybct_objtyp_vers TYPE TABLE OF ybct_objtyp_vers,
       wt_ybct_xls_custom TYPE TABLE OF ybct_xls_custom,
       wt_fieldlist       TYPE TABLE OF ybct_xls_custom,
       wt_select_rows   TYPE lvc_t_row,
       wt_rowid         TYPE lvc_t_roid,
       wt_object_list   TYPE TABLE OF t_object_list,
       wt_novers_obj    TYPE TABLE OF t_novers_obj,
       wt_version_list  TYPE TABLE OF vrsd,
       wt_lversno_list  TYPE TABLE OF vrsn,
       wt_object_desc   TYPE TABLE OF rseui_set,
       wt_devclass      TYPE sinfo_devclass,
       wt_obj_analyzed  TYPE TABLE OF t_obj_analyzed.



************************************************************************
*             STRUCTURES
************************************************************************
DATA : ws_e070            TYPE t_e070,
       ws_e07t            TYPE t_e07t,
       ws_ybct_objtyp_vers TYPE ybct_objtyp_vers,
       ws_e070_novers     TYPE t_e070_task,
       ws_e070_task       TYPE t_e070_task,
       ws_e070c           TYPE t_e070c,
       ws_e071            TYPE t_e071,
       ws_es_cofile       TYPE ctslg_cofile,
       ws_systems         TYPE ctslg_system,
       ws_tr_text         TYPE t_tr_text,
       ws_ybcs_trs        TYPE ybcs_trs,
*       ws_sherlock        TYPE t_sherlock,
       ws_ybcs_troa       TYPE ybcs_troa,
       ws_text_header     TYPE thead,
       ws_funcname        TYPE rsrange,
       ws_tline           TYPE tline,
       ws_bapiaddr3       TYPE bapiaddr3,
       ws_ybct_custom     TYPE ybct_custom,
       ws_ybct_xls_custom TYPE ybct_xls_custom,
       ws_steps           TYPE ctslg_step,
       ws_actions         TYPE ctslg_action,
       ws_fieldcat        TYPE lvc_s_fcat,
       ws_layout          TYPE lvc_s_layo,
       ws_rowid           TYPE lvc_s_roid,
       ws_seq_type        TYPE ybct_seq_type,
       ws_object_list     TYPE t_object_list,
       ws_novers_obj      TYPE t_novers_obj,
       ws_version_list    TYPE vrsd,
       ws_object_desc     TYPE rseui_set,
       ws_request_to      TYPE trwbo_request,
       ws_request_from    TYPE trwbo_request,
       ws_tr_list         TYPE t_tr_list,
       ws_obj_analyzed    TYPE t_obj_analyzed.

************************************************************************
*             CONSTANTS
************************************************************************

************************************************************************
*             DATA
************************************************************************
DATA : w_sel_crit_error      TYPE flag,
       w_first_display_troa  TYPE flag,
       w_first_display_trs   TYPE flag,
       w_custom_error        TYPE flag,
       w_cust_sel            TYPE flag,
       w_col_indx            TYPE i,
       w_sequence            TYPE i,
       w_line_indx           TYPE i,
       w_project             TYPE string,
       w_snct                TYPE i,
       w_var1                TYPE string,
       w_var2                TYPE string,
       w_obj_exist           TYPE flag,
       w_num_row             TYPE i,
       w_num_row_sel         TYPE i,
       w_filename            TYPE string,
       w_path                TYPE string,
       w_system              TYPE char3,
       w_alias               TYPE char4,
       w_first_launch        TYPE flag,
       w_fullpath            TYPE string,
       w_filter_obj_ko       TYPE flag,
       w_ot1                 TYPE string,
       w_ot2                 TYPE string,
       w_answer              TYPE string,
       w_counter             TYPE i,
       w_date_limit          TYPE sy-datum,
       w_tr_devl             TYPE i,
       w_tr_devl_rel         TYPE i,
       w_tr_qual             TYPE i,
       w_tr_old              TYPE i,
       w_column_desc         TYPE string,
       w_column_value        TYPE string,
       w_tr_inte             TYPE i,
       w_tr_prod             TYPE i,
       w_sel_crit_error_troa TYPE flag,
       w_obj_desc            TYPE char80.

* ALV Container
DATA : o_custom_container_trs  TYPE REF TO cl_gui_custom_container,
       o_custom_container_troa TYPE REF TO cl_gui_custom_container.
* ALV Objet
DATA : o_alv_trs             TYPE REF TO cl_gui_alv_grid,
       o_alv_troa            TYPE REF TO cl_gui_alv_grid.

* Object defined according to the created specific class
DATA : o_event_receiver_trs      TYPE REF TO ylc_spec_alv,
       o_event_receiver_troa     TYPE REF TO ylc_spec_alv.

DATA  o_top_container        TYPE REF TO cl_gui_container."Top Container
* Splitter in order to edit a top of page in the the Custom container
DATA  o_split                TYPE REF TO cl_gui_easy_splitter_container.
* Bottom Container
DATA  o_bottom_container     TYPE REF TO cl_gui_container.

*Excel objects
DATA: o_excel     TYPE ole2_object,
      o_workbook  TYPE ole2_object,
      o_tmp_workbook  TYPE ole2_object,
      o_sheet     TYPE ole2_object,
      o_cell      TYPE ole2_object,
      o_cell1     TYPE ole2_object,
      o_interior  TYPE ole2_object,
      o_cell2     TYPE ole2_object,
      o_column    TYPE ole2_object,
      o_range     TYPE ole2_object,
      o_borders   TYPE ole2_object,
      o_button    TYPE ole2_object,
      o_int       TYPE ole2_object,
      o_font      TYPE ole2_object,
      o_row       TYPE ole2_object.

* Exluded buttons of the ALV toolbar
DATA : wt_exclude TYPE ui_functions.
DATA : BEGIN OF ws_exclude,
         fcode TYPE rsmpe-func,
       END OF ws_exclude.

************************************************************************
*             FIELD-SYMBOLS
************************************************************************
FIELD-SYMBOLS : <w_fieldvalue> TYPE any.
