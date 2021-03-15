*&---------------------------------------------------------------------*
*&  Include           YBCRP_CGI_TRM_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F100_DATA_SELECTION_TRS
*&---------------------------------------------------------------------*
*       Data Selection according to selection screen criteria
*----------------------------------------------------------------------*
FORM f100_data_selection_trs.

* According to selection criteria, there are different methods to
* select Transport request information :
*    - Selection by Transport Request Number
*    - Selection by Transport Request Description
*    - Selection by Transport Request Project (Not Yet implemented)
  REFRESH : r_trfunction,
            r_trfunction_task,
            r_pgmid.

  CLEAR w_sel_crit_error.
*	SR1a : Selection by Transport Request Number
  IF p_trnum = 'X'.

* UPDATE - TEST SHERLOCK *
*    DATA : lt_ekpo TYPE TABLE OF ekpo,
*           ls_ekpo  TYPE ekpo,
*           ls_ekpo2 TYPE lt_ekpo.
*
*    REFRESH lt_ekpo.
*
*    SELECT * FROM ekpo INTO TABLE lt_ekpo.
*
*    LOOP AT lt_ekpo INTO ls_ekpo.
*      LOOP AT lt_ekpo INTO ls_ekpo2.
*      ENDLOOP.
*    ENDLOOP.
* UPDATE - TEST SHERLOCK *

*   Selection criteria check
    PERFORM f200_check_selection_criteria USING 'S_TRNUM'
                                          CHANGING w_sel_crit_error.

*   If check failed, display an error message
    IF w_sel_crit_error = 'X'.
      MESSAGE i000(ytrm_msg) DISPLAY LIKE 'E' WITH %cs01004_0110.
      " Transport Resquest List Label.
    ELSE.
      PERFORM f000_get_constants_data USING 'CGI_TRM'
                                            'TRFUNCTION'
                                      CHANGING r_trfunction.

*     Constants data error ==> Exit treatment
      IF w_sel_crit_error = 'X'.
        EXIT.
      ENDIF.

*    Check successfully processed, we start data selection
*   (Header data and description) by TR Number
      PERFORM f200_select_data_by_tr_number.

    ENDIF.

* SR1b : Selection by transport Request Description
  ELSEIF p_trdesc = 'X'.
*   Selection criteria check
    PERFORM f200_check_selection_criteria USING 'S_TRDES'
                                          CHANGING w_sel_crit_error.
*   If check failed, display an error message
    IF w_sel_crit_error = 'X'.
      MESSAGE i000(ytrm_msg) DISPLAY LIKE 'E' WITH %cs02010_0110.
      " Transport Resquest Desc. Label
    ELSE.
      PERFORM f000_get_constants_data USING 'CGI_TRM'
                                            'TRFUNCTION'
                                      CHANGING r_trfunction.

      IF w_sel_crit_error = 'X'.
        EXIT.
      ENDIF.
*    Check successfully processed, we start data selection
*      (Header data and description)by description
      PERFORM f200_select_data_by_tr_descr.
    ENDIF.
* SR1c : Selection by Transport Request Project
*  ELSE.
**   Selection criteria check
*    PERFORM f200_check_selection_criteria USING 'P_PROJ'
*                                          CHANGING w_sel_crit_error.
**   If check failed, display an error message
*    IF w_sel_crit_error = 'X'.
*      MESSAGE i000(ytrm_msg) DISPLAY LIKE 'E' WITH %cs03016_0110.
*      " Transport Resquest Proj. Label
*    ELSE.
*    ENDIF.
*    " To be defined Later
  ENDIF.

  IF w_sel_crit_error = 'X'.
    EXIT.
  ELSE.
* SR2 - Selection of Transport request tasks data (Task Number and
*       contents)
    PERFORM f000_get_constants_data USING 'CGI_TRM'
                                          'TASKFUNCTION'
                                    CHANGING r_trfunction_task.

    IF w_sel_crit_error IS INITIAL.
      PERFORM f000_get_constants_data USING 'CGI_TRM'
                                            'PGMID'
                                      CHANGING r_pgmid.
    ELSE.
      EXIT.
    ENDIF.

    IF w_sel_crit_error IS INITIAL.
      PERFORM f200_select_of_tr_tasks.
    ELSE.
      EXIT.
* SR3
    ENDIF.

    IF w_sel_crit_error IS INITIAL.
*Get the systems information in transport queue
      PERFORM f200_get_si_in_transport_queue.
    ENDIF.
  ENDIF.


ENDFORM.                    " F100_DATA_SELECTION_TRS

*&---------------------------------------------------------------------*
*&      Form  F200_CHECK_SELECTION_CRITERIA
*&---------------------------------------------------------------------*
*       Check selection criteria filled according to radio button
*       checked
*----------------------------------------------------------------------*
*      -->FUW_SEL_CRIT      Selection criteria name
*      <--FCw_sel_crit_error  Flag for checking result
*----------------------------------------------------------------------*
FORM f200_check_selection_criteria  USING    fuw_sel_crit TYPE string
                                    CHANGING fcw_sel_crit_error TYPE
                                    flag.

  CLEAR fcw_sel_crit_error.
* Check if the selection criteria is filled
* If selection criteria is empty, we returned a check failed
  CASE fuw_sel_crit.
    WHEN 'S_TRNUM'.
      IF s_trnum[] IS INITIAL.
        fcw_sel_crit_error = 'X'.
      ENDIF.

    WHEN 'S_TRDES'.
      IF s_trdes[] IS INITIAL.
        fcw_sel_crit_error = 'X'.
      ENDIF.

    WHEN 'P_PROJ'.
      IF p_proj IS INITIAL.
        fcw_sel_crit_error = 'X'.
      ENDIF.
    WHEN OTHERS.
      fcw_sel_crit_error = 'X'.
  ENDCASE.


ENDFORM.                    " F200_CHECK_SELECTION_CRITERIA

*&---------------------------------------------------------------------*
*&      Form  F200_SELECT_DATA_BY_TR_NUMBER
*&---------------------------------------------------------------------*
*       Selection by transport request number  Header Data and
*       Description Data
*----------------------------------------------------------------------*
FORM f200_select_data_by_tr_number .
  REFRESH wt_e070.
  REFRESH wt_e07t.
* Selection by transport request number  Header Data
* Selection of : TR Number, TR type, TR status, Owner
  SELECT trkorr
         trfunction
         trstatus
         as4user
         as4date
         as4time
    FROM e070
    INTO TABLE wt_e070
    WHERE trkorr IN s_trnum
    AND trfunction IN r_trfunction
    AND as4user IN s_trown
    AND strkorr = space.

* if we have selected some data
  IF NOT wt_e070[] IS INITIAL.
    SORT wt_e070 BY trkorr.
* Selection by transport request number  Description Data
* Selection of : Request/Task, Short Description of Repository Objects
    SELECT trkorr
           as4text
    FROM e07t
    INTO TABLE wt_e07t
    FOR ALL ENTRIES IN wt_e070
    WHERE trkorr = wt_e070-trkorr.
*    AND langu = sy-langu.

    IF sy-subrc = 0.
      SORT wt_e07t BY trkorr.
    ENDIF.
  ELSE.
* SR1a error message : " No data selected according
* to the selection criteria set
    MESSAGE i001(ytrm_msg) DISPLAY LIKE 'E'.
    w_sel_crit_error = 'X'.
  ENDIF.



ENDFORM.                    " F200_SELECT_DATA_BY_TR_NUMBER
*&---------------------------------------------------------------------*
*&      Form  F000_GET_CONSTANTS_DATA
*&---------------------------------------------------------------------*
*       perform to get constant_data from table YBCT_CONSTANTS
*----------------------------------------------------------------------*
*      -->FUW_OBJNM    Object Name
*      -->FUW_CONST    Constant Name
*      <--FCWT_VALUES  Range Values
*----------------------------------------------------------------------*
FORM f000_get_constants_data  USING    fuw_objnm TYPE eu_i_name
                                       fuw_const TYPE yconsname
                              CHANGING fcwt_values TYPE yrsrange_tt.

  CALL FUNCTION 'Y_BC_CONSTANTS_GET_DATA'
    EXPORTING
      iw_objnm         = fuw_objnm
      iw_const         = fuw_const
    IMPORTING
      ewt_values       = fcwt_values
    EXCEPTIONS
      objnm_not_filled = 1
      no_entry_found   = 2
      OTHERS           = 3.

  IF sy-subrc <> 0 AND ( fuw_const = 'SYSTEMINTE' OR
    fuw_const = 'SYSTEMQUAL' OR fuw_const = 'SYSTEMPROD' ).

  ELSEIF sy-subrc <> 0.
* Implement suitable error handling here
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno DISPLAY LIKE 'E'
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    w_sel_crit_error = 'X'.
  ENDIF.

ENDFORM.                    " F000_GET_CONSTANTS_DATA
*&---------------------------------------------------------------------*
*&      Form  F200_SELECT_DATA_BY_TR_DESCR
*&---------------------------------------------------------------------*
*       Perform to get Description Data and header data
*       by transport request description
*----------------------------------------------------------------------*

FORM f200_select_data_by_tr_descr .
  DATA : l_description TYPE string.
*         ls_rsrange TYPE rsrange.

  REFRESH wt_e070.
  REFRESH wt_e07t.
* Selection by transport request number  Description Data
* Selection of : Request/Task, Short Description of Repository Objects
*  CLEAR ls_rsrange.
*  LOOP AT s_trdes INTO ls_rsrange.
*
*    CLEAR l_description.
*    CONCATENATE '%' s_trdes-low '%' INTO l_description.
*    s_trdes-low = l_description.
*    ls_rsrange-option = 'CP'.
*    MODIFY s_trdes FROM ls_rsrange INDEX sy-tabix.
*  ENDLOOP.

  SELECT trkorr
         as4text
    FROM e07t
    INTO TABLE wt_e07t
*    WHERE langu = sy-langu
    WHERE as4text IN s_trdes.

* if we have selected some data
  IF NOT wt_e07t[] IS INITIAL.
    SORT wt_e07t BY trkorr.
* Selection by transport request number  Header Data
* Selection of : TR Number, TR type, TR status, Owner
    SELECT trkorr
           trfunction
           trstatus
           as4user
           as4date
            as4time
    FROM e070
    INTO TABLE wt_e070
    FOR ALL ENTRIES IN wt_e07t
    WHERE trkorr = wt_e07t-trkorr
    AND trfunction IN r_trfunction
    AND as4user IN s_trown
    AND strkorr = space.

    IF sy-subrc = 0.
      SORT wt_e070 BY trkorr.
    ENDIF.
  ELSE.
* SR1b error message : " No data selected according to
* the selection criteria set
    MESSAGE i001(ytrm_msg) DISPLAY LIKE 'E'.
    w_sel_crit_error = 'X'.
  ENDIF.

ENDFORM.                    " F200_SELECT_DATA_BY_TR_DESCR
*&---------------------------------------------------------------------*
*&      Form  F200_SELECT_OF_TR_TASKS
*&---------------------------------------------------------------------*
*       Perform to get Transport request tasks and contents SR2
*----------------------------------------------------------------------*

FORM f200_select_of_tr_tasks .

  REFRESH wt_e070_task.
  REFRESH wt_e071.
  IF wt_e070[] IS NOT INITIAL.
* Selection of Transport request tasks
    SELECT trkorr
           trfunction
           as4date
           as4time
           strkorr
    FROM e070
    INTO TABLE wt_e070_task
      FOR ALL ENTRIES IN wt_e070
    WHERE strkorr = wt_e070-trkorr
    AND trfunction IN r_trfunction_task.
* if we have selected some data
    IF sy-subrc = 0 AND NOT wt_e070_task[] IS INITIAL.
      SORT wt_e070_task BY strkorr.

* Selection of Transport request task contents
      SELECT trkorr
             as4pos
             pgmid
             object
             obj_name
             objfunc
             lockflag
             gennum
             lang
             activity
      FROM e071
      INTO TABLE wt_e071
      FOR ALL ENTRIES IN wt_e070_task
      WHERE trkorr = wt_e070_task-trkorr
      AND pgmid IN r_pgmid
      AND obj_name IN s_trobj.

      IF sy-subrc = 0.
        SORT wt_e071 BY trkorr.
      ENDIF.
    ELSE.
* SR2 error message : " No data selected according to
* the selection criteria set
      MESSAGE i001(ytrm_msg) DISPLAY LIKE 'E'.
      w_sel_crit_error = 'X'.

    ENDIF.

    SORT wt_e071 BY trkorr.
    REFRESH wt_e070c.


    SELECT trkorr
           client
           FROM e070c
           INTO TABLE wt_e070c
           FOR ALL ENTRIES IN wt_e070
    WHERE  trkorr = wt_e070-trkorr.

    IF sy-subrc = 0.
      SORT wt_e070c BY trkorr.
    ENDIF.

*    REFRESH wt_sherlock.

*    SELECT trkorr
*           check_status
*           FROM zzcit_appro_tr
*           INTO TABLE wt_sherlock
*           FOR ALL ENTRIES IN wt_e070
*    WHERE  trkorr = wt_e070-trkorr.

*    IF sy-subrc = 0.
*      SORT wt_sherlock BY trkorr.
*    ENDIF.
  ENDIF.
ENDFORM.                    " F200_SELECT_OF_TR_TASKS


*&---------------------------------------------------------------------*
*&      Form  F200_GET_SI_IN_TRANSPORT_QUEUE
*&---------------------------------------------------------------------*
*       SR3 Get the systems information in transport queue
*----------------------------------------------------------------------*
FORM f200_get_si_in_transport_queue .
*  CLEAR ws_conf.
  REFRESH wt_syst.
  CALL FUNCTION 'TMS_CFG_READ_CONFIGURATION'
    EXPORTING
      iv_domain             = space
      iv_system             = space
      iv_local_domain       = 'X'
      iv_plus_virtual       = space
      iv_plus_server        = 'X'
      iv_text_required      = 'X'
*    IMPORTING
*     es_conf               = ws_conf
    TABLES
      tt_syst               = wt_syst
    EXCEPTIONS
      tms_is_not_configured = 1
      tms_is_not_active     = 2
      system_not_active     = 3
      system_is_virtual     = 4
      system_is_foreign     = 5
      invalid_ci_config     = 6
      domain_not_found      = 7
      system_not_found      = 8
      group_not_found       = 9
      invalid_controller    = 10
      invalid_backup_ctl    = 11
      OTHERS                = 12.

  IF sy-subrc <> 0.
* Implement suitable error handling here
* Error when getting the system configuration data
    MESSAGE i003(ytrm_msg) DISPLAY LIKE 'E'.
    w_sel_crit_error = 'X'.
*  ELSE.
*    DELETE wt_syst WHERE sysnam = sy-sysid.
  ENDIF.
ENDFORM.                    " F200_GET_SI_IN_TRANSPORT_QUEUE
*&---------------------------------------------------------------------*
*&      Form  F100_FILL_ALV_STRUCTURE_TRS
*&---------------------------------------------------------------------*
*       SR4 + ALV STRUCTURE BASE
*----------------------------------------------------------------------*
FORM f100_fill_alv_structure_trs .
  DATA : l_const TYPE yconsname,
         l_name  TYPE thead-tdname,
         l_text_exist TYPE flag.

* SR4 - For all line selected in SR1, we retrieve the following
*       informations :
*           - Transport request log data
*           - User information (For Owner field)
*           - Transport request comment text
  CLEAR ws_e070.
  CLEAR w_filter_obj_ko.
*  REFRESH wt_tr_text.
  REFRESH wt_ybcs_trs.
  REFRESH : wt_object_list,
            wt_novers_obj.
  CLEAR : w_tr_devl,
          w_tr_qual,
          w_tr_old,
          w_tr_inte,
          w_tr_devl_rel,
          w_tr_prod.

  LOOP AT wt_e070 INTO ws_e070.

*   SR4.1 - Get TR log data
    CLEAR ws_es_cofile.
    CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
      EXPORTING
        iv_trkorr = ws_e070-trkorr
      IMPORTING
        es_cofile = ws_es_cofile.

* Select User information : Retrieve the user first and last name
* associated to the current TR owner
    CLEAR ws_bapiaddr3.
    REFRESH wt_bapiret2.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = ws_e070-as4user
      IMPORTING
        address  = ws_bapiaddr3 "stucture bapiaddr3
      TABLES
        return   = wt_bapiret2. "structure bapiret2

    CLEAR l_name.
    l_name = ws_e070-trkorr.
    CLEAR l_text_exist.
* Retrieve TR comment text              .
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'YTRM' "sapscript
        language                = sy-langu "thead-tdspras
        name                    = l_name
        object                  = 'YTRM'
      TABLES
        lines                   = wt_tline "structure tline
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF sy-subrc = 0.
      l_text_exist = 'X'.
* Get the data (Text value) returned into LINES parameters
* Store those information in a specific internal table in order
* to manage text updating (User Toolbar button) after ALV displaying
*      CLEAR ws_tr_text.
*      APPEND LINES OF wt_tline TO ws_tr_text-text_lines.
*      ws_tr_text-trkorr = ws_e070-trkorr.
*      APPEND ws_tr_text TO wt_tr_text.
    ENDIF.

* FIELD 01 GET TR Number
    CLEAR ws_ybcs_trs.

    ws_ybcs_trs-trkorr = ws_e070-trkorr.

* Field 21 Get Sequence order
    PERFORM f200_get_seq_number.
    IF w_filter_obj_ko IS INITIAL.
* FIELD 02 Get TR Description
      CLEAR ws_e07t.
      READ TABLE wt_e07t INTO ws_e07t WITH KEY trkorr = ws_e070-trkorr
                                      BINARY SEARCH.
      IF sy-subrc = 0.
        ws_ybcs_trs-as4text = ws_e07t-as4text.
      ENDIF.

* FIELD 03 GET TR TYPE. Implement a transacodification :
*      - When K : CUST
*      - When W : DEVL
      REFRESH r_funcname.
      CLEAR l_const.
      l_const = ws_e070-trfunction.
      PERFORM f000_get_constants_data USING 'CGI_TRM'
                                            l_const
                                      CHANGING r_funcname.

      CLEAR ws_funcname.
*   Only one entry could be retrieved
      READ TABLE r_funcname INTO ws_funcname INDEX 1.
      IF sy-subrc = 0.
        ws_ybcs_trs-trfunction = ws_funcname-low.
      ELSE.
        ws_ybcs_trs-trfunction = 'UNDF'.
      ENDIF.

* FIELD 04 TR Owner
      ws_ybcs_trs-as4user = ws_e070-as4user.
* FIELD 05 TR Owner Firstname
      ws_ybcs_trs-firstname = ws_bapiaddr3-firstname.
* FIELD 06 TR Owner Lastname
      ws_ybcs_trs-lastname = ws_bapiaddr3-lastname.


* FIELD 07 to 18 : Set TR log information (Status/Date/Time) for
* Developement (Release) / Quality,Pre prod,Prod (Import) systems
      IF w_date_limit <= ws_e070-as4date.
        PERFORM f200_fill_tr_status_info USING 'WS_YBCS_TRS'
                                                ws_ybcs_trs-trfunction
                                                'X'.

* If Transport request is not yet released, set a light out in
* development status field
        IF ws_es_cofile-systems[] IS INITIAL.
          ws_ybcs_trs-status_devl = icon_light_out.
          ADD 1 TO w_tr_devl.
          ws_ybcs_trs-sort_devl = 'A'.
          ws_ybcs_trs-sort_qual = 'B'.
          ws_ybcs_trs-sort_inte = 'B'.
*      ELSE.
*        ws_ybcs_trs-sort_devl = 'B'.
        ENDIF.
      ELSE.
        ADD 1 TO w_tr_old.
        ws_ybcs_trs-status_devl = icon_warning.
        ws_ybcs_trs-date_devl = ws_e070-as4date.
        ws_ybcs_trs-time_devl = ws_e070-as4time.
        ws_ybcs_trs-sort_devl = 'A'.
        ws_ybcs_trs-sort_qual = 'B'.
        ws_ybcs_trs-sort_inte = 'B'.

*        PERFORM f200_fill_tr_status_old USING 'WS_YBCS_TRS'
*                                              ws_ybcs_trs-trfunction
*                                              'X'.


      ENDIF.

      IF ws_ybcs_trs-sort_qual IS INITIAL.
        ws_ybcs_trs-sort_qual = 'B'.
      ENDIF.

      IF ws_ybcs_trs-sort_inte IS INITIAL.
        ws_ybcs_trs-sort_inte = 'B'.
      ENDIF.

      IF ws_ybcs_trs-status_prod IS NOT INITIAL.
        ws_ybcs_trs-sort_qual = 'B'.
        ws_ybcs_trs-sort_inte = 'B'.
      ENDIF.

* Field 19 Get TR Comment text
* Set the Text icon in the column when a text exists.
* Otherwise, keep field empty.
      IF l_text_exist = 'X'.
        ws_ybcs_trs-tr_text = icon_display_text.
      ENDIF.

** Field 20 Get SHERLOCK Status
*      CLEAR ws_sherlock.
*      READ TABLE wt_sherlock INTO ws_sherlock
*                             WITH KEY trkorr = ws_ybcs_trs-trkorr
*                             BINARY SEARCH.
*
*      IF sy-subrc = 0.
*        IF ws_sherlock-check_status = 'KO'.
*          ws_ybcs_trs-status_sherlock = icon_incomplete.
*        ELSE.
*          ws_ybcs_trs-status_sherlock = icon_checked.
*        ENDIF.
*      ENDIF.


      APPEND ws_ybcs_trs TO wt_ybcs_trs.
    ENDIF.
    CLEAR w_filter_obj_ko.
  ENDLOOP.
  CLEAR w_num_row.
  DESCRIBE TABLE wt_ybcs_trs LINES w_num_row.

ENDFORM.                    " F100_FILL_ALV_STRUCTURE_TRS
*&---------------------------------------------------------------------*
*&      Form  F200_FILL_TR_STATUS_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f200_fill_tr_status_info USING fuw_struct_name TYPE char20
                                    fuw_trfunction TYPE ytrfunction
                                    fuw_counter    TYPE flag.
  DATA : l_key1 TYPE ykey1,
         l_key2 TYPE ykey2.
  DATA : l_fieldname TYPE string,
         l_system TYPE char4.
* After reading TR log (With FM TR_READ_GLOBAL_INFO_OF_REQUEST), we
* process the following treatement for all systems included in SAP
* server

  CLEAR ws_systems.
  LOOP AT ws_es_cofile-systems INTO ws_systems.
*   Get custom data for the current system treated
*   We sent current System Id and TR function value
*  (Developement or Customzing). This case is managed only for
*   Developement system because we work into two different clients
*  Custom data returned :
*       - Client and step needed to analyze
*       - Alias system in order to manage ALV fields dynamically
*               - DEVL : Development (E1D)
*               - QUAL : Quality (E1I)
*               - INTE : Pre Producion (E1Q)
*               - PROD : Producion (E1P)
    CLEAR : l_key1,
            l_key2,
            w_custom_error,
            l_fieldname.

    IF ws_systems-systemid IN r_systlist[].
      l_key1 = ws_systems-systemid.
      l_key2 = fuw_trfunction.
      PERFORM f000_get_customs_data USING l_key1
                                          l_key2
                                          space
                                          space
                                          space
                                          space
                                    CHANGING ws_ybct_custom.
*   If no data selected
      IF w_custom_error = 'X'.
* We handle the others case with only the system id is filled in
* custom data table (For Quality, pre production and production system).
* We use "Check Star" functionality
        PERFORM f000_get_customs_data USING l_key1
                                            space
                                            space
                                            space
                                            space
                                            'X'
                                      CHANGING ws_ybct_custom.
*     If no data selected we leave
        IF w_custom_error = 'X'.
*          MESSAGE i004(ytrm_msg) WITH l_key1 l_key2 DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

      IF  w_custom_error <> 'X'.
        CLEAR l_system.
        l_system = ws_ybct_custom-valchar3.
      ENDIF.

*   Reading to access to the step wished according to custom data
*   selected previously
      CLEAR ws_steps.
      READ TABLE ws_systems-steps INTO ws_steps
                 WITH KEY clientid = ws_ybct_custom-valchar1 "Client ID
                            stepid = ws_ybct_custom-valchar2."Step number

      IF sy-subrc <> 0.
        CLEAR ws_steps.
        READ TABLE ws_systems-steps INTO ws_steps
                   WITH KEY stepid = ws_ybct_custom-valchar2."Step number

        IF sy-subrc <> 0.
          CLEAR ws_steps.
        ENDIF.
      ENDIF.

      IF ws_steps IS NOT INITIAL.
*     Reading to access to the actions information of the step selected
*     previously. SAP only managed one action per step. Therefore, we
*     check the first table entry only.
        CLEAR ws_actions.
        READ TABLE ws_steps-actions INTO ws_actions INDEX 1.
        IF sy-subrc = 0.
          CLEAR l_fieldname.
          UNASSIGN <w_fieldvalue>.
*       According to the current system analyzed, we fill the
*       correspondinfg status field (VALCHAR3 contained the system
*       alias) :
*               - DEVL : Development (E1D)
*               - QUAL : Quality (E1I)
*               - INTE : Pre Producion (E1Q)
*               - PROD : Producion (E1P)
          CONCATENATE fuw_struct_name '-STATUS_' ws_ybct_custom-valchar3
          INTO l_fieldname.

*       In order to manage all status fields dynamically, we create a
*       pointer on each Status field by assigning a field symbol
          ASSIGN (l_fieldname)  TO <w_fieldvalue>.
          IF sy-subrc = 0.
            CASE ws_actions-rc.
              WHEN 0.
                <w_fieldvalue> = icon_green_light.
              WHEN 4.
                <w_fieldvalue> = icon_yellow_light.
              WHEN 8.
                <w_fieldvalue> = icon_red_light.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.

          CLEAR l_fieldname.

          IF fuw_struct_name = 'WS_YBCS_TRS'.
            CONCATENATE fuw_struct_name '-SORT_' ws_ybct_custom-valchar3
            INTO l_fieldname.
            UNASSIGN <w_fieldvalue>.
            ASSIGN (l_fieldname)  TO <w_fieldvalue>.

            CASE ws_ybct_custom-valchar3.
              WHEN 'DEVL'.
                <w_fieldvalue> = 'A'.
              WHEN 'QUAL'.
                <w_fieldvalue> = 'A'.
                ws_ybcs_trs-sort_devl = 'B'.
              WHEN 'INTE'.
                <w_fieldvalue> = 'A'.
                ws_ybcs_trs-sort_devl = 'B'.
              WHEN OTHERS.
            ENDCASE.
          ENDIF.

          CLEAR l_fieldname.
          UNASSIGN <w_fieldvalue>.
*       According to the current system analyzed, we fill the
*       correspondinfg Date field (VALCHAR3 contained the system
*       alias) :
*               - DEVL : Development (E1D)
*               - QUAL : Quality (E1I)
*               - INTE : Pre Producion (E1Q)
*               - PROD : Producion (E1P)
          CONCATENATE fuw_struct_name '-DATE_' ws_ybct_custom-valchar3
          INTO l_fieldname.
*       In order to manage all status fields dynamically, we create a
*       pointer on each Date field by assigning a field symbol
          ASSIGN (l_fieldname)  TO <w_fieldvalue>.
          IF sy-subrc = 0.
            <w_fieldvalue> = ws_actions-date.
          ENDIF.

          CLEAR l_fieldname.
          UNASSIGN <w_fieldvalue>.
*       According to the current system analyzed, we fill the
*       correspondinfg Time field (VALCHAR3 contained the system
*       alias) :
*               - DEVL : Development (E1D)
*               - QUAL : Quality (E1I)
*               - INTE : Pre Producion (E1Q)
*               - PROD : Producion (E1P)
          CONCATENATE fuw_struct_name '-TIME_' ws_ybct_custom-valchar3
          INTO l_fieldname.
*       In order to manage all status fields dynamically, we create a
*       pointer on each Time field by assigning a field symbol
          ASSIGN (l_fieldname)  TO <w_fieldvalue>.
          IF sy-subrc = 0.
            <w_fieldvalue> = ws_actions-time.
          ENDIF.
        ENDIF.
      ELSE.
        CONCATENATE fuw_struct_name '-SORT_' ws_ybct_custom-valchar3
           INTO l_fieldname.
        UNASSIGN <w_fieldvalue>.
        ASSIGN (l_fieldname)  TO <w_fieldvalue>.

        CASE ws_ybct_custom-valchar3.
          WHEN 'QUAL'.
            <w_fieldvalue> = 'B'.
          WHEN 'INTE'.
            <w_fieldvalue> = 'B'.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF fuw_counter = 'X'.
    CASE l_system.
      WHEN 'PROD'.
        ADD 1 TO w_tr_prod.
      WHEN 'INTE'.
        IF ws_ybcs_trs-status_prod IS INITIAL.
          ADD 1 TO w_tr_inte.
        ELSE.
          ADD 1 TO w_tr_prod.
        ENDIF.
      WHEN 'QUAL'.
        IF ws_ybcs_trs-status_prod IS INITIAL AND ws_ybcs_trs-status_inte IS INITIAL.
          ADD 1 TO w_tr_qual.
        ELSEIF ws_ybcs_trs-status_prod IS INITIAL AND NOT ws_ybcs_trs-status_inte IS INITIAL.
          ADD 1 TO w_tr_inte.
        ELSEIF NOT ws_ybcs_trs-status_prod IS INITIAL.
          ADD 1 TO w_tr_prod.
        ENDIF.
      WHEN 'DEVL'.
        IF ws_ybcs_trs-status_prod IS INITIAL AND ws_ybcs_trs-status_inte IS INITIAL AND ws_ybcs_trs-status_qual IS INITIAL.
          ADD 1 TO w_tr_devl_rel.
        ELSEIF NOT ws_ybcs_trs-status_prod IS INITIAL.
          ADD 1 TO w_tr_prod.
        ELSEIF ws_ybcs_trs-status_prod IS INITIAL AND ws_ybcs_trs-status_inte IS INITIAL AND NOT ws_ybcs_trs-status_qual IS INITIAL.
          ADD 1 TO w_tr_qual.
        ELSEIF ws_ybcs_trs-status_prod IS INITIAL AND NOT ws_ybcs_trs-status_inte IS INITIAL.
          ADD 1 TO w_tr_inte.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDIF.
ENDFORM.                    " F200_FILL_TR_STATUS_INFO

*&---------------------------------------------------------------------

*&      Form  f000_get_customs_data
*&---------------------------------------------------------------------
*       Get custom data
*----------------------------------------------------------------------
FORM f000_get_customs_data USING fuw_key1       TYPE ykey1
                                 fuw_key2       TYPE ykey2
                                 fuw_key3       TYPE ykey3
                                 fuw_key4       TYPE ykey4
                                 fuw_key5       TYPE ykey5
                                 fuw_check_star TYPE flag
                           CHANGING fcws_ybct_custom TYPE ybct_custom.

  CLEAR fcws_ybct_custom.
  CALL FUNCTION 'Y_BC_CUSTOMS_GET_DATA'
    EXPORTING
      iw_key1              = fuw_key1
      iw_key2              = fuw_key2
      iw_key3              = fuw_key3
      iw_key4              = fuw_key4
      iw_key5              = fuw_key5
      iw_check_star        = fuw_check_star
    IMPORTING
      ews_ybct_custom      = fcws_ybct_custom
    EXCEPTIONS
      key_not_filled       = 1
      no_data_found        = 2
      single_access_failed = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
    w_custom_error = 'X'.
  ELSE.
    CLEAR w_custom_error.
  ENDIF.

ENDFORM.                    "f000_get_customs_data
*&---------------------------------------------------------------------*
*&      Form  F100_DISPLAY_ALV
*&---------------------------------------------------------------------*
*       Display the alv with all the dynamic parameter
*----------------------------------------------------------------------*
FORM f100_display_alv USING
             fuo_custom_container TYPE REF TO cl_gui_custom_container
             fuw_container_name   TYPE char20
             fuw_structure_name   TYPE dd02l-tabname
             fuw_split            TYPE flag
             fuo_alv_object       TYPE REF TO cl_gui_alv_grid
             fuo_event_receiver   TYPE REF TO ylc_spec_alv
             fuwt_alv_data        TYPE ANY TABLE
             fuw_first_display    TYPE flag
                     CHANGING fcwt_fieldcat TYPE lvc_t_fcat.

* Generate ALV objects and properties only once
* If trs or troa custom container is initial
  IF fuo_custom_container IS INITIAL.
* Creation of the field catalog
    PERFORM f300_fieldcatalog USING fuw_structure_name
                              CHANGING fcwt_fieldcat.
* Creation of the ALV objects
    PERFORM f300_create_object USING fuo_custom_container
                                     fuw_container_name
                                     fuw_split
                                     fuo_alv_object.

* Define layout options
    PERFORM f300_layout_options.
* Management of the events
    PERFORM f300_events USING fuw_split
                              fuo_alv_object
                              fuo_event_receiver.
* Define the status toolbar
    PERFORM f300_toolbar.
  ENDIF.

* ALV display
  PERFORM f300_alv_display USING fuw_split
                                 fuo_alv_object
                                 fuwt_alv_data
                                 fcwt_fieldcat
                                 fuw_first_display.

ENDFORM.                    " F100_DISPLAY_ALV
*&---------------------------------------------------------------------*
*&      Form  F300_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Fieldcatalog to allow to define specifications on the column
*       display of the ALV.
*----------------------------------------------------------------------*
FORM f300_fieldcatalog USING fuw_structure_name TYPE dd02l-tabname
                       CHANGING fcwt_fieldcat TYPE lvc_t_fcat.

  REFRESH fcwt_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = fuw_structure_name "name of the structure
    CHANGING
      ct_fieldcat            = fcwt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc = 0.
    CLEAR ws_fieldcat.
    LOOP AT fcwt_fieldcat INTO ws_fieldcat.
      CASE ws_fieldcat-fieldname.
        WHEN 'CHECKED'.
*         ws_trs_fieldcat-checkbox = 'X'.
*         ws_trs_fieldcat-edit = space.
          ws_fieldcat-tech = 'X'.
* ALV 2
        WHEN 'OBJ_NAME'.
          ws_fieldcat-reptext   = text-029.
          ws_fieldcat-scrtext_s = text-028.
          ws_fieldcat-scrtext_m = text-029.
          ws_fieldcat-scrtext_l = text-029.
        WHEN 'OBJ_DESC'.
          ws_fieldcat-reptext   = text-030.
          ws_fieldcat-scrtext_s = text-037.
          ws_fieldcat-scrtext_m = text-030.
          ws_fieldcat-scrtext_l = text-030.
          ws_fieldcat-tech = 'X'.
* alv 1
        WHEN 'TRKORR'.
*         We allow the simple clic on the field
          ws_fieldcat-hotspot = 'X'.
*         We set our parameter as a key, he will be always display
          ws_fieldcat-key = 'X'.
*         We handle the short/medium/long field label
          ws_fieldcat-reptext   = text-002.
          ws_fieldcat-scrtext_s = text-001.
          ws_fieldcat-scrtext_m = text-002.
          ws_fieldcat-scrtext_l = text-002.
* for both alv
        WHEN 'AS4TEXT'.
*         We allow the cell to be editable when it's specified in the
*         style table associated to the edition table
*          ws_fieldcat-edit = 'X'.
*         We set our parameter as a key, he will be always display
          ws_fieldcat-key = 'X'.
*         We handle the short/medium/long field label
          ws_fieldcat-reptext =  text-005.
          ws_fieldcat-scrtext_s = text-004.
          ws_fieldcat-scrtext_m = text-034.
          ws_fieldcat-scrtext_l = text-034.
* for second alv
        WHEN 'TR_SEQ'.
          ws_fieldcat-reptext   = text-036.
          ws_fieldcat-scrtext_s = text-035.
          ws_fieldcat-scrtext_m = text-035.
          ws_fieldcat-scrtext_l = text-036.
* alv 1
        WHEN 'TRFUNCTION'.
          ws_fieldcat-scrtext_s = text-006.
          ws_fieldcat-scrtext_m = text-007.
          ws_fieldcat-scrtext_l = text-007.
        WHEN 'AS4USER'.
*         We handle the differents text length
          ws_fieldcat-reptext = text-008.
          ws_fieldcat-scrtext_s = text-008.
          ws_fieldcat-scrtext_m = text-038.
          ws_fieldcat-scrtext_l = text-038.
        WHEN 'FIRSTNAME'.
*         We handle the differents text length
          ws_fieldcat-scrtext_s = text-009.
          ws_fieldcat-scrtext_m = text-009.
          ws_fieldcat-scrtext_l = text-009.
        WHEN 'LASTNAME'.
*         We handle the differents text length
          ws_fieldcat-scrtext_s = text-010.
          ws_fieldcat-scrtext_m = text-010.
          ws_fieldcat-scrtext_l = text-010.
        WHEN 'STATUS_DEVL'.
*         We allow the simple clic on the field
          ws_fieldcat-hotspot = 'X'.
*         We justify the icon for this field
          ws_fieldcat-just = 'C'.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C400'.
          PERFORM f400_create_column_name.
        WHEN 'DATE_DEVL'.
*         We define a clor for this field
          ws_fieldcat-emphasize = 'C400'.
          PERFORM f400_create_column_name.
        WHEN 'TIME_DEVL'.
*         We define a clor for this field
          ws_fieldcat-emphasize = 'C400'.
          PERFORM f400_create_column_name.
        WHEN 'STATUS_QUAL'.
*         We allow the simple clic on the field
          ws_fieldcat-hotspot = 'X'.
*         We justify the icon for this field
          ws_fieldcat-just = 'C'.
*         We define a clor for this field
          ws_fieldcat-emphasize = 'C300'.
          PERFORM f400_create_column_name.
        WHEN 'DATE_QUAL'.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C300'.
          PERFORM f400_create_column_name.
        WHEN 'TIME_QUAL'.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C300'.
          PERFORM f400_create_column_name.
        WHEN 'STATUS_INTE'.
*         We allow the simple clic on the field
          ws_fieldcat-hotspot = 'X'.
*         We justify the icon for this field
          ws_fieldcat-just = 'C'.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C500'.
          PERFORM f400_create_column_name.
        WHEN 'DATE_INTE'.
          CLEAR : w_var1, w_var2.
          SPLIT ws_fieldcat-fieldname AT '_' INTO w_var1 w_var2.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C500'.
          PERFORM f400_create_column_name.
        WHEN 'TIME_INTE'.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C500'.
          PERFORM f400_create_column_name.
        WHEN 'STATUS_PROD'.
*         We allow the simple clic on the field
          ws_fieldcat-hotspot = 'X'.
*         We justify the icon for this field
          ws_fieldcat-just = 'C'.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C600'.
          PERFORM f400_create_column_name.
        WHEN 'DATE_PROD'.
*         We define a color for this field
          ws_fieldcat-emphasize = 'C600'.
          PERFORM f400_create_column_name.
        WHEN 'TIME_PROD'.
*         We define a clor for this field
          ws_fieldcat-emphasize = 'C600'.
          PERFORM f400_create_column_name.
        WHEN 'TR_TEXT'.
*         We allow the simple clic on the field
          ws_fieldcat-hotspot = 'X'.

          ws_fieldcat-just = 'C'.
*         We handle the differents text length
          ws_fieldcat-reptext = text-013.
          ws_fieldcat-scrtext_s = text-012.
          ws_fieldcat-scrtext_m = text-013.
          ws_fieldcat-scrtext_l = text-013.
        WHEN 'STATUS_SHERLOCK'.
*         We handle the differents text length
          ws_fieldcat-just = 'C'.
          ws_fieldcat-tech = 'X'.
          ws_fieldcat-reptext = text-027.
          ws_fieldcat-scrtext_s = text-026.
          ws_fieldcat-scrtext_m = text-027.
          ws_fieldcat-scrtext_l = text-027.
        WHEN 'SEQUENCE_ORDER'.
*  This column is available for selection. Nevertheless we dont see it
          ws_fieldcat-tech = 'X'.
        WHEN 'SEQUENCE_ORDER_HIGH'.
*  This column is available for selection. Nevertheless we dont see it
          ws_fieldcat-tech = 'X'.
        WHEN 'SORT_DEVL'.
          ws_fieldcat-tech = 'X'.
        WHEN 'SORT_QUAL'.
          ws_fieldcat-tech = 'X'.
        WHEN 'SORT_INTE'.
          ws_fieldcat-tech = 'X'.
        WHEN OTHERS.
      ENDCASE.

      MODIFY fcwt_fieldcat FROM ws_fieldcat.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " F300_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  F300_CREATE_OBJECT
*&---------------------------------------------------------------------*
*       Creation of the ALV object
*----------------------------------------------------------------------*
FORM f300_create_object USING fuo_custom_container
                              TYPE REF TO cl_gui_custom_container
                              fuw_container_name  TYPE char20
                              fuw_split TYPE flag
                              fuo_alv_object
                              TYPE REF TO cl_gui_alv_grid.

  DATA : l_name TYPE string.

* Creation of the first container TRS Transport Request Statutes
  CREATE OBJECT fuo_custom_container
    EXPORTING
      container_name              = fuw_container_name
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  IF fuw_split = 'X'.
    CLEAR o_split.
* Splitting the Custom container to edit a TOP of page
    CREATE OBJECT o_split
      EXPORTING
        parent        = fuo_custom_container
        sash_position = 31 "Position of Splitter Bar (in Percent)
        with_border   = 0. "With Border = 1 Without Border = 0

    CLEAR o_top_container.
    CLEAR o_bottom_container.
*   Placing the containers in the splitter
    o_top_container = o_split->top_left_container .
    o_bottom_container = o_split->bottom_right_container .


* Creation of alv object in the bottom container
    CREATE OBJECT fuo_alv_object
      EXPORTING
        i_parent          = o_bottom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    FREE o_alv_trs_top.
    CREATE OBJECT o_alv_trs_top
      EXPORTING
        style = 'ALV_GRID'.
  ELSE.

* Creation of alv object in the bottom container
    CREATE OBJECT fuo_alv_object
      EXPORTING
        i_parent          = fuo_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    CLEAR l_name.
    l_name = fuw_container_name.
    CALL METHOD fuo_alv_object->set_name
      EXPORTING
        name           = l_name
      EXCEPTIONS
        cntl_error     = 1
        parent_no_name = 2
        illegal_name   = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.

ENDFORM.                    " F300_CREATE_OBJECT
*&---------------------------------------------------------------------*
*&      Form  F300_LAYOUT_OPTIONS
*&---------------------------------------------------------------------*
*       Define the layout options
*----------------------------------------------------------------------*
FORM f300_layout_options .

  CLEAR ws_layout.
* If this field is set, the list shows a striped pattern in
* the print preview and when it is printed.
  ws_layout-zebra = 'X'.
* Optimization of the columns width
  ws_layout-cwidth_opt = 'X'.
* In order to select one or more rows
  ws_layout-sel_mode = 'A'.

ENDFORM.                    " F300_LAYOUT_OPTIONS
*&---------------------------------------------------------------------*
*&      Form  F300_EVENTS
*&---------------------------------------------------------------------*
*       Management of the events
*----------------------------------------------------------------------*
FORM f300_events USING fuw_split TYPE flag
                       fuo_alv_object TYPE REF TO cl_gui_alv_grid
                       fuo_event_receiver TYPE REF TO ylc_spec_alv.

  CALL METHOD fuo_alv_object->register_edit_event
    EXPORTING
* - Either when the user press the ENTER key
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

* Creation of an object refering to the class that has been defined and
* implemented in the data declarations
  FREE fuo_event_receiver.
  CREATE OBJECT fuo_event_receiver.

* If we have a split of the "display" we handle the top of page
  IF fuw_split = 'X'.
*    SET HANDLER fuo_event_receiver->modif_data
*                FOR fuo_alv_object.
    SET HANDLER fuo_event_receiver->handle_top_of_page
                FOR fuo_alv_object.
  ENDIF.

  SET HANDLER fuo_event_receiver->simple_clic
              FOR fuo_alv_object.
*  SET HANDLER o_event_receiver->handle_user_command FOR o_alv_trs.
*  SET HANDLER fuo_event_receiver->double_clic
*              FOR fuo_alv_object.
ENDFORM.                    " F300_EVENTS
*&---------------------------------------------------------------------*
*&      Form  F300_TOOLBAR
*&---------------------------------------------------------------------*
*       Define the toolbar of the ALV
*----------------------------------------------------------------------*

FORM f300_toolbar .
* We put in an internal table the default buttons which we do not want
  REFRESH : wt_exclude.
  CLEAR : ws_exclude.
* (We find all button names in the class CL_GUI_ALV_GRID, tab
* "Attributes"; the names begin by MC* and are typed UI_FUNC)
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_check."check entries
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_cut. "cut
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_copy. "copy
* Exclude button insert with overwrite
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_paste.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_undo. "undo
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_append_row.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_insert_row.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_delete_row.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_copy_row.
* Exclude Total Menu Button
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_mb_sum.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_print.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_views.
* Exclude button display graph
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_graph.
* Exclude button end user docu. (help)
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_info.
*  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_pc_file.
* We exclude the differents possibilities for the export like html,
* word, local file, office, spreadsheet, send and abc analysis
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_html.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_to_office.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_call_abc.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_word_processor.
*  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_pc_file.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_call_xxl. " stop
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_send.
  PERFORM f300_exclude USING cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
ENDFORM.                    " F300_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  F300_ALV_DISPLAY
*&---------------------------------------------------------------------*
*       ALV Display
*----------------------------------------------------------------------*

FORM f300_alv_display USING fuw_split TYPE flag
                            fuo_alv_object TYPE REF TO cl_gui_alv_grid
                            fuwt_alv_data TYPE ANY TABLE
                            fuwt_fieldcat TYPE lvc_t_fcat
                            fuw_first_display TYPE flag.

  IF fuw_first_display IS INITIAL.
* If we have the split so if we are in the case of the first alv
    IF fuw_split = 'X'.
      CALL METHOD o_alv_trs_top->initialize_document.
    ENDIF.

    fuw_first_display  = 'X'.

    CALL METHOD fuo_alv_object->set_table_for_first_display
      EXPORTING
*       is_variant                    = ws_variant  "Variant
        is_layout                     = ws_layout   "Layout
        it_toolbar_excluding          = wt_exclude  "Excluded buttons
        i_save                        = 'A'         "Save layout
      CHANGING
        it_outtab                     = fuwt_alv_data[]
        it_fieldcatalog               = fuwt_fieldcat[]
*       it_sort                       = wt_sort[]
*       it_filter                     = wt_filter[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    IF fuw_split = 'X'.
      CALL METHOD fuo_alv_object->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = o_alv_trs_top.
    ENDIF.

* Next displays (just a refresh)
  ELSE.
    IF fuw_split = 'X'.
      CALL METHOD o_alv_trs_top->initialize_document.
    ENDIF.

    CALL METHOD fuo_alv_object->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.

    IF fuw_split = 'X'.
      CALL METHOD fuo_alv_object->list_processing_events
        EXPORTING
          i_event_name = 'TOP_OF_PAGE'
          i_dyndoc_id  = o_alv_trs_top.
    ENDIF.
  ENDIF.



ENDFORM.                    " F300_ALV_DISPLAY
*&---------------------------------------------------------------------*
*&      Form  f300_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Select information of the top of page
*----------------------------------------------------------------------*

FORM f300_top_of_page USING fuo_alv_trs_top TYPE REF TO cl_dd_document.

* Display the top of page. it contains following information :
*     - Number of TR selected
*     - Number of TR not released
*     - Number of TR released in Production system
*     - Number of TR released in Pre Production system
*     - Number of TR released in Quality system

  PERFORM f300_top_of_page_add_text USING fuo_alv_trs_top
                                          text-014
                                          space
                                          w_num_row.
*   Number of Transport Requests not released
  PERFORM f300_top_of_page_add_text USING fuo_alv_trs_top
                                          text-015
                                          space
                                          w_tr_devl.

*   Number of Transport Requests released
  PERFORM f300_top_of_page_add_text USING fuo_alv_trs_top
                                          text-017
                                          space
                                          w_tr_devl_rel.
*   Number of Transport Requests in Quality system
  PERFORM f300_top_of_page_add_text USING fuo_alv_trs_top
                                          text-016
                                          'SYSTEMQUAL'
                                          w_tr_qual.
*   Number of Transport Requests in Pre-Production
*  PERFORM f300_top_of_page_add_text USING fuo_alv_trs_top
*                                          text-016
*                                          'SYSTEMINTE'
*                                          w_tr_inte.
*   Number of Transport Requests in Production system
  PERFORM f300_top_of_page_add_text USING fuo_alv_trs_top
                                          text-016
                                          'SYSTEMPROD'
                                           w_tr_prod.

*   Number of Transport Requests without log (too old)
  PERFORM f300_top_of_page_add_text USING fuo_alv_trs_top
                                          text-047
                                          space
                                          w_tr_old.

  IF o_html_control IS INITIAL.
    CREATE OBJECT o_html_control
      EXPORTING
        parent = o_top_container.
  ENDIF.

*  CALL METHOD fuo_alv_trs_top->merge_document.
  fuo_alv_trs_top->html_control = o_html_control.

  CALL METHOD fuo_alv_trs_top->display_document
    EXPORTING
      reuse_control      = 'X'
      parent             = o_top_container
    EXCEPTIONS
      html_display_error = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
ENDFORM.                    " f300_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  F300_EXCLUDE
*&---------------------------------------------------------------------*
*      -->P_CL_GUI_ALV_GRID=>MC_FC_CHECK  text
*----------------------------------------------------------------------*
FORM f300_exclude  USING fuw_code TYPE ui_func.
  CLEAR  ws_exclude.
  MOVE   fuw_code TO ws_exclude-fcode.
  APPEND ws_exclude TO wt_exclude.
ENDFORM.                    " F300_EXCLUDE
**&---------------------------------------------------------------------
*
**&      Form  F300_RETRIEVE_SELECT_CRIT
**&---------------------------------------------------------------------
*
**       Select data for display informations in the top of page
**----------------------------------------------------------------------
*
*FORM f300_retrieve_select_crit .
*
*  DATA : lt_info TYPE TABLE OF soli.
*
*  DATA : lt_info1 TYPE TABLE OF t_info.
*  DATA : ls_info1 TYPE t_info.
*
*  DATA: l_repid    TYPE sy-repid,
*        l_variante TYPE sy-slset.
*
*  CLEAR l_repid.
*  CLEAR l_variante.
*
*  l_repid = sy-repid.
*  l_variante = sy-slset.
*
*  REFRESH lt_info.
*
** Use a standard function module to get the data
*  CALL FUNCTION 'RS_COVERPAGE_SELECTIONS'
*    EXPORTING
*      report            = l_repid
*      variant           = l_variante
*      no_import         = c_x
*    TABLES
*      infotab           = lt_info
*    EXCEPTIONS
*      error_message     = 1
*      variant_not_found = 2
*      OTHERS            = 3.
*  IF sy-subrc <> 0.
*    EXIT.
*  ENDIF.
*
*  REFRESH lt_info1.
*  lt_info1[] = lt_info[].
*
** Parameters values
*  CLEAR ls_info1.
*  LOOP AT lt_info1 INTO ls_info1.
*    IF ls_info1-flag <> c_h.
*      MOVE ls_info1-line TO ws_header.
*      APPEND ws_header TO wt_header.
*    ENDIF.
*  ENDLOOP.
*ENDFORM.                    " F300_RETRIEVE_SELECT_CRIT
**&---------------------------------------------------------------------
*
**&      Form  F100_HANDLE_USER_COMMAND
**&---------------------------------------------------------------------
*
**      -->P_E_UCOMM  text
**----------------------------------------------------------------------
*
*FORM f100_handle_user_command  USING    fuw_ucomm TYPE sy-ucomm.
*
*  IF sy-subrc = 0.
*    CLEAR fuw_ucomm.
*  ENDIF.
*
*ENDFORM.                    " F100_HANDLE_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  F100_GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*       Get the selected rows and thier number into w_num_row_sel
*----------------------------------------------------------------------*
FORM f100_get_selected_rows .
  REFRESH : wt_select_rows,
            wt_rowid.
  CLEAR :   w_num_row,
            w_num_row_sel.
* Method to get the selected rows
  CALL METHOD o_alv_trs->get_selected_rows
    IMPORTING
      et_index_rows = wt_select_rows
      et_row_no     = wt_rowid.
* Allow to have the number of row selected into w_num_row
  DESCRIBE TABLE wt_select_rows LINES w_num_row_sel.
ENDFORM.                    " F100_GET_SELECTED_ROWS
*&---------------------------------------------------------------------*
*&      Form  F300_TOP_OF_PAGE_ADD_TEXT
*&---------------------------------------------------------------------*
*       Add text to the top of page of the first ALV
*----------------------------------------------------------------------*
FORM f300_top_of_page_add_text USING fuo_alv_trs_top
                                     TYPE REF TO cl_dd_document
                                     fuw_texte
                                     fuw_system TYPE yconsname
                                     fuw_value.
  DATA : l_text(255) TYPE c.
  DATA : l_num(5) TYPE c.
  DATA : ls_sysname TYPE rsrange,
         lr_sysname TYPE yrsrange_tt.

  REFRESH lr_sysname.
  IF fuw_system IS NOT INITIAL.
    PERFORM f000_get_constants_data USING 'CGI_TRM'
                                          fuw_system
                                    CHANGING lr_sysname.

    CLEAR ls_sysname.
*   Only one entry could be retrieved
    READ TABLE lr_sysname INTO ls_sysname INDEX 1.
    IF sy-subrc <> 0.
      CLEAR ls_sysname.
    ENDIF.
  ENDIF.

  CLEAR l_text.
  CLEAR l_num.
  l_num = fuw_value.
  CONCATENATE fuw_texte ls_sysname-low ':' l_num INTO l_text
              SEPARATED BY space.

  CALL METHOD fuo_alv_trs_top->add_text
    EXPORTING
      text = l_text.

  CALL METHOD fuo_alv_trs_top->new_line.

ENDFORM.                    " F300_TOP_OF_PAGE_ADD_TEXT
*&---------------------------------------------------------------------*
*&      Form  F400_CREATE_COLUMN_NAME
*&---------------------------------------------------------------------*
*       Perform to get the different Column Name at different size
*----------------------------------------------------------------------*

FORM f400_create_column_name.
  DATA : ls_sysname TYPE rsrange,
         l_const TYPE yconsname,
         lr_sysname TYPE yrsrange_tt,
         l_text TYPE char10.

  CLEAR : w_var1, w_var2.
  SPLIT ws_fieldcat-fieldname AT '_' INTO w_var1 w_var2.

  REFRESH lr_sysname.
  CLEAR l_const.
  CONCATENATE 'SYSTEM' w_var2 INTO l_const.
  PERFORM f000_get_constants_data USING 'CGI_TRM'
                                        l_const
                                  CHANGING lr_sysname.

  IF lr_sysname IS INITIAL.
    ws_fieldcat-no_out = 'X'.
  ELSE.
    CLEAR ls_sysname.
*   Only one entry could be retrieved
    READ TABLE lr_sysname INTO ls_sysname INDEX 1.
    IF sy-subrc <> 0.
      CLEAR ls_sysname.
    ENDIF.


*         We handle the differents text length
    CLEAR l_text.
    CASE w_var1.
      WHEN 'STATUS'.
        CONCATENATE text-044 ls_sysname-low INTO l_text
                                            SEPARATED BY space.
      WHEN 'TIME'.
        CONCATENATE text-046 ls_sysname-low INTO l_text
                                            SEPARATED BY space.
      WHEN 'DATE'.
        CONCATENATE text-045 ls_sysname-low INTO l_text
                                            SEPARATED BY space.
      WHEN OTHERS.
    ENDCASE.

    ws_fieldcat-scrtext_s = l_text.
    ws_fieldcat-scrtext_m = l_text.
    ws_fieldcat-scrtext_l = l_text.
    ws_fieldcat-reptext = l_text.
  ENDIF.
ENDFORM.                    " F400_CREATE_COLUMN_NAME
*&---------------------------------------------------------------------*
*&      Form  F200_GET_SEQ_NUMBER
*&---------------------------------------------------------------------*
*       Get sequence number : We must keep the lower value according
*       to the objects included in the transport request
*----------------------------------------------------------------------*

FORM f200_get_seq_number .

  CLEAR :   ws_e070_task,
            ws_e071.
* Loop on request tasks related to the current TR request
  LOOP AT wt_e070_task INTO ws_e070_task
    WHERE strkorr = ws_ybcs_trs-trkorr.
*   Loop on Task details (which contained all objects included in the
*   current transport request)
    LOOP AT wt_e071 INTO ws_e071 WHERE trkorr = ws_e070_task-trkorr.
      CLEAR :   ws_seq_type.
*     Retrieve the sequence number corresponding to the current task
*     object
      READ TABLE wt_seq_type INTO ws_seq_type
                 WITH KEY object_type = ws_e071-object
                 BINARY SEARCH.
      IF sy-subrc = 0 .
*       First object analyzed, we get the object sequence number
        IF ws_ybcs_trs-sequence_order IS INITIAL.
          ws_ybcs_trs-sequence_order = ws_seq_type-sequence.
*       For all others objects, we get the new sequence number if the
*       current object has a lower sequence order
        ELSEIF ws_ybcs_trs-sequence_order > ws_seq_type-sequence.
          ws_ybcs_trs-sequence_order = ws_seq_type-sequence.
        ENDIF.

        IF ws_ybcs_trs-sequence_order_high IS INITIAL.
          ws_ybcs_trs-sequence_order_high = ws_seq_type-sequence.
*       For all others objects, we get the new sequence number if the
*       current object has a lower sequence order
        ELSEIF ws_ybcs_trs-sequence_order_high < ws_seq_type-sequence.
          ws_ybcs_trs-sequence_order_high = ws_seq_type-sequence.
        ENDIF.
      ELSE.
        IF ws_ybcs_trs-sequence_order IS INITIAL.
          ws_ybcs_trs-sequence_order = 'Z99'.
        ENDIF.

        IF ws_ybcs_trs-sequence_order_high IS INITIAL.
          ws_ybcs_trs-sequence_order_high = 'Z99'.
        ENDIF.
      ENDIF.

*     Get aliases or object relevance
      IF ws_ybcs_trs-trfunction NE 'CUST'.
        CLEAR ws_ybct_objtyp_vers.
        READ TABLE wt_ybct_objtyp_vers INTO ws_ybct_objtyp_vers
             WITH KEY objtyp_src = ws_e071-object
             BINARY SEARCH.

        IF sy-subrc = 0 AND ws_ybct_objtyp_vers-unrelevant IS INITIAL.
*     Create an internal table which stores link between Transport
*     request and objects
          CLEAR ws_object_list.
          ws_object_list-trkorr   = ws_ybcs_trs-trkorr.
          ws_object_list-task     = ws_e071-trkorr.
          IF ws_ybct_objtyp_vers-objtyp_tgt IS INITIAL.
            ws_object_list-obj_type = ws_e071-object.
            CLEAR ws_novers_obj.
            ws_novers_obj-obj_type = ws_e071-object.
            ws_novers_obj-obj_name = ws_e071-obj_name.
            APPEND ws_novers_obj TO wt_novers_obj.
          ELSE.
            ws_object_list-obj_type = ws_ybct_objtyp_vers-objtyp_tgt.
          ENDIF.
          ws_object_list-obj_name = ws_e071-obj_name.


* Check object existance
          CLEAR w_obj_exist.
          CALL FUNCTION 'TR_CHECK_EXIST'
            EXPORTING
              iv_pgmid             = ws_e071-pgmid
              iv_object            = ws_e071-object
              iv_obj_name          = ws_e071-obj_name
            IMPORTING
              e_exist              = w_obj_exist
            EXCEPTIONS
              tr_no_check_function = 1
              OTHERS               = 2.

          IF sy-subrc <> 0.
            ws_object_list-obj_delete = 'X'.
          ELSEIF w_obj_exist IS INITIAL.
            ws_object_list-obj_delete = 'X'.
          ELSE.
            ws_object_list-obj_delete = space.
          ENDIF.

          APPEND ws_object_list TO wt_object_list.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF sy-subrc <> 0.
      w_filter_obj_ko = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
* Case of transport request without task assigned.
  IF sy-subrc <> 0.
    ws_ybcs_trs-sequence_order = 'Z99'.
    ws_ybcs_trs-sequence_order_high = 'Z99'.
  ENDIF.
  SORT wt_novers_obj BY obj_type obj_name.
  DELETE ADJACENT DUPLICATES FROM wt_novers_obj COMPARING ALL FIELDS.

  SORT wt_object_list BY trkorr obj_type obj_name.
  DELETE ADJACENT DUPLICATES FROM wt_object_list COMPARING ALL FIELDS.
ENDFORM.                    " F200_GET_SEQ_NUMBER
*&---------------------------------------------------------------------*
*&      Form  F100_DO_ACTION_CHECK
*&---------------------------------------------------------------------*
*       Do the actions of the button : check TR syntax
*----------------------------------------------------------------------*
FORM f100_do_action_check .
*       If no row check, display an error message
  IF w_num_row_sel = 0.
    MESSAGE i005(ytrm_msg) DISPLAY LIKE 'E'.
*  Case where the check tr syntax works
  ELSEIF w_num_row_sel = '1'.
    CLEAR ws_rowid.
* We read the
    READ TABLE wt_rowid INTO ws_rowid INDEX 1.
    IF sy-subrc = 0.
      CLEAR ws_ybcs_trs.
* We read the
      READ TABLE wt_ybcs_trs INTO ws_ybcs_trs INDEX ws_rowid-row_id.
      IF sy-subrc = 0.
        CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
          EXPORTING
            iv_object  = ws_ybcs_trs-trkorr
            iv_type    = 'TASK'
            iv_command = 'CHAO'.
      ENDIF.
    ENDIF.
*       If more than 2 rows check, display an error message
  ELSE.
    MESSAGE i006(ytrm_msg) DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " F100_DO_ACTION_CHECK
*&---------------------------------------------------------------------*
*&      Form  F100_DO_ACTION_MERGE
*&---------------------------------------------------------------------*
*       Do the actions of the button : Merge
*----------------------------------------------------------------------*

FORM f100_do_action_merge .
*     If no row check, display an error message
  IF w_num_row_sel = 0.
    MESSAGE i007(ytrm_msg) DISPLAY LIKE 'E'.
  ELSEIF w_num_row_sel = '1'.
    MESSAGE i008(ytrm_msg) DISPLAY LIKE 'E'.
  ELSEIF w_num_row_sel = '2'.
*       Get the transport request number selected.
*        if wt_select_rows = '1'.
    CLEAR ws_rowid.
    CLEAR w_counter.
    LOOP AT wt_rowid INTO ws_rowid.
      ADD 1 TO w_counter.
      CLEAR ws_ybcs_trs.
      READ TABLE wt_ybcs_trs INTO ws_ybcs_trs
                             INDEX ws_rowid-row_id.

      IF sy-subrc = 0.
        IF ws_ybcs_trs-status_devl NE icon_light_out.
          MESSAGE i026(ytrm_msg).
          EXIT.
        ENDIF.
        IF w_counter = 1.
          w_ot1 = ws_ybcs_trs-trkorr.
        ELSE.
          w_ot2 = ws_ybcs_trs-trkorr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF w_ot1 IS NOT INITIAL AND w_ot2 IS NOT INITIAL.
      CLEAR w_answer.
      CALL FUNCTION 'POPUP_WITH_2_BUTTONS_TO_CHOOSE'
        EXPORTING
          diagnosetext1 = text-020
          diagnosetext2 = text-021
          diagnosetext3 = text-022
          textline1     = space
          text_option1  = w_ot1 "text on button
          text_option2  = w_ot2 "text on button
          titel         = text-025 "title
        IMPORTING
          answer        = w_answer.

      CASE w_answer.
        WHEN '1'.
          CLEAR ws_request_from.
          ws_request_from-h-trkorr = w_ot2.

          CLEAR ws_request_to.
          ws_request_to-h-trkorr = w_ot1.
        WHEN '2'.
          CLEAR ws_request_to.
          ws_request_to-h-trkorr = w_ot2.

          CLEAR ws_request_from.
          ws_request_from-h-trkorr = w_ot1.
        WHEN OTHERS.
      ENDCASE.

      CLEAR w_answer.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = text-032
          text_question         = text-023
          text_button_1         = text-024
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-031
          icon_button_2         = 'ICON_INCOMPLETE'
          display_cancel_button = space
          popup_type            = 'ICON_MESSAGE_QUESTION'
        IMPORTING
          answer                = w_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc = 0.
        CASE w_answer.
          WHEN '1'.
*         Get TR information
            CALL FUNCTION 'TRINT_READ_REQUEST'
              EXPORTING
                iv_read_e070       = 'X'
                iv_read_e07t       = 'X'
                iv_read_e070c      = 'X'
                iv_read_e070m      = 'X'
                iv_read_objs_keys  = 'X'
                iv_read_objs       = 'X'
                iv_read_attributes = 'X'
              CHANGING
                cs_request         = ws_request_from
              EXCEPTIONS
                error_occured      = 1
                OTHERS             = 2.
            IF sy-subrc <> 0.
              MESSAGE e015(ytrm_msg).
              EXIT.
            ENDIF.

            CALL FUNCTION 'TRINT_READ_REQUEST'
              EXPORTING
                iv_read_e070       = 'X'
                iv_read_e07t       = 'X'
                iv_read_e070c      = 'X'
                iv_read_e070m      = 'X'
                iv_read_objs_keys  = 'X'
                iv_read_objs       = 'X'
                iv_read_attributes = 'X'
              CHANGING
                cs_request         = ws_request_to
              EXCEPTIONS
                error_occured      = 1
                OTHERS             = 2.
            IF sy-subrc <> 0.
              MESSAGE e015(ytrm_msg).
              EXIT.
            ENDIF.

            CALL FUNCTION 'TRINT_MERGE_REQUESTS'
              EXPORTING
                is_request_from                = ws_request_from
                is_request_to                  = ws_request_to
              EXCEPTIONS
                check_error_from_request       = 1
                check_error_to_request         = 2
                check_error_fromto_consistency = 3
                db_access_error                = 4
                OTHERS                         = 5.
            IF sy-subrc <> 0.
* Implement suitable error handling here
              MESSAGE e015(ytrm_msg).
              EXIT.
            ELSE.
* Success for Merging process
              MESSAGE s016(ytrm_msg).
            ENDIF.


          WHEN '2'.
* Success message for cancelling merging.
            MESSAGE s015(ytrm_msg).
            EXIT.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDIF.
  ELSE.
* Select more than 2 rows
    MESSAGE i009(ytrm_msg) DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.                    " F100_DO_ACTION_MERGE
*&---------------------------------------------------------------------*
*&      Form  F100_DATA_SELECTION_TROA
*&---------------------------------------------------------------------*
*       Data Selection according to the row selected
*----------------------------------------------------------------------*
FORM f100_data_selection_troa .
  REFRESH : wt_tr_list.
  CLEAR :   ws_tr_list.

* For each selected line (contained in wt_rowid), read the table
* ALV (WT_YBCS_TRS) to get the Number of OT (TRKORR)
* Save Number of OT in internal table wt_ybcs_troa
  CLEAR : w_cust_sel, w_sel_crit_error_troa.

  CLEAR ws_rowid.
  LOOP AT wt_rowid INTO ws_rowid.
    CLEAR ws_ybcs_trs.
    READ TABLE wt_ybcs_trs INTO ws_ybcs_trs INDEX ws_rowid-row_id.
    IF sy-subrc = 0.
      IF ws_ybcs_trs-trfunction NE 'CUST'.

        CLEAR :   ws_tr_list.
        ws_tr_list-trkorr = ws_ybcs_trs-trkorr.
        ws_tr_list-trfunction = ws_ybcs_trs-trfunction.
        APPEND ws_tr_list TO wt_tr_list.
      ELSE.
        CLEAR w_cust_sel.
        w_cust_sel = 'X'.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF wt_tr_list[] IS INITIAL.
    w_sel_crit_error_troa = 'X'.
    MESSAGE i012(ytrm_msg) DISPLAY LIKE 'E'.
    EXIT.
  ELSE.
    IF w_cust_sel = 'X'.
      MESSAGE i011(ytrm_msg).
    ENDIF.

    SORT wt_tr_list BY trkorr.

* For each recovered version(s) we :
*   - Recovery of the description of OT (table e07t)
*   - Recovery of the transp statutes. FM TR_READ_GLOBAL_INFO_OF_REQUEST
*   - Fill the ALV Table(YBCS_TROA)
*   - Fill the fields with the data previously retrieved

* SR3 Get TR Description
    PERFORM f200_get_tr_description_troa.

    REFRESH wt_e071_novers.
    IF wt_novers_obj[] IS NOT INITIAL.
      SELECT trkorr
             as4pos
             pgmid
             object
             obj_name
             objfunc
             lockflag
             gennum
             lang
             activity
     FROM e071
     INTO TABLE wt_e071_novers
     FOR ALL ENTRIES IN wt_novers_obj
     WHERE ( pgmid = 'R3TR' OR pgmid = 'LIMU' )
       AND object = wt_novers_obj-obj_type
       AND obj_name = wt_novers_obj-obj_name.

      IF sy-subrc = 0.
        SORT wt_e071_novers BY trkorr object obj_name ASCENDING.

        IF wt_e071_novers[] IS NOT INITIAL.
          REFRESH wt_e070_novers.
* Selection of Transport request tasks
          SELECT trkorr
                 trfunction
                 as4date
                 as4time
                 strkorr
          FROM e070
          INTO TABLE wt_e070_novers
          FOR ALL ENTRIES IN wt_e071_novers
          WHERE trkorr = wt_e071_novers-trkorr
          AND strkorr = space.

          IF sy-subrc = 0.
            SORT wt_e070_novers BY trkorr.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F100_DATA_SELECTION_TROA
*&---------------------------------------------------------------------*
*&      Form  F200_GET_TR_DESCRIPTION_TROA
*&---------------------------------------------------------------------*
*       Perform to get Description Data and header data
*       for transport request object analysis
*----------------------------------------------------------------------*
FORM f200_get_tr_description_troa .

  REFRESH : wt_e07t_troa.
*  CLEAR : w_sel_crit_error_troa.

  SELECT trkorr
         as4text
         FROM e07t
         INTO TABLE wt_e07t_troa
         FOR ALL ENTRIES IN wt_tr_list
    WHERE trkorr = wt_tr_list-trkorr.
*    AND   langu = sy-langu.

*   if we have selected some data
  IF NOT wt_e07t_troa[] IS INITIAL.
    SORT wt_e07t_troa BY trkorr.
  ENDIF.

ENDFORM.                    " F200_GET_TR_DESCRIPTION_TROA
*&---------------------------------------------------------------------*
*&      Form  F300_GET_TR_STATUTES_TROA
*&---------------------------------------------------------------------*
*       Perform to get Transport Request Statutes
*       for transport request object analysis
*----------------------------------------------------------------------*
FORM f300_get_tr_statutes_troa USING fuw_trkorr TYPE char20.
  DATA : l_trkorr TYPE trkorr.
  CLEAR l_trkorr.
  l_trkorr = fuw_trkorr.
  CLEAR : ws_es_cofile.
  CALL FUNCTION 'TR_READ_GLOBAL_INFO_OF_REQUEST'
    EXPORTING
      iv_trkorr = l_trkorr
    IMPORTING
      es_cofile = ws_es_cofile.
ENDFORM.                    " F300_GET_TR_STATUTES_TROA
*&---------------------------------------------------------------------*
*&      Form  F200_GET_OBJECT_DESC_VERSION
*&---------------------------------------------------------------------*
*       Perform to get Object description and Object Version
*       for transport request object analysis
*----------------------------------------------------------------------*
FORM f200_get_object_desc_version .
  DATA : l_objname TYPE vrsd-objname,
         l_objtype TYPE  vrsd-objtype,
         l_tr_seq TYPE versno.



  READ TABLE wt_novers_obj WITH KEY obj_type = ws_object_list-obj_type
                                    obj_name = ws_object_list-obj_name
                           TRANSPORTING NO FIELDS.

  IF sy-subrc <> 0.


    CLEAR : l_objname, l_objtype.
    l_objname = ws_object_list-obj_name.
    l_objtype = ws_object_list-obj_type.

* SR2 Get Object Versions
    REFRESH : wt_version_list,
              wt_lversno_list.
* Retrieve all current object versions with using the function module
* with parameters (object type and name)
    CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
      EXPORTING
        objname                = l_objname
        objtype                = l_objtype
      TABLES
        lversno_list           = wt_lversno_list
        version_list           = wt_version_list
      EXCEPTIONS
        no_entry               = 1
        communication_failure_ = 2
        system_failure         = 3
        OTHERS                 = 4.
    IF sy-subrc = 0.
      CLEAR ws_version_list.
      DELETE wt_version_list WHERE korrnum IS INITIAL
                                OR versmode = 'I'.

      SORT wt_version_list BY datum zeit versno ASCENDING.

      CLEAR l_tr_seq.
      LOOP AT wt_version_list INTO ws_version_list.
* SR4 Get Transport Request Statutes
        PERFORM f300_get_tr_statutes_troa USING ws_version_list-korrnum.

*   ALV structure alimentation
        CLEAR ws_ybcs_troa.
*   Field 01 Object Name
        ws_ybcs_troa-obj_name = ws_object_list-obj_name.
*   Field 02 Object Description
        ws_ybcs_troa-obj_desc = w_obj_desc.
*   Field 03 Transport Request Version
*        IF ws_version_list-versno IS INITIAL.
*          ws_ybcs_troa-tr_seq = '99999'.
*        ELSE.
*          ws_ybcs_troa-tr_seq = ws_version_list-versno.
*        ENDIF.

        ADD 1 TO l_tr_seq.

        ws_ybcs_troa-tr_seq = l_tr_seq.

*      CLEAR ws_e07t.
*      READ TABLE wt_e07t_troa INTO ws_e07t
*           WITH KEY trkorr = ws_ybcs_troa-trkorr
*           BINARY SEARCH.
*      IF sy-subrc = 0.
**   Field 04 Transport Request Description
*        ws_ybcs_troa-as4text = ws_e07t-as4text.
*      ENDIF.



* FIELD 06 to 17 : Set TR log information (Status/Date/Time) for
* Developement (Release) / Quality,Pre prod,Prod (Import) systems
        IF w_date_limit <= ws_version_list-datum.
          PERFORM f200_fill_tr_status_info USING 'WS_YBCS_TROA'
                                                 ws_tr_list-trfunction
                                                 space.

* If Transport request is not yet released, set a light out in
* development status field
          IF ws_es_cofile-systems[] IS INITIAL.
            ws_ybcs_troa-status_devl = icon_light_out.
            ws_ybcs_troa-trkorr = ws_version_list-korrnum.
          ELSE.
            ws_ybcs_troa-trkorr = ws_version_list-korrnum.
          ENDIF.
        ELSE.
          ws_ybcs_troa-status_devl = icon_warning.
          ws_ybcs_troa-trkorr = ws_version_list-korrnum.
          ws_ybcs_troa-date_devl = ws_version_list-datum.
          ws_ybcs_troa-time_devl = ws_version_list-zeit.

          PERFORM f200_fill_tr_status_old USING ws_ybcs_troa
                                                'IE1'
                                                space.

          PERFORM f200_fill_tr_status_old USING ws_ybcs_troa
                                        'RE1'
                                        space.
          PERFORM f200_fill_tr_status_old USING ws_ybcs_troa
                                        'PE1'
                                        space.
        ENDIF.

        CLEAR ws_ybcs_troa-as4text.
        SELECT SINGLE as4text
                      FROM e07t
                      INTO ws_ybcs_troa-as4text
        WHERE trkorr =  ws_ybcs_troa-trkorr.
*          AND langu = sy-langu.

        IF sy-subrc <> 0.
          CLEAR ws_ybcs_troa-as4text.
        ENDIF.


        APPEND ws_ybcs_troa TO wt_ybcs_troa.
      ENDLOOP.
    ENDIF.
  ELSE.
    CLEAR ws_e071.
    CLEAR l_tr_seq.
    LOOP AT wt_e071_novers INTO ws_e071
                           WHERE object = ws_object_list-obj_type
                             AND obj_name = ws_object_list-obj_name.
      CLEAR ws_e070_novers.
      READ TABLE wt_e070_novers INTO ws_e070_novers
          WITH KEY trkorr = ws_e071-trkorr.

      IF sy-subrc = 0.
* SR4 Get Transport Request Statutes
        PERFORM f300_get_tr_statutes_troa USING ws_e071-trkorr.

*   ALV structure alimentation
        CLEAR ws_ybcs_troa.
*   Field 01 Object Name
        ws_ybcs_troa-obj_name = ws_object_list-obj_name.
*   Field 02 Object Description
        ws_ybcs_troa-obj_desc = w_obj_desc.
**   Field 03 Transport Request Version
*      IF ws_version_list-versno IS INITIAL.
*        ws_ybcs_troa-tr_seq = '99999'.
*      ELSE.
*        ws_ybcs_troa-tr_seq = ws_version_list-versno.
*      ENDIF.
        ADD 1 TO l_tr_seq.

        ws_ybcs_troa-tr_seq = l_tr_seq.

*      CLEAR ws_e07t.
*      READ TABLE wt_e07t_troa INTO ws_e07t
*           WITH KEY trkorr = ws_ybcs_troa-trkorr
*           BINARY SEARCH.
*      IF sy-subrc = 0.
**   Field 04 Transport Request Description
*        ws_ybcs_troa-as4text = ws_e07t-as4text.
*      ENDIF.



* FIELD 06 to 17 : Set TR log information (Status/Date/Time) for
* Developement (Release) / Quality,Pre prod,Prod (Import) systems
        IF w_date_limit <= ws_e070_novers-as4date.
          PERFORM f200_fill_tr_status_info USING 'WS_YBCS_TROA'
                                                 ws_tr_list-trfunction
                                                 space.

* If Transport request is not yet released, set a light out in
* development status field
          IF ws_es_cofile-systems[] IS INITIAL.
            ws_ybcs_troa-status_devl = icon_light_out.
            ws_ybcs_troa-trkorr = ws_e071-trkorr.
          ELSE.
            ws_ybcs_troa-trkorr = ws_e071-trkorr.
          ENDIF.
        ELSE.
          ws_ybcs_troa-status_devl = icon_warning.
          ws_ybcs_troa-trkorr = ws_e071-trkorr.
          ws_ybcs_troa-date_devl = ws_e070_novers-as4date.
          ws_ybcs_troa-time_devl = ws_e070_novers-as4time.


        ENDIF.
        CLEAR ws_ybcs_troa-as4text.
        SELECT SINGLE as4text
                      FROM e07t
                      INTO ws_ybcs_troa-as4text
        WHERE trkorr =  ws_ybcs_troa-trkorr.
*          AND langu = sy-langu.

        IF sy-subrc <> 0.
          CLEAR ws_ybcs_troa-as4text.
        ENDIF.

        APPEND ws_ybcs_troa TO wt_ybcs_troa.
      ENDIF.
    ENDLOOP.
  ENDIF.

  SORT wt_ybcs_troa BY obj_name ASCENDING tr_seq DESCENDING.
ENDFORM.                    " F200_GET_OBJECT_DESC_VERSION
*&---------------------------------------------------------------------*
*&      Form  F100_FILL_ALV_STRUCTURE_TROA
*&---------------------------------------------------------------------*
*       Fill the alv stucture for transport request object analysis
*----------------------------------------------------------------------*

FORM f100_fill_alv_structure_troa .
* For each OT selected, loop on the table (WT_OBJECT_LIST)
* For each object :
* - Recover the description of the object FM REPOSITORY_INFO_SYSTEM_TEXT
* - Retrieve the versions on the object FM SVRS_GET_VERSION_DIRECTORY_4
  CLEAR ws_tr_list.
  LOOP AT wt_tr_list INTO ws_tr_list.

    CLEAR : ws_object_list.

    LOOP AT wt_object_list INTO ws_object_list
                           WHERE trkorr = ws_tr_list-trkorr.


      IF ws_object_list-obj_name IN wt_obj_analyzed[]
      AND wt_obj_analyzed[] IS NOT INITIAL.
        CONTINUE.
      ENDIF.

* SR1 Get object description
      IF ws_object_list-obj_delete IS INITIAL.
*        PERFORM f200_get_object_description.
      ENDIF.

      PERFORM f200_get_object_desc_version.

      CLEAR ws_obj_analyzed.
      ws_obj_analyzed-sign = 'I'.
      ws_obj_analyzed-option = 'EQ'.
      ws_obj_analyzed-low = ws_object_list-obj_name.
      APPEND ws_obj_analyzed TO wt_obj_analyzed.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " F100_FILL_ALV_STRUCTURE_TROA
*&---------------------------------------------------------------------*
*&      Form  F200_GET_OBJECT_DESCRIPTION
*&---------------------------------------------------------------------*
*       Get object Description for Transport Request Object Analysis
*----------------------------------------------------------------------*
FORM f200_get_object_description .

  DATA : l_obj_type TYPE euobj-id.

  CLEAR l_obj_type.
  l_obj_type = ws_object_list-obj_type.

  REFRESH wt_object_desc.
  REFRESH wt_devclass.
* Allow to retrieve the object description using the function module
* with current object type and name
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_TEXT'
    EXPORTING
      object_type = l_obj_type
      object_name = ws_object_list-obj_name
      mode        = space
    TABLES
      objectlist  = wt_object_desc
      devclass    = wt_devclass
    EXCEPTIONS
      wrong_type  = 1
      OTHERS      = 2.

  IF sy-subrc = 0.
    READ TABLE wt_object_desc INTO ws_object_desc INDEX 1.
    IF sy-subrc = 0.
      CLEAR w_obj_desc.
      w_obj_desc = ws_object_desc-obj_text.
    ENDIF.

  ENDIF.
ENDFORM.                    " F200_GET_OBJECT_DESCRIPTION
*&---------------------------------------------------------------------*
*&      Form  F100_SIMPLE_CLIC
*&---------------------------------------------------------------------*
*       Some fields have the propriety hotspot so here we handle
*       the clic on this fields (se01 for status) and commentary text
*----------------------------------------------------------------------*

FORM f100_simple_clic  USING    fuws_row_id TYPE  lvc_s_row
                                fuws_column_id TYPE  lvc_s_col
                                fuo_sender TYPE REF TO cl_gui_alv_grid.

  DATA : ls_row    TYPE lvc_s_row,
         ls_column TYPE lvc_s_col,
         l_fieldname TYPE string,
         l_name TYPE string,
         l_text_name TYPE thead-tdname,
         l_action_process TYPE flag.

  FIELD-SYMBOLS : <l_fieldvalue> TYPE any.

  CLEAR l_action_process.
  CLEAR : ls_row, ls_column.
  ls_row    = fuws_row_id.
  ls_column = fuws_column_id.

  UNASSIGN <l_fieldvalue>.
  CLEAR l_fieldname.

  CLEAR l_name.
  CALL METHOD fuo_sender->get_name
    RECEIVING
      name = l_name.

* When the user click on a document number, the transaction SE01 is
* called to show the document
  IF ls_column-fieldname = 'TRKORR'.
    IF l_name NP 'TROA*'.

      CLEAR ws_ybcs_trs.
      READ TABLE wt_ybcs_trs
            INTO ws_ybcs_trs
           INDEX ls_row-index.
      IF sy-subrc = 0.
        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr = ws_ybcs_trs-trkorr.

        l_action_process = 'X'.
      ENDIF.
    ELSE.

      CLEAR ws_ybcs_troa.
      READ TABLE wt_ybcs_troa
            INTO ws_ybcs_troa
           INDEX ls_row-index.
      IF sy-subrc = 0.
        CALL FUNCTION 'TR_PRESENT_REQUEST'
          EXPORTING
            iv_trkorr = ws_ybcs_troa-trkorr.
        l_action_process = 'X'.
      ENDIF.
    ENDIF.
  ELSEIF ls_column-fieldname CP 'STATUS*'.
    IF l_name NP 'TROA*'.
      CLEAR ws_ybcs_trs.
      READ TABLE wt_ybcs_trs
            INTO ws_ybcs_trs
           INDEX ls_row-index.

      IF sy-subrc = 0.
        CONCATENATE 'WS_YBCS_TRS-' ls_column-fieldname INTO l_fieldname.
        IF <l_fieldvalue> IS ASSIGNED.
          UNASSIGN <l_fieldvalue>.
        ENDIF.
        ASSIGN (l_fieldname) TO <l_fieldvalue>.
        IF sy-subrc = 0 AND <l_fieldvalue> IS NOT INITIAL.
          CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
            EXPORTING
              iv_object  = ws_ybcs_trs-trkorr
              iv_type    = 'TASK'
              iv_command = 'TAST'.
          l_action_process = 'X'.
        ENDIF.

      ENDIF.
    ELSE.
      CLEAR ws_ybcs_troa.
      READ TABLE wt_ybcs_troa
            INTO ws_ybcs_troa
           INDEX ls_row-index.

      IF sy-subrc = 0.
        CONCATENATE 'WS_YBCS_TROA-' ls_column-fieldname
                    INTO l_fieldname.
        IF <l_fieldvalue> IS ASSIGNED.
          UNASSIGN <l_fieldvalue>.
        ENDIF.
        ASSIGN (l_fieldname) TO <l_fieldvalue>.
        IF sy-subrc = 0 AND <l_fieldvalue> IS NOT INITIAL.
* Click on the trtext
          CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
            EXPORTING
              iv_object  = ws_ybcs_troa-trkorr
              iv_type    = 'TASK'
              iv_command = 'TAST'.
          l_action_process = 'X'.

        ENDIF.
      ENDIF.
    ENDIF.
* Handle the clic on commentary text, we read and we can edit.
  ELSEIF ls_column-fieldname = 'TR_TEXT'.
    CLEAR ws_ybcs_trs.
    READ TABLE wt_ybcs_trs
          INTO ws_ybcs_trs
         INDEX ls_row-index.

    IF sy-subrc = 0.
      CLEAR ws_text_header.
      REFRESH wt_tline.
      CLEAR l_text_name.
      l_text_name = ws_ybcs_trs-trkorr.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
*         CLIENT                  = SY-MANDT
          id                      = 'YTRM'
          language                = sy-langu
          name                    = l_text_name
          object                  = 'YTRM'
*         ARCHIVE_HANDLE          = 0
*         LOCAL_CAT               = ' '
        IMPORTING
          header                  = ws_text_header
        TABLES
          lines                   = wt_tline
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
      IF sy-subrc = 0.
        CALL FUNCTION 'EDIT_TEXT'
          EXPORTING
            header        = ws_text_header
          TABLES
            lines         = wt_tline
          EXCEPTIONS
            id            = 1
            language      = 2
            linesize      = 3
            name          = 4
            object        = 5
            textformat    = 6
            communication = 7
            OTHERS        = 8.
        IF sy-subrc = 0.
          MESSAGE s014(ytrm_msg) WITH ws_ybcs_trs-trkorr.
          l_action_process = 'X'.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

  IF l_action_process = 'X'.
* Back to the ALV : refreshment of the display
    CALL METHOD o_alv_trs->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ENDIF.
ENDFORM.                    " F100_SIMPLE_CLIC
*&---------------------------------------------------------------------*
*&      Form  F100_DO_ACTION_ADDTEXT
*&---------------------------------------------------------------------*
*       We add commentary text which can be modify
*----------------------------------------------------------------------*

FORM f100_do_action_addtext .
  DATA : l_name TYPE thead-tdname.
* No row selected
  IF w_num_row_sel = 0.
    MESSAGE i005(ytrm_msg) DISPLAY LIKE 'E'.
  ELSEIF w_num_row_sel = 1.
    CLEAR ws_rowid.
* We read the row selected
    READ TABLE wt_rowid INTO ws_rowid INDEX 1.
    IF sy-subrc = 0.
      CLEAR ws_ybcs_trs.
* We read the data for the tr owner (line) selected of the first ALV
      READ TABLE wt_ybcs_trs INTO ws_ybcs_trs INDEX ws_rowid-row_id.
      IF sy-subrc = 0.
        CLEAR ws_text_header.
        REFRESH wt_tline.
        CLEAR l_name.
        l_name = ws_ybcs_trs-trkorr.
* We read the text with a function module and we have this text
* editable in order to modify it.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'YTRM'
            language                = sy-langu
            name                    = l_name
            object                  = 'YTRM'
          IMPORTING
            header                  = ws_text_header
          TABLES
            lines                   = wt_tline
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc = 0.
          CALL FUNCTION 'EDIT_TEXT'
            EXPORTING
              header        = ws_text_header
            TABLES
              lines         = wt_tline
            EXCEPTIONS
              id            = 1
              language      = 2
              linesize      = 3
              name          = 4
              object        = 5
              textformat    = 6
              communication = 7
              OTHERS        = 8.

          IF sy-subrc = 0.
* Message with confirmation of add commentary with the number of OT
            MESSAGE s014(ytrm_msg) WITH ws_ybcs_trs-trkorr.
          ENDIF.
        ELSE.
          CLEAR ws_text_header.
          ws_text_header-tdobject = 'YTRM'.
          ws_text_header-tdname = l_name.
          ws_text_header-tdid = 'YTRM'.
          ws_text_header-tdspras = sy-langu.
          CLEAR ws_tline.
          ws_tline-tdline = text-c01.
          APPEND ws_tline TO wt_tline.
          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              header          = ws_text_header
              insert          = 'X'
              savemode_direct = 'X'
            IMPORTING
              newheader       = ws_text_header
            TABLES
              lines           = wt_tline
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.
          IF sy-subrc = 0.
            ws_text_header-tdlinesize = 32.
            CALL FUNCTION 'EDIT_TEXT'
              EXPORTING
                header        = ws_text_header
              TABLES
                lines         = wt_tline
              EXCEPTIONS
                id            = 1
                language      = 2
                linesize      = 3
                name          = 4
                object        = 5
                textformat    = 6
                communication = 7
                OTHERS        = 8.

            IF sy-subrc = 0.
* Message with confirmation of add commentary with the number of OT
              MESSAGE s014(ytrm_msg) WITH ws_ybcs_trs-trkorr.
              ws_ybcs_trs-tr_text = icon_display_text.
              MODIFY wt_ybcs_trs FROM ws_ybcs_trs INDEX ws_rowid-row_id.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
* Back to the ALV : refreshment of the display
    CALL METHOD o_alv_trs->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ELSEIF w_num_row_sel > 1.
    MESSAGE i006(ytrm_msg) DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.                    " F100_DO_ACTION_ADDTEXT
*&---------------------------------------------------------------------*
*&      Form  F100_DO_ACTION_SEQUENCE
*&---------------------------------------------------------------------*
*       We sort the differents OT in order to have these OTs in the
*       order with we will make them released.
*----------------------------------------------------------------------*
FORM f100_do_action_sequence .
*  IF w_num_row_sel = 0.
*    MESSAGE i005(ytrm_msg) DISPLAY LIKE 'E'.
*  ELSE.
* We sort the differents OT in order to have this OT in the order
* with we will make them released.
**************
  DATA : lr_syslist TYPE TABLE OF rsrange,
         ls_syslist TYPE rsrange,
         l_answer TYPE c,
         lt_spopli TYPE TABLE OF spopli,
         ls_spopli TYPE spopli.

  REFRESH lt_spopli.
  CLEAR l_answer.
  REFRESH lr_syslist.
  PERFORM f000_get_constants_data USING 'CGI_TRM'
                                        'SYSLIST'
                                  CHANGING lr_syslist.

  CLEAR ls_syslist.
  LOOP AT lr_syslist INTO ls_syslist.
    CLEAR ls_spopli.
    ls_spopli-varoption = ls_syslist-low.
    APPEND ls_spopli TO lt_spopli.
  ENDLOOP.

  CLEAR ls_spopli.
  ls_spopli-varoption = text-t12.
  APPEND ls_spopli TO lt_spopli.

*************
  CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
    EXPORTING
      start_col          = 5
      start_row          = 5
      textline1          = text-p05
      textline2          = text-p06
      titel              = text-p08
    IMPORTING
*  qual , inte , prod : environment where we want realize
* our deployement
      answer             = l_answer
    TABLES
      t_spopli           = lt_spopli
    EXCEPTIONS
      not_enough_answers = 1
      too_much_answers   = 2
      too_much_marks     = 3
      OTHERS             = 4.

  IF sy-subrc = 0 AND l_answer NE 'A'.
    CLEAR w_system.
    CLEAR ls_spopli.
    READ TABLE lt_spopli INTO ls_spopli INDEX l_answer.
    IF sy-subrc = 0.
      w_system = ls_spopli-varoption.
    ENDIF.

    CLEAR w_alias.
    CASE l_answer.
*     Release to Quality system
      WHEN 1.
*       Check if at least on TR can be released in Quality system.
        LOOP AT wt_ybcs_trs INTO ws_ybcs_trs WHERE sort_devl = 'A'.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          SORT wt_ybcs_trs BY sort_devl sort_qual sort_inte
                              date_devl time_devl date_qual
                              time_qual date_inte time_inte
                              sequence_order sequence_order_high.
          w_alias = 'QUAL'.
          MESSAGE s028(ytrm_msg) WITH w_system.
        ELSE.
          MESSAGE i027(ytrm_msg) DISPLAY LIKE 'E' WITH w_system.
          MESSAGE e031(ytrm_msg) WITH w_system.
        ENDIF.
*     Release to Integration system
      WHEN 2.
*       Check if at least on TR can be released in Quality system.
        LOOP AT wt_ybcs_trs INTO ws_ybcs_trs WHERE sort_qual = 'A'.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          SORT wt_ybcs_trs BY sort_qual sort_devl sort_inte
                              date_qual time_qual date_devl
                              time_devl date_inte time_inte
                              sequence_order sequence_order_high.
          w_alias = 'INTE'.
          MESSAGE s028(ytrm_msg) WITH w_system.
        ELSE.
          MESSAGE i027(ytrm_msg) DISPLAY LIKE 'E' WITH w_system.
          MESSAGE e031(ytrm_msg) WITH w_system.
        ENDIF.
*     Release to Production system
      WHEN 3.
*       Check if at least on TR can be released in Quality system.
        LOOP AT wt_ybcs_trs INTO ws_ybcs_trs WHERE sort_inte = 'A'.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          SORT wt_ybcs_trs BY sort_inte sort_qual sort_devl
                              date_inte time_inte date_qual
                              time_qual date_devl time_devl
                              sequence_order sequence_order_high.
          w_alias = 'PROD'.
          MESSAGE s028(ytrm_msg) WITH w_system.
        ELSE.
          MESSAGE i027(ytrm_msg) DISPLAY LIKE 'E' WITH w_system.
          MESSAGE e031(ytrm_msg) WITH w_system.
        ENDIF.
      WHEN 4.
        MESSAGE s034(ytrm_msg).
      WHEN OTHERS.

    ENDCASE.
*    MESSAGE s028(ytrm_msg) WITH w_system.

  ENDIF.

* Back to the ALV : refreshment of the display
  CALL METHOD o_alv_trs->refresh_table_display
    EXCEPTIONS
      finished = 1
      OTHERS   = 2.
*  ENDIF.
ENDFORM.                    " F100_DO_ACTION_SEQUENCE

*&---------------------------------------------------------------------*
*&      Form  f100_do_export_excel
*&---------------------------------------------------------------------*
*       Here we call methods for realize our export and we display a
*       popup in order to define of the name of excel file and his
*       location.
*----------------------------------------------------------------------*
FORM f100_do_export_excel.
  DATA : l_window_title TYPE string.
  CLEAR : w_filename,
          w_path,
          w_fullpath.

  CLEAR l_window_title.
  l_window_title = text-033.

*  Display a pop-up to save the excel file with automatically xls type
  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title         = l_window_title
      default_extension    = 'XLS'
      default_file_name    = '*.xls'
*     with_encoding        =
      file_filter          = 'Excel Files (*.XLS;*.XLSX;*.XLSM)|*.XLS;*.XLSX;*.XLSM|'
*     initial_directory    =
*     prompt_on_overwrite  = 'X'
    CHANGING
      filename             = w_filename
      path                 = w_path
      fullpath             = w_fullpath
*     user_action          =
*     file_encoding        =
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc = 0 AND w_fullpath IS NOT INITIAL.
* We create our excel file
    PERFORM f200_create_excel_file.
*    SORT wt_ybcs_trs BY status_devl sequence_order.
    PERFORM f100_do_action_sequence.
* Excel Sheet 1 :ALV TRS data
    CLEAR w_snct.
    ADD 1 TO w_snct.
    REFRESH wt_fieldlist.
    wt_fieldlist[] = wt_ybct_xls_custom[].
    DELETE wt_fieldlist WHERE tabname NE 'TRS_ALV'.
    PERFORM f200_create_excel_sheets USING text-x01
                                           wt_fieldlist
                                           text-x03
                                           w_snct.

* Excel Sheet 2 : TR follow up data
    ADD 1 TO w_snct.
    REFRESH wt_fieldlist.
    wt_fieldlist[] = wt_ybct_xls_custom[].
    DELETE wt_fieldlist WHERE tabname = 'TRS_ALV'.
    PERFORM f200_create_excel_sheets USING text-x02
                                           wt_fieldlist
                                           text-x04
                                           w_snct.
* We save our excel file hard mode
    PERFORM f200_save_excel_file.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    MESSAGE i019(ytrm_msg) DISPLAY LIKE 'I'.
  ENDIF.

ENDFORM.                    " F100_DO_EXPORT_EXCEL


*&---------------------------------------------------------------------

*&      Form  F200_CREATE_EXCEL_FILE
*&---------------------------------------------------------------------
*       We create our excel file and we defile some properties
*      (workbook, number of sheets, background or foreground)
*----------------------------------------------------------------------*
FORM f200_create_excel_file .
  FREE o_excel.
  CREATE OBJECT o_excel 'Excel.Application'.
  IF sy-subrc NE 0.
    MESSAGE i017(ytrm_msg) DISPLAY LIKE 'E'.
  ENDIF.

* Skip messages
  SET PROPERTY OF o_excel    'DisplayAlerts' = 0.
* Create a new excel file
  CALL METHOD OF
      o_excel
      'WORKBOOKS' = o_workbook.
* Build the file in background (0) or foreground (1)
  SET PROPERTY OF o_excel    'VISIBLE' = 0.
* Number of sheets in the file
  SET PROPERTY OF o_excel    'SheetsInNewWorkbook' = 2.
* Functionality
  CALL METHOD OF
      o_workbook
      'ADD'.

ENDFORM.                    " F200_CREATE_EXCEL_FILE
*&---------------------------------------------------------------------

*&      Form  F200_CREATE_EXCEL_SHEETS
*&---------------------------------------------------------------------
*       We define the 2 sheets (name, title, content, color, form)
*----------------------------------------------------------------------*
FORM f200_create_excel_sheets USING
                                  fuw_sheetname TYPE trnl_tx132
                                  fuwt_fieldlist TYPE ybct_xls_custom_tt
                                  fuw_sheettitle TYPE trnl_tx132
                                  fuw_sheetnumber TYPE i.
  DATA : l_text(250)  TYPE c.
  DATA : l_index      TYPE sy-tabix,
         l_fieldname TYPE string.

* Sheet properties
  CALL METHOD OF
      o_excel
      'WORKSHEETS' = o_sheet
    EXPORTING
      #1           = fuw_sheetnumber.

  CALL METHOD OF
      o_sheet
      'ACTIVATE'.
  SET PROPERTY OF o_sheet 'NAME' = fuw_sheetname.
  FREE OBJECT o_sheet.

* Sheet Title
  CLEAR l_index.
  MOVE 1 TO l_index.
  CALL METHOD OF
      o_excel
      'CELLS' = o_cell
    EXPORTING
      #1      = l_index
      #2      = 5.

  PERFORM f300_font USING 3 20 3. "color font

  SET PROPERTY OF o_cell 'VALUE' = fuw_sheettitle.
  FREE OBJECT o_cell.

* Set Target system information for release
  IF fuw_sheetnumber = 2.
    CLEAR l_index.
    MOVE 3 TO l_index.
    CALL METHOD OF
        o_excel
        'CELLS' = o_cell
      EXPORTING
        #1      = l_index
        #2      = 1.

    PERFORM f300_font USING 3 14 1. "color font

    SET PROPERTY OF o_cell 'VALUE' = text-v08.
    FREE OBJECT o_cell.

    CALL METHOD OF
        o_excel
        'CELLS' = o_cell
      EXPORTING
        #1      = l_index
        #2      = 2.

    PERFORM f300_font USING 3 14 1. "color font

    SET PROPERTY OF o_cell 'VALUE' = w_system.
    FREE OBJECT o_cell.
  ENDIF.
* Columns titles line/values
  CLEAR ws_ybcs_trs.
  CLEAR : w_col_indx, w_line_indx.

  w_col_indx = 2.
  w_line_indx = 6.
  CLEAR w_sequence.
  w_sequence = 1.
* We loop on all ALV TRS data in order to generate XLS file
  LOOP AT wt_ybcs_trs INTO ws_ybcs_trs.
    AT FIRST.
*     Columns Header
      CLEAR ws_ybct_xls_custom.
      LOOP AT fuwt_fieldlist INTO ws_ybct_xls_custom.

        CALL METHOD OF
            o_excel
            'CELLS' = o_cell
          EXPORTING
            #1      = w_line_indx
            #2      = w_col_indx.

        CALL METHOD OF
            o_cell
            'BORDERS' = o_borders.
        SET PROPERTY OF o_borders 'LineStyle' = 1.
        SET PROPERTY OF o_borders 'Weight'    = 2.
        FREE OBJECT o_borders.

        SET PROPERTY OF o_cell 'HorizontalAlignment' = -4108.

        CLEAR w_column_desc.
        IF ws_ybct_xls_custom-field_exit IS NOT INITIAL.
          PERFORM (ws_ybct_xls_custom-field_exit)
                           IN PROGRAM ybcrp_cgi_trm
                                USING 'X'
                                      ws_ybct_xls_custom-fieldname
                             CHANGING w_column_desc
                             IF FOUND.
        ELSE.
          CLEAR ws_fieldcat.
          READ TABLE wt_trs_fieldcat INTO ws_fieldcat
                     WITH KEY fieldname = ws_ybct_xls_custom-fieldname.
          IF sy-subrc = 0.
            w_column_desc = ws_fieldcat-scrtext_l.
          ENDIF.
        ENDIF.



        SET PROPERTY OF o_cell 'VALUE' = w_column_desc.
        PERFORM f300_font USING 1 12 5.
        FREE OBJECT o_cell.

        ADD 1 TO w_col_indx.
      ENDLOOP.
    ENDAT.

* Column values
    CLEAR w_col_indx.
    w_col_indx = 2.
    ADD 1 TO w_line_indx.

    CLEAR ws_ybct_xls_custom.

    LOOP AT fuwt_fieldlist INTO ws_ybct_xls_custom.
      CLEAR w_column_value.
      IF <w_fieldvalue> IS ASSIGNED.
        UNASSIGN <w_fieldvalue>.
      ENDIF.
      IF ws_ybct_xls_custom-field_exit IS NOT INITIAL.
        CLEAR l_fieldname.
        CONCATENATE 'WS_YBCS_TRS-' ws_ybct_xls_custom-fieldname
        INTO l_fieldname.

        ASSIGN (l_fieldname) TO <w_fieldvalue>.
        IF sy-subrc = 0.
          w_column_value = <w_fieldvalue>.
        ENDIF.

        PERFORM (ws_ybct_xls_custom-field_exit)
                            IN PROGRAM ybcrp_cgi_trm
                                 USING space
                                       ws_ybct_xls_custom-fieldname
                              CHANGING w_column_value
                              IF FOUND.
        IF <w_fieldvalue> IS ASSIGNED.
          UNASSIGN <w_fieldvalue>.
        ENDIF.
        ASSIGN w_column_value TO <w_fieldvalue>.

      ELSE.

        CLEAR l_fieldname.
        CONCATENATE 'WS_YBCS_TRS-' ws_ybct_xls_custom-fieldname
        INTO l_fieldname.

        ASSIGN (l_fieldname) TO <w_fieldvalue>.

      ENDIF.

      CALL METHOD OF
          o_excel
          'CELLS' = o_cell
        EXPORTING
          #1      = w_line_indx
          #2      = w_col_indx.

      CALL METHOD OF
          o_cell
          'BORDERS' = o_borders.
      SET PROPERTY OF o_borders 'LineStyle' = 1.
      SET PROPERTY OF o_borders 'Weight'    = 2.
      FREE OBJECT o_borders.

      SET PROPERTY OF o_cell 'HorizontalAlignment' = -4108.

      IF <w_fieldvalue> IS ASSIGNED.
        SET PROPERTY OF o_cell 'VALUE' = <w_fieldvalue>.
      ENDIF.

      PERFORM f300_font USING 0 11 1.
      FREE OBJECT o_cell.

      ADD 1 TO w_col_indx.

    ENDLOOP.
    ADD 1 TO w_sequence.
  ENDLOOP.

* Automatic adjustment of the columns width
  CALL METHOD OF
      o_excel
      'COLUMNS' = o_column.
  CALL METHOD OF
      o_column
      'AUTOFIT'.
  FREE OBJECT o_column.

ENDFORM.                    " F200_CREATE_EXCEL_SHEETS
*&---------------------------------------------------------------------

*&      Form  F200_SAVE_EXCEL_FILE
*&---------------------------------------------------------------------
*       Perform to save in hard mode our excel file.
*----------------------------------------------------------------------*
FORM f200_save_excel_file .

  GET PROPERTY OF o_excel 'ActiveSheet' = o_sheet.
  FREE OBJECT o_sheet.
  FREE OBJECT o_workbook.

  GET PROPERTY OF o_excel 'ActiveWorkbook' = o_workbook.

  CALL METHOD OF
      o_workbook
      'SAVEAS'

    EXPORTING
      #1         = w_fullpath
      #2         = 1.
  CALL METHOD OF
      o_workbook
      'CLOSE'.
  CALL METHOD OF
      o_excel
      'QUIT'.

  FREE OBJECT o_sheet.
  FREE OBJECT o_workbook.
  FREE OBJECT o_excel.

  MESSAGE i018(ytrm_msg) WITH text-039 "the file
                               w_fullpath
                              text-040 "has been created
                               space.
ENDFORM.                    " F200_SAVE_EXCEL_FILE
*&---------------------------------------------------------------------*
*&      Form  F300_FONT
*&---------------------------------------------------------------------*
*       We handle bold size color
*----------------------------------------------------------------------*
FORM f300_font USING bold size color.

  CONSTANTS : k_arial(5) TYPE c VALUE 'Arial'.

  CALL METHOD OF
      o_cell
      'FONT' = o_font.
  SET PROPERTY OF o_font 'BOLD' = bold.
  SET PROPERTY OF o_font 'SIZE' = size.
  SET PROPERTY OF o_font 'ColorIndex' = color.
  SET PROPERTY OF o_font 'family'     = k_arial.
  FREE OBJECT o_font.

* Colors
*1  "Black
*53 "Brown
*52 "Olive Green
*51 "Dark Green
*49 "Dark Teal
*11 "Dark Blue
*55 "Indigo
*56 "Gray 80%
*9  "Dark Red
*46 "Orange
*12 "Dark Yellow
*10 "Green
*14 "Teal
*5  "Blue
*47 "Blue Gray
*16 "Gray 50%
*3  "Red
*45 "Light Orange
*43 "Lime
*50 "Sea Green
*42 "Aqua
*41 "Light Blue
*13 "Violet
*48 "Gray 40%
*7  "Pink
*44 "Gold
*6  "Yellow
*4  "Bright Green
*8  "Turquoise
*33 "Sky Blue
*54 "Plum
*15 "Gray 25%
*38 "Rose
*40 "Tan
*36 "Light Yellow
*35 "Light Green
*34 "Light Turquoise
*37 "Pale Blue
*39 "Lavender
*2  "White
ENDFORM.                                                    " F300_FONT
*&---------------------------------------------------------------------*
*&      Form  F300_FONT_CELL
*&---------------------------------------------------------------------*
*       Handle the font of a cell
*----------------------------------------------------------------------*
FORM f300_font_cell USING color pattern.

  CALL METHOD OF
      o_range
      'INTERIOR' = o_int.
  SET PROPERTY OF o_int  'ColorIndex' = color.
  SET PROPERTY OF o_int  'Pattern'    = pattern.
  FREE OBJECT o_int.

  CALL METHOD OF
      o_range
      'BORDERS' = o_borders.
  SET PROPERTY OF o_borders 'LineStyle' = 1.
  SET PROPERTY OF o_borders 'Weight'    = 2.
  FREE OBJECT o_borders.

  SET PROPERTY OF o_range 'HorizontalAlignment' = -4108.

ENDFORM.                    " F300_FONT_CELL
*&---------------------------------------------------------------------*
*&      Form  F200_FILL_BACKCOL
*&---------------------------------------------------------------------*
*       We handle the back color for the diffrent status of release OT
*----------------------------------------------------------------------*
FORM f200_fill_backcol  USING    fuw_color
                                 .

  CALL METHOD OF
      o_excel
      'CELLS' = o_cell
    EXPORTING
      #1      = w_line_indx
      #2      = w_col_indx.
  CALL METHOD OF
      o_cell
      'INTERIOR' = o_interior.
  SET PROPERTY OF o_interior 'COLORINDEX' = fuw_color.

ENDFORM.                    " F200_FILL_BACKCOL

*&---------------------------------------------------------------------*
*&      Form  F100_DISPLAY_HELP
*&---------------------------------------------------------------------*
*       We handle the displaying of Help on clicking on the button
*       'Application Help"
*----------------------------------------------------------------------*
FORM f100_display_help.

  DATA : lt_links TYPE TABLE OF tline.

  REFRESH lt_links.
  CALL FUNCTION 'HELP_OBJECT_SHOW'
    EXPORTING
      dokclass          = 'TX'
      doklangu          = sy-langu
      dokname           = 'Y_CGI_TRM_APPLICATION_HELP'
      doktitle          = 'Docu Matthieu'
      called_by_program = sy-cprog
      called_by_dynp    = sy-dynnr
      called_by_cuaprog = sy-cprog
      called_by_cuastat = sy-pfkey
    TABLES
      links             = lt_links
    EXCEPTIONS
      object_not_found  = 1
      sapscript_error   = 2
      OTHERS            = 3.
  IF sy-subrc <> 0.
    MESSAGE e020(ytrm_msg).
  ENDIF.




ENDFORM.                    " F100_DISPLAY_HELP
*&---------------------------------------------------------------------*
*&      Form  F100_DO_CHANGE_OWNER
*&---------------------------------------------------------------------*
*       Perform to replace the owner of an OT by the current owner
*----------------------------------------------------------------------*

FORM f100_do_change_owner .
  DATA : l_tr_ko TYPE flag,
         l_tr_ok TYPE flag,
         l_question TYPE string.

  CLEAR l_question.
  CONCATENATE text-042 text-043 INTO l_question SEPARATED BY space.

  CLEAR : l_tr_ko, l_tr_ok.
  IF w_num_row_sel = 0.
    MESSAGE i025(ytrm_msg) DISPLAY LIKE 'E'.
  ELSE.
* Confirm your decision
    CLEAR w_answer.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = text-041
        text_question         = l_question
        text_button_1         = text-024
        icon_button_1         = 'ICON_CHECKED'
        text_button_2         = text-031
        icon_button_2         = 'ICON_INCOMPLETE'
        display_cancel_button = space
        popup_type            = 'ICON_MESSAGE_QUESTION'
      IMPORTING
        answer                = w_answer
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    IF sy-subrc = 0 AND w_answer = 1.
      CLEAR ws_rowid.
* We loop into the row selected
      LOOP AT wt_rowid INTO ws_rowid.
        CLEAR ws_ybcs_trs.
* We read the data for the tr owner (line) selected of the first ALV
        READ TABLE wt_ybcs_trs INTO ws_ybcs_trs INDEX ws_rowid-row_id.
        IF sy-subrc = 0 AND ws_ybcs_trs-as4user <> sy-uname
          AND ws_ybcs_trs-status_devl = icon_light_out.
          CALL FUNCTION 'TR_CHANGE_USERNAME'
            EXPORTING
              wi_dialog           = 'X'
              wi_user             = sy-uname
              wi_trkorr           = ws_ybcs_trs-trkorr
            EXCEPTIONS
              already_released    = 1
              e070_update_error   = 2
              file_access_error   = 3
              not_exist_e070      = 4
              user_does_not_exist = 5
              tr_enqueue_failed   = 6
              no_authorization    = 7
              wrong_client        = 8
              unallowed_user      = 9
              OTHERS              = 10.
          IF sy-subrc <> 0.
            CLEAR l_tr_ko.
            l_tr_ko = 'X'.
          ELSE.
            CLEAR l_tr_ok.
            l_tr_ok = 'X'.
          ENDIF.
        ENDIF.
      ENDLOOP.

*   All tr updated successfully
      IF l_tr_ok = 'X' AND l_tr_ko IS INITIAL.
        MESSAGE s021(ytrm_msg).
        CALL METHOD o_alv_trs->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
*   TR update with errors
      ELSEIF l_tr_ok = 'X' AND l_tr_ko = 'X'.
        MESSAGE i022(ytrm_msg) DISPLAY LIKE 'W'.
        CALL METHOD o_alv_trs->refresh_table_display
          EXCEPTIONS
            finished = 1
            OTHERS   = 2.
*   All Tr with errors
      ELSEIF l_tr_ok IS INITIAL AND l_tr_ko = 'X'.
        MESSAGE i023(ytrm_msg) DISPLAY LIKE 'E'.
*   No change made
      ELSEIF l_tr_ok IS INITIAL AND l_tr_ko IS INITIAL.
        MESSAGE s024(ytrm_msg) DISPLAY LIKE 'E'
                               WITH ws_ybcs_trs-trkorr.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " F100_DO_CHANGE_OWNER
*&---------------------------------------------------------------------*
*&      Form  F200_FILL_TR_STATUS_OLD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f200_fill_tr_status_old  USING fuw_struct_name TYPE ybcs_troa
                                    fuw_sysid      TYPE sy-sysid
                                    fuw_counter    TYPE flag.
  DATA : l_destination TYPE rfcdes-rfcdest.
  DATA : l_objname TYPE vrsd-objname,
         l_objtype TYPE  vrsd-objtype,
         l_tr_seq TYPE versno.



  CLEAR : l_objname, l_objtype.
  l_objname = ws_object_list-obj_name.
  l_objtype = ws_object_list-obj_type.
  CLEAR l_destination.
  CONCATENATE 'TMSADM@' fuw_sysid '.DOMAIN_' sy-sysid INTO l_destination.
  REFRESH : wt_lversno_list,
            wt_version_list.

  CALL FUNCTION 'SVRS_GET_VERSION_DIRECTORY_46'
    EXPORTING
      destination            = l_destination
      objname                = l_objname
      objtype                = l_objtype
    TABLES
      lversno_list           = wt_lversno_list
      version_list           = wt_version_list
    EXCEPTIONS
      no_entry               = 1
      communication_failure_ = 2
      system_failure         = 3
      OTHERS                 = 4.
  IF sy-subrc = 0.
    CASE fuw_sysid.
      WHEN 'IE1'.
        CLEAR ws_version_list.
        READ TABLE wt_version_list INTO ws_version_list INDEX 1.
        IF sy-subrc = 0.
          fuw_struct_name-status_qual = icon_checked.
          fuw_struct_name-date_qual = ws_version_list-datum.
          fuw_struct_name-time_qual = ws_version_list-zeit.
        ELSE.
          fuw_struct_name-status_qual = icon_incomplete.
        ENDIF.
      WHEN 'RE1'.
        CLEAR ws_version_list.
        READ TABLE wt_version_list INTO ws_version_list INDEX 1.
        IF sy-subrc = 0.
          fuw_struct_name-status_inte = icon_checked.
          fuw_struct_name-date_inte = ws_version_list-datum.
          fuw_struct_name-time_inte = ws_version_list-zeit.
        ELSE.
          fuw_struct_name-status_inte = icon_incomplete.
        ENDIF.
      WHEN 'PE1'.
        CLEAR ws_version_list.
        READ TABLE wt_version_list INTO ws_version_list INDEX 1.
        IF sy-subrc = 0.
          fuw_struct_name-status_prod = icon_checked.
          fuw_struct_name-date_prod = ws_version_list-datum.
          fuw_struct_name-time_prod = ws_version_list-zeit.
        ELSE.
          fuw_struct_name-status_prod = icon_incomplete.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDIF.

ENDFORM.                    " F200_FILL_TR_STATUS_OLD
