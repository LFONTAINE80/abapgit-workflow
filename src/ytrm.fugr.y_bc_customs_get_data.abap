FUNCTION Y_BC_CUSTOMS_GET_DATA.
*"--------------------------------------------------------------------
*"*"Interface locale :
*"  IMPORTING
*"     REFERENCE(IW_KEY1) TYPE  YKEY1
*"     REFERENCE(IW_KEY2) TYPE  YKEY2 OPTIONAL
*"     REFERENCE(IW_KEY3) TYPE  YKEY3 OPTIONAL
*"     REFERENCE(IW_KEY4) TYPE  YKEY4 OPTIONAL
*"     REFERENCE(IW_KEY5) TYPE  YKEY5 OPTIONAL
*"     REFERENCE(IW_CHECK_STAR) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(EWS_YBCT_CUSTOM) TYPE  YBCT_CUSTOM
*"  EXCEPTIONS
*"      KEY_NOT_FILLED
*"      NO_DATA_FOUND
*"      SINGLE_ACCESS_FAILED
*"--------------------------------------------------------------------
DATA : lt_ybct_custom TYPE TABLE OF ybct_custom,
         ls_ybct_custom TYPE ybct_custom.


  IF iw_key1 IS INITIAL OR iw_key1 = '*'.
    RAISE key_not_filled.
  ELSE.
*-------------   retrieve all corresponding task code data
    CLEAR ls_ybct_custom.
    SELECT SINGLE * FROM  ybct_custom
                    INTO ls_ybct_custom
           WHERE  key1  = iw_key1
           AND    key2  = iw_key2
           AND    key3  = iw_key3
           AND    key4  = iw_key4
           AND    key5  = iw_key5.

    IF sy-subrc = 0.
      ews_ybct_custom = ls_ybct_custom.
    ELSE.

      IF iw_check_star IS INITIAL.
        RAISE single_access_failed.
      ELSE.

*-------------   check if a key contains string '*'
*       retrieve all entries for input task code
        REFRESH lt_ybct_custom.
        SELECT * FROM  ybct_custom
                 INTO TABLE lt_ybct_custom
          WHERE  key1  = iw_key1.

        IF sy-subrc = 0.
*       delete entries without '*' in key fields
          CLEAR ls_ybct_custom.
          LOOP AT lt_ybct_custom INTO ls_ybct_custom.
            IF  ls_ybct_custom-key2 NS '*'
            AND ls_ybct_custom-key3 NS '*'
            AND ls_ybct_custom-key4 NS '*'
            AND ls_ybct_custom-key5 NS '*'.
              DELETE lt_ybct_custom INDEX sy-tabix.
            ENDIF.
          ENDLOOP.

*         check if input data and activation entries match
          SORT lt_ybct_custom DESCENDING BY key1 key2 key3 key4 key5.
          CLEAR ls_ybct_custom.
          LOOP AT lt_ybct_custom INTO ls_ybct_custom.
            IF NOT ls_ybct_custom-key2 IS INITIAL           "Key2
              AND NOT iw_key2 CP ls_ybct_custom-key2.
              CONTINUE.
            ENDIF.
            IF NOT ls_ybct_custom-key3 IS INITIAL           "Key3
              AND NOT iw_key3 CP ls_ybct_custom-key3.
              CONTINUE.
            ENDIF.
            IF NOT ls_ybct_custom-key4 IS INITIAL           "Key4
              AND NOT iw_key4 CP ls_ybct_custom-key4.
              CONTINUE.
            ENDIF.
            IF NOT ls_ybct_custom-key5 IS INITIAL           "Key5
              AND NOT iw_key5 CP ls_ybct_custom-key5.
              CONTINUE.
            ENDIF.
*           if all entries are correct, export data and exit the loop
            ews_ybct_custom = ls_ybct_custom.
            EXIT.
          ENDLOOP.

          IF ews_ybct_custom IS INITIAL.
            RAISE single_access_failed.
          ENDIF.
        ELSE.
          IF lt_ybct_custom[] IS INITIAL.
*         if no entry -> exit FM
            RAISE single_access_failed.
          ENDIF.
        ENDIF.  "lt_YBCT_CUSTOM IS INITIAL
      ENDIF.  "check_star IS INITIAL
    ENDIF.
  ENDIF.  "SELECT SINGLE * FROM  YBCT_CUSTOM





ENDFUNCTION.
