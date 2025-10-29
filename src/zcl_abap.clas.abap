CLASS zcl_abap DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS ztd_abap.

  PUBLIC SECTION.
    INTERFACES zif_abap.

    CLASS-METHODS get_singleton
      RETURNING VALUE(ro_result) TYPE REF TO zif_abap.

    METHODS get_write_to_memory_data
      RETURNING VALUE(rt_result) TYPE string_table.

    METHODS redirect_write_to_memory
      IMPORTING iv_redirect TYPE abap_bool DEFAULT abap_true.

  PRIVATE SECTION.
    CLASS-DATA go_singleton TYPE REF TO zif_abap.

    DATA t_write_to_memory_data     TYPE string_table.
    DATA o_redirect_write_to_memory TYPE REF TO zcl_abap_write_to_memory.

    CLASS-METHODS set_singleton
      IMPORTING io_singleton TYPE REF TO zif_abap.
ENDCLASS.


CLASS zcl_abap IMPLEMENTATION.
  METHOD get_write_to_memory_data.
    rt_result = t_write_to_memory_data.
  ENDMETHOD.

  METHOD get_singleton.
    IF go_singleton IS NOT BOUND.
      CREATE OBJECT go_singleton TYPE zcl_abap.
    ENDIF.
    ro_result = go_singleton.
  ENDMETHOD.

  METHOD redirect_write_to_memory.
    DATA lv_reference_write_to_memory TYPE REF TO string_table.

    IF iv_redirect = abap_true.
      GET REFERENCE OF t_write_to_memory_data INTO lv_reference_write_to_memory.
      CREATE OBJECT o_redirect_write_to_memory TYPE zcl_abap_write_to_memory
        EXPORTING
          it_itab_to_write_to = lv_reference_write_to_memory.
    ELSE.
      CLEAR o_redirect_write_to_memory.
    ENDIF.
  ENDMETHOD.

  METHOD set_singleton.
    go_singleton = io_singleton.
  ENDMETHOD.

  METHOD zif_abap~close_dataset.
    CLOSE DATASET iv_file_path.
  ENDMETHOD.

  METHOD zif_abap~delete_dataset.
    DELETE DATASET iv_file_path.
  ENDMETHOD.

  METHOD zif_abap~get_sy_datum.
    rv_result = sy-datum.
  ENDMETHOD.

  METHOD zif_abap~get_sy_linsz.
    rv_result = sy-linsz.
  ENDMETHOD.

  METHOD zif_abap~get_sy_repid.
    DATA lt_abap_call_stack TYPE cl_abap_get_call_stack=>formatted_entry_stack.

    " In this class, SY-REPID equals ZCL_abap======================CP.
    " It's neede to simulate SY-REPID in the calling program by looking at the ABAP call stack.
    lt_abap_call_stack = cl_abap_get_call_stack=>format_call_stack_with_struct( cl_abap_get_call_stack=>get_call_stack( ) ).
    rv_result = lt_abap_call_stack[ 2 ]-progname.
  ENDMETHOD.

  METHOD zif_abap~get_sy_slset.
    rv_result = sy-slset.
  ENDMETHOD.

  METHOD zif_abap~get_sy_title.
    rv_result = sy-title.
  ENDMETHOD.

  METHOD zif_abap~get_sy_uname.
    rv_result = sy-uname.
  ENDMETHOD.

  METHOD zif_abap~get_sy_uzeit.
    rv_result = sy-uzeit.
  ENDMETHOD.

  METHOD zif_abap~message.
    IF iv_use_system_message = abap_true.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      DATA(lv_display_like) = COND symsgty( WHEN iv_display_like IS NOT INITIAL
                                            THEN iv_display_like
                                            ELSE iv_mtype ).
      MESSAGE iv_text TYPE iv_mtype DISPLAY LIKE lv_display_like.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abap~open_dataset.
    IF     iv_for      = zif_abap=>cs_open_dataset-for-output
       AND iv_in_mode  = zif_abap=>cs_open_dataset-in_mode-text
       AND iv_encoding = zif_abap=>cs_open_dataset-encoding-default.
      OPEN DATASET iv_file_path
           FOR OUTPUT
           IN TEXT MODE
           ENCODING DEFAULT
           MESSAGE ev_message.
    ELSEIF     iv_for                        = zif_abap=>cs_open_dataset-for-output
           AND iv_in_mode                    = zif_abap=>cs_open_dataset-in_mode-legacy_text
           AND iv_ignoring_conversion_errors = abap_true.
      OPEN DATASET iv_file_path
           FOR OUTPUT
           IN LEGACY TEXT MODE
           MESSAGE ev_message
           IGNORING CONVERSION ERRORS.
    ELSEIF     iv_for      = zif_abap=>cs_open_dataset-for-appending
           AND iv_in_mode  = zif_abap=>cs_open_dataset-in_mode-text
           AND iv_encoding = zif_abap=>cs_open_dataset-encoding-default.
      OPEN DATASET iv_file_path
           FOR APPENDING
           IN TEXT MODE
           ENCODING DEFAULT
           MESSAGE ev_message.

    ELSEIF     iv_for                        = zif_abap=>cs_open_dataset-for-appending
           AND iv_in_mode                    = zif_abap=>cs_open_dataset-in_mode-legacy_text
           AND iv_ignoring_conversion_errors = abap_true.
      OPEN DATASET iv_file_path
           FOR APPENDING
           IN LEGACY TEXT MODE
           MESSAGE ev_message
           IGNORING CONVERSION ERRORS.
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_file_open
        EXPORTING
          filename = iv_file_path.
    ENDIF.
    ev_subrc = sy-subrc.
  ENDMETHOD.

  METHOD zif_abap~transfer.
    TRANSFER iv_data TO iv_file_path.
  ENDMETHOD.

  METHOD zif_abap~uline.
    ULINE.
  ENDMETHOD.

  METHOD zif_abap~write.
    IF o_redirect_write_to_memory IS BOUND.
      o_redirect_write_to_memory->zif_abap~write( iv_text      = iv_text
                                                  iv_same_line = iv_same_line
                                                  iv_position  = iv_position ).
      RETURN.
    ENDIF.

    IF iv_same_line = abap_false.
      IF iv_text IS INITIAL.
        SKIP 1.
      ELSE.
        IF iv_position IS INITIAL.
          IF iv_color IS INITIAL.
            WRITE / iv_text.
          ELSE.
            WRITE / iv_text COLOR = iv_color.
          ENDIF.
        ELSE.
          IF iv_color IS INITIAL.
            WRITE AT /iv_position iv_text.
          ELSE.
            WRITE AT /iv_position iv_text COLOR = iv_color.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      IF iv_position IS INITIAL.
        IF iv_color IS INITIAL.
          WRITE iv_text.
        ELSE.
          WRITE iv_text COLOR = iv_color.
        ENDIF.
      ELSE.
        IF iv_color IS INITIAL.
          WRITE AT iv_position iv_text.
        ELSE.
          WRITE AT iv_position iv_text COLOR = iv_color.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abap~write_to.
    DATA lt_itab_to_write_to           TYPE string_table.
    DATA lv_reference_itab_to_write_to TYPE REF TO string_table.
    DATA lo_write_to_itab              TYPE REF TO zcl_abap_write_to_memory.

    CLEAR lt_itab_to_write_to.
    GET REFERENCE OF lt_itab_to_write_to INTO lv_reference_itab_to_write_to.
    CREATE OBJECT lo_write_to_itab TYPE zcl_abap_write_to_memory
      EXPORTING
        it_itab_to_write_to = lv_reference_itab_to_write_to.
    lo_write_to_itab->zif_abap~write( iv_text ).
    rv_field = lt_itab_to_write_to[ 1 ].
  ENDMETHOD.
ENDCLASS.
