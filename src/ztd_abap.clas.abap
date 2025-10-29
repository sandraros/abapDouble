CLASS ztd_abap DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_abap.

    TYPES:
      BEGIN OF ts_dataset,
        file_path TYPE string,
        encoding  TYPE zif_abap=>ts_open_dataset-encoding,
        for       TYPE zif_abap=>ts_open_dataset-for,
        in_mode   TYPE zif_abap=>ts_open_dataset-in_mode,
        content   TYPE string_table,
        opened    TYPE abap_bool,
      END OF ts_dataset.
    TYPES tt_dataset TYPE SORTED TABLE OF ts_dataset WITH UNIQUE KEY file_path.

    METHODS constructor.

    CLASS-METHODS get_singleton
      RETURNING VALUE(ro_result) TYPE REF TO ztd_abap.

    METHODS get_abap_list
      RETURNING VALUE(rt_result) TYPE string_table.

    METHODS get_dataset
      IMPORTING iv_file_path     TYPE csequence
      RETURNING VALUE(rs_result) TYPE ts_dataset.

    METHODS internal_session_start.

    METHODS internal_session_stop.

    METHODS set_sy_datum
      IMPORTING iv_value TYPE sydatum.

    METHODS set_sy_linsz
      IMPORTING iv_value TYPE sylinsz.

    METHODS set_sy_repid
      IMPORTING iv_value TYPE syrepid.

    METHODS set_sy_slset
      IMPORTING iv_value TYPE syslset.

    METHODS set_sy_title
      IMPORTING iv_value TYPE sytitle.

    METHODS set_sy_uname
      IMPORTING iv_value TYPE syuname.

    METHODS set_sy_uzeit
      IMPORTING iv_value TYPE syuzeit.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ts_message,
        timestamp    TYPE timestampl,
        texte        TYPE string,
        type         TYPE symsgty,
        display_like TYPE symsgty,
      END OF ts_message.
    TYPES tt_message   TYPE SORTED TABLE OF ts_message WITH NON-UNIQUE KEY timestamp.
    TYPES tt_abap_list TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ts_internal_session,
        abap_list TYPE tt_abap_list,
      END OF ts_internal_session.
    TYPES tt_internal_session TYPE STANDARD TABLE OF ts_internal_session WITH EMPTY KEY.

    CLASS-DATA go_singleton TYPE REF TO ztd_abap.

    DATA gs_sy               TYPE sy.
    DATA gv_sy_repid         TYPE syrepid.
    DATA gt_dataset          TYPE tt_dataset.
    DATA gt_message          TYPE tt_message.
    DATA gt_internal_session TYPE tt_internal_session.
    DATA gs_internal_session TYPE REF TO ts_internal_session.
ENDCLASS.


CLASS ztd_abap IMPLEMENTATION.
  METHOD constructor.
    internal_session_start( ).
  ENDMETHOD.

  METHOD get_abap_list.
    rt_result = gs_internal_session->abap_list.
  ENDMETHOD.

  METHOD get_dataset.
    DATA lv_file_path TYPE string.

    lv_file_path = iv_file_path.
    READ TABLE gt_dataset WITH TABLE KEY file_path = lv_file_path INTO rs_result. "#EC CI_SUBRC
  ENDMETHOD.

  METHOD get_singleton.
    IF go_singleton IS NOT BOUND.
      CREATE OBJECT go_singleton TYPE ztd_abap.
      zcl_abap=>set_singleton( go_singleton ).
    ENDIF.
    ro_result = go_singleton.
  ENDMETHOD.

  METHOD internal_session_start.
    INSERT INITIAL LINE INTO TABLE gt_internal_session REFERENCE INTO gs_internal_session.
  ENDMETHOD.

  METHOD internal_session_stop.
    IF lines( gt_internal_session ) = 1.
      RETURN.
    ENDIF.
    DELETE gt_internal_session INDEX lines( gt_internal_session ).
    gs_internal_session = REF #( gt_internal_session[ lines( gt_internal_session ) ] ).
  ENDMETHOD.

  METHOD set_sy_datum.
    gs_sy-datum = iv_value.
  ENDMETHOD.

  METHOD set_sy_linsz.
    gs_sy-linsz = iv_value.
  ENDMETHOD.

  METHOD set_sy_repid.
    gv_sy_repid = iv_value.
  ENDMETHOD.

  METHOD set_sy_slset.
    gs_sy-slset = iv_value.
  ENDMETHOD.

  METHOD set_sy_title.
    gs_sy-title = iv_value.
  ENDMETHOD.

  METHOD set_sy_uname.
    gs_sy-uname = iv_value.
  ENDMETHOD.

  METHOD set_sy_uzeit.
    gs_sy-uzeit = iv_value.
  ENDMETHOD.

  METHOD zif_abap~close_dataset.
    DATA ls_dataset TYPE REF TO ts_dataset.
    DATA lv_file_path TYPE string.

    lv_file_path = iv_file_path.
    READ TABLE gt_dataset WITH TABLE KEY file_path = lv_file_path REFERENCE INTO ls_dataset.
    IF sy-subrc = 0.
      ls_dataset->opened = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abap~delete_dataset.
    DATA lv_file_path TYPE string.

    lv_file_path = iv_file_path.
    DELETE gt_dataset WHERE file_path = lv_file_path.
  ENDMETHOD.

  METHOD zif_abap~get_sy_datum.
    rv_result = gs_sy-datum.
  ENDMETHOD.

  METHOD zif_abap~get_sy_linsz.
    rv_result = gs_sy-linsz.
  ENDMETHOD.

  METHOD zif_abap~get_sy_repid.
    rv_result = gv_sy_repid.
  ENDMETHOD.

  METHOD zif_abap~get_sy_slset.
    rv_result = gs_sy-slset.
  ENDMETHOD.

  METHOD zif_abap~get_sy_title.
    rv_result = gs_sy-title.
  ENDMETHOD.

  METHOD zif_abap~get_sy_uname.
    rv_result = gs_sy-uname.
  ENDMETHOD.

  METHOD zif_abap~get_sy_uzeit.
    rv_result = gs_sy-uzeit.
  ENDMETHOD.

  METHOD zif_abap~message.
    DATA ls_message   TYPE ts_message.

    GET TIME STAMP FIELD ls_message-timestamp.
    ls_message-texte        = iv_text.
    ls_message-type         = iv_mtype.
    ls_message-display_like = iv_display_like.
    INSERT ls_message INTO TABLE gt_message.
  ENDMETHOD.

  METHOD zif_abap~open_dataset.
    DATA lv_file_path TYPE string.
    DATA ls_dataset TYPE REF TO ts_dataset.
    DATA ls_new_dataset TYPE ts_dataset.

    lv_file_path = iv_file_path.
    READ TABLE gt_dataset WITH TABLE KEY file_path = lv_file_path REFERENCE INTO ls_dataset.
    IF sy-subrc = 0
       AND ls_dataset->opened  = abap_true.
      RAISE EXCEPTION TYPE cx_sy_file_open
        EXPORTING
          filename = lv_file_path.
    ENDIF.

    IF ls_dataset IS NOT BOUND.
      ls_new_dataset-file_path = lv_file_path.
      INSERT ls_new_dataset INTO TABLE gt_dataset
             REFERENCE INTO ls_dataset.
    ENDIF.

    ls_dataset->encoding = iv_encoding.
    ls_dataset->for      = iv_for.
    ls_dataset->in_mode  = iv_in_mode.
    ls_dataset->opened   = abap_true.
    IF iv_for = zif_abap=>cs_open_dataset-for-output.
      CLEAR ls_dataset->content.
    ENDIF.
  ENDMETHOD.

  METHOD zif_abap~transfer.
    DATA lv_file_path TYPE string.
    DATA ls_dataset   TYPE REF TO ts_dataset.
    DATA lv_text      TYPE string.

    lv_file_path = iv_file_path.
    READ TABLE gt_dataset WITH TABLE KEY file_path = lv_file_path REFERENCE INTO ls_dataset.
    IF    sy-subrc        <> 0
       OR ls_dataset->for  = zif_abap=>cs_open_dataset-for-input.
      RAISE EXCEPTION TYPE cx_sy_file_open_mode
        EXPORTING
          filename = iv_file_path.
    ENDIF.

    " Allow flat structures
    lv_text = iv_data.
    INSERT lv_text INTO TABLE ls_dataset->content.
  ENDMETHOD.

  METHOD zif_abap~uline.
    zif_abap~write( |{ repeat( val = '-'
                               occ = zif_abap~get_sy_linsz( ) ) }| ).
  ENDMETHOD.

  METHOD zif_abap~write.
    DATA lv_text      TYPE string.
    DATA lv_last_line TYPE REF TO string.
    DATA lv_tabix     TYPE i.

* Start fix 2 for abaplint 7.02
    FIELD-SYMBOLS <lt_abap_list> TYPE tt_abap_list.
*    FIELD-SYMBOLS <lt_abap_list> TYPE ts_internal_session-abap_list.
* Start fix 2 for abaplint 7.02

    lv_text = iv_text.
    IF iv_same_line = abap_false.
      IF iv_position IS INITIAL.
        INSERT lv_text INTO TABLE gs_internal_session->abap_list.
      ELSE.
        lv_text = repeat( val = ` `
                          occ = iv_position - 1 )
                  && lv_text.
        INSERT lv_text INTO TABLE gs_internal_session->abap_list.
      ENDIF.
    ELSE.
      IF gs_internal_session->abap_list IS INITIAL.
        INSERT lv_text INTO TABLE gs_internal_session->abap_list.
      ELSE.
        lv_tabix = lines( gs_internal_session->abap_list ).
* Start fix 1 for abaplint 7.02
        ASSIGN gs_internal_session->abap_list TO <lt_abap_list>.
        READ TABLE <lt_abap_list> INDEX lv_tabix REFERENCE INTO lv_last_line.
*        READ TABLE gs_internal_session->abap_list INDEX lv_tabix REFERENCE INTO lv_last_line.
* End fix 1 for abaplint 7.02
        ASSERT sy-subrc = 0.
        IF iv_position IS INITIAL.
          lv_last_line->* = lv_last_line->* && ` ` && lv_text.
        ELSE.
          IF strlen( lv_last_line->* ) >= iv_position.
            lv_last_line->* = lv_last_line->*(iv_position).
          ELSE.
            lv_last_line->* = lv_last_line->* && repeat( val = ` `
                                                         occ = iv_position - strlen( lv_last_line->* ) - 1 ).
          ENDIF.
          lv_last_line->* = lv_last_line->* && lv_text.
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
