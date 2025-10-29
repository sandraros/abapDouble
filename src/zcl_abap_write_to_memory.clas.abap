"! Somewhat equivalent to writing the ABAP List through SUBMIT EXPORTING LIST TO MEMORY and the function module LIST_FROM_MEMORY,
"! except that it's redirecting to the memory immediately when the ABAP List is being written (and so, actually there's no ABAP List written).
"! The class is used privately by the method REDIRECT_WRITE_TO_MEMORY and WRITE_TO of the class ZCL_ABAP.
CLASS zcl_abap_write_to_memory DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_abap
                 ztd_abap.

  PUBLIC SECTION.
    INTERFACES zif_abap.

    METHODS constructor
      IMPORTING it_itab_to_write_to TYPE REF TO string_table.

  PRIVATE SECTION.
    DATA go_abap             TYPE REF TO zif_abap.
    DATA gt_itab_to_write_to TYPE REF TO string_table.
ENDCLASS.


CLASS zcl_abap_write_to_memory IMPLEMENTATION.
  METHOD constructor.
    go_abap = zcl_abap=>get_singleton( ).
    gt_itab_to_write_to = it_itab_to_write_to.
  ENDMETHOD.

  METHOD zif_abap~close_dataset.
  ENDMETHOD.

  METHOD zif_abap~delete_dataset.
  ENDMETHOD.

  METHOD zif_abap~get_sy_datum.
  ENDMETHOD.

  METHOD zif_abap~get_sy_linsz.
  ENDMETHOD.

  METHOD zif_abap~get_sy_repid.
  ENDMETHOD.

  METHOD zif_abap~get_sy_slset.
  ENDMETHOD.

  METHOD zif_abap~get_sy_title.
  ENDMETHOD.

  METHOD zif_abap~get_sy_uname.
  ENDMETHOD.

  METHOD zif_abap~get_sy_uzeit.
  ENDMETHOD.

  METHOD zif_abap~message.
  ENDMETHOD.

  METHOD zif_abap~open_dataset.
  ENDMETHOD.

  METHOD zif_abap~transfer.
  ENDMETHOD.

  METHOD zif_abap~uline.
  ENDMETHOD.

  METHOD zif_abap~write.
    TYPES tt_string_table TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    DATA lv_text      TYPE string.
    DATA lv_tabix     TYPE i.
    DATA lv_last_line TYPE REF TO string.

    FIELD-SYMBOLS <lt_itab_to_write_to> TYPE tt_string_table.

    ASSIGN gt_itab_to_write_to->* TO <lt_itab_to_write_to>.
    lv_text = iv_text.
    IF iv_same_line = abap_false.
      IF iv_position IS INITIAL.
        INSERT lv_text INTO TABLE <lt_itab_to_write_to>.
      ELSE.
        lv_text = repeat( val = ` `
                          occ = iv_position - 1 )
                  && lv_text.
        INSERT lv_text
               INTO TABLE <lt_itab_to_write_to>.
      ENDIF.
    ELSE.
      IF gt_itab_to_write_to->* IS INITIAL.
        INSERT lv_text INTO TABLE <lt_itab_to_write_to>.
      ELSE.
        lv_tabix = lines( <lt_itab_to_write_to> ).
        READ TABLE <lt_itab_to_write_to> INDEX lv_tabix REFERENCE INTO lv_last_line.
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
  ENDMETHOD.
ENDCLASS.
