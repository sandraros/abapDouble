INTERFACE zif_abap
  PUBLIC.

  TYPES: BEGIN OF ts_open_dataset,
           encoding TYPE c LENGTH 7,
           for      TYPE c LENGTH 9,
           in_mode  TYPE c LENGTH 11,
         END OF ts_open_dataset.

  CONSTANTS:
    BEGIN OF cs_message_type,
      abort       TYPE symsgty VALUE 'A',
      error       TYPE symsgty VALUE 'E',
      information TYPE symsgty VALUE 'I',
      success     TYPE symsgty VALUE 'S',
      warning     TYPE symsgty VALUE 'W',
      x_exception TYPE symsgty VALUE 'X',
    END OF cs_message_type.

  CONSTANTS:
    BEGIN OF cs_message,
      type         LIKE cs_message_type VALUE cs_message_type,
      display_like LIKE cs_message_type VALUE cs_message_type,
    END OF cs_message.

  CONSTANTS:
    BEGIN OF cs_open_dataset,
      BEGIN OF encoding,
        default TYPE ts_open_dataset-encoding VALUE 'DEFAULT',
        utf_8   TYPE ts_open_dataset-encoding VALUE 'UTF-8',
      END OF encoding,
      BEGIN OF for,
        input     TYPE ts_open_dataset-for VALUE 'INPUT',
        output    TYPE ts_open_dataset-for VALUE 'OUTPUT',
        appending TYPE ts_open_dataset-for VALUE 'APPENDING',
      END OF for,
      BEGIN OF in_mode,
        binary      TYPE ts_open_dataset-in_mode VALUE 'BINARY',
        legacy_text TYPE ts_open_dataset-in_mode VALUE 'LEGACY TEXT',
        text        TYPE ts_open_dataset-in_mode VALUE 'TEXT',
      END OF in_mode,
    END OF cs_open_dataset.

  METHODS close_dataset
    IMPORTING iv_file_path TYPE csequence.

  METHODS delete_dataset
    IMPORTING iv_file_path TYPE csequence.

  METHODS get_sy_datum
    RETURNING VALUE(rv_result) TYPE sydatum.

  METHODS get_sy_linsz
    RETURNING VALUE(rv_result) TYPE sylinsz.

  METHODS get_sy_repid
    RETURNING VALUE(rv_result) TYPE syrepid.

  METHODS get_sy_slset
    RETURNING VALUE(rv_result) TYPE syslset.

  METHODS get_sy_title
    RETURNING VALUE(rv_result) TYPE sytitle.

  METHODS get_sy_uname
    RETURNING VALUE(rv_result) TYPE syuname.

  METHODS get_sy_uzeit
    RETURNING VALUE(rv_result) TYPE syuzeit.

  "! @parameter iv_use_system_message | <ul>
  "!        <li>'X' : send the message like ID sy-msgid NUMBER sy-msgno TYPE sy-msgty WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4</li>
  "!        <li>' ' : send the message like MESSAGE iv_text TYPE iv_mtype DISPLAY LIKE iv_display_like.</li>
  "!        </ul>
  "! @parameter iv_text | send a message text, the parameter iv_mtype must be passed. Same as MESSAGE text TYPE mtype.
  "! @parameter iv_mtype | Message type 'A', 'E', 'I', 'S', 'W', 'X'
  "! @parameter iv_display_like | ' ' (default) : takes the same value as parameter IV_MTYPE.
  METHODS message
    IMPORTING iv_use_system_message TYPE abap_bool DEFAULT abap_false
              iv_text               TYPE csequence OPTIONAL
              iv_mtype              TYPE symsgty   OPTIONAL
              iv_display_like       TYPE symsgty   OPTIONAL.

  METHODS open_dataset
    IMPORTING iv_file_path                  TYPE csequence
              iv_for                        TYPE ts_open_dataset-for      DEFAULT cs_open_dataset-for-input
              iv_in_mode                    TYPE ts_open_dataset-in_mode  DEFAULT cs_open_dataset-in_mode-text
              iv_encoding                   TYPE ts_open_dataset-encoding DEFAULT cs_open_dataset-encoding-default
              iv_ignoring_conversion_errors TYPE abap_bool                DEFAULT abap_false
    EXPORTING ev_message                    TYPE string
              ev_subrc                      TYPE sysubrc
    RAISING   cx_sy_file_open.

  METHODS transfer
    IMPORTING iv_data      TYPE simple
              iv_file_path TYPE csequence
    RAISING   cx_sy_file_open_mode.

  METHODS uline.

  METHODS write
    IMPORTING iv_text      TYPE csequence
              iv_same_line TYPE abap_bool DEFAULT abap_false
              iv_color     TYPE i         OPTIONAL
              iv_position  TYPE i         OPTIONAL.

  METHODS write_to
    IMPORTING iv_text         TYPE csequence
    RETURNING VALUE(rv_field) TYPE string.
ENDINTERFACE.
