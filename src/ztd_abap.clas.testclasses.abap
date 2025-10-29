*"* use this source file for your ABAP unit test classes

CLASS lth_cut DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS dataset_transfer.
ENDCLASS.


CLASS lth_cut IMPLEMENTATION.
  METHOD dataset_transfer.
    DATA abap      TYPE REF TO zif_abap.
    DATA file_path TYPE string.

* This method is equivalent to this classic ABAP code:
*    DATA file_path TYPE string.
*    DATA text      TYPE string.
*
*    file_path = '/tmp/test'.
*    OPEN DATASET file_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
*    text = |Hello world, now is { sy-uzeit TIME = USER }|.
*    TRANSFER text TO file_path.
*    CLOSE DATASET file_path.

    abap = zcl_abap=>get_singleton( ).
    file_path = '/tmp/test'.
    abap->open_dataset( iv_file_path = file_path
                        iv_for       = abap->cs_open_dataset-for-output
                        iv_in_mode   = abap->cs_open_dataset-in_mode-text
                        iv_encoding  = abap->cs_open_dataset-encoding-utf_8 ).
    abap->transfer( iv_data      = |Hello world, now is { abap->get_sy_uzeit( ) TIME = USER }|
                    iv_file_path = file_path ).
    abap->close_dataset( iv_file_path = file_path ).
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_test_app DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS dataset_transfer FOR TESTING RAISING cx_static_check.
    METHODS call_cut.
ENDCLASS.


CLASS ltcl_test_app IMPLEMENTATION.
  METHOD call_cut.
    lth_cut=>dataset_transfer( ).
  ENDMETHOD.

  METHOD dataset_transfer.
    DATA abap TYPE REF TO ztd_abap.

    " GIVEN now is 152800 (fake)
    abap = ztd_abap=>get_singleton( ).
    abap->set_sy_uzeit( '152800' ).

    " WHEN code under test is called
    call_cut( ).

    " THEN the file contains this data with fake 152800
    cl_abap_unit_assert=>assert_equals( act = abap->get_dataset( '/tmp/test' )-content
                                        exp = VALUE string_table( ( |Hello world, now is 15:28:00| ) ) ).
  ENDMETHOD.
ENDCLASS.
