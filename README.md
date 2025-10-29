# abapDouble
Wrapper and test doubler of ABAP Language elements OPEN DATASET, WRITE...

1. If you had this code:
   ```abap
   DATA(file_path) = '/tmp/test'.
   OPEN DATASET file_path FOR OUTPUT IN TEXT MODE ENCODING UTF-8.
   TRANSFER |Hello world, now is { sy-uzeit TIME = USER }| TO file_path.
   CLOSE DATASET file_path.
   ```
1. Now use this:
   ```abap
   DATA(abap) = zcl_abap=>get_singleton( ).
   DATA(file_path) = '/tmp/test'.
   abap->open_dataset( iv_file_path = file_path 
                       iv_for       = 
                       iv_in_mode   = 
                       iv_encoding  = ).
   abap->open_dataset( 
       iv_data      = |Hello world, now is { abap->get_sy_uzeit( ) TIME = USER }|
       iv_file_path = file_path ). 
   abap->close_dataset( iv_file_path = file_path ).
   ```
1. Test this way:
   ```abap
     METHOD first_test. " declared FOR TESTING
       " GIVEN now is 152800 (fake)
       DATA(abap) = zzsro_td_abap=>get_singleton( ).
       abap->set_sy_uzeit( '152800' ).
   
       " WHEN code under test is called
       call_cut( ).
   
       " THEN the file contains this data with fake 152800
       cl_abap_unit_assert=>assert_equals( act = abap->get_dataset( '/tmp/test' )-content
                                           exp = VALUE string_table( ( |Hello world, now is 15:28:00| ) ) ).
     ENDMETHOD.
   ```
   NB: currently, the user time formatting is expected to be based on a 24-hours clock, but the running user could be based on a 12-hours clock AM/PM which would make the ABAP Unit fail.

# List of supported keywords and system variables

Currently, these ABAP keywords are supported:
- `CLOSE DATASET`
- `DELETE DATASET`
- `MESSAGE`
  - Limited to text, message type, display like. It also gives the possibility to re-send the current system message (variables `SY-MSGNO`, etc.)
- `OPEN DATASET`
  - Limited to text mode, encodings default and UTF-8
- `TRANSFER`
- `WRITE`
  - Limited to character-like text and to these options `/`: (start new line), not `/` (same line), colors, position in the line.
- `WRITE TO`
  - Limited to character-like text.

Also, these system variables are supported:
- `SY-DATUM`
- `SY-LINSZ`
- `SY-REPID`
- `SY-SLSET`
- `SY-TITLE`
- `SY-UNAME`
- `SY-UZEIT`
