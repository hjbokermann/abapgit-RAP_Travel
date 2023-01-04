*&---------------------------------------------------------------------*
*& Report ZBC_UT_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbc_ut_report.

DATA:
  gs_SPFLI     TYPE spfli,
  gs_sbook     TYPE sbook,
  gs_selection TYPE zbc_s_ut_selection.

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS so_carr FOR gs_spfli-carrid.
  SELECT-OPTIONS so_conn FOR gs_spfli-connid.
  SELECT-OPTIONS so_fdat FOR gs_sbook-fldate.

SELECTION-SCREEN END OF BLOCK 1.


START-OF-SELECTION.

  gs_selection-r_carrid = so_carr[].
  gs_selection-r_connid = so_conn[].
  gs_selection-r_fldate = so_fdat[].

  DATA(go_ctrl) = zcl_ut_report_fct=>get_report_ctrl( ).

  go_ctrl->set_context( is_context = gs_selection ).
  go_ctrl->run( ).
