*"* use this source file for your ABAP unit test classes
CLASS ltc_ut_report_ctrl DEFINITION FOR TESTING
                          RISK LEVEL HARMLESS
                          DURATION SHORT.

  PRIVATE SECTION.
    DATA:
      mo_ctu     TYPE REF TO zif_ut_context,
      mo_td_view TYPE REF TO zif_ut_report_view.

    METHODS:
      setup,
      run_successful FOR TESTING,
      run_unsuccessful FOR TESTING.

ENDCLASS.

CLASS ltd_ut_report_view DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES: zif_ut_report_view PARTIALLY IMPLEMENTED.

    DATA: mv_was_here TYPE boolean_flg.

ENDCLASS.

CLASS ltd_ut_report_view IMPLEMENTATION.

  METHOD zif_ut_report_view~set_outtab.

  ENDMETHOD.

  METHOD zif_ut_report_view~display.

    mv_was_here = abap_true.

  ENDMETHOD.

ENDCLASS.

CLASS ltc_ut_report_ctrl IMPLEMENTATION.

  METHOD setup.

    mo_ctu = zcl_ut_report_fct=>get_report_ctrl( ).
    mo_td_view = CAST zif_ut_report_view( NEW ltd_ut_report_view( )  ).
    zcl_ut_report_fct_inj=>inject_report_view( io_report_view = mo_td_view ).

  ENDMETHOD.

  METHOD run_successful.

    DATA:
      ls_context TYPE zbc_s_ut_selection.

    ls_context-r_carrid = VALUE #( ( sign = 'I' option = 'EQ' low = 'LH' ) ).
    ls_context-r_connid = VALUE #( ( sign = 'I' option = 'EQ' low = '2407' ) ).

    mo_ctu->set_context( is_context = ls_context ).

    cl_abap_unit_assert=>assert_equals(  act = mo_ctu->run( )
                                         exp = 0 ).

  ENDMETHOD.

  METHOD run_unsuccessful.

    DATA:
      ls_context TYPE zbc_s_ut_selection.

    ls_context-r_carrid = VALUE #( ( sign = 'I' option = 'EQ' low = 'LH' ) ).
    ls_context-r_connid = VALUE #( ( sign = 'I' option = 'EQ' low = '2407' ) ).
    ls_context-r_fldate = VALUE #( ( sign = 'I' option = 'EQ' low = '20190617' ) ).

    mo_ctu->set_context( is_context = ls_context ).

    cl_abap_unit_assert=>assert_equals(  act = mo_ctu->run( )
                                         exp = 4 ).

  ENDMETHOD.

ENDCLASS.
