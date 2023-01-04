*"* use this source file for your ABAP unit test classes
CLASS ltc_ut_report_dao DEFINITION FOR TESTING
                          RISK LEVEL HARMLESS
                          DURATION SHORT.

  PRIVATE SECTION.
    DATA:
       mo_cut TYPE REF TO zif_ut_report_dao.

    METHODS:
      setup,
      selection_successul FOR TESTING,
      selection_unsuccessful FOR TESTING.


ENDCLASS.

CLASS ltc_ut_report_dao IMPLEMENTATION.

  METHOD setup.

    mo_cut = zcl_ut_report_fct=>get_report_dao( ).

  ENDMETHOD.

  METHOD selection_successul.

    DATA:
      ls_selection TYPE zbc_s_ut_selection.

    ls_selection =  VALUE #( r_carrid = VALUE #( ( sign = 'I'
                                                   option = 'EQ'
                                                   low = 'LH' ) )
                             r_connid = VALUE #( ( sign = 'I'
                                                   option = 'EQ'
                                                   low = '401' ) ) ).

    cl_abap_unit_assert=>assert_not_initial( act =  mo_cut->get_data( is_selection = ls_selection ) ).

  ENDMETHOD.

  METHOD selection_unsuccessful.

    DATA:
      ls_selection TYPE zbc_s_ut_selection.

    ls_selection =  VALUE #( r_carrid = VALUE #( ( sign = 'I'
                                                   option = 'EQ'
                                                   low = 'LH' ) )
                             r_connid = VALUE #( ( sign = 'I'
                                                   option = 'EQ'
                                                   low = '401' ) )
                             r_fldate = VALUE #( ( sign = 'I'
                                                   option = 'EQ'
                                                   low = '20200617' ) )   ).

    cl_abap_unit_assert=>assert_initial( act =  mo_cut->get_data( is_selection = ls_selection ) ).

  ENDMETHOD.

ENDCLASS.
