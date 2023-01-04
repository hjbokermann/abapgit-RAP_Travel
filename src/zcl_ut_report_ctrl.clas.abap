class ZCL_UT_REPORT_CTRL definition
  public
  final
  create protected

  global friends ZCL_UT_REPORT_FCT .

public section.

  interfaces ZIF_UT_CONTEXT .
  interfaces ZIF_UT_REPORT_CONST .

  aliases RUN
    for ZIF_UT_CONTEXT~RUN .
  aliases SET_CONTEXT
    for ZIF_UT_CONTEXT~SET_CONTEXT .
  aliases GTY_S_BOOKINGS
    for ZIF_UT_REPORT_CONST~GTY_S_BOOKINGS .
  aliases GTY_TT_BOOKINGS
    for ZIF_UT_REPORT_CONST~GTY_TT_BOOKINGS .
protected section.
private section.

  data MS_CONTEXT type ZBC_S_UT_SELECTION .
ENDCLASS.



CLASS ZCL_UT_REPORT_CTRL IMPLEMENTATION.


  METHOD zif_ut_context~run.

    DATA:
      lt_bookings TYPE gty_tt_bookings.


    DATA(lo_dao) = zcl_ut_report_fct=>get_report_dao( ).

    lt_bookings = lo_dao->get_data( is_selection = ms_context ).

    IF lt_bookings IS NOT INITIAL.
      rv_subrc = 0.
    ELSE.
      rv_subrc = 4.
    ENDIF.

    DATA(lo_view) = zcl_ut_report_fct=>get_report_view( ).
    lo_view->set_outtab( it_outtab = lt_bookings ).
    lo_view->display( ).


  ENDMETHOD.


  METHOD zif_ut_context~set_context.

    DATA:
      lr_context TYPE REF TO data.

    FIELD-SYMBOLS:
      <ls_context> TYPE zbc_s_ut_selection.

    TRY.

        CREATE DATA lr_context LIKE is_context.
        ASSIGN lr_context->* TO <ls_context>.
        <ls_context> = is_context.
        ms_context = <ls_context>.

      CATCH cx_sy_assign_error.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
