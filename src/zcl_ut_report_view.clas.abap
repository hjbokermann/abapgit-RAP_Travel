class ZCL_UT_REPORT_VIEW definition
  public
  final
  create private

  global friends ZCL_UT_REPORT_FCT .

public section.

  interfaces ZIF_UT_REPORT_VIEW .
  interfaces ZIF_UT_REPORT_CONST .

  aliases DISPLAY
    for ZIF_UT_REPORT_VIEW~DISPLAY .
  aliases SET_OUTTAB
    for ZIF_UT_REPORT_VIEW~SET_OUTTAB .
  aliases GTY_S_BOOKINGS
    for ZIF_UT_REPORT_CONST~GTY_S_BOOKINGS .
  aliases GTY_TT_BOOKINGS
    for ZIF_UT_REPORT_CONST~GTY_TT_BOOKINGS .
protected section.
private section.

  data MT_OUTTAB type GTY_TT_BOOKINGS .
ENDCLASS.



CLASS ZCL_UT_REPORT_VIEW IMPLEMENTATION.


  METHOD zif_ut_report_view~display.

    DATA:
      lo_salv_table TYPE REF TO cl_salv_table.

    TRY.

        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = lo_salv_table                          " Basis Class Simple ALV Tables
          CHANGING
            t_table        = mt_outtab ).

      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.

    IF lo_salv_table IS BOUND.

      lo_salv_table->display( ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_ut_report_view~set_outtab.

    DATA:
      lr_data TYPE REF TO data.

    FIELD-SYMBOLS:
      <lt_outtab> TYPE gty_tt_bookings.

    TRY.

        CREATE DATA lr_data LIKE it_outtab.
        ASSIGN lr_data->* TO <lt_outtab>.
        <lt_outtab> = it_outtab.
        mt_outtab = <lt_outtab>.

      CATCH cx_sy_assign_error.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
