class ZCL_UT_REPORT_FCT definition
  public
  final
  create public

  global friends ZCL_UT_REPORT_FCT_INJ .

public section.

  class-methods GET_REPORT_CTRL
    returning
      value(RO_RESULT) type ref to ZIF_UT_CONTEXT .
  class-methods GET_REPORT_VIEW
    returning
      value(RO_RESULT) type ref to ZIF_UT_REPORT_VIEW .
  class-methods GET_REPORT_DAO
    returning
      value(RO_RESULT) type ref to ZIF_UT_REPORT_DAO .
protected section.
private section.

  class-data GO_REPORT_CTRL type ref to ZIF_UT_CONTEXT .
  class-data GO_REPORT_VIEW type ref to ZIF_UT_REPORT_VIEW .
  class-data GO_REPORT_DAO type ref to ZIF_UT_REPORT_DAO .
ENDCLASS.



CLASS ZCL_UT_REPORT_FCT IMPLEMENTATION.


  METHOD get_report_ctrl.

    IF go_report_ctrl IS NOT BOUND.
      go_report_ctrl = NEW zcl_ut_report_ctrl( ).
    ENDIF.
    ro_result = go_report_ctrl.

  ENDMETHOD.


  METHOD get_report_dao.

    IF go_report_dao IS NOT BOUND.
      go_report_dao = NEW zcl_ut_report_dao( ).
    ENDIF.
    ro_result = go_report_dao.

  ENDMETHOD.


  METHOD get_report_view.

    IF go_report_view IS NOT BOUND.
      go_report_view = NEW zcl_ut_report_view( ).
    ENDIF.
    ro_result = go_report_view.

  ENDMETHOD.
ENDCLASS.
