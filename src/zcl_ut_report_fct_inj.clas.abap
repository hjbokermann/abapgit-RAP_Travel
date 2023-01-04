class ZCL_UT_REPORT_FCT_INJ definition
  public
  final
  create private .

public section.

  class-methods INJECT_REPORT_CTRL
    importing
      !IO_REPORT_CTRL type ref to ZIF_UT_CONTEXT .
  class-methods INJECT_REPORT_VIEW
    importing
      !IO_REPORT_VIEW type ref to ZIF_UT_REPORT_VIEW .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UT_REPORT_FCT_INJ IMPLEMENTATION.


  METHOD inject_report_ctrl.

    zcl_ut_report_fct=>go_report_ctrl = io_report_ctrl.

  ENDMETHOD.


  METHOD inject_report_view.

    zcl_ut_report_fct=>go_report_view = io_report_view.

  ENDMETHOD.
ENDCLASS.
