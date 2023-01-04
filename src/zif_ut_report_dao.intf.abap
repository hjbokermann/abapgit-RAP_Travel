interface ZIF_UT_REPORT_DAO
  public .


  methods GET_DATA
    importing
      !IS_SELECTION type ZBC_S_UT_SELECTION
    returning
      value(RT_RESULT) type ZIF_UT_REPORT_CONST=>GTY_TT_BOOKINGS .
endinterface.
