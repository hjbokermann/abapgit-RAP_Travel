interface ZIF_UT_CONTEXT
  public .


  methods SET_CONTEXT
    importing
      !IS_CONTEXT type ANY .
  methods RUN
    returning
      value(RV_SUBRC) type SYSUBRC .
endinterface.
