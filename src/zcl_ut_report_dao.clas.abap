class ZCL_UT_REPORT_DAO definition
  public
  final
  create protected

  global friends ZCL_UT_REPORT_FCT .

public section.

  interfaces ZIF_UT_REPORT_CONST .
  interfaces ZIF_UT_REPORT_DAO .

  aliases GET_DATA
    for ZIF_UT_REPORT_DAO~GET_DATA .
  aliases GTY_S_BOOKINGS
    for ZIF_UT_REPORT_CONST~GTY_S_BOOKINGS .
  aliases GTY_TT_BOOKINGS
    for ZIF_UT_REPORT_CONST~GTY_TT_BOOKINGS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_UT_REPORT_DAO IMPLEMENTATION.


  METHOD zif_ut_report_dao~get_data.

    DATA:
      lt_bookings TYPE gty_tt_bookings.


    SELECT carrierid, connectionid, flightdate, bookid, customerid,
           customertype, class, localcurrencyamount, localcurrencycode, agencynumber, passname
      FROM s_bookings
      INTO TABLE @lt_bookings
     WHERE carrierid IN @is_selection-r_carrid
       AND connectionid IN @is_selection-r_connid
       AND flightdate IN @is_selection-r_fldate
     ORDER BY carrierid, connectionid, flightdate.

    rt_result = lt_bookings.

  ENDMETHOD.
ENDCLASS.
