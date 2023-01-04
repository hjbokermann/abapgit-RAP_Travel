INTERFACE zif_ut_report_const
  PUBLIC .

  TYPES:
    BEGIN OF gty_s_bookings,
      carrierid           TYPE s_carr_id,
      connectionid        TYPE s_conn_id,
      flightdate          TYPE s_date,
      bookid              TYPE s_book_id,
      customerid          TYPE s_customer,
      customertype        TYPE s_custtype,
      class               TYPE s_class,
      localcurrencyamount TYPE s_l_cur_pr,
      localcurrencycode   TYPE s_currcode,
      agencynumber        TYPE s_agncynum,
      passname            TYPE s_passname,
    END OF gty_s_bookings.

  TYPES:  gty_tt_bookings TYPE STANDARD TABLE OF gty_s_bookings WITH KEY carrierid connectionid flightdate bookid.

ENDINTERFACE.
