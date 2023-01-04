CLASS lhc_booking DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS calculateBookingID FOR DETERMINE ON MODIFY
      IMPORTING keys FOR booking~calculateBookingID.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR booking~calculateTotalPrice.

ENDCLASS.

CLASS lhc_booking IMPLEMENTATION.

  METHOD calculateBookingID.

    DATA: lv_max_bookingid TYPE /dmo/booking_id.
    DATA: lt_update TYPE TABLE FOR UPDATE zbc_i_rap_travel_007\\booking.

    " Read all travels for the requested bookings.
    " If multiple bookings of the same travel are requested, the travel is returned only once.
    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
    ENTITY Booking BY \_Travel
      FIELDS ( TravelUUID )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    " Process all affected Travels. Read respective bookings, determine the max-id and update the bookings without ID.
    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travels>).
      READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
        ENTITY Travel BY \_Booking
          FIELDS ( BookingID )
        WITH VALUE #( ( %tky = <ls_travels>-%tky ) )
        RESULT DATA(lt_bookings).

      " Find max used BookingID in all bookings of this travel
      lv_max_bookingid ='0000'.
      LOOP AT lt_bookings ASSIGNING FIELD-SYMBOL(<ls_bookings>).
        IF <ls_bookings>-BookingID > lv_max_bookingid.
          lv_max_bookingid = <ls_bookings>-BookingID.
        ENDIF.
      ENDLOOP.

      " Provide a booking ID for all bookings that have none.
      LOOP AT lt_bookings ASSIGNING <ls_bookings>
        WHERE BookingID IS INITIAL.
        lv_max_bookingid += 10.
        APPEND VALUE #( %tky      = <ls_bookings>-%tky
                        BookingID = lv_max_bookingid
                      ) TO lt_update.
      ENDLOOP.
    ENDLOOP.

    " Update the Booking ID of all relevant bookings
    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
    ENTITY Booking
      UPDATE FIELDS ( BookingID ) WITH lt_update
    REPORTED DATA(lt_update_reported).

    reported = CORRESPONDING #( DEEP lt_update_reported ).


  ENDMETHOD.

  METHOD calculateTotalPrice.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Booking BY \_Travel
        FIELDS ( TravelUUID )
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels)
      FAILED DATA(lt_read_failed).

    " Trigger calculation of the total price
    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
    ENTITY Travel
      EXECUTE recalcTotalPrice
      FROM CORRESPONDING #( lt_travels )
    REPORTED DATA(lt_execute_reported).

    reported = CORRESPONDING #( DEEP lt_execute_reported ).


  ENDMETHOD.

ENDCLASS.
