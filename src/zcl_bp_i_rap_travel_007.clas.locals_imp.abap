CLASS lhc_Travel DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    CONSTANTS:
      BEGIN OF gc_s_travel_status,
        open      TYPE c LENGTH 1 VALUE 'O',
        accepted  TYPE c LENGTH 1 VALUE 'A',
        cancelled TYPE c LENGTH 1 VALUE 'X',
      END OF gc_s_travel_status.

    METHODS get_instance_features FOR INSTANCE FEATURES
      IMPORTING keys REQUEST requested_features FOR Travel RESULT result.

    METHODS acceptTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~acceptTravel RESULT result.

    METHODS recalcTotalPrice FOR MODIFY
      IMPORTING keys FOR ACTION Travel~recalcTotalPrice.

    METHODS rejectTravel FOR MODIFY
      IMPORTING keys FOR ACTION Travel~rejectTravel RESULT result.

    METHODS calculateTotalPrice FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~calculateTotalPrice.

    METHODS setInitialStatus FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Travel~setInitialStatus.

    METHODS calculateTravelID FOR DETERMINE ON SAVE
      IMPORTING keys FOR Travel~calculateTravelID.

    METHODS validateAgency FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateAgency.

    METHODS validateCustomer FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateCustomer.

    METHODS validateDates FOR VALIDATE ON SAVE
      IMPORTING keys FOR Travel~validateDates.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Travel RESULT result.

    METHODS is_update_granted IMPORTING iv_has_before_image      TYPE abap_bool
                                        iv_overall_status        TYPE /dmo/overall_status
                              RETURNING VALUE(rv_update_granted) TYPE abap_bool.

    METHODS is_delete_granted IMPORTING iv_has_before_image      TYPE abap_bool
                                        iv_overall_status        TYPE /dmo/overall_status
                              RETURNING VALUE(rv_delete_granted) TYPE abap_bool.

    METHODS is_create_granted RETURNING VALUE(rv_create_granted) TYPE abap_bool.

ENDCLASS.

CLASS lhc_Travel IMPLEMENTATION.

  METHOD get_instance_features.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
       ENTITY Travel
         FIELDS ( TravelStatus ) WITH CORRESPONDING #( keys )
       RESULT DATA(lt_travels)
       FAILED failed.

    result =
      VALUE #(
        FOR <ls_travels> IN lt_travels
          LET lv_is_accepted =   COND #( WHEN <ls_travels>-TravelStatus = gc_s_travel_status-accepted
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled  )
              lv_is_rejected =   COND #( WHEN <ls_travels>-TravelStatus = gc_s_travel_status-cancelled
                                      THEN if_abap_behv=>fc-o-disabled
                                      ELSE if_abap_behv=>fc-o-enabled )
          IN
            ( %tky                 = <ls_travels>-%tky
              %action-acceptTravel = lv_is_accepted
              %action-rejectTravel = lv_is_rejected
             ) ).


  ENDMETHOD.

  METHOD acceptTravel.

    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
    ENTITY Travel
    UPDATE FIELDS ( TravelStatus )
      WITH VALUE #( FOR key IN keys ( %tky = key-%tky
                                      TravelStatus = gc_s_travel_status-accepted ) )
    FAILED failed
    REPORTED reported.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    result = VALUE #( FOR <lv_travel> IN lt_travels
                        (  %tky = <lv_travel>-%tky
                           %param = <lv_travel> ) ).

  ENDMETHOD.

  METHOD recalcTotalPrice.

    TYPES: BEGIN OF lty_s_amount_per_currencycode,
             amount        TYPE /dmo/total_price,
             currency_code TYPE /dmo/currency_code,
           END OF lty_s_amount_per_currencycode.

    DATA: lt_amount_per_currencycode TYPE STANDARD TABLE OF lty_s_amount_per_currencycode.

    " Read all relevant travel instances.
    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
          ENTITY Travel
             FIELDS ( BookingFee CurrencyCode )
             WITH CORRESPONDING #( keys )
          RESULT DATA(lt_travels).

    DELETE lt_travels WHERE CurrencyCode IS INITIAL.

    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travels>).
      " Set the start for the calculation by adding the booking fee.
      lt_amount_per_currencycode = VALUE #( ( amount        = <ls_travels>-BookingFee
                                            currency_code = <ls_travels>-CurrencyCode ) ).
      " Read all associated bookings and add them to the total price.
      READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
         ENTITY Travel BY \_Booking
            FIELDS ( FlightPrice CurrencyCode )
          WITH VALUE #( ( %tky = <ls_travels>-%tky ) )
          RESULT DATA(lt_bookings).
      LOOP AT lt_bookings ASSIGNING FIELD-SYMBOL(<ls_booking>)
        WHERE CurrencyCode IS NOT INITIAL.

        COLLECT VALUE lty_s_amount_per_currencycode( amount        = <ls_booking>-FlightPrice
                                                     currency_code = <ls_booking>-CurrencyCode ) INTO lt_amount_per_currencycode.
      ENDLOOP.

      CLEAR <ls_travels>-TotalPrice.
      LOOP AT lt_amount_per_currencycode ASSIGNING FIELD-SYMBOL(<ls_amount_per_currencycode>).
        " If needed do a Currency Conversion
        IF <ls_amount_per_currencycode>-currency_code = <ls_travels>-CurrencyCode.
          <ls_travels>-TotalPrice += <ls_amount_per_currencycode>-amount.
        ELSE.
          /dmo/cl_flight_amdp=>convert_currency(
             EXPORTING
               iv_amount                   =  <ls_amount_per_currencycode>-amount
               iv_currency_code_source     =  <ls_amount_per_currencycode>-currency_code
               iv_currency_code_target     =  <ls_travels>-CurrencyCode
               iv_exchange_rate_date       =  cl_abap_context_info=>get_system_date( )
             IMPORTING
               ev_amount                   = DATA(lv_tot_booking_price_per_curr) ).
          <ls_travels>-TotalPrice += lv_tot_booking_price_per_curr.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " write back the modified total_price of travels
    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY travel
        UPDATE FIELDS ( TotalPrice )
        WITH CORRESPONDING #( lt_travels ).

  ENDMETHOD.

  METHOD rejectTravel.

    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
    ENTITY Travel
    UPDATE FIELDS ( TravelStatus )
      WITH VALUE #( FOR <ls_key> IN keys ( %tky = <ls_key>-%tky
                                      TravelStatus = gc_s_travel_status-cancelled ) )
    FAILED failed
    REPORTED reported.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Travel
        ALL FIELDS WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    result = VALUE #( FOR <lv_travel> IN lt_travels
                        (  %tky = <lv_travel>-%tky
                           %param = <lv_travel> ) ).

  ENDMETHOD.

  METHOD calculateTotalPrice.

    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Travel
        EXECUTE recalcTotalPrice
        FROM CORRESPONDING #( keys )
        REPORTED DATA(lt_execute_reported).

    reported = CORRESPONDING #( DEEP lt_execute_reported ).

  ENDMETHOD.

  METHOD setInitialStatus.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelStatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    DELETE lt_travels WHERE TravelStatus IS NOT INITIAL.
    CHECK lt_travels IS NOT INITIAL.

    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
    ENTITY Travel
      UPDATE
        FIELDS ( TravelStatus )
       WITH VALUE #( FOR <ls_travels> IN lt_travels
                       ( %tky   = <ls_travels>-%tky
                         TravelStatus = gc_s_travel_status-open ) )
      REPORTED DATA(lt_update_reported).

    reported = CORRESPONDING #( DEEP lt_update_reported ).

  ENDMETHOD.

  METHOD calculateTravelID.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    DELETE lt_travels WHERE TravelID IS NOT INITIAL.
    CHECK lt_travels IS NOT INITIAL.

    SELECT SINGLE
      FROM zbc_rap_trav_007
      FIELDS MAX( travel_id ) AS travelID
      INTO @DATA(lv_max_travelid).

    MODIFY ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
    ENTITY Travel
      UPDATE
        FROM VALUE #( FOR <ls_travels> IN lt_travels INDEX INTO lv_i (
          %tky      = <ls_travels>-%tky
          TravelID  = lv_max_travelid + lv_i
          %control-TravelID = if_abap_behv=>mk-on ) )
      REPORTED DATA(lt_update_reported).

    reported = CORRESPONDING #( DEEP lt_update_reported ).

  ENDMETHOD.

  METHOD validateAgency.

    DATA:
      lt_agencies TYPE SORTED TABLE OF /dmo/agency WITH UNIQUE KEY agency_id.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
     ENTITY Travel
     FIELDS (  AgencyID ) WITH CORRESPONDING #( keys )
     RESULT DATA(lt_travels).

    lt_agencies = CORRESPONDING #( lt_travels DISCARDING DUPLICATES MAPPING agency_id = AgencyID EXCEPT * ).
    DELETE lt_agencies WHERE agency_id IS INITIAL.

*    IF lt_agencies IS NOT INITIAL.

*      SELECT FROM /dmo/agency
*        FIELDS agency_id
*        FOR ALL ENTRIES IN @lt_agencies
*        WHERE agency_id = @lt_agencies-agency_id
*        INTO TABLE @DATA(lt_agencies_db).

*    ENDIF.

*    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travels>).
*
*      APPEND VALUE #( %tky   = <ls_travels>-%tky
*                      %state_area = 'VALIDATE_AGENCY' )
*             TO reported-travel.
*
*      IF <ls_travels>-AgencyID IS INITIAL OR NOT line_exists( lt_agencies_db[ agency_id = <ls_travels>-AgencyID ] ).
*
*        APPEND VALUE #( %tky = <ls_travels>-%tky ) TO failed-travel.
*
*        APPEND VALUE #( %tky = <ls_travels>-%tky
*                        %state_area = 'VALIDATE_AGENCY'
*                        %msg = NEW zcx_bc_rap_travel_007( iv_severity = if_abap_behv_message=>severity-error
*                                                          iv_textid = zcx_bc_rap_travel_007=>gc_agency_unknown
*                                                          iv_agencyid = <ls_travels>-AgencyID )
*                        %element-AgencyID = if_abap_behv=>mk-on )
*          TO reported-travel.
*
*      ENDIF.
*    ENDLOOP.

    DATA filter_conditions  TYPE if_rap_query_filter=>tt_name_range_pairs .
    DATA ranges_table TYPE if_rap_query_filter=>tt_range_option .
    DATA business_data TYPE TABLE OF zbc_ce_rap_agency_007.
    IF lt_agencies IS NOT INITIAL.
      ranges_table = VALUE #( FOR agency IN lt_agencies (  sign = 'I' option = 'EQ' low = agency-agency_id ) ).
      filter_conditions = VALUE #( ( name = 'AGENCYID'  range = ranges_table ) ).
      TRY.
          "skip and top must not be used
          "but an appropriate filter will be provided
          NEW zcl_bc_ce_rap_agency_007( )->get_agencies(
             EXPORTING
               filter_cond    = filter_conditions
               is_data_requested  = abap_true
               is_count_requested = abap_false
             IMPORTING
               business_data  = business_data
             ) .
        CATCH /iwbep/cx_cp_remote
              /iwbep/cx_gateway
              cx_web_http_client_error
              cx_http_dest_provider_error INTO DATA(exception).
          DATA(exception_message) = cl_message_helper=>get_latest_t100_exception( exception )->if_message~get_text( ) .
          "Raise msg for problems calling the remote OData service
          LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travel>)
            WHERE AgencyID IN ranges_table.
              APPEND VALUE #( %tky = <ls_travel>-%tky ) TO failed-travel.
              APPEND VALUE #( %tky        = <ls_travel>-%tky
                              %state_area = 'VALIDATE_AGENCY'
                              %msg        =  new_message_with_text( severity = if_abap_behv_message=>severity-error text = exception_message )
                              %element-AgencyID = if_abap_behv=>mk-on )
              TO reported-travel.
          ENDLOOP.
          RETURN.
      ENDTRY.
    ENDIF.

    " Raise msg for non existing and initial agencyID
    LOOP AT lt_travels ASSIGNING <ls_travel>.

      IF <ls_travel>-AgencyID IS INITIAL OR NOT line_exists( business_data[ agencyid = <ls_travel>-AgencyID ] ).
        APPEND VALUE #( %tky = <ls_travel>-%tky ) TO failed-travel.
        APPEND VALUE #( %tky        = <ls_travel>-%tky
                        %state_area = 'VALIDATE_AGENCY'
                        %msg        = NEW zcx_bc_rap_travel_007(
                                            iv_severity = if_abap_behv_message=>severity-error
                                            iv_textid   = zcx_bc_rap_travel_007=>gc_agency_unknown
                                            iv_agencyid = <ls_travel>-AgencyID )
                        %element-AgencyID = if_abap_behv=>mk-on )
          TO reported-travel.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.

  METHOD validateCustomer.

    DATA:
       lt_customers TYPE SORTED TABLE OF /dmo/customer WITH UNIQUE KEY customer_id.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
     ENTITY Travel
     FIELDS ( CustomerID ) WITH CORRESPONDING #( keys )
     RESULT DATA(lt_travels).

    lt_customers = CORRESPONDING #( lt_travels DISCARDING DUPLICATES MAPPING customer_id = CustomerID EXCEPT * ).
    DELETE lt_customers WHERE customer_id IS INITIAL.

    IF lt_customers IS NOT INITIAL.

      SELECT FROM /dmo/customer
        FIELDS customer_id
        FOR ALL ENTRIES IN @lt_customers
        WHERE customer_id = @lt_customers-customer_id
        INTO TABLE @DATA(lt_customers_db).

    ENDIF.

    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travels>).

      APPEND VALUE #( %tky   = <ls_travels>-%tky
                      %state_area = 'VALIDATE_CUSTOMER' )
             TO reported-travel.

      IF <ls_travels>-CustomerID IS INITIAL OR NOT line_exists( lt_customers_db[ customer_id = <ls_travels>-CustomerID ] ).

        APPEND VALUE #( %tky = <ls_travels>-%tky ) TO failed-travel.

        APPEND VALUE #( %tky = <ls_travels>-%tky
                        %state_area = 'VALIDATE_CUSTOMER'
                        %msg = NEW zcx_bc_rap_travel_007( iv_severity = if_abap_behv_message=>severity-error
                                                          iv_textid = zcx_bc_rap_travel_007=>gc_customer_unknown
                                                          iv_customerid = <ls_travels>-CustomerID )
                        %element-CustomerId = if_abap_behv=>mk-on )
          TO reported-travel.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD validateDates.

    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelID BeginDate EndDate ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels).

    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travels>).

      APPEND VALUE #(  %tky   = <ls_travels>-%tky
                       %state_area = 'VALIDATE_DATES' )
        TO reported-travel.

      IF <ls_travels>-EndDate < <ls_travels>-BeginDate.

        APPEND VALUE #( %tky   = <ls_travels>-%tky ) TO failed-travel.
        APPEND VALUE #( %tky   = <ls_travels>-%tky
                        %state_area = 'VALIDATE_DATES'
                        %msg   = NEW zcx_bc_rap_travel_007(
                                       iv_severity = if_abap_behv_message=>severity-error
                                       iv_textid   = zcx_bc_rap_travel_007=>gc_date_interval
                                       iv_begindate = <ls_travels>-BeginDate
                                       iv_enddate  = <ls_travels>-EndDate
                                       iv_travelid = <ls_travels>-TravelID )
                       %element-begindate = if_abap_behv=>mk-on
                       %element-enddate = if_abap_behv=>mk-on ) TO reported-travel.

      ELSEIF <ls_travels>-BeginDate < cl_abap_context_info=>get_system_date( ).

        APPEND VALUE #( %tky   = <ls_travels>-%tky ) TO failed-travel.
        APPEND VALUE #( %tky   = <ls_travels>-%tky
                        %state_area = 'VALIDATE_DATES'
                        %msg   = NEW zcx_bc_rap_travel_007(
                                       iv_severity = if_abap_behv_message=>severity-error
                                       iv_textid   = zcx_bc_rap_travel_007=>gc_date_interval
                                       iv_begindate = <ls_travels>-BeginDate
                                       iv_enddate  = <ls_travels>-EndDate
                                       iv_travelid = <ls_travels>-TravelID )
                       %element-begindate = if_abap_behv=>mk-on ) TO reported-travel.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_instance_authorizations.

    DATA:
      lv_has_before_image    TYPE abap_bool,
      lv_is_update_requested TYPE abap_bool,
      lv_is_delete_requested TYPE abap_bool,
      lv_update_granted      TYPE abap_bool,
      lv_delete_granted      TYPE abap_bool.

    DATA: ls_failed_travel LIKE LINE OF failed-travel.

    " Read the existing travels
    READ ENTITIES OF zbc_i_rap_travel_007 IN LOCAL MODE
      ENTITY Travel
        FIELDS ( TravelStatus ) WITH CORRESPONDING #( keys )
      RESULT DATA(lt_travels)
      FAILED failed.

    CHECK lt_travels IS NOT INITIAL.

*   In this example the authorization is defined based on the Activity + Travel Status
*   For the Travel Status we need the before-image from the database. We perform this for active (is_draft=00) as well as for drafts (is_draft=01) as we can't distinguish between edit or new drafts
    SELECT FROM zbc_rap_trav_007
      FIELDS travel_uuid, overall_status
      FOR ALL ENTRIES IN @lt_travels
      WHERE travel_uuid EQ @lt_travels-TravelUUID
      ORDER BY PRIMARY KEY
      INTO TABLE @DATA(lt_travels_before_image).

    lv_is_update_requested = COND #( WHEN requested_authorizations-%update              = if_abap_behv=>mk-on OR
                                          requested_authorizations-%action-acceptTravel = if_abap_behv=>mk-on OR
                                          requested_authorizations-%action-rejectTravel = if_abap_behv=>mk-on OR
                                          requested_authorizations-%action-Prepare      = if_abap_behv=>mk-on OR
                                          requested_authorizations-%action-Edit         = if_abap_behv=>mk-on OR
                                          requested_authorizations-%assoc-_Booking      = if_abap_behv=>mk-on
                                     THEN abap_true
                                     ELSE abap_false ).

    lv_is_delete_requested = COND #( WHEN requested_authorizations-%delete = if_abap_behv=>mk-on
                                    THEN abap_true ELSE abap_false ).

    LOOP AT lt_travels ASSIGNING FIELD-SYMBOL(<ls_travels>).

      lv_update_granted = lv_delete_granted = abap_false.

      READ TABLE lt_travels_before_image ASSIGNING FIELD-SYMBOL(<ls_travels_before_image>)
           WITH KEY travel_uuid = <ls_travels>-TravelUUID BINARY SEARCH.
      lv_has_before_image = COND #( WHEN sy-subrc = 0
                                    THEN abap_true
                                    ELSE abap_false ).

      IF lv_is_update_requested = abap_true.
        " Edit of an existing record -> check update authorization
        IF lv_has_before_image = abap_true.
          lv_update_granted = is_update_granted( iv_has_before_image = lv_has_before_image
                                                 iv_overall_status = <ls_travels_before_image>-overall_status ).
          IF lv_update_granted = abap_false.
            APPEND VALUE #( %tky        = <ls_travels>-%tky
                            %msg        = NEW zcx_bc_rap_travel_007( iv_severity = if_abap_behv_message=>severity-error
                                                                     iv_textid   = zcx_bc_rap_travel_007=>gc_unauthorized )
                          ) TO reported-travel.
          ENDIF.
          " Creation of a new record -> check create authorization
        ELSE.
          lv_update_granted = is_create_granted( ).
          IF lv_update_granted = abap_false.
            APPEND VALUE #( %tky        = <ls_travels>-%tky
                            %msg        = NEW zcx_bc_rap_travel_007( iv_severity = if_abap_behv_message=>severity-error
                                                                     iv_textid   = zcx_bc_rap_travel_007=>gc_unauthorized )
                          ) TO reported-travel.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_is_delete_requested = abap_true.
        lv_delete_granted = is_delete_granted( iv_has_before_image = lv_has_before_image
                                               iv_overall_status = COND #( WHEN <ls_travels_before_image> IS ASSIGNED
                                                                             THEN <ls_travels_before_image>-overall_status
                                                                             ELSE ' ' ) ).
        IF lv_delete_granted = abap_false.
          APPEND VALUE #( %tky        = <ls_travels>-%tky
                          %msg        = NEW zcx_bc_rap_travel_007( iv_severity = if_abap_behv_message=>severity-error
                                                                   iv_textid   = zcx_bc_rap_travel_007=>gc_unauthorized )
                        ) TO reported-travel.
        ENDIF.
      ENDIF.

      APPEND VALUE #( %tky = <ls_travels>-%tky

                      %update              = COND #( WHEN lv_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-acceptTravel = COND #( WHEN lv_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-rejectTravel = COND #( WHEN lv_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-Prepare      = COND #( WHEN lv_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %action-Edit         = COND #( WHEN lv_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                      %assoc-_Booking      = COND #( WHEN lv_update_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )

                      %delete              = COND #( WHEN lv_delete_granted = abap_true THEN if_abap_behv=>auth-allowed ELSE if_abap_behv=>auth-unauthorized )
                    )
        TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD is_create_granted.

    AUTHORITY-CHECK OBJECT 'ZBC_OSTAT'
         ID 'ZBC_OSTAT' DUMMY
         ID 'ACTVT' FIELD '01'.

    rv_create_granted = COND #( WHEN sy-subrc = 0
                                THEN abap_true
                                ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    rv_create_granted = abap_true.

  ENDMETHOD.

  METHOD is_delete_granted.

    IF iv_has_before_image = abap_true.
      AUTHORITY-CHECK OBJECT 'ZBC_OSTAT'
        ID 'ZBC_OSTAT ' FIELD iv_overall_status
        ID 'ACTVT' FIELD '06'.
    ELSE.
      AUTHORITY-CHECK OBJECT 'ZBC_OSTAT '
        ID 'ZBC_OSTAT' DUMMY
        ID 'ACTVT' FIELD '06'.
    ENDIF.

    rv_delete_granted = COND #( WHEN sy-subrc = 0
                                THEN abap_true
                                ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    rv_delete_granted = abap_true.

  ENDMETHOD.

  METHOD is_update_granted.

    IF iv_has_before_image = abap_true.
      AUTHORITY-CHECK OBJECT 'ZBC_OSTAT'
        ID 'ZBC_OSTAT ' FIELD iv_overall_status
        ID 'ACTVT' FIELD '02'.
    ELSE.
      AUTHORITY-CHECK OBJECT 'ZBC_OSTAT '
        ID 'ZBC_OSTAT' DUMMY
        ID 'ACTVT' FIELD '02'.
    ENDIF.

    rv_update_granted = COND #( WHEN sy-subrc = 0
                                THEN abap_true
                                ELSE abap_false ).

    " Simulate full access - for testing purposes only! Needs to be removed for a productive implementation.
    rv_update_granted = abap_true.

  ENDMETHOD.

ENDCLASS.
