CLASS zcl_bc_rap_eml_007 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_bc_rap_eml_007 IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

*    READ ENTITIES OF zbc_i_rap_travel_007
*      ENTITY Travel
*      FROM VALUE #( ( TravelUUID = '9BA55360B4EFB944180076A6C57DDD4A' ) )
*      RESULT DATA(lt_travel).

*    READ ENTITIES OF zbc_i_rap_travel_007
*      ENTITY Travel
*        FIELDS ( AgencyID CustomerID )
*        WITH VALUE #( ( TravelUUID = '9BA55360B4EFB944180076A6C57DDD4A' ) )
*      RESULT DATA(lt_travel).

*    READ ENTITIES OF zbc_i_rap_travel_007
*      ENTITY Travel
*        ALL FIELDS WITH VALUE #( ( TravelUUID = '9BA55360B4EFB944180076A6C57DDD4A' ) )
*      RESULT DATA(lt_travel).

*    READ ENTITIES OF zbc_i_rap_travel_007
*      ENTITY Travel by \_Booking
*        ALL FIELDS WITH VALUE #( ( TravelUUID = '9BA55360B4EFB944180076A6C57DDD4A' ) )
*      RESULT DATA(lt_travel).

*    READ ENTITIES OF zbc_i_rap_travel_007
*      ENTITY Travel BY \_Booking
*        ALL FIELDS WITH VALUE #( ( TravelUUID = '111111111111111111111111111111' ) )
*      RESULT DATA(lt_travel)
*      FAILED DATA(lt_failed)
*      REPORTED DATA(lt_reported).
*
*    out->write( lt_travel ).
*    out->write( lt_failed ).
*    out->write( lt_reported ).

*    MODIFY ENTITIES OF zbc_i_rap_travel_007
*      ENTITY Travel
*      UPDATE SET FIELDS WITH VALUE
*         #( ( TravelUUID = '9BA55360B4EFB944180076A6C57DDD4A'
*              Description = 'Holy Cow what a torture!' ) )
*      FAILED DATA(lt_failed)
*      REPORTED DATA(lt_reported).

*    MODIFY ENTITIES OF zbc_i_rap_travel_007
*      ENTITY Travel
*      CREATE SET FIELDS WITH VALUE
*        #(  (  %cid = 'My Content ID_1'
*               AgencyID = '70012'
*               CustomerID = '14'
*               BeginDate = cl_abap_context_info=>get_system_date(  )
*               EndDate   = cl_abap_context_info=>get_system_date(  ) + 10
*               Description = 'Holy Cow Im going mad!' ) )
*
*      MAPPED DATA(lt_mapped)
*      FAILED DATA(lt_failed)
*      REPORTED DATA(lt_reported).
*
*    out->write( lt_mapped-travel ).

*VALUE #(
*( TravelUUID = '96B55360B4EFB944180076A6C57DDD4A' TravelID = '00000017' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000478' CustomerName = 'Sessler' BeginDate = '20220924' EndDate = '20230723' BookingFee = '120.00 '
*TotalPrice = '6087.00 ' CurrencyCode = 'USD' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Delon' CreatedAt = '20220823020208.0000000 ' LastChangedBy = 'CB9980004878' LastChangedAt = '20221206123531.1927760 '
*LocalLastChangedAt = '20221206123531.1927760 '  )
*( TravelUUID = '6DA55360B4EFB944180076A6C57DDD4A' TravelID = '00000025' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000478' CustomerName = 'Sessler' BeginDate = '20220924' EndDate = '20230723' BookingFee = '40.00 '
*TotalPrice = '2213.00 ' CurrencyCode = 'USD' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Moyano' CreatedAt = '20220907094548.0000000 ' LastChangedBy = 'D´Oultrement' LastChangedAt = '20220927164255.0000000 '
*LocalLastChangedAt = '20220927164255.0000000 '  )
*( TravelUUID = '7BA55360B4EFB944180076A6C57DDD4A' TravelID = '00000039' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000027' CustomerName = 'Jeremias' BeginDate = '20230721' EndDate = '20230723' BookingFee = '60.00 '
*TotalPrice = '3601.00 ' CurrencyCode = 'USD' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Müller' CreatedAt = '20221119111557.0000000 ' LastChangedBy = 'Matthaeus' LastChangedAt = '20221209221150.0000000 '
*LocalLastChangedAt = '20221209221150.0000000 '  )
*( TravelUUID = 'A1A55360B4EFB944180076A6C57DDD4A' TravelID = '00000077' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000409' CustomerName = 'Illner' BeginDate = '20220926' EndDate = '20220926' BookingFee = '20.00 '
*TotalPrice = '1868.00 ' CurrencyCode = 'USD' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Wohl' CreatedAt = '20220828205602.0000000 ' LastChangedBy = 'Kirk' LastChangedAt = '20220916045418.0000000 '
*LocalLastChangedAt = '20220916045418.0000000 '  )
*( TravelUUID = 'C5A55360B4EFB944180076A6C57DDD4A' TravelID = '00000113' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000190' CustomerName = 'Goelke' BeginDate = '20220920' EndDate = '20230719' BookingFee = '50.00 '
*TotalPrice = '14315.00 ' CurrencyCode = 'EUR' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Hoffen' CreatedAt = '20220827125821.0000000 ' LastChangedBy = 'Babilon' LastChangedAt = '20220916055443.0000000 '
*LocalLastChangedAt = '20220916055443.0000000 '  )
*( TravelUUID = 'D2A55360B4EFB944180076A6C57DDD4A' TravelID = '00000126' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000622' CustomerName = 'Pan' BeginDate = '20220920' EndDate = '20230719' BookingFee = '100.00 '
*TotalPrice = '28709.00 ' CurrencyCode = 'EUR' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Picard' CreatedAt = '20220827163213.0000000 ' LastChangedBy = 'Moyano' LastChangedAt = '20220915205736.0000000 '
*LocalLastChangedAt = '20220915205736.0000000 '  )
*( TravelUUID = 'D7A55360B4EFB944180076A6C57DDD4A' TravelID = '00000131' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000175' CustomerName = 'Dumbach' BeginDate = '20220920' EndDate = '20230719' BookingFee = '100.00 '
*TotalPrice = '29350.00 ' CurrencyCode = 'EUR' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Delon' CreatedAt = '20220819175357.0000000 ' LastChangedBy = 'Sudhoff' LastChangedAt = '20220908100253.0000000 '
*LocalLastChangedAt = '20220908100253.0000000 '  )
*( TravelUUID = 'F0A55360B4EFB944180076A6C57DDD4A' TravelID = '00000156' AgencyID = '070048' AgencyName = 'Rocky Horror Tours' CustomerID = '000295' CustomerName = 'Legrand' BeginDate = '20220920' EndDate = '20230719' BookingFee = '150.00 '
*TotalPrice = '47464.00 ' CurrencyCode = 'EUR' Description = 'Vacation' TravelStatus = 'O' CreatedBy = 'Matthaeus' CreatedAt = '20220820133156.0000000 ' LastChangedBy = 'Illner' LastChangedAt = '20220909113925.0000000 '
*LocalLastChangedAt = '20220909113925.0000000 '  )
* )

    MODIFY ENTITIES OF zbc_i_rap_travel_007
      ENTITY Travel
      DELETE FROM VALUE #( ( TravelUUID = '9BA55360B4EFB944180076A6C57DDD4A' ) )

      FAILED DATA(lt_failed)
      REPORTED DATA(lt_reported).

    COMMIT ENTITIES
    RESPONSE OF zbc_i_rap_travel_007
    FAILED DATA(lt_failed_commit)
    REPORTED DATA(lt_reported_commit).

    out->write( 'Delete done' ).

  ENDMETHOD.

ENDCLASS.
