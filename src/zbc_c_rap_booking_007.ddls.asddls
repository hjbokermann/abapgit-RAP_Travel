@EndUserText.label: 'Booking BO projection view'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Search.searchable: true
@Metadata.allowExtensions: true

define view entity zbc_c_rap_booking_007
  as projection on zbc_i_rap_booking_007 as Booking
{
   key BookingUUID,
       TravelUUID,
       @Search.defaultSearchElement: true
       BookingID,
       BookingDate,
       @Consumption.valueHelpDefinition: [{ entity : {name: '/DMO/I_Customer', element: 'CustomerID'  } }]
       @ObjectModel.text.element: ['CustomerName']
       @Search.defaultSearchElement: true
       CustomerID,
       _Customer.LastName as CustomerName,
       @Consumption.valueHelpDefinition: [{entity: {name: '/DMO/I_Carrier', element: 'AirlineID' }}]
       @ObjectModel.text.element: ['CarrierName']
       CarrierID,
       _Carrier.Name      as CarrierName,
       @Consumption.valueHelpDefinition: [ {entity: {name: '/DMO/I_Flight', element: 'ConnectionID'},
                                            additionalBinding: [ { localElement: 'CarrierID',    element: 'AirlineID' },
                                                                 { localElement: 'FlightDate',   element: 'FlightDate',   usage: #RESULT},
                                                                 { localElement: 'FlightPrice',  element: 'Price',        usage: #RESULT },
                                                                 { localElement: 'CurrencyCode', element: 'CurrencyCode', usage: #RESULT } ] } ]
       ConnectionID,
       FlightDate,
       @Semantics.amount.currencyCode: 'CurrencyCode'
       FlightPrice,
       @Consumption.valueHelpDefinition: [{entity: {name: 'I_Currency', element: 'Currency' }}]
       CurrencyCode,
       LocalLastChangedAt,

       /* associations */
       _Travel : redirected to parent zbc_c_rap_travel_007,
       _Customer,
       _Carrier,
       _Connection,
       _Flight
}
