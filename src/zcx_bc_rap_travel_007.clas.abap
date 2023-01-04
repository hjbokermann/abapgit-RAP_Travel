CLASS zcx_bc_rap_travel_007 DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .
    INTERFACES if_abap_behv_message.

    CONSTANTS:
      BEGIN OF gc_date_interval,
        msgid TYPE symsgid VALUE 'ZBC_RAP_TRAVEL_007',
        msgno TYPE symsgno VALUE '001',
        attr1 TYPE scx_attrname VALUE 'BEGINDATE',
        attr2 TYPE scx_attrname VALUE 'ENDDATE',
        attr3 TYPE scx_attrname VALUE 'TRAVELID',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_date_interval .
    CONSTANTS:
      BEGIN OF gc_begin_date_before_sys_date,
        msgid TYPE symsgid VALUE 'ZBC_RAP_TRAVEL_007',
        msgno TYPE symsgno VALUE '002',
        attr1 TYPE scx_attrname VALUE 'BEGINDATE',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_begin_date_before_sys_date .
    CONSTANTS:
      BEGIN OF gc_customer_unknown,
        msgid TYPE symsgid VALUE 'ZBC_RAP_TRAVEL_007',
        msgno TYPE symsgno VALUE '003',
        attr1 TYPE scx_attrname VALUE 'CUSTOMERID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_customer_unknown .
    CONSTANTS:
      BEGIN OF gc_agency_unknown,
        msgid TYPE symsgid VALUE 'ZBC_RAP_TRAVEL_007',
        msgno TYPE symsgno VALUE '004',
        attr1 TYPE scx_attrname VALUE 'AGENCYID',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_agency_unknown .
    CONSTANTS:
      BEGIN OF gc_unauthorized,
        msgid TYPE symsgid VALUE 'ZBC_RAP_TRAVEL_007',
        msgno TYPE symsgno VALUE '005',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF gc_unauthorized .


    METHODS constructor
      IMPORTING
        iv_severity   TYPE if_abap_behv_message=>t_severity DEFAULT if_abap_behv_message=>severity-error
        iv_textid     LIKE if_t100_message=>t100key OPTIONAL
        iv_previous   TYPE REF TO cx_root OPTIONAL
        iv_begindate  TYPE /dmo/begin_date OPTIONAL
        iv_enddate    TYPE /dmo/end_date OPTIONAL
        iv_travelid   TYPE /dmo/travel_id OPTIONAL
        iv_customerid TYPE /dmo/customer_id OPTIONAL
        iv_agencyid   TYPE /dmo/agency_id  OPTIONAL.


    DATA: mv_begindate  TYPE /dmo/begin_date READ-ONLY,
          mv_enddate    TYPE /dmo/end_date READ-ONLY,
          mv_travelid   TYPE string READ-ONLY,
          mv_customerid TYPE string READ-ONLY,
          mv_agencyid   TYPE string READ-ONLY.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_bc_rap_travel_007 IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = iv_textid.
    ENDIF.

    if_abap_behv_message~m_severity = iv_severity.
    mv_begindate = iv_begindate.
    mv_enddate = iv_enddate.
    mv_travelid = |{ iv_travelid ALPHA = OUT }|.
    mv_customerid = |{ iv_customerid ALPHA = OUT }|.
    mv_agencyid = |{ iv_agencyid ALPHA = OUT }|.

  ENDMETHOD.
ENDCLASS.
