CLASS zcl_bs_demo_cds_extract DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES td_reason TYPE c LENGTH 15.

    TYPES: BEGIN OF ts_object,
             cds   TYPE sxco_cds_object_name,
             table TYPE string,
           END OF ts_object.
    TYPES tt_object TYPE STANDARD TABLE OF ts_object WITH EMPTY KEY.

    TYPES: BEGIN OF ts_mapping,
             cds         TYPE sxco_cds_object_name,
             cds_field   TYPE string,
             table       TYPE string,
             table_field TYPE string,
           END OF ts_mapping.
    TYPES tt_mapping TYPE STANDARD TABLE OF ts_mapping WITH EMPTY KEY.

    TYPES: BEGIN OF ts_repo,
             cds_name   TYPE sxco_cds_object_name,
             table_name TYPE string,
             mapping    TYPE tt_mapping,
           END OF ts_repo.
    TYPES tt_repo    TYPE STANDARD TABLE OF ts_repo WITH EMPTY KEY.

    TYPES tt_r_cds   TYPE RANGE OF sxco_cds_object_name.
    TYPES tt_r_table TYPE RANGE OF string.

    CONSTANTS: BEGIN OF cs_reason,
                 fixed_value          TYPE td_reason VALUE 'FIXED',
                 unknown_query        TYPE td_reason VALUE 'UNKNOWN',
                 unsupported_function TYPE td_reason VALUE 'UNSUPPORTED',
                 content_error        TYPE td_reason VALUE 'CONTENT_ERROR',
               END OF cs_reason.

    "! Get Mapping in JSON format for output
    "! @parameter it_r_cds   | Filter for CDS
    "! @parameter it_r_table | Filter for Table
    "! @parameter is_fixed   | Read only the fixed value
    "! @parameter rd_result  | JSON String
    METHODS get_mapping_in_json_format
      IMPORTING it_r_cds         TYPE tt_r_cds   OPTIONAL
                it_r_table       TYPE tt_r_table OPTIONAL
                is_fixed         TYPE ts_object  OPTIONAL
      RETURNING VALUE(rd_result) TYPE string.

    "! Get mapping in table format (ABAP)
    "! @parameter it_r_cds   | Filter for CDS
    "! @parameter it_r_table | Filter for Table
    "! @parameter is_fixed   | Read only the fixed value
    "! @parameter rt_result  | Table with objects
    METHODS get_mapping_as_table
      IMPORTING it_r_cds         TYPE tt_r_cds   OPTIONAL
                it_r_table       TYPE tt_r_table OPTIONAL
                is_fixed         TYPE ts_object  OPTIONAL
      RETURNING VALUE(rt_result) TYPE tt_repo.

    "! Read Cloudification Repository for objects
    "! @parameter it_r_cds   | Filter for CDS
    "! @parameter it_r_table | Filter for Table
    "! @parameter rt_result  | New Core Data Services
    METHODS get_relevant_objs_from_repo
      IMPORTING it_r_cds         TYPE tt_r_cds
                it_r_table       TYPE tt_r_table
      RETURNING VALUE(rt_result) TYPE tt_object.

    "! Create mapping CDS to TABLE
    "! @parameter id_cds    | Name of Core Data Service
    "! @parameter id_table  | Name of the core table
    "! @parameter rt_result | Mapping between the two objects
    METHODS get_mapping_from_cds
      IMPORTING id_cds           TYPE sxco_cds_object_name
                id_table         TYPE string
      RETURNING VALUE(rt_result) TYPE tt_mapping.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_successor,
             tadirobject  TYPE string,
             tadirobjname TYPE string,
           END OF ts_successor.
    TYPES tt_successor TYPE STANDARD TABLE OF ts_successor WITH EMPTY KEY.

    TYPES: BEGIN OF ts_object_info,
             tadirobject  TYPE string,
             tadirobjname TYPE string,
             successors   TYPE tt_successor,
           END OF ts_object_info.
    TYPES tt_object_info TYPE STANDARD TABLE OF ts_object_info WITH EMPTY KEY.

    TYPES: BEGIN OF ts_cr,
             formatversion     TYPE string,
             objectreleaseinfo TYPE tt_object_info,
           END OF ts_cr.

    TYPES: BEGIN OF ts_function,
             name    TYPE string,
             content TYPE string,
             full    TYPE string,
           END OF ts_function.
    TYPES tt_function TYPE STANDARD TABLE OF ts_function WITH EMPTY KEY.

    CONSTANTS c_url_cloudification_repo TYPE string VALUE `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectReleaseInfoLatest.json`.
    CONSTANTS c_url_mappings            TYPE string VALUE `https://raw.githubusercontent.com/Xexer/abap-cds-field-mapping/main/mapping/core-data-services.json`.

    CONSTANTS: BEGIN OF cs_supported_function,
                 case          TYPE string VALUE 'CASE',
                 abs           TYPE string VALUE 'ABS',
                 cast          TYPE string VALUE 'CAST',
                 div           TYPE string VALUE `DIVISION`,
                 lpad          TYPE string VALUE `LPAD`,
                 coalesce      TYPE string VALUE `COALESCE`,
                 round         TYPE string VALUE `ROUND`,
                 substring     TYPE string VALUE `SUBSTRING`,
                 data_dec      TYPE string VALUE `ABAP.DEC`,
                 data_char     TYPE string VALUE `ABAP.CHAR`,
                 dats_to_tstmp TYPE string VALUE `DATS_TIMS_TO_TSTMP`,
               END OF cs_supported_function.

    DATA mt_error TYPE string_table.

    METHODS get_table_fields_from_cds
      IMPORTING id_cds           TYPE sxco_cds_object_name
                id_table         TYPE string
                id_toupper       TYPE abap_bool
      RETURNING VALUE(rt_result) TYPE tt_mapping.

    METHODS get_field_value
      IMPORTING io_expression    TYPE REF TO if_xco_ddl_expression
                is_content       TYPE if_xco_cds_view_content=>ts_content
      RETURNING VALUE(rd_result) TYPE string.

    METHODS find_table_field
      IMPORTING it_mapping       TYPE tt_mapping
                is_source        TYPE ts_mapping
                id_table         TYPE string
      RETURNING VALUE(rd_result) TYPE string.

    METHODS replace_alias
      IMPORTING id_field         TYPE string
                is_content       TYPE if_xco_cds_view_content=>ts_content
      RETURNING VALUE(rd_result) TYPE string.

    METHODS read_cloudification_repository
      RETURNING VALUE(rs_result) TYPE ts_cr.

    METHODS read_mappings
      RETURNING VALUE(rt_result) TYPE tt_repo.

    METHODS get_content_form_url
      IMPORTING id_url           TYPE string
      RETURNING VALUE(rd_result) TYPE string.

    METHODS add_error
      IMPORTING id_reason TYPE td_reason
                id_value  TYPE any OPTIONAL.

    METHODS replace_cds_functions
      IMPORTING id_field         TYPE string
      RETURNING VALUE(rd_result) TYPE string.

    METHODS extract_functions
      IMPORTING id_field         TYPE string
      RETURNING VALUE(rt_result) TYPE tt_function.

    METHODS is_supported_function
      IMPORTING id_function      TYPE string
      RETURNING VALUE(rd_result) TYPE abap_bool.

    METHODS get_function_name
      IMPORTING id_start         TYPE i
                id_field         TYPE string
      RETURNING VALUE(rd_result) TYPE string.
ENDCLASS.


CLASS zcl_bs_demo_cds_extract IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA ls_fixed   TYPE ts_object.
    DATA lt_r_cds   TYPE tt_r_cds.
    DATA lt_r_table TYPE tt_r_table.

    " Read a specific entity from repository
*    INSERT VALUE #( sign   = 'I'
*                    option = 'EQ'
*                    low    = 'I_BUSINESSPARTNER' ) INTO TABLE lt_r_cds.

    " Read a fixed pair, without repository
    ls_fixed = VALUE #( cds   = 'I_WORKCENTER'
                        table = 'CRHD' ).

    DATA(ld_json) = get_mapping_in_json_format( it_r_cds   = lt_r_cds
                                                it_r_table = lt_r_table
                                                is_fixed   = ls_fixed ).
    out->write( ld_json ).

    IF mt_error IS NOT INITIAL.
      out->write( mt_error ).
    ENDIF.
  ENDMETHOD.


  METHOD get_mapping_in_json_format.
    DATA(lt_repo) = get_mapping_as_table( it_r_cds   = it_r_cds
                                          it_r_table = it_r_table
                                          is_fixed   = is_fixed ).

    rd_result = /ui2/cl_json=>serialize( data          = lt_repo
                                         pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                                         format_output = abap_true ).
  ENDMETHOD.


  METHOD get_mapping_as_table.
    DATA lt_objects TYPE tt_object.

    IF is_fixed IS NOT INITIAL.
      INSERT is_fixed INTO TABLE lt_objects.
    ELSE.
      lt_objects = get_relevant_objs_from_repo( it_r_cds   = it_r_cds
                                                it_r_table = it_r_table ).
    ENDIF.

    LOOP AT lt_objects INTO DATA(ls_object).
      INSERT VALUE #( cds_name   = ls_object-cds
                      table_name = ls_object-table
                      mapping    = get_mapping_from_cds( id_cds   = ls_object-cds
                                                         id_table = ls_object-table ) )
             INTO TABLE rt_result.
    ENDLOOP.

    DELETE rt_result WHERE mapping IS INITIAL.
  ENDMETHOD.


  METHOD get_relevant_objs_from_repo.
    DATA(ls_cr) = read_cloudification_repository( ).
    DATA(lt_mapping) = read_mappings( ).

    LOOP AT ls_cr-objectreleaseinfo INTO DATA(ls_object) WHERE tadirobject = 'TABL' AND tadirobjname IN it_r_table.
      LOOP AT ls_object-successors INTO DATA(ls_successor) WHERE tadirobject = 'DDLS' AND tadirobjname IN it_r_cds.
        TRY.
            DATA(ls_mapping) = lt_mapping[ cds_name   = ls_successor-tadirobjname
                                           table_name = ls_object-tadirobjname ].
            IF    ls_mapping-mapping IS NOT INITIAL
               OR (     ls_successor-tadirobjname NOT IN it_r_cds
                    AND ls_object-tadirobjname NOT    IN it_r_table ).
              CONTINUE.
            ENDIF.

          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        INSERT VALUE #( cds   = ls_successor-tadirobjname
                        table = ls_object-tadirobjname )
               INTO TABLE rt_result.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD read_cloudification_repository.
    /ui2/cl_json=>deserialize( EXPORTING json = get_content_form_url( c_url_cloudification_repo )
                               CHANGING  data = rs_result ).
  ENDMETHOD.


  METHOD read_mappings.
    /ui2/cl_json=>deserialize( EXPORTING json        = get_content_form_url( c_url_mappings )
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data        = rt_result ).
  ENDMETHOD.


  METHOD get_content_form_url.
    " BTP Connection
*    TRY.
*        DATA(lo_destination) = cl_http_destination_provider=>create_by_url( id_url ).
*        DATA(lo_client) = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
*        DATA(lo_response) = lo_client->execute( i_method = if_web_http_client=>get ).
*
*      CATCH cx_root.
*        RETURN.
*    ENDTRY.
*
*    IF lo_response->get_status( )-code = 200.
*      rd_result = lo_response->get_text( ).
*    ENDIF.

    " On-Premise Connection
    cl_http_client=>create_by_url( EXPORTING url    = id_url
                                   IMPORTING client = DATA(lo_client) ).

    lo_client->send( ).
    lo_client->receive( ).
    lo_client->response->get_status( IMPORTING code = DATA(ld_code) ).

    IF ld_code = 200.
      rd_result = lo_client->response->get_cdata( ).
    ENDIF.
  ENDMETHOD.


  METHOD get_mapping_from_cds.
    DATA(lt_mapping) = get_table_fields_from_cds( id_cds     = id_cds
                                                  id_table   = id_table
                                                  id_toupper = abap_false ).

    LOOP AT lt_mapping INTO DATA(ls_mapping) WHERE cds = id_cds AND cds_field IS NOT INITIAL.
      DATA(ld_table) = id_table.
      DATA(ld_field) = find_table_field( it_mapping = lt_mapping
                                         is_source  = ls_mapping
                                         id_table   = id_table ).

      IF ld_field IS INITIAL.
        CLEAR ld_table.
      ENDIF.

      INSERT VALUE #( cds         = ls_mapping-cds
                      cds_field   = ls_mapping-cds_field
                      table       = ld_table
                      table_field = ld_field )
             INTO TABLE rt_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_table_fields_from_cds.
    DATA(lo_cds) = xco_cp_cds=>view( id_cds ).

    IF NOT lo_cds->exists( ).
      RETURN.
    ENDIF.

    TRY.
        DATA(ls_view_entity_content) = lo_cds->content( )->get( ).
      CATCH cx_root.
        add_error( id_reason = cs_reason-content_error
                   id_value  = id_cds ).
    ENDTRY.

    LOOP AT lo_cds->fields->all->get( ) INTO DATA(lo_field).
      DATA(ls_field) = lo_field->content( )->get( ).
      DATA(ld_field) = ls_field-alias.

      IF ld_field IS INITIAL.
        ld_field = ls_field-original_name.
      ENDIF.

      IF id_toupper = abap_true.
        ld_field = to_upper( ld_field ).
      ENDIF.

      DATA(ld_new_field) = get_field_value( io_expression = ls_field-expression
                                            is_content    = ls_view_entity_content ).

      INSERT VALUE #( cds         = id_cds
                      cds_field   = ld_field
                      table       = to_upper( ls_view_entity_content-data_source-entity )
                      table_field = ld_new_field )
             INTO TABLE rt_result.
    ENDLOOP.

    IF to_upper( ls_view_entity_content-data_source-entity ) = id_table OR ls_view_entity_content-data_source-entity IS INITIAL.
      RETURN.
    ELSE.
      INSERT LINES OF get_table_fields_from_cds( id_cds     = ls_view_entity_content-data_source-entity
                                                 id_table   = id_table
                                                 id_toupper = abap_true ) INTO TABLE rt_result.
    ENDIF.
  ENDMETHOD.


  METHOD get_field_value.
    IF io_expression IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_strings) = io_expression->if_xco_text~get_lines( ).
    DATA(ld_result) = concat_lines_of( table = lo_strings->value
                                       sep   = ` ` ).

    ld_result = replace_cds_functions( to_upper( ld_result ) ).

    IF ld_result CS cs_supported_function-case.
      CLEAR ld_result.
      RETURN.
    ENDIF.

    IF ld_result CS `(` OR ld_result CS `)`.
      add_error( id_reason = cs_reason-unknown_query
                 id_value  = ld_result ).
      CLEAR ld_result.
      RETURN.
    ENDIF.

    IF ld_result CS `'`.
      add_error( id_reason = cs_reason-fixed_value
                 id_value  = ld_result ).
      CLEAR ld_result.
      RETURN.
    ENDIF.

    ld_result = replace_alias( id_field   = ld_result
                               is_content = is_content ).

    rd_result = to_upper( ld_result ).
  ENDMETHOD.


  METHOD find_table_field.
    DATA(ls_actual) = is_source.
    DATA(ld_last_field) = ls_actual-table_field.

    DO.
      IF ls_actual-table = id_table.
        rd_result = ls_actual-table_field.
        RETURN.
      ENDIF.

      TRY.
          ls_actual = it_mapping[ cds       = ls_actual-table
                                  cds_field = ls_actual-table_field ].
        CATCH cx_sy_itab_line_not_found.
          rd_result = ld_last_field.
          RETURN.
      ENDTRY.

      IF ls_actual-table_field IS NOT INITIAL.
        ld_last_field = ls_actual-table_field.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD replace_alias.
    DATA(ld_field) = id_field.

    IF NOT ld_field CS `.`.
      rd_result = ld_field.
      RETURN.
    ENDIF.

    DATA(lt_replace) = VALUE string_table( ( CONV #( is_content-data_source-alias ) )
                                           ( CONV #( is_content-data_source-entity ) ) ).

    LOOP AT lt_replace INTO DATA(ld_replace) WHERE table_line IS NOT INITIAL.
      DATA(ld_new) = replace( val  = ld_field
                              sub  = |{ to_upper( ld_replace ) }.|
                              with = `` ).
      IF ld_field <> ld_new.
        rd_result = ld_new.
        RETURN.
      ENDIF.
    ENDLOOP.

    CLEAR rd_result.
  ENDMETHOD.


  METHOD add_error.
    IF id_value IS INITIAL.
      INSERT |{ id_reason }: No value| INTO TABLE mt_error.
    ELSE.
      INSERT |{ id_reason }: { id_value }| INTO TABLE mt_error.
    ENDIF.
  ENDMETHOD.


  METHOD replace_cds_functions.
    DATA lt_split TYPE string_table.

    rd_result = id_field.

    IF NOT rd_result CS `(` AND NOT rd_result CS `)`.
      RETURN.
    ENDIF.

    DATA(lt_function) = extract_functions( rd_result ).

    LOOP AT lt_function INTO DATA(ls_function) STEP -1.
      CASE ls_function-name.
        WHEN cs_supported_function-cast.
          SPLIT ls_function-content AT ` ` INTO TABLE lt_split.
          IF line_exists( lt_split[ 1 ] ).
            rd_result = lt_split[ 1 ].
            RETURN.
          ENDIF.

        WHEN cs_supported_function-lpad.
          SPLIT ls_function-content AT `,` INTO TABLE lt_split.
          IF line_exists( lt_split[ 1 ] ).
            rd_result = lt_split[ 1 ].
            RETURN.
          ENDIF.

        WHEN cs_supported_function-abs.
          rd_result = ls_function-content.
          RETURN.

      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD extract_functions.
    DATA(ld_field) = id_field.
    DATA(ld_counter) = 0.

    IF ld_field CS cs_supported_function-case.
      RETURN.
    ENDIF.

    WHILE ld_counter < 15.
      DATA(ld_start) = find( val = ld_field
                             sub = `(`
                             occ = 1 ).
      DATA(ld_end) = find( val = ld_field
                           sub = `)`
                           occ = -1 ).

      IF ld_start = -1 OR ld_end = -1.
        EXIT.
      ENDIF.

      DATA(ls_function) = VALUE ts_function( full    = ld_field
                                             content = substring( val = ld_field
                                                                  off = ld_start + 1
                                                                  len = ld_end - ld_start - 1 )
                                             name    = get_function_name( id_start = ld_start
                                                                          id_field = ld_field ) ).

      IF NOT is_supported_function( ls_function-name ).
        EXIT.
      ENDIF.

      INSERT ls_function INTO TABLE rt_result.
      ld_field = ls_function-content.
      ld_counter += 1.
    ENDWHILE.
  ENDMETHOD.


  METHOD get_function_name.
    DATA(lt_signs) = VALUE string_table( ( ` ` ) ( `,` ) ( `(` ) ).

    LOOP AT lt_signs INTO DATA(ld_sign).
      DATA(ld_space) = find( val = id_field
                             sub = ld_sign
                             off = id_start - 1
                             occ = -1 ).

      IF ld_space <> -1.
        rd_result = substring( val = id_field
                               off = ld_space + 1
                               len = id_start - ld_space - 1 ).
      ENDIF.
    ENDLOOP.

    IF rd_result IS INITIAL.
      rd_result = substring( val = id_field
                             len = id_start ).
    ENDIF.

    rd_result = replace( val  = rd_result
                         sub  = ` `
                         with = `` ).
  ENDMETHOD.


  METHOD is_supported_function.
    CASE id_function.
      WHEN cs_supported_function-abs OR cs_supported_function-cast OR cs_supported_function-div OR cs_supported_function-lpad.
        rd_result = abap_true.
      WHEN cs_supported_function-coalesce OR cs_supported_function-data_char OR cs_supported_function-data_dec
      OR cs_supported_function-dats_to_tstmp OR cs_supported_function-substring OR cs_supported_function-round.
        rd_result = abap_false.
      WHEN OTHERS.
        add_error( id_reason = cs_reason-unsupported_function
                   id_value  = id_function ).
        rd_result = abap_false.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.