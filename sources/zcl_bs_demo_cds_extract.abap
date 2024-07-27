CLASS zcl_bs_demo_cds_extract DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

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

    CONSTANTS c_url_cloudification_repo TYPE string VALUE `https://raw.githubusercontent.com/SAP/abap-atc-cr-cv-s4hc/main/src/objectReleaseInfoLatest.json`.
    CONSTANTS c_url_mappings            TYPE string VALUE `https://raw.githubusercontent.com/Xexer/abap-cds-field-mapping/main/mapping/core-data-services.json`.

    METHODS get_table_fields_from_cds
      IMPORTING id_cds           TYPE sxco_cds_object_name
                id_table         TYPE string
                id_toupper       TYPE abap_bool
      RETURNING VALUE(rt_result) TYPE tt_mapping.

    METHODS get_field_value
      IMPORTING io_expression    TYPE REF TO if_xco_ddl_expression
      RETURNING VALUE(rd_result) TYPE string.

    METHODS find_table_field
      IMPORTING it_mapping       TYPE tt_mapping
                is_source        TYPE ts_mapping
                id_table         TYPE string
      RETURNING VALUE(rd_result) TYPE string.

    METHODS cleanup_field
      IMPORTING id_field         TYPE string
                is_content       TYPE if_xco_cds_view_content=>ts_content
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
*    ls_fixed = VALUE #( cds   = 'I_ACADEMICTITLE'
*                        table = 'TSAD2' ).

    DATA(ld_json) = get_mapping_in_json_format( it_r_cds   = lt_r_cds
                                                it_r_table = lt_r_table
                                                is_fixed   = ls_fixed ).
    out->write( ld_json ).
  ENDMETHOD.


  METHOD get_mapping_in_json_format.
    DATA(lt_repo) = get_mapping_as_table( it_r_cds   = it_r_cds
                                          it_r_table = it_r_table
                                          is_fixed   = is_fixed ).

    RETURN /ui2/cl_json=>serialize( data          = lt_repo
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
            IF     ls_mapping-mapping            IS NOT INITIAL
               AND ls_successor-tadirobjname NOT IN it_r_cds
               AND ls_object-tadirobjname NOT    IN it_r_table.
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
    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_url( id_url ).
        DATA(lo_client) = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        DATA(lo_response) = lo_client->execute( i_method = if_web_http_client=>get ).

      CATCH cx_root.
        RETURN.
    ENDTRY.

    IF lo_response->get_status( )-code = 200.
      rd_result = lo_response->get_text( ).
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

    DATA(ls_view_entity_content) = lo_cds->content( )->get( ).

    LOOP AT lo_cds->fields->all->get( ) INTO DATA(lo_field).
      DATA(ls_field) = lo_field->content( )->get( ).
      DATA(ld_field) = ls_field-alias.

      IF ld_field IS INITIAL.
        ld_field = ls_field-original_name.
      ENDIF.

      IF id_toupper = abap_true.
        ld_field = to_upper( ld_field ).
      ENDIF.

      DATA(ld_new_field) = get_field_value( ls_field-expression ).

      INSERT VALUE #( cds         = id_cds
                      cds_field   = ld_field
                      table       = to_upper( ls_view_entity_content-data_source-entity )
                      table_field = cleanup_field( id_field   = ld_new_field
                                                   is_content = ls_view_entity_content ) )
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
      RETURN ``.
    ENDIF.

    DATA(lo_strings) = io_expression->if_xco_text~get_lines( ).
    DATA(lt_strings) = lo_strings->value.
    DATA(ld_result) = ``.

    IF line_exists( lt_strings[ 1 ] ).
      ld_result = lt_strings[ 1 ].
    ENDIF.

*      IF ld_result CS `concat(`.
*        BREAK-POINT.
*      ENDIF.

    IF ld_result CS `cast(`.
      SPLIT ld_result AT ` ` INTO TABLE DATA(lt_split).
      RETURN lt_split[ 2 ].
    ENDIF.

    IF ld_result CS `case(`.
      RETURN ``.
    ENDIF.

    RETURN ld_result.
  ENDMETHOD.


  METHOD find_table_field.
    DATA(ls_actual) = is_source.
    DATA(ld_last_field) = ls_actual-table_field.

    DO.
      IF ls_actual-table = id_table.
        RETURN ls_actual-table_field.
      ENDIF.

      TRY.
          ls_actual = it_mapping[ cds       = ls_actual-table
                                  cds_field = ls_actual-table_field ].
        CATCH cx_sy_itab_line_not_found.
          RETURN ld_last_field.
      ENDTRY.

      IF ls_actual-table_field IS NOT INITIAL.
        ld_last_field = ls_actual-table_field.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD cleanup_field.
    DATA(ld_field) = replace_alias( id_field   = id_field
                                    is_content = is_content ).

    ld_field = to_upper( ld_field ).

    IF ld_field CS `ABS(`.
      ld_field = replace( val  = ld_field
                          sub  = `ABS(`
                          with = `` ).
      ld_field = replace( val  = ld_field
                          sub  = `)`
                          with = `` ).
    ENDIF.

    IF ld_field CS `'`.
      CLEAR ld_field.
    ENDIF.

    RETURN ld_field.
  ENDMETHOD.


  METHOD replace_alias.
    DATA(ld_field) = id_field.

    IF NOT ld_field CS `.`.
      RETURN ld_field.
    ENDIF.

    DATA(lt_replace) = VALUE string_table( ( CONV #( is_content-data_source-alias ) )
                                           ( CONV #( is_content-data_source-entity ) ) ).

    LOOP AT lt_replace INTO DATA(ld_replace) WHERE table_line IS NOT INITIAL.
      DATA(ld_new) = replace( val  = ld_field
                              sub  = |{ ld_replace }.|
                              with = `` ).
      IF ld_field <> ld_new.
        RETURN ld_new.
      ENDIF.
    ENDLOOP.

    RETURN ``.
  ENDMETHOD.
ENDCLASS.