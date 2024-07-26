CLASS zcl_bs_demo_cds_extract DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    TYPES: BEGIN OF ts_mapping,
             cds         TYPE sxco_cds_object_name,
             cds_field   TYPE string,
             table       TYPE string,
             table_field TYPE string,
           END OF ts_mapping.
    TYPES tt_mapping TYPE STANDARD TABLE OF ts_mapping WITH EMPTY KEY.

    METHODS get_mapping_from_cds
      IMPORTING id_cds           TYPE sxco_cds_object_name
                id_table         TYPE string
      RETURNING VALUE(rt_result) TYPE tt_mapping.

  PRIVATE SECTION.
    METHODS get_table_fields_from_cds
      IMPORTING id_cds           TYPE sxco_cds_object_name
                id_table         TYPE string
                id_toupper       TYPE abap_bool
      RETURNING VALUE(rt_result) TYPE tt_mapping.

    METHODS get_field_value
      IMPORTING io_expression    TYPE REF TO if_xco_ddl_expression
      RETURNING VALUE(rd_result) TYPE string.

    METHODS get_simple_expression
      IMPORTING io_simple        TYPE REF TO if_xco_ddl_expression
      RETURNING VALUE(rd_result) TYPE string.

    METHODS find_table_field
      IMPORTING it_mapping       TYPE tt_mapping
                is_source        TYPE ts_mapping
                id_table         TYPE string
      RETURNING VALUE(rd_result) TYPE string.

    METHODS get_cast_expression
      IMPORTING io_cast          TYPE REF TO cl_xco_ddl_expr_cast
      RETURNING VALUE(rd_result) TYPE string.

    METHODS cleanup_field
      IMPORTING id_field         TYPE string
      RETURNING VALUE(rd_result) TYPE string.
ENDCLASS.


CLASS zcl_bs_demo_cds_extract IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA(lt_mapping) = get_mapping_from_cds( id_cds   = 'I_OPERATIONALACCTGDOCITEM'
                                             id_table = 'BSEG' ).

    out->write( lt_mapping ).
  ENDMETHOD.


  METHOD get_mapping_from_cds.
    DATA(lt_mapping) = get_table_fields_from_cds( id_cds     = id_cds
                                                  id_table   = id_table
                                                  id_toupper = abap_false ).

    LOOP AT lt_mapping INTO DATA(ls_mapping) WHERE cds = id_cds AND cds_field IS NOT INITIAL.
      DATA(ld_field) = find_table_field( it_mapping = lt_mapping
                                         is_source  = ls_mapping
                                         id_table   = id_table ).

      INSERT VALUE #( cds         = ls_mapping-cds
                      cds_field   = ls_mapping-cds_field
                      table       = id_table
                      table_field = ld_field )
             INTO TABLE rt_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_table_fields_from_cds.
    DATA(lo_cds) = xco_cds=>view( id_cds ).

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

      INSERT VALUE #( cds         = id_cds
                      cds_field   = ld_field
                      table       = to_upper( ls_view_entity_content-data_source-entity )
                      table_field = cleanup_field( get_field_value( ls_field-expression ) ) )
             INTO TABLE rt_result.
    ENDLOOP.

    IF ls_view_entity_content-data_source-entity = id_table OR ls_view_entity_content-data_source-entity IS INITIAL.
      RETURN.
    ELSE.
      INSERT LINES OF get_table_fields_from_cds( id_cds     = ls_view_entity_content-data_source-entity
                                                 id_table   = id_table
                                                 id_toupper = abap_true ) INTO TABLE rt_result.
    ENDIF.
  ENDMETHOD.


  METHOD get_field_value.
    CASE TYPE OF io_expression.
      WHEN TYPE cl_xco_ddl_expr_simple INTO DATA(lo_simple).
        RETURN get_simple_expression( lo_simple ).
      WHEN TYPE cl_xco_ddl_expr_field INTO DATA(lo_field).
        RETURN get_simple_expression( lo_field ).
      WHEN TYPE cl_xco_ddl_expr_cast INTO DATA(lo_cast).
        RETURN get_cast_expression( lo_cast ).
*      WHEN TYPE cl_xco_ddl_expr_case INTO DATA(lo_case).
*        RETURN ``.
    ENDCASE.
  ENDMETHOD.


  METHOD get_simple_expression.
    DATA(lo_strings) = io_simple->if_xco_text~get_lines( ).
    DATA(lt_strings) = lo_strings->value.

    IF line_exists( lt_strings[ 1 ] ).
      RETURN lt_strings[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD get_cast_expression.
    DATA(lo_string) = io_cast->get_source( )->get_lines( ).

    LOOP AT lo_string->value INTO DATA(ld_line).
*      IF ld_line CS `concat(`.
*        BREAK-POINT.
*      ENDIF.

      IF ld_line CS `cast(`.
        SPLIT ld_line AT ` ` INTO TABLE DATA(lt_split).
        RETURN lt_split[ 2 ].
      ENDIF.
    ENDLOOP.
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
    DATA(ld_field) = to_upper( id_field ).

    IF ld_field CS `ABS(`.
      ld_field = replace( val  = ld_field
                          sub  = `ABS(`
                          with = `` ).
      ld_field = replace( val  = ld_field
                          sub  = `)`
                          with = `` ).
    ENDIF.

    RETURN ld_field.
  ENDMETHOD.
ENDCLASS.