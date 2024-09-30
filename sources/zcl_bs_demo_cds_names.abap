CLASS zcl_bs_demo_cds_names DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

  PRIVATE SECTION.
    TYPES tt_r_name TYPE RANGE OF ddddlsrc-ddlname.

    TYPES: BEGIN OF ts_mapping,
             ddlname  TYPE ddddlsrc-ddlname,
             cds_name TYPE string,
           END OF ts_mapping.
    TYPES tt_mapping TYPE SORTED TABLE OF ts_mapping WITH UNIQUE KEY ddlname.

    METHODS extract_cds_name
      IMPORTING it_r_name        TYPE tt_r_name
      RETURNING VALUE(rt_result) TYPE tt_mapping.
ENDCLASS.


CLASS zcl_bs_demo_cds_names IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    out->write( extract_cds_name( VALUE #( ) ) ).
  ENDMETHOD.


  METHOD extract_cds_name.
    SELECT FROM ddddlsrc
      FIELDS ddlname, source
      WHERE ddlname IN @it_r_name
      INTO TABLE @DATA(lt_views).

    LOOP AT lt_views REFERENCE INTO DATA(lr_view).
      INSERT VALUE #( ddlname = lr_view->ddlname ) INTO TABLE rt_result REFERENCE INTO DATA(lr_result).

      lr_view->source = replace( val  = lr_view->source
                                 sub  = cl_abap_char_utilities=>cr_lf
                                 with = ` `
                                 occ  = 0 ).
      SPLIT lr_view->source AT ` ` INTO TABLE DATA(lt_split).

      DATA(ld_start) = line_index( lt_split[ table_line = `define` ] ).

      LOOP AT lt_split INTO DATA(ld_split) FROM ld_start.
        IF to_upper( ld_split ) = lr_view->ddlname.
          lr_result->cds_name = ld_split.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lr_result->cds_name IS INITIAL.
        lr_result->cds_name = lr_view->ddlname.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.