*&---------------------------------------------------------------------*
*& Include  ZCL_VECTOR
*&---------------------------------------------------------------------*

TYPES: ty_float TYPE p LENGTH 16 DECIMALS 14,
       ty_float_vector TYPE TABLE OF ty_float WITH DEFAULT KEY,
       ty_float_matrix TYPE TABLE OF ty_float_vector WITH DEFAULT KEY,
       tt_float TYPE TABLE OF ty_float WITH DEFAULT KEY,
       tt_matrix TYPE TABLE OF ty_float_vector WITH DEFAULT KEY.


*----------------------------------------------------------------------*
* VECTOR OPERATIONS
*----------------------------------------------------------------------*

CLASS zcl_vector DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get RETURNING VALUE(rt_vector) TYPE tt_float,
      size RETURNING VALUE(rv_size) TYPE i,
      set IMPORTING it_column TYPE ANY TABLE EXCEPTIONS SIZE_ERROR INPUT_ERROR,
      append IMPORTING iv_value TYPE ty_float,
      delete IMPORTING iv_index TYPE i EXCEPTIONS WRONG_INDEX,
      set_value IMPORTING iv_index TYPE i iv_value TYPE ty_float EXCEPTIONS WRONG_INDEX,
      get_value IMPORTING iv_index TYPE i RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS WRONG_INDEX,
      get_index IMPORTING iv_value TYPE ty_float RETURNING VALUE(rv_index) TYPE i,
      get_indexes IMPORTING iv_value TYPE ty_float RETURNING VALUE(rt_index) TYPE INT4_TABLE,

      slice_by_index IMPORTING iv_from TYPE i OPTIONAL iv_to TYPE i OPTIONAL it_indexes TYPE int4_table OPTIONAL iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS INPUT_ERROR,

      add IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      subtract IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      multv IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      divv  IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      addn  IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      multn IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      divn  IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      pown  IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,

      dot  IMPORTING it_vector TYPE REF TO zcl_vector RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS SIZE_ERROR,
      absv RETURNING VALUE(rv_value) TYPE ty_float,
      min  RETURNING VALUE(rv_value) TYPE ty_float,
      max  RETURNING VALUE(rv_value) TYPE ty_float,
      mean RETURNING VALUE(rv_value) TYPE ty_float,
      std  RETURNING VALUE(rv_value) TYPE ty_float,
      sum  RETURNING VALUE(rv_value) TYPE ty_float,

      scale IMPORTING iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      normalize IMPORTING iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      shuffle IMPORTING iv_seed TYPE i DEFAULT 1 iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      apply_func IMPORTING iv_funcstr TYPE c iv_inplace TYPE flag DEFAULT ' ' iv_argstr TYPE string OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS COUNT_ERROR,

      print,

      constructor IMPORTING size TYPE i fill_value TYPE ty_float OPTIONAL.
    CLASS-METHODS:
      create_from IMPORTING it_column TYPE ANY TABLE RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR INPUT_ERROR,
      create_range IMPORTING iv_from TYPE ty_float DEFAULT 1 iv_to TYPE ty_float DEFAULT 10 iv_step TYPE ty_float DEFAULT 1 RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS INPUT_ERROR,
      cdot IMPORTING io_vector1 TYPE REF TO zcl_vector io_vector2 TYPE REF TO zcl_vector RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS SIZE_ERROR INPUT_ERROR.

  PRIVATE SECTION.
    METHODS:
      operation_v IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' iv_op TYPE char1 RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      operation_n IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' iv_op TYPE char1 RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      operation_f IMPORTING iv_funcstr TYPE c iv_inplace TYPE flag DEFAULT ' ' iv_argstr TYPE string OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS COUNT_ERROR.

    DATA: mt_float_vector TYPE tt_float.
ENDCLASS.

CLASS zcl_vector IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_fill TYPE ty_float.
    IF fill_value IS SUPPLIED.
      lv_fill = fill_value.
    ENDIF.
    REFRESH mt_float_vector.
    IF size > 0.
      DO size TIMES.
        APPEND lv_fill TO mt_float_vector.
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD get.
    rt_vector = mt_float_vector.
  ENDMETHOD.

  METHOD size.
    rv_size = LINES( mt_float_vector ).
  ENDMETHOD.

  METHOD set.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector LIKE mt_float_vector.

    IF LINES( it_column ) <> LINES( mt_float_vector ).
      RAISE SIZE_ERROR.
    ENDIF.

    TRY.
        LOOP AT it_column INTO lv_float.
          APPEND lv_float TO lt_float_vector.
        ENDLOOP.
        mt_float_vector[] = lt_float_vector[].
      CATCH cx_root.
        RAISE INPUT_ERROR.
    ENDTRY.
  ENDMETHOD.

  METHOD create_from.
    CREATE OBJECT ro_vector EXPORTING size = LINES( it_column ).

    ro_vector->set( EXPORTING it_column = it_column
                    EXCEPTIONS OTHERS = 1 ).

    IF sy-subrc = 1.
      FREE ro_vector.
    ENDIF.
  ENDMETHOD.

  METHOD create_range.
    DATA: lv_value TYPE ty_float.

    CREATE OBJECT ro_vector EXPORTING size = 1 fill_value = iv_from.
    IF iv_from = iv_to.
      RETURN.
    ENDIF.

    lv_value = iv_from.
    DO.
      lv_value = lv_value + iv_step.
      IF lv_value > iv_to.
        EXIT.
      ENDIF.
      ro_vector->append( lv_value ).
    ENDDO.
  ENDMETHOD.

  METHOD append.
    APPEND iv_value TO mt_float_vector.
  ENDMETHOD.

  METHOD delete.
    IF iv_index < 0 OR iv_index > LINES( mt_float_vector ).
      RAISE WRONG_INDEX.
    ENDIF.
    DELETE mt_float_vector INDEX iv_index.
  ENDMETHOD.

  METHOD set_value.
    FIELD-SYMBOLS: <fs> TYPE ty_float.

    IF iv_index < 0 OR iv_index > LINES( mt_float_vector ).
      RAISE WRONG_INDEX.
    ENDIF.
    READ TABLE mt_float_vector ASSIGNING <fs> INDEX iv_index.
    <fs> = iv_value.
  ENDMETHOD.

  METHOD get_value.
    IF iv_index < 0 OR iv_index > LINES( mt_float_vector ).
      RAISE WRONG_INDEX.
    ENDIF.
    READ TABLE mt_float_vector INTO rv_value INDEX iv_index.
  ENDMETHOD.

  METHOD get_index.
    rv_index = 0.
    READ TABLE mt_float_vector TRANSPORTING NO FIELDS WITH KEY table_line = iv_value.
    IF sy-subrc = 0.
      rv_index = sy-tabix.
    ENDIF.
  ENDMETHOD.

  METHOD get_indexes.
    CLEAR rt_index[].
    LOOP AT mt_float_vector TRANSPORTING NO FIELDS WHERE table_line = iv_value.
      APPEND sy-tabix TO rt_index.
    ENDLOOP.
  ENDMETHOD.

  METHOD slice_by_index.
    DATA: lt_new_vector TYPE ty_float_vector.
    DATA: lv_index TYPE i.
    DATA: lv_value TYPE ty_float.

    IF iv_from > iv_to.
      RAISE INPUT_ERROR.
    ENDIF.

    REFRESH lt_new_vector.

    IF iv_from IS NOT INITIAL AND iv_to IS NOT INITIAL.
      LOOP AT mt_float_vector INTO lv_value FROM iv_from TO iv_to.
        APPEND lv_value TO lt_new_vector.
      ENDLOOP.

    ELSEIF it_indexes[] IS NOT INITIAL.
      LOOP AT it_indexes INTO lv_index.
        READ TABLE mt_float_vector INTO lv_value INDEX lv_index.
        IF sy-subrc = 0.
          APPEND lv_value TO lt_new_vector.
        ENDIF.
      ENDLOOP.

    ELSE.
      RAISE INPUT_ERROR.

    ENDIF.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_vector EXPORTING size = LINES( lt_new_vector ).
      ro_vector->set( lt_new_vector ).
    ELSE.
      mt_float_vector[] = lt_new_vector[].
      ro_vector = me.
    ENDIF.

  ENDMETHOD.


  METHOD operation_v.
    FIELD-SYMBOLS: <fs> TYPE ty_float,
                   <fs2> TYPE ty_float.
    DATA: lv_index TYPE i.
    DATA: lt_float_vector TYPE tt_float.
    DATA: lt_float_vector_out TYPE tt_float.

    lt_float_vector = it_vector->get( ).
    IF LINES( lt_float_vector ) <> LINES( mt_float_vector ).
      RAISE SIZE_ERROR.
    ENDIF.

    IF iv_inplace = abap_false.
      lt_float_vector_out[] = mt_float_vector[].
    ENDIF.

    DO LINES( mt_float_vector ) TIMES.
      lv_index = sy-index.
      IF iv_inplace = abap_false.
        READ TABLE lt_float_vector_out ASSIGNING <fs> INDEX lv_index.
      ELSE.
        READ TABLE mt_float_vector ASSIGNING <fs> INDEX lv_index.
      ENDIF.

      READ TABLE lt_float_vector ASSIGNING <fs2> INDEX lv_index.
      CASE iv_op.
        WHEN '+'.
          <fs> = <fs> + <fs2>.
        WHEN '-'.
          <fs> = <fs> - <fs2>.
        WHEN '*'.
          <fs> = <fs> * <fs2>.
        WHEN '/'.
          <fs> = <fs> / <fs2>.
      ENDCASE.
    ENDDO.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_vector EXPORTING size = LINES( mt_float_vector ).
      ro_vector->set( lt_float_vector_out ).
    ELSE.
      ro_vector = me.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    ro_vector = me->operation_v( it_vector = it_vector iv_inplace = iv_inplace iv_op = '+' ).
  ENDMETHOD.

  METHOD subtract.
    ro_vector = me->operation_v( it_vector = it_vector iv_inplace = iv_inplace iv_op = '-' ).
  ENDMETHOD.

  METHOD multv.
    ro_vector = me->operation_v( it_vector = it_vector iv_inplace = iv_inplace iv_op = '*' ).
  ENDMETHOD.

  METHOD divv.
    ro_vector = me->operation_v( it_vector = it_vector iv_inplace = iv_inplace iv_op = '/' ).
  ENDMETHOD.

  METHOD operation_n.
    FIELD-SYMBOLS: <fs> TYPE ty_float.
    DATA: lv_index TYPE i.
    DATA: lt_float_vector_out TYPE tt_float.

    IF iv_inplace = abap_false.
      lt_float_vector_out[] = mt_float_vector[].
    ENDIF.

    DO LINES( mt_float_vector ) TIMES.
      lv_index = sy-index.
      IF iv_inplace = abap_false.
        READ TABLE lt_float_vector_out ASSIGNING <fs> INDEX lv_index.
      ELSE.
        READ TABLE mt_float_vector ASSIGNING <fs> INDEX lv_index.
      ENDIF.

      CASE iv_op.
        WHEN '+'.
          <fs> = <fs> + iv_value.
        WHEN '-'.
          <fs> = <fs> - iv_value.
        WHEN '*'.
          <fs> = <fs> * iv_value.
        WHEN '/'.
          <fs> = <fs> / iv_value.
        WHEN '^'.
          <fs> = <fs> ** iv_value.
      ENDCASE.
    ENDDO.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_vector EXPORTING size = LINES( mt_float_vector ).
      ro_vector->set( lt_float_vector_out ).
    ELSE.
      ro_vector = me.
    ENDIF.
  ENDMETHOD.

  METHOD addn.
    ro_vector = me->operation_n( iv_value = iv_value iv_inplace = iv_inplace iv_op = '+' ).
  ENDMETHOD.

  METHOD multn.
    ro_vector = me->operation_n( iv_value = iv_value iv_inplace = iv_inplace iv_op = '*' ).
  ENDMETHOD.

  METHOD divn.
    ro_vector = me->operation_n( iv_value = iv_value iv_inplace = iv_inplace iv_op = '/' ).
  ENDMETHOD.

  METHOD pown.
    ro_vector = me->operation_n( iv_value = iv_value iv_inplace = iv_inplace iv_op = '^' ).
  ENDMETHOD.

  METHOD operation_f.
    FIELD-SYMBOLS: <fs> TYPE ty_float.
    DATA: lv_index TYPE i.
    DATA: lt_float_vector_out TYPE tt_float.

    IF iv_inplace = abap_false.
      lt_float_vector_out[] = mt_float_vector[].
    ENDIF.

    DO LINES( mt_float_vector ) TIMES.
      lv_index = sy-index.
      IF iv_inplace = abap_false.
        READ TABLE lt_float_vector_out ASSIGNING <fs> INDEX lv_index.
      ELSE.
        READ TABLE mt_float_vector ASSIGNING <fs> INDEX lv_index.
      ENDIF.

      TRY.
          CASE iv_funcstr.
            WHEN 'abs'.   <fs> = abs( <fs> ).
            WHEN 'sign'.  <fs> = sign( <fs> ).
            WHEN 'ceil'.  <fs> = ceil( <fs> ).
            WHEN 'floor'. <fs> = floor( <fs> ).
            WHEN 'sin'.   <fs> = sin( <fs> ).
            WHEN 'cos'.   <fs> = cos( <fs> ).
            WHEN 'tan'.   <fs> = tan( <fs> ).
            WHEN 'log'.   <fs> = log( <fs> ).
            WHEN 'log1p'. <fs> = log( <fs> + 1 ).
            WHEN 'expm1'. <fs> = exp( <fs> ) - 1.
            WHEN 'round'. <fs> = round( val = <fs> dec = iv_argstr mode = cl_abap_math=>round_half_up ).
            WHEN 'sigmoid'.
              IF <fs> > 50.
                <fs> = 1.
              ELSEIF <fs> < -50.
                <fs> = 0.
              ELSE.
               <fs> = '1.0' / ( '1.0' + exp( -1 * <fs> ) ).
              ENDIF.
          ENDCASE.
        CATCH cx_root.
          RAISE COUNT_ERROR.
      ENDTRY.
    ENDDO.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_vector EXPORTING size = LINES( mt_float_vector ).
      ro_vector->set( lt_float_vector_out ).
    ELSE.
      ro_vector = me.
    ENDIF.
  ENDMETHOD.

  METHOD apply_func.
    ro_vector = me->operation_f( iv_funcstr = iv_funcstr iv_inplace = iv_inplace iv_argstr = iv_argstr ).
  ENDMETHOD.

  METHOD dot.
    FIELD-SYMBOLS: <fs> TYPE ty_float,
                   <fs2> TYPE ty_float.
    DATA: lv_index TYPE i.
    DATA: lt_float_vector TYPE tt_float.

    lt_float_vector = it_vector->get( ).

    IF LINES( lt_float_vector ) <> LINES( mt_float_vector ).
      RAISE SIZE_ERROR.
    ENDIF.

    rv_value = 0.

    DO LINES( mt_float_vector ) TIMES.
      lv_index = sy-index.
      READ TABLE mt_float_vector ASSIGNING <fs> INDEX lv_index.
      READ TABLE lt_float_vector ASSIGNING <fs2> INDEX lv_index.
      rv_value = rv_value + <fs> * <fs2>.
    ENDDO.

  ENDMETHOD.

  METHOD cdot.
    IF io_vector1 IS NOT BOUND OR io_vector2 IS NOT BOUND.
      RAISE INPUT_ERROR.
    ENDIF.
    rv_value = io_vector1->dot( io_vector2 ).
  ENDMETHOD.

  METHOD absv.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE tt_float.

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      rv_value = rv_value + lv_float ** 2.
    ENDLOOP.
    rv_value = sqrt( rv_value ).

  ENDMETHOD.

  METHOD min.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE tt_float.

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      IF sy-tabix = 1 OR ( sy-tabix > 1 AND lv_float < rv_value ).
        rv_value = lv_float.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD max.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE tt_float.

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      IF sy-tabix = 1 OR ( sy-tabix > 1 AND lv_float > rv_value ).
        rv_value = lv_float.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD mean.
    DATA: lv_float TYPE ty_float.

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      rv_value = rv_value + lv_float.
    ENDLOOP.
    rv_value = rv_value / lines( mt_float_vector ).

  ENDMETHOD.

  METHOD std.
    DATA: lv_float TYPE ty_float.
    DATA: lv_mean TYPE ty_float.
    DATA: lt_float_vector TYPE tt_float.

    IF LINES( mt_float_vector ) = 1.
      rv_value = 0.
      RETURN.
    ENDIF.

    lv_mean = me->mean( ).

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      rv_value = rv_value + ( lv_float - lv_mean ) ** 2.
    ENDLOOP.
    rv_value = rv_value / ( LINES( mt_float_vector ) - 1 ).
    rv_value = sqrt( rv_value ).
  ENDMETHOD.

  METHOD sum.
    DATA: lv_float TYPE ty_float.

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      rv_value = rv_value + lv_float.
    ENDLOOP.

  ENDMETHOD.

  METHOD scale.
    FIELD-SYMBOLS: <fs> TYPE ty_float.
    DATA: lv_min TYPE ty_float.
    DATA: lv_max TYPE ty_float.
    DATA: lv_scaled_value TYPE ty_float.
    DATA: lt_float_vector_out TYPE tt_float.

    lv_min = me->min( ).
    lv_max = me->max( ).
    LOOP AT mt_float_vector ASSIGNING <fs>.
      lv_scaled_value = ( <fs> - lv_min ) / ( lv_max - lv_min ).
      IF iv_inplace = abap_false.
        APPEND lv_scaled_value TO lt_float_vector_out.
      ELSE.
        <fs> = lv_scaled_value.
      ENDIF.
    ENDLOOP.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_vector EXPORTING size = LINES( mt_float_vector ).
      ro_vector->set( lt_float_vector_out ).
    ELSE.
      ro_vector = me.
    ENDIF.
  ENDMETHOD.

  METHOD normalize.
    FIELD-SYMBOLS: <fs> TYPE ty_float.
    DATA: lv_mean TYPE ty_float.
    DATA: lv_std TYPE ty_float.
    DATA: lv_scaled_value TYPE ty_float.
    DATA: lt_float_vector_out TYPE tt_float.

    lv_mean = me->mean( ).
    lv_std = me->std( ).
    LOOP AT mt_float_vector ASSIGNING <fs>.
      lv_scaled_value = ( <fs> - lv_mean ) / lv_std.
      IF iv_inplace = abap_false.
        APPEND lv_scaled_value TO lt_float_vector_out.
      ELSE.
        <fs> = lv_scaled_value.
      ENDIF.
    ENDLOOP.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_vector EXPORTING size = LINES( mt_float_vector ).
      ro_vector->set( lt_float_vector_out ).
    ELSE.
      ro_vector = me.
    ENDIF.
  ENDMETHOD.

  METHOD shuffle.
    DATA: lv_cur_index TYPE i.
    DATA: lv_tmp_value TYPE ty_float.
    DATA: lv_rnd_index TYPE i.
    DATA: lo_rgen TYPE REF TO cl_abap_random_int.

    lv_cur_index = LINES( mt_float_vector ).
    lo_rgen = cl_abap_random_int=>create( min = 1 max = lv_cur_index seed = iv_seed ).

    IF iv_inplace = abap_false.
      ro_vector = me.
    ELSE.
      ro_vector = zcl_vector=>create_from( me->get( ) ).
    ENDIF.

    WHILE lv_cur_index <> 0.
      " Pick a remaining element...
      lv_rnd_index = lo_rgen->get_next( ).

      " And swap it with the current element.
      lv_tmp_value = ro_vector->get_value( lv_cur_index ).
      ro_vector->set_value( iv_index = lv_cur_index iv_value = ro_vector->get_value( lv_rnd_index ) ).
      ro_vector->set_value( iv_index = lv_rnd_index iv_value = lv_tmp_value ).

      lv_cur_index = lv_cur_index - 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD print.
    DATA: lv_float TYPE ty_float.
    LOOP AT mt_float_vector INTO lv_float.
      WRITE: / lv_float.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
