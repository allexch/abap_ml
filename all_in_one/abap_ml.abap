*&---------------------------------------------------------------------*
*& Report  ZREPORT_ML
*&
*&---------------------------------------------------------------------*
*& Classes and functionality to reproduce basic Machine Learning stuff
*& in ABAP. Starting from vector/matrix API
*&
*& -  operations with float vector/matrix ( zcl_matrix, zcl_vector )
*& -  stochastic gradient descent ( zcl_ml_sgd )
*& -  regression models ( zcl_ml_linear_regression, zcl_ml_sgd_regression )
*& -  classification models ( zcl_ml_logistic_regression, zcl_ml_sgd_classifier )
*&
*& Examples included
*&---------------------------------------------------------------------*

REPORT  ZREPORT_ML.

TYPES: ty_float TYPE p LENGTH 16 DECIMALS 14,
       ty_float_vector TYPE TABLE OF ty_float WITH DEFAULT KEY,
       ty_float_matrix TYPE TABLE OF ty_float_vector WITH DEFAULT KEY.


*----------------------------------------------------------------------*
* PART1: VECTOR OPERATIONS
*----------------------------------------------------------------------*

CLASS zcl_vector DEFINITION.
  PUBLIC SECTION.
    METHODS:
* get internal table of float values
      get RETURNING VALUE(rt_vector) TYPE ty_float_vector,
* get size of the vector
      size RETURNING VALUE(rv_size) TYPE i,
* set internal table of float values as a vector => better use zcl_vector=>create_from
      set IMPORTING it_column TYPE ANY TABLE EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* append value to the vector
      append IMPORTING iv_value TYPE ty_float,
* delete value to the vector
      delete IMPORTING iv_index TYPE i EXCEPTIONS WRONG_INDEX,
* set value in position
      set_value IMPORTING iv_index TYPE i iv_value TYPE ty_float EXCEPTIONS WRONG_INDEX,
* get value from position
      get_value IMPORTING iv_index TYPE i RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS WRONG_INDEX,
* get index of values (0 if not found)
      get_index IMPORTING iv_value TYPE ty_float RETURNING VALUE(rv_index) TYPE i,
* find value in vector and return list of indexes
      get_indexes IMPORTING iv_value TYPE ty_float RETURNING VALUE(rt_index) TYPE INT4_TABLE,
* get information about containing values in list of pairs "value"-"number_of_occurences"
      get_values_count RETURNING VALUE(rt_values) TYPE wdy_key_value_list,

* get part of vector by from-to indexes, or by defined list of indexes
      slice_by_index IMPORTING iv_from TYPE i OPTIONAL iv_to TYPE i OPTIONAL it_indexes TYPE int4_table OPTIONAL iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS INPUT_ERROR,

* operations with another vector
      add IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      subtract IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      multv IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      divv  IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,

* operations with number
      addn  IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      multn IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      divn  IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      pown  IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,

* dot product with another vector
      dot  IMPORTING it_vector TYPE REF TO zcl_vector RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS SIZE_ERROR,

* unary operations
      absv RETURNING VALUE(rv_value) TYPE ty_float,
      min  RETURNING VALUE(rv_value) TYPE ty_float,
      max  RETURNING VALUE(rv_value) TYPE ty_float,
      mean RETURNING VALUE(rv_value) TYPE ty_float,
      std  RETURNING VALUE(rv_value) TYPE ty_float,
      sum  RETURNING VALUE(rv_value) TYPE ty_float,

* scale vector values to [0; 1] interval
      scale IMPORTING iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
* normalize vector
      normalize IMPORTING iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
* shuffle values in random mode
      shuffle IMPORTING iv_seed TYPE i DEFAULT 1 iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
* apply predefined function to all elements of the vector (see list of supported func in code)
      apply_func IMPORTING iv_funcstr TYPE c iv_inplace TYPE flag DEFAULT ' ' iv_argstr TYPE string OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS COUNT_ERROR,

* write contents to spool
      print,

* constructor:
      constructor IMPORTING size TYPE i                        " size > 0
                            fill_value TYPE ty_float OPTIONAL. " fill_value - default filler value
    CLASS-METHODS:
* create vector from any table of numeric elements
      create_from IMPORTING it_column TYPE ANY TABLE RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create copy of vector
      create_copy IMPORTING io_vector TYPE REF TO zcl_vector RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create sequence of values starting with iv_from, ending with iv_end or higher, and stepped by iv_step. (Like "range" in python)
      create_range IMPORTING iv_from TYPE ty_float DEFAULT 1 iv_to TYPE ty_float DEFAULT 10 iv_step TYPE ty_float DEFAULT 1 RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS INPUT_ERROR,
* dot product of two vectors (as class method)
      cdot IMPORTING io_vector1 TYPE REF TO zcl_vector io_vector2 TYPE REF TO zcl_vector RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS SIZE_ERROR INPUT_ERROR.

  PRIVATE SECTION.
    METHODS:
      operation_v IMPORTING it_vector TYPE REF TO zcl_vector iv_inplace TYPE flag DEFAULT ' ' iv_op TYPE char1 RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
      operation_n IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' iv_op TYPE char1 RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      operation_f IMPORTING iv_funcstr TYPE c iv_inplace TYPE flag DEFAULT ' ' iv_argstr TYPE string OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS COUNT_ERROR,
      get_ref RETURNING VALUE(rr_data) TYPE REF TO data.

    DATA: mt_float_vector TYPE ty_float_vector.
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

  METHOD get_ref.
    GET REFERENCE OF mt_float_vector INTO rr_data.
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

  METHOD create_copy.
    DATA: lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_float_vector> TYPE INDEX TABLE.

    lr_data = io_vector->get_ref( ).
    ASSIGN lr_data->* TO <fs_float_vector>.
    ro_vector = zcl_vector=>create_from( it_column = <fs_float_vector> ).
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

  METHOD get_values_count.
    TYPES: BEGIN OF ty_list,
            value TYPE ty_float,
            num TYPE i,
          END OF ty_list.
    DATA: ls_line TYPE LINE OF wdy_key_value_list.
    DATA: lv_strkey TYPE string.
    DATA: ls_list TYPE ty_list.
    DATA: lt_list TYPE SORTED TABLE OF ty_list WITH UNIQUE KEY value.
    FIELD-SYMBOLS: <fs> TYPE ty_float.
    FIELD-SYMBOLS: <fs_list> TYPE ty_list.

    LOOP AT mt_float_vector ASSIGNING <fs>.
      READ TABLE lt_list ASSIGNING <fs_list> WITH KEY value = <fs>.
      IF sy-subrc = 0.
        ADD 1 TO <fs_list>-num.
      ELSE.
        ls_list-value = <fs>.
        ls_list-num = 1.
        INSERT ls_list INTO TABLE lt_list.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_list ASSIGNING <fs_list>.
      ls_line-key = <fs_list>-value.
      ls_line-value = <fs_list>-num.
      APPEND ls_line TO rt_values.
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
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lt_float_vector_out TYPE ty_float_vector.

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
    DATA: lt_float_vector_out TYPE ty_float_vector.

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
    DATA: lt_float_vector_out TYPE ty_float_vector.

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
            WHEN 'exp'.   <fs> = exp( <fs> ).
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
    DATA: lt_float_vector TYPE ty_float_vector.

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
    DATA: lt_float_vector TYPE ty_float_vector.

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      rv_value = rv_value + lv_float ** 2.
    ENDLOOP.
    rv_value = sqrt( rv_value ).

  ENDMETHOD.

  METHOD min.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.

    rv_value = 0.
    LOOP AT mt_float_vector INTO lv_float.
      IF sy-tabix = 1 OR ( sy-tabix > 1 AND lv_float < rv_value ).
        rv_value = lv_float.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD max.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.

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
    DATA: lt_float_vector TYPE ty_float_vector.

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
    DATA: lt_float_vector_out TYPE ty_float_vector.

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
    DATA: lt_float_vector_out TYPE ty_float_vector.

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
      ro_vector = zcl_vector=>create_copy( me ).
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

*----------------------------------------------------------------------*
* PART2: MATRIX OPERATIONS
*----------------------------------------------------------------------*

CLASS zcl_matrix DEFINITION.
  PUBLIC SECTION.
    METHODS:
* get internal table of internal tables of float values (if somebody need it)
      get RETURNING VALUE(rt_matrix) TYPE ty_float_matrix,
* get row of the matrix as a vector
      get_row IMPORTING iv_index TYPE i RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector  EXCEPTIONS INDEX_ERROR,
* get column of the matrix as a vector
      get_column IMPORTING iv_index TYPE i RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS INDEX_ERROR,
* get ij-element of the matrix
      get_pos IMPORTING i TYPE i j TYPE i RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS INDEX_ERROR,
* set the matrix from any table (see zcl_matrix=>create from) :
      set IMPORTING it_table TYPE INDEX TABLE iv_check TYPE flag DEFAULT 'X' EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* set row of the matrix from a vector
      set_row IMPORTING iv_index TYPE i io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR INDEX_ERROR,
* set column of the matrix from a vector
      set_column IMPORTING iv_index TYPE i io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR INDEX_ERROR,
* set value in position
      set_pos IMPORTING i TYPE i j TYPE i iv_value TYPE ty_float EXCEPTIONS SIZE_ERROR INDEX_ERROR,
* get number of rows
      nrows RETURNING VALUE(rv_size) TYPE i,
* get number of columns
      ncolumns RETURNING VALUE(rv_size) TYPE i,
* append row to the matrix
      append_row IMPORTING io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
* append column to the matrix
      append_column IMPORTING io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,

* get certain rows of the matrix by from-to indexes, or by defined list of indexes
      slice_by_rows IMPORTING iv_from TYPE i OPTIONAL iv_to TYPE i OPTIONAL it_indexes TYPE int4_table OPTIONAL iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS INPUT_ERROR,

* operations with another matrix
      add IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
      subtract IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
      multiply IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
* multiply by vector
      multiplyv IMPORTING io_vector TYPE REF TO zcl_vector RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
* multiply by number
      multiplyn IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,

* transpose
      transpose IMPORTING iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix,
* solve linear system of equations Ax = b, with A as current matrix and b as io_vector
      solve IMPORTING io_vector TYPE REF TO zcl_vector RETURNING VALUE(ro_result) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR RANK_ERROR COUNT_ERROR,

      min  RETURNING VALUE(rv_value) TYPE ty_float,
      max  RETURNING VALUE(rv_value) TYPE ty_float,

* write contents to spool (be careful with large dimensions)
      print,
* save matrix in csv file
      save_csv IMPORTING iv_filename TYPE string iv_sep TYPE c DEFAULT ';' EXCEPTIONS OUTPUT_ERROR,

* constructor:
      constructor IMPORTING rows TYPE i cols TYPE i            " size > 0
                            fill_value TYPE ty_float OPTIONAL. " fill_value - default filler value
    CLASS-METHODS:
* create a matrix from any table :
*   if it is standard internal table - process only numeric columns (not TYPE n) - make matrix from them
*   if it is ty_float_matrix         - check consistency and make a copy
*   if it is ty_float_vector         - make matrix from a vector  ( N rows, 1 columns )
      create_from IMPORTING it_table TYPE ANY TABLE iv_check TYPE flag DEFAULT 'X' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create copy of the matrix
      create_copy IMPORTING io_matrix TYPE REF TO zcl_matrix RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create diagonal matrix with filler value or with certain vector on diag.
      create_diag IMPORTING iv_size TYPE i iv_value TYPE ty_float DEFAULT 1 io_vector TYPE REF TO zcl_vector OPTIONAL RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
* create matrix from csv-file
      load_from_csv IMPORTING iv_filename TYPE string iv_sep TYPE c DEFAULT ';' iv_header TYPE flag DEFAULT 'X' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create matrix from strings separated by chosen "separator"
      load_from_text IMPORTING it_strings TYPE table_of_strings iv_sep TYPE c DEFAULT ';' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
      is_numeric_type_kind IMPORTING iv_type_kind TYPE c RETURNING VALUE(rv_ok) TYPE flag.


  PRIVATE SECTION.
    METHODS:
      get_ref RETURNING VALUE(rr_data) TYPE REF TO data,
      operation_m IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' iv_op TYPE char1 RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR.

    DATA: mt_float_matrix TYPE ty_float_matrix,
          mv_rows TYPE i,
          mv_cols TYPE i.
ENDCLASS.

CLASS zcl_matrix IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_fill TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.

    IF fill_value IS SUPPLIED.
      lv_fill = fill_value.
    ENDIF.

    REFRESH mt_float_matrix.
    mv_rows = nmax( val1 = rows val2 = 0 ).
    mv_cols = nmax( val1 = cols val2 = 0 ).

    DO rows TIMES.
      DO cols TIMES.
        APPEND lv_fill TO lt_float_vector.
      ENDDO.
      APPEND lt_float_vector TO mt_float_matrix.
    ENDDO.
  ENDMETHOD.

  METHOD get.
    rt_matrix[] = mt_float_matrix[].
  ENDMETHOD.

  METHOD get_ref.
    GET REFERENCE OF mt_float_matrix INTO rr_data.
  ENDMETHOD.

  METHOD get_row.
    FIELD-SYMBOLS: <fs_float_vector> TYPE ty_float_vector.

    IF iv_index < 0 OR iv_index > mv_rows.
      RAISE INDEX_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_float_vector> INDEX iv_index.
    ro_vector = zcl_vector=>create_from( <fs_float_vector> ).
  ENDMETHOD.

  METHOD get_column.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_value TYPE ty_float.
    FIELD-SYMBOLS: <fs_tmp_vector> TYPE ty_float_vector.

    IF iv_index < 0 OR iv_index > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.

    LOOP AT mt_float_matrix ASSIGNING <fs_tmp_vector>.
      READ TABLE <fs_tmp_vector> INTO lv_value INDEX iv_index.
      APPEND lv_value TO lt_float_vector.
    ENDLOOP.

    ro_vector = zcl_vector=>create_from( lt_float_vector ).
  ENDMETHOD.

  METHOD get_pos.
    FIELD-SYMBOLS: <fs_float_vector> TYPE ty_float_vector.

    IF i < 0 OR i > mv_rows OR j < 0 OR j > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_float_vector> INDEX i.
    READ TABLE <fs_float_vector> INTO rv_value INDEX j.
  ENDMETHOD.

  METHOD is_numeric_type_kind.
    rv_ok = abap_false.
    IF iv_type_kind = cl_abap_typedescr=>typekind_int OR
       iv_type_kind = cl_abap_typedescr=>typekind_int1 OR
       iv_type_kind = cl_abap_typedescr=>typekind_int2 OR
       "iv_type_kind = cl_abap_typedescr=>typekind_num OR
       "iv_type_kind = cl_abap_typedescr=>typekind_numeric OR
       iv_type_kind = cl_abap_typedescr=>typekind_decfloat OR
       iv_type_kind = cl_abap_typedescr=>typekind_decfloat16 OR
       iv_type_kind = cl_abap_typedescr=>typekind_decfloat34 OR
       iv_type_kind = cl_abap_typedescr=>typekind_float OR
       iv_type_kind = cl_abap_typedescr=>typekind_packed.
      rv_ok = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set.
    DATA: lo_struc TYPE REF TO cl_abap_structdescr.
    DATA: lo_typedesc TYPE REF TO cl_abap_typedescr.
    DATA: lo_table TYPE REF TO cl_abap_tabledescr.
    DATA: lo_elem TYPE REF TO cl_abap_elemdescr.
    DATA: lt_comp TYPE abap_compdescr_tab,
          ls_comp LIKE LINE OF lt_comp.
    DATA: lt_names TYPE ttfieldname.
    DATA: lv_name LIKE LINE OF lt_names.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_vector TYPE flag.
    DATA: lv_assumed_cols TYPE i.
    FIELD-SYMBOLS: <fs_line> TYPE ANY,
                   <fs> TYPE ANY,
                   <fs_table> TYPE INDEX TABLE.

    IF iv_check = abap_false.
      mt_float_matrix[] = it_table[].
      mv_rows = LINES( it_table ).
      READ TABLE it_table ASSIGNING <fs_table> INDEX 1.
      mv_cols = LINES( <fs_table> ).
    ENDIF.

    IF LINES( it_table ) = 0.
      RAISE SIZE_ERROR.
    ENDIF.

    READ TABLE it_table ASSIGNING <fs_line> INDEX 1.

    CALL METHOD cl_abap_structdescr=>describe_by_data
      EXPORTING
        p_data      = <fs_line>
      RECEIVING
        p_descr_ref = lo_typedesc.

    IF lo_typedesc->kind = cl_abap_typedescr=>kind_table.
*   В строчке одна ячейка и это внутренняя таблица
*   Т.е это наш внутренний формат ty_float_matrix
      ASSIGN <fs_line> TO <fs_table>.
      lv_assumed_cols = LINES( <fs_table> ).

      " проверим типы оставшихся строк и
      " проверим равенство столбцов во всех строках
      LOOP AT it_table ASSIGNING <fs_line> FROM 2.

        CALL METHOD cl_abap_structdescr=>describe_by_data
          EXPORTING
            p_data      = <fs_line>
          RECEIVING
            p_descr_ref = lo_typedesc.
        IF NOT lo_typedesc->kind = cl_abap_typedescr=>kind_table.
          RAISE INPUT_ERROR.
        ENDIF.

        ASSIGN <fs_line> TO <fs_table>.
        IF LINES( <fs_table> ) <> lv_assumed_cols.
          RAISE SIZE_ERROR.
        ENDIF.

      ENDLOOP.

      " Все ок, переконвертируем все во float, если вдруг это было не так
      REFRESH mt_float_matrix[].
      LOOP AT it_table ASSIGNING <fs_table>.
        REFRESH lt_float_vector.
        LOOP AT <fs_table> ASSIGNING <fs>.
          lv_float = <fs>.
          APPEND lv_float TO lt_float_vector.
        ENDLOOP.
        APPEND lt_float_vector TO mt_float_matrix.
      ENDLOOP.

      mv_rows = LINES( it_table ).
      mv_cols = lv_assumed_cols.

    ELSEIF lo_typedesc->kind = cl_abap_typedescr=>kind_elem.
*   В строчке одна ячейка / поле
*   Если это число, то м.б. это наш внутрений формат ty_float_vector

      lo_elem ?= lo_typedesc.
      lv_assumed_cols = 1.

      IF zcl_matrix=>is_numeric_type_kind( lo_elem->type_kind ) = abap_false.
        RAISE INPUT_ERROR.
      ENDIF.

      " проверим что типы оставшихся строк такие же - просто числа
      LOOP AT it_table ASSIGNING <fs_line> FROM 2.

        CALL METHOD cl_abap_structdescr=>describe_by_data
          EXPORTING
            p_data      = <fs_line>
          RECEIVING
            p_descr_ref = lo_typedesc.

        IF NOT lo_typedesc->kind = cl_abap_typedescr=>kind_elem.
          RAISE INPUT_ERROR.
        ENDIF.
        lo_elem ?= lo_typedesc.
        IF zcl_matrix=>is_numeric_type_kind( lo_elem->type_kind ) = abap_false.
          RAISE INPUT_ERROR.
        ENDIF.

      ENDLOOP.

      " Все ок, переконвертируем все во float, если вдруг это было не так
      REFRESH mt_float_matrix[].
      LOOP AT it_table ASSIGNING <fs_line>.
        REFRESH lt_float_vector.
        lv_float = <fs_line>.
        APPEND lv_float TO lt_float_vector.
        APPEND lt_float_vector TO mt_float_matrix.
      ENDLOOP.

      mv_rows = LINES( it_table ).
      mv_cols = lv_assumed_cols.

    ELSE.
*   В строчке какая-то структура
*   Отфильтруем все численные поля и возьмем в матрицу

      lo_struc ?= lo_typedesc.
      lt_comp = lo_struc->components.

      LOOP AT lt_comp INTO ls_comp.
        IF zcl_matrix=>is_numeric_type_kind( ls_comp-type_kind ) = abap_true.
          APPEND ls_comp-name TO lt_names.
        ENDIF.
      ENDLOOP.

      IF LINES( lt_names ) = 0.
        RAISE INPUT_ERROR.
      ENDIF.

      mv_rows = LINES( it_table ).
      mv_cols = LINES( lt_names ).
      REFRESH mt_float_matrix.

      " Все ок, переконвертируем все во float
      LOOP AT it_table ASSIGNING <fs_line>.
        REFRESH lt_float_vector.
        LOOP AT lt_names INTO lv_name.
          ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fs>.
          lv_float = <fs>.
          APPEND lv_float TO lt_float_vector.
        ENDLOOP.
        APPEND lt_float_vector TO mt_float_matrix.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD create_from.
    CREATE OBJECT ro_matrix EXPORTING rows = 0 cols = 0.

    ro_matrix->set( EXPORTING it_table = it_table iv_check = iv_check ).
  ENDMETHOD.

  METHOD create_copy.
    DATA: lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_table> TYPE INDEX TABLE.

    lr_data = io_matrix->get_ref( ).
    ASSIGN lr_data->* TO <fs_table>.
    ro_matrix = zcl_matrix=>create_from( it_table = <fs_table> iv_check = ' ' ).
  ENDMETHOD.

  METHOD create_diag.
    IF iv_size <= 0.
      RAISE SIZE_ERROR.
    ENDIF.

    CREATE OBJECT ro_matrix EXPORTING rows = iv_size cols = iv_size.
    IF io_vector IS NOT BOUND.
      DO iv_size TIMES.
        ro_matrix->set_pos( i = sy-index j = sy-index iv_value = iv_value ).
      ENDDO.
    ELSE.
      DO io_vector->size( ) TIMES.
        ro_matrix->set_pos( i = sy-index j = sy-index iv_value = io_vector->get_value( sy-index ) ).
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD set_row.
    FIELD-SYMBOLS: <fs_row> TYPE ANY TABLE.

    IF iv_index < 0 OR iv_index > mv_rows.
      RAISE INDEX_ERROR.
    ENDIF.
    IF mv_cols <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_row> INDEX iv_index.
    <fs_row> = io_vector->get( ).
  ENDMETHOD.

  METHOD set_column.
    DATA: lt_float_vector TYPE ty_float_vector,
          lv_float TYPE ty_float.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE,
                   <fs> TYPE ty_float.

    IF iv_index < 0 OR iv_index > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.
    IF mv_rows <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lt_float_vector = io_vector->get( ).

    LOOP AT mt_float_matrix ASSIGNING <fs_row>.
      READ TABLE lt_float_vector INTO lv_float INDEX sy-tabix.
      READ TABLE <fs_row> ASSIGNING <fs> INDEX iv_index.
      <fs> = lv_float.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_pos.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE,
                   <fs> TYPE ty_float.

    IF i < 0 OR i > mv_rows OR j < 0 OR j > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_row> INDEX i.
    READ TABLE <fs_row> ASSIGNING <fs> INDEX j.
    <fs> = iv_value.
  ENDMETHOD.

  METHOD nrows.
    rv_size = mv_rows.
  ENDMETHOD.

  METHOD ncolumns.
    rv_size = mv_cols.
  ENDMETHOD.

  METHOD append_row.
    IF io_vector->size( ) <> mv_cols.
      RAISE SIZE_ERROR.
    ENDIF.

    APPEND io_vector->get( ) TO mt_float_matrix.
    mv_rows = mv_rows + 1.
  ENDMETHOD.

  METHOD append_column.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_float TYPE ty_float.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE.

    IF io_vector->size( ) <> mv_rows.
      RAISE SIZE_ERROR.
    ENDIF.

    lt_float_vector = io_vector->get( ).

    LOOP AT mt_float_matrix ASSIGNING <fs_row>.
      READ TABLE lt_float_vector INTO lv_float INDEX sy-tabix.
      APPEND lv_float TO <fs_row>.
    ENDLOOP.
    mv_cols = mv_cols + 1.
  ENDMETHOD.

  METHOD slice_by_rows.
    DATA: lr_data TYPE REF TO DATA.
    DATA: lt_new_matrix TYPE ty_float_matrix.
    DATA: lv_index TYPE i.
    FIELD-SYMBOLS: <fs_float_matrix> TYPE INDEX TABLE.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE.

    IF iv_from > iv_to.
      RAISE INPUT_ERROR.
    ENDIF.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_matrix EXPORTING rows = 0 cols = 0. "empty
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix>.
    ELSE.
      REFRESH lt_new_matrix.
      ASSIGN lt_new_matrix TO <fs_float_matrix>.
    ENDIF.

    IF iv_from IS NOT INITIAL AND iv_to IS NOT INITIAL.
      LOOP AT mt_float_matrix ASSIGNING <fs_row> FROM iv_from TO iv_to.
        APPEND <fs_row> TO <fs_float_matrix>.
      ENDLOOP.

    ELSEIF it_indexes[] IS NOT INITIAL.
      LOOP AT it_indexes INTO lv_index.
        READ TABLE mt_float_matrix ASSIGNING <fs_row> INDEX lv_index.
        IF sy-subrc = 0.
          APPEND <fs_row> TO <fs_float_matrix>.
        ENDIF.
      ENDLOOP.

    ELSE.
      RAISE INPUT_ERROR.

    ENDIF.

    IF iv_inplace = abap_false.
      ro_matrix->mv_rows = LINES( <fs_float_matrix> ).
      ro_matrix->mv_cols = me->mv_cols.
    ELSE.
      me->set( it_table = lt_new_matrix iv_check = ' ' ).
      ro_matrix = me.
    ENDIF.

  ENDMETHOD.

  METHOD operation_m.
    FIELD-SYMBOLS: <fs> TYPE ty_float,
                   <fs2> TYPE ty_float,
                   <fs_float_vector1> TYPE ty_float_vector,
                   <fs_float_vector2> TYPE ty_float_vector,
                   <fs_float_matrix1> TYPE ty_float_matrix,
                   <fs_float_matrix2> TYPE ty_float_matrix.
    DATA: lt_float_matrix_out TYPE ty_float_matrix.
    DATA: lr_data TYPE REF TO data.

    lr_data = io_matrix->get_ref( ).
    ASSIGN lr_data->* TO <fs_float_matrix2>.

    IF io_matrix->nrows( ) <> mv_rows OR io_matrix->ncolumns( ) <> mv_cols.
      RAISE SIZE_ERROR.
    ENDIF.

    IF iv_inplace = abap_false.
      ro_matrix = zcl_matrix=>create_from( it_table = mt_float_matrix iv_check = ' ' ).
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix1>.
    ELSE.
      ASSIGN mt_float_matrix TO <fs_float_matrix1>.
    ENDIF.

    LOOP AT <fs_float_matrix1> ASSIGNING <fs_float_vector1>.
      READ TABLE <fs_float_matrix2> ASSIGNING <fs_float_vector2> INDEX sy-tabix.

      LOOP AT <fs_float_vector1> ASSIGNING <fs>.
        READ TABLE <fs_float_vector2> ASSIGNING <fs2> INDEX sy-tabix.

        CASE iv_op.
          WHEN '+'.
            <fs> = <fs> + <fs2>.
          WHEN '-'.
            <fs> = <fs> - <fs2>.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    IF iv_inplace = abap_true.
      ro_matrix = me.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    ro_matrix = me->operation_m( io_matrix = io_matrix iv_inplace = iv_inplace iv_op = '+' ).
  ENDMETHOD.

  METHOD subtract.
    ro_matrix = me->operation_m( io_matrix = io_matrix iv_inplace = iv_inplace iv_op = '-' ).
  ENDMETHOD.

  METHOD multiply.
    DATA: m TYPE i.
    DATA: n TYPE i.
    DATA: k TYPE i.
    DATA: i TYPE i.
    DATA: j TYPE i.
    DATA: l TYPE i.
    DATA: val TYPE ty_float.
    DATA: val1 TYPE ty_float.
    DATA: val2 TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lt_new_vector TYPE ty_float_vector.
    DATA: lt_new_matrix TYPE ty_float_matrix.
    "DATA: lo_row TYPE REF TO zcl_vector.
    "DATA: lo_column TYPE REF TO zcl_vector.
    DATA: lv_first TYPE i.
    DATA: lr_data TYPE REF TO DATA.
    FIELD-SYMBOLS: <fs_float_matrix> TYPE ty_float_matrix.

    m = mv_rows.
    k = mv_cols.
    IF k <> io_matrix->nrows( ).
      RAISE SIZE_ERROR.
    ENDIF.
    n = io_matrix->ncolumns( ).

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_matrix EXPORTING rows = 0 cols = 0. "empty
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix>.
      REFRESH <fs_float_matrix>.
    ELSE.
      REFRESH lt_new_matrix.
      ASSIGN lt_new_matrix TO <fs_float_matrix>.
    ENDIF.

    DO m TIMES.
      i = sy-index.
      CLEAR: lt_new_vector.
      "lo_row = me->get_row( i ).
      DO n TIMES.
        j = sy-index.
        "lo_column = io_matrix->get_column( j ).
        CLEAR val.
        DO k TIMES.
          l = sy-index.
          TRY.
            val = val + me->get_pos( i = i j = l ) * io_matrix->get_pos( i = l j = j ).
            CATCH cx_root.
              EXIT.
          ENDTRY.
"          val = val + lo_row->dot( lo_column ).
        ENDDO.
        APPEND val TO lt_new_vector.
      ENDDO.
      APPEND lt_new_vector TO <fs_float_matrix>.
    ENDDO.

    IF iv_inplace = abap_false.
      ro_matrix->mv_rows = m.
      ro_matrix->mv_cols = n.
    ELSE.
      me->set( it_table = lt_new_matrix iv_check = ' ' ).
      ro_matrix = me.
    ENDIF.
  ENDMETHOD.

  METHOD multiplyv.
    DATA: m TYPE i.
    DATA: n TYPE i.
    DATA: i TYPE i.
    DATA: lo_row TYPE REF TO zcl_vector.

    m = mv_rows.
    n = mv_cols.
    IF n <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    CREATE OBJECT ro_vector EXPORTING size = m.

    DO m TIMES.
      i = sy-index.
      lo_row = me->get_row( i ).
      ro_vector->set_value( iv_index = i iv_value = lo_row->dot( io_vector ) ).
    ENDDO.
  ENDMETHOD.

  METHOD multiplyn.
    FIELD-SYMBOLS: <fs> TYPE ty_float,
                   <fs_float_vector> TYPE ty_float_vector,
                   <fs_float_matrix> TYPE ty_float_matrix.
    DATA: lt_float_matrix_out TYPE ty_float_matrix.
    DATA: lr_data TYPE REF TO data.

    IF iv_inplace = abap_false.
      ro_matrix = zcl_matrix=>create_from( it_table = mt_float_matrix iv_check = ' ' ).
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix>.
    ELSE.
      ASSIGN mt_float_matrix TO <fs_float_matrix>.
    ENDIF.

    LOOP AT <fs_float_matrix> ASSIGNING <fs_float_vector>.
      LOOP AT <fs_float_vector> ASSIGNING <fs>.
        <fs> = <fs> * iv_value.
      ENDLOOP.
    ENDLOOP.

    IF iv_inplace = abap_true.
      ro_matrix = me.
    ENDIF.
  ENDMETHOD.

  METHOD transpose.
    DATA:   lt_float_vector TYPE ty_float_vector,
            lv_float_item TYPE ty_float,
            lv_vector_size TYPE i,
            lv_current_index TYPE i,
            lt_transposed_vector TYPE ty_float_vector,
            lt_transposed_matrix TYPE ty_float_matrix.

    CHECK NOT mt_float_matrix[] IS INITIAL.
    READ TABLE mt_float_matrix INTO lt_float_vector INDEX 1.
    DESCRIBE TABLE lt_float_vector LINES lv_vector_size.

    DO lv_vector_size TIMES.
      lv_current_index = sy-index.
      CLEAR lt_transposed_vector.
      LOOP AT mt_float_matrix INTO lt_float_vector.
        READ TABLE lt_float_vector INTO lv_float_item INDEX lv_current_index.
        APPEND lv_float_item TO lt_transposed_vector.
      ENDLOOP.
      APPEND lt_transposed_vector TO lt_transposed_matrix.
    ENDDO.

    IF iv_inplace = abap_true.
      mt_float_matrix[] = lt_transposed_matrix[].
      mv_cols = mv_rows.
      mv_rows = LINES( mt_float_matrix ).
      ro_matrix = me.
    ELSE.
      ro_matrix = zcl_matrix=>create_from( lt_transposed_matrix ).
    ENDIF.
  ENDMETHOD.

  METHOD solve.
* Решение уравнения Ах = b, где А - наша матрица, b - входящий вектор
* Размерности: А = n * n (иначе не работает), b = 1 * n (иначе не работает)
    DATA: i TYPE i,
          j TYPE i,
          k TYPE i.
    DATA: lv_diag_k_value TYPE ty_float,
          lv_vect_k_value TYPE ty_float,
          lv_max_value TYPE ty_float,
          lv_max_index TYPE i,
          lv_value TYPE ty_float,
          lv_koef TYPE ty_float,
          lv_sum TYPE ty_float.
    DATA: lo_copy_matrix TYPE REF TO zcl_matrix,
          lo_copy_vector TYPE REF TO zcl_vector,
          lo_temp TYPE REF TO zcl_vector.
    DATA: lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_table> TYPE INDEX TABLE.


    IF mv_rows <> mv_cols OR mv_rows <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_copy_matrix = zcl_matrix=>create_from( it_table = mt_float_matrix iv_check = ' ' ).
    lo_copy_vector = zcl_vector=>create_from( it_column = io_vector->get( ) ).
    CREATE OBJECT ro_result EXPORTING size = mv_rows.

* Прямой ход. Приводим матрицу к треугольному виду
    DO mv_rows TIMES.
      k = sy-index.

      " определение максимального элемента в k-столбце
      lv_max_value = abs( me->get_pos( i = k j = k ) ).
      lv_max_index = k.
      i = k + 1.
      WHILE i <= mv_rows.
        lv_value = abs( me->get_pos( i = i j = k ) ).
        IF lv_value > lv_max_value.
          lv_max_value = lv_value.
          lv_max_index = i.
        ENDIF.
        i = i + 1.
      ENDWHILE.

      " ошибка ранга, нет ненулевых диаг.элементов, матрица плохо обусловлена )
      IF lv_max_value < '0.000001'.
        RAISE RANK_ERROR.
      ENDIF.

      " перестановка lv_max_index и k строки
      lo_temp = me->get_row( lv_max_index ).
      me->set_row( iv_index = lv_max_index io_vector = me->get_row( k ) ).
      me->set_row( iv_index = k io_vector = lo_temp ).
      lv_value = io_vector->get_value( lv_max_index ).
      io_vector->set_value( iv_index = lv_max_index iv_value = io_vector->get_value( k ) ).
      io_vector->set_value( iv_index = k iv_value = lv_value ).

      " выполняем преобразования
      lv_diag_k_value = me->get_pos( i = k j = k ).
      lv_vect_k_value = io_vector->get_value( k ).
      j = sy-index + 1.
      WHILE j <= mv_rows.
        lv_koef = me->get_pos( i = j j = k ) / lv_diag_k_value.
        i = k.
        WHILE i <= mv_rows.
          lv_value = me->get_pos( i = j j = i ) - lv_koef * me->get_pos( i = k j = i ).
          me->set_pos( i = j j = i iv_value = lv_value ).
          i = i + 1.
        ENDWHILE.
        lv_value = io_vector->get_value( j ) - lv_koef * lv_vect_k_value.
        io_vector->set_value( iv_index = j iv_value = lv_value ).

        j = j + 1.
      ENDWHILE.
    ENDDO.

* Обратный ход, вычиляем коэффициенты
    DO mv_rows TIMES.  " k = n..1
      k = mv_rows - sy-index + 1.
      lv_sum = 0.
      j = k + 1.
      WHILE j <= mv_rows.
        lv_sum = lv_sum + me->get_pos( i = k j = j ) * ro_result->get_value( j ).
        j = j + 1.
      ENDWHILE.

      lv_value = ( io_vector->get_value( k ) - lv_sum ) / me->get_pos( i = k j = k ).
      ro_result->set_value( iv_index = k iv_value = lv_value ).
    ENDDO.

* Вернем значение матрицы и вектора в исходное
    lr_data = lo_copy_matrix->get_ref( ).
    ASSIGN lr_data->* TO <fs_table>.

    me->set( it_table = <fs_table> iv_check = ' ' ).
    io_vector->set( it_column = lo_copy_vector->get( ) ).
    FREE lo_copy_matrix.
    FREE lo_copy_vector.

  ENDMETHOD.

  METHOD min.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_row TYPE i.

    rv_value = 0.
    LOOP AT mt_float_matrix INTO lt_float_vector.
      lv_row = sy-tabix.
      LOOP AT lt_float_vector INTO lv_float.
        IF ( lv_row = 1 AND sy-tabix = 1 ) OR ( lv_float < rv_value ).
          rv_value = lv_float.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD max.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_row TYPE i.

    rv_value = 0.
    LOOP AT mt_float_matrix INTO lt_float_vector.
      lv_row = sy-tabix.
      LOOP AT lt_float_vector INTO lv_float.
        IF ( lv_row = 1 AND sy-tabix = 1 ) OR ( lv_float > rv_value ).
          rv_value = lv_float.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD print.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.

    LOOP AT mt_float_matrix INTO lt_float_vector.
      LOOP AT lt_float_vector INTO lv_float.
        WRITE: lv_float.
      ENDLOOP.
      NEW-LINE.
    ENDLOOP.
  ENDMETHOD.

  METHOD load_from_csv.
    DATA: lt_strings TYPE table_of_strings.
    DATA: lv_string TYPE string.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = iv_filename
      TABLES
        data_tab                = lt_strings
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0 OR LINES( lt_strings ) = 0.
      RAISE INPUT_ERROR.
    ENDIF.

    IF iv_header = abap_true.
      DELETE lt_strings INDEX 1.
    ENDIF.

    ro_matrix = load_from_text( it_strings = lt_strings iv_sep = iv_sep ).
  ENDMETHOD.

  METHOD load_from_text.
    DATA: lv_string TYPE string.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lt_rawfields TYPE TABLE OF string.
    DATA: lv_rawfield TYPE string.
    DATA: lv_float TYPE ty_float.
    DATA: lv_first TYPE i.

    IF LINES( it_strings ) = 0.
      RAISE INPUT_ERROR.
    ENDIF.


    LOOP AT it_strings INTO lv_string.
      CLEAR: lt_rawfields, lt_float_vector.

      SPLIT lv_string  AT iv_sep INTO TABLE lt_rawfields.
      LOOP AT lt_rawfields INTO lv_rawfield.
        TRY.
            lv_float = lv_rawfield.
          CATCH cx_root.
            RAISE INPUT_ERROR.
        ENDTRY.
        APPEND lv_float TO lt_float_vector.
      ENDLOOP.

      IF lv_first = 0.
        ro_matrix = zcl_matrix=>create_from( lt_float_vector ).
        ro_matrix->transpose( iv_inplace = 'X' ).
        lv_first = 1.
      ELSE.
        ro_matrix->append_row( zcl_vector=>create_from( lt_float_vector ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_csv.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lt_strings TYPE table_of_strings.
    DATA: lv_string TYPE string.
    DATA: lv_strval(30).

    LOOP AT mt_float_matrix INTO lt_float_vector.
      LOOP AT lt_float_vector INTO lv_float.
        WRITE lv_float TO lv_strval LEFT-JUSTIFIED.
        IF lv_string IS INITIAL.
          lv_string = lv_strval.
        ELSE.
          CONCATENATE lv_string lv_strval INTO lv_string SEPARATED BY iv_sep.
        ENDIF.
      ENDLOOP.
      APPEND lv_string TO lt_strings.
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = iv_filename
        write_field_separator   = iv_sep
      TABLES
        data_tab                = lt_strings
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      RAISE OUTPUT_ERROR.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* PART3: REGRESSION
*----------------------------------------------------------------------*

* Base abstract class
*----------------------------------------------------------------------*
CLASS zcl_ml_regression DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor,
      fit IMPORTING io_X TYPE REF TO zcl_matrix  io_y TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR SOLVE_ERROR WRONG_LABELS,
      predict IMPORTING io_X TYPE REF TO zcl_matrix RETURNING VALUE(ro_y_pred) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR KOEFS_ERROR,
      get_coefs RETURNING VALUE(ro_coefs) TYPE REF TO zcl_vector,
      get_predict_score IMPORTING io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS SIZE_ERROR KOEFS_ERROR.

  PROTECTED SECTION.
    DATA:
      mo_coefs TYPE REF TO zcl_vector.
ENDCLASS.

CLASS zcl_ml_regression IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD fit.
  ENDMETHOD.

  METHOD predict.
  ENDMETHOD.

* return copy of coefs
  METHOD get_coefs.
    ro_coefs = zcl_vector=>create_copy( mo_coefs ).
  ENDMETHOD.

* return coefficient of determination R^2 of the prediction
  METHOD get_predict_score.
    DATA: u TYPE ty_float.
    DATA: v TYPE ty_float.
    DATA: lo_y_pred TYPE REF TO zcl_vector.
    DATA: lo_y_mean TYPE REF TO zcl_vector.
    DATA: lv_mean TYPE ty_float.

    IF mo_coefs IS NOT BOUND.
      RAISE KOEFS_ERROR.
    ENDIF.

    lo_y_pred = predict( io_X ).
    u = io_y->subtract( lo_y_pred )->pown( 2 )->sum( ).

    lv_mean = io_y->mean( ).
    CREATE OBJECT lo_y_mean EXPORTING size = io_y->size( ) fill_value = lv_mean.
    v = io_y->subtract( lo_y_mean )->pown( 2 )->sum( ).

    rv_value = 1 - ( u / v ).
  ENDMETHOD.
ENDCLASS.

* (Weighted) Ordinary Least Squares
*----------------------------------------------------------------------*
CLASS zcl_ml_linear_regression DEFINITION INHERITING FROM zcl_ml_regression.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_ridge_coef TYPE ty_float DEFAULT 0,  " iv_coef = 0 - OLS; iv_coef > 0 - ridge regression
      fit REDEFINITION,
      predict REDEFINITION.

  PROTECTED SECTION.
    DATA:
      mv_ridge_coef TYPE ty_float.
ENDCLASS.

CLASS zcl_ml_linear_regression IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_ridge_coef = iv_ridge_coef.
  ENDMETHOD.

*  solve equation (X^T*X)coefs = X^T*y
  METHOD fit.
    DATA: lo_transpose_X TYPE REF TO zcl_matrix.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_ones_column TYPE REF TO zcl_vector.
    DATA: lo_diag_matrix TYPE REF TO zcl_matrix.

    IF io_X->nrows( ) = 0 OR io_y->size( ) = 0 OR io_X->nrows( ) <> io_y->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones_column EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones_column ).

    IF mv_ridge_coef > 0.
      " solve equation (X^T*X + c*I)coefs = X^T*y
      lo_transpose_X = lo_X->transpose( ).
      lo_diag_matrix = zcl_matrix=>create_diag( iv_size = lo_X->ncolumns( ) iv_value = mv_ridge_coef ).
      mo_coefs = lo_transpose_X->multiply( lo_X )->add( lo_diag_matrix )->solve( io_vector = lo_transpose_X->multiplyv( io_y ) ).
    ELSE.
      " solve equation (X^T*X)coefs = X^T*y
      lo_transpose_X = lo_X->transpose( ).
      mo_coefs = lo_transpose_X->multiply( lo_X )->solve( io_vector = lo_transpose_X->multiplyv( io_y ) ).
    ENDIF.
  ENDMETHOD.

* y = X * coefs
  METHOD predict.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_column TYPE REF TO zcl_vector.
    DATA: lo_ones TYPE REF TO zcl_vector.
    DATA: i TYPE i.

    IF mo_coefs IS NOT BOUND.
      RAISE KOEFS_ERROR.
    ENDIF.

    IF io_X->ncolumns( ) + 1 <> mo_coefs->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones ).

    ro_y_pred = lo_X->multiplyv( mo_coefs ).
  ENDMETHOD.

ENDCLASS.

* Logistic regression, analytic solution based on iterative algorithm
*----------------------------------------------------------------------*
CLASS zcl_ml_logistic_regression DEFINITION INHERITING FROM zcl_ml_regression.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_ridge_coef TYPE ty_float DEFAULT 0 iv_eps TYPE ty_float DEFAULT '0.0001',
      fit REDEFINITION,
      predict REDEFINITION,
      get_info EXPORTING ev_steps TYPE i
                         et_conv_info TYPE wdy_key_value_list.

  PROTECTED SECTION.
    DATA:
      mv_ridge_coef TYPE ty_float,
      mv_eps TYPE ty_float,
      mv_steps TYPE i,                       " number of steps
      mt_conv_info TYPE wdy_key_value_list.  " step-by-step convergency information
ENDCLASS.

CLASS zcl_ml_logistic_regression IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_ridge_coef = iv_ridge_coef.
    mv_eps = iv_eps.
  ENDMETHOD.

  METHOD fit.
    DATA: lo_transpose_X TYPE REF TO zcl_matrix.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_ones_column TYPE REF TO zcl_vector.
    DATA: lo_diag_matrix TYPE REF TO zcl_matrix.
    DATA: lo_diag_W TYPE REF TO zcl_matrix.
    DATA: lv_ok TYPE flag.
    DATA: lo_z TYPE REF TO zcl_vector.
    DATA: lo_p TYPE REF TO zcl_vector.
    DATA: lo_w TYPE REF TO zcl_vector.
    DATA: lo_u TYPE REF TO zcl_vector.
    DATA: lo_koefs_old TYPE REF TO zcl_vector.
    DATA: lv_value TYPE ty_float.
    DATA: lv_diff TYPE ty_float.
    DATA: ls_info LIKE LINE OF mt_conv_info.
    DATA: lv_char(30) TYPE c.
    DATA: lo_upd_w TYPE REF TO zcl_vector.
    DATA: lt_indexes TYPE int4_table.
    DATA: lv_index TYPE i.

    IF io_X->nrows( ) = 0 OR io_y->size( ) = 0 OR io_X->nrows( ) <> io_y->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

* add column for constant feature
    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones_column EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones_column ).

    CREATE OBJECT mo_coefs EXPORTING size = lo_X->ncolumns( ).

* first coef approximation: [ lv_value, 0, 0, ... 0 ]
    lv_value = log( io_y->mean( ) / ( '1.0' - io_y->mean( ) ) ).
    mo_coefs->set_value( iv_index = 1 iv_value = lv_value ).

* iterative algorithm
    WHILE lv_ok = abap_false AND mv_steps < 10000 .
      lo_z = lo_X->multiplyv( mo_coefs ).                           " z = Xk
      lo_p = lo_z->apply_func( 'sigmoid' ).                     " p = sigmoid(z)
      lo_w = lo_p->multv( lo_p->multn( -1 )->addn( 1 ) ).       " w = p * (1-p)

      " in order to avoid division by zero:
      lo_upd_w = zcl_vector=>create_copy( lo_w ).
      lt_indexes = lo_w->get_indexes( 0 ).
      LOOP AT lt_indexes INTO lv_index.
        lo_upd_w->set_value( iv_index = lv_index iv_value = '0.01' ).
      ENDLOOP.

      lo_u = lo_z->add( io_y->subtract( lo_p )->divv( lo_upd_w ) ). " u = z + (y-p)/w
      lo_koefs_old = zcl_vector=>create_copy( mo_coefs ).

      lo_transpose_X = lo_X->transpose( ).
      lo_diag_W = zcl_matrix=>create_diag( iv_size = lo_w->size( ) io_vector = lo_w ).

      IF mv_ridge_coef > 0.
        " solve equation (X^T*W*X+cI)coefs = X^T*W*u
        lo_diag_matrix = zcl_matrix=>create_diag( iv_size = lo_X->ncolumns( ) iv_value = mv_ridge_coef ).
        mo_coefs = lo_transpose_X->multiply( lo_diag_W )->multiply( lo_X )->add( lo_diag_matrix )->solve( io_vector = lo_transpose_X->multiply( lo_diag_W )->multiplyv( lo_u ) ).
      ELSE.
        " solve equation (X^T*W*X)coefs = X^T*W*u
        mo_coefs = lo_transpose_X->multiply( lo_diag_W )->multiply( lo_X )->solve( io_vector = lo_transpose_X->multiply( lo_diag_W )->multiplyv( lo_u ) ).
      ENDIF.

      lv_diff = lo_koefs_old->subtract( mo_coefs )->pown( 2 )->sum( ).
      IF lv_diff < mv_eps.
        lv_ok = abap_true.
      ENDIF.

      mv_steps = mv_steps + 1.

      " log convergence info
      ls_info-key = mv_steps.
      WRITE lv_diff TO lv_char LEFT-JUSTIFIED DECIMALS 8.
      ls_info-value = lv_char.
      APPEND ls_info TO mt_conv_info.
    ENDWHILE.

  ENDMETHOD.

* y = sigmoid(X * coefs)
  METHOD predict.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_column TYPE REF TO zcl_vector.
    DATA: lo_ones TYPE REF TO zcl_vector.
    DATA: i TYPE i.

    IF mo_coefs IS NOT BOUND.
      RAISE KOEFS_ERROR.
    ENDIF.

    IF io_X->ncolumns( ) + 1 <> mo_coefs->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones ).

    ro_y_pred = lo_X->multiplyv( mo_coefs )->apply_func( 'sigmoid' ).
  ENDMETHOD.

  METHOD get_info.
    ev_steps = mv_steps.
    et_conv_info = mt_conv_info.
  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
* PART4: GRADIENT DESCENT
*----------------------------------------------------------------------*

CLASS zcl_ml_sgd DEFINITION.
  PUBLIC SECTION.
    METHODS:
      minimize IMPORTING iv_str_func TYPE c                           " can be any custom function, implemented as class method
                         iv_str_func_grad TYPE c OPTIONAL             " can be omitted, so grad will be calculated numerically
                         io_X TYPE REF TO zcl_matrix OPTIONAL         " in case of ML problem - X matrix takes part in loss function, and therefore should be passed via parameter
                         io_y TYPE REF TO zcl_vector OPTIONAL         " in case of ML problem - y vector takes part in loss function, and therefore should be passed via parameter
                         ir_args TYPE REF TO DATA OPTIONAL            " any additional data in implementation of your custom function
                         iv_batch_size TYPE ty_float DEFAULT 0        " <> 0 - for stochastic grad.descent (part of X matrix for one batch), 0 - usual grad.descend
                         iv_batch_all TYPE flag DEFAULT ' '           " used for SGD mode, ' ' - batch selected randomly once per epoch, 'X' - epoch consist of several batches and covered all data
                         iv_learning_rate TYPE ty_float DEFAULT '0.1' " coefficient of gradient
                         iv_eps TYPE ty_float DEFAULT '0.0001'        " stop-criteria, minimun difference of loss functions between epochs
                         iv_max_steps TYPE i DEFAULT 100              " stop-criteria, maximum number of iterations
                         iv_random_seed TYPE i DEFAULT 1              " random seed
                         io_w0 TYPE REF TO zcl_vector OPTIONAL        " initial guess
               EXPORTING ev_ok TYPE flag                              " True - if convergency is reached
                         eo_result TYPE REF TO zcl_vector             " result vector
              EXCEPTIONS UNKNOWN_FUNC INPUT_ERROR SOLVE_ERROR,

      get_result RETURNING VALUE(ro_result) TYPE REF TO zcl_vector,   " get result vector
      get_info EXPORTING ev_steps TYPE i                              " get step-by-step convergency information
                         et_conv_info TYPE wdy_key_value_list,
      constructor.

    CLASS-METHODS:
      test IMPORTING io_w TYPE REF TO zcl_vector RETURNING VALUE(rv_value) TYPE ty_float.   " Test function, use string 'test' to pass it "iv_str_func" parameter

  PRIVATE SECTION.
    DATA:
      mo_result TYPE REF TO zcl_vector,
      mv_steps TYPE i,
      mt_conv_info TYPE wdy_key_value_list,
      mv_str_func TYPE string,
      mv_class TYPE string,
      mv_method TYPE string,
      mv_str_func_grad TYPE string,
      mv_class_grad TYPE string,
      mv_method_grad TYPE string,
      mv_batch_all TYPE flag,
      mv_batch_passes TYPE i,
      mv_batch_size TYPE ty_float,
      mv_batch_curr TYPE i,
      mo_shuffle_indexes TYPE REF TO zcl_vector.
    METHODS:
      parse_str_func IMPORTING iv_str_func TYPE c
                      CHANGING cv_str_func TYPE string
                               cv_class TYPE string
                               cv_method TYPE string
                      EXCEPTIONS UNKNOWN_FUNC INPUT_ERROR,
      get_next_batch IMPORTING io_X TYPE REF TO zcl_matrix
                               io_y TYPE REF TO zcl_vector
                               iv_random_seed TYPE i
                     EXPORTING ro_X TYPE REF TO zcl_matrix
                               ro_y TYPE REF TO zcl_vector
                               ro_done TYPE flag,
      update_batch IMPORTING io_w TYPE REF TO zcl_vector
                             io_X TYPE REF TO zcl_matrix
                             io_y TYPE REF TO zcl_vector
                             ir_args TYPE REF TO DATA
                             iv_learning_rate TYPE ty_float
                             iv_eps TYPE ty_float
                   EXPORTING rv_loss_value TYPE ty_float,

      run_func IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(rv_value) TYPE ty_float,
      run_grad_func IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      calculate_grad_numerically IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector.
ENDCLASS.

CLASS zcl_ml_sgd IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD run_func.
    CASE mv_str_func.
      WHEN 'test'.
        rv_value = test( io_w = io_w ).

      WHEN OTHERS.
        CALL METHOD (mv_class)=>(mv_method)
          EXPORTING io_w = io_w
                    io_X = io_X
                    io_y = io_y
                    ir_args = ir_args
          RECEIVING rv_value = rv_value.
    ENDCASE.
  ENDMETHOD.

  METHOD run_grad_func.
    IF mv_class_grad IS NOT INITIAL.
      CALL METHOD (mv_class_grad)=>(mv_method_grad)
        EXPORTING io_w = io_w
                  io_X = io_X
                  io_y = io_y
                  ir_args = ir_args
        RECEIVING ro_vector = ro_vector.
    ELSE.
      ro_vector = calculate_grad_numerically( io_w = io_w io_X = io_X io_y = io_y ir_args = ir_args ).
    ENDIF.
  ENDMETHOD.

  METHOD calculate_grad_numerically.
    DATA: lv_eps TYPE ty_float VALUE '0.000001'.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lo_y_pred TYPE REF TO zcl_vector.
    DATA: lv_copy_value TYPE ty_float.
    DATA: lv_index TYPE i.
    DATA: lv_value TYPE ty_float.
    DATA: lv_delta_plus TYPE ty_float.
    DATA: lv_delta_minus TYPE ty_float.

    DO io_w->size( ) TIMES.
      lv_index = sy-index.
      lv_copy_value = io_w->get_value( lv_index ).

      lv_value = lv_copy_value + lv_eps.
      io_w->set_value( iv_index = lv_index iv_value = lv_value ).
      lv_delta_plus = run_func( io_w = io_w io_X = io_X io_y = io_y ir_args = ir_args ).

      lv_value = lv_copy_value - lv_eps.
      io_w->set_value( iv_index = lv_index iv_value = lv_value ).
      lv_delta_minus = run_func( io_w = io_w io_X = io_X io_y = io_y ir_args = ir_args ).

      lv_value = ( lv_delta_plus - lv_delta_minus ) / ( 2 * lv_eps ).
      APPEND lv_value TO lt_float_vector.
      io_w->set_value( iv_index = lv_index iv_value = lv_copy_value ).
    ENDDO.
    ro_vector = zcl_vector=>create_from( lt_float_vector ).
  ENDMETHOD.

  METHOD parse_str_func.
    FIND '=>' IN iv_str_func.
    IF sy-subrc = 0.
      SPLIT iv_str_func AT '=>' INTO cv_class cv_method.
      IF sy-subrc = 4 OR cv_class IS INITIAL OR cv_method IS INITIAL.
        RAISE INPUT_ERROR.
      ENDIF.
      TRY.
          CALL METHOD (cv_class)=>(cv_method).
        CATCH cx_sy_dyn_call_illegal_class cx_sy_dyn_call_illegal_method.
          RAISE UNKNOWN_FUNC.
        CATCH cx_sy_dyn_call_param_missing.
          . "OK
        CATCH cx_root.
          RAISE UNKNOWN_FUNC.
      ENDTRY.
      cv_str_func = iv_str_func.
    ELSEIF iv_str_func = 'test'.
      cv_str_func = iv_str_func.
    ELSE.
      RAISE UNKNOWN_FUNC.
    ENDIF.
  ENDMETHOD.

  METHOD get_next_batch.
    DATA: lv_get TYPE i.
    DATA: lv_val TYPE i.
    DATA: lt_indexes TYPE HASHED TABLE OF int4 WITH UNIQUE DEFAULT KEY.
    DATA: lt_simple_indexes TYPE int4_table.
    DATA: lo_rgen TYPE REF TO cl_abap_random_int.
    DATA: lv_index TYPE i.
    DATA: lv_from TYPE i.
    DATA: lv_to TYPE i.
    DATA: lo_batch_indexes TYPE REF TO zcl_vector.

    IF mv_batch_size = 0.
      ro_X = io_X.
      ro_y = io_y.
      ro_done = abap_true.
      RETURN.
    ENDIF.

    lo_rgen = cl_abap_random_int=>create( min = 1 max = io_X->nrows( ) seed = iv_random_seed ).

    IF mv_batch_all = abap_false.

      WHILE lv_get < mv_batch_size.
        lv_val = lo_rgen->get_next( ).
        READ TABLE lt_indexes TRANSPORTING NO FIELDS WITH TABLE KEY table_line = lv_val.
        IF sy-subrc <> 0.
          INSERT lv_val INTO TABLE lt_indexes.
          lv_get = lv_get + 1.
        ENDIF.
      ENDWHILE.

      lt_simple_indexes = zcl_vector=>create_from( lt_indexes )->get( ).
      ro_X = io_X->slice_by_rows( it_indexes = lt_simple_indexes ).
      ro_y = io_y->slice_by_index( it_indexes = lt_simple_indexes ).
      ro_done = abap_true.

    ELSE.

      IF mv_batch_curr = mv_batch_passes.
        IF mv_batch_passes = 0.
          "mv_batch_passes = io_X->nrows( ) * '1.0' / mv_batch_size.
          mv_batch_passes = round( val = io_X->nrows( ) * '1.0' / mv_batch_size
                                   dec = 0
                                   mode = cl_abap_math=>round_half_up ).
        ENDIF.

        mv_batch_curr = 1.
        mo_shuffle_indexes = zcl_vector=>create_range( iv_from = 1 iv_to = io_X->nrows( ) ).
        mo_shuffle_indexes->shuffle( iv_seed = iv_random_seed iv_inplace = 'X' ).
      ELSE.
        mv_batch_curr = mv_batch_curr + 1.
      ENDIF.

      IF mv_batch_curr < mv_batch_passes.
        lv_from = mv_batch_size * ( mv_batch_curr - 1 ) + 1.
        lv_to   = mv_batch_size * mv_batch_curr.
        lo_batch_indexes = mo_shuffle_indexes->slice_by_index( iv_from = lv_from iv_to = lv_to ).
        ro_done = abap_false.
      ELSE.
        lv_from = mv_batch_size * ( mv_batch_curr - 1 ) + 1.
        lv_to   = io_X->nrows( ).
        lo_batch_indexes = mo_shuffle_indexes->slice_by_index( iv_from = lv_from iv_to = lv_to ).
        ro_done = abap_true.
      ENDIF.

      lt_simple_indexes = lo_batch_indexes->get( ).
      ro_X = io_X->slice_by_rows( it_indexes = lt_simple_indexes ).
      ro_y = io_y->slice_by_index( it_indexes = lt_simple_indexes ).

    ENDIF.

  ENDMETHOD.

  METHOD update_batch.
    DATA: lv_target_value_new TYPE ty_float.
    DATA: lo_grad TYPE REF TO zcl_vector.
    DATA: lv_size TYPE ty_float.
    DATA: ls_info LIKE LINE OF mt_conv_info.

    lo_grad = run_grad_func( io_w = io_w io_X = io_X io_y = io_y ir_args = ir_args ).
    lo_grad = lo_grad->multn( iv_learning_rate ).
    io_w->subtract( it_vector = lo_grad iv_inplace = 'X' ).
    lv_target_value_new = run_func( io_w = io_w io_X = io_X io_y = io_y ir_args = ir_args ).

    IF mv_batch_passes = 0.
      ls_info-key = mv_steps.
    ELSE.
      ls_info-key = mv_steps && '(' && mv_batch_curr && ')'.
    ENDIF.
    DATA: lv_char(30) TYPE c.
    WRITE lv_target_value_new TO lv_char LEFT-JUSTIFIED DECIMALS 8.
    ls_info-value = lv_char.
    APPEND ls_info TO mt_conv_info.

    rv_loss_value = lv_target_value_new.
  ENDMETHOD.

  METHOD minimize.
    DATA: lv_index TYPE i.
    DATA: lo_X_part TYPE REF TO zcl_matrix.
    DATA: lo_y_part TYPE REF TO zcl_vector.
    DATA: lv_ok TYPE flag.
    DATA: lv_loss_up TYPE i.
    DATA: lv_loss_value TYPE ty_float.
    DATA: lv_prev_loss_value TYPE ty_float.
    DATA: lv_diff TYPE ty_float.
    DATA: lv_done TYPE flag.

    parse_str_func(
      EXPORTING iv_str_func = iv_str_func
       CHANGING cv_str_func = mv_str_func
                cv_class    = mv_class
                cv_method   = mv_method
      EXCEPTIONS OTHERS      = 1 ).
    IF sy-subrc = 1.
      RAISE UNKNOWN_FUNC.
    ENDIF.

    IF iv_str_func_grad IS NOT INITIAL.
      parse_str_func(
        EXPORTING iv_str_func = iv_str_func_grad
         CHANGING cv_str_func = mv_str_func_grad
                  cv_class    = mv_class_grad
                  cv_method   = mv_method_grad
        EXCEPTIONS OTHERS      = 1 ).
      IF sy-subrc = 1.
        RAISE UNKNOWN_FUNC.
      ENDIF.
    ENDIF.

    mv_steps = 0.
    mv_batch_all = iv_batch_all.
    REFRESH mt_conv_info.
    ev_ok = abap_false.
    FREE mo_result.
    IF io_w0 IS BOUND.
      mo_result = zcl_vector=>create_copy( io_w0 ).
    ELSE.
      CREATE OBJECT mo_result EXPORTING size = io_X->ncolumns( ).
    ENDIF.

    IF iv_batch_size <> 0.
      IF NOT ( io_X IS BOUND AND io_y IS BOUND ).
        mv_batch_size = 0.
      ELSEIF iv_batch_size > io_X->nrows( ).
        mv_batch_size = 0.
      ELSEIF iv_batch_size >= 1.
        mv_batch_size = iv_batch_size.
      ELSEIF iv_batch_size < 1.
        mv_batch_size = iv_batch_size * io_X->nrows( ).
        mv_batch_size = nmax( val1 = mv_batch_size val2 = 1 ).
      ENDIF.
    ENDIF.
    mv_batch_passes = 0.
    mv_batch_curr = 0.
    FREE mo_shuffle_indexes.


    WHILE mv_steps < iv_max_steps AND ev_ok = abap_false.
      mv_steps = mv_steps + 1.

      lv_done = abap_false.
      WHILE lv_done <> abap_true.

        get_next_batch(
          EXPORTING io_X           = io_X
                    io_y           = io_y
                    iv_random_seed = iv_random_seed
          IMPORTING ro_X           = lo_X_part
                    ro_y           = lo_y_part
                    ro_done        = lv_done ).

        update_batch(
          EXPORTING io_w             = mo_result
                    io_X             = lo_X_part
                    io_y             = lo_y_part
                    ir_args          = ir_args
                    iv_eps           = iv_eps
                    iv_learning_rate = iv_learning_rate
          IMPORTING rv_loss_value    = lv_loss_value ).

      ENDWHILE.

      lv_diff = abs( lv_loss_value - lv_prev_loss_value ).
      IF lv_diff < iv_eps.
        ev_ok = abap_true.
      ENDIF.

      IF mv_steps > 1 AND lv_loss_value > lv_prev_loss_value.
        ADD 1 TO lv_loss_up.
      ELSE.
        lv_loss_up = 0.
      ENDIF.
      IF lv_loss_up = 5.
        ev_ok = abap_false.
        EXIT.  " SOLVE_ERROR
      ENDIF.
      lv_prev_loss_value = lv_loss_value.

    ENDWHILE.
    eo_result = mo_result.

  ENDMETHOD.

  METHOD get_result.
    ro_result = mo_result.
  ENDMETHOD.

  METHOD get_info.
    ev_steps = mv_steps.
    et_conv_info = mt_conv_info.
  ENDMETHOD.

  METHOD test.
    " F(x) = (x1-1)^2 + (x2-2)^2 + (x3-3)^2 + (x4-4)^2 + (x5+5)^2 + (x6+6)^2 - 10
    " minimum at X = [1; 2; 3; 4; -5; -6]
    " min(F(x)) = -10
    rv_value = ( io_w->get_value( 1 ) - 1 ) ** 2 + ( io_w->get_value( 2 ) - 2 ) ** 2 + ( io_w->get_value( 3 ) - 3 ) ** 2 + ( io_w->get_value( 4 ) - 4 ) ** 2 + ( io_w->get_value( 5 ) + 5 ) ** 2 + ( io_w->get_value( 6 ) + 6 ) ** 2 - 10.
  ENDMETHOD.

ENDCLASS.


*----------------------------------------------------------------------*
* PART5: GRADIENT DESCENT MODELS
*----------------------------------------------------------------------*

* Base abstract class for SGD-models
*----------------------------------------------------------------------*
CLASS zcl_ml_sgd_regression DEFINITION INHERITING FROM zcl_ml_regression ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_l1_ratio TYPE ty_float DEFAULT 0      " L1-regularization coef
                            iv_l2_ratio TYPE ty_float DEFAULT '0.01' " L2-regularization coef
                            iv_batch_size TYPE ty_float DEFAULT 0         " <> 0 - for stochastic grad.descent (part of X matrix for one batch), 0 - usual grad.descend
                            iv_batch_all TYPE flag DEFAULT ' '            " used for SGD mode, ' ' - batch selected randomly once per epoch, 'X' - epoch consist of several batches and cover all data
                            iv_learning_rate TYPE ty_float DEFAULT '0.1'  " GD coefficient of gradient
                            iv_eps TYPE ty_float DEFAULT '0.0001'         " GD stop-criteria, minimun difference of loss functions between epochs
                            iv_max_steps TYPE i DEFAULT 100               " GD stop-criteria, maximum number of iterations
                            iv_random_seed TYPE i DEFAULT 1,              " random seed
      fit REDEFINITION,
      predict REDEFINITION,
      get_info EXPORTING ev_steps TYPE i                                  " get step-by-step convergency information
                         et_conv_info TYPE wdy_key_value_list.

    CONSTANTS:
      mc_field_l1 TYPE char10 VALUE 'L1',
      mc_field_l2 TYPE char10 VALUE 'L2',
      mc_field_y_pred TYPE char10 VALUE 'y_pred'.

  PROTECTED SECTION.
    DATA:
      mv_l1_ratio TYPE ty_float,
      mv_l2_ratio TYPE ty_float,
      mv_batch_size TYPE ty_float,
      mv_batch_all TYPE flag,
      mv_learning_rate TYPE ty_float,
      mv_eps TYPE ty_float,
      mv_max_steps TYPE i,
      mv_random_seed TYPE i,
      mv_steps TYPE i,
      mt_conv_info TYPE wdy_key_value_list,
      mt_scale TYPE TABLE OF ty_float,
      mv_loss_func TYPE text128,
      mv_loss_func_grad TYPE text128,
      mo_sgd TYPE REF TO zcl_ml_sgd.

    CLASS-METHODS:
      parse_ir_args IMPORTING ir_args TYPE REF TO DATA
                    EXPORTING ev_l1 TYPE ty_float
                              ev_l2 TYPE ty_float
                              er_yp TYPE REF TO zcl_vector.

    METHODS:
      scale_fit_transform CHANGING co_X TYPE REF TO zcl_matrix,
      scale_transform CHANGING co_X TYPE REF TO zcl_matrix.

ENDCLASS.

CLASS zcl_ml_sgd_regression IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    mv_l1_ratio = iv_l1_ratio.
    mv_l2_ratio = iv_l2_ratio.
    mv_batch_size = iv_batch_size.
    mv_batch_all = iv_batch_all.
    IF iv_learning_rate <= 0.
      mv_learning_rate = '0.001'.
    ELSE.
      mv_learning_rate = iv_learning_rate.
    ENDIF.
    mv_eps = iv_eps.
    mv_max_steps = iv_max_steps.
    mv_random_seed = iv_random_seed.

    mv_loss_func = ''.
    mv_loss_func_grad = ''.
  ENDMETHOD.

  METHOD parse_ir_args.
    FIELD-SYMBOLS: <fs_data> TYPE any.
    FIELD-SYMBOLS: <fs_field> TYPE any.

    ASSIGN ir_args->* TO <fs_data>.
    IF <fs_data> IS ASSIGNED.
      ASSIGN COMPONENT mc_field_l1 OF STRUCTURE <fs_data> TO <fs_field>.
      IF sy-subrc = 0.
        ev_l1 = <fs_field>.
      ENDIF.
      ASSIGN COMPONENT mc_field_l2 OF STRUCTURE <fs_data> TO <fs_field>.
      IF sy-subrc = 0.
        ev_l2 = <fs_field>.
      ENDIF.
      ASSIGN COMPONENT mc_field_y_pred OF STRUCTURE <fs_data> TO <fs_field>.
      IF sy-subrc = 0.
        IF <fs_field> IS BOUND.
          er_yp = <fs_field>.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD scale_fit_transform.
    DATA: lv_index TYPE i.
    DATA: lv_scale_value TYPE ty_float.
    DATA: lo_column TYPE REF TO zcl_vector.

    REFRESH mt_scale.

    DO co_X->ncolumns( ) TIMES.
      lv_index = sy-index.
      lo_column = co_X->get_column( lv_index ).
      lv_scale_value = nmax( val1 = abs( lo_column->min( ) )
                             val2 = abs( lo_column->max( ) ) ).
      lo_column = lo_column->divn( lv_scale_value ).
      co_X->set_column( iv_index = lv_index io_vector = lo_column ).
      APPEND lv_scale_value TO mt_scale.
    ENDDO.
  ENDMETHOD.

  METHOD scale_transform.
    DATA: lv_index TYPE i.
    DATA: lv_scale_value TYPE ty_float.
    DATA: lo_column TYPE REF TO zcl_vector.

    DO co_X->ncolumns( ) TIMES.
      lv_index = sy-index.
      READ TABLE mt_scale INTO lv_scale_value INDEX lv_index.
      lo_column = co_X->get_column( lv_index )->divn( lv_scale_value ).
      co_X->set_column( iv_index = lv_index io_vector = lo_column ).
    ENDDO.
  ENDMETHOD.

  METHOD fit.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_ones_column TYPE REF TO zcl_vector.
    DATA: lv_loss_func TYPE text128,
          lv_loss_func_grad TYPE text128.
    DATA: BEGIN OF ls_args,
            l1 TYPE ty_float,
            l2 TYPE ty_float,
            y_pred TYPE REF TO zcl_vector,
          END OF ls_args.
    DATA: lr_args TYPE REF TO data.
    DATA: lo_w0 TYPE REF TO zcl_vector.
    DATA: lv_ok TYPE flag.

    IF io_X->nrows( ) = 0 OR io_y->size( ) = 0 OR io_X->nrows( ) <> io_y->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

* add column for constant feature
    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones_column EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones_column ).

* scale columns of X to [-1; 1]
    scale_fit_transform( CHANGING co_X = lo_X ).

    CREATE OBJECT mo_coefs EXPORTING size = lo_X->ncolumns( ).

    ls_args-l1 = mv_l1_ratio.
    ls_args-l2 = mv_l2_ratio.
    GET REFERENCE OF ls_args INTO lr_args.

    CREATE OBJECT mo_sgd.
    CREATE OBJECT lo_w0 EXPORTING size = lo_X->ncolumns( ) fill_value = 0.

* Gradient descent on chosen loss function
    mo_sgd->minimize(
      EXPORTING
        iv_str_func      = mv_loss_func
        iv_str_func_grad = mv_loss_func_grad
        io_X             = lo_X
        io_y             = io_y
        ir_args          = lr_args
        iv_learning_rate = mv_learning_rate
        iv_eps           = mv_eps
        iv_max_steps     = mv_max_steps
        iv_batch_size    = mv_batch_size
        iv_batch_all     = mv_batch_all
        iv_random_seed   = mv_random_seed
        io_w0            = lo_w0
      IMPORTING
        ev_ok            = lv_ok
      EXCEPTIONS
        INPUT_ERROR      = 1
        SOLVE_ERROR      = 2
        OTHERS           = 3 ).

    IF sy-subrc <> 0.
      RAISE SOLVE_ERROR.
    ELSE.
      mo_coefs = mo_sgd->get_result( ).
    ENDIF.

  ENDMETHOD.

  METHOD predict.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_column TYPE REF TO zcl_vector.
    DATA: lo_ones TYPE REF TO zcl_vector.
    DATA: i TYPE i.

    IF mo_coefs IS NOT BOUND.
      RAISE KOEFS_ERROR.
    ENDIF.

    IF io_X->ncolumns( ) + 1 <> mo_coefs->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones ).

* scale columns of X as it was done in 'fit' method
    scale_transform( CHANGING co_X = lo_X ).

    ro_y_pred = lo_X->multiplyv( mo_coefs ).
  ENDMETHOD.

  METHOD get_info.
    IF mo_sgd IS BOUND.
      mo_sgd->get_info(
        IMPORTING
          ev_steps     = ev_steps
          et_conv_info = et_conv_info ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

* Linear regression with optimized square loss-function
*----------------------------------------------------------------------*
CLASS zcl_ml_sgd_regressor DEFINITION INHERITING FROM zcl_ml_sgd_regression.   " as is
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_l1_ratio TYPE ty_float DEFAULT 0      " L1-regularization coef
                            iv_l2_ratio TYPE ty_float DEFAULT '0.01' " L2-regularization coef
                            iv_batch_size TYPE ty_float DEFAULT 0
                            iv_batch_all TYPE flag DEFAULT ' '
                            iv_learning_rate TYPE ty_float DEFAULT '0.1'
                            iv_eps TYPE ty_float DEFAULT '0.0001'
                            iv_max_steps TYPE i DEFAULT 100
                            iv_random_seed TYPE i DEFAULT 1.

    CLASS-METHODS:
      square_loss IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(rv_value) TYPE ty_float,
      square_loss_grad IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector.
ENDCLASS.

CLASS zcl_ml_sgd_regressor IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_l1_ratio       = iv_l1_ratio
                        iv_l2_ratio       = iv_l2_ratio
                        iv_batch_size     = iv_batch_size
                        iv_batch_all      = iv_batch_all
                        iv_learning_rate  = iv_learning_rate
                        iv_eps            = iv_eps
                        iv_max_steps      = iv_max_steps
                        iv_random_seed    = iv_random_seed ).

    mv_loss_func = 'ZCL_ML_SGD_REGRESSOR=>SQUARE_LOSS'.
    mv_loss_func_grad = 'ZCL_ML_SGD_REGRESSOR=>SQUARE_LOSS_GRAD'.
  ENDMETHOD.

**********************************************************************
  METHOD square_loss.
* sum 1/2*[( y - Xw ) ^ 2] +  L2/2 * sum(w^2) + L1 * sum(abs(w))
    DATA: lo_vector TYPE REF TO zcl_vector.
    DATA: lv_l1_ratio TYPE ty_float.
    DATA: lv_l2_ratio TYPE ty_float.
    DATA: lo_y_pred TYPE REF TO zcl_vector.

    parse_ir_args( EXPORTING ir_args = ir_args
                   IMPORTING ev_l1   = lv_l1_ratio
                             ev_l2   = lv_l2_ratio
                             er_yp   = lo_y_pred ).

    IF lo_y_pred IS NOT BOUND.
      lo_y_pred = io_X->multiplyv( io_w ).
    ENDIF.
    lo_vector = lo_y_pred->subtract( io_y ).
    lo_vector->pown( iv_value = 2 iv_inplace = 'X' ).
    rv_value = lo_vector->sum( ) / 2.

    IF lv_l1_ratio > 0.
      rv_value = rv_value + lv_l1_ratio * io_w->apply_func( 'abs' )->sum( ).
    ENDIF.
    IF lv_l2_ratio > 0.
      rv_value = rv_value + ( lv_l2_ratio / 2 ) * io_w->pown( 2 )->sum( ).
    ENDIF.

  ENDMETHOD.

  METHOD square_loss_grad.
* d/d(wj) = sum(i) [ -( yi - yi_pred ) * xij ] +  L2 * wj + L1 * sign(wj)
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lo_y_pred TYPE REF TO zcl_vector.
    DATA: lo_tmp_vector TYPE REF TO zcl_vector.
    DATA: lv_value TYPE ty_float.
    DATA: lv_l1_ratio TYPE ty_float.
    DATA: lv_l2_ratio TYPE ty_float.
    DATA: lv_index TYPE i.


    parse_ir_args( EXPORTING ir_args = ir_args
                   IMPORTING ev_l1   = lv_l1_ratio
                             ev_l2   = lv_l2_ratio
                             er_yp   = lo_y_pred ).
    IF lo_y_pred IS NOT BOUND.
      lo_y_pred = io_X->multiplyv( io_w ).
    ENDIF.

    lo_tmp_vector = io_y->subtract( lo_y_pred )->multn( -1 ).
    DO io_w->size( ) TIMES.
      lv_index = sy-index.

      lv_value = lo_tmp_vector->dot( io_X->get_column( lv_index ) ).

      IF lv_l1_ratio > 0.
        lv_value = lv_value + lv_l1_ratio * sign( io_w->get_value( lv_index ) ).
      ENDIF.
      IF lv_l2_ratio > 0.
        lv_value = lv_value + lv_l2_ratio * io_w->get_value( lv_index ).
      ENDIF.
      APPEND lv_value TO lt_float_vector.
    ENDDO.

    ro_vector = zcl_vector=>create_from( lt_float_vector ).
  ENDMETHOD.
ENDCLASS.

* Logistic regression with different loss functions
*----------------------------------------------------------------------*
CLASS zcl_ml_sgd_classifier DEFINITION INHERITING FROM zcl_ml_sgd_regression.  " on sigmoid function
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_loss_func TYPE c DEFAULT 'log'        "'log' or 'square'
                            iv_l1_ratio TYPE ty_float DEFAULT 0      " L1-regularization coef
                            iv_l2_ratio TYPE ty_float DEFAULT '0.01' " L2-regularization coef
                            iv_batch_size TYPE ty_float DEFAULT 0
                            iv_batch_all TYPE flag DEFAULT ' '
                            iv_learning_rate TYPE ty_float DEFAULT '0.1'
                            iv_eps TYPE ty_float DEFAULT '0.0001'
                            iv_max_steps TYPE i DEFAULT 100
                            iv_random_seed TYPE i DEFAULT 1,
      fit REDEFINITION,
      predict REDEFINITION.

    CONSTANTS:
      mc_square_loss TYPE char10 VALUE 'square',
      mc_log_loss TYPE char10 VALUE  'log'.

    CLASS-METHODS:
      square_loss IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(rv_value) TYPE ty_float,
      square_loss_grad IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector,
      log_loss IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(rv_value) TYPE ty_float,
      log_loss_grad IMPORTING io_w TYPE REF TO zcl_vector io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector ir_args TYPE REF TO DATA OPTIONAL RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector.

  PRIVATE SECTION.
    METHODS:
      check_and_fit_labels IMPORTING io_labels TYPE REF TO zcl_vector RETURNING VALUE(rv_ok) TYPE flag.

ENDCLASS.

CLASS zcl_ml_sgd_classifier IMPLEMENTATION.
  METHOD constructor.
    super->constructor( iv_l1_ratio       = iv_l1_ratio
                        iv_l2_ratio       = iv_l2_ratio
                        iv_batch_size     = iv_batch_size
                        iv_batch_all      = iv_batch_all
                        iv_learning_rate  = iv_learning_rate
                        iv_eps            = iv_eps
                        iv_max_steps      = iv_max_steps
                        iv_random_seed    = iv_random_seed ).

    IF iv_loss_func = mc_square_loss.
      mv_loss_func = 'ZCL_ML_SGD_CLASSIFIER=>SQUARE_LOSS'.
      mv_loss_func_grad = 'ZCL_ML_SGD_CLASSIFIER=>SQUARE_LOSS_GRAD'.
    ELSE.
      mv_loss_func = 'ZCL_ML_SGD_CLASSIFIER=>LOG_LOSS'.
      mv_loss_func_grad = 'ZCL_ML_SGD_CLASSIFIER=>LOG_LOSS_GRAD'.
    ENDIF.
  ENDMETHOD.

**********************************************************************
  METHOD square_loss.
* sum [( y - sigma(Xw) ) ^ 2]  +  L2/2 * sum(w^2) + L1 * sum(abs(w))
    DATA: lo_vector TYPE REF TO zcl_vector.
    DATA: lv_l1_ratio TYPE ty_float.
    DATA: lv_l2_ratio TYPE ty_float.
    DATA: lo_y_pred TYPE REF TO zcl_vector.

    parse_ir_args( EXPORTING ir_args = ir_args
                   IMPORTING ev_l1   = lv_l1_ratio
                             ev_l2   = lv_l2_ratio
                             er_yp   = lo_y_pred ).

    IF lo_y_pred IS NOT BOUND.
      lo_y_pred = io_X->multiplyv( io_w )->apply_func( 'sigmoid' ).
    ENDIF.
    lo_vector = lo_y_pred->subtract( io_y ).
    lo_vector->pown( iv_value = 2 iv_inplace = 'X' ).
    rv_value = lo_vector->sum( ).

    IF lv_l1_ratio > 0.
      rv_value = rv_value + lv_l1_ratio * io_w->apply_func( 'abs' )->sum( ).
    ENDIF.
    IF lv_l2_ratio > 0.
      rv_value = rv_value + ( lv_l2_ratio / 2 ) * io_w->pown( 2 )->sum( ).
    ENDIF.

  ENDMETHOD.

  METHOD square_loss_grad.
* d/d(wj) = 1/n * sum(i) [ ( yi_pred - yi ) * yi_pred * ( 1 - yi_pred ) * xij ]  +  L2 * wj + L1 * sign(wj)
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lo_y_pred TYPE REF TO zcl_vector.
    DATA: lo_tmp_vector TYPE REF TO zcl_vector.
    DATA: lv_value TYPE ty_float.
    DATA: lv_scaler TYPE ty_float.
    DATA: lv_l1_ratio TYPE ty_float.
    DATA: lv_l2_ratio TYPE ty_float.
    DATA: lv_index TYPE i.


    parse_ir_args( EXPORTING ir_args = ir_args
                   IMPORTING ev_l1   = lv_l1_ratio
                             ev_l2   = lv_l2_ratio
                             er_yp   = lo_y_pred ).
    IF lo_y_pred IS NOT BOUND.
      lo_y_pred = io_X->multiplyv( io_w )->apply_func( 'sigmoid' ).
    ENDIF.

    lo_tmp_vector = lo_y_pred->subtract( io_y )->multv( lo_y_pred )->multv( lo_y_pred->multn( -1 )->addn( 1 ) ).
    DO io_w->size( ) TIMES.
      lv_index = sy-index.

      lv_value = lo_tmp_vector->dot( io_X->get_column( lv_index ) ).

      IF lv_l1_ratio > 0.
        lv_value = lv_value + lv_l1_ratio * sign( io_w->get_value( lv_index ) ).
      ENDIF.
      IF lv_l2_ratio > 0.
        lv_value = lv_value + lv_l2_ratio * io_w->get_value( lv_index ).
      ENDIF.
      APPEND lv_value TO lt_float_vector.
    ENDDO.

    ro_vector = zcl_vector=>create_from( lt_float_vector ).
  ENDMETHOD.

  METHOD log_loss. " cross-entropy
* -1 * sum(i) [ yi * ln(yi_pred) + (1-yi)*ln(1-yi_pred) ]  +  L2/2 * sum(w^2) + L1 * sum(abs(w))
    DATA: lo_vector TYPE REF TO zcl_vector.
    DATA: lo_vector2 TYPE REF TO zcl_vector.
    DATA: lv_l1_ratio TYPE ty_float.
    DATA: lv_l2_ratio TYPE ty_float.
    DATA: lo_y_pred TYPE REF TO zcl_vector.
    DATA: lv_value TYPE ty_float.
    CONSTANTS: lc_too_low TYPE ty_float VALUE '0.000000001'.

    parse_ir_args( EXPORTING ir_args = ir_args
                   IMPORTING ev_l1   = lv_l1_ratio
                             ev_l2   = lv_l2_ratio
                             er_yp   = lo_y_pred ).
    IF lo_y_pred IS NOT BOUND.
      lo_y_pred = io_X->multiplyv( io_w )->apply_func( 'sigmoid' ).
    ENDIF.

    lo_vector = zcl_vector=>create_copy( lo_y_pred ).
    "check zero values
    DO lo_vector->size( ) TIMES.
      lv_value = lo_vector->get_value( sy-index ).
      IF lv_value < lc_too_low.
        lo_vector->set_value( iv_index = sy-index iv_value = lc_too_low ).
      ENDIF.
    ENDDO.
    lo_vector = lo_vector->apply_func( 'log' ).
    lo_vector = io_y->multv( lo_vector ).

    lo_vector2  = lo_y_pred->multn( -1 )->addn( 1 ).
    "check zero values
    DO lo_vector2->size( ) TIMES.
      lv_value = lo_vector2->get_value( sy-index ).
      IF lv_value < lc_too_low.
        lo_vector2->set_value( iv_index = sy-index iv_value = lc_too_low ).
      ENDIF.
    ENDDO.
    lo_vector2 = lo_vector2->apply_func( 'log' ).
    lo_vector2 =  io_y->multn( -1 )->addn( 1 )->multv( lo_vector2 ).

    lo_vector = lo_vector->add( lo_vector2 ).
    rv_value = lo_vector->sum( ) * -1.

    IF lv_l1_ratio > 0.
      rv_value = rv_value + lv_l1_ratio * io_w->apply_func( 'abs' )->sum( ).
    ENDIF.
    IF lv_l2_ratio > 0.
      rv_value = rv_value + ( lv_l2_ratio / 2 ) * io_w->pown( 2 )->sum( ).
    ENDIF.
  ENDMETHOD.

  METHOD log_loss_grad.
* d/d(wj) = sum(i) [ ( yi_pred - yi ) * xij ]  +  L2 * wj + L1 * sign(wj)
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lo_y_pred TYPE REF TO zcl_vector.
    DATA: lo_tmp_vector TYPE REF TO zcl_vector.
    DATA: lv_value TYPE ty_float.
    DATA: lv_scaler TYPE ty_float.
    DATA: lv_l1_ratio TYPE ty_float.
    DATA: lv_l2_ratio TYPE ty_float.
    DATA: lv_index TYPE i.

    parse_ir_args( EXPORTING ir_args = ir_args
                   IMPORTING ev_l1   = lv_l1_ratio
                             ev_l2   = lv_l2_ratio
                             er_yp   = lo_y_pred ).
    IF lo_y_pred IS NOT BOUND.
      lo_y_pred = io_X->multiplyv( io_w )->apply_func( 'sigmoid' ).
    ENDIF.

    lo_tmp_vector = lo_y_pred->subtract( io_y ).
    DO io_w->size( ) TIMES.
      lv_index = sy-index.

      lv_value = lo_tmp_vector->dot( io_X->get_column( lv_index ) ).

      IF lv_l1_ratio > 0.
        lv_value = lv_value + lv_l1_ratio * sign( io_w->get_value( lv_index ) ).
      ENDIF.
      IF lv_l2_ratio > 0.
        lv_value = lv_value + lv_l2_ratio * io_w->get_value( lv_index ).
      ENDIF.
      APPEND lv_value TO lt_float_vector.
    ENDDO.

    ro_vector = zcl_vector=>create_from( lt_float_vector ).
  ENDMETHOD.

**********************************************************************
  METHOD fit.
    DATA: lo_labels TYPE REF TO zcl_vector.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_ones_column TYPE REF TO zcl_vector.
    DATA: lv_loss_func TYPE text128,
          lv_loss_func_grad TYPE text128.
    DATA: BEGIN OF ls_args,
            l1 TYPE ty_float,
            l2 TYPE ty_float,
            y_pred TYPE REF TO zcl_vector,
          END OF ls_args.
    DATA: lr_args TYPE REF TO data.
    DATA: lo_w0 TYPE REF TO zcl_vector.
    DATA: lv_ok TYPE flag.

    IF io_X->nrows( ) = 0 OR io_y->size( ) = 0 OR io_X->nrows( ) <> io_y->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

* add column for constant feature
    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones_column EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones_column ).

* scale columns of X to [-1; 1]
    scale_fit_transform( CHANGING co_X = lo_X ).

* check class labels
    lo_labels = zcl_vector=>create_copy( io_y ).
    IF check_and_fit_labels( lo_labels ) = abap_false.
      RAISE WRONG_LABELS.
    ELSE.
      " we have labels transformed to {0; 1} classes
    ENDIF.

    CREATE OBJECT mo_coefs EXPORTING size = lo_X->ncolumns( ).

    ls_args-l1 = mv_l1_ratio.
    ls_args-l2 = mv_l2_ratio.
    GET REFERENCE OF ls_args INTO lr_args.

    CREATE OBJECT mo_sgd.
    CREATE OBJECT lo_w0 EXPORTING size = lo_X->ncolumns( ) fill_value = 0.

* Gradient descent on chosen loss function
    mo_sgd->minimize(
      EXPORTING
        iv_str_func      = mv_loss_func
        iv_str_func_grad = mv_loss_func_grad
        io_X             = lo_X
        io_y             = lo_labels
        ir_args          = lr_args
        iv_learning_rate = mv_learning_rate
        iv_eps           = mv_eps
        iv_max_steps     = mv_max_steps
        iv_batch_size    = mv_batch_size
        iv_batch_all     = mv_batch_all
        iv_random_seed   = mv_random_seed
        io_w0            = lo_w0
      IMPORTING
        ev_ok            = lv_ok
      EXCEPTIONS
        INPUT_ERROR      = 1
        SOLVE_ERROR      = 2
        OTHERS           = 3 ).

    IF sy-subrc <> 0.
      RAISE SOLVE_ERROR.
    ELSE.
      mo_coefs = mo_sgd->get_result( ).
    ENDIF.

  ENDMETHOD.

  METHOD predict.
    ro_y_pred = super->predict( io_X )->apply_func( 'sigmoid' ).
  ENDMETHOD.

  METHOD check_and_fit_labels.
    DATA: lt_values TYPE wdy_key_value_list.
    DATA: lv_one TYPE ty_float.
    DATA: lv_two TYPE ty_float.
    DATA: ls_value LIKE LINE OF lt_values.
    DATA: lt_indexes TYPE int4_table.
    DATA: lv_index TYPE i.

    lt_values = io_labels->get_values_count( ).
    IF LINES( lt_values ) <> 2.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    READ TABLE lt_values INTO ls_value INDEX 1.
    lv_one = ls_value-key.
    READ TABLE lt_values INTO ls_value INDEX 2.
    lv_two = ls_value-key.

    IF ( lv_one <> 0 AND lv_two <> 0 ) OR lv_one < -1 OR lv_two < -1.
      rv_ok = abap_false.
      RETURN.

    ELSEIF ( lv_one = 0 AND lv_two <> 1 ) OR ( lv_one <> 1 OR lv_two = 0 ).
      IF lv_one > 0.
        lt_indexes = io_labels->get_indexes( lv_one ).
      ELSE.
        lt_indexes = io_labels->get_indexes( lv_two ).
      ENDIF.
      LOOP AT lt_indexes INTO lv_index.
        io_labels->set_value( iv_index = lv_index iv_value = 1 ).
      ENDLOOP.

    ELSEIF ( lv_one = -1 AND lv_two = 1 ) OR ( lv_one = 1 AND lv_two = -1 ).
      IF lv_one = -1.
        lt_indexes = io_labels->get_indexes( lv_one ).
      ELSE.
        lt_indexes = io_labels->get_indexes( lv_two ).
      ENDIF.
      LOOP AT lt_indexes INTO lv_index.
        io_labels->set_value( iv_index = lv_index iv_value = 0 ).
      ENDLOOP.

    ELSEIF ( lv_one = 0 AND lv_two = 1 ) OR ( lv_one = 1 OR lv_two = 0 ).
      . "all is ok

    ELSE.
      rv_ok = abap_false.
      RETURN.
    ENDIF.

    rv_ok = abap_true.

  ENDMETHOD.
ENDCLASS.

**********************************************************************
* EXAMPLES                                                           *
**********************************************************************

START-OF-SELECTION.
  "PERFORM load_from_text.
  "PERFORM solve.
  "PERFORM linear_regression.
  "PERFORM linear_regression_sgd.
  "PERFORM logistic_regression.
  PERFORM logistic_regression_sgd.
  "PERFORM sgd_test.



**********************************************************************
* Example of loading and multiplication of matrices
**********************************************************************
FORM load_from_text.
  DATA: lt_table TYPE TABLE OF string.
  DATA: A TYPE REF TO zcl_matrix.
  DATA: B TYPE REF TO zcl_matrix.

  APPEND '1;2;3;4;5;6;7;8;9;10' TO lt_table.
  APPEND '0;0;1;0;0;1;0;0;0;1' TO lt_table.
  APPEND '1;0;0;0;0;1;0;0;0;1' TO lt_table.
  A = zcl_matrix=>load_from_text( lt_table ).
  REFRESH lt_table.
  APPEND '0;1;1' TO lt_table.
  APPEND '0;2;1' TO lt_table.
  APPEND '1;3;1' TO lt_table.
  APPEND '0;4;1' TO lt_table.
  APPEND '0;5;1' TO lt_table.
  APPEND '1;6;1' TO lt_table.
  APPEND '0;7;1' TO lt_table.
  APPEND '0;8;1' TO lt_table.
  APPEND '0;9;1' TO lt_table.
  APPEND '1;10;1' TO lt_table.
  B = zcl_matrix=>load_from_text( lt_table ).

  B = A->multiply( B ).
  B->print( ).
ENDFORM.

**********************************************************************
* Example of solving system of linear equations
**********************************************************************
FORM solve.
  DATA: lt_str TYPE TABLE OF string.
  DATA: lt_int TYPE TABLE OF ty_float.
  DATA: A TYPE REF TO zcl_matrix.
  DATA: b TYPE REF TO zcl_vector.
  DATA: x TYPE REF TO zcl_vector.

  " Load matrix from strings (also supporting file and any table)
  APPEND '-2;1;2' TO lt_str.
  APPEND '2;1;-1' TO lt_str.
  APPEND '-3;-1;2' TO lt_str.
  A = zcl_matrix=>load_from_text( lt_str ).

  " Load vector
  APPEND '-3' TO lt_int.
  APPEND '8' TO lt_int.
  APPEND '-11' TO lt_int.
  b = zcl_vector=>create_from( lt_int ).

  " Solve system of linear equations Ax = b
  x = A->solve( b ).
  WRITE: /, 'Solution:', /.
  x->print( ).
  WRITE: /, 'Check:', /.
  x = A->multiplyv( x ).
  x->print( ).

ENDFORM.

**********************************************************************
*  DATA: lt_pa2001 TYPE TABLE OF pa2001.
*  SELECT * INTO TABLE lt_pa2001
*    FROM pa2001 UP TO 100 ROWS.
*
*GET TIME STAMP FIELD t1.
*  lo_m2 = zcl_matrix=>create_from( lt_pa2001 ).
*  lo_m1 = lo_m2->transpose( ).
*  lo_m1 = lo_m1->multiply( lo_m2 ).
*GET TIME STAMP FIELD t2.
*t0 = t2 - t1.
*WRITE: /.
*WRITE: / t0, /.
*
*  DATA: lt TYPE wdy_key_value_list.
*  lt = lo->get_values_count( ).
*EXIT.
***********************************************************************

**********************************************************************
* Example of linear regression on randomly generated data
**********************************************************************
FORM linear_regression.
  DATA: BEGIN OF ls,
    a TYPE i,
    b TYPE i,
    c TYPE i,
    d TYPE i,
    e TYPE i,
  END OF ls,
  lt LIKE TABLE OF ls,
  lv TYPE ty_float_vector.

  DATA: lo_rgen TYPE REF TO cl_abap_random_float.
  DATA: val TYPE ty_float.

  lo_rgen = cl_abap_random_float=>create( 1 ).

  DO 12 TIMES.
    ls-a = lo_rgen->get_next( ) * 10.
    ls-b = lo_rgen->get_next( ) * 10.
    ls-c = lo_rgen->get_next( ) * 10.
    ls-d = lo_rgen->get_next( ) * 10.
    ls-e = lo_rgen->get_next( ) * 10.
    val = 10 + 1 * ls-a - 2 * ls-b + 3 * ls-c - 4 * ls-d + 5 * ls-e + lo_rgen->get_next( ).
    APPEND ls TO lt.
    APPEND val TO lv.
  ENDDO.

  DATA: lo_X TYPE REF TO zcl_matrix.
  DATA: lo_y TYPE REF TO zcl_vector.
  DATA: lo_k TYPE REF TO zcl_vector.

  lo_X = zcl_matrix=>create_from( lt ).
  lo_y = zcl_vector=>create_from( lv ).

  WRITE: /'X matrix:'. NEW-LINE.
  lo_X->print( ).
  WRITE: /.

  WRITE: /'y:'. NEW-LINE.
  lo_y->print( ).
  WRITE: /.

  DATA: lo_reg TYPE REF TO zcl_ml_linear_regression.

  CREATE OBJECT lo_reg EXPORTING iv_ridge_coef = '0.01'.
  lo_reg->fit( io_X = lo_X io_y = lo_y ).
  lo_k = lo_reg->get_coefs( ).

  WRITE: /'coefs and score:'. NEW-LINE.
  lo_k->print( ).

  DATA: dev TYPE ty_float.
  dev = lo_reg->get_predict_score( io_X = lo_X io_y = lo_y ).
  WRITE dev.
ENDFORM.

**********************************************************************
* example of function minimization using gradient descent
**********************************************************************
FORM sgd_test.
  DATA: lo_vector TYPE REF TO zcl_vector.
  DATA: BEGIN OF ls,
    a TYPE ty_float,
    b TYPE ty_float,
    c TYPE ty_float,
    d TYPE ty_float,
    e TYPE ty_float,
    f TYPE ty_float,
  END OF ls,
  lt LIKE TABLE OF ls,
  lv TYPE ty_float_vector.

  DATA: lo_rgen TYPE REF TO cl_abap_random_float.
  DATA: val TYPE ty_float.

  lo_rgen = cl_abap_random_float=>create( 1 ).

  DO 12 TIMES.
    CREATE OBJECT lo_vector EXPORTING size = 0.
    ls-a = lo_rgen->get_next( ) * 10. lo_vector->append( ls-a ).
    ls-b = lo_rgen->get_next( ) * 10. lo_vector->append( ls-b ).
    ls-c = lo_rgen->get_next( ) * 10. lo_vector->append( ls-c ).
    ls-d = lo_rgen->get_next( ) * 10. lo_vector->append( ls-d ).
    ls-e = lo_rgen->get_next( ) * 10. lo_vector->append( ls-e ).
    ls-f = lo_rgen->get_next( ) * 10. lo_vector->append( ls-f ).
    val = ZCL_ML_SGD=>TEST( lo_vector ).   " special test-function of class zcl_ml_sgd
    APPEND ls TO lt.
    APPEND val TO lv.
  ENDDO.

  DATA: lo_X TYPE REF TO zcl_matrix.
  DATA: lo_y TYPE REF TO zcl_vector.
  DATA: lo_k TYPE REF TO zcl_vector.

  lo_X = zcl_matrix=>create_from( lt ).
  lo_y = zcl_vector=>create_from( lv ).

  WRITE: /'X matrix:'. NEW-LINE.
  lo_X->print( ).
  WRITE: /.

  WRITE: /'y:'. NEW-LINE.
  lo_y->print( ).
  WRITE: /.

  DATA: lo_sgd TYPE REF TO zcl_ml_sgd.
  DATA: lv_steps TYPE i.
  DATA: lt_info TYPE wdy_key_value_list.
  DATA: ls_info TYPE LINE OF wdy_key_value_list.
  DATA: lo_w0 TYPE REF TO zcl_vector.
  DATA: lv_value TYPE ty_float.

  CREATE OBJECT lo_sgd.
  CREATE OBJECT lo_w0 EXPORTING size = lo_X->ncolumns( ) fill_value = 0.

  lo_sgd->minimize( iv_str_func = 'test'  " special test-function of class zcl_ml_sgd
                    io_X = lo_X
                    io_y = lo_y
                    iv_learning_rate = '0.05'
                    iv_max_steps = 1000
                    iv_batch_size = 0
                    iv_batch_all = abap_true
                    io_w0 = lo_w0 ).

  lo_k = lo_sgd->get_result( ).
  lo_sgd->get_info( IMPORTING ev_steps = lv_steps
                              et_conv_info = lt_info ).

  WRITE: /'result:'. NEW-LINE.
  lo_k->print( ).
  WRITE: /.

  READ TABLE lt_info INTO ls_info INDEX LINES( lt_info ).
  WRITE: /(7) ls_info-key, (20) ls_info-value.
  WRITE: /.

  LOOP AT lt_info INTO ls_info.
    WRITE: /(7) ls_info-key, (20) ls_info-value.
  ENDLOOP.
ENDFORM.

**********************************************************************
* Example of linear regression on randomly generated data
* Loss optimization is made by stochastic gradient descent
**********************************************************************
FORM linear_regression_sgd.
  DATA: BEGIN OF ls,
    a TYPE i,
    b TYPE i,
    c TYPE i,
    d TYPE i,
    e TYPE i,
  END OF ls,
  lt LIKE TABLE OF ls,
  lv TYPE ty_float_vector.

  DATA: lo_rgen TYPE REF TO cl_abap_random_float.
  DATA: val TYPE ty_float.

  lo_rgen = cl_abap_random_float=>create( 1 ).

  DO 12 TIMES.
    ls-a = lo_rgen->get_next( ) * 10.
    ls-b = lo_rgen->get_next( ) * 10.
    ls-c = lo_rgen->get_next( ) * 10.
    ls-d = lo_rgen->get_next( ) * 10.
    ls-e = lo_rgen->get_next( ) * 10.
    val = 10 + 1 * ls-a - 2 * ls-b + 3 * ls-c - 4 * ls-d + 5 * ls-e + lo_rgen->get_next( ).
    APPEND ls TO lt.
    APPEND val TO lv.
  ENDDO.

  DATA: lo_X TYPE REF TO zcl_matrix.
  DATA: lo_y TYPE REF TO zcl_vector.
  DATA: lo_k TYPE REF TO zcl_vector.

  lo_X = zcl_matrix=>create_from( lt ).
  lo_y = zcl_vector=>create_from( lv ).

  WRITE: /'X matrix:'. NEW-LINE.
  lo_X->print( ).
  WRITE: /.

  WRITE: /'y:'. NEW-LINE.
  lo_y->print( ).
  WRITE: /.

  DATA: lo_reg TYPE REF TO zcl_ml_sgd_regressor.
  DATA: lv_steps TYPE i.
  DATA: lt_info TYPE wdy_key_value_list.
  DATA: ls_info TYPE LINE OF wdy_key_value_list.
  DATA: lv_value TYPE ty_float.

  CREATE OBJECT lo_reg
    EXPORTING
      iv_l1_ratio  = '0'
      iv_l2_ratio  = '0'
      iv_learning_rate = '0.1'
      iv_max_steps = 2000
      iv_batch_size = 5
      iv_batch_all = abap_true.

  lo_reg->fit(
    EXPORTING io_X = lo_X io_y = lo_y
    EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    WRITE 'NOT SOLVED, INTERNAL ERROR'.
    lo_reg->get_info( IMPORTING ev_steps = lv_steps
                                et_conv_info = lt_info ).
    READ TABLE lt_info INTO ls_info INDEX LINES( lt_info ).
    WRITE: /(7) ls_info-key, (20) ls_info-value.
    WRITE: /.
    EXIT.
  ENDIF.

  lo_k = lo_reg->get_coefs( ).

  WRITE: /'result:'. NEW-LINE.
  lo_k->print( ).
  WRITE: /.

  lo_reg->get_info( IMPORTING ev_steps = lv_steps
                              et_conv_info = lt_info ).

  READ TABLE lt_info INTO ls_info INDEX LINES( lt_info ).
  WRITE: /(7) ls_info-key, (20) ls_info-value.
  DATA: dev TYPE ty_float.
  dev = lo_reg->get_predict_score( io_X = lo_X io_y = lo_y ).
  WRITE: '   score:', (20) dev.
  WRITE: /.

  LOOP AT lt_info INTO ls_info.
    WRITE: /(7) ls_info-key, (20) ls_info-value.
  ENDLOOP.

ENDFORM.

**********************************************************************
* Example of logistic regression on randomly generated data
**********************************************************************
FORM logistic_regression.
  DATA: BEGIN OF ls,
    a TYPE i,
    b TYPE i,
    c TYPE i,
    d TYPE i,
    e TYPE i,
  END OF ls,
  lt LIKE TABLE OF ls,
  lv TYPE ty_float_vector.

  DATA: lo_rgen TYPE REF TO cl_abap_random_float.
  DATA: val TYPE ty_float.
  DATA: lt_info TYPE wdy_key_value_list.
  DATA: ls_info TYPE LINE OF wdy_key_value_list.
  DATA: lv_steps TYPE i.

  lo_rgen = cl_abap_random_float=>create( 1 ).

  DO 30 TIMES.
    ls-a = lo_rgen->get_next( ) * 10.
    ls-b = lo_rgen->get_next( ) * 10.
    ls-c = lo_rgen->get_next( ) * 10.
    ls-d = lo_rgen->get_next( ) * 10.
    ls-e = lo_rgen->get_next( ) * 10.
    val = 1 + 1 * ls-a - 2 * ls-b + 3 * ls-c - 4 * ls-d + 5 * ls-e + lo_rgen->get_next( ).
    val = '1.0' / ( '1.0' + exp( -1 * val ) ).
    IF val > '0.5'.
      val = 1.
    ELSE.
      val = 0.
    ENDIF.
    APPEND ls TO lt.
    APPEND val TO lv.
  ENDDO.

  DATA: lo_X TYPE REF TO zcl_matrix.
  DATA: lo_y TYPE REF TO zcl_vector.
  DATA: lo_k TYPE REF TO zcl_vector.

  lo_X = zcl_matrix=>create_from( lt ).
  lo_y = zcl_vector=>create_from( lv ).

  WRITE: /'X matrix:'. NEW-LINE.
  lo_X->print( ).
  WRITE: /.

  WRITE: /'y:'. NEW-LINE.
  lo_y->print( ).
  WRITE: /.

  DATA: lo_reg TYPE REF TO zcl_ml_logistic_regression.

  CREATE OBJECT lo_reg EXPORTING iv_ridge_coef = '0.001' iv_eps = '0.0001'.
  lo_reg->fit( io_X = lo_X io_y = lo_y ).
  lo_k = lo_reg->get_coefs( ).
  lo_reg->get_info( IMPORTING ev_steps = lv_steps
                              et_conv_info = lt_info ).

  WRITE: /'coefs and score:'. NEW-LINE.
  lo_k->print( ).

  READ TABLE lt_info INTO ls_info INDEX LINES( lt_info ).
  WRITE: /(7) ls_info-key, (20) ls_info-value.
  WRITE: /.

  LOOP AT lt_info INTO ls_info.
    WRITE: /(7) ls_info-key, (20) ls_info-value.
  ENDLOOP.

  DATA: dev TYPE ty_float.
  dev = lo_reg->get_predict_score( io_X = lo_X io_y = lo_y ).
  WRITE dev.
ENDFORM.

**********************************************************************
* Example of logistic regression on randomly generated data
* Loss optimization is made by stochastic gradient descent
**********************************************************************
FORM logistic_regression_sgd.
  DATA: BEGIN OF ls,
    a TYPE i,
    b TYPE i,
    c TYPE i,
    d TYPE i,
    e TYPE i,
  END OF ls,
  lt LIKE TABLE OF ls,
  lv TYPE ty_float_vector.

  DATA: lo_rgen TYPE REF TO cl_abap_random_float.
  DATA: val TYPE ty_float.
  DATA: lt_info TYPE wdy_key_value_list.
  DATA: ls_info TYPE LINE OF wdy_key_value_list.
  DATA: lv_steps TYPE i.

  lo_rgen = cl_abap_random_float=>create( 1 ).

  DO 30 TIMES.
    ls-a = lo_rgen->get_next( ) * 10.
    ls-b = lo_rgen->get_next( ) * 10.
    ls-c = lo_rgen->get_next( ) * 10.
    ls-d = lo_rgen->get_next( ) * 10.
    ls-e = lo_rgen->get_next( ) * 10.
    val = 1 + 1 * ls-a - 2 * ls-b + 3 * ls-c - 4 * ls-d + 5 * ls-e + lo_rgen->get_next( ).
    val = '1.0' / ( '1.0' + exp( -1 * val ) ).
    IF val > '0.5'.
      val = 1.
    ELSE.
      val = 0.
    ENDIF.
    APPEND ls TO lt.
    APPEND val TO lv.
  ENDDO.

  DATA: lo_X TYPE REF TO zcl_matrix.
  DATA: lo_y TYPE REF TO zcl_vector.
  DATA: lo_k TYPE REF TO zcl_vector.

  lo_X = zcl_matrix=>create_from( lt ).
  lo_y = zcl_vector=>create_from( lv ).

  WRITE: /'X matrix:'. NEW-LINE.
  lo_X->print( ).
  WRITE: /.

  WRITE: /'y:'. NEW-LINE.
  lo_y->print( ).
  WRITE: /.


  DATA: lo_cls TYPE REF TO zcl_ml_sgd_classifier.
  DATA: lv_value TYPE ty_float.

  CREATE OBJECT lo_cls
    EXPORTING
      iv_loss_func = 'log'
      iv_l1_ratio  = '0'
      iv_l2_ratio  = '0'
      iv_learning_rate = '0.5'
      iv_max_steps = 1000
      iv_batch_size = 5
      iv_batch_all = abap_true.

  lo_cls->fit(
    EXPORTING io_X = lo_X io_y = lo_y
    EXCEPTIONS OTHERS = 1 ).
  IF sy-subrc <> 0.
    WRITE 'NOT SOLVED'.
    lo_cls->get_info( IMPORTING ev_steps = lv_steps
                                et_conv_info = lt_info ).
    READ TABLE lt_info INTO ls_info INDEX LINES( lt_info ).
    WRITE: /(7) ls_info-key, (20) ls_info-value.
    WRITE: /.
    EXIT.
  ENDIF.

  lo_k = lo_cls->get_coefs( ).

  WRITE: /'result:'. NEW-LINE.
  lo_k->print( ).
  WRITE: /.

  lo_cls->get_info( IMPORTING ev_steps = lv_steps
                              et_conv_info = lt_info ).

  READ TABLE lt_info INTO ls_info INDEX LINES( lt_info ).
  WRITE: /(7) ls_info-key, (20) ls_info-value.
  DATA: dev TYPE ty_float.
  dev = lo_cls->get_predict_score( io_X = lo_X io_y = lo_y ).
  WRITE: '   score:', (20) dev.
  WRITE: /.

  LOOP AT lt_info INTO ls_info.
    WRITE: /(7) ls_info-key, (20) ls_info-value.
  ENDLOOP.

ENDFORM.