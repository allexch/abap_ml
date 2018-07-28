*&---------------------------------------------------------------------*
*& Include  ZCL_REGRESSION
*&---------------------------------------------------------------------*

INCLUDE ZCL_MATRIX.

*----------------------------------------------------------------------*
* REGRESSOR
*----------------------------------------------------------------------*

CLASS zcl_ml_regressor DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor,
      fit IMPORTING io_X TYPE REF TO zcl_matrix  io_y TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR SOLVE_ERROR,
      predict IMPORTING io_X TYPE REF TO zcl_matrix RETURNING VALUE(ro_y_pred) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR KOEFS_ERROR,
      get_koefs RETURNING VALUE(ro_koefs) TYPE REF TO zcl_vector,
      get_predict_score IMPORTING io_X TYPE REF TO zcl_matrix io_y TYPE REF TO zcl_vector RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS SIZE_ERROR KOEFS_ERROR.

  PROTECTED SECTION.
    DATA:
      mo_koefs TYPE REF TO zcl_vector.
ENDCLASS.

CLASS zcl_ml_regressor IMPLEMENTATION.
  METHOD constructor.
  ENDMETHOD.

  METHOD fit.
  ENDMETHOD.

  METHOD predict.
  ENDMETHOD.

* return copy of koefs
  METHOD get_koefs.
    ro_koefs = zcl_vector=>create_from( mo_koefs->get( ) ).
  ENDMETHOD.

* return coefficient of determination R^2 of the prediction
  METHOD get_predict_score.
    DATA: u TYPE ty_float.
    DATA: v TYPE ty_float.
    DATA: lo_y_pred TYPE REF TO zcl_vector.
    DATA: lo_y_mean TYPE REF TO zcl_vector.
    DATA: lv_mean TYPE ty_float.

    IF mo_koefs IS NOT BOUND.
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

*----------------------------------------------------------------------*

CLASS zcl_ml_ols_regressor DEFINITION INHERITING FROM zcl_ml_regressor.
  PUBLIC SECTION.
    METHODS:
      constructor IMPORTING iv_normalize TYPE flag DEFAULT ' ',
      fit REDEFINITION,
      predict REDEFINITION.

  PROTECTED SECTION.
    METHODS:
      normalize_features IMPORTING io_X TYPE REF TO zcl_matrix RETURNING VALUE(ro_X) TYPE REF TO zcl_matrix.
    DATA:
      mv_normalize TYPE flag.
ENDCLASS.

CLASS zcl_ml_ols_regressor IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    mv_normalize = iv_normalize.
  ENDMETHOD.

* normalize io_X by columns ( not done )
  METHOD normalize_features.
    DATA: lo_column TYPE REF TO zcl_vector.
    DATA: i TYPE i.
    DATA: lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_table> TYPE INDEX TABLE.

    ro_X = zcl_matrix=>create_copy( io_X ).
  ENDMETHOD.

*  solve equation (X^T*X)koefs = X^T*y
  METHOD fit.
    DATA: lo_transpose_X TYPE REF TO zcl_matrix.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_ones TYPE REF TO zcl_vector.

    IF io_X->nrows( ) = 0 OR io_y->size( ) = 0 OR io_X->nrows( ) <> io_y->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones ).

    lo_transpose_X = lo_X->transpose( ).
    mo_koefs = lo_transpose_X->multiply( lo_X )->solve( io_vector = lo_transpose_X->multiplyv( io_y ) ).
  ENDMETHOD.

* y = X * koefs
  METHOD predict.
    DATA: lo_X TYPE REF TO zcl_matrix.
    DATA: lo_column TYPE REF TO zcl_vector.
    DATA: lo_ones TYPE REF TO zcl_vector.
    DATA: i TYPE i.

    IF mo_koefs IS NOT BOUND.
      RAISE KOEFS_ERROR.
    ENDIF.

    IF io_X->ncolumns( ) + 1 <> mo_koefs->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_X = zcl_matrix=>create_copy( io_X ).
    CREATE OBJECT lo_ones EXPORTING size = lo_X->nrows( ) fill_value = 1.
    lo_X->append_column( lo_ones ).

    ro_y_pred = lo_X->multiplyv( mo_koefs ).
  ENDMETHOD.

ENDCLASS.
