*&---------------------------------------------------------------------*
*& Include  ZCL_REGRESSION
*&---------------------------------------------------------------------*

INCLUDE ZCL_SGD.

*----------------------------------------------------------------------*
* REGRESSION CLASSES
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
