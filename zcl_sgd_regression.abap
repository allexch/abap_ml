*&---------------------------------------------------------------------*
*& Include  ZCL_SGD_REGRESSION
*&---------------------------------------------------------------------*

INCLUDE ZCL_REGRESSION.

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
                            iv_batch_all TYPE flag DEFAULT ' '            " used for SGD mode, ' ' - batch selected randomly once per epoch, 'X' - epoch consist of several batches and covered all data
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
