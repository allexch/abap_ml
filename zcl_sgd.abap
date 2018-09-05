*&---------------------------------------------------------------------*
*& Include  ZCL_SGD
*&---------------------------------------------------------------------*

INCLUDE ZCL_MATRIX.

*----------------------------------------------------------------------*
* GRADIENT DESCENT
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
