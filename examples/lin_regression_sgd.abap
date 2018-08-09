REPORT z_ml_example.

**********************************************************************
* Example of linear regression on randomly generated data 
* Loss optimization is made by stochastic gradient descend
**********************************************************************

INCLUDE ZCL_REGRESSION.

PERFORM linear_regression_sgd.

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

  DATA: lo_sgd TYPE REF TO zcl_ml_sgd.
  DATA: lv_steps TYPE i.
  DATA: lt_info TYPE wdy_key_value_list.
  DATA: ls_info TYPE LINE OF wdy_key_value_list.
  DATA: lo_w0 TYPE REF TO zcl_vector.
  DATA: lv_value TYPE ty_float.

***
  CREATE OBJECT lo_w0 EXPORTING size = lo_X->nrows( ) fill_value = 1.
  lo_X->append_column( lo_w0 ).   " emulate constant feature
  FREE lo_w0.
***

  CREATE OBJECT lo_sgd.
  CREATE OBJECT lo_w0 EXPORTING size = lo_X->ncolumns( ) fill_value = 0.

  lo_sgd->minimize( iv_str_func = 'ZCL_ML_SGD=>LINREG_SQUARE_LOSS'   " can be any custom function, implemented as class method 
                    iv_str_func_grad = 'ZCL_ML_SGD=>LINREG_SQUARE_LOSS_GRAD' " can be omitted, so grad will calculated numerically 
                    io_X = lo_X        " in case ML problem - X matrix takes part in loss function, and therefore should be passed via parameter
                    io_y = lo_y        " in case ML problem - y vector takes part in loss function, and therefore should be passed via parameter
*					io_args = ...      " any additional data in implementation of your custom function 
                    iv_learning_rate = '0.05'  
                    iv_max_steps = 1000
                    iv_batch_size = 0  " <> 0 - for stochastic grad.descend (part of X matrix for one batch), 0 - usual grad.descend					
                    iv_batch_all = abap_true
                    io_w0 = lo_w0 ).   " initial guess 


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

  lo_w0->set_value( iv_index = 1 iv_value = 1 ).
  lo_w0->set_value( iv_index = 2 iv_value = -2 ).
  lo_w0->set_value( iv_index = 3 iv_value = 3 ).
  lo_w0->set_value( iv_index = 4 iv_value = -4 ).
  lo_w0->set_value( iv_index = 5 iv_value = 5 ).
  lo_w0->set_value( iv_index = 6 iv_value = 10 ).
  lv_value = zcl_ml_sgd=>linreg_square_loss( io_w = lo_w0 io_X = lo_X io_y = lo_y ).
  WRITE: /'True loss value...', lv_value.
ENDFORM.