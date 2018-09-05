REPORT z_ml_example.

**********************************************************************
* Example of function minimization using gradient descent
**********************************************************************

INCLUDE ZCL_SGD.

PERFORM sgd_test.

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
