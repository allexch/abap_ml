REPORT z_ml_example.

**********************************************************************
* Example of logistic regression on randomly generated data
* Loss optimization is made by stochastic gradient descent
**********************************************************************

INCLUDE ZCL_SGD_REGRESSION.

PERFORM logistic_regression_sgd.

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