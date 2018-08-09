REPORT z_ml_example.

**********************************************************************
* Example of logistic regression on randomly generated data 
**********************************************************************

INCLUDE ZCL_REGRESSION.

PERFORM logistic_regression.

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