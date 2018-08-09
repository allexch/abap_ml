REPORT z_ml_example.

**********************************************************************
* Example of linear regression on randomly generated data
**********************************************************************

INCLUDE ZCL_REGRESSION.

PERFORM linear_regression.

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
