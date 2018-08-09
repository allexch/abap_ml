REPORT z_ml_example.

**********************************************************************
* Example of solving system of linear equations
**********************************************************************

INCLUDE ZCL_REGRESSION.

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
  APPEND '-3' TO t_vector.
  APPEND '8' TO t_vector.
  APPEND '-11' TO t_vector.
  b = zcl_vector=>create_from( lt_int ).

" Solve system of linear equations Ax = b
  x = A->solve( b ).
  WRITE: /, 'Solution:', /.
  x->print( ).
  WRITE: /, 'Check:', /.
  x = A->multiplyv( x ).
  x->print( ). 

