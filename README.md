# abap_ml
Classes and functionality to reproduce basic Machine Learning stuff in ABAP. Starting from vector/matrix API

- operations with float vector/matrix  (zcl_matrix, zcl_vector)
- linear regression (zcl_regression)
- logistic regression ( ..in progress .. )
- stochastic gradient descend  ( ..in progress .. )
- also I'll try to implement simple neural net  ( ..in progress .. )

- class for maintaining dynamic dataset, that supports feature engineering like python pandas DataFrame (  ..in progress .. )

All will be available here, and can be installed by copy/paste and maybe some renaming.

----
Example of matrix/vector operations

DATA: lt_str TYPE TABLE OF string.
DATA: lt_int TYPE TABLE OF ty_float.
DATA: A TYPE REF TO zcl_matrix.
DATA: b TYPE REF TO zcl_vector.
DATA: x TYPE REF TO zcl_vector.

* Load matrix from strings (also supporting file and any table)
  APPEND '-2;1;2' TO lt_str.
  APPEND '2;1;-1' TO lt_str.
  APPEND '-3;-1;2' TO lt_str.
  A = zcl_matrix=>load_from_text( lt_str ).

* Load vector
  APPEND '-3' TO t_vector.
  APPEND '8' TO t_vector.
  APPEND '-11' TO t_vector.
  b = zcl_vector=>create_from( lt_int ).

* Solve system of linear equations Ax = b
  x = A->solve( b ).
  WRITE: /, 'Solution:', /.
  x->print( ).
  WRITE: /, 'Check:', /.
  x = A->multiplyv( x ).
  x->print( ).  
