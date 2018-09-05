# abap_ml
Classes and functionality to reproduce basic Machine Learning stuff in ABAP. Starting from vector/matrix API

- operations with float vector/matrix  ( zcl_matrix, zcl_vector )
- stochastic gradient descent  ( zcl_ml_sgd )
- regression models ( zcl_ml_linear_regressor, zcl_ml_sgd_regressor )
- classification models ( zcl_ml_logistic_regressor, zcl_ml_sgd_classifier )

- class for handling dynamic dataset, that supports feature engineering as python pandas DataFrame (..in future..)

All are available here, and can be installed on your SAP environment by copy/paste and maybe some renaming.

----
You can find examples of matrix/vector operations, linear/log regression and sgd-optimizations in folder "examples"

P.S. Yes, ABAP can do math :)
