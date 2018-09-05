*&---------------------------------------------------------------------*
*& Include  ZCL_MATRIX
*&---------------------------------------------------------------------*

INCLUDE ZCL_VECTOR.

*----------------------------------------------------------------------*
* MATRIX OPERATIONS
*----------------------------------------------------------------------*

CLASS zcl_matrix DEFINITION.
  PUBLIC SECTION.
    METHODS:
* get internal table of internal tables of float values (if somebody need it)
      get RETURNING VALUE(rt_matrix) TYPE ty_float_matrix,
* get row of the matrix as a vector
      get_row IMPORTING iv_index TYPE i RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector  EXCEPTIONS INDEX_ERROR,
* get column of the matrix as a vector
      get_column IMPORTING iv_index TYPE i RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS INDEX_ERROR,
* get ij-element of the matrix
      get_pos IMPORTING i TYPE i j TYPE i RETURNING VALUE(rv_value) TYPE ty_float EXCEPTIONS INDEX_ERROR,
* set the matrix from any table (see zcl_matrix=>create from) :
      set IMPORTING it_table TYPE INDEX TABLE iv_check TYPE flag DEFAULT 'X' EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* set row of the matrix from a vector
      set_row IMPORTING iv_index TYPE i io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR INDEX_ERROR,
* set column of the matrix from a vector
      set_column IMPORTING iv_index TYPE i io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR INDEX_ERROR,
* set value in position
      set_pos IMPORTING i TYPE i j TYPE i iv_value TYPE ty_float EXCEPTIONS SIZE_ERROR INDEX_ERROR,
* get number of rows
      nrows RETURNING VALUE(rv_size) TYPE i,
* get number of columns
      ncolumns RETURNING VALUE(rv_size) TYPE i,
* append row to the matrix
      append_row IMPORTING io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
* append column to the matrix
      append_column IMPORTING io_vector TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,

* get certain rows of the matrix by from-to indexes, or by defined list of indexes
      slice_by_rows IMPORTING iv_from TYPE i OPTIONAL iv_to TYPE i OPTIONAL it_indexes TYPE int4_table OPTIONAL iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS INPUT_ERROR,

* operations with another matrix
      add IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
      subtract IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
      multiply IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
* multiply by vector
      multiplyv IMPORTING io_vector TYPE REF TO zcl_vector RETURNING VALUE(ro_vector) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR,
* multiply by number
      multiplyn IMPORTING iv_value TYPE ty_float iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,

* transpose
      transpose IMPORTING iv_inplace TYPE flag DEFAULT ' ' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix,
* solve linear system of equations Ax = b, with A as current matrix and b as io_vector
      solve IMPORTING io_vector TYPE REF TO zcl_vector RETURNING VALUE(ro_result) TYPE REF TO zcl_vector EXCEPTIONS SIZE_ERROR RANK_ERROR COUNT_ERROR,

      min  RETURNING VALUE(rv_value) TYPE ty_float,
      max  RETURNING VALUE(rv_value) TYPE ty_float,

* write contents to spool (be careful with large dimensions)
      print,
* save matrix in csv file
      save_csv IMPORTING iv_filename TYPE string iv_sep TYPE c DEFAULT ';' EXCEPTIONS OUTPUT_ERROR,

* constructor:
      constructor IMPORTING rows TYPE i cols TYPE i            " size > 0
                            fill_value TYPE ty_float OPTIONAL. " fill_value - default filler value
    CLASS-METHODS:
* create a matrix from any table :
*   if it is standard internal table - process only numeric columns (not TYPE n) - make matrix from them
*   if it is ty_float_matrix         - check consistency and make a copy
*   if it is ty_float_vector         - make matrix from a vector  ( N rows, 1 columns )
      create_from IMPORTING it_table TYPE ANY TABLE iv_check TYPE flag DEFAULT 'X' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create copy of the matrix
      create_copy IMPORTING io_matrix TYPE REF TO zcl_matrix RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create diagonal matrix with filler value or with certain vector on diag.
      create_diag IMPORTING iv_size TYPE i iv_value TYPE ty_float DEFAULT 1 io_vector TYPE REF TO zcl_vector OPTIONAL RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR,
* create matrix from csv-file
      load_from_csv IMPORTING iv_filename TYPE string iv_sep TYPE c DEFAULT ';' iv_header TYPE flag DEFAULT 'X' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
* create matrix from strings separated by chosen "separator"
      load_from_text IMPORTING it_strings TYPE table_of_strings iv_sep TYPE c DEFAULT ';' RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR INPUT_ERROR,
      is_numeric_type_kind IMPORTING iv_type_kind TYPE c RETURNING VALUE(rv_ok) TYPE flag.


  PRIVATE SECTION.
    METHODS:
      get_ref RETURNING VALUE(rr_data) TYPE REF TO data,
      operation_m IMPORTING io_matrix TYPE REF TO zcl_matrix iv_inplace TYPE flag DEFAULT ' ' iv_op TYPE char1 RETURNING VALUE(ro_matrix) TYPE REF TO zcl_matrix EXCEPTIONS SIZE_ERROR.

    DATA: mt_float_matrix TYPE ty_float_matrix,
          mv_rows TYPE i,
          mv_cols TYPE i.
ENDCLASS.

CLASS zcl_matrix IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_fill TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.

    IF fill_value IS SUPPLIED.
      lv_fill = fill_value.
    ENDIF.

    REFRESH mt_float_matrix.
    mv_rows = nmax( val1 = rows val2 = 0 ).
    mv_cols = nmax( val1 = cols val2 = 0 ).

    DO rows TIMES.
      DO cols TIMES.
        APPEND lv_fill TO lt_float_vector.
      ENDDO.
      APPEND lt_float_vector TO mt_float_matrix.
    ENDDO.
  ENDMETHOD.

  METHOD get.
    rt_matrix[] = mt_float_matrix[].
  ENDMETHOD.

  METHOD get_ref.
    GET REFERENCE OF mt_float_matrix INTO rr_data.
  ENDMETHOD.

  METHOD get_row.
    FIELD-SYMBOLS: <fs_float_vector> TYPE ty_float_vector.

    IF iv_index < 0 OR iv_index > mv_rows.
      RAISE INDEX_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_float_vector> INDEX iv_index.
    ro_vector = zcl_vector=>create_from( <fs_float_vector> ).
  ENDMETHOD.

  METHOD get_column.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_value TYPE ty_float.
    FIELD-SYMBOLS: <fs_tmp_vector> TYPE ty_float_vector.

    IF iv_index < 0 OR iv_index > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.

    LOOP AT mt_float_matrix ASSIGNING <fs_tmp_vector>.
      READ TABLE <fs_tmp_vector> INTO lv_value INDEX iv_index.
      APPEND lv_value TO lt_float_vector.
    ENDLOOP.

    ro_vector = zcl_vector=>create_from( lt_float_vector ).
  ENDMETHOD.

  METHOD get_pos.
    FIELD-SYMBOLS: <fs_float_vector> TYPE ty_float_vector.

    IF i < 0 OR i > mv_rows OR j < 0 OR j > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_float_vector> INDEX i.
    READ TABLE <fs_float_vector> INTO rv_value INDEX j.
  ENDMETHOD.

  METHOD is_numeric_type_kind.
    rv_ok = abap_false.
    IF iv_type_kind = cl_abap_typedescr=>typekind_int OR
       iv_type_kind = cl_abap_typedescr=>typekind_int1 OR
       iv_type_kind = cl_abap_typedescr=>typekind_int2 OR
       "iv_type_kind = cl_abap_typedescr=>typekind_num OR
       "iv_type_kind = cl_abap_typedescr=>typekind_numeric OR
       iv_type_kind = cl_abap_typedescr=>typekind_decfloat OR
       iv_type_kind = cl_abap_typedescr=>typekind_decfloat16 OR
       iv_type_kind = cl_abap_typedescr=>typekind_decfloat34 OR
       iv_type_kind = cl_abap_typedescr=>typekind_float OR
       iv_type_kind = cl_abap_typedescr=>typekind_packed.
      rv_ok = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD set.
    DATA: lo_struc TYPE REF TO cl_abap_structdescr.
    DATA: lo_typedesc TYPE REF TO cl_abap_typedescr.
    DATA: lo_table TYPE REF TO cl_abap_tabledescr.
    DATA: lo_elem TYPE REF TO cl_abap_elemdescr.
    DATA: lt_comp TYPE abap_compdescr_tab,
          ls_comp LIKE LINE OF lt_comp.
    DATA: lt_names TYPE ttfieldname.
    DATA: lv_name LIKE LINE OF lt_names.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_vector TYPE flag.
    DATA: lv_assumed_cols TYPE i.
    FIELD-SYMBOLS: <fs_line> TYPE ANY,
                   <fs> TYPE ANY,
                   <fs_table> TYPE INDEX TABLE.

    IF iv_check = abap_false.
      mt_float_matrix[] = it_table[].
      mv_rows = LINES( it_table ).
      READ TABLE it_table ASSIGNING <fs_table> INDEX 1.
      mv_cols = LINES( <fs_table> ).
    ENDIF.

    IF LINES( it_table ) = 0.
      RAISE SIZE_ERROR.
    ENDIF.

    READ TABLE it_table ASSIGNING <fs_line> INDEX 1.

    CALL METHOD cl_abap_structdescr=>describe_by_data
      EXPORTING
        p_data      = <fs_line>
      RECEIVING
        p_descr_ref = lo_typedesc.

    IF lo_typedesc->kind = cl_abap_typedescr=>kind_table.
*   В строчке одна ячейка и это внутренняя таблица
*   Т.е это наш внутренний формат ty_float_matrix
      ASSIGN <fs_line> TO <fs_table>.
      lv_assumed_cols = LINES( <fs_table> ).

      " проверим типы оставшихся строк и
      " проверим равенство столбцов во всех строках
      LOOP AT it_table ASSIGNING <fs_line> FROM 2.

        CALL METHOD cl_abap_structdescr=>describe_by_data
          EXPORTING
            p_data      = <fs_line>
          RECEIVING
            p_descr_ref = lo_typedesc.
        IF NOT lo_typedesc->kind = cl_abap_typedescr=>kind_table.
          RAISE INPUT_ERROR.
        ENDIF.

        ASSIGN <fs_line> TO <fs_table>.
        IF LINES( <fs_table> ) <> lv_assumed_cols.
          RAISE SIZE_ERROR.
        ENDIF.

      ENDLOOP.

      " Все ок, переконвертируем все во float, если вдруг это было не так
      REFRESH mt_float_matrix[].
      LOOP AT it_table ASSIGNING <fs_table>.
        REFRESH lt_float_vector.
        LOOP AT <fs_table> ASSIGNING <fs>.
          lv_float = <fs>.
          APPEND lv_float TO lt_float_vector.
        ENDLOOP.
        APPEND lt_float_vector TO mt_float_matrix.
      ENDLOOP.

      mv_rows = LINES( it_table ).
      mv_cols = lv_assumed_cols.

    ELSEIF lo_typedesc->kind = cl_abap_typedescr=>kind_elem.
*   В строчке одна ячейка / поле
*   Если это число, то м.б. это наш внутрений формат ty_float_vector

      lo_elem ?= lo_typedesc.
      lv_assumed_cols = 1.

      IF zcl_matrix=>is_numeric_type_kind( lo_elem->type_kind ) = abap_false.
        RAISE INPUT_ERROR.
      ENDIF.

      " проверим что типы оставшихся строк такие же - просто числа
      LOOP AT it_table ASSIGNING <fs_line> FROM 2.

        CALL METHOD cl_abap_structdescr=>describe_by_data
          EXPORTING
            p_data      = <fs_line>
          RECEIVING
            p_descr_ref = lo_typedesc.

        IF NOT lo_typedesc->kind = cl_abap_typedescr=>kind_elem.
          RAISE INPUT_ERROR.
        ENDIF.
        lo_elem ?= lo_typedesc.
        IF zcl_matrix=>is_numeric_type_kind( lo_elem->type_kind ) = abap_false.
          RAISE INPUT_ERROR.
        ENDIF.

      ENDLOOP.

      " Все ок, переконвертируем все во float, если вдруг это было не так
      REFRESH mt_float_matrix[].
      LOOP AT it_table ASSIGNING <fs_line>.
        REFRESH lt_float_vector.
        lv_float = <fs_line>.
        APPEND lv_float TO lt_float_vector.
        APPEND lt_float_vector TO mt_float_matrix.
      ENDLOOP.

      mv_rows = LINES( it_table ).
      mv_cols = lv_assumed_cols.

    ELSE.
*   В строчке какая-то структура
*   Отфильтруем все численные поля и возьмем в матрицу

      lo_struc ?= lo_typedesc.
      lt_comp = lo_struc->components.

      LOOP AT lt_comp INTO ls_comp.
        IF zcl_matrix=>is_numeric_type_kind( ls_comp-type_kind ) = abap_true.
          APPEND ls_comp-name TO lt_names.
        ENDIF.
      ENDLOOP.

      IF LINES( lt_names ) = 0.
        RAISE INPUT_ERROR.
      ENDIF.

      mv_rows = LINES( it_table ).
      mv_cols = LINES( lt_names ).
      REFRESH mt_float_matrix.

      " Все ок, переконвертируем все во float
      LOOP AT it_table ASSIGNING <fs_line>.
        REFRESH lt_float_vector.
        LOOP AT lt_names INTO lv_name.
          ASSIGN COMPONENT lv_name OF STRUCTURE <fs_line> TO <fs>.
          lv_float = <fs>.
          APPEND lv_float TO lt_float_vector.
        ENDLOOP.
        APPEND lt_float_vector TO mt_float_matrix.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD create_from.
    CREATE OBJECT ro_matrix EXPORTING rows = 0 cols = 0.

    ro_matrix->set( EXPORTING it_table = it_table iv_check = iv_check ).
  ENDMETHOD.

  METHOD create_copy.
    DATA: lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_table> TYPE INDEX TABLE.

    lr_data = io_matrix->get_ref( ).
    ASSIGN lr_data->* TO <fs_table>.
    ro_matrix = zcl_matrix=>create_from( it_table = <fs_table> iv_check = ' ' ).
  ENDMETHOD.

  METHOD create_diag.
    IF iv_size <= 0.
      RAISE SIZE_ERROR.
    ENDIF.

    CREATE OBJECT ro_matrix EXPORTING rows = iv_size cols = iv_size.
    IF io_vector IS NOT BOUND.
      DO iv_size TIMES.
        ro_matrix->set_pos( i = sy-index j = sy-index iv_value = iv_value ).
      ENDDO.
    ELSE.
      DO io_vector->size( ) TIMES.
        ro_matrix->set_pos( i = sy-index j = sy-index iv_value = io_vector->get_value( sy-index ) ).
      ENDDO.
    ENDIF.
  ENDMETHOD.

  METHOD set_row.
    FIELD-SYMBOLS: <fs_row> TYPE ANY TABLE.

    IF iv_index < 0 OR iv_index > mv_rows.
      RAISE INDEX_ERROR.
    ENDIF.
    IF mv_cols <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_row> INDEX iv_index.
    <fs_row> = io_vector->get( ).
  ENDMETHOD.

  METHOD set_column.
    DATA: lt_float_vector TYPE ty_float_vector,
          lv_float TYPE ty_float.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE,
                   <fs> TYPE ty_float.

    IF iv_index < 0 OR iv_index > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.
    IF mv_rows <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lt_float_vector = io_vector->get( ).

    LOOP AT mt_float_matrix ASSIGNING <fs_row>.
      READ TABLE lt_float_vector INTO lv_float INDEX sy-tabix.
      READ TABLE <fs_row> ASSIGNING <fs> INDEX iv_index.
      <fs> = lv_float.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_pos.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE,
                   <fs> TYPE ty_float.

    IF i < 0 OR i > mv_rows OR j < 0 OR j > mv_cols.
      RAISE INDEX_ERROR.
    ENDIF.

    READ TABLE mt_float_matrix ASSIGNING <fs_row> INDEX i.
    READ TABLE <fs_row> ASSIGNING <fs> INDEX j.
    <fs> = iv_value.
  ENDMETHOD.

  METHOD nrows.
    rv_size = mv_rows.
  ENDMETHOD.

  METHOD ncolumns.
    rv_size = mv_cols.
  ENDMETHOD.

  METHOD append_row.
    IF io_vector->size( ) <> mv_cols.
      RAISE SIZE_ERROR.
    ENDIF.

    APPEND io_vector->get( ) TO mt_float_matrix.
    mv_rows = mv_rows + 1.
  ENDMETHOD.

  METHOD append_column.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_float TYPE ty_float.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE.

    IF io_vector->size( ) <> mv_rows.
      RAISE SIZE_ERROR.
    ENDIF.

    lt_float_vector = io_vector->get( ).

    LOOP AT mt_float_matrix ASSIGNING <fs_row>.
      READ TABLE lt_float_vector INTO lv_float INDEX sy-tabix.
      APPEND lv_float TO <fs_row>.
    ENDLOOP.
    mv_cols = mv_cols + 1.
  ENDMETHOD.

  METHOD slice_by_rows.
    DATA: lr_data TYPE REF TO DATA.
    DATA: lt_new_matrix TYPE ty_float_matrix.
    DATA: lv_index TYPE i.
    FIELD-SYMBOLS: <fs_float_matrix> TYPE INDEX TABLE.
    FIELD-SYMBOLS: <fs_row> TYPE INDEX TABLE.

    IF iv_from > iv_to.
      RAISE INPUT_ERROR.
    ENDIF.

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_matrix EXPORTING rows = 0 cols = 0. "empty
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix>.
    ELSE.
      REFRESH lt_new_matrix.
      ASSIGN lt_new_matrix TO <fs_float_matrix>.
    ENDIF.

    IF iv_from IS NOT INITIAL AND iv_to IS NOT INITIAL.
      LOOP AT mt_float_matrix ASSIGNING <fs_row> FROM iv_from TO iv_to.
        APPEND <fs_row> TO <fs_float_matrix>.
      ENDLOOP.

    ELSEIF it_indexes[] IS NOT INITIAL.
      LOOP AT it_indexes INTO lv_index.
        READ TABLE mt_float_matrix ASSIGNING <fs_row> INDEX lv_index.
        IF sy-subrc = 0.
          APPEND <fs_row> TO <fs_float_matrix>.
        ENDIF.
      ENDLOOP.

    ELSE.
      RAISE INPUT_ERROR.

    ENDIF.

    IF iv_inplace = abap_false.
      ro_matrix->mv_rows = LINES( <fs_float_matrix> ).
      ro_matrix->mv_cols = me->mv_cols.
    ELSE.
      me->set( it_table = lt_new_matrix iv_check = ' ' ).
      ro_matrix = me.
    ENDIF.

  ENDMETHOD.

  METHOD operation_m.
    FIELD-SYMBOLS: <fs> TYPE ty_float,
                   <fs2> TYPE ty_float,
                   <fs_float_vector1> TYPE ty_float_vector,
                   <fs_float_vector2> TYPE ty_float_vector,
                   <fs_float_matrix1> TYPE ty_float_matrix,
                   <fs_float_matrix2> TYPE ty_float_matrix.
    DATA: lt_float_matrix_out TYPE ty_float_matrix.
    DATA: lr_data TYPE REF TO data.

    lr_data = io_matrix->get_ref( ).
    ASSIGN lr_data->* TO <fs_float_matrix2>.

    IF io_matrix->nrows( ) <> mv_rows OR io_matrix->ncolumns( ) <> mv_cols.
      RAISE SIZE_ERROR.
    ENDIF.

    IF iv_inplace = abap_false.
      ro_matrix = zcl_matrix=>create_from( it_table = mt_float_matrix iv_check = ' ' ).
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix1>.
    ELSE.
      ASSIGN mt_float_matrix TO <fs_float_matrix1>.
    ENDIF.

    LOOP AT <fs_float_matrix1> ASSIGNING <fs_float_vector1>.
      READ TABLE <fs_float_matrix2> ASSIGNING <fs_float_vector2> INDEX sy-tabix.

      LOOP AT <fs_float_vector1> ASSIGNING <fs>.
        READ TABLE <fs_float_vector2> ASSIGNING <fs2> INDEX sy-tabix.

        CASE iv_op.
          WHEN '+'.
            <fs> = <fs> + <fs2>.
          WHEN '-'.
            <fs> = <fs> - <fs2>.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.

    IF iv_inplace = abap_true.
      ro_matrix = me.
    ENDIF.
  ENDMETHOD.

  METHOD add.
    ro_matrix = me->operation_m( io_matrix = io_matrix iv_inplace = iv_inplace iv_op = '+' ).
  ENDMETHOD.

  METHOD subtract.
    ro_matrix = me->operation_m( io_matrix = io_matrix iv_inplace = iv_inplace iv_op = '-' ).
  ENDMETHOD.

  METHOD multiply.
    DATA: m TYPE i.
    DATA: n TYPE i.
    DATA: k TYPE i.
    DATA: i TYPE i.
    DATA: j TYPE i.
    DATA: l TYPE i.
    DATA: val TYPE ty_float.
    DATA: val1 TYPE ty_float.
    DATA: val2 TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lt_new_vector TYPE ty_float_vector.
    DATA: lt_new_matrix TYPE ty_float_matrix.
    "DATA: lo_row TYPE REF TO zcl_vector.
    "DATA: lo_column TYPE REF TO zcl_vector.
    DATA: lv_first TYPE i.
    DATA: lr_data TYPE REF TO DATA.
    FIELD-SYMBOLS: <fs_float_matrix> TYPE ty_float_matrix.

    m = mv_rows.
    k = mv_cols.
    IF k <> io_matrix->nrows( ).
      RAISE SIZE_ERROR.
    ENDIF.
    n = io_matrix->ncolumns( ).

    IF iv_inplace = abap_false.
      CREATE OBJECT ro_matrix EXPORTING rows = 0 cols = 0. "empty
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix>.
      REFRESH <fs_float_matrix>.
    ELSE.
      REFRESH lt_new_matrix.
      ASSIGN lt_new_matrix TO <fs_float_matrix>.
    ENDIF.

    DO m TIMES.
      i = sy-index.
      CLEAR: lt_new_vector.
      "lo_row = me->get_row( i ).
      DO n TIMES.
        j = sy-index.
        "lo_column = io_matrix->get_column( j ).
        CLEAR val.
        DO k TIMES.
          l = sy-index.
          TRY.
            val = val + me->get_pos( i = i j = l ) * io_matrix->get_pos( i = l j = j ).
            CATCH cx_root.
              EXIT.
          ENDTRY.
"          val = val + lo_row->dot( lo_column ).
        ENDDO.
        APPEND val TO lt_new_vector.
      ENDDO.
      APPEND lt_new_vector TO <fs_float_matrix>.
    ENDDO.

    IF iv_inplace = abap_false.
      ro_matrix->mv_rows = m.
      ro_matrix->mv_cols = n.
    ELSE.
      me->set( it_table = lt_new_matrix iv_check = ' ' ).
      ro_matrix = me.
    ENDIF.
  ENDMETHOD.

  METHOD multiplyv.
    DATA: m TYPE i.
    DATA: n TYPE i.
    DATA: i TYPE i.
    DATA: lo_row TYPE REF TO zcl_vector.

    m = mv_rows.
    n = mv_cols.
    IF n <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    CREATE OBJECT ro_vector EXPORTING size = m.

    DO m TIMES.
      i = sy-index.
      lo_row = me->get_row( i ).
      ro_vector->set_value( iv_index = i iv_value = lo_row->dot( io_vector ) ).
    ENDDO.
  ENDMETHOD.

  METHOD multiplyn.
    FIELD-SYMBOLS: <fs> TYPE ty_float,
                   <fs_float_vector> TYPE ty_float_vector,
                   <fs_float_matrix> TYPE ty_float_matrix.
    DATA: lt_float_matrix_out TYPE ty_float_matrix.
    DATA: lr_data TYPE REF TO data.

    IF iv_inplace = abap_false.
      ro_matrix = zcl_matrix=>create_from( it_table = mt_float_matrix iv_check = ' ' ).
      lr_data = ro_matrix->get_ref( ).
      ASSIGN lr_data->* TO <fs_float_matrix>.
    ELSE.
      ASSIGN mt_float_matrix TO <fs_float_matrix>.
    ENDIF.

    LOOP AT <fs_float_matrix> ASSIGNING <fs_float_vector>.
      LOOP AT <fs_float_vector> ASSIGNING <fs>.
        <fs> = <fs> * iv_value.
      ENDLOOP.
    ENDLOOP.

    IF iv_inplace = abap_true.
      ro_matrix = me.
    ENDIF.
  ENDMETHOD.

  METHOD transpose.
    DATA:   lt_float_vector TYPE ty_float_vector,
            lv_float_item TYPE ty_float,
            lv_vector_size TYPE i,
            lv_current_index TYPE i,
            lt_transposed_vector TYPE ty_float_vector,
            lt_transposed_matrix TYPE ty_float_matrix.

    CHECK NOT mt_float_matrix[] IS INITIAL.
    READ TABLE mt_float_matrix INTO lt_float_vector INDEX 1.
    DESCRIBE TABLE lt_float_vector LINES lv_vector_size.

    DO lv_vector_size TIMES.
      lv_current_index = sy-index.
      CLEAR lt_transposed_vector.
      LOOP AT mt_float_matrix INTO lt_float_vector.
        READ TABLE lt_float_vector INTO lv_float_item INDEX lv_current_index.
        APPEND lv_float_item TO lt_transposed_vector.
      ENDLOOP.
      APPEND lt_transposed_vector TO lt_transposed_matrix.
    ENDDO.

    IF iv_inplace = abap_true.
      mt_float_matrix[] = lt_transposed_matrix[].
      mv_cols = mv_rows.
      mv_rows = LINES( mt_float_matrix ).
      ro_matrix = me.
    ELSE.
      ro_matrix = zcl_matrix=>create_from( lt_transposed_matrix ).
    ENDIF.
  ENDMETHOD.

  METHOD solve.
* Решение уравнения Ах = b, где А - наша матрица, b - входящий вектор
* Размерности: А = n * n (иначе не работает), b = 1 * n (иначе не работает)
    DATA: i TYPE i,
          j TYPE i,
          k TYPE i.
    DATA: lv_diag_k_value TYPE ty_float,
          lv_vect_k_value TYPE ty_float,
          lv_max_value TYPE ty_float,
          lv_max_index TYPE i,
          lv_value TYPE ty_float,
          lv_koef TYPE ty_float,
          lv_sum TYPE ty_float.
    DATA: lo_copy_matrix TYPE REF TO zcl_matrix,
          lo_copy_vector TYPE REF TO zcl_vector,
          lo_temp TYPE REF TO zcl_vector.
    DATA: lr_data TYPE REF TO data.
    FIELD-SYMBOLS: <fs_table> TYPE INDEX TABLE.


    IF mv_rows <> mv_cols OR mv_rows <> io_vector->size( ).
      RAISE SIZE_ERROR.
    ENDIF.

    lo_copy_matrix = zcl_matrix=>create_from( it_table = mt_float_matrix iv_check = ' ' ).
    lo_copy_vector = zcl_vector=>create_from( it_column = io_vector->get( ) ).
    CREATE OBJECT ro_result EXPORTING size = mv_rows.

* Прямой ход. Приводим матрицу к треугольному виду
    DO mv_rows TIMES.
      k = sy-index.

      " определение максимального элемента в k-столбце
      lv_max_value = abs( me->get_pos( i = k j = k ) ).
      lv_max_index = k.
      i = k + 1.
      WHILE i <= mv_rows.
        lv_value = abs( me->get_pos( i = i j = k ) ).
        IF lv_value > lv_max_value.
          lv_max_value = lv_value.
          lv_max_index = i.
        ENDIF.
        i = i + 1.
      ENDWHILE.

      " ошибка ранга, нет ненулевых диаг.элементов, матрица плохо обусловлена )
      IF lv_max_value < '0.000001'.
        RAISE RANK_ERROR.
      ENDIF.

      " перестановка lv_max_index и k строки
      lo_temp = me->get_row( lv_max_index ).
      me->set_row( iv_index = lv_max_index io_vector = me->get_row( k ) ).
      me->set_row( iv_index = k io_vector = lo_temp ).
      lv_value = io_vector->get_value( lv_max_index ).
      io_vector->set_value( iv_index = lv_max_index iv_value = io_vector->get_value( k ) ).
      io_vector->set_value( iv_index = k iv_value = lv_value ).

      " выполняем преобразования
      lv_diag_k_value = me->get_pos( i = k j = k ).
      lv_vect_k_value = io_vector->get_value( k ).
      j = sy-index + 1.
      WHILE j <= mv_rows.
        lv_koef = me->get_pos( i = j j = k ) / lv_diag_k_value.
        i = k.
        WHILE i <= mv_rows.
          lv_value = me->get_pos( i = j j = i ) - lv_koef * me->get_pos( i = k j = i ).
          me->set_pos( i = j j = i iv_value = lv_value ).
          i = i + 1.
        ENDWHILE.
        lv_value = io_vector->get_value( j ) - lv_koef * lv_vect_k_value.
        io_vector->set_value( iv_index = j iv_value = lv_value ).

        j = j + 1.
      ENDWHILE.
    ENDDO.

* Обратный ход, вычиляем коэффициенты
    DO mv_rows TIMES.  " k = n..1
      k = mv_rows - sy-index + 1.
      lv_sum = 0.
      j = k + 1.
      WHILE j <= mv_rows.
        lv_sum = lv_sum + me->get_pos( i = k j = j ) * ro_result->get_value( j ).
        j = j + 1.
      ENDWHILE.

      lv_value = ( io_vector->get_value( k ) - lv_sum ) / me->get_pos( i = k j = k ).
      ro_result->set_value( iv_index = k iv_value = lv_value ).
    ENDDO.

* Вернем значение матрицы и вектора в исходное
    lr_data = lo_copy_matrix->get_ref( ).
    ASSIGN lr_data->* TO <fs_table>.

    me->set( it_table = <fs_table> iv_check = ' ' ).
    io_vector->set( it_column = lo_copy_vector->get( ) ).
    FREE lo_copy_matrix.
    FREE lo_copy_vector.

  ENDMETHOD.

  METHOD min.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_row TYPE i.

    rv_value = 0.
    LOOP AT mt_float_matrix INTO lt_float_vector.
      lv_row = sy-tabix.
      LOOP AT lt_float_vector INTO lv_float.
        IF ( lv_row = 1 AND sy-tabix = 1 ) OR ( lv_float < rv_value ).
          rv_value = lv_float.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD max.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lv_row TYPE i.

    rv_value = 0.
    LOOP AT mt_float_matrix INTO lt_float_vector.
      lv_row = sy-tabix.
      LOOP AT lt_float_vector INTO lv_float.
        IF ( lv_row = 1 AND sy-tabix = 1 ) OR ( lv_float > rv_value ).
          rv_value = lv_float.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD print.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.

    LOOP AT mt_float_matrix INTO lt_float_vector.
      LOOP AT lt_float_vector INTO lv_float.
        WRITE: lv_float.
      ENDLOOP.
      NEW-LINE.
    ENDLOOP.
  ENDMETHOD.

  METHOD load_from_csv.
    DATA: lt_strings TYPE table_of_strings.
    DATA: lv_string TYPE string.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = iv_filename
      TABLES
        data_tab                = lt_strings
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0 OR LINES( lt_strings ) = 0.
      RAISE INPUT_ERROR.
    ENDIF.

    IF iv_header = abap_true.
      DELETE lt_strings INDEX 1.
    ENDIF.

    ro_matrix = load_from_text( it_strings = lt_strings iv_sep = iv_sep ).
  ENDMETHOD.

  METHOD load_from_text.
    DATA: lv_string TYPE string.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lt_rawfields TYPE TABLE OF string.
    DATA: lv_rawfield TYPE string.
    DATA: lv_float TYPE ty_float.
    DATA: lv_first TYPE i.

    IF LINES( it_strings ) = 0.
      RAISE INPUT_ERROR.
    ENDIF.


    LOOP AT it_strings INTO lv_string.
      CLEAR: lt_rawfields, lt_float_vector.

      SPLIT lv_string  AT iv_sep INTO TABLE lt_rawfields.
      LOOP AT lt_rawfields INTO lv_rawfield.
        TRY.
            lv_float = lv_rawfield.
          CATCH cx_root.
            RAISE INPUT_ERROR.
        ENDTRY.
        APPEND lv_float TO lt_float_vector.
      ENDLOOP.

      IF lv_first = 0.
        ro_matrix = zcl_matrix=>create_from( lt_float_vector ).
        ro_matrix->transpose( iv_inplace = 'X' ).
        lv_first = 1.
      ELSE.
        ro_matrix->append_row( zcl_vector=>create_from( lt_float_vector ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD save_csv.
    DATA: lv_float TYPE ty_float.
    DATA: lt_float_vector TYPE ty_float_vector.
    DATA: lt_strings TYPE table_of_strings.
    DATA: lv_string TYPE string.
    DATA: lv_strval(30).

    LOOP AT mt_float_matrix INTO lt_float_vector.
      LOOP AT lt_float_vector INTO lv_float.
        WRITE lv_float TO lv_strval LEFT-JUSTIFIED.
        IF lv_string IS INITIAL.
          lv_string = lv_strval.
        ELSE.
          CONCATENATE lv_string lv_strval INTO lv_string SEPARATED BY iv_sep.
        ENDIF.
      ENDLOOP.
      APPEND lv_string TO lt_strings.
    ENDLOOP.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename                = iv_filename
        write_field_separator   = iv_sep
      TABLES
        data_tab                = lt_strings
      EXCEPTIONS
        file_open_error         = 1
        file_read_error         = 2
        no_batch                = 3
        gui_refuse_filetransfer = 4
        invalid_type            = 5
        no_authority            = 6
        unknown_error           = 7
        bad_data_format         = 8
        header_not_allowed      = 9
        separator_not_allowed   = 10
        header_too_long         = 11
        unknown_dp_error        = 12
        access_denied           = 13
        dp_out_of_memory        = 14
        disk_full               = 15
        dp_timeout              = 16
        OTHERS                  = 17.
    IF sy-subrc <> 0.
      RAISE OUTPUT_ERROR.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
