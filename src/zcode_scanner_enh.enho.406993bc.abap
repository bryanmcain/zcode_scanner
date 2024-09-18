"Name: \PR:AFX_CODE_SCANNER\IC:AFX_CODE_SCANNER\SE:END\EI
ENHANCEMENT 0 ZCODE_SCANNER_ENH.
SELECTION-SCREEN begin of block z1 with FRAME TITLE ll_title.
  PARAMETERS: p_enh as CHECKBOX DEFAULT con_true,
              p_wdy as CHECKBOX DEFAULT con_true,
              p_bsp as CHECKBOX DEFAULT con_true,
              p_frm as CHECKBOX DEFAULT con_true.
SELECTION-SCREEN end of block z1.


INITIALIZATION.
  ll_title = 'Extended Scan Options'.
  %_p_enh_%_app_%-text = 'Enhancements'.
  %_p_wdy_%_app_%-text = 'Web Dynpro Components'.
  %_p_bsp_%_app_%-text = 'BSP Applications'.
  %_p_frm_%_app_%-text = 'Adobe/Smart forms(May be slow)'.

  "default typical namespace for customer and output routines
  s_devc-low = 'Z*'.
  s_devc-sign = 'I'.
  s_devc-option = 'CP'.
  APPEND s_devc.

  s_devc-option = 'EQ'.
  s_devc-low = 'VN'.
  append s_devc.

  s_devc-low = 'VKON'.
  append s_devc.


*&---------------------------------------------------------------------*
*&      Form  scan_devc_enh
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_DEVC     text
*      -->U_INDEX    text
*      -->U_COUNT    text
*      -->U_CNT_LINE text
*----------------------------------------------------------------------*
FORM scan_devc_enh  USING u_devc     TYPE devclass
                          u_index    TYPE i
                          u_count    TYPE i
                          u_cnt_line TYPE n.
  DATA: l_tab_tadir     TYPE TABLE OF tadir,
        l_str_tadir     TYPE tadir,
        l_cnt           TYPE i,
        l_cnt_str(10)   TYPE c,
        l_idx_devc(10)  TYPE c,
        l_cnt_devc(10)  TYPE c,
        l_aux_devc(20)  TYPE c,
        l_percentage    TYPE p,
        l_tabix_str(10) TYPE c,
        l_rep_name      TYPE sobj_name,
        l_tab_source    TYPE t_tab_long_lines,    "CB
        l_text          TYPE itex132.

* Initialization
  l_idx_devc = u_index.
  l_cnt_devc = u_count.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Get programs of current package
  REFRESH l_tab_tadir.
  IF u_devc <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'ENHO' AND
            devclass = u_devc AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT

  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'ENHO' AND
            devclass = u_devc AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignore invalid TADIR entries.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of programs into list
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
**    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
**    WRITE: / 'Keine Programme in Paket',
**             u_devc, 'gefunden'.
    EXIT.
  ENDIF.
**  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
**  WRITE: / 'Anzahl gefundener Programme in Paket',
**           u_devc, ':', l_cnt.
**  SKIP.

* Process all program sources
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Display progress indicator
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) u_devc l_aux_devc
                '(' 'Enh'(015) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.

*   Read program source and search for specified strings
*    write: / l_str_tadir-obj_name.
*    l_rep_name = l_str_tadir-obj_name.
    SELECT SINGLE enhinclude
      INTO l_rep_name
      FROM enhincinx
     WHERE enhname = l_str_tadir-obj_name.

    CHECK sy-subrc = 0.

    REFRESH l_tab_source.
    READ REPORT l_rep_name INTO l_tab_source.
    IF sy-subrc = 0.
      PERFORM scan_prog USING    u_devc
                                 l_rep_name
                                 u_cnt_line
                        CHANGING l_tab_source.     "CB

**    ELSE.
**      FORMAT COLOR COL_NEGATIVE.
**      WRITE: / 'Report', l_rep_name, 'nicht gefunden!'.
    ENDIF.

  ENDLOOP.


ENDFORM.                    "scan_devc_enh
*&---------------------------------------------------------------------*
*&      Form  scan_devc_wdy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->U_DEVC     text
*      -->U_INDEX    text
*      -->U_COUNT    text
*      -->U_CNT_LINE text
*----------------------------------------------------------------------*
FORM scan_devc_wdy  USING u_devc     TYPE devclass
                          u_index    TYPE i
                          u_count    TYPE i
                          u_cnt_line TYPE n.

  DATA: l_tab_tadir     TYPE TABLE OF tadir,
        l_str_tadir     TYPE tadir,
        l_cnt           TYPE i,
        l_cnt_str(10)   TYPE c,
        l_idx_devc(10)  TYPE c,
        l_cnt_devc(10)  TYPE c,
        l_aux_devc(20)  TYPE c,
        l_percentage    TYPE p,
        l_tabix_str(10) TYPE c,
        l_rep_name      TYPE sobj_name,
        l_tab_source    TYPE t_tab_long_lines,    "CB
        l_text          TYPE itex132.

* Initialization
  l_idx_devc = u_index.
  l_cnt_devc = u_count.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Get programs of current package
  REFRESH l_tab_tadir.
  IF u_devc <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'WDYN' AND
            devclass = u_devc AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT

  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'WDYN' AND
            devclass = u_devc AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

  "if we're also looking at enhancements
  IF p_enh = con_true.
    IF u_devc <> c_devc_tmp.
      SELECT tadir~pgmid
             tadir~object
             enhobj~main_name AS obj_name
             tadir~korrnum
             tadir~srcsystem
             tadir~author
             tadir~srcdep
             tadir~devclass
             tadir~genflag
             tadir~edtflag
             tadir~cproject
             tadir~masterlang
             tadir~versid
             tadir~paknocheck
             tadir~objstablty
             tadir~component
             tadir~crelease
             tadir~delflag
             tadir~translttxt
        FROM tadir
       INNER JOIN enhobj
          ON tadir~obj_name = enhobj~enhname
   APPENDING CORRESPONDING FIELDS OF TABLE l_tab_tadir
       WHERE tadir~pgmid    = 'R3TR' AND
             tadir~object   = 'ENHO' AND
             tadir~devclass = u_devc AND
             tadir~obj_name IN s_rest AND             "#EC CI_SGLSELECT
             enhobj~main_type = 'WDYN'.

    ELSE.
      SELECT tadir~pgmid
             tadir~object
             enhobj~main_name AS obj_name
             tadir~korrnum
             tadir~srcsystem
             tadir~author
             tadir~srcdep
             tadir~devclass
             tadir~genflag
             tadir~edtflag
             tadir~cproject
             tadir~masterlang
             tadir~versid
             tadir~paknocheck
             tadir~objstablty
             tadir~component
             tadir~crelease
             tadir~delflag
             tadir~translttxt
        FROM tadir
       INNER JOIN enhobj
          ON tadir~obj_name = enhobj~obj_name
   APPENDING CORRESPONDING FIELDS OF TABLE l_tab_tadir
       WHERE tadir~pgmid    = 'R3TR' AND
             tadir~object   = 'ENHO' AND
             tadir~devclass = u_devc AND
             tadir~author   = sy-uname AND
             tadir~obj_name IN s_rest AND             "#EC CI_SGLSELECT
             enhobj~main_type = 'WDYN'.
    ENDIF.


    SORT l_tab_tadir BY pgmid object obj_name.
    DELETE ADJACENT DUPLICATES FROM l_tab_tadir
                    COMPARING pgmid object obj_name.

  ENDIF.

* Ignore invalid TADIR entries.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of programs into list
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
**    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
**    WRITE: / 'Keine Programme in Paket',
**             u_devc, 'gefunden'.
    EXIT.
  ENDIF.
**  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
**  WRITE: / 'Anzahl gefundener Programme in Paket',
**           u_devc, ':', l_cnt.
**  SKIP.

* Process all program sources
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Display progress indicator
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) u_devc l_aux_devc
                '(' 'WDA'(016) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.

    DATA: classname TYPE seoclsname.
    DATA: compname TYPE wdy_component_name.
    DATA: includename(35) TYPE c.
    compname = l_str_tadir-obj_name.

*   Read program source and search for specified strings
*    write: / l_str_tadir-obj_name.
*    l_rep_name = l_str_tadir-obj_name.
    cl_wdy_wb_naming_service=>get_classname_for_component(
      EXPORTING
        p_component        = compname
      RECEIVING
        p_classname        = classname
      EXCEPTIONS
        no_generation_info = 1
        OTHERS             = 2 ).

    CHECK sy-subrc = 0.

    CONCATENATE classname '==ccimp' INTO includename.
    CONDENSE classname NO-GAPS.

    DATA: l_tab_includes LIKE l_tab_source.
    FIELD-SYMBOLS: <includes> LIKE LINE OF l_tab_includes.
    READ REPORT includename INTO l_tab_includes.
    DELETE l_tab_includes WHERE table_line(7) <> 'INCLUDE'.
    CHECK l_tab_includes[] IS NOT INITIAL.

    LOOP AT l_tab_includes ASSIGNING <includes>.
      REFRESH l_tab_source.

      l_rep_name = <includes>+8(30).

      READ REPORT l_rep_name INTO l_tab_source.
      IF sy-subrc = 0.
        PERFORM scan_prog USING    u_devc
                                   l_rep_name
                                   u_cnt_line
                          CHANGING l_tab_source.     "CB

**    ELSE.
**      FORMAT COLOR COL_NEGATIVE.
**      WRITE: / 'Report', l_rep_name, 'nicht gefunden!'.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    "scan_devc_wdy
FORM scan_devc_bsp USING   u_devc     TYPE devclass
                           u_index    TYPE i
                           u_count    TYPE i
                           u_cnt_line TYPE n.
  DATA: l_tab_tadir     TYPE TABLE OF tadir,
        l_str_tadir     TYPE tadir,
        l_str_e071      TYPE e071,
        l_cnt           TYPE i,
        l_cnt_str(10)   TYPE c,
        l_idx_devc(10)  TYPE c,
        l_cnt_devc(10)  TYPE c,
        l_aux_devc(20)  TYPE c,
        l_percentage    TYPE p,
        l_tabix_str(10) TYPE c,
        l_rep_name      TYPE sobj_name,
        l_tab_source    TYPE TABLE OF t_abapsource_long,
        l_text          TYPE itex132,
        l_tab_trdir     TYPE STANDARD TABLE OF trdir,
        l_str_trdir     LIKE LINE OF l_tab_trdir,
        l_tab_selopt    TYPE STANDARD TABLE OF rsdsselopt,
        l_str_selopt    LIKE LINE OF l_tab_selopt,
        lt_pages        TYPE o2pagelist,
        bsp_app_name    TYPE o2applname,
        ls_pagekey      TYPE o2pagkey,
        imp_class       TYPE seoclsname.


* Initialization
  l_idx_devc = u_index.
  l_cnt_devc = u_count.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Get classes of current package
  REFRESH l_tab_tadir.
  IF u_devc <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'WAPA' AND
            devclass = u_devc AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            object   = 'WAPA' AND
            devclass = u_devc AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignore invalid TADIR entries.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of function pools into list
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
*    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*    WRITE: / 'Keine Klassen in Entwicklungsklasse',
*             u_devc, 'gefunden'.
*    EXIT.
  ENDIF.
*  SKIP.
*  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
*  WRITE: / 'Anzahl gefundener Klassen in Entwicklungsklasse',
*           u_devc, ':', l_cnt.
*  SKIP.

* Process all function pools
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Display progress indicator
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) u_devc l_aux_devc
                '(' 'Klasse'(012) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.


    bsp_app_name = l_str_tadir-obj_name.
    cl_o2_api_pages=>get_all_pages(
      EXPORTING
        p_applname     = bsp_app_name
        p_version      = so2_version_active
      IMPORTING
        p_pages        = lt_pages
           ).


* get includes for current class
    REFRESH l_tab_selopt.

    LOOP AT lt_pages ASSIGNING FIELD-SYMBOL(<page>).

      REFRESH l_tab_selopt.
      l_str_selopt-sign = 'I'.
      l_str_selopt-option = 'CP'.
      CONCATENATE <page>-implclass '*' INTO l_str_selopt-low.
      APPEND l_str_selopt TO l_tab_selopt.

      SELECT * FROM trdir APPENDING TABLE l_tab_trdir
                WHERE name IN l_tab_selopt.           "#EC CI_SGLSELECT

    ENDLOOP.

    LOOP AT l_tab_trdir INTO l_str_trdir.
      l_rep_name = l_str_e071-obj_name.
      REFRESH l_tab_source.
      l_rep_name = l_str_trdir-name.
      READ REPORT l_rep_name INTO l_tab_source.

      IF sy-subrc = 0.
        PERFORM scan_prog USING    u_devc
                                   l_rep_name
                                   u_cnt_line
                          CHANGING l_tab_source.     "CB
      ELSE.
        FORMAT COLOR COL_NEGATIVE.
        WRITE: / 'Report'(009), l_rep_name, 'nicht gefunden!'(013).
      ENDIF.
    ENDLOOP.

  ENDLOOP .

ENDFORM.                    " scan_devc_bsp

FORM scan_devc_forms USING u_devc     TYPE devclass
                          u_index    TYPE i
                          u_count    TYPE i
                          u_cnt_line TYPE n.
  DATA: l_tab_tadir     TYPE TABLE OF tadir,
        l_str_tadir     TYPE tadir,
        l_tab_e071      TYPE TABLE OF e071,
        l_str_e071      TYPE e071,
        l_str_tfdir     TYPE tfdir,
        l_cnt           TYPE i,
        l_cnt_str(10)   TYPE c,
        l_idx_devc(10)  TYPE c,
        l_cnt_devc(10)  TYPE c,
        l_aux_devc(20)  TYPE c,
        l_percentage    TYPE p,
        l_tabix_str(10) TYPE c,
        l_rep_name      TYPE sobj_name,
        l_tab_source    TYPE TABLE OF t_abapsource_long,       "CB
        l_text          TYPE itex132,
        lv_fm_name      TYPE rs38l_fnam,
        lv_form_name    TYPE char30.

* Initialization
  l_idx_devc = u_index.
  l_cnt_devc = u_count.
  CONCATENATE l_idx_devc '/' l_cnt_devc INTO l_aux_devc.
  CONDENSE l_aux_devc.

* Get function pools of current package
  REFRESH l_tab_tadir.
  IF u_devc <> c_devc_tmp.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            ( object   = 'SSFO' OR
              object   = 'SFPF' ) AND
            devclass = u_devc AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ELSE.
    SELECT * FROM tadir INTO TABLE l_tab_tadir
      WHERE pgmid    = 'R3TR' AND
            ( object   = 'SSFO' OR
              object   = 'SFPF' ) AND
            devclass = u_devc AND
            author   = sy-uname AND
            obj_name IN s_rest.                       "#EC CI_SGLSELECT
  ENDIF.

* Ignore invalid TADIR entries.
  DELETE l_tab_tadir WHERE obj_name IS INITIAL.

* Write count of function pools into list
  DESCRIBE TABLE l_tab_tadir LINES l_cnt.
  IF l_cnt = 0.
**    FORMAT COLOR COL_NORMAL INTENSIFIED ON.
**    WRITE: / 'Keine Funktionsgruppen in Paket',
**             u_devc, 'gefunden'.
    EXIT.
  ENDIF.
**  SKIP.
**  FORMAT COLOR COL_NORMAL INTENSIFIED ON.
**  WRITE: / 'Anzahl gefundener Funktionsgruppen in Paket',
**           u_devc, ':', l_cnt.
**  SKIP.

* Process all function pools
  l_cnt_str = l_cnt.
  CONDENSE l_cnt_str.
  LOOP AT l_tab_tadir INTO l_str_tadir.
    l_tabix_str = sy-tabix.
    CONDENSE l_tabix_str.

*   Display progress indicator
    l_percentage = 100 * ( sy-tabix / l_cnt ).
    CONCATENATE 'Scanne Paket'(008) u_devc l_aux_devc
                '(' 'FuGr'(011) l_tabix_str 'von'(010) l_cnt_str ')'
                INTO l_text SEPARATED BY space.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = l_percentage
        text       = l_text.

    CLEAR: lv_fm_name, lv_form_name.

    lv_form_name = l_str_tadir-obj_name.

    CASE l_str_tadir-object.
      WHEN 'SSFO'.
        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = lv_form_name
          IMPORTING
            fm_name            = lv_fm_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

      WHEN 'SFPF'.
        TRY.
            CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
              EXPORTING
                i_name     = lv_form_name
              IMPORTING
                e_funcname = lv_fm_name.
          CATCH cx_fp_api_repository.
            continue.
          CATCH cx_fp_api_usage.
            continue.
          CATCH cx_fp_api_internal.
            continue.
        ENDTRY.

        IF lv_fm_name IS INITIAL.
          CONTINUE.
        ENDIF.

    ENDCASE.



*   Get function pool objects
*    write: / l_str_tadir-obj_name.
    l_str_e071-pgmid    = l_str_tadir-pgmid.
    l_str_e071-object   = l_str_tadir-object.
    l_str_e071-obj_name = lv_fm_name.
    REFRESH l_tab_e071.
    CALL FUNCTION 'STOR_RESOLVE_FUGR'
      EXPORTING
        is_e071 = l_str_e071
      TABLES
        tt_e071 = l_tab_e071
      EXCEPTIONS
        OTHERS  = 0.

*   Read basis program sources and search for specified strings
    LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'REPO' .
      l_rep_name = l_str_e071-obj_name.
      REFRESH l_tab_source.
      READ REPORT l_rep_name INTO l_tab_source.
      IF sy-subrc = 0.
        PERFORM scan_prog USING    u_devc
                                   l_rep_name
                                   u_cnt_line
                          CHANGING l_tab_source.       "CB
**      ELSE.
**        FORMAT COLOR COL_NEGATIVE.
**        WRITE: / 'Report', l_rep_name, 'nicht gefunden!'.
      ENDIF.
    ENDLOOP .

* (A) Keine generierten Dialoge?!? Das sollte man evtl. optional
*     anbieten (Zeitpunkt-Routinen!)
*   Read function module sources and search for specified strings
    LOOP AT l_tab_e071 INTO l_str_e071 WHERE object = 'FUNC' .
      IF l_str_e071-obj_name(4) = 'VIEW'. "Keine gen. Dialoge
        CONTINUE.
      ENDIF.
      SELECT SINGLE * FROM tfdir INTO l_str_tfdir
        WHERE funcname = l_str_e071-obj_name.         "#EC CI_SGLSELECT
      IF sy-subrc = 0.
        CONCATENATE l_str_tfdir-pname 'U' l_str_tfdir-include
                    INTO l_rep_name.
        REPLACE 'SAPL' WITH 'L' INTO l_rep_name.
        REFRESH l_tab_source.
        READ REPORT l_rep_name INTO l_tab_source.
        IF sy-subrc = 0.
          PERFORM scan_prog USING    u_devc
                                     l_rep_name
                                     u_cnt_line
                            CHANGING l_tab_source.     "CB
**        ELSE.
**          FORMAT COLOR COL_NEGATIVE.
**          WRITE: / 'Report', l_rep_name, 'nicht gefunden!'.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP .

ENDFORM.                    " scan_devc_forms
form enhanced_scan USING u_devc         TYPE devclass
                         u_index        TYPE i
                         u_count        TYPE i
                         u_cnt_line     TYPE n.
  IF p_enh = con_true.
    PERFORM scan_devc_enh
      USING u_devc u_index u_count u_cnt_line.
  ENDIF.
  IF p_wdy = con_true.
    PERFORM scan_devc_wdy
      USING u_devc u_index u_count u_cnt_line.
  ENDIF.
  IF p_bsp = con_true.
    PERFORM scan_devc_bsp
      USING u_devc u_index u_count u_cnt_line.
  ENDIF.
  IF p_frm = con_true.
    PERFORM scan_devc_forms
      USING u_devc u_index u_count u_cnt_line.
  ENDIF.
endform.
ENDENHANCEMENT.
