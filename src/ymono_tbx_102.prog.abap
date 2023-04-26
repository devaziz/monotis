*&---------------------------------------------------------------------*
*& Report YMONO_TBX_101
*&---------------------------------------------------------------------*
*& Company          : Monotis Digital Innovation
*& Author           : Aziz AKAL
*& E-Mail           : aziz.akal@monotis.com
*& Creation Date    : 01.0.20234
*& Title            : CKM3N sync.(w/o PP)
*&---------------------------------------------------------------------*
*& Description      : CKM3N üzerinden toplu malzeme listeleme sync.(w/o)
*&---------------------------------------------------------------------*
*& CHANGE HISTORY:
*& Version Date.      Author       Description
*&
*&---------------------------------------------------------------------*
REPORT ymono_tbx_102.

TABLES:ckmlhd,mara,mbew,mlkey.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS : p_bukrs LIKE tck07-bukrs MEMORY ID buk OBLIGATORY.
  SELECT-OPTIONS : s_matnr FOR ckmlhd-matnr,
                   s_mtart FOR mara-mtart,
                   s_bklas FOR mbew-bklas,
                   s_matkl FOR mara-matkl,
                   s_vbeln FOR mlkey-vbeln,
                   s_posnr FOR mlkey-posnr,
                   s_pspnr FOR mlkey-pspnr,
                   s_bwkey FOR ckmlhd-bwkey OBLIGATORY.
  PARAMETERS : p_curtp LIKE ckmlcr-curtp OBLIGATORY DEFAULT '10',
               p_bdatj LIKE ckmlcr-bdatj OBLIGATORY,
               p_poper LIKE ckmlcr-poper OBLIGATORY.
  PARAMETERS : p_elehk LIKE tckh3-elehk NO-DISPLAY.
  PARAMETERS : p_value LIKE ckml_s_display-ckm3_value AS LISTBOX
            VISIBLE LENGTH 20,
               p_kkzst LIKE mlccs_s_doc-sel_kkzst AS LISTBOX VISIBLE
                         LENGTH 20,
               p_keart LIKE mlccs_s_doc-sel_keart AS LISTBOX VISIBLE
                         LENGTH 20 MODIF ID g1.

SELECTION-SCREEN END OF BLOCK b1.

DATA : gt_keph_mlcd TYPE ccs01_t_keph_mlcd.
DATA : count(5)  TYPE c,
       tcount(5) TYPE c,
       zcount    TYPE i,
       xcount    TYPE i,
       ycount(5) TYPE c,
       ytext(30) TYPE c.

TYPES : BEGIN OF ty_out.
          INCLUDE STRUCTURE ymono_tbx_102_st1.
TYPES : sort(2),
        END OF ty_out.
DATA : gt_out TYPE STANDARD TABLE OF ty_out.
DATA : gt_fieldcat TYPE TABLE OF lvc_s_fcat WITH HEADER LINE,
       gs_fieldcat TYPE lvc_s_fcat,
       gs_layout   TYPE lvc_s_layo,
       gs_variant  LIKE disvariant.
DATA: gt_sort TYPE TABLE OF lvc_s_sort,
      gs_sort TYPE lvc_s_sort.
DATA : BEGIN OF lt_tckh3 OCCURS 0,
         el_hv LIKE tckh3-el_hv,
         txele LIKE tckh1-txele,
       END OF lt_tckh3.
DATA : lt_alproc_texts TYPE TABLE OF ckml_s_alproc_texts WITH HEADER
LINE.
DATA : lv_datum LIKE sy-datum.
DATA : ls_tck07 LIKE tck07.
DATA : lt_tck07 LIKE TABLE OF tck07.
DATA lv_value TYPE char50.
FIELD-SYMBOLS: <fs> TYPE any.

INITIALIZATION.
  p_bdatj = sy-datum(4).
  p_poper = sy-datum+4(2).

AT SELECTION-SCREEN OUTPUT.
  PERFORM selscr_output.

AT SELECTION-SCREEN.
  PERFORM selscr_input.


START-OF-SELECTION.
  PERFORM get_data.
  PERFORM fill_alv_data.
  PERFORM read_other.
  PERFORM display_data.

*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
FORM get_data .
  DATA: message_failure(128).
  DATA : zgt_keph_mlcd TYPE ccs01_t_keph_mlcd WITH HEADER LINE,
         lt_kalnr      TYPE ckmv0_matobj_str OCCURS 0 WITH HEADER LINE,
         ls_zlt_kalnr  TYPE ckmv0_matobj_str,
         zlt_kalnr     TYPE SORTED TABLE OF ckmv0_matobj_str
                   WITH NON-UNIQUE KEY kalnr.
  DATA : lr_keart TYPE RANGE OF keart WITH HEADER LINE,
         lr_mlcct TYPE RANGE OF ck_slml WITH HEADER LINE,
         lr_kkzst TYPE RANGE OF ck_stufe WITH HEADER LINE,
         lr_curtp TYPE RANGE OF curtp WITH HEADER LINE.
  DATA : lv_poper_beg LIKE ckmlcr-poper,
         lv_poper_end LIKE ckmlcr-poper.
  DATA : lv_matnr LIKE mara-matnr.
  DATA : ls_keph_mlcd TYPE LINE OF ccs01_t_keph_mlcd.
  DATA : lv_count TYPE i.
  DATA : ls_out TYPE ty_out.

  REFRESH : gt_out,gt_keph_mlcd.
  REFRESH : lt_kalnr,lr_keart,lr_mlcct,lr_kkzst,lr_curtp,
            lt_tckh3.
  IF p_keart IS INITIAL.
    MESSAGE ID 'ZCO' TYPE 'I' NUMBER '001'
       WITH 'Masraf öğesi türünü boş geçmeyiniz'
    DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  IF p_keart = 'H'.
    p_elehk = ls_tck07-elehk.
  ELSE.
    p_elehk = ls_tck07-elehkns.

  ENDIF.

  CLEAR : lv_poper_beg,lv_poper_end.

  lv_poper_beg = p_poper.
  lv_poper_end = p_poper.
  SELECT d~kalnr d~bwkey d~matnr d~bwtar d~kzbws d~xobew
         d~sobkz d~vbeln d~posnr d~pspnr d~lifnr
       INTO CORRESPONDING FIELDS OF TABLE lt_kalnr
           FROM ckmlhd AS d
          INNER JOIN mara AS m
             ON d~matnr EQ m~matnr
          INNER JOIN mbew AS b
             ON d~matnr EQ b~matnr AND
                d~bwkey EQ b~bwkey AND
                d~bwtar EQ b~bwtar
          WHERE d~matnr IN s_matnr
            AND d~bwkey IN s_bwkey
            AND d~mlast EQ '3'
            AND m~matnr IN s_matnr
            AND m~mtart IN s_mtart
            AND m~matkl IN s_matkl
            AND b~bklas IN s_bklas.
  CHECK lt_kalnr[] IS NOT INITIAL.
  DATA: BEGIN OF lt_ckmlpp OCCURS 0,
          kalnr  TYPE ckmlpp-kalnr,
          bdatj  TYPE ckmlpp-bdatj,
          poper  TYPE ckmlpp-poper,
          untper TYPE ckmlpp-untper.
  DATA: END OF lt_ckmlpp.
  RANGES: lr_kalnr FOR ckmlpp-kalnr.
  SELECT
    kalnr
    bdatj
    poper
    untper
  INTO TABLE lt_ckmlpp
  FROM ckmlpp AS a  FOR ALL ENTRIES IN lt_kalnr
  WHERE
    kalnr = lt_kalnr-kalnr AND
    bdatj = p_bdatj AND
    poper = p_poper.
  LOOP AT lt_ckmlpp .
    lr_kalnr-sign = 'I'.
    lr_kalnr-option = 'EQ'.
    lr_kalnr-low = lt_ckmlpp-kalnr.
    COLLECT lr_kalnr.
  ENDLOOP.
  DELETE lt_kalnr
  WHERE
  kalnr NOT IN lr_kalnr.
  SORT lt_kalnr.
  DELETE ADJACENT DUPLICATES FROM lt_kalnr COMPARING ALL FIELDS.

  lr_curtp-sign = 'I'.
  lr_curtp-option = 'EQ'.
  lr_curtp-low = p_curtp.
  APPEND lr_curtp.

  SORT lt_kalnr BY kalnr.
  zlt_kalnr[] = lt_kalnr[].

  REFRESH lt_kalnr.
  zcount = 1.

  lv_count = lines( zlt_kalnr ).
  tcount = lv_count.

  CLEAR lr_keart.
  lr_keart-sign = 'I'.
  lr_keart-option = 'EQ'.
  lr_keart-low = p_keart.
  COLLECT lr_keart.

  CLEAR lr_mlcct.
  lr_mlcct-sign = 'I'.
  lr_mlcct-option = 'EQ'.
  lr_mlcct-low = p_value.
  COLLECT lr_mlcct.

  IF p_kkzst EQ 'S'.
    CLEAR lr_kkzst.
    lr_kkzst-sign = 'I'.
    lr_kkzst-option = 'EQ'.
    lr_kkzst-low = ' '.
    COLLECT lr_kkzst.

    CLEAR lr_kkzst.
    lr_kkzst-sign = 'I'.
    lr_kkzst-option = 'EQ'.
    lr_kkzst-low = 'X'.
    COLLECT lr_kkzst.
  ELSE.
    CLEAR lr_kkzst.
    lr_kkzst-sign = 'I'.
    lr_kkzst-option = 'EQ'.
    lr_kkzst-low = p_kkzst.
    COLLECT lr_kkzst.
  ENDIF.
  LOOP AT zlt_kalnr INTO ls_zlt_kalnr.
    APPEND ls_zlt_kalnr TO lt_kalnr.
    count = count + 1.
    xcount = xcount + 1.
    IF count = 100 OR xcount = tcount.
      ycount = zcount.
      CONCATENATE ycount '00/' tcount  'malzeme işlendi' INTO ytext.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = ytext.

      CLEAR zgt_keph_mlcd.
      REFRESH zgt_keph_mlcd.

      CALL FUNCTION 'MLCCS_KEPH_MLCD_READ'
        EXPORTING
          i_refresh_buffer = 'X'
          it_kalnr         = lt_kalnr[]
          i_from_bdatj     = p_bdatj
          i_from_poper     = lv_poper_beg
          i_to_bdatj       = p_bdatj
          i_to_poper       = lv_poper_end
          ir_keart         = lr_keart[]
          ir_mlcct         = lr_mlcct[]
          ir_kkzst         = lr_kkzst[]
          ir_curtp         = lr_curtp[]
        IMPORTING
          et_keph_mlcd     = zgt_keph_mlcd[].
      LOOP AT zgt_keph_mlcd.
        COLLECT zgt_keph_mlcd INTO gt_keph_mlcd.
        IF zgt_keph_mlcd-bvalt IS NOT INITIAL.
          CLEAR lt_alproc_texts.
          lt_alproc_texts-kalnr = zgt_keph_mlcd-bvalt.
          COLLECT lt_alproc_texts.
        ENDIF.
      ENDLOOP.
      CLEAR count.
      zcount = zcount + 1.
      REFRESH lt_kalnr.
    ENDIF.
  ENDLOOP.

  CHECK gt_keph_mlcd[] IS NOT INITIAL.

  SELECT h~el_hv t~txele INTO TABLE lt_tckh3
        FROM tckh3 AS h
       INNER JOIN tckh1 AS t
          ON h~elehk EQ t~elehk
         AND h~elemt EQ t~elemt
       WHERE h~elehk EQ p_elehk
          AND t~spras EQ sy-langu.
  IF lt_alproc_texts[] IS NOT INITIAL.
    CALL FUNCTION 'CKML_MGV_PROCALTN_TEXT_READ'
      EXPORTING
        i_language      = sy-langu
      CHANGING
        ct_alproc_texts = lt_alproc_texts[]
      EXCEPTIONS
        kalnr_not_found = 1
        OTHERS          = 2.
  ENDIF.
  LOOP AT gt_keph_mlcd INTO ls_keph_mlcd.
    IF ( ls_keph_mlcd-categ EQ 'VN' OR ls_keph_mlcd-categ EQ 'ZU' ) AND
        ls_keph_mlcd-ptyp IS INITIAL.
      CONTINUE.
    ENDIF.
    CLEAR : ls_out,lt_kalnr.

    IF ls_keph_mlcd-categ EQ 'AB'.
      CHECK ls_keph_mlcd-objtyp EQ 'CA'.
      CHECK ls_keph_mlcd-poper EQ lv_poper_beg.
    ENDIF.
    IF ls_keph_mlcd-categ EQ 'EB'.
      CHECK ls_keph_mlcd-objtyp EQ 'CA'.
      CHECK ls_keph_mlcd-poper EQ lv_poper_end.
    ENDIF.

    IF p_kkzst EQ 'S' AND ls_keph_mlcd-kkzst EQ 'X'.
      ls_keph_mlcd-kst001 = - ls_keph_mlcd-kst001.
      ls_keph_mlcd-kst002 = - ls_keph_mlcd-kst002.
      ls_keph_mlcd-kst003 = - ls_keph_mlcd-kst003.
      ls_keph_mlcd-kst004 = - ls_keph_mlcd-kst004.
      ls_keph_mlcd-kst005 = - ls_keph_mlcd-kst005.
      ls_keph_mlcd-kst006 = - ls_keph_mlcd-kst006.
      ls_keph_mlcd-kst007 = - ls_keph_mlcd-kst007.
      ls_keph_mlcd-kst008 = - ls_keph_mlcd-kst008.
      ls_keph_mlcd-kst009 = - ls_keph_mlcd-kst009.
      ls_keph_mlcd-kst010 = - ls_keph_mlcd-kst010.
      ls_keph_mlcd-kst011 = - ls_keph_mlcd-kst011.
      ls_keph_mlcd-kst012 = - ls_keph_mlcd-kst012.
      ls_keph_mlcd-kst013 = - ls_keph_mlcd-kst013.
      ls_keph_mlcd-kst014 = - ls_keph_mlcd-kst014.
      ls_keph_mlcd-kst015 = - ls_keph_mlcd-kst015.
      ls_keph_mlcd-kst016 = - ls_keph_mlcd-kst016.
      ls_keph_mlcd-kst017 = - ls_keph_mlcd-kst017.
      ls_keph_mlcd-kst018 = - ls_keph_mlcd-kst018.
      ls_keph_mlcd-kst019 = - ls_keph_mlcd-kst019.
      ls_keph_mlcd-kst020 = - ls_keph_mlcd-kst020.
      ls_keph_mlcd-kst021 = - ls_keph_mlcd-kst021.
      ls_keph_mlcd-kst022 = - ls_keph_mlcd-kst022.
      ls_keph_mlcd-kst023 = - ls_keph_mlcd-kst023.
      ls_keph_mlcd-kst024 = - ls_keph_mlcd-kst024.
      ls_keph_mlcd-kst025 = - ls_keph_mlcd-kst025.
      ls_keph_mlcd-kst026 = - ls_keph_mlcd-kst026.
      ls_keph_mlcd-kst027 = - ls_keph_mlcd-kst027.
      ls_keph_mlcd-kst028 = - ls_keph_mlcd-kst028.
      ls_keph_mlcd-kst029 = - ls_keph_mlcd-kst029.
      ls_keph_mlcd-kst030 = - ls_keph_mlcd-kst030.
      ls_keph_mlcd-kst031 = - ls_keph_mlcd-kst031.
      ls_keph_mlcd-kst032 = - ls_keph_mlcd-kst032.
      ls_keph_mlcd-kst033 = - ls_keph_mlcd-kst033.
      ls_keph_mlcd-kst034 = - ls_keph_mlcd-kst034.
      ls_keph_mlcd-kst035 = - ls_keph_mlcd-kst035.
      ls_keph_mlcd-kst036 = - ls_keph_mlcd-kst036.
      ls_keph_mlcd-kst037 = - ls_keph_mlcd-kst037.
      ls_keph_mlcd-kst038 = - ls_keph_mlcd-kst038.
      ls_keph_mlcd-kst039 = - ls_keph_mlcd-kst039.
      ls_keph_mlcd-kst040 = - ls_keph_mlcd-kst040.
      ls_keph_mlcd-menge  = - ls_keph_mlcd-menge.
    ENDIF.
    IF ls_keph_mlcd-categ EQ 'ZU' OR ls_keph_mlcd-categ EQ 'AB'.
      ls_keph_mlcd-kst001 = - ls_keph_mlcd-kst001.
      ls_keph_mlcd-kst002 = - ls_keph_mlcd-kst002.
      ls_keph_mlcd-kst003 = - ls_keph_mlcd-kst003.
      ls_keph_mlcd-kst004 = - ls_keph_mlcd-kst004.
      ls_keph_mlcd-kst005 = - ls_keph_mlcd-kst005.
      ls_keph_mlcd-kst006 = - ls_keph_mlcd-kst006.
      ls_keph_mlcd-kst007 = - ls_keph_mlcd-kst007.
      ls_keph_mlcd-kst008 = - ls_keph_mlcd-kst008.
      ls_keph_mlcd-kst009 = - ls_keph_mlcd-kst009.
      ls_keph_mlcd-kst010 = - ls_keph_mlcd-kst010.
      ls_keph_mlcd-kst011 = - ls_keph_mlcd-kst011.
      ls_keph_mlcd-kst012 = - ls_keph_mlcd-kst012.
      ls_keph_mlcd-kst013 = - ls_keph_mlcd-kst013.
      ls_keph_mlcd-kst014 = - ls_keph_mlcd-kst014.
      ls_keph_mlcd-kst015 = - ls_keph_mlcd-kst015.
      ls_keph_mlcd-kst016 = - ls_keph_mlcd-kst016.
      ls_keph_mlcd-kst017 = - ls_keph_mlcd-kst017.
      ls_keph_mlcd-kst018 = - ls_keph_mlcd-kst018.
      ls_keph_mlcd-kst019 = - ls_keph_mlcd-kst019.
      ls_keph_mlcd-kst020 = - ls_keph_mlcd-kst020.
      ls_keph_mlcd-kst021 = - ls_keph_mlcd-kst021.
      ls_keph_mlcd-kst022 = - ls_keph_mlcd-kst022.
      ls_keph_mlcd-kst023 = - ls_keph_mlcd-kst023.
      ls_keph_mlcd-kst024 = - ls_keph_mlcd-kst024.
      ls_keph_mlcd-kst025 = - ls_keph_mlcd-kst025.
      ls_keph_mlcd-kst026 = - ls_keph_mlcd-kst026.
      ls_keph_mlcd-kst027 = - ls_keph_mlcd-kst027.
      ls_keph_mlcd-kst028 = - ls_keph_mlcd-kst028.
      ls_keph_mlcd-kst029 = - ls_keph_mlcd-kst029.
      ls_keph_mlcd-kst030 = - ls_keph_mlcd-kst030.
      ls_keph_mlcd-kst031 = - ls_keph_mlcd-kst031.
      ls_keph_mlcd-kst032 = - ls_keph_mlcd-kst032.
      ls_keph_mlcd-kst033 = - ls_keph_mlcd-kst033.
      ls_keph_mlcd-kst034 = - ls_keph_mlcd-kst034.
      ls_keph_mlcd-kst035 = - ls_keph_mlcd-kst035.
      ls_keph_mlcd-kst036 = - ls_keph_mlcd-kst036.
      ls_keph_mlcd-kst037 = - ls_keph_mlcd-kst037.
      ls_keph_mlcd-kst038 = - ls_keph_mlcd-kst038.
      ls_keph_mlcd-kst039 = - ls_keph_mlcd-kst039.
      ls_keph_mlcd-kst040 = - ls_keph_mlcd-kst040.
      ls_keph_mlcd-menge  = - ls_keph_mlcd-menge.
    ENDIF.
    MOVE-CORRESPONDING ls_keph_mlcd TO ls_out.
    ls_out-lbkum = ls_keph_mlcd-menge.

    CLEAR ls_zlt_kalnr.
    READ TABLE zlt_kalnr INTO ls_zlt_kalnr WITH TABLE KEY
    kalnr = ls_keph_mlcd-kalnr.
    ls_out-matnr = ls_zlt_kalnr-matnr.
    ls_out-bwkey = ls_zlt_kalnr-bwkey.
    ls_out-bwtar = ls_zlt_kalnr-bwtar.

    IF ls_out-bvalt IS NOT INITIAL.
      CLEAR lt_alproc_texts.
      READ TABLE lt_alproc_texts WITH KEY kalnr = ls_out-bvalt.
      IF ls_out-categ EQ 'VN' AND ls_out-ptyp EQ 'VF'.
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = lt_alproc_texts-header_data-matnr
          IMPORTING
            output = lv_matnr.
        CONCATENATE lv_matnr
                    lt_alproc_texts-header_data-werks
               INTO ls_out-gener_name
               SEPARATED BY space.
      ELSE.
        ls_out-gener_name = lt_alproc_texts-gener_name.
      ENDIF.

    ENDIF.

    COLLECT ls_out INTO gt_out.
  ENDLOOP.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_DATA
*&---------------------------------------------------------------------*
FORM display_data .
  CHECK gt_out[] IS NOT INITIAL.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_bypassing_buffer = 'X'
      i_callback_program = sy-repid
      is_layout_lvc      = gs_layout
      it_fieldcat_lvc    = gt_fieldcat[]
      i_default          = 'X'
      i_save             = 'A'
      it_sort_lvc        = gt_sort
      is_variant         = gs_variant
    TABLES
      t_outtab           = gt_out
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_FIELD_CAT
*&---------------------------------------------------------------------*
FORM fill_field_cat CHANGING pt_fieldcat TYPE lvc_t_fcat .
  REFRESH pt_fieldcat.
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZCO_CCS_CKM3N'
      i_bypassing_buffer     = 'X'
    CHANGING
      ct_fieldcat            = pt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " FILL_FIELD_CAT
*&---------------------------------------------------------------------*
*&      Form  FILL_SORT
*&---------------------------------------------------------------------*
FORM fill_sort .
  REFRESH gt_sort.
  CLEAR gs_sort.
  gs_sort-spos      = 1.
  gs_sort-fieldname = 'MATNR'.
  APPEND gs_sort TO gt_sort.


  CLEAR gs_sort.
  gs_sort-spos      = 2.
  gs_sort-fieldname = 'MAKTX'.
  APPEND gs_sort TO gt_sort.
ENDFORM.                    " FILL_SORT
*&---------------------------------------------------------------------*
*&      Form  READ_OTHER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_other .
  DATA : lt_out TYPE STANDARD TABLE OF ty_out.
  DATA : ls_out TYPE ty_out.
  TYPES : BEGIN OF ty_mbew,
            matnr TYPE mbew-matnr,
            bwkey TYPE mbew-bwkey,
            bwtar TYPE mbew-bwtar,
            bklas TYPE mbew-bklas,
          END OF ty_mbew.
  DATA : lt_mbew TYPE HASHED TABLE OF ty_mbew
                 WITH UNIQUE KEY matnr bwkey bwtar.
  DATA : ls_mbew TYPE ty_mbew.
  TYPES : BEGIN OF ty_mara,
            matnr TYPE mara-matnr,
            matkl TYPE mara-matkl,
          END OF ty_mara.
  DATA : lt_mara TYPE HASHED TABLE OF ty_mara
                 WITH UNIQUE KEY matnr.
  DATA : ls_mara TYPE ty_mara.
  TYPES : BEGIN OF ty_makt,
            matnr TYPE makt-matnr,
            spras TYPE makt-spras,
            maktx TYPE makt-maktx,
          END OF ty_makt.
  DATA : lt_makt TYPE HASHED TABLE OF ty_makt
                 WITH UNIQUE KEY matnr spras.
  DATA : ls_makt TYPE ty_makt.
  DATA : lt_dd07t TYPE SORTED TABLE OF dd07t
                  WITH NON-UNIQUE KEY domvalue_l.
  DATA : ls_dd07t TYPE dd07t.
  DATA : lt_ckmlmv009t TYPE SORTED TABLE OF ckmlmv009t
                       WITH NON-UNIQUE KEY ptyp.
  DATA : ls_ckmlmv009t TYPE ckmlmv009t.
  TYPES : BEGIN OF ty_mkal,
            matnr TYPE mkal-matnr,
            werks TYPE mkal-werks,
            verid TYPE mkal-verid,
            mdv01 TYPE mkal-mdv01,
          END OF ty_mkal.
  DATA : lt_mkal TYPE HASHED TABLE OF ty_mkal
                 WITH UNIQUE KEY matnr werks verid.
  DATA : ls_mkal TYPE ty_mkal.
  TYPES : BEGIN OF ty_ckmlmv001_bf,
            kalnr    TYPE ckmlmv001-kalnr,
            pmatn_nd TYPE ckmlmv001-pmatn_nd,
            plwrk_nd TYPE ckmlmv001-plwrk_nd,
            verid_nd TYPE ckmlmv001-verid_nd,
          END OF ty_ckmlmv001_bf.
  DATA : lt_ckmlmv001_bf_temp TYPE STANDARD TABLE OF ty_ckmlmv001_bf.
  DATA : lt_ckmlmv001_bf TYPE SORTED TABLE OF ty_ckmlmv001_bf
                         WITH NON-UNIQUE KEY kalnr.
  DATA : ls_ckmlmv001_bf TYPE ty_ckmlmv001_bf.
  TYPES : BEGIN OF ty_ckmlmv001_vf,
            proc_kalnr TYPE ckmlmv001-kalnr,
            pmatn_nd   TYPE ckmlmv001-pmatn_nd,
            plwrk_nd   TYPE ckmlmv001-plwrk_nd,
            verid_nd   TYPE ckmlmv001-verid_nd,
          END OF ty_ckmlmv001_vf.
  DATA : lt_ckmlmv001_temp TYPE STANDARD TABLE OF ty_ckmlmv001_vf.
  DATA : lt_ckmlmv001_vf TYPE SORTED TABLE OF ty_ckmlmv001_vf
                         WITH NON-UNIQUE KEY proc_kalnr.
  DATA : ls_ckmlmv001_vf TYPE ty_ckmlmv001_vf.


  FIELD-SYMBOLS : <fs_out> TYPE ty_out.

  CHECK gt_out[] IS NOT INITIAL.

  SELECT * FROM dd07t INTO TABLE lt_dd07t
          WHERE domname EQ 'CKML_CATEG'
            AND ddlanguage EQ sy-langu.
  SELECT * FROM ckmlmv009t INTO TABLE lt_ckmlmv009t
          WHERE spras EQ sy-langu.

  lt_out[] = gt_out[].
  SORT lt_out BY matnr bwkey bwtar.
  DELETE ADJACENT DUPLICATES FROM lt_out
        COMPARING matnr bwkey bwtar.
  SELECT matnr bwkey bwtar bklas
    INTO TABLE lt_mbew
    FROM mbew
     FOR ALL ENTRIES IN lt_out
   WHERE matnr EQ lt_out-matnr
     AND bwkey EQ lt_out-bwkey
     AND bwtar EQ lt_out-bwtar.
  DELETE ADJACENT DUPLICATES FROM lt_out
        COMPARING matnr .
  SELECT matnr matkl
    FROM mara
    INTO TABLE lt_mara
     FOR ALL ENTRIES IN lt_out
   WHERE matnr EQ lt_out-matnr.
  SELECT matnr spras maktx
    FROM makt
    INTO TABLE lt_makt
     FOR ALL ENTRIES IN lt_out
   WHERE matnr EQ lt_out-matnr
     AND spras EQ sy-langu.

  lt_out[] = gt_out[].
  DELETE lt_out WHERE ptyp NE 'BF'.
  IF lt_out[] IS NOT INITIAL.
    SORT lt_out BY bvalt.
    DELETE ADJACENT DUPLICATES FROM lt_out COMPARING bvalt.
    SELECT kalnr pmatn_nd plwrk_nd verid_nd
      INTO TABLE lt_ckmlmv001_bf
      FROM ckmlmv001
       FOR ALL ENTRIES IN lt_out
     WHERE kalnr EQ lt_out-bvalt.
  ENDIF.

  lt_out[] = gt_out[].
  DELETE lt_out WHERE ptyp NE 'VF'.
  IF lt_out[] IS NOT INITIAL.
    SORT lt_out BY bvalt.
    DELETE ADJACENT DUPLICATES FROM lt_out COMPARING bvalt.
    SELECT proc_kalnr pmatn_nd plwrk_nd verid_nd
      INTO TABLE lt_ckmlmv001_vf
      FROM ckmlmv001
       FOR ALL ENTRIES IN lt_out
     WHERE proc_kalnr EQ lt_out-bvalt.
  ENDIF.

  APPEND LINES OF lt_ckmlmv001_bf TO lt_ckmlmv001_temp.
  APPEND LINES OF lt_ckmlmv001_vf TO lt_ckmlmv001_temp.
  IF lt_ckmlmv001_temp[] IS NOT INITIAL.
    SORT lt_ckmlmv001_temp BY pmatn_nd plwrk_nd verid_nd.
    DELETE ADJACENT DUPLICATES FROM lt_ckmlmv001_temp
          COMPARING pmatn_nd plwrk_nd verid_nd.
    SELECT matnr werks verid mdv01
      FROM mkal
      INTO TABLE lt_mkal
       FOR ALL ENTRIES IN lt_ckmlmv001_temp
     WHERE matnr EQ lt_ckmlmv001_temp-pmatn_nd
       AND werks EQ lt_ckmlmv001_temp-plwrk_nd
       AND verid EQ lt_ckmlmv001_temp-verid_nd.
  ENDIF.

  LOOP AT gt_out ASSIGNING <fs_out>.
    IF <fs_out>-categ IS NOT INITIAL.
      CLEAR ls_dd07t.
      READ TABLE lt_dd07t INTO ls_dd07t
            WITH TABLE KEY domvalue_l = <fs_out>-categ.
      <fs_out>-zcateg = ls_dd07t-ddtext.
    ENDIF.
    IF <fs_out>-ptyp IS NOT INITIAL.
      CLEAR ls_ckmlmv009t.
      READ TABLE lt_ckmlmv009t INTO ls_ckmlmv009t
            WITH TABLE KEY ptyp  = <fs_out>-ptyp.
      <fs_out>-zptyp = ls_ckmlmv009t-ktext.
    ENDIF.
    CLEAR ls_makt.
    READ TABLE lt_makt INTO ls_makt
          WITH KEY matnr = <fs_out>-matnr
                   spras = sy-langu.
    <fs_out>-maktx = ls_makt-maktx.

    CLEAR ls_mara.
    READ TABLE lt_mara INTO ls_mara
          WITH KEY matnr = <fs_out>-matnr.

    <fs_out>-matkl = ls_mara-matkl.

    CLEAR ls_mbew.
    READ TABLE lt_mbew INTO ls_mbew
          WITH KEY matnr = <fs_out>-matnr
                   bwkey = <fs_out>-bwkey
                   bwtar = <fs_out>-bwtar.
    <fs_out>-bklas = ls_mbew-bklas.

    PERFORM calc_toplam CHANGING <fs_out>.

    IF <fs_out>-ptyp EQ 'BF' OR <fs_out>-ptyp EQ 'VF' .
      IF <fs_out>-ptyp EQ 'BF'.
        CLEAR : ls_ckmlmv001_bf, ls_mkal.
        READ TABLE lt_ckmlmv001_bf INTO ls_ckmlmv001_bf
              WITH KEY kalnr = <fs_out>-bvalt.
        READ TABLE lt_mkal INTO ls_mkal
              WITH TABLE KEY
                       matnr = ls_ckmlmv001_bf-pmatn_nd
                       werks = ls_ckmlmv001_bf-plwrk_nd
                       verid = ls_ckmlmv001_bf-verid_nd.
      ELSEIF <fs_out>-ptyp EQ 'VF'.
        CLEAR : ls_ckmlmv001_vf, ls_mkal.
        READ TABLE lt_ckmlmv001_vf INTO ls_ckmlmv001_vf
              WITH KEY proc_kalnr = <fs_out>-bvalt.
        READ TABLE lt_mkal INTO ls_mkal
              WITH TABLE KEY
                       matnr = ls_ckmlmv001_vf-pmatn_nd
                       werks = ls_ckmlmv001_vf-plwrk_nd
                       verid = ls_ckmlmv001_vf-verid_nd.

      ENDIF.
*      <fs_out>-mdv01 = ls_mkal-mdv01.
    ENDIF.

  ENDLOOP.

  IF p_kkzst EQ 'S'.
    PERFORM when_s_selected.
  ENDIF.

ENDFORM.                    " READ_OTHER

*&---------------------------------------------------------------------*
*&      Form  SELSCR_OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selscr_output .
  LOOP AT SCREEN.
    IF screen-group1 EQ 'G1'.
      IF ls_tck07-elehkns IS NOT INITIAL.
        screen-invisible = 0.
      ELSE.
        screen-invisible = 1.
        CLEAR p_keart.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
  IF ls_tck07-elehkns IS INITIAL.
    p_keart = 'H'.
  ENDIF.
ENDFORM.                    " SELSCR_OUTPUT

*&---------------------------------------------------------------------*
*&      Form  SELSCR_INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selscr_input .
  CLEAR : ls_tck07.
  CONCATENATE p_bdatj p_poper+1(2) '01' INTO lv_datum.

  IF p_bukrs IS NOT INITIAL AND
     p_bdatj IS NOT INITIAL AND
     p_poper IS NOT INITIAL.
    SELECT * FROM tck07 INTO CORRESPONDING FIELDS OF TABLE lt_tck07
            WHERE valid_from <= lv_datum
            AND bukrs EQ p_bukrs.
    SORT lt_tck07 BY valid_from DESCENDING.
    READ TABLE lt_tck07 INTO ls_tck07 INDEX 1.
  ENDIF.
ENDFORM.                    " SELSCR_INPUT
*&---------------------------------------------------------------------*
*&      Form  WHEN_S_SELECTED
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM when_s_selected .
  DATA : lt_out_temp TYPE STANDARD TABLE OF ty_out.
  DATA : lt_out TYPE STANDARD TABLE OF ty_out.
  DATA : ls_out TYPE ty_out.
  TYPES : BEGIN OF ty_ckmlmv001,
            proc_kalnr TYPE ckmlmv001-proc_kalnr,
            matnr      TYPE ckmlmv001-matnr,
            bwkey      TYPE ckmlmv001-bwkey,
            kalnr      TYPE ckmlmv001-kalnr,
          END OF ty_ckmlmv001.
  DATA : ls_ckmlmv001 TYPE ty_ckmlmv001.
  DATA : lt_ckmlmv001 TYPE SORTED TABLE OF ty_ckmlmv001
                      WITH NON-UNIQUE KEY proc_kalnr.
  FIELD-SYMBOLS : <fs_out> LIKE LINE OF gt_out.

  lt_out[] = gt_out[].

  DELETE lt_out WHERE categ NE 'VN'
                  AND ptyp NE 'VF' .

  IF lt_out IS NOT INITIAL.
    lt_out_temp = lt_out.
    SORT lt_out_temp BY bvalt.
    DELETE ADJACENT DUPLICATES FROM lt_out_temp COMPARING bvalt.
    SELECT proc_kalnr matnr bwkey kalnr
      FROM ckmlmv001
      INTO TABLE lt_ckmlmv001
       FOR ALL ENTRIES IN lt_out_temp
     WHERE proc_kalnr EQ lt_out_temp-bvalt.
  ENDIF.

  LOOP AT lt_out INTO ls_out
    WHERE bklas EQ '9110'
       OR bklas EQ '9120'
       OR bklas EQ '9130' .

    CLEAR ls_ckmlmv001.
    READ TABLE lt_ckmlmv001 INTO ls_ckmlmv001
          WITH TABLE KEY
          proc_kalnr = ls_out-bvalt.
    CHECK sy-subrc EQ 0 .

    LOOP AT gt_out ASSIGNING <fs_out>
      WHERE matnr = ls_ckmlmv001-matnr
        AND bwkey = ls_ckmlmv001-bwkey
        AND bvalt = ls_ckmlmv001-kalnr
        AND categ = 'ZU'
        AND ptyp  = 'BF'.



      <fs_out>-kst001 = <fs_out>-kst001 + ls_out-kst001.
      <fs_out>-kst002 = <fs_out>-kst002 + ls_out-kst002.
      <fs_out>-kst003 = <fs_out>-kst003 + ls_out-kst003.
      <fs_out>-kst004 = <fs_out>-kst004 + ls_out-kst004.
      <fs_out>-kst005 = <fs_out>-kst005 + ls_out-kst005.
      <fs_out>-kst006 = <fs_out>-kst006 + ls_out-kst006.
      <fs_out>-kst007 = <fs_out>-kst007 + ls_out-kst007.
      <fs_out>-kst008 = <fs_out>-kst008 + ls_out-kst008.
      <fs_out>-kst009 = <fs_out>-kst009 + ls_out-kst009.
      <fs_out>-kst010 = <fs_out>-kst010 + ls_out-kst010.
      <fs_out>-kst011 = <fs_out>-kst011 + ls_out-kst011.
      <fs_out>-kst012 = <fs_out>-kst012 + ls_out-kst012.
      <fs_out>-kst013 = <fs_out>-kst013 + ls_out-kst013.
      <fs_out>-kst014 = <fs_out>-kst014 + ls_out-kst014.
      <fs_out>-kst015 = <fs_out>-kst015 + ls_out-kst015.
      <fs_out>-kst016 = <fs_out>-kst016 + ls_out-kst016.
      <fs_out>-kst017 = <fs_out>-kst017 + ls_out-kst017.
      <fs_out>-kst018 = <fs_out>-kst018 + ls_out-kst018.
      <fs_out>-kst019 = <fs_out>-kst019 + ls_out-kst019.
      <fs_out>-kst020 = <fs_out>-kst020 + ls_out-kst020.
      <fs_out>-kst021 = <fs_out>-kst021 + ls_out-kst021.
      <fs_out>-kst022 = <fs_out>-kst022 + ls_out-kst022.
      <fs_out>-kst023 = <fs_out>-kst023 + ls_out-kst023.
      <fs_out>-kst024 = <fs_out>-kst024 + ls_out-kst024.
      <fs_out>-kst025 = <fs_out>-kst025 + ls_out-kst025.
      <fs_out>-kst026 = <fs_out>-kst026 + ls_out-kst026.
      <fs_out>-kst027 = <fs_out>-kst027 + ls_out-kst027.
      <fs_out>-kst028 = <fs_out>-kst028 + ls_out-kst028.
      PERFORM calc_toplam CHANGING <fs_out>.

    ENDLOOP.

  ENDLOOP.


ENDFORM.                    " WHEN_S_SELECTED
*&---------------------------------------------------------------------*
*&      Form  FILL_ALV_DATA
*&---------------------------------------------------------------------*
FORM fill_alv_data .

  CLEAR gs_layout.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-zebra      = 'X'.

  gs_variant-report      = sy-repid.
  gs_variant-username    = sy-uname.

  PERFORM fill_field_cat  CHANGING gt_fieldcat[].
  PERFORM fill_sort.
  LOOP AT gt_fieldcat INTO gs_fieldcat.
    IF gs_fieldcat-fieldname(3) EQ 'KST'.
      CLEAR lt_tckh3.
      READ TABLE lt_tckh3 WITH KEY el_hv = gs_fieldcat-fieldname+3(3).
      IF sy-subrc IS NOT INITIAL.
        DELETE gt_fieldcat.
        CONTINUE.
      ELSE.
        gs_fieldcat-coltext   = lt_tckh3-txele.
        gs_fieldcat-scrtext_l = lt_tckh3-txele.
        gs_fieldcat-scrtext_m = lt_tckh3-txele.
        gs_fieldcat-scrtext_s = lt_tckh3-txele.
        MODIFY gt_fieldcat FROM gs_fieldcat.
      ENDIF.
    ENDIF.
    IF gs_fieldcat-fieldname EQ 'TOPLAM'.
      gs_fieldcat-coltext   = 'Toplam'.
      gs_fieldcat-scrtext_l = 'Toplam'.
      gs_fieldcat-scrtext_m = 'Toplam'.
      gs_fieldcat-scrtext_s = 'Toplam'.
      MODIFY gt_fieldcat FROM gs_fieldcat.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " FILL_ALV_DATA
*&---------------------------------------------------------------------*
*&      Form  CALC_TOPLAM
*&---------------------------------------------------------------------*
FORM calc_toplam  CHANGING cs_out TYPE ty_out.
  FIELD-SYMBOLS : <fs>.
  CLEAR cs_out-toplam.
  LOOP AT gt_fieldcat INTO gs_fieldcat.
    IF gs_fieldcat-fieldname(3) EQ 'KST'.
      ASSIGN COMPONENT gs_fieldcat-fieldname
          OF STRUCTURE cs_out TO <fs>.
      CHECK <fs> IS ASSIGNED.
      ADD <fs> TO cs_out-toplam.
      UNASSIGN <fs>.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CALC_TOPLAM
