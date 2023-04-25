*&---------------------------------------------------------------------*
*& Report YMONO_TBX_SIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymono_tbx_sil.
Try.
  Call Function 'Ztest'
  Exceptions
     Others = 9.
  If Sy-subrc = 9.
    Break-point.
  Endif.
  Catch Cx_sy_dyn_call_illegal_func.
    Break-point.
Endtry.
