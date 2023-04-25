*&---------------------------------------------------------------------*
*& Report YMONO_TBX_101
*&---------------------------------------------------------------------*
*& Company          : Monotis Digital Innovation
*& Author           : Aziz AKAL
*& E-Mail           : aziz.akal@monotis.com
*& Creation Date    : 01.02.2023
*& Title            : Fonksiyon kullanırken exception yönetimi
*&---------------------------------------------------------------------*
*& Description      : Ürün için kullanılan fonksiyon sistemde yoksa bile
*&                    short-dump a düşmemeli aşağıdaki şekilde
*&                    error handling yapılmalıdır.
*&---------------------------------------------------------------------*
*& CHANGE HISTORY:
*& Version Date.      Author       Description
*&
*&---------------------------------------------------------------------*
REPORT ymono_tbx_101.
TRY.
    CALL FUNCTION 'ZTEST'
      EXCEPTIONS
        OTHERS = 9.
    IF Sy-subrc = 9.
* Fonksiyon var fakat exceptions alıyoruz
      BREAK-POINT.
    ENDIF.

  CATCH Cx_sy_dyn_call_illegal_func.
* Fonksiyon yok !!!
    BREAK-POINT.
ENDTRY.
