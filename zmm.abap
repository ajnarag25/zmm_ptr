*&---------------------------------------------------------------------*
*& Report ZMM_PTR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMM_PTR.

TABLES: EKKO, EKPO, T001W, LFA1, ZCT_RSB_UOM.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: agg RADIOBUTTON GROUP rad,
              cem RADIOBUTTON GROUP rad,
              rsb RADIOBUTTON GROUP rad.
  SELECT-OPTIONS: a_period FOR ekpo-prdat OBLIGATORY.
SELECTION-SCREEN END OF BLOCK blk1.

TYPES: BEGIN OF trnd_report,
        matnum TYPE EKPO-MATNR,
        matname TYPE EKPO-TXZ01,
        plantcode TYPE EKPO-WERKS,
        prdat TYPE EKPO-PRDAT,
        poqty TYPE EKPO-MENGE,
        poamnt TYPE EKPO-NETWR,
        plantname TYPE T001W-NAME1,

        cdate TYPE EKPO-AEDAT,
        vendor TYPE EKKO-LIFNR,
        vname1 TYPE LFA1-NAME1,
        vname2 TYPE LFA1-NAME2,
        matgrp TYPE EKPO-MATKL,
        purord TYPE EKKO-EBELN,
        litem TYPE EKPO-EBELP,
        matdesc TYPE EKPO-TXZ01,
        uom TYPE EKPO-MEINS,
        netpr TYPE EKPO-NETPR,

        poave TYPE DECIMALS,

        totqty TYPE EKPO-MENGE,
        totamnt TYPE EKPO-MENGE,
        totave TYPE EKPO-MENGE,

        poqty_jan TYPE EKPO-MENGE,
        poqty_feb TYPE EKPO-MENGE,
        poqty_mar TYPE EKPO-MENGE,
        poqty_apr TYPE EKPO-MENGE,
        poqty_may TYPE EKPO-MENGE,
        poqty_jun TYPE EKPO-MENGE,
        poqty_jul TYPE EKPO-MENGE,
        poqty_aug TYPE EKPO-MENGE,
        poqty_sep TYPE EKPO-MENGE,
        poqty_oct TYPE EKPO-MENGE,
        poqty_nov TYPE EKPO-MENGE,
        poqty_dec TYPE EKPO-MENGE,

        poamnt_jan TYPE EKPO-MENGE,
        poamnt_feb TYPE EKPO-MENGE,
        poamnt_mar TYPE EKPO-MENGE,
        poamnt_apr TYPE EKPO-MENGE,
        poamnt_may TYPE EKPO-MENGE,
        poamnt_jun TYPE EKPO-MENGE,
        poamnt_jul TYPE EKPO-MENGE,
        poamnt_aug TYPE EKPO-MENGE,
        poamnt_sep TYPE EKPO-MENGE,
        poamnt_oct TYPE EKPO-MENGE,
        poamnt_nov TYPE EKPO-MENGE,
        poamnt_dec TYPE EKPO-MENGE,

        poave_jan TYPE EKPO-MENGE,
        poave_feb TYPE EKPO-MENGE,
        poave_mar TYPE EKPO-MENGE,
        poave_apr TYPE EKPO-MENGE,
        poave_may TYPE EKPO-MENGE,
        poave_jun TYPE EKPO-MENGE,
        poave_jul TYPE EKPO-MENGE,
        poave_aug TYPE EKPO-MENGE,
        poave_sep TYPE EKPO-MENGE,
        poave_oct TYPE EKPO-MENGE,
        poave_nov TYPE EKPO-MENGE,
        poave_dec TYPE EKPO-MENGE,

        poqty_field TYPE string,
        current_month TYPE i,

      END OF trnd_report.

TYPES: BEGIN OF trnd_report_cem,
        vendid TYPE LFA1-LIFNR,
        venname1 TYPE LFA1-NAME1,
        venname2 TYPE LFA1-NAME2,
        uom TYPE EKPO-MEINS,
        prdat TYPE EKPO-PRDAT,
        poqty TYPE EKPO-MENGE,
        poamnt TYPE EKPO-NETWR,

        cdate TYPE EKPO-AEDAT,
        matgrp TYPE EKPO-MATKL,
        vendor TYPE EKKO-LIFNR,
        purord TYPE EKKO-EBELN,
        litem TYPE EKPO-EBELP,
        matnum TYPE EKPO-MATNR,
        matdesc TYPE EKPO-TXZ01,
        plant TYPE EKPO-WERKS,
        plantname TYPE V_T001W-NAME1,
        netpr TYPE EKPO-NETPR,

        totqty TYPE EKPO-MENGE,
        totamnt TYPE EKPO-MENGE,
        totave TYPE EKPO-MENGE,

        poqty_jan TYPE EKPO-MENGE,
        poqty_feb TYPE EKPO-MENGE,
        poqty_mar TYPE EKPO-MENGE,
        poqty_apr TYPE EKPO-MENGE,
        poqty_may TYPE EKPO-MENGE,
        poqty_jun TYPE EKPO-MENGE,
        poqty_jul TYPE EKPO-MENGE,
        poqty_aug TYPE EKPO-MENGE,
        poqty_sep TYPE EKPO-MENGE,
        poqty_oct TYPE EKPO-MENGE,
        poqty_nov TYPE EKPO-MENGE,
        poqty_dec TYPE EKPO-MENGE,

        poamnt_jan TYPE EKPO-MENGE,
        poamnt_feb TYPE EKPO-MENGE,
        poamnt_mar TYPE EKPO-MENGE,
        poamnt_apr TYPE EKPO-MENGE,
        poamnt_may TYPE EKPO-MENGE,
        poamnt_jun TYPE EKPO-MENGE,
        poamnt_jul TYPE EKPO-MENGE,
        poamnt_aug TYPE EKPO-MENGE,
        poamnt_sep TYPE EKPO-MENGE,
        poamnt_oct TYPE EKPO-MENGE,
        poamnt_nov TYPE EKPO-MENGE,
        poamnt_dec TYPE EKPO-MENGE,

        poave_jan TYPE EKPO-MENGE,
        poave_feb TYPE EKPO-MENGE,
        poave_mar TYPE EKPO-MENGE,
        poave_apr TYPE EKPO-MENGE,
        poave_may TYPE EKPO-MENGE,
        poave_jun TYPE EKPO-MENGE,
        poave_jul TYPE EKPO-MENGE,
        poave_aug TYPE EKPO-MENGE,
        poave_sep TYPE EKPO-MENGE,
        poave_oct TYPE EKPO-MENGE,
        poave_nov TYPE EKPO-MENGE,
        poave_dec TYPE EKPO-MENGE,

   END OF trnd_report_cem.

TYPES: BEGIN OF trnd_report_rsb,
        grade TYPE ZCT_RSB_UOM-GRADE,
        prdat TYPE EKPO-PRDAT,
        poqty TYPE EKPO-MENGE,
        poamnt TYPE EKPO-NETWR,

*        matnum TYPE ZCT_RSB_UOM-MATNR,
*        matdesc TYPE ZCT_RSB_UOM-MARTX,
        dia TYPE ZCT_RSB_UOM-DIA,
        len TYPE ZCT_RSB_UOM-LENGTH,
        mass TYPE ZCT_RSB_UOM-MASS,
        kgs TYPE  ZCT_RSB_UOM-KGSPC,
        meins TYPE  ZCT_RSB_UOM-MEINS,

        cdate TYPE EKPO-AEDAT,
        matgrp TYPE EKPO-MATKL,
        purord TYPE EKKO-EBELN,
        litem TYPE EKPO-EBELP,
        matnum TYPE EKPO-MATNR,
        matdesc TYPE EKPO-TXZ01,
        plantname TYPE V_T001W-NAME1,
        uom TYPE EKPO-MEINS,
        netpr TYPE EKPO-NETPR,

        totqty TYPE EKPO-MENGE,
        totamnt TYPE EKPO-MENGE,
        totave TYPE EKPO-MENGE,

        totkgs TYPE EKPO-MENGE,
        totavekgs TYPE EKPO-MENGE,

        poqty_jan TYPE EKPO-MENGE,
        poqty_feb TYPE EKPO-MENGE,
        poqty_mar TYPE EKPO-MENGE,
        poqty_apr TYPE EKPO-MENGE,
        poqty_may TYPE EKPO-MENGE,
        poqty_jun TYPE EKPO-MENGE,
        poqty_jul TYPE EKPO-MENGE,
        poqty_aug TYPE EKPO-MENGE,
        poqty_sep TYPE EKPO-MENGE,
        poqty_oct TYPE EKPO-MENGE,
        poqty_nov TYPE EKPO-MENGE,
        poqty_dec TYPE EKPO-MENGE,

        poamnt_jan TYPE EKPO-MENGE,
        poamnt_feb TYPE EKPO-MENGE,
        poamnt_mar TYPE EKPO-MENGE,
        poamnt_apr TYPE EKPO-MENGE,
        poamnt_may TYPE EKPO-MENGE,
        poamnt_jun TYPE EKPO-MENGE,
        poamnt_jul TYPE EKPO-MENGE,
        poamnt_aug TYPE EKPO-MENGE,
        poamnt_sep TYPE EKPO-MENGE,
        poamnt_oct TYPE EKPO-MENGE,
        poamnt_nov TYPE EKPO-MENGE,
        poamnt_dec TYPE EKPO-MENGE,

        poave_jan TYPE EKPO-MENGE,
        poave_feb TYPE EKPO-MENGE,
        poave_mar TYPE EKPO-MENGE,
        poave_apr TYPE EKPO-MENGE,
        poave_may TYPE EKPO-MENGE,
        poave_jun TYPE EKPO-MENGE,
        poave_jul TYPE EKPO-MENGE,
        poave_aug TYPE EKPO-MENGE,
        poave_sep TYPE EKPO-MENGE,
        poave_oct TYPE EKPO-MENGE,
        poave_nov TYPE EKPO-MENGE,
        poave_dec TYPE EKPO-MENGE,

   END OF trnd_report_rsb.

DATA: lt_trnd TYPE TABLE OF trnd_report,
      ls_trnd TYPE trnd_report,
      output_trnd TYPE TABLE OF trnd_report,
      ls_output TYPE trnd_report,
      lt_cem_trnd TYPE TABLE OF trnd_report_cem,
      ls_cem_trnd TYPE trnd_report_cem,
      lt_cem_output TYPE TABLE OF trnd_report_cem,
      ls_cem_output TYPE trnd_report_cem,

      lt_rsb_trnd TYPE TABLE OF trnd_report_rsb,
      ls_rsb_trnd TYPE trnd_report_rsb,
      lt_rsb_output TYPE TABLE OF trnd_report_rsb,
      ls_rsb_output TYPE trnd_report_rsb,

      lt_hotspot_aggre TYPE TABLE OF trnd_report,
      ls_hotspot_aggre TYPE trnd_report,

      lt_hotspot_cem TYPE TABLE OF trnd_report_cem,
      ls_hotspot_cem TYPE trnd_report_cem,

      lt_hotspot_rsb TYPE TABLE OF trnd_report_rsb,
      ls_hotspot_rsb TYPE trnd_report_rsb,

      totalkgs TYPE EKPO-MENGE,
      totalavekgs TYPE EKPO-MENGE,

      it_fieldcat TYPE TABLE OF slis_fieldcat_alv,
      it_fieldcat2 TYPE TABLE OF slis_fieldcat_alv,
      it_fieldcat3 TYPE TABLE OF slis_fieldcat_alv,
      it_fieldcat4 TYPE TABLE OF slis_fieldcat_alv,

      wa_fieldcat TYPE slis_fieldcat_alv,
      current_month TYPE sy-datum,
      current_year TYPE string,

      poqty_total TYPE EKPO-MENGE,
      poqty_field TYPE string,

      poamnt_total TYPE EKPO-MENGE,
      poamnt_field TYPE string,

      poave_total TYPE EKPO-MENGE,
      poave_field TYPE string,

      show_data_jan TYPE abap_bool,
      show_data_feb TYPE abap_bool,
      show_data_mar TYPE abap_bool,
      show_data_apr TYPE abap_bool,
      show_data_may TYPE abap_bool,
      show_data_jun TYPE abap_bool,
      show_data_jul TYPE abap_bool,
      show_data_aug TYPE abap_bool,
      show_data_sep TYPE abap_bool,
      show_data_oct TYPE abap_bool,
      show_data_nov TYPE abap_bool,
      show_data_dec TYPE abap_bool.

" ======================================================== "

START-OF-SELECTION.

IF AGG EQ 'X'.
  PERFORM get_data_aggre.
  PERFORM fill_fieldcat_aggre.
  PERFORM call_alv_grid_display_aggre.
ELSEIF CEM EQ 'X'.
  PERFORM get_data_cement.
  PERFORM fill_fieldcat_cement.
  PERFORM call_alv_grid_display_cement.
ELSE.
  PERFORM get_data_rsb.
  PERFORM fill_fieldcat_rsb.
  PERFORM call_alv_grid_display_rsb.
ENDIF.

" ======================================================== "
FORM get_data_aggre.
  SELECT e~MATNR
         e~TXZ01
         e~WERKS
         e~PRDAT
         e~MENGE
         e~NETWR
         w~NAME1
         e~AEDAT
         ek~LIFNR
         l~NAME1
         l~NAME2
         e~MATKL
         ek~EBELN
         e~EBELP
         e~TXZ01
         e~MEINS
         e~NETPR
   INTO TABLE lt_trnd
   FROM EKPO AS e
   INNER JOIN T001W AS w ON e~WERKS = w~WERKS
   INNER JOIN EKKO AS ek ON e~EBELN = ek~EBELN
   INNER JOIN LFA1 AS l ON ek~LIFNR = l~LIFNR
   WHERE e~PRDAT IN a_period.

  " ======================================================== "

  SORT lt_trnd BY matnum.
  MOVE lt_trnd TO output_trnd.
  DELETE ADJACENT DUPLICATES FROM output_trnd COMPARING matnum.

  LOOP AT output_trnd INTO ls_output.
    CLEAR ls_output-totqty.

    LOOP AT lt_trnd INTO ls_trnd WHERE matnum = ls_output-matnum.
      current_month = ls_trnd-prdat+4(2).
      current_year = ls_trnd-prdat(4).

      IF current_month = '01'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_jan.
        ADD ls_trnd-poamnt TO ls_output-poamnt_jan.
        ADD poave_total TO ls_output-poave_jan.

        show_data_jan = abap_true.
      ELSEIF current_month = '02'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_feb.
        ADD ls_trnd-poamnt TO ls_output-poamnt_feb.
        ADD poave_total TO ls_output-poave_feb.

        show_data_feb = abap_true.
      ELSEIF current_month = '03'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_mar.
        ADD ls_trnd-poamnt TO ls_output-poamnt_mar.
        ADD poave_total TO ls_output-poave_mar.

        show_data_mar = abap_true.
      ELSEIF current_month = '04'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_apr.
        ADD ls_trnd-poamnt TO ls_output-poamnt_apr.
        ADD poave_total TO ls_output-poave_apr.

        show_data_apr = abap_true.
      ELSEIF current_month = '05'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_may.
        ADD ls_trnd-poamnt TO ls_output-poamnt_may.
        ADD poave_total TO ls_output-poave_may.

        show_data_may = abap_true.
      ELSEIF current_month = '06'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_jun.
        ADD ls_trnd-poamnt TO ls_output-poamnt_jun.
        ADD poave_total TO ls_output-poave_jun.

        show_data_jun = abap_true.
      ELSEIF current_month = '07'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_jul.
        ADD ls_trnd-poamnt TO ls_output-poamnt_jul.
        ADD poave_total TO ls_output-poave_jul.

        show_data_jul = abap_true.
      ELSEIF current_month = '08'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_aug.
        ADD ls_trnd-poamnt TO ls_output-poamnt_aug.
        ADD poave_total TO ls_output-poave_aug.

        show_data_aug = abap_true.
      ELSEIF current_month = '09'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_sep.
        ADD ls_trnd-poamnt TO ls_output-poamnt_sep.
        ADD poave_total TO ls_output-poave_sep.

        show_data_sep = abap_true.
      ELSEIF current_month = '10'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_oct.
        ADD ls_trnd-poamnt TO ls_output-poamnt_oct.
        ADD poave_total TO ls_output-poave_oct.

        show_data_oct = abap_true.
      ELSEIF current_month = '11'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_nov.
        ADD ls_trnd-poamnt TO ls_output-poamnt_nov.
        ADD poave_total TO ls_output-poave_nov.

        show_data_nov = abap_true.
      ELSEIF current_month = '12'.
        poave_total = ls_trnd-poamnt / ls_trnd-poqty.
        ADD ls_trnd-poqty TO ls_output-poqty_dec.
        ADD ls_trnd-poamnt TO ls_output-poamnt_dec.
        ADD poave_total TO ls_output-poave_dec.

        show_data_dec = abap_true.
      ENDIF.

      ADD ls_trnd-poqty TO ls_output-totqty.
      ADD ls_trnd-poamnt TO ls_output-totamnt.
      ADD poave_total TO ls_output-totave.

*      ADD ls_trnd-poqty TO ls_hotspot_aggre-totqty.
*      ADD ls_trnd-poamnt TO ls_hotspot_aggre-totamnt.
*      ADD poave_total TO ls_hotspot_aggre-totave.

      MODIFY output_trnd FROM ls_output.

    ENDLOOP.
  ENDLOOP.

  " ======================================================== "

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM get_data_cement.
  SELECT   l~LIFNR
           l~NAME1
           l~NAME2
           e~MEINS
           e~PRDAT
           e~MENGE
           e~NETWR
           e~AEDAT
           e~MATKL
           ek~LIFNR
           ek~EBELN
           e~EBELP
           e~MATNR
           e~TXZ01
           e~WERKS
           t~NAME1
           e~NETPR
    INTO TABLE lt_cem_trnd
    FROM LFA1 AS l
    INNER JOIN EKKO AS ek ON ek~LIFNR = l~LIFNR
    INNER JOIN EKPO AS e ON e~EBELN = ek~EBELN
    INNER JOIN T001W AS t ON t~WERKS = e~WERKS
   WHERE e~PRDAT IN a_period.

   SORT lt_cem_trnd BY vendid.
   MOVE lt_cem_trnd TO lt_cem_output.
   DELETE ADJACENT DUPLICATES FROM lt_cem_output COMPARING vendid.

   LOOP AT lt_cem_output INTO ls_cem_output.
    CLEAR ls_cem_output-totqty.

    LOOP AT lt_cem_trnd INTO ls_cem_trnd WHERE vendid = ls_cem_output-vendid.
      current_month = ls_cem_trnd-prdat+4(2).
      current_year = ls_cem_trnd-prdat(4).

      IF current_month = '01'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_jan.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_jan.
        ADD poave_total TO ls_cem_output-poave_jan.

        show_data_jan = abap_true.
      ELSEIF current_month = '02'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_feb.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_feb.
        ADD poave_total TO ls_cem_output-poave_feb.

        show_data_feb = abap_true.
      ELSEIF current_month = '03'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_mar.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_mar.
        ADD poave_total TO ls_cem_output-poave_mar.

        show_data_mar = abap_true.
      ELSEIF current_month = '04'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_apr.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_apr.
        ADD poave_total TO ls_cem_output-poave_apr.

        show_data_apr = abap_true.
      ELSEIF current_month = '05'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_may.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_may.
        ADD poave_total TO ls_cem_output-poave_may.

        show_data_may = abap_true.
      ELSEIF current_month = '06'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_jun.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_jun.
        ADD poave_total TO ls_cem_output-poave_jun.

        show_data_jun = abap_true.
      ELSEIF current_month = '07'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_jul.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_jul.
        ADD poave_total TO ls_cem_output-poave_jul.

        show_data_jul = abap_true.
      ELSEIF current_month = '08'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_aug.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_aug.
        ADD poave_total TO ls_cem_output-poave_aug.

        show_data_aug = abap_true.
      ELSEIF current_month = '09'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_sep.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_sep.
        ADD poave_total TO ls_cem_output-poave_sep.

        show_data_sep = abap_true.
      ELSEIF current_month = '10'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_oct.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_oct.
        ADD poave_total TO ls_cem_output-poave_oct.

        show_data_oct = abap_true.
      ELSEIF current_month = '11'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_nov.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_nov.
        ADD poave_total TO ls_cem_output-poave_nov.

        show_data_nov = abap_true.
      ELSEIF current_month = '12'.
        poave_total = ls_cem_trnd-poamnt / ls_cem_trnd-poqty.
        ADD ls_cem_trnd-poqty TO ls_cem_output-poqty_dec.
        ADD ls_cem_trnd-poamnt TO ls_cem_output-poamnt_dec.
        ADD poave_total TO ls_cem_output-poave_dec.

        show_data_dec = abap_true.
      ENDIF.

      ADD ls_cem_trnd-poqty TO ls_cem_output-totqty.
      ADD ls_cem_trnd-poamnt TO ls_cem_output-totamnt.
      ADD poave_total TO ls_cem_output-totave.

*      ADD ls_cem_trnd-poqty TO ls_hotspot_cem-totqty.
*      ADD ls_cem_trnd-poamnt TO ls_hotspot_cem-totamnt.
*      ADD poave_total TO ls_hotspot_cem-totave.

      MODIFY lt_cem_output FROM ls_cem_output.

    ENDLOOP.
  ENDLOOP.

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM get_data_rsb.
  SELECT z~GRADE
         e~PRDAT
         e~MENGE
         e~NETWR
         z~DIA
         z~LENGTH
         z~MASS
         z~KGSPC
         z~MEINS
         e~AEDAT
         e~MATKL
         ek~EBELN
         e~EBELP
         e~MATNR
         e~TXZ01
         t~NAME1
         e~MEINS
         e~NETPR
   INTO TABLE lt_rsb_trnd
   FROM ZCT_RSB_UOM AS z
   INNER JOIN EKPO AS e ON e~MATNR = z~MATNR
   INNER JOIN EKKO AS ek ON ek~EBELN = e~EBELN
   INNER JOIN T001W AS t ON t~WERKS = e~WERKS
   WHERE e~PRDAT IN a_period.

   SORT lt_rsb_trnd BY grade.
   MOVE lt_rsb_trnd TO lt_rsb_output.
   DELETE ADJACENT DUPLICATES FROM lt_rsb_output COMPARING grade.

   LOOP AT lt_rsb_output INTO ls_rsb_output.
    CLEAR ls_rsb_output-totqty.

    LOOP AT lt_rsb_trnd INTO ls_rsb_trnd WHERE grade = ls_rsb_output-grade.
      current_month = ls_rsb_trnd-prdat+4(2).
      current_year = ls_rsb_trnd-prdat(4).

      IF current_month = '01'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_jan.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_jan.
        ADD poave_total TO ls_rsb_output-poave_jan.

        show_data_jan = abap_true.
      ELSEIF current_month = '02'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_feb.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_feb.
        ADD poave_total TO ls_rsb_output-poave_feb.

        show_data_feb = abap_true.
      ELSEIF current_month = '03'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_mar.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_mar.
        ADD poave_total TO ls_rsb_output-poave_mar.

        show_data_mar = abap_true.
      ELSEIF current_month = '04'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_apr.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_apr.
        ADD poave_total TO ls_rsb_output-poave_apr.

        show_data_apr = abap_true.
      ELSEIF current_month = '05'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_may.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_may.
        ADD poave_total TO ls_rsb_output-poave_may.

        show_data_may = abap_true.
      ELSEIF current_month = '06'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_jun.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_jun.
        ADD poave_total TO ls_rsb_output-poave_jun.

        show_data_jun = abap_true.
      ELSEIF current_month = '07'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_jul.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_jul.
        ADD poave_total TO ls_rsb_output-poave_jul.

        show_data_jul = abap_true.
      ELSEIF current_month = '08'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_aug.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_aug.
        ADD poave_total TO ls_rsb_output-poave_aug.

        show_data_aug = abap_true.
      ELSEIF current_month = '09'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_sep.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_sep.
        ADD poave_total TO ls_rsb_output-poave_sep.

        show_data_sep = abap_true.
      ELSEIF current_month = '10'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_oct.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_oct.
        ADD poave_total TO ls_rsb_output-poave_oct.

        show_data_oct = abap_true.
      ELSEIF current_month = '11'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_nov.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_nov.
        ADD poave_total TO ls_rsb_output-poave_nov.

        show_data_nov = abap_true.
      ELSEIF current_month = '12'.
        poave_total = ls_rsb_trnd-poamnt / ls_rsb_trnd-poqty.
        totalkgs = ls_rsb_trnd-kgs * ls_rsb_trnd-poqty.
        totalavekgs = ls_rsb_trnd-poamnt / totalkgs.
        ADD ls_rsb_trnd-poqty TO ls_rsb_output-poqty_dec.
        ADD ls_rsb_trnd-poamnt TO ls_rsb_output-poamnt_dec.
        ADD poave_total TO ls_rsb_output-poave_dec.

        show_data_dec = abap_true.
      ENDIF.

      ADD ls_rsb_trnd-poqty TO ls_rsb_output-totqty.
      ADD ls_rsb_trnd-poamnt TO ls_rsb_output-totamnt.
      ADD poave_total TO ls_rsb_output-totave.

*      ADD ls_rsb_trnd-poqty TO ls_hotspot_rsb-totqty.
*      ADD ls_rsb_trnd-poamnt TO ls_hotspot_rsb-totamnt.
*      ADD poave_total TO ls_hotspot_rsb-totave.
*      ADD totalkgs TO ls_hotspot_rsb-totkgs.
*      ADD totalavekgs TO ls_hotspot_rsb-totavekgs.

      MODIFY lt_rsb_output FROM ls_rsb_output.

    ENDLOOP.
  ENDLOOP.

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM fill_fieldcat_aggre.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'matname'.
  wa_fieldcat-reptext_ddic  = 'matname'.
  wa_fieldcat-seltext_l     = 'matname'.
  wa_fieldcat-hotspot       = 'x'.
  wa_fieldcat-seltext_m     = 'Material Name'.
  wa_fieldcat-seltext_s     = 'Mat. Name'.
  APPEND wa_fieldcat TO it_fieldcat.

  CLEAR: wa_fieldcat.

  " ======================================================== "

  IF show_data_jan = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jan'.
    wa_fieldcat-reptext_ddic = 'poqty_jan'.
    wa_fieldcat-seltext_l    = 'poqty_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jan'.
    wa_fieldcat-reptext_ddic = 'poamnt_jan'.
    wa_fieldcat-seltext_l    = 'poamnt_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jan'.
    wa_fieldcat-reptext_ddic = 'poave_jan'.
    wa_fieldcat-seltext_l    = 'poave_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_feb = 'X'.
    wa_fieldcat-fieldname    = 'poqty_feb'.
    wa_fieldcat-reptext_ddic = 'poqty_feb'.
    wa_fieldcat-seltext_l    = 'poqty_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_feb'.
    wa_fieldcat-reptext_ddic = 'poamnt_feb'.
    wa_fieldcat-seltext_l    = 'poamnt_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_feb'.
    wa_fieldcat-reptext_ddic = 'poave_feb'.
    wa_fieldcat-seltext_l    = 'poave_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_mar = 'X'.
    wa_fieldcat-fieldname    = 'poqty_mar'.
    wa_fieldcat-reptext_ddic = 'poqty_mar'.
    wa_fieldcat-seltext_l    = 'poqty_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_mar'.
    wa_fieldcat-reptext_ddic = 'poamnt_mar'.
    wa_fieldcat-seltext_l    = 'poamnt_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_mar'.
    wa_fieldcat-reptext_ddic = 'poave_mar'.
    wa_fieldcat-seltext_l    = 'poave_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_apr = 'X'.
    wa_fieldcat-fieldname    = 'poqty_apr'.
    wa_fieldcat-reptext_ddic = 'poqty_apr'.
    wa_fieldcat-seltext_l    = 'poqty_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_apr'.
    wa_fieldcat-reptext_ddic = 'poamnt_apr'.
    wa_fieldcat-seltext_l    = 'poamnt_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_apr'.
    wa_fieldcat-reptext_ddic = 'poave_apr'.
    wa_fieldcat-seltext_l    = 'poave_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_may = 'X'.
    wa_fieldcat-fieldname    = 'poqty_may'.
    wa_fieldcat-reptext_ddic = 'poqty_may'.
    wa_fieldcat-seltext_l    = 'poqty_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_may'.
    wa_fieldcat-reptext_ddic = 'poamnt_may'.
    wa_fieldcat-seltext_l    = 'poamnt_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_may'.
    wa_fieldcat-reptext_ddic = 'poave_may'.
    wa_fieldcat-seltext_l    = 'poave_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_jun = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jun'.
    wa_fieldcat-reptext_ddic = 'poqty_jun'.
    wa_fieldcat-seltext_l    = 'poqty_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jun'.
    wa_fieldcat-reptext_ddic = 'poamnt_jun'.
    wa_fieldcat-seltext_l    = 'poamnt_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jun'.
    wa_fieldcat-reptext_ddic = 'poave_jun'.
    wa_fieldcat-seltext_l    = 'poave_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_jul = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jul'.
    wa_fieldcat-reptext_ddic = 'poqty_jul'.
    wa_fieldcat-seltext_l    = 'poqty_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jul'.
    wa_fieldcat-reptext_ddic = 'poamnt_jul'.
    wa_fieldcat-seltext_l    = 'poamnt_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jul'.
    wa_fieldcat-reptext_ddic = 'poave_jul'.
    wa_fieldcat-seltext_l    = 'poave_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_aug = 'X'.
    wa_fieldcat-fieldname    = 'poqty_aug'.
    wa_fieldcat-reptext_ddic = 'poqty_aug'.
    wa_fieldcat-seltext_l    = 'poqty_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_aug'.
    wa_fieldcat-reptext_ddic = 'poamnt_aug'.
    wa_fieldcat-seltext_l    = 'poamnt_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_aug'.
    wa_fieldcat-reptext_ddic = 'poave_aug'.
    wa_fieldcat-seltext_l    = 'poave_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_sep = 'X'.
    wa_fieldcat-fieldname    = 'poqty_sep'.
    wa_fieldcat-reptext_ddic = 'poqty_sep'.
    wa_fieldcat-seltext_l    = 'poqty_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_sep'.
    wa_fieldcat-reptext_ddic = 'poamnt_sep'.
    wa_fieldcat-seltext_l    = 'poamnt_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_sep'.
    wa_fieldcat-reptext_ddic = 'poave_sep'.
    wa_fieldcat-seltext_l    = 'poave_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_oct = 'X'.
    wa_fieldcat-fieldname    = 'poqty_oct'.
    wa_fieldcat-reptext_ddic = 'poqty_oct'.
    wa_fieldcat-seltext_l    = 'poqty_oct'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_oct'.
    wa_fieldcat-reptext_ddic = 'poamnt_oct'.
    wa_fieldcat-seltext_l    = 'poamnt_oct'.
    wa_fieldcat-seltext_m    = 'Oct-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_oct'.
    wa_fieldcat-reptext_ddic = 'poave_oct'.
    wa_fieldcat-seltext_l    = 'poave_oct'.
    wa_fieldcat-seltext_m    = 'Oct-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_nov = 'X'.
    wa_fieldcat-fieldname    = 'poqty_nov'.
    wa_fieldcat-reptext_ddic = 'poqty_nov'.
    wa_fieldcat-seltext_l    = 'poqty_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_nov'.
    wa_fieldcat-reptext_ddic = 'poamnt_nov'.
    wa_fieldcat-seltext_l    = 'poamnt_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_nov'.
    wa_fieldcat-reptext_ddic = 'poave_nov'.
    wa_fieldcat-seltext_l    = 'poave_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_dec = 'X'.
    wa_fieldcat-fieldname    = 'poqty_dec'.
    wa_fieldcat-reptext_ddic = 'poqty_dec'.
    wa_fieldcat-seltext_l    = 'poqty_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_dec'.
    wa_fieldcat-reptext_ddic = 'poamnt_dec'.
    wa_fieldcat-seltext_l    = 'poamnt_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_dec'.
    wa_fieldcat-reptext_ddic = 'poave_dec'.
    wa_fieldcat-seltext_l    = 'poave_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM fill_fieldcat_cement.

    wa_fieldcat-fieldname    = 'venname1'.
    wa_fieldcat-reptext_ddic = 'venname1'.
    wa_fieldcat-seltext_l    = 'venname1'.
    wa_fieldcat-hotspot       = 'x'.
    wa_fieldcat-seltext_m    = 'Vendor Name 1' .
    wa_fieldcat-seltext_s    = 'vname1'.
    APPEND wa_fieldcat TO it_fieldcat.

    CLEAR: wa_fieldcat.

   " ======================================================== "

  IF show_data_jan = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jan'.
    wa_fieldcat-reptext_ddic = 'poqty_jan'.
    wa_fieldcat-seltext_l    = 'poqty_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jan'.
    wa_fieldcat-reptext_ddic = 'poamnt_jan'.
    wa_fieldcat-seltext_l    = 'poamnt_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jan'.
    wa_fieldcat-reptext_ddic = 'poave_jan'.
    wa_fieldcat-seltext_l    = 'poave_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_feb = 'X'.
    wa_fieldcat-fieldname    = 'poqty_feb'.
    wa_fieldcat-reptext_ddic = 'poqty_feb'.
    wa_fieldcat-seltext_l    = 'poqty_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_feb'.
    wa_fieldcat-reptext_ddic = 'poamnt_feb'.
    wa_fieldcat-seltext_l    = 'poamnt_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_feb'.
    wa_fieldcat-reptext_ddic = 'poave_feb'.
    wa_fieldcat-seltext_l    = 'poave_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_mar = 'X'.
    wa_fieldcat-fieldname    = 'poqty_mar'.
    wa_fieldcat-reptext_ddic = 'poqty_mar'.
    wa_fieldcat-seltext_l    = 'poqty_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_mar'.
    wa_fieldcat-reptext_ddic = 'poamnt_mar'.
    wa_fieldcat-seltext_l    = 'poamnt_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_mar'.
    wa_fieldcat-reptext_ddic = 'poave_mar'.
    wa_fieldcat-seltext_l    = 'poave_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_apr = 'X'.
    wa_fieldcat-fieldname    = 'poqty_apr'.
    wa_fieldcat-reptext_ddic = 'poqty_apr'.
    wa_fieldcat-seltext_l    = 'poqty_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_apr'.
    wa_fieldcat-reptext_ddic = 'poamnt_apr'.
    wa_fieldcat-seltext_l    = 'poamnt_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_apr'.
    wa_fieldcat-reptext_ddic = 'poave_apr'.
    wa_fieldcat-seltext_l    = 'poave_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_may = 'X'.
    wa_fieldcat-fieldname    = 'poqty_may'.
    wa_fieldcat-reptext_ddic = 'poqty_may'.
    wa_fieldcat-seltext_l    = 'poqty_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_may'.
    wa_fieldcat-reptext_ddic = 'poamnt_may'.
    wa_fieldcat-seltext_l    = 'poamnt_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_may'.
    wa_fieldcat-reptext_ddic = 'poave_may'.
    wa_fieldcat-seltext_l    = 'poave_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_jun = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jun'.
    wa_fieldcat-reptext_ddic = 'poqty_jun'.
    wa_fieldcat-seltext_l    = 'poqty_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jun'.
    wa_fieldcat-reptext_ddic = 'poamnt_jun'.
    wa_fieldcat-seltext_l    = 'poamnt_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jun'.
    wa_fieldcat-reptext_ddic = 'poave_jun'.
    wa_fieldcat-seltext_l    = 'poave_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_jul = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jul'.
    wa_fieldcat-reptext_ddic = 'poqty_jul'.
    wa_fieldcat-seltext_l    = 'poqty_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jul'.
    wa_fieldcat-reptext_ddic = 'poamnt_jul'.
    wa_fieldcat-seltext_l    = 'poamnt_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jul'.
    wa_fieldcat-reptext_ddic = 'poave_jul'.
    wa_fieldcat-seltext_l    = 'poave_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_aug = 'X'.
    wa_fieldcat-fieldname    = 'poqty_aug'.
    wa_fieldcat-reptext_ddic = 'poqty_aug'.
    wa_fieldcat-seltext_l    = 'poqty_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_aug'.
    wa_fieldcat-reptext_ddic = 'poamnt_aug'.
    wa_fieldcat-seltext_l    = 'poamnt_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_aug'.
    wa_fieldcat-reptext_ddic = 'poave_aug'.
    wa_fieldcat-seltext_l    = 'poave_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_sep = 'X'.
    wa_fieldcat-fieldname    = 'poqty_sep'.
    wa_fieldcat-reptext_ddic = 'poqty_sep'.
    wa_fieldcat-seltext_l    = 'poqty_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_sep'.
    wa_fieldcat-reptext_ddic = 'poamnt_sep'.
    wa_fieldcat-seltext_l    = 'poamnt_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_sep'.
    wa_fieldcat-reptext_ddic = 'poave_sep'.
    wa_fieldcat-seltext_l    = 'poave_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_oct = 'X'.
    wa_fieldcat-fieldname    = 'poqty_oct'.
    wa_fieldcat-reptext_ddic = 'poqty_oct'.
    wa_fieldcat-seltext_l    = 'poqty_oct'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_oct'.
    wa_fieldcat-reptext_ddic = 'poamnt_oct'.
    wa_fieldcat-seltext_l    = 'poamnt_oct'.
    wa_fieldcat-seltext_m    = 'Oct-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_oct'.
    wa_fieldcat-reptext_ddic = 'poave_oct'.
    wa_fieldcat-seltext_l    = 'poave_oct'.
    wa_fieldcat-seltext_m    = 'Oct-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_nov = 'X'.
    wa_fieldcat-fieldname    = 'poqty_nov'.
    wa_fieldcat-reptext_ddic = 'poqty_nov'.
    wa_fieldcat-seltext_l    = 'poqty_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_nov'.
    wa_fieldcat-reptext_ddic = 'poamnt_nov'.
    wa_fieldcat-seltext_l    = 'poamnt_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_nov'.
    wa_fieldcat-reptext_ddic = 'poave_nov'.
    wa_fieldcat-seltext_l    = 'poave_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_dec = 'X'.
    wa_fieldcat-fieldname    = 'poqty_dec'.
    wa_fieldcat-reptext_ddic = 'poqty_dec'.
    wa_fieldcat-seltext_l    = 'poqty_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_dec'.
    wa_fieldcat-reptext_ddic = 'poamnt_dec'.
    wa_fieldcat-seltext_l    = 'poamnt_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_dec'.
    wa_fieldcat-reptext_ddic = 'poave_dec'.
    wa_fieldcat-seltext_l    = 'poave_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM fill_fieldcat_rsb.

    wa_fieldcat-fieldname    = 'grade'.
    wa_fieldcat-reptext_ddic = 'grade'.
    wa_fieldcat-seltext_l    = 'grade'.
     wa_fieldcat-hotspot       = 'x'.
    wa_fieldcat-seltext_m    = 'Grade' .
    wa_fieldcat-seltext_s    = 'grd'.
    APPEND wa_fieldcat TO it_fieldcat.

    CLEAR: wa_fieldcat.

   " ======================================================== "

  IF show_data_jan = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jan'.
    wa_fieldcat-reptext_ddic = 'poqty_jan'.
    wa_fieldcat-seltext_l    = 'poqty_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jan'.
    wa_fieldcat-reptext_ddic = 'poamnt_jan'.
    wa_fieldcat-seltext_l    = 'poamnt_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jan'.
    wa_fieldcat-reptext_ddic = 'poave_jan'.
    wa_fieldcat-seltext_l    = 'poave_jan'.
    wa_fieldcat-seltext_m    = 'Jan-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_feb = 'X'.
    wa_fieldcat-fieldname    = 'poqty_feb'.
    wa_fieldcat-reptext_ddic = 'poqty_feb'.
    wa_fieldcat-seltext_l    = 'poqty_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_feb'.
    wa_fieldcat-reptext_ddic = 'poamnt_feb'.
    wa_fieldcat-seltext_l    = 'poamnt_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_feb'.
    wa_fieldcat-reptext_ddic = 'poave_feb'.
    wa_fieldcat-seltext_l    = 'poave_feb'.
    wa_fieldcat-seltext_m    = 'Feb-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_mar = 'X'.
    wa_fieldcat-fieldname    = 'poqty_mar'.
    wa_fieldcat-reptext_ddic = 'poqty_mar'.
    wa_fieldcat-seltext_l    = 'poqty_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_mar'.
    wa_fieldcat-reptext_ddic = 'poamnt_mar'.
    wa_fieldcat-seltext_l    = 'poamnt_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_mar'.
    wa_fieldcat-reptext_ddic = 'poave_mar'.
    wa_fieldcat-seltext_l    = 'poave_mar'.
    wa_fieldcat-seltext_m    = 'Mar-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_apr = 'X'.
    wa_fieldcat-fieldname    = 'poqty_apr'.
    wa_fieldcat-reptext_ddic = 'poqty_apr'.
    wa_fieldcat-seltext_l    = 'poqty_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_apr'.
    wa_fieldcat-reptext_ddic = 'poamnt_apr'.
    wa_fieldcat-seltext_l    = 'poamnt_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_apr'.
    wa_fieldcat-reptext_ddic = 'poave_apr'.
    wa_fieldcat-seltext_l    = 'poave_apr'.
    wa_fieldcat-seltext_m    = 'Apr-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_may = 'X'.
    wa_fieldcat-fieldname    = 'poqty_may'.
    wa_fieldcat-reptext_ddic = 'poqty_may'.
    wa_fieldcat-seltext_l    = 'poqty_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_may'.
    wa_fieldcat-reptext_ddic = 'poamnt_may'.
    wa_fieldcat-seltext_l    = 'poamnt_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_may'.
    wa_fieldcat-reptext_ddic = 'poave_may'.
    wa_fieldcat-seltext_l    = 'poave_may'.
    wa_fieldcat-seltext_m    = 'May-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_jun = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jun'.
    wa_fieldcat-reptext_ddic = 'poqty_jun'.
    wa_fieldcat-seltext_l    = 'poqty_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jun'.
    wa_fieldcat-reptext_ddic = 'poamnt_jun'.
    wa_fieldcat-seltext_l    = 'poamnt_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jun'.
    wa_fieldcat-reptext_ddic = 'poave_jun'.
    wa_fieldcat-seltext_l    = 'poave_jun'.
    wa_fieldcat-seltext_m    = 'Jun-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_jul = 'X'.
    wa_fieldcat-fieldname    = 'poqty_jul'.
    wa_fieldcat-reptext_ddic = 'poqty_jul'.
    wa_fieldcat-seltext_l    = 'poqty_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_jul'.
    wa_fieldcat-reptext_ddic = 'poamnt_jul'.
    wa_fieldcat-seltext_l    = 'poamnt_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_jul'.
    wa_fieldcat-reptext_ddic = 'poave_jul'.
    wa_fieldcat-seltext_l    = 'poave_jul'.
    wa_fieldcat-seltext_m    = 'Jul-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_aug = 'X'.
    wa_fieldcat-fieldname    = 'poqty_aug'.
    wa_fieldcat-reptext_ddic = 'poqty_aug'.
    wa_fieldcat-seltext_l    = 'poqty_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_aug'.
    wa_fieldcat-reptext_ddic = 'poamnt_aug'.
    wa_fieldcat-seltext_l    = 'poamnt_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_aug'.
    wa_fieldcat-reptext_ddic = 'poave_aug'.
    wa_fieldcat-seltext_l    = 'poave_aug'.
    wa_fieldcat-seltext_m    = 'Aug-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_sep = 'X'.
    wa_fieldcat-fieldname    = 'poqty_sep'.
    wa_fieldcat-reptext_ddic = 'poqty_sep'.
    wa_fieldcat-seltext_l    = 'poqty_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_sep'.
    wa_fieldcat-reptext_ddic = 'poamnt_sep'.
    wa_fieldcat-seltext_l    = 'poamnt_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_sep'.
    wa_fieldcat-reptext_ddic = 'poave_sep'.
    wa_fieldcat-seltext_l    = 'poave_sep'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_oct = 'X'.
    wa_fieldcat-fieldname    = 'poqty_oct'.
    wa_fieldcat-reptext_ddic = 'poqty_oct'.
    wa_fieldcat-seltext_l    = 'poqty_oct'.
    wa_fieldcat-seltext_m    = 'Sep-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_oct'.
    wa_fieldcat-reptext_ddic = 'poamnt_oct'.
    wa_fieldcat-seltext_l    = 'poamnt_oct'.
    wa_fieldcat-seltext_m    = 'Oct-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_oct'.
    wa_fieldcat-reptext_ddic = 'poave_oct'.
    wa_fieldcat-seltext_l    = 'poave_oct'.
    wa_fieldcat-seltext_m    = 'Oct-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_nov = 'X'.
    wa_fieldcat-fieldname    = 'poqty_nov'.
    wa_fieldcat-reptext_ddic = 'poqty_nov'.
    wa_fieldcat-seltext_l    = 'poqty_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_nov'.
    wa_fieldcat-reptext_ddic = 'poamnt_nov'.
    wa_fieldcat-seltext_l    = 'poamnt_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_nov'.
    wa_fieldcat-reptext_ddic = 'poave_nov'.
    wa_fieldcat-seltext_l    = 'poave_nov'.
    wa_fieldcat-seltext_m    = 'Nov-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

  IF show_data_dec = 'X'.
    wa_fieldcat-fieldname    = 'poqty_dec'.
    wa_fieldcat-reptext_ddic = 'poqty_dec'.
    wa_fieldcat-seltext_l    = 'poqty_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Qty' .
    wa_fieldcat-seltext_s    = 'pqty'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poamnt_dec'.
    wa_fieldcat-reptext_ddic = 'poamnt_dec'.
    wa_fieldcat-seltext_l    = 'poamnt_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Amt' .
    wa_fieldcat-seltext_s    = 'pamt'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.

    wa_fieldcat-fieldname    = 'poave_dec'.
    wa_fieldcat-reptext_ddic = 'poave_dec'.
    wa_fieldcat-seltext_l    = 'poave_dec'.
    wa_fieldcat-seltext_m    = 'Dec-' && current_year && ' Ave' .
    wa_fieldcat-seltext_s    = 'pave'.
    APPEND wa_fieldcat TO it_fieldcat.
    CLEAR: wa_fieldcat.
  ENDIF.

  " ======================================================== "

ENDFORM.

" ======================================================== "

FORM call_alv_grid_display_aggre.
  DATA: lt_layout TYPE slis_layout_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = 'Price Trend Report - Aggregates'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout          = lt_layout
      it_fieldcat        = it_fieldcat
    TABLES
      t_outtab           = output_trnd.
ENDFORM.

" ======================================================== "

FORM call_alv_grid_display_cement.
  DATA: lt_layout TYPE slis_layout_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = 'Price Trend Report - Cement'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout          = lt_layout
      it_fieldcat        = it_fieldcat
    TABLES
      t_outtab           = lt_cem_output.
ENDFORM.

" ======================================================== "

FORM call_alv_grid_display_rsb.
  DATA: lt_layout TYPE slis_layout_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title       = 'Price Trend Report - Reinforced Steel Bars'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout          = lt_layout
      it_fieldcat        = it_fieldcat
    TABLES
      t_outtab           = lt_rsb_output.
ENDFORM.

" ======================================================== "

FORM user_command USING r_ucomm TYPE sy-ucomm
                        rs_selfield TYPE slis_selfield.
  CASE r_ucomm.
    WHEN '&IC1'.
      IF agg = 'X'.
        PERFORM show_details_agg USING rs_selfield-value.
      ELSEIF cem = 'X'.
        PERFORM show_details_cem USING rs_selfield-value.
      ELSEIF rsb = 'X'.
        PERFORM show_details_rsb USING rs_selfield-value.
      ENDIF.
    WHEN OTHERS.
      ...
  ENDCASE.

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM show_details_agg USING p_value.

  REFRESH: lt_hotspot_aggre,
           it_fieldcat2.

  LOOP AT output_trnd INTO ls_output WHERE matname = p_value.
     MOVE ls_output TO ls_hotspot_aggre.
     APPEND ls_hotspot_aggre TO lt_hotspot_aggre.
  ENDLOOP.

  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'cdate'.
  wa_fieldcat-reptext_ddic = 'cdate'.
  wa_fieldcat-seltext_l    = 'cdate'.
  wa_fieldcat-seltext_m    = 'Created Date'.
  wa_fieldcat-seltext_s    = 'cdate'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'plantcode'.
  wa_fieldcat-reptext_ddic  = 'plantcode'.
  wa_fieldcat-seltext_l     = 'plantcode'.
  wa_fieldcat-seltext_m     = 'Plant Code'.
  wa_fieldcat-seltext_s     = 'pt code'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'plantname'.
  wa_fieldcat-reptext_ddic  = 'plantname'.
  wa_fieldcat-seltext_l     = 'plantname'.
  wa_fieldcat-seltext_m     = 'Plant Name'.
  wa_fieldcat-seltext_s     = 'pl name'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'vendor'.
  wa_fieldcat-reptext_ddic  = 'vendor'.
  wa_fieldcat-seltext_l     = 'vendor'.
  wa_fieldcat-seltext_m     = 'Vendor'.
  wa_fieldcat-seltext_s     = 'vend'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'vname1'.
  wa_fieldcat-reptext_ddic  = 'vname1'.
  wa_fieldcat-seltext_l     = 'vname1'.
  wa_fieldcat-seltext_m     = 'Vendor Name'.
  wa_fieldcat-seltext_s     = 'vendn'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'matgrp'.
  wa_fieldcat-reptext_ddic  = 'matgrp'.
  wa_fieldcat-seltext_l     = 'matgrp'.
  wa_fieldcat-seltext_m     = 'Material Group'.
  wa_fieldcat-seltext_s     = 'matgrp'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'purord'.
  wa_fieldcat-reptext_ddic  = 'purord'.
  wa_fieldcat-seltext_l     = 'purord'.
  wa_fieldcat-seltext_m     = 'Purchase Order'.
  wa_fieldcat-seltext_s     = 'purord'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'litem'.
  wa_fieldcat-reptext_ddic  = 'litem'.
  wa_fieldcat-seltext_l     = 'litem'.
  wa_fieldcat-seltext_m     = 'Line Item'.
  wa_fieldcat-seltext_s     = 'litem'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'matnum'.
  wa_fieldcat-reptext_ddic  = 'matnum'.
  wa_fieldcat-seltext_l     = 'matnum'.
  wa_fieldcat-seltext_m     = 'Material No.'.
  wa_fieldcat-seltext_s     = 'matnum'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'matdesc'.
  wa_fieldcat-reptext_ddic  = 'matdesc'.
  wa_fieldcat-seltext_l     = 'matdesc'.
  wa_fieldcat-seltext_m     = 'Material Description'.
  wa_fieldcat-seltext_s     = 'matdesc'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'poqty'.
  wa_fieldcat-reptext_ddic = 'poqty'.
  wa_fieldcat-seltext_l    = 'poqty'.
  wa_fieldcat-seltext_m    = 'Quantity' .
  wa_fieldcat-seltext_s    = 'tqty'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'uom'.
  wa_fieldcat-reptext_ddic = 'uom'.
  wa_fieldcat-seltext_l    = 'uom'.
  wa_fieldcat-seltext_m    = 'UoM' .
  wa_fieldcat-seltext_s    = 'uom'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'netpr'.
  wa_fieldcat-reptext_ddic = 'netpr'.
  wa_fieldcat-seltext_l    = 'netpr'.
  wa_fieldcat-seltext_m    = 'Net Price' .
  wa_fieldcat-seltext_s    = 'netpr'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'poamnt'.
  wa_fieldcat-reptext_ddic = 'poamnt'.
  wa_fieldcat-seltext_l    = 'poamnt'.
  wa_fieldcat-seltext_m    = 'Net Amount' .
  wa_fieldcat-seltext_s    = 'poamnt'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat2.
  CLEAR: wa_fieldcat.

  DATA: lt_layout TYPE slis_layout_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title      = 'Price Trend Report - Aggregates (Sub-Reports)'
      is_layout         = lt_layout
      it_fieldcat       = it_fieldcat2
    TABLES
      t_outtab          = lt_hotspot_aggre.

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM show_details_cem USING p_value.

  REFRESH: lt_hotspot_cem,
           it_fieldcat3.

  LOOP AT lt_cem_output INTO ls_cem_output WHERE venname1 = p_value.
     MOVE ls_cem_output TO ls_hotspot_cem.
     APPEND ls_hotspot_cem TO lt_hotspot_cem.
  ENDLOOP.

  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'cdate'.
  wa_fieldcat-reptext_ddic = 'cdate'.
  wa_fieldcat-seltext_l    = 'cdate'.
  wa_fieldcat-seltext_m    = 'Created Date'.
  wa_fieldcat-seltext_s    = 'cdate'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'matgrp'.
  wa_fieldcat-reptext_ddic  = 'matgrp'.
  wa_fieldcat-seltext_l     = 'matgrp'.
  wa_fieldcat-seltext_m     = 'Material Group'.
  wa_fieldcat-seltext_s     = 'mgrp'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'vendor'.
  wa_fieldcat-reptext_ddic  = 'vendor'.
  wa_fieldcat-seltext_l     = 'vendor'.
  wa_fieldcat-seltext_m     = 'Vendor'.
  wa_fieldcat-seltext_s     = 'vend'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'venname1'.
  wa_fieldcat-reptext_ddic = 'venname1'.
  wa_fieldcat-seltext_l    = 'venname1'.
  wa_fieldcat-seltext_m    = 'Vendor Name' .
  wa_fieldcat-seltext_s    = 'vendn'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'purord'.
  wa_fieldcat-reptext_ddic = 'purord'.
  wa_fieldcat-seltext_l    = 'purord'.
  wa_fieldcat-seltext_m    = 'Purchase Order' .
  wa_fieldcat-seltext_s    = 'pord'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'litem'.
  wa_fieldcat-reptext_ddic = 'litem'.
  wa_fieldcat-seltext_l    = 'litem'.
  wa_fieldcat-seltext_m    = 'Line Item' .
  wa_fieldcat-seltext_s    = 'litem'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'matnum'.
  wa_fieldcat-reptext_ddic = 'matnum'.
  wa_fieldcat-seltext_l    = 'matnum'.
  wa_fieldcat-seltext_m    = 'Material Number' .
  wa_fieldcat-seltext_s    = 'mnum'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'matdesc'.
  wa_fieldcat-reptext_ddic = 'matdesc'.
  wa_fieldcat-seltext_l    = 'matdesc'.
  wa_fieldcat-seltext_m    = 'Material Description' .
  wa_fieldcat-seltext_s    = 'mdesc'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'plant'.
  wa_fieldcat-reptext_ddic = 'plant'.
  wa_fieldcat-seltext_l    = 'plant'.
  wa_fieldcat-seltext_m    = 'Plant' .
  wa_fieldcat-seltext_s    = 'plnt'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'plantname'.
  wa_fieldcat-reptext_ddic = 'plantname'.
  wa_fieldcat-seltext_l    = 'plantname'.
  wa_fieldcat-seltext_m    = 'Plant Name' .
  wa_fieldcat-seltext_s    = 'plntn'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'poqty'.
  wa_fieldcat-reptext_ddic = 'poqty'.
  wa_fieldcat-seltext_l    = 'poqty'.
  wa_fieldcat-seltext_m    = 'Quantity' .
  wa_fieldcat-seltext_s    = 'poqty'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'uom'.
  wa_fieldcat-reptext_ddic = 'uom'.
  wa_fieldcat-seltext_l    = 'uom'.
  wa_fieldcat-seltext_m    = 'UoM' .
  wa_fieldcat-seltext_s    = 'uom'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'netpr'.
  wa_fieldcat-reptext_ddic = 'netpr'.
  wa_fieldcat-seltext_l    = 'netpr'.
  wa_fieldcat-seltext_m    = 'Net Price' .
  wa_fieldcat-seltext_s    = 'netpr'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'poamnt'.
  wa_fieldcat-reptext_ddic = 'poamnt'.
  wa_fieldcat-seltext_l    = 'poamnt'.
  wa_fieldcat-seltext_m    = 'Net Amount' .
  wa_fieldcat-seltext_s    = 'poamnt'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat3.
  CLEAR: wa_fieldcat.

  DATA: lt_layout TYPE slis_layout_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title      = 'Price Trend Report - Cement (Sub-Reports)'
      is_layout         = lt_layout
      it_fieldcat       = it_fieldcat3
    TABLES
      t_outtab          = lt_hotspot_cem.

ENDFORM.

" ======================================================== "
" ======================================================== "

FORM show_details_rsb USING p_value.

  REFRESH: lt_hotspot_rsb,
           it_fieldcat4.

  LOOP AT lt_rsb_output INTO ls_rsb_output WHERE grade = p_value.
     MOVE ls_rsb_output TO ls_hotspot_rsb.
     APPEND ls_hotspot_rsb TO lt_hotspot_rsb.
  ENDLOOP.

  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'cdate'.
  wa_fieldcat-reptext_ddic = 'cdate'.
  wa_fieldcat-seltext_l    = 'cdate'.
  wa_fieldcat-seltext_m    = 'Created Date'.
  wa_fieldcat-seltext_s    = 'cdate'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'matgrp'.
  wa_fieldcat-reptext_ddic = 'matgrp'.
  wa_fieldcat-seltext_l    = 'matgrp'.
  wa_fieldcat-seltext_m    = 'Material Group'.
  wa_fieldcat-seltext_s    = 'mgrp'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'grade'.
  wa_fieldcat-reptext_ddic = 'grade'.
  wa_fieldcat-seltext_l    = 'grade'.
  wa_fieldcat-seltext_m    = 'Grade'.
  wa_fieldcat-seltext_s    = 'grd'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'purord'.
  wa_fieldcat-reptext_ddic = 'purord'.
  wa_fieldcat-seltext_l    = 'purord'.
  wa_fieldcat-seltext_m    = 'Purchase Order'.
  wa_fieldcat-seltext_s    = 'purord'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'litem'.
  wa_fieldcat-reptext_ddic = 'litem'.
  wa_fieldcat-seltext_l    = 'litem'.
  wa_fieldcat-seltext_m    = 'Line Item'.
  wa_fieldcat-seltext_s    = 'litem'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'matnum'.
  wa_fieldcat-reptext_ddic  = 'matnum'.
  wa_fieldcat-seltext_l     = 'matnum'.
  wa_fieldcat-seltext_m     = 'Material Number'.
  wa_fieldcat-seltext_s     = 'mname'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'matdesc'.
  wa_fieldcat-reptext_ddic  = 'matdesc'.
  wa_fieldcat-seltext_l     = 'matdesc'.
  wa_fieldcat-seltext_m     = 'Material Description'.
  wa_fieldcat-seltext_s     = 'mdesc'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'dia'.
  wa_fieldcat-reptext_ddic  = 'dia'.
  wa_fieldcat-seltext_l     = 'dia'.
  wa_fieldcat-seltext_m     = 'Dia(mm)'.
  wa_fieldcat-seltext_s     = 'dia'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'len'.
  wa_fieldcat-reptext_ddic  = 'len'.
  wa_fieldcat-seltext_l     = 'len'.
  wa_fieldcat-seltext_m     = 'Length(m)'.
  wa_fieldcat-seltext_s     = 'len'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'mass'.
  wa_fieldcat-reptext_ddic  = 'mass'.
  wa_fieldcat-seltext_l     = 'mass'.
  wa_fieldcat-seltext_m     = 'Mass(kg/m)'.
  wa_fieldcat-seltext_s     = 'mass'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'kgs'.
  wa_fieldcat-reptext_ddic  = 'kgs'.
  wa_fieldcat-seltext_l     = 'kgs'.
  wa_fieldcat-seltext_m     = 'Kgs/Pc'.
  wa_fieldcat-seltext_s     = 'mass'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname     = 'plantname'.
  wa_fieldcat-reptext_ddic  = 'plantname'.
  wa_fieldcat-seltext_l     = 'plantname'.
  wa_fieldcat-seltext_m     = 'Plant Name'.
  wa_fieldcat-seltext_s     = 'plantname'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'poqty'.
  wa_fieldcat-reptext_ddic = 'poqty'.
  wa_fieldcat-seltext_l    = 'poqty'.
  wa_fieldcat-seltext_m    = 'Total Qty'.
  wa_fieldcat-seltext_s    = 'poqty'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'totkgs'.
  wa_fieldcat-reptext_ddic = 'totkgs'.
  wa_fieldcat-seltext_l    = 'totkgs'.
  wa_fieldcat-seltext_m    = 'Total Kgs'.
  wa_fieldcat-seltext_s    = 'totkgs'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'uom'.
  wa_fieldcat-reptext_ddic = 'uom'.
  wa_fieldcat-seltext_l    = 'uom'.
  wa_fieldcat-seltext_m    = 'UoM'.
  wa_fieldcat-seltext_s    = 'uom'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'netpr'.
  wa_fieldcat-reptext_ddic = 'netpr'.
  wa_fieldcat-seltext_l    = 'netpr'.
  wa_fieldcat-seltext_m    = 'Net Price'.
  wa_fieldcat-seltext_s    = 'netpr'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'poamnt'.
  wa_fieldcat-reptext_ddic = 'poamnt'.
  wa_fieldcat-seltext_l    = 'poamnt'.
  wa_fieldcat-seltext_m    = 'Net Amount'.
  wa_fieldcat-seltext_s    = 'poamnt'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  wa_fieldcat-fieldname    = 'totavekgs'.
  wa_fieldcat-reptext_ddic = 'totavekgs'.
  wa_fieldcat-seltext_l    = 'totavekgs'.
  wa_fieldcat-seltext_m    = 'Ave. per kg'.
  wa_fieldcat-seltext_s    = 'totavekgs'.
  wa_fieldcat-do_sum       = 'X'.
  APPEND wa_fieldcat 	TO it_fieldcat4.
  CLEAR: wa_fieldcat.

  DATA: lt_layout TYPE slis_layout_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid
      i_grid_title      = 'Price Trend Report - Reinforced Steel Bar (Sub-Reports)'
      is_layout         = lt_layout
      it_fieldcat       = it_fieldcat4
    TABLES
      t_outtab          = lt_hotspot_rsb.

ENDFORM.
