      *DD0351 11/10/12 pase Changement destinataire de mail 
      *DD0448 06/06/11 elgu nouveau code taxe
      *DD0351 13/11/09 elgu  modif code tarif pour isorub 7 ==> 8
      *DD0351 27/10/09 elgu 
      *DD0351 19/10/09 elgu suppression mode REPRISE, restaurer resjnlvt
      *DD0422 18/02/09 elgu pour la slovaquie controler paramcpt avec ouverture pour les factures code geo 0 et exoneree
      *DD0422 16/02/09 elgu ne pas mettre a jour code journal edite se fera dans amcpvent(interface iris)
      *                     supprimer le controle date
      *DD0422 28/01/09 elgu prendre le code geographique du livre a
      *DD0351 14/10/08 elgu modif display pour slovaquie
      *DD0394 01/02/08 elgu appeler creation commande fournisseur pour facture cession
      *DD0351 17/01/08 elgu mettre display en langue
      *DD0370 25/09/07 elgu ajout titre de l'etat avec societe
      *DD0326 30/06/06 elgu
      *DD0316 25/04/06 elgu
      *DD0314 27/03/06 elgu fusion LEAU dans environnement GPI
      *DDE338 28/01/03 elgu
       IDENTIFICATION DIVISION.
       PROGRAM-ID. FACTC025.
      *
      ***********************************************************
      * GPICMT  GPIGPI EDITION  DU  JOURNAL  DES  VENTES        *
      ***********************************************************
      *
      * EP
      *
      *elgu remplace DECHETS par BSR
      *DDE089: modif suite a passage comptasyl en EUROS
      *        ajout edition ligne totaux montants en francs
      *DDE042: ajout trt classe 6 avec controle de saisie pour client DAISIF
      *        et remise en page des totaux (gdpublic/indusrie/JUZIERS/dechet)
      *M0100 : suite modif M0699 la ventilation cl 5 etait bonne sauf sur l'etat
      *M0699 : ajout trt gde classe 5 ADH  PROFESSIONNELS
      *M1198 : ajout trt gde classe 4 ADH  INDUSTRIE
      *M0797 : modifs pour nlle compta
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DPS-4.
       OBJECT-COMPUTER. DPS-4.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT ECRJO    ASSIGN TO ECR-MSD300
      *
      *    SELECT FFACTURE ASSIGN TO FFA-MSD300
      *
      *    SELECT PARAM    ASSIGN TO PAC-MSD300
      *
      *    SELECT FTRAITES ASSIGN TO FTR-MSD300
      *
      *    SELECT EXTRA    ASSIGN TO EXT-MSD300
      *
      *    SELECT PARAMGPI ASSIGN TO PAI-MSD300
      *
      *    SELECT PERCO    ASSIGN TO PER-MSD300
      *
      *    SELECT FCLIENTS ASSIGN TO FCL-MSD300
      *
      *    SELECT PARBATCH ASSIGN TO PAR-MSD300
      *
           SELECT ETAT1 ASSIGN TO wlabel-etat1
                       organization line sequential.
      *
           SELECT ETAT2 ASSIGN TO wlabel-etat2
                       organization line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ETAT1
                DATA RECORD LIGNE
                LINAGE IS 64
                LINES AT TOP 2.
       01  LIGNE              PIC X(128).
       01  L1.
           02 FILLER          PIC X(20).
           02 ETIT1           PIC X(44).
           02 EDAT.
             03 EJ1           PIC 99.
             03 ESL1          PIC X.
             03 EM1           PIC 99.
             03 ESL2         PIC X.
             03 EA1          PIC 99.
           02 FILLER         PIC X(37).
           02 EPAG           PIC X(7).
           02 ENPA           PIC ZZ9.
           02 FILLER         PIC X(9).
       01  L2.
           02 ETIT2.
             03 EAS1         PIC XX.
             03 ETA          PIC 999B.
             03 EAS2         PIC X.
             03 EGEO         PIC 9B.
             03 ECLI         PIC 9(6).
             03 EAS3         PIC XX.
DD0326       03 EFAC         PIC 9(7)B.
             03 EAS4         PIC X.
             03 ETAXX.
               04 ETAXE  OCCURS  4.
                 05 ETAX     PIC 9.
           02 EAS5           PIC XX.
           02 EVHT           PIC Z(7)9V,99-      BLANK ZERO.
           02 EAS6           PIC XX.
           02 EPOR           PIC Z(7)9V,99-      BLANK ZERO.
           02 EAS7           PIC X.
           02 EPPO           PIC ZZ9V,99         BLANK ZERO.
           02 EAS8           PIC XX.
           02 ETVA           PIC Z(7)9V,99-      BLANK ZERO.
           02 EAS9           PIC XX.
           02 EAFF           PIC Z(7)9V,99-      BLANK ZERO.
           02 EAS10          PIC XX.
           02 ENET           PIC Z(7)9V,99-      BLANK ZERO.
           02 EAS11          PIC XX.
           02 EANO           PIC X(20).
           02 EAS12          PIC X.
       01  L3.
           02 L31.
             03 ETIT3        PIC X(98).
             03 EJ2          PIC 99.
             03 ESL3         PIC X.
             03 EM2          PIC 99.
             03 ESL4         PIC X.
             03 EA2          PIC 99.
             03 FILLER       PIC X(15).
           02 L32  REDEFINES  L31.
             03 EBS1         PIC XXX.
             03 ETARB        PIC 9BB.
             03 EBS2         PIC XXX.
             03 ETAXB        PIC 9B.
             03 EBS3         PIC XXX.
             03 EPAYB        PIC 9B.
             03 EBS4         PIC X.
             03 EMONT.
               04 EMT  OCCURS  4.
                 05 EMTF     PIC Z(7)9V,99-       BLANK ZERO.
                 05 FILLER   PIC X.
                 05 EMTA     PIC Z(7)9V,99-       BLANK ZERO.
                 05 EBS      PIC X.
           02 FILLER         PIC X(7).
       01  L4.
           02 FILLER         PIC X(12).
           02 ELTAR          PIC X(18).
           02 EHTTA          PIC Z(7)9V,99-       BLANK ZERO.
           02 FILLER         PIC X.
           02 ELTAX          PIC X(13).
           02 EHTTAX         PIC Z(7)9V,99-       BLANK ZERO.
           02 FILLER         PIC X(16).
           02 ELPAY          PIC X(24).
           02 EHTPAY         PIC Z(7)9V,99-       BLANK ZERO.
           02 FILLER         PIC X(8).
       01  L5.
           02 FILLER         PIC X(32).
           02 ELIB           PIC X(96).
       01  L6.
           02 FILLER         PIC X(12).
           02 ELTAR6         PIC X(18).
           02 EHTTVA         PIC Z(7)9V,99-      BLANK ZERO.
           02 FILLER         PIC X.
           02 EHTSUS         PIC Z(7)9V,99-      BLANK ZERO.
           02 FILLER         PIC X.
           02 EHTMCO         PIC Z(7)9V,99-      BLANK ZERO.
           02 FILLER         PIC X(14).
           02 EHTETR         PIC Z(7)9V,99-      BLANK ZERO.
           02 FILLER         PIC X(14).
           02 EHTFRA         PIC Z(7)9V,99-      BLANK ZERO.
           02 FILLER         PIC X(8).
       FD  ETAT2
               DATA RECORD IS LIGN3
               LINAGE IS 64
               LINES AT TOP 2.
       01  LIGN3.
           02 FILLER         PIC X(10).
           02 LNCPT          PIC 9(6).
           02 FILLER         PIC X(41).
      *
       WORKING-STORAGE SECTION.
           copy "/usr/action/ADL/copy/wor-adl".
           copy "../copy/wor-parbatch".
      *----> M0797 (D)
      *    copy "../copy/wor-fclients".
      *----> M0797 (F)
           copy "../copy/wor-percogr0".
      *----> M0797 (D)
      *    copy "../copy/wor-percoel2".
      *    copy "../copy/wor-paramgpi".
      *    copy "../copy/wor-extracpt".
      *    copy "../copy/wor-ftraites".
      *----> M0797 (F)
           copy "../copy/wor-paramcpt".
           copy "../copy/wor-ffacture-cdesup".
           copy "../copy/wor-ecrcptjo".
DDE089     copy '../copy/mmca-devi.com'.                                *GPICMT
DD0370     copy '../copy/mmlp-titr.com'.                                *GPICMT
DD0351     copy '../copy/mmlp-mail.com'.                                *GPICMT
DD0394     copy '../copy/cmpa-tycd.com'.                                *GPICMT
DD0422     copy '../copy/mmti-date.com'.                                *GPICMT
       77  WHTG              PIC S9(8)V99.
       77  WPOG              PIC S9(8)V99.
       77  WAFG              PIC S9(8)V99.
       77  WTVG              PIC S9(8)V99.
       77  WTTG              PIC S9(8)V99.
       77  WPOAF             PIC S9(8)V99.
       77  WHTP              PIC S9(8)V99.
       77  WPOAFP            PIC S9(8)V99.
       77  WCMONTD           PIC S9(8)V99.
       77  WCMONTC           PIC S9(8)V99.
       77  WCMONT            PIC S9(8)V99.
       77  WEXMT1            PIC S9(10).
       77  WPO               PIC S9(8)V99.
       77  WAF               PIC S9(8)V99.
       77  WTV               PIC S9(8)V99.
       77  WCPTR             PIC 999.
       77  WPAGE             PIC 999.
       77  WNEC3             PIC s9.
       77  WNEC2             PIC s9.
       77  WNEC1             PIC s9.
       77  I                 PIC 99.
       77  J                 PIC 99.
       77  K                 PIC 99.
       77  L                 PIC 99.
       77  M                 PIC 99.
       77  WCTL              PIC 999.
       01  wlabel-etat1 pic x(64) value space.
       01  var-name1 pic x(64).
       01  var-data1 pic x(64).
       01  wlabel-etat2 pic x(64) value space.
       01  var-name2 pic x(64).
       01  var-data2 pic x(64).
       01  WZONES.
DD0370     02   wnom-prog                PIC X(10) value 'factc025'.
DD0394     02 sys-var                   PIC X(200).
DD0394     02 syst-rtn                  PIC s9(4)  comp.
           02 WDATE.
             03 WJ           PIC 99.
             03 WM           PIC 99.
             03 WA           PIC 99.
           02 WDAT9          REDEFINES  WDATE  PIC 9(6).
           02 WNPSJ          PIC 9(6).
           02 WTOP           PIC 9.
           02 WTOP2          PIC 9.
           02 WTOP3          PIC 9.
           02 WTOP4          PIC 9.
           02 WTOP5          PIC 9.
           02 WTOP6          PIC 9.
           02 WTOP7          PIC 9.
           02 WTOP8          PIC 99.
           02 WERREUR        PIC X(17).
           02 WTVEX.
             03 WVT          OCCURS 3   PIC 9.
           02 WTVE           REDEFINES  WTVEX PIC 999.
           02 WCOMPTE.
             03 WNCL         PIC 9(6).
             03 filler       PIC 9(4).
           02 WCOMPT9  REDEFINES  WCOMPTE  PIC 9(10).
           02 WLIOP.
DD0326       03 WFA          PIC X(7).
DD0326       03 WNFA         PIC 9(6).
             03 WESP         PIC X.
             03 WNCLL        PIC 9(6).
           02 WCFA           PIC X.
           02 WLIOP2.
             03 WECH         PIC X(12).
             03 WEJ          PIC 99.
             03 WSL1         PIC X.
             03 WEM          PIC 99.
             03 WSL2         PIC X.
             03 WEA          PIC 99.
           02 WDATEC9        PIC 9(6).
           02 WCLEGPI.
             03 WLIB         PIC X(8).
             03 WREG         PIC 99.
           02 WAFFCPT        PIC 9(10).
           02 WAFFECPT       PIC 9(10).
           02 WMTMINI        PIC S9(10).
           02 WMEMO          PIC 9.
           02 WHT1.
      *----> M1198 (D)
      *      03 WHTREC1  OCCURS  7  PIC S9(8)V99.
      *----> M0699 (D)
      *      03 WHTREC1  OCCURS  8  PIC S9(8)V99.
      *      03 WHTREC1  OCCURS  9  PIC S9(8)V99.
             03 WHTREC1  OCCURS  10  PIC S9(8)V99.                      *DDE042
      *----> M0699 (F)

      *----> M1198 (F)
           02 WHT2.
             03 WHTREC2  OCCURS  5  PIC S9(8)V99.
           02 WHT3.
             03 WHTREC3  OCCURS  4  PIC S9(8)V99.
           02 WHT4.
      *----> M1198 (D)
      *      03 WHT44  OCCURS  7.
      *----> M0699 (D)
      *      03 WHT44  OCCURS  8.
      *      03 WHT44  OCCURS  9.                                       *DDE042
             03 WHT44  OCCURS  10.                                      *DDE042
      *----> M0699 (F)

      *----> M1198 (F)
               04 WHTREC4 OCCURS 5  PIC S9(8)V99.
           02 WEJNAS1        PIC 9(6).
           02 WNCL3          PIC 9(6).
           02 WEXETS2        PIC 99.
           02 WEXNCPT2       PIC 9(10).
           02 WEXECHC2       PIC 9(6).
           02 WPO4.
      *----> M1198 (D)
      *      03 WPO44        OCCURS  7.
      *----> M0699 (D)
      *      03 WPO44        OCCURS  8.
      *      03 WPO44        OCCURS  9.                                 *DDE042
             03 WPO44        OCCURS  10.                                *DDE042
      *----> M0699 (F)

      *----> M1198 (F)
               04 WPOREC4 OCCURS 5  PIC S9(8)V99.
           02 WRES.
             03 WRESU     OCCURS 4  PIC S9(8)V99.
           02 WCOMPJ.
             03 WZER         PIC 9(8).
             03 WJNL         PIC 99.
           02 WCOMPTJ9       REDEFINES  WCOMPJ   PIC 9(10).
           02 WCMONTS        PIC X.
           02 WCODFA         PIC 9.
           02 WMONTECF       PIC S9(8)V99.
           02 WMONTECC       REDEFINES  WMONTECF
                                    PIC S9(10).
           02 WSIGN          PIC X.
           02 WTT            PIC S9(8)V99.
           02 WTTC REDEFINES WTT PIC S9(10).
           02 WHT            PIC S9(8)V99.
           02 WHTC REDEFINES WHT PIC S9(10).
           02 WMONTD         PIC S9(8)V99.
           02 WMONTDC        REDEFINES WMONTD
                                    PIC S9(10).
           02 WMONTC         PIC S9(8)V99.
           02 WMONTCC        REDEFINES WMONTC
                                    PIC S9(10).
           02 WAEDX.
             03 WAEDA        PIC 99.
             03 WAEDM        PIC 99.
             03 WAEDJ        PIC 99.
           02 WAED           REDEFINES WAEDX     PIC 9(6).
           02 WTEST          PIC 9.
DD0326     02 WNFACT         PIC 9(7).
           02 WREPONSE       PIC XXX.
       01            WENRECJ.
           02        WAPACECR.
             03      WEJCLE.
               04    WEJNAS     PIC 9(6).
               04    wejcunix   pic 9(6).
             03      WEJCVA     PIC 9.
             03      WEJAC4.
               04    WEJKLE.
                 05  WEJETS     PIC 99.
                 05  WEJGRP     PIC 9.
                 05  WEJNCPT    PIC 9(10).
                 05  WEJCDAOP   PIC 9(6).
                 05  WEJCOPDA   REDEFINES       WEJCDAOP.
                  06 WEJCDAOA   PIC 99.
                  06 WEJCDAOM   PIC 99.
                  06 WEJCDAOJ   PIC 99.
                 05  wejunix    pic 9(6).
               04    WEJTYEL    PIC 9.
               04    WEJNPCE    PIC 9(8).
               04    WEJCDSO    PIC 9.
               04    WEJDAPE    PIC 9(4).
               04    WEJPEDA    REDEFINES       WEJDAPE.
                 05  WEJDAPA    PIC 99.
                 05  WEJDAPM    PIC 99.
               04    WEJNECR    PIC 9(6).
               04    WEJCJNL    PIC 99.
               04    WEJCCDPC   PIC 99.
               04    WEJCCDBQ   PIC 9.
               04    WEJCDAEC   PIC 9(6).
               04    WEJCECDA   REDEFINES       WEJCDAEC.
                 05  WEJCDAEA   PIC 99.
                 05  WEJCDAEM   PIC 99.
                 05  WEJCDAEJ   PIC 99.
               04    WEJCCONT   PIC 9(10).
               04    WEJCMONT   PIC 9(10).
               04 WEJMONTF REDEFINES WEJCMONT PIC 9(8)V99.
               04    WEJCSIGN   PIC X.
               04    WEJCLIOP   PIC X(20).
               04    WEJCLETT.
                 05  WEJCLET1   PIC X.
                 05  WEJCLET2   PIC X.
               04    WEJCSOFA   PIC 9(10).
               04 WEJSOFAF REDEFINES WEJCSOFA PIC 9(8)V99.
               04    WEJCSIFA   PIC X.
               04    WEJCSOCF   PIC 9(10).
               04 WEJSOCFF REDEFINES WEJCSOCF PIC 9(8)V99.
               04    WEJCSICF   PIC X.
               04    WEJCMCLI   PIC 9(6).
               04    WEJCTRES   PIC 9.
               04    WEJCAGE    PIC 99.
               04    WEJCSUC    PIC 9.
               04    WEJREGL    PIC 99.
               04    WEJFIL7.
                 05  WEJREGR    PIC 9(3).
               04    WEJCSECT   PIC 9(6).
               04    WEJCLOT.
                 05  WEJCCL1    PIC 9.
                 05  WEJCCL2    PIC 9.
               04    filler     PIC XXX.
       01            WENREXC.
           02        WAPACEXC.
             03      WEXCLE.
               04    WEXETS     PIC 99.
               04    WEXGRP     PIC 9.
               04    WEXNCPT    PIC 9(10).
               04    WEXECHC    PIC 9(6).
               04    WEXCECH    REDEFINES       WEXECHC.
                 05  WEXECHA    PIC 99.
                 05  WEXECHM    PIC 99.
                 05  WEXECHJ    PIC 99.
               04    WEXTYEL    PIC 9.
               04    wexunix    pic 9(5).
             03      WEXCRTR    PIC 99.
             03      WEXNDPA    PIC 99.
             03      WEXNPCE    PIC 9(8).
             03      WEXCDSO    PIC 9.
             03      WEXDAPE    PIC 9(4).
             03      WEXPEDA    REDEFINES       WEXDAPE.
               04    WEXDAPA    PIC 99.
               04    WEXDAPM    PIC 99.
             03      WEXNECR    PIC 9(6).
             03      WEXCJNL    PIC 99.
             03      WEXCDAOP   PIC 9(6).
             03      WEXCOPDA   REDEFINES       WEXCDAOP.
               04    WEXCDAOA   PIC 99.
               04    WEXCDAOM   PIC 99.
               04    WEXCDAOJ   PIC 99.
             03      WEXCCDPC   PIC 99.
             03      WEXCCDBQ   PIC 9.
             03      WEXCDAEC   PIC 9(6).
             03      WEXCECDA   REDEFINES       WEXCDAEC.
               04    WEXCDAEA   PIC 99.
               04    WEXCDAEM   PIC 99.
               04    WEXCDAEJ   PIC 99.
             03      WEXCCONT   PIC 9(10).
             03      WEXCMONT   PIC 9(10).
             03      WEXCSIGN   PIC X.
             03      WEXCLIOP   PIC X(20).
             03      WEXCLETT.
               04    WEXCLET1   PIC X.
               04    WEXCLET2   PIC X.
             03      WEXCSOFA   PIC 9(10).
             03      WEXCSIFA   PIC X.
             03      WEXCSOCF   PIC 9(10).
             03      WEXCSICF   PIC X.
             03      WEXCMCLI   PIC 9(6).
             03      WEXCTRES   PIC 9.
             03      WEXCAGE    PIC 99.
             03      WEXCSUC    PIC 9.
             03      WEXCSECT   PIC 9(6).
             03      WEXCRI     PIC 99.
             03      WEXDAD     PIC 9(6).
             03      WEXADD     REDEFINES       WEXDAD.
               04    WEXDAA     PIC 99.
               04    WEXDAM     PIC 99.
               04    WEXDAJ     PIC 99.
             03      WEXDVR     PIC 9(6).
             03      WEXVRD     REDEFINES       WEXDVR.
               04    WEXDVA     PIC 99.
               04    WEXDVM     PIC 99.
               04    WEXDVJ     PIC 99.
             03      WEXCODG.
               04    WEXCED     PIC 9.
               04    WEXCSU     PIC 9.
               04    WEXCREM    PIC 9.
               04    WEXCREL    PIC 9.
               04    WEXDIV     PIC 9.
       01            WENRTCL.
           02        WAPTCL.
             03      WTCCLE.
               04    WTCCLP     PIC 9(6).
               04    WTCNRA     PIC 9(5).
               04    WTCNFA     PIC 9(5).
               04    WTCRAN     PIC 9.
               04    WTCTYE     PIC 9.
               04    wtcunix    pic 9(3).
             03      WTCREG     PIC 99.
             03      WTCDOP     PIC 9(6).
             03      WTCDOX     REDEFINES        WTCDOP.
               04    WTCDOA     PIC 99.
               04    WTCDOM     PIC 99.
               04    WTCDOJ     PIC 99.
             03      WTCCRE     PIC 9.
             03      WTCDEC     PIC 9(6).
             03      WTCDEX     REDEFINES        WTCDEC.
               04    WTCDEA     PIC 99.
               04    WTCDEM     PIC 99.
               04    WTCDEJ     PIC 99.
             03      WTCMEC     PIC 9(10).
             03      WTCMEX     REDEFINES        WTCMEC.
               04    WTCMED     PIC 9(8)V99.
             03      WTCSIG     PIC X.
             03      WTCCLL     PIC 9(6).
             03      WTCACT     PIC 9(10).
             03      WTCAEX     PIC 9(10).
             03      WTCXEA     REDEFINES        WTCAEX.
               04    FILLER     PIC X(6).
               04    WTCCTR     PIC S9(3)        sign leading separate.
             03      WTCT1      PIC X.
             03      WTCT2      PIC X.
      *
       01            wwxxyy.
           02        wecr       pic 9(6).
           02        wextr      pic 9(5).
           02        wtra       pic 9(3).
           02        wexcle2.
             03      wexcnt2    pic x(19).
             03      filler     pic x(6).
           02        wokda      pic xxx.
       01            wran       pic 9.
       PROCEDURE DIVISION.
      *
       TRAIT SECTION.
           call 'mmdt-envi1' using adl-art
           ACCEPT WREPONSE.
      *
       T10.
      *****OPEN INPUT FCLIENTS PARAMGPI.
      *----> M0797 (D)
      *    move 'I' to gfkey.
      *    perform op-fclients.
      *    move 'I' to gfkey.
      *    perform op-paramgpi.
      *----> M0797 (F)
      *****OPEN I-O ECRJO FFACTURE PARAM FTRAITES EXTRA PARBATCH PERCO.
           move 'W' to gfkey.
           perform op-parbatch.
           move 'W' to gfkey.
           perform op-percogr0.
      *----> M0797 (D)
      *    move 'W' to gfkey.
      *    perform op-percoel2.
      *    move 'W' to gfkey.
      *    perform op-extracpt.
      *    move 'W' to gfkey.
      *    perform op-ftraites.
      *----> M0797 (F)
           move 'W' to gfkey.
           perform op-paramcpt.
           move 'W' to gfkey.
           perform op-ffacture.
           move 'W' to gfkey.
           perform op-ecrcptjo.
           string 'ADLPID' x'00' delimited by size
                  into var-name1.
           move space to var-data1.
           call 'genvcc' using var-name1 var-data1.
           string 'factc025.'
                  var-data1 delimited by ' '
                                     into wlabel-etat1.
           OPEN OUTPUT ETAT1.
           string 'ADLPID' x'00' delimited by size
                  into var-name2.
           move space to var-data2.
           call 'genvcc' using var-name2 var-data2.
           string 'anofac25.'
                  var-data2 delimited by ' '
                                     into wlabel-etat2.
           OPEN OUTPUT ETAT2.
      *
      **** TRAITEMENT NORMAL OU REPRISE ? ****
      *
       t11.
           move 1      to wtra.
           move zeroes to wecr wextr wran.
           MOVE ZERO TO WTOP3 WTOP5 WTOP7 WPAGE WTEST.
           MOVE 90 TO WCTL.
           MOVE "UTFACTC025" TO PHCLE.
      *****READ PARBATCH INVALID GO TO INVA1.
           perform r-parbatch.
           if file-status not = zero
                 GO TO INVA1.
           IF PHBAR     = ALL "0" GO TO T20.
DD0351   perform env-mail
         if mmdt-langue = "FR"
           DISPLAY "** UT EN REPRISE SUR INCIDENT : 3 POSSIBILITES **"
           DISPLAY "           <FIN> =  ABANDON DE L'UT"
DD0351*    DISPLAY "           <OUI> =   SUITE  DE L'UT  EN  REPRISE"
DD0351*    DISPLAY "           <NON> =   SUITE  DE L'UT SANS REPRISE"
         else
           if mmdt-societe = "SLOVAQ"
              display "** PROGRAM IN ERROR : PREVENT COMPUTING SERVICE"
              display "           <FIN> =  ABANDONMENT PROGRAM"
           else
              display "** PROGRAM IN ERROR : 3 POSSIBILITIES **"
              display "<FIN> = ABANDONMENT PROGRAM"
DD0351*       DISPLAY "<OUI> = CONTINUATION OF PROGRAM IN RESUMPTION"
DD0351*       DISPLAY "<NON> = CONTINUATION PROGRAM WITHOUT RESUMPTION"
           end-if
         end-if
           .
       T15.
           ACCEPT WREPONSE.
           if mmdt-langue = "FR"
              display "REPONSE DONNEE : " wreponse
           else
              display "GIVEN ANSWER : " wreponse
           end-if
GPICMT* elgu le 18/10/09 suppression mode reprise car plus de controle sur date
GPICMT* si reprise on va en fin
DD0351*    if mmdt-societe = "SLOVAQ"
              go to fin
DD0351*    end-if
           IF WREPONSE     = "FIN" OR WREPONSE     = "fin" GO TO FIN.
           IF WREPONSE     = "OUI" OR WREPONSE     = "oui"
                              MOVE 1 TO WTOP3  GO TO REPRISEA.
           IF WREPONSE NOT = "NON" AND WREPONSE NOT = "non"
              if mmdt-langue = "FR"
                 DISPLAY
              "REPONSE : <" WREPONSE "> INCORRECTE , RE-COMMENCER"
              else
                 DISPLAY
              "ANSWER : >" WREPONSE "> INCORRECT, BEGIN AGAIN"
              end-if
              GO TO T15.
           MOVE ALL "0" TO PHBAR.
      *
      **** TRAITEMENT CONSOLE ****
      *
       T20.
DD0422*    MOVE SPACES TO WDATE.
  -      if mmdt-langue = "FR"
  -        DISPLAY "EDITION DU JOURNAL DES VENTES EN COURS, PATIENTEZ"
  -   *    DISPLAY "ENTRER DATE DE FACTURATION : <JJMMAA>"
  -      else
  -        DISPLAY "PUBLISHING OF the SALES BOOK IN PROGRESS, WAITING"
  -   *    DISPLAY "ENTER DATE OF INVOICING : <DDMMYY>"
  -      end-if
  -   *    ACCEPT WDATE.
  -   *    IF WDATE NOT NUMERIC GO TO T20.
  -   *    IF WJ < 1 OR WJ > 31 GO TO T20.
  -   *    IF WM < 1 OR WM > 12 GO TO T20.
  -   *  if mmdt-langue = "FR"
  -   *    display "DATE " wj "/" wm "/" wa " Bonne : <oui/non> ?"
  -   *  else
  -   *    display "DATE " wj "/" wm "/" wa " OK : <oui/non> ?"
  -   *  end-if
  -   *    accept wokda.
  -   *    if wokda not = "oui" and not = "OUI" go to t20.
  -   * recuperation date du jour                                       *GPICMT
  -        move 'D' to immti-date-taj
  -        call 'mmti-date1' using mmti-date adl-art
DD0422     move wmmti-date-jma to wdate
      *
      **** DEBUT TRAITEMENT ****
      *
           MOVE WDAT9 TO PHIDJO.
           MOVE ZERO TO PCETS PCGRP PCNCPT.
           move zero to pctyel.
      *****READ PERCO INVALID GO TO INVA2.
           perform r-percogr0.
           if file-status not = zero
                 GO TO INVA2.
           MOVE PCNPSJ TO WNPSJ PHINEC.
           ADD 1 TO PCNPSJ.
      *****REWRITE ENRPCT INVALID GO TO INVA3.
           perform rw-percogr0.
           if file-status not = zero
                GO TO INVA3.
      *****REWRITE ENRBGP INVALID GO TO INVA4.
           perform rw-parbatch.
           if file-status not = zero
                GO TO INVA4.
       T30.
           MOVE zeroes    TO CPCLE.
      *****START PARAM KEY IS NOT < CPCLE INVALID
      *****                DISPLAY "FICH. PARAMCPT VIDE"
      *****                GO TO FIN.
           perform snl-paramcpt.
           if file-status not = zero
              if mmdt-langue = "FR"
                 DISPLAY "FICH. PARAMCPT VIDE" GO TO FIN
              else
                 DISPLAY "PARAMCPT : EMPTY FILE" GO TO FIN
              end-if
           end-if
      *****READ PARAM NEXT AT END DISPLAY "FICH. PARAMCPT VIDE"
           perform n-paramcpt.
           if file-status not = zero
              if mmdt-langue = "FR"
                 DISPLAY "FICH. PARAMCPT VIDE"
                                  GO TO FIN
              else
                 DISPLAY "PARAMCPT : EMPTY FILE"
                                  GO TO FIN
              end-if
           end-if
           .
       T35.
           MOVE 0 TO CPMONTF (1) CPMONTA (1) CPMONTF (2) CPMONTA (2)
                     CPMONTF (3) CPMONTA (3) CPMONTF (4) CPMONTA (4).
           MOVE "0" TO CPCD1 CPCD2.
           IF CPCD3 NOT = "0" GO TO T37.
           MOVE 0    TO CPNMM CPPOM CPAFM CPTVM.
       T37.
      *****REWRITE ENRPAF INVALID GO TO INVA6.
           perform rw-paramcpt.
           if file-status not = zero
                GO TO INVA6.
      *****READ PARAM NEXT AT END GO TO T40.
           perform n-paramcpt.
           if file-status not = zero
                GO TO T40.
           GO TO T35.
      *
      ******************************************************************
      *  PHASE A : EDITION DU JOURNAL DES VENTES
      *            MAJ PARAMCPT
      *            CREATION DE L'ECRITURE COMPTABLE DANS LES ECRITURES
      *                            DU JOUR
      *            MAJ DE L'EXTRA COMPTABLE
      *            CREATION DE TRAITES
      ******************************************************************
      *
       T40.
           MOVE 0 TO WHTG WPOG WAFG WTVG WTTG WHTP WPOAFP.
           MOVE ZERO TO WTOP.
           MOVE 90 TO WCPTR.
       T40A.
           MOVE zeroes    TO FBCLE.
      *****START FFACTURE KEY file-status NOT < FBCLE INVALID
      *****                   DISPLAY "FICH. FACTURE VIDE"
      *****                   GO TO FIN.
           perform snl-ffacture.
           if file-status not = zero
              if mmdt-langue = "FR"
                 DISPLAY "FICH. FACTURE VIDE" GO TO FIN
              else
                 DISPLAY "FFACTURE EMPTY FILE" GO TO FIN
              end-if
           end-if
      *****READ FFACTURE NEXT AT END DISPLAY "FICH. FACTURE VIDE"
           perform n-ffacture.
           if file-status not = zero
              if mmdt-langue = "FR"
                DISPLAY "FICH. FACTURE VIDE"
                                     GO TO FIN
              else
                DISPLAY "FFACTURE EMPTY FILE"
                                     GO TO FIN
              end-if
           end-if
           .
       T41.
DD0422*    IF FBDAF NOT = WDAT9 GO TO T100.
           IF FBJVT NOT = ZERO AND WTOP3 = ZERO GO TO T100.
           IF WTOP5 = ZERO MOVE 1 TO WTOP5.
           MOVE FBNFA TO WNFACT.
           IF FBNEC NOT = ZERO GO TO T43.
           MOVE 1 TO FBNEC.
           MOVE FBNPF TO FBE1F.
           MOVE FBNPD TO FBE1D.
           IF FBDFM = 2 MOVE 28 TO FBD1J
             ELSE       MOVE 30 TO FBD1J.
           MOVE FBDFM TO FBD1M.
           MOVE FBDFA TO FBD1A.
       T43.
           MOVE 0 TO WHT WPOAF.
           IF FBCFA = 5 OR FBCFA = 6 OR FBCFA = 7
                        MOVE 0 TO WNEC1 WNEC2 WNEC3
                        MOVE 1 TO WCODFA
             ELSE       MOVE FBNEC TO WNEC1 WNEC2 WNEC3
                        MOVE ZERO TO WCODFA.
           IF WNEC1 = 9 MOVE 1 TO WNEC1 WNEC2 WNEC3.
      *
      **** EDITION DU JOURNAL ****
      *
       T45.
           IF WCPTR > 50 PERFORM TITRE THRU FTITRE.
           IF FBFRA  =  2   GO TO T45A.
           ADD FBHT1F FBHT2F GIVING WHT.
           SUBTRACT FBPOF FROM WHT.
           MOVE FBPOF TO WPO.
           MOVE FBAFF TO WAF.
           ADD FBTX1F FBTX2F GIVING WTV.
           GO TO T45B.
       T45A.
           ADD FBHT1F FBHT2F GIVING WHT.
           MOVE 0 TO WPO WAF.
           ADD FBTX1F FBTX2F GIVING WTV.
           ADD FBPOF FBAFF GIVING WPOAF.
           IF WPOAF = 0 OR WHT = 0 GO TO T45B.
      *
      * CALCUL %PORT
      *
           MULTIPLY 100 BY WPOAF.
           DIVIDE WPOAF BY WHT GIVING EPPO.
      *
      * CONTROL TRAVERS
      *
       T45B.
           ADD WHT WPO WAF WTV GIVING WTT.
           IF WTT NOT = FBNPF MOVE "   ERREUR TRAVERS" TO WERREUR
             ELSE             MOVE SPACES              TO WERREUR.
      *
      * EDITION LIGNE
      *
           MOVE "*" TO EAS1 EAS2 EAS3 EAS4 EAS5 EAS6 EAS7 EAS8 EAS9
                       EAS10 EAS11 EAS12.
           MOVE FBTVE TO ETA.
DD0422*    MOVE FBPMP TO EGEO.
DD0422     MOVE FBPML TO EGEO.
           MOVE FBNCP TO ECLI.
           MOVE FBNFA TO EFAC.
           IF FBTAX < 1 OR FBTAX > 3 MOVE 4 TO I
             ELSE                    MOVE FBTAX TO I.
           MOVE FBTAX TO ETAX (I).
           IF WCODFA = ZERO GO TO T45C.
           MULTIPLY WHT BY -1 GIVING EVHT.
           MULTIPLY WTV BY -1 GIVING ETVA.
           MULTIPLY WTT BY -1 GIVING ENET.
           MULTIPLY WPO BY -1 GIVING EPOR.
           MULTIPLY WAF BY -1 GIVING EAFF.
           GO TO T46.
       T45C.
           MOVE WHT TO EVHT.
           MOVE WPO TO EPOR.
           MOVE WTV TO ETVA.
           MOVE WAF TO EAFF.
           MOVE WTT TO ENET.
       T46.
           MOVE WERREUR TO EANO.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACES TO LIGNE.
           IF WERREUR NOT = SPACES GO TO T50.
      *
      * CUMUL TOTAL GENERAL
      *
           IF WCODFA NOT = ZERO GO TO T46A.
           ADD WHT TO WHTG.
           ADD WPO TO WPOG.
           ADD WAF TO WAFG.
           ADD WTV TO WTVG.
           ADD WTT TO WTTG.
           IF FBFRA NOT = 2 GO TO T50.
           ADD WHT TO WHTP.
           ADD FBPOF FBAFF TO WPOAFP.
           GO TO T50.
       T46A.
           SUBTRACT WHT FROM WHTG.
           SUBTRACT WPO FROM WPOG.
           SUBTRACT WAF FROM WAFG.
           SUBTRACT WTV FROM WTVG.
           SUBTRACT WTT FROM WTTG.
           IF FBFRA NOT = 2 GO TO T50.
           SUBTRACT WHT FROM WHTP.
           SUBTRACT FBPOF FBAFF FROM WPOAFP.
      *
      **** MAJ PARAMCPT ****
      *
       T50.
           IF FBJVT NOT = ZERO AND PHIPHA NOT = ZERO GO TO T60.
           MOVE FBTVE TO WTVE.
      *----> Ajout le 18/11/98 du test avec 4
           IF   WVT (1) NOT = 1 AND WVT (1) NOT = 2 AND WVT (1) NOT = 3
                                AND WVT (1) NOT = 4
      *----> M0100 (D)
                                AND WVT (1) NOT = 5
      *----> M0100 (F)
                                AND WVT (1) NOT = 6                     *DDE042
            AND WVT (1) NOT = 7 AND WVT (1) NOT = 8 AND WVT (1) NOT = 9
                       MOVE ZERO TO WVT (1).
           MOVE WVT (1) TO CPTARIF.
           MOVE FBTAX TO CPTAXE.
DD0448* pour code taxe 6 ==> on charge 4
DD0448* pour code taxe > 6 on charge 3, pour ne pas planter le joural des ventes, qui ne devrait plus servir, car remplace par amcp-jnlv, extraction des ca par secteur et taxe
DD0448     if CPTAXE = 6 
  -           move 4 to CPTAXE
  -        else
  -           if CPTAXE > 6
  -              move 3 to CPTAXE
  -           end-if
DD0448     end-if
  
GPICMT* pour eviter des blocages on prend le code geo du livrea
DD0422*    IF FBPMP > 2 AND FBPMP < 9 MOVE 2 TO CPPAYS
DD0422*      ELSE                     MOVE FBPMP TO CPPAYS.
DD0422     IF FBPML > 2 AND FBPML < 9 MOVE 2 TO CPPAYS
DD0422       ELSE                     MOVE FBPML TO CPPAYS.
GPICMT* GPIWARNING pour les factures avec code geo 0 on peut par exemple pour les tansporteur (marchandises abimees)
GPICMT*    faire une facture exoneree et utiliser le code 4 ==> prevoir gestion de ce cas
GPICMT*    gpi met un code taxe 3 au lieu de 4 et ca passe mais on ne devrait pas utiliser ce code 3 qui est fait
GPICMT*    pour de la suspension de taxe avec un montant ht affacte
DD0422     if mmdt-societe = 'SLOVAQ'
DD0422       if CPPAYS = 0 and CPTAXE = 4
  -           move 3 to CPTAXE
  -          end-if
  -          if CPPAYS = 1 and CPTAXE = 1
  -           move 0 to cppays
  -          end-if
DD0422     end-if
      *****READ PARAM INVALID GO TO INVA5.
           perform r-paramcpt.
           if file-status not = zero
                 GO TO INVA5.
           IF WCODFA NOT = ZERO GO TO T50A.
           ADD WHT TO CPMONTF (1).
           ADD WPO TO CPMONTF (2).
           ADD WAF TO CPMONTF (3).
           ADD WTV TO CPMONTF (4).
           GO TO T52.
       T50A.
           ADD WHT TO CPMONTA (1).
           ADD WPO TO CPMONTA (2).
           ADD WAF TO CPMONTA (3).
           ADD WTV TO CPMONTA (4).
       T52.
      *****REWRITE ENRPAF INVALID GO TO INVA6B.
           perform rw-paramcpt.
           if file-status not = zero
                GO TO INVA6B.
      *
      **** CREATION ECRITURE COMPTABLE DANS ECRITURES DU JOUR ****
      *
       T60.
           MOVE zeroes    TO WENRECJ.
           MOVE ZERO TO WEJCVA WEJGRP WEJTYEL WEJCDSO WEJCCDBQ WEJCTRES
                        WEJCSUC WEJCCL1 WEJCCL2 WEJREGR.
           MOVE SPACES TO WEJCSIGN WEJCLET1 WEJCLET2 WEJCSIFA WEJCSICF.
      *****               WEJFIL8.
           MOVE WNPSJ TO WEJNAS.
           move wecr  to wejcunix.
           add  1     to wecr.
           MOVE 3     TO WEJGRP.
           MOVE zeroes    TO WCOMPTE.
           MOVE FBNCP TO WNCL.
           MOVE WCOMPT9 TO WEJNCPT.
           MOVE 4     TO WEJTYEL.
           MOVE WA    TO WEJDAPA.
           MOVE WM    TO WEJDAPM.
           MOVE FBNFA TO WEJNECR.
           MOVE 11    TO WEJCJNL.
DD0422*    MOVE WJ    TO WEJCDAOJ.
  -   *    MOVE WM    TO WEJCDAOM.
  -   *    MOVE WA    TO WEJCDAOA.
  -        MOVE FBDFJ    TO WEJCDAOJ.
  -        MOVE FBDFM    TO WEJCDAOM.
DD0422     MOVE FBDFA    TO WEJCDAOA.
       T60A.
           EXIT.
       T61.
           IF WCODFA = ZERO MOVE 1 TO WEJCCDPC
             ELSE          MOVE 2 TO WEJCCDPC.
           IF FBDE1 NOT = ZERO MOVE FBD1J TO WEJCDAEJ
                               MOVE FBD1M TO WEJCDAEM
                               MOVE FBD1A TO WEJCDAEA
                               GO TO T62.
           IF WM = 2 MOVE 28 TO WEJCDAEJ GO TO T61A.
           IF WM = 1 OR WM = 3 OR WM = 5 OR WM = 7 OR WM = 8 OR WM = 10
             OR WM = 12    MOVE 31 TO WEJCDAEJ   GO TO T61A.
           MOVE 30 TO WEJCDAEJ.
       T61A.
           MOVE WM TO WEJCDAEM.
           MOVE WA TO WEJCDAEA.
       T62.
           MOVE WTTC TO WEJCMONT.
           MOVE WEJCDAEC TO WDATEC9.   MOVE SPACES TO WLIOP.
DD0326     IF WCODFA = ZERO MOVE "D" TO WEJCSIGN  MOVE "FACTUR"
                                                                 TO WFA
             ELSE          MOVE "C" TO WEJCSIGN  MOVE "AVOIR"   TO WFA.
           MOVE FBNFA TO WNFA.
           MOVE SPACE TO WESP.
           IF FBNCP NOT = FBNCL MOVE FBNCL TO WNCLL.
           MOVE WLIOP TO WEJCLIOP.
           MOVE WEJCMONT TO WEJCSOFA.
           MOVE WHTC    TO WEJCSOCF.
           MOVE WEJCSIGN TO WEJCSIFA WEJCSICF.
           IF FBNCL NOT = FBNCP MOVE FBNCL TO WEJCMCLI.
           MOVE FBSUC TO WEJCSUC.
           MOVE FBREG TO WEJREGL.
           MOVE WENRECJ TO ENRECJ.
      *    IF WTOP NOT = ZERO GO TO T62A.
      *****WRITE ENRECJ INVALID MOVE 1 TO WTOP8 GO TO INVA7.
      *    perform w-ecrcptjo.
      *    if file-status not = zero
      *         MOVE 1 TO WTOP8 GO TO INVA7.
           MOVE 1 TO WTOP.
      *    GO TO T65.
      *T62A.
      *****WRITE COMPL ENRECJ.
           perform w-ecrcptjo.
           IF file-status NOT = "00" MOVE 2 TO WTOP8 GO TO INVA7.
      *
      **** MAJ DE L'EXTRA COMPTABLE ****
      *
       T65.
           IF FBJVT NOT = ZERO GO TO T100.
      *----> M0797 (D)
      *    MOVE FBE1F TO WMONTECF.
      *    MOVE ZERO TO WTOP4.
      *    MOVE WDATEC9 TO WAED.
      *T70.
      *    MOVE ZERO TO EXETS.
      *    MOVE WAED TO EXECHC.
      *    MOVE 3 TO EXGRP.
      *    MOVE WCOMPT9 TO EXNCPT.
      *    move 3 to extyel.
      *    move 0 to exunix.
      *****  ajout d'une START                ***********
      *    perform snl-extracpt.
      *    if file-status not = zero GO TO crextrap.
      *****READ EXTRA INVALID GO TO CREXTRAP.
      *    perform n-extracpt.
      *    if file-status not = zero
      *          GO TO CREXTRAP.
      *    if exncpt not = wcompt9 go to crextrap.
      *    if exechc not = waed    go to crextrap.
      *    if extyel not = 3       go to crextrap.
      *    move excle  to wexcle2.
      *    MOVE ENREXC TO WENREXC.
      *    IF WTOP3 = 1 GO TO REPRIS2A.
      *T70F.
      *    IF WCODFA = ZERO MOVE "D" TO WCFA
      *      ELSE           MOVE "C" TO WCFA.
      *    MOVE EXCMONT TO WEXMT1.
      *    MOVE EXCSIGN TO WSIGN.
      *T70A.
      *    IF WSIGN NOT = WCFA GO TO T70G.
      *    ADD WMONTECC TO WEXMT1.
      *    GO TO T70I.
      *T70G.
      *    IF WMONTECC > WEXMT1 GO TO T70H.
      *    SUBTRACT WMONTECC FROM WEXMT1.
      *    GO TO T70I.
      *T70H.
      *    SUBTRACT WEXMT1 FROM WMONTECC GIVING WEXMT1.
      *    MOVE WCFA TO WSIGN.
      *T70I.
      *    EXIT.
      *T71.
      *    MOVE WEXMT1 TO EXCMONT.
      *    MOVE WSIGN TO EXCSIGN.
      *    MOVE EXCSOFA TO WEXMT1.
      *    MOVE EXCSIFA TO WSIGN.
      *    PERFORM T70A THRU T70I.
      *    MOVE WEXMT1 TO EXCSOFA.
      *    MOVE WSIGN TO EXCSIFA.
      *T72.
      *****REWRITE ENREXC INVALID GO TO INVA8.
      *    perform rw-extracpt.
      *    if file-status not = zero
      *         GO TO INVA8.
      *    GO TO T75.
      *CREXTRAP.
      *    MOVE zeroes    TO WENREXC.
      *    MOVE ZERO TO WEXGRP WEXTYEL WEXCDSO WEXCCDBQ WEXCTRES WEXCSUC
      *                 WEXCED WEXCSU WEXCREM WEXCREL WEXDIV.
      *    MOVE SPACES TO WEXCSIGN WEXCLIOP WEXCLETT WEXCSIFA WEXCSICF.
      *    MOVE 3 TO WEXGRP.
      *    MOVE WCOMPT9 TO WEXNCPT.
      *    MOVE WAED TO WEXECHC WEXCDAEC.
      *    MOVE WMONTECC TO WEXCMONT WEXCSOFA.
      *    IF WCODFA = ZERO MOVE "D" TO WEXCSIGN WEXCSIFA WEXCSICF
      *      ELSE           MOVE "C" TO WEXCSIGN WEXCSIFA WEXCSICF.
      *T72B.
      *    MOVE "ECHEANCE DU" TO WECH.
      *    MOVE "/" TO WSL1 WSL2.
      *    MOVE WAEDJ TO WEJ.
      *    MOVE WAEDM TO WEM.
      *    MOVE WAEDA TO WEA.
      *    MOVE WLIOP2 TO WEXCLIOP.
      *    MOVE FBREG TO WEXCRI.
      *T74.
      *    MOVE 3       TO WEXTYEL.
      *    move zeroes  to wexunix.
      *    move 1       to wextr.
      *    MOVE WENREXC TO ENREXC.
      *    move excle   to wexcle2.
      *****WRITE ENREXC INVALID MOVE 3 TO WTOP8 GO TO INVA9.
      *    perform w-extracpt.
      *    if file-status not = zero
      *         MOVE 3 TO WTOP8 GO TO INVA9.
      *    go to t75a.
      *T75.
      **   pour avoir dans wunix dernier no + 1 ***********
      **   rupture supposee sur 1er el = 3      ***********
      *    perform n-extracpt.
      *    if file-status not = zero GO TO T75a.
      *    if extyel not = 5 go to t75a.
      *    move exunix to wextr.
      *    add  1      to wextr.
      *    go to t75.
      *t75a.
      *    MOVE WMONTECC TO WEXCMONT WEXCSOFA.
      *    MOVE WA TO WEXDAPA.
      *    MOVE WM TO WEXDAPM.
      *    MOVE FBNFA TO WEXNECR.
      *    MOVE 11 TO WEXCJNL.
      *    MOVE WA TO WEXCDAOA.
      *    MOVE WM TO WEXCDAOM.
      *    MOVE WJ TO WEXCDAOJ.
      *    IF WCODFA = ZERO MOVE 1 TO WEXCCDPC
      *      ELSE           MOVE 2 TO WEXCCDPC.
      *    IF FBNCL NOT = FBNCP MOVE FBNCL TO WEXCMCLI.
      *    MOVE FBSUC TO WEXCSUC.
           MOVE FBREG TO WEXCRI.
      *    MOVE 5 TO WEXTYEL.
      *    move wextr to wexunix.
      *    MOVE WLIOP TO WEXCLIOP.
      *    IF WCODFA = ZERO MOVE "D" TO WEXCSIGN WEXCSIFA WEXCSICF
      *      ELSE           MOVE "C" TO WEXCSIGN WEXCSIFA WEXCSICF.
      *T76A.
      *    MOVE WENREXC TO ENREXC.
      *T76B.
      *    EXIT.
      *T76C.
      *****WRITE COMPL ENREXC.
      *    perform w-extracpt.
      *    IF file-status NOT = "00" MOVE 4 TO WTOP8 GO TO INVA9.
      *T77.
      *    SUBTRACT 1 FROM WNEC1.
      *    IF WNEC1 NOT > 1 GO TO T77B.
      *    IF WTOP4 NOT = ZERO GO TO T77A.
      *    MOVE FBD2J TO WAEDJ.
      *    MOVE FBD2M TO WAEDM.
      *    MOVE FBD2A TO WAEDA.
      *    MOVE FBE2F TO WMONTECF.
      *    MOVE 2 TO WTOP4.
      *    MOVE 3 TO WNEC1.
      *    GO TO T70.
      *T77A.
      *    MOVE FBD3J TO WAEDJ.
      *    MOVE FBD3M TO WAEDM.
      *    MOVE FBD3A TO WAEDA.
      *    MOVE FBE3F TO WMONTECF.
      *    MOVE 1 TO WNEC1.
      *    GO TO T70.
      *T77B.
      *    IF WNEC1 NOT > 0 GO TO T80.
      *    MOVE FBD2J TO WAEDJ.
      *    MOVE FBD2M TO WAEDM.
      *    MOVE FBD2A TO WAEDA.
      *    MOVE FBE2F TO WMONTECF.
      *    GO TO T70.
      *
      **** CREATION TRAITE ****
      *
       T80.
      *----> M0797 (D)
      *-- les lignes suivantes seront a supprimees jusqu'a t98
      *    IF FBREL = 1 GO TO T98.
      *    IF FBREG NOT = 6 AND FBREG NOT = 7 AND FBREG NOT = 8 AND
      *       FBREG NOT = 9 AND FBREG NOT = 12 AND FBREG NOT = 13 AND
      *       FBREG NOT = 14 GO TO T98.
      *    MOVE "REGLTCLI" TO WLIB.
      *    MOVE FBREG TO WREG.
      *    MOVE WCLEGPI TO PGCLE.
      *****READ PARAMGPI INVALID GO TO INVA10.
      *    perform r-paramgpi.
      *    if file-status not = zero
      *          GO TO INVA10.
      *    MOVE PGHACT TO WAFFCPT.
      *    MOVE PGHAEX TO WAFFECPT.
      *    MOVE PGHMMI TO WMTMINI.
      *    MOVE ZERO TO WMEMO.
      *    IF WCODFA NOT = ZERO MOVE 1 TO WMEMO GO TO T82.
      *    IF FBE1F < WMTMINI  MOVE 1 TO WMEMO GO TO T82.
      *    SUBTRACT 1 FROM WNEC2.
      *    IF WNEC2 NOT > 0 GO TO T82.
      *    IF FBE2F < WMTMINI  MOVE 1 TO WMEMO GO TO T82.
      *    SUBTRACT 1 FROM WNEC2.
      *    IF WNEC2 NOT > 0 GO TO T82.
      *    IF FBE3F < WMTMINI  MOVE 1 TO WMEMO.
      *T82.
      *    move 1      to wtra.
      *    MOVE ZERO   TO WTOP4.
      *    move zeroes to tccle.
      *    MOVE WNCL   TO TCCLP.
      *****READ FTRAITES INVALID GO TO CREATIO2.
      *    perform r-ftraites.
      *    if file-status not = zero
      *          GO TO CREATIO2.
      *    MOVE "0" TO TCT1 TCT2.
      *****REWRITE ENRTCL INVALID GO TO INVA21.
      *    perform rw-ftraites.
      *    if file-status not = zero
      *         GO TO INVA21.
      *****READ FTRAITES COMPL AT END GO TO CREATIO3.
      *    perform n-ftraites.
      *    if file-status not = zero
      *         GO TO CREATIO3.
      *    if tcclp not = wncl go to creatio3.
      *    move tcunix to wtra.
      *    add 1 to wtra.
      *    MOVE ENRTCL TO WENRTCL.
      *    IF WTOP3 = 1 GO TO REPRIS6A.
      *T82E.
      *    IF TCTYE NOT = 4  GO TO T83.
      *    IF TCT2 NOT = "0" GO TO T83.
      *    IF WMEMO NOT = ZERO GO TO T82G.
      *    IF TCT1 NOT = "1" GO TO T82F.
      *    GO TO T83.
      *T82F.
      *    MOVE 1 TO WMEMO.
      *    GO TO T83.
      *T82G.
      *    IF TCT1 NOT = "1" GO TO T83.
      *    MOVE "0" TO TCT1.
      *****REWRITE COMPL ENRTCL.
      *    perform rw-ftraites.
      *    GO TO T83.
      *T83.
      *****READ FTRAITES COMPL AT END GO TO CREATIO3.
      *    perform n-ftraites.
      *    if file-status not = zero
      *         GO TO CREATIO3.
      *    if tcclp not = wncl go to creatio3.
      *    move tcunix to wtra.
      *    add 1 to wtra.
      *    GO TO T82E.
      *CREATIO2.
      *    MOVE spaces    TO ENRTCL.
      *    move zeroes    to tccle.
      *    MOVE ZERO TO TCTYE.
      *    MOVE 0 TO TCCTR.
      *    MOVE "0" TO TCT1 TCT2.
      **   MOVE SPACES TO TCZLI.
      **   MOVE WREG TO TCREG.
      *    MOVE WNCL TO TCCLP.
      **   MOVE WNFA TO TCNFA TCNRA.
      *****WRITE ENRTCL INVALID MOVE 5 TO WTOP8 GO TO INVA12.
      *    perform w-ftraites.
      *    if file-status not = zero
      *         MOVE 5 TO WTOP8 GO TO INVA12.
      *CREATIO3.
      *    MOVE zeroes    TO WENRTCL.
      *    move zeroes    to wtccle.
      *    MOVE ZERO TO WTCTYE WTCCRE.
      *    MOVE "0" TO WTCT1 WTCT2.
      *    MOVE SPACE TO WTCSIG.
      *    MOVE FBREG TO WTCREG.
      *    MOVE WNCL TO WTCCLP.
      *    MOVE WNFA TO WTCNFA WTCNRA.
      *    MOVE WJ TO WTCDOJ.
      *    MOVE WM TO WTCDOM.
      *    MOVE WA TO WTCDOA.
      *    MOVE FBREL TO WTCCRE.
      *    IF WCODFA = ZERO MOVE "D" TO WTCSIG
      *      ELSE          MOVE "C" TO WTCSIG.
      *    MOVE FBNCL TO WTCCLL.
      *    MOVE WAFFCPT TO WTCACT.
      *    MOVE WAFFECPT TO WTCAEX.
      *    IF WMEMO = ZERO MOVE "1" TO WTCT1
      *       ELSE         MOVE "0" TO WTCT1.
      *T90.
      *    MOVE WDATEC9 TO WAED.
      *    MOVE FBE1F TO WMONTECF.
      *T95.
      *    move wran      to wtcran.
      *    MOVE 4 TO WTCTYE.
      *    MOVE WAED TO WTCDEC.
      *    MOVE WMONTECC TO WTCMEC.
      *    move wtra      to wtcunix.
      *    add  1         to wtra.
      *    MOVE WENRTCL TO ENRTCL.
      *****WRITE COMPL ENRTCL.
      *    perform w-ftraites.
      *    IF file-status NOT = "00" MOVE 6 TO WTOP8 GO TO INVA12.
      *    IF FBREG = 7 OR FBREG = 13 GO TO T95B.
      *T95A.
      *    move wran      to wtcran.
      *    MOVE 5 TO WTCTYE.
      *    move wtra    to wtcunix.
      *    add  1       to wtra.
      *    MOVE WENRTCL TO ENRTCL.
      *****WRITE COMPL ENRTCL.
      *    perform w-ftraites.
      *    IF file-status NOT = "00" MOVE 7 TO WTOP8 GO TO INVA12.
      *T95B.
      *    SUBTRACT 1 FROM WNEC3.
      *    add      1 to   wran.
      *    IF WNEC3 NOT > 1 GO TO T95D.
      *    IF WTOP4 NOT = ZERO GO TO T95C.
      *    MOVE FBD2J TO WAEDJ.
      *    MOVE FBD2M TO WAEDM.
      *    MOVE FBD2A TO WAEDA.
      *    MOVE FBE2F TO WMONTECF.
      *    MOVE 2 TO WTOP4.
      *    MOVE 3 TO WNEC3.
      *    GO TO T95.
      *T95C.
      *    MOVE FBD3J TO WAEDJ.
      *    MOVE FBD3M TO WAEDM.
      *    MOVE FBD3A TO WAEDA.
      *    MOVE FBE3F TO WMONTECF.
      *    MOVE 1 TO WNEC3.
      *    GO TO T95.
      *T95D.
      *    IF WNEC3 NOT > 0 GO TO T98.
      *    MOVE FBD2J TO WAEDJ.
      *    MOVE FBD2M TO WAEDM.
      *    MOVE FBD2A TO WAEDA.
      *    MOVE FBE2F TO WMONTECF.
      *    GO TO T95.
      *----> M0797 (F)
       T98.
           move 0 to wran.
DD0422*    MOVE 1 TO FBJVT.
DD0422     if mmdt-societe = 'SLOVAQ'
  -           MOVE 1 TO FBJVT
DD0422     end-if
      *****REWRITE ENRFAC  INVALID GO TO INVA14.
           perform rw-ffacture.
           if file-status not = zero
                GO TO INVA14.

GPICMT* si facture de cession et une seule commande associe on appelle la creation
GPICMT* de la commande fournisseur
DD0394     if fbtyp = ccmpa-tycd-typ-cession(1:1)
  -              and fbncd not = spaces
  -              and fbncd not = zero
  -           perform cre-cdefour
DD0394     end-if
           .

       T100.
      *****READ FFACTURE NEXT AT END GO TO T110.
           perform n-ffacture.
           if file-status not = zero
                GO TO T110.
           GO TO T41.
      *
      **** FIN FACTURES ****
      *
       T110.
           IF WTOP5 NOT = 1 GO TO F1.
           IF WHTP = 0 OR WPOAFP = 0 GO TO T115.
           MULTIPLY 100 BY WPOAFP.
           DIVIDE WPOAFP BY WHTP GIVING WHTP.
      *
      * MEP, ECRITURE LIGNE TOTAL
      *
       T115.
           MOVE SPACES TO LIGNE.
           WRITE LIGNE BEFORE 2.
DDE089*    MOVE "         TOTAL  GENERAL" TO ETIT2.
DDE089*    MOVE "        TOTAL  GENERAL (EUR)" TO ETIT2.
DD0351     string "        TOTAL  GENERAL (" mmdt-envi-devcpt
  -               ")" delimited size into ETIT2
           MOVE WHTG TO EVHT.
           MOVE WPOG TO EPOR.
           MOVE WHTP TO EPPO.
           MOVE WTVG TO ETVA.
           MOVE WAFG TO EAFF.
           MOVE WTTG TO ENET.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.

      *DDE089 edition des montants en francs
           MOVE "        TOTAL  GENERAL (FRF)" TO ETIT2.
DD0351     if mmdt-societe = "SLOVAQ"
  "           MOVE "        TOTAL  GENERAL (EUR)" TO ETIT2
DD0351     end-if
           move whtg  to weu-pht
           perform cal-francs
           MOVE weu-pht TO EVHT.
           move wpog  to weu-pht
           perform cal-francs
           MOVE weu-pht TO EPOR.
           move whtp  to weu-pht
           perform cal-francs
           MOVE weu-pht TO EPPO.
           move wtvg  to weu-pht
           perform cal-francs
           MOVE weu-pht TO ETVA.
           move wafg  to weu-pht
           perform cal-francs
           MOVE weu-pht TO EAFF.
           move wttg  to weu-pht
           perform cal-francs
           MOVE weu-pht TO ENET.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.

           MOVE "UTFACTC025" TO PHCLE.
      *****READ PARBATCH INVALID GO TO INVA1.
           perform r-parbatch.
           if file-status not = zero
                 GO TO INVA1.
           MOVE 1 TO PHIPHA.
      *****REWRITE ENRBGP INVALID GO TO INVA4.
           perform rw-parbatch.
           if file-status not = zero
                GO TO INVA4.
           GO TO T120.
       TITRE.
           MOVE SPACES TO LIGNE.
           WRITE LIGNE BEFORE PAGE.

DD0370*       ajout ligne titre
           move wlabel-etat1 to immlp-titr-etat
           move wnom-prog   to immlp-titr-prog
           call 'mmlp-titr1' using mmlp-titr adl-art
           move ommlp-titr-titr to ligne
           write LIGNE BEFORE 1
           move spaces to ligne

           MOVE "J O U R N A L   D E S   V E N T E S   DU" TO ETIT1.
           MOVE "/" TO ESL1 ESL2.
           MOVE WJ TO EJ1.
           MOVE WM TO EM1.
           MOVE WA TO EA1.
           MOVE "PAGE :" TO EPAG.
           ADD 1 TO WPAGE.
           MOVE WPAGE TO ENPA.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE ALL "*" TO LIGNE.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE "* CODE* NUMERO *NUMERO *CODE*   VENTES    *   PORT OU
      -    " *      *  MONTANT    *AFFRANCHISSE.*    NET  A   *
      -    "            *" TO LIGNE.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE "*TARIF* CLIENT *FACTURE*TAXE* HORS-TAXES  * EMBALLAGE
      -    " *% PORT*   T-V-A     *   POSTAL    *    PAYER    *   A N O
      -    "M A L I E S *" TO LIGNE.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE ALL "*" TO LIGNE.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE ZERO TO WCPTR.
       FTITRE.
           EXIT.
      *
      ******************************************************************
      *  PHASE B : EDITION DU RECAPITULATIF DES VENTES
      *            CREATION ECRITURE COMPTABLE CONTRE PARTIE
      ******************************************************************
      *
       T120.
           MOVE 0 TO WHTREC1 (1)  WHTREC1 (2) WHTREC1 (3) WHTREC1 (4)
      *----> M1198 (D)
      *              WHTREC1 (5)  WHTREC1 (6) WHTREC1 (7).
      *----> M0699 (D)
      *              WHTREC1 (5)  WHTREC1 (6) WHTREC1 (7) whtrec1 (8).
                     WHTREC1 (5)  WHTREC1 (6) WHTREC1 (7) whtrec1 (8)
                     whtrec1(9) whtrec1(10).                            *DDE042
      *----> M0699 (F)

      *----> M1198 (F)
           MOVE 0 TO WHTREC2 (1)  WHTREC2 (2) WHTREC2 (3) WHTREC2 (4)
                     WHTREC2 (5).
           MOVE 0 TO WHTREC3 (1)  WHTREC3 (2) WHTREC3 (3) WHTREC3 (4).
           MOVE 0 TO WHTREC4 (1,1)  WHTREC4 (1,2) WHTREC4 (1,3)
                     WHTREC4 (1,4)  WHTREC4 (1,5).
           MOVE 0 TO WHTREC4 (2,1)  WHTREC4 (2,2) WHTREC4 (2,3)
                     WHTREC4 (2,4)  WHTREC4 (2,5).
           MOVE 0 TO WHTREC4 (3,1)  WHTREC4 (3,2) WHTREC4 (3,3)
                     WHTREC4 (3,4)  WHTREC4 (3,5).
           MOVE 0 TO WHTREC4 (4,1)  WHTREC4 (4,2) WHTREC4 (4,3)
                     WHTREC4 (4,4)  WHTREC4 (4,5).
           MOVE 0 TO WHTREC4 (5,1)  WHTREC4 (5,2) WHTREC4 (5,3)
                     WHTREC4 (5,4)  WHTREC4 (5,5).
           MOVE 0 TO WHTREC4 (6,1)  WHTREC4 (6,2) WHTREC4 (6,3)
                     WHTREC4 (6,4)  WHTREC4 (6,5).
           MOVE 0 TO WHTREC4 (7,1)  WHTREC4 (7,2) WHTREC4 (7,3)
                     WHTREC4 (7,4)  WHTREC4 (7,5).
      *----> M1198 (D)
           MOVE 0 TO WHTREC4 (8,1)  WHTREC4 (8,2) WHTREC4 (8,3)
                     WHTREC4 (8,4)  WHTREC4 (8,5).
      *----> M1198 (F)

      *----> M0699 (D)
           MOVE 0 TO WHTREC4 (9,1)  WHTREC4 (9,2) WHTREC4 (9,3)
                     WHTREC4 (9,4)  WHTREC4 (9,5).
      *----> M0699 (F)

           MOVE 0 TO WHTREC4 (10,1)  WHTREC4 (10,2) WHTREC4 (10,3)      *DDE042
                     WHTREC4 (10,4)  WHTREC4 (10,5).                    *DDE042

           MOVE 0 TO WPOREC4 (1,1) WPOREC4 (1,2) WPOREC4 (1,3)
                     WPOREC4 (1,4) WPOREC4 (1,5).
           MOVE 0 TO WPOREC4 (2,1) WPOREC4 (2,2) WPOREC4 (2,3)
                     WPOREC4 (2,4) WPOREC4 (2,5).
           MOVE 0 TO WPOREC4 (3,1) WPOREC4 (3,2) WPOREC4 (3,3)
                     WPOREC4 (3,4) WPOREC4 (3,5).
           MOVE 0 TO WPOREC4 (4,1) WPOREC4 (4,2) WPOREC4 (4,3)
                     WPOREC4 (4,4) WPOREC4 (4,5).
           MOVE 0 TO WPOREC4 (5,1) WPOREC4 (5,2) WPOREC4 (5,3)
                     WPOREC4 (5,4) WPOREC4 (5,5).
           MOVE 0 TO WPOREC4 (6,1) WPOREC4 (6,2) WPOREC4 (6,3)
                     WPOREC4 (6,4) WPOREC4 (6,5).
           MOVE 0 TO WPOREC4 (7,1) WPOREC4 (7,2) WPOREC4 (7,3)
                     WPOREC4 (7,4) WPOREC4 (7,5).
      *----> M1198 (D)
           MOVE 0 TO WPOREC4 (8,1) WPOREC4 (8,2) WPOREC4 (8,3)
                     WPOREC4 (8,4) WPOREC4 (8,5).
      *----> M1198 (F)

      *----> M0699 (D)
           MOVE 0 TO WPOREC4 (9,1) WPOREC4 (9,2) WPOREC4 (9,3)
                     WPOREC4 (9,4) WPOREC4 (9,5).
      *----> M0699 (F)

           MOVE 0 TO WPOREC4 (10,1) WPOREC4 (10,2) WPOREC4 (10,3)       *DDE042
                     WPOREC4 (10,4) WPOREC4 (10,5).                     *DDE042

           PERFORM T30.
           MOVE 90 TO WCPTR.
           MOVE ZERO TO WPAGE.
       T120A.
           IF CPMONTF (1) = 0 AND CPMONTA (1) = 0 AND CPMONTF (2) = 0
            AND CPMONTA (2) = 0 AND CPMONTF (3) = 0 AND CPMONTA (3) = 0
            AND CPMONTF (4) = 0 AND CPMONTA (4) = 0 GO TO T140.
      *
      * MEP, ECRITURE LIGNE
      *
           IF WCPTR > 50 PERFORM TITR2 THRU FTITR2.
       T121.
           MOVE "*" TO EBS1 EBS2 EBS3 EBS4 EBS (1) EBS (2) EBS (3)
                       EBS (4).
           MOVE CPTARIF TO ETARB.
           MOVE CPTAXE TO ETAXB.
           MOVE CPPAYS TO EPAYB.
           MOVE ZERO TO I.
       T122.
           ADD 1 TO I.
           IF I > 4 GO TO T125.
           MOVE I TO J.
           IF I = 3 MOVE 4 TO J.
           IF I = 4 MOVE 3 TO J.
           IF CPMONTF (I) = ZERO AND CPMONTA (I) = ZERO GO TO T122.
           IF CPMONTA (I) > CPMONTF (I) GO TO T122A.
           SUBTRACT CPMONTA (I) FROM CPMONTF (I) GIVING EMTF (J).
           GO TO T122.
       T122A.
           SUBTRACT CPMONTF (I) FROM CPMONTA (I) GIVING EMTA (J).
           GO TO T122.
       T125.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACES TO LIGNE.
           PERFORM T121.
       T125A.
           ADD 1 TO I.
           IF I > 4 GO TO T128.
           MOVE I TO J.
           IF I = 3 MOVE 4 TO J.
           IF I = 4 MOVE 3 TO J.
           MOVE CPMONTF (I) TO EMTF (J).
           MOVE CPMONTA (I) TO EMTA (J).
           GO TO T125A.
       T128.
           WRITE LIGNE BEFORE 2.
           ADD 2 TO WCPTR.
           MOVE SPACES TO LIGNE.
      *
      * PREPARATION RECAP. VENTES
      *
           IF CPTARIF = ZERO MOVE 1 TO L GO TO T129.
           IF CPTARIF = 1    MOVE 2 TO L GO TO T129.
           IF CPTARIF = 2    MOVE 3 TO L GO TO T129.
GPICMT* code tarif 7 = MDD ==> GP, 8=ISRUB
DD0351*    IF CPTARIF = 7    MOVE 4 TO L GO TO T129.
DD0351*    IF CPTARIF = 8    MOVE 5 TO L GO TO T129.
DD0351     IF CPTARIF = 7    MOVE 1 TO L GO TO T129.
DD0351     IF CPTARIF = 8    MOVE 4 TO L GO TO T129.
           IF CPTARIF = 9    MOVE 6 TO L GO TO T129.
           IF CPTARIF = 3    MOVE 7 TO L GO TO T129.
      *----> M1198 (D)
           IF CPTARIF = 4    MOVE 8 TO L GO TO T129.
      *----> M1198 (F)

      *----> M0699 (D)
           IF CPTARIF = 5    MOVE 9 TO L GO TO T129.
      *----> M0699 (F)

           IF CPTARIF = 6    MOVE 10 TO L GO TO T129.                   *DDE042

       T129.
           MOVE CPTAXE TO J.
           IF J < 1 OR J > 4 MOVE 5 TO J.
           IF CPPAYS = ZERO MOVE 1 TO K GO TO T129A.
           IF CPPAYS = 1    MOVE 2 TO K GO TO T129A.
           IF CPPAYS = 9    MOVE 4 TO K GO TO T129A.
                            MOVE 3 TO K.
       T129A.
           IF K NOT = 1 GO TO T129B.
           IF J NOT = 1 AND J NOT = 2 MOVE 2 TO M
             ELSE                     MOVE 1 TO M.
           GO TO T129C.
       T129B.
           IF K = 2 MOVE 3 TO M GO TO T129C.
           IF K = 4 MOVE 5 TO M GO TO T129C.
                    MOVE 4 TO M.
       T129C.
           IF CPMONTF (1) NOT = 0 ADD CPMONTF (1) TO WHTREC1 (L)
                   WHTREC2 (J) WHTREC3 (K) WHTREC4 (L,M).
           IF CPMONTA (1) NOT = 0 SUBTRACT CPMONTA (1) FROM WHTREC1 (L)
                   WHTREC2 (J) WHTREC3 (K) WHTREC4 (L,M).
           IF CPMONTF (2) NOT = 0 ADD CPMONTF (2) TO WPOREC4 (L,M).
           IF CPMONTA (2) NOT = 0 SUBTRACT CPMONTA (2) FROM
                     WPOREC4 (L,M).
           MOVE ZERO TO I.
           GO TO T130A.
      *
      **** CREATION ECRITURE COMPTABLE ****
      *
       T130.
           PERFORM T60 THRU T60A.
           MOVE 1 TO WEJGRP.
           MOVE CPCOMPTE (I) TO WEJNCPT.
           MOVE ZERO TO WEJNECR.
           MOVE "/" TO WSL1 WSL2.
           MOVE WJ TO WEJ.
           MOVE WM TO WEM.
           MOVE WA TO WEA.
       T130A.
           ADD 1 TO I.
           IF I > 4 GO TO T135.
           IF CPMONTF (I) = 0 GO TO T130B.
           PERFORM T130.
           MOVE "FACTURES DU" TO WECH.
           MOVE WLIOP2 TO WEJCLIOP.
           MOVE 1 TO WEJCCDPC.
           MOVE CPMONTF (I) TO WEJMONTF WEJSOFAF WEJSOCFF.
           MOVE "C" TO WEJCSIGN WEJCSIFA WEJCSICF.
      * CAS GENERAL = POSITIF   SINON SIGNE INVERSE
           IF CPMONTF (I) < 0  MOVE "D" TO WEJCSIGN WEJCSIFA WEJCSICF.
           MOVE WENRECJ TO ENRECJ.
      *****WRITE COMPL ENRECJ.
           perform w-ecrcptjo.
           IF file-status NOT = "00" MOVE 8 TO WTOP8 GO TO INVA7.
       T130B.
           IF CPMONTA (I) = 0 GO TO T130A.
           PERFORM T130.
           MOVE "AVOIRS DU" TO WECH.
           MOVE WLIOP2 TO WEJCLIOP.
           MOVE 2 TO WEJCCDPC.
           MOVE CPMONTA (I) TO WEJMONTF WEJSOFAF WEJSOCFF.
           MOVE "D" TO WEJCSIGN WEJCSIFA WEJCSICF.
      * CAS GENERAL = POSITIF   SINON SIGNE INVERSE
           IF CPMONTA (I) < 0  MOVE "C" TO WEJCSIGN WEJCSIFA WEJCSICF.
           MOVE WENRECJ TO ENRECJ.
      *****WRITE COMPL ENRECJ.
           perform w-ecrcptjo.
           IF file-status NOT = "00" MOVE 9 TO WTOP8 GO TO INVA7.
           GO TO T130A.
       T135.
           MOVE "1" TO CPCD1.
      *****REWRITE ENRPAF INVALID GO TO INVA6C.
           perform rw-paramcpt.
           if file-status not = zero
                GO TO INVA6C.
      *
      * FIN PARAMCPT
      *
       T140.
      *****READ PARAM NEXT AT END GO TO T145.
           perform n-paramcpt.
           if file-status not = zero
                GO TO T145.
           GO TO T120A.
       TITR2.
           MOVE SPACES TO LIGNE.
           WRITE LIGNE BEFORE PAGE.

DD0351*       ajout ligne titre
           move wlabel-etat1 to immlp-titr-etat
           move wnom-prog   to immlp-titr-prog
           call 'mmlp-titr1' using mmlp-titr adl-art
           move ommlp-titr-titr to ligne
           write LIGNE BEFORE 1
           move spaces to ligne

           MOVE "      I M P U T A T I O N   D E S   V E N T E S   :
      -    "E T A T   R E C A P I T U L A T I F   D U" TO ETIT3.
           MOVE "/" TO ESL3 ESL4.
           MOVE WJ TO EJ2.
           MOVE WM TO EM2.
           MOVE WA TO EA2.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE ALL "*" TO L31.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE "* CODE*CODE*CODE*    VENTES  HORS-TAXES   *         P O
      -    " R T         *        T - V - A        *  AFFRANCHISSEMENT
      -    "     *" TO L31.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE "*TARIF*TAXE*PAYS* FACTURES   -  AVOIRS    * FACTURES
      -    "-  AVOIRS    * FACTURES   -  AVOIRS    * FACTURES   -  AVOIR
      -    "S    *" TO L31.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE ALL "*" TO L31.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE ZERO TO WCPTR.
       FTITR2.
           EXIT.
      *
      **** EDITION RECAP DE VENTES ****
      *
       T145.
           MOVE SPACES TO LIGNE.
           WRITE LIGNE BEFORE PAGE.

DD0351*       ajout ligne titre
           move wlabel-etat1 to immlp-titr-etat
           move wnom-prog   to immlp-titr-prog
           call 'mmlp-titr1' using mmlp-titr adl-art
           move ommlp-titr-titr to ligne
           write LIGNE BEFORE 1
           move spaces to ligne

           MOVE "      I M P U T A T I O N   D E S   V E N T E S   :   E
      -    " T A T   R E C A P I T U L A T I F   D U :" TO ETIT3.
           MOVE "/" TO ESL3 ESL4.
           MOVE WJ TO EJ2.
           MOVE WM TO EM2.
           MOVE WA TO EA2.
           WRITE LIGNE BEFORE 3.
           MOVE SPACES TO LIGNE.
           MOVE "NON   AFFECTES :" TO ELTAR.
           MOVE WHTREC1 (1) TO EHTTA.
           MOVE "    TAXE 1 :" TO ELTAX.
           MOVE WHTREC2 (1) TO EHTTAX.
           MOVE "F R A N C E         :" TO ELPAY.
           MOVE WHTREC3 (1) TO EHTPAY.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE "  GRAND PUBLIC :" TO ELTAR.
           MOVE WHTREC1 (2) TO EHTTA.
           MOVE "    TAXE 2 :" TO ELTAX.
           MOVE WHTREC2 (2) TO EHTTAX.
           MOVE "MARCHE COMMUN (CEE) :" TO ELPAY.
           MOVE WHTREC3 (2) TO EHTPAY.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE "     DAISIF/BSR:" TO ELTAR.
           MOVE WHTREC1 (10) TO EHTTA.
           MOVE "    TAXE 3 :" TO ELTAX.
           MOVE WHTREC2 (3) TO EHTTAX.
           MOVE "AUTRES  P A Y S     :" TO ELPAY.
           MOVE WHTREC3 (3) TO EHTPAY.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE "ADHESIF  VRAC  :" TO ELTAR.
           MOVE WHTREC1 (7) TO EHTTA.
      *----> M0699 (D)
      *    MOVE "    TAXE 3 :" TO ELTAX.
      *    MOVE WHTREC2 (3) TO EHTTAX.
      *    MOVE "AUTRES  P A Y S     :" TO ELPAY.
      *    MOVE WHTREC3 (3) TO EHTPAY.
           MOVE "    TAXE 4 :" TO ELTAX.
           MOVE WHTREC2 (4) TO EHTTAX.
           MOVE "  Z O N E     FRANC :" TO ELPAY.
           MOVE WHTREC3 (4) TO EHTPAY.
      *----> M0699 (F)
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
DDE338     MOVE "     ISORUB    :" TO ELTAR.
           MOVE WHTREC1 (4) TO EHTTA.
      *----> M0699 (D)
      *    MOVE "    TAXE 4 :" TO ELTAX.
      *    MOVE WHTREC2 (4) TO EHTTAX.
      *    MOVE "  Z O N E     FRANC :" TO ELPAY.
      *    MOVE WHTREC3 (4) TO EHTPAY.
           MOVE "AUTRE TAXE :" TO ELTAX.
           MOVE WHTREC2 (5) TO EHTTAX.
      *----> M0699 (F)
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE "ADH PROFESSION.:" TO ELTAR.
           MOVE WHTREC1 (9) TO EHTTA.
      *----> M1198 (D)
      *    MOVE "    TAXE 4 :" TO ELTAX.
      *    MOVE WHTREC2 (4) TO EHTTAX.
      *    MOVE "  Z O N E     FRANC :" TO ELPAY.
      *    MOVE WHTREC3 (4) TO EHTPAY.
      *----> M0699 (D)
      *    MOVE "AUTRE TAXE :" TO ELTAX.
      *    MOVE WHTREC2 (5) TO EHTTAX.
      *----> M0699 (F)

      *----> M1198 (F)
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE "  INDUSTRIE    :" TO ELTAR.
           MOVE WHTREC1 (3) TO EHTTA.
      *----> M1198 (D)
      *    MOVE "AUTRE TAXE :" TO ELTAX.
      *    MOVE WHTREC2 (5) TO EHTTAX.
      *----> M1198 (F)
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.

           move "ADH INDUSTRIE  :" to eltar.
           MOVE WHTREC1 (8) TO EHTTA.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.

           MOVE "USINE JUZIERS  :" TO ELTAR.
           MOVE WHTREC1 (5) TO EHTTA.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE "   LEAU        :" TO ELTAR.
           MOVE WHTREC1 (6) TO EHTTA.
           WRITE LIGNE BEFORE 3.
           MOVE SPACES TO LIGNE.
           MOVE "           V E N T I L A T I O N   D U   H O R S - T A
      -    "X E" TO ELIB.
       T146.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE "     F R A N C E       MARCHE COMMUN              AUTRE
      -    "S  PAYS              ZONE   FRANC" TO ELIB.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE "(T-V-A)    (SUSPENSION)" TO ELIB.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           MOVE "NON   AFFECTES :" TO ELTAR6.
           MOVE 1 TO I.
       T147A.
           MOVE WHTREC4 (I,1) TO EHTTVA.
           MOVE WHTREC4 (I,2) TO EHTSUS.
           MOVE WHTREC4 (I,3) TO EHTMCO.
           MOVE WHTREC4 (I,4) TO EHTETR.
           MOVE WHTREC4 (I,5) TO EHTFRA.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
       T147B.
           MOVE "  GRAND PUBLIC :" TO ELTAR6.
           MOVE 2 TO I.
           PERFORM T147A.
           MOVE "     DAISIF/BSR:" TO ELTAR6.
           MOVE 10 TO I.
           PERFORM T147A.
           MOVE "ARTICLES VRAC  :" TO ELTAR6.
           MOVE 7 TO I.
           PERFORM T147A.
DDE338     MOVE "     ISORUB    :" TO ELTAR6.
           MOVE 4 TO I.
           PERFORM T147A.
           MOVE "ADH PROFESSION.:" TO ELTAR6.
           MOVE 9 TO I.
           PERFORM T147A.

           MOVE "  INDUSTRIE    :" TO ELTAR6.
           MOVE 3 TO I.
           PERFORM T147A.
      *----> M1198 (D)
           MOVE "ADH  INDUSTRIE :" TO ELTAR6.
           MOVE 8 TO I.
           PERFORM T147A.
      *----> M1198 (F)
           MOVE "USINE JUZIERS  :" TO ELTAR6.
           MOVE 5 TO I.
           PERFORM T147A.
           MOVE "   LEAU        :" TO ELTAR6.
           MOVE 6 TO I.
           PERFORM T147A.
           WRITE LIGNE BEFORE 3.
           MOVE SPACES TO LIGNE.
           MOVE "           V E N T I L A T I O N   D U   P O R T" TO
                          ELIB.
           PERFORM T146.
       T147C.
           MOVE WPOREC4 (I,1) TO EHTTVA.
           MOVE WPOREC4 (I,2) TO EHTSUS.
           MOVE WPOREC4 (I,3) TO EHTMCO.
           MOVE WPOREC4 (I,4) TO EHTETR.
           MOVE WPOREC4 (I,5) TO EHTFRA.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
       T147D.
           MOVE "  GRAND PUBLIC :" TO ELTAR6.
           MOVE 2 TO I.
           PERFORM T147C.
           MOVE "     DAISIF/BSR:" TO ELTAR6.
           MOVE 10 TO I.
           PERFORM T147C.
           MOVE "ARTICLES VRAC  :" TO ELTAR6.
           MOVE 7 TO I.
           PERFORM T147C.
DDE338     MOVE "     ISORUB    :" TO ELTAR6.
           MOVE 4 TO I.
           PERFORM T147C.
           MOVE "ADH PROFESSION.:" TO ELTAR6.
           MOVE 9 TO I.
           PERFORM T147C.

           MOVE "  INDUSTRIE    :" TO ELTAR6.
           MOVE 3 TO I.
           PERFORM T147C.

      *----> M1198 (D)
           MOVE "ADH  INDUSTRIE :" TO ELTAR6.
           MOVE 8 TO I.
           PERFORM T147C.
      *----> M1198 (F)
           MOVE "USINE JUZIERS  :" TO ELTAR6.
           MOVE 5 TO I.
           PERFORM T147C.
           MOVE "   LEAU        :" TO ELTAR6.
           MOVE 6 TO I.
           PERFORM T147C.
           WRITE LIGNE BEFORE PAGE.
           CLOSE ETAT1.
           MOVE 1 TO WTOP7.
      *
      **** FIN PHASE B ****
      *
           MOVE "UTFACTC025" TO PHCLE.
      *****READ PARBATCH INVALID GO TO INVA1.
           perform r-parbatch.
           if file-status not = zero
                 GO TO INVA1.
           MOVE 1 TO PHIPHB.
      *****REWRITE ENRBGP INVALID GO TO INVA4.
           perform rw-parbatch.
           if file-status not = zero
                GO TO INVA4.
      *
      ******************************************************************
      *  PHASE C : MAJ ZONES MOIS DANS PARAMCPT
      ******************************************************************
      *
       T150.
           PERFORM T30.
       T150A.
           MOVE ZERO TO I.
       T150B.
           ADD 1 TO I.
           IF I > 4 GO TO T152.
           SUBTRACT CPMONTA (I) FROM CPMONTF (I) GIVING WRESU (I).
           GO TO T150B.
       T152.
           ADD WRESU (1) TO CPNMM.
           ADD WRESU (2) TO CPPOM.
           ADD WRESU (3) TO CPAFM.
           ADD WRESU (4) TO CPTVM.
           MOVE "1" TO CPCD2 CPCD3.
      *****REWRITE ENRPAF INVALID GO TO INVA6D.
           perform rw-paramcpt.
           if file-status not = zero
                GO TO INVA6D.
      *****READ PARAM NEXT AT END GO TO T155.
           perform n-paramcpt.
           if file-status not = zero
                GO TO T155.
           GO TO T150A.
      *
      **** FIN PHASE C ****
      *
       T155.
           MOVE "UTFACTC025" TO PHCLE.
      *****READ PARBATCH INVALID GO TO INVA1.
           perform r-parbatch.
           if file-status not = zero
                 GO TO INVA1.
           MOVE 1 TO PHIPHC.
      *****REWRITE ENRBGP INVALID GO TO INVA4.
           perform rw-parbatch.
           if file-status not = zero
                GO TO INVA4.
      *
      ******************************************************************
      *  PHASE D : CONTROLE DEBITS = CREDITS DES ECRITURES COMPTABLES
      *            MAJ CENTRALISATEUR DEBIT ET CREDIT DE PERCOMPT
      ******************************************************************
      *
       T160.
           MOVE 0 TO WMONTC WMONTD.
           move zeroes to ejcle.
           MOVE WNPSJ TO EJNAS.
      *****READ ECRJO INVALID GO TO INVA16.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO INVA16.
       T161.
           IF EJCSIGN = "C" ADD EJCMONT TO WMONTCC
             ELSE           ADD EJCMONT TO WMONTDC.
      *****READ ECRJO COMPL AT END GO TO T162.
           perform n-ecrcptjo.
           if file-status not = zero
                GO TO T162.
           if ejnas not = wnpsj go to t162.
           GO TO T161.
       T162.
           EXIT.
       T162A.
           IF WMONTC NOT = WMONTD GO TO ERREUR.
      *
      **** MAJ PERCOMPT ****
      *
       T163.
           MOVE zeroes    TO PCCLE.
           MOVE ZERO      TO PCGRP.
           MOVE zeroes    TO WCOMPJ.
           MOVE 11        TO WJNL.
           MOVE WCOMPTJ9  TO PCNCPT.
           move 1         to pctyel.
       T163A.
      *****READ PERCO INVALID GO TO INVA15.
           perform r-percogr0.
           if file-status not = zero
                 GO TO INVA15.
           IF PCTYEL NOT = 1 GO TO INVA15B.
           ADD WMONTCC TO PCCCR.
           ADD WMONTDC TO PCCDT.
      *****REWRITE ENRPCT INVALID GO TO INVA3B.
           perform rw-percogr0.
           if file-status not = zero
                GO TO INVA3B.
      *
      **** FIN PHASE D ****
      *
       T165.
           MOVE "UTFACTC025" TO PHCLE.
      *****READ PARBATCH INVALID GO TO INVA1.
           perform r-parbatch.
           if file-status not = zero
                 GO TO INVA1.
           MOVE 1 TO PHIPHD.
      *****REWRITE ENRBGP INVALID GO TO INVA4.
           perform rw-parbatch.
           if file-status not = zero
                GO TO INVA4.
      *
      ******************************************************************
      *  PHASE E : MAJ SOLDE CONSULTATION ET CA DU PERCOMPT
      *            MAJ SOLDE CONSULTATION DES ECRITURES DU JOUR
      ******************************************************************
      *
       T170.
           MOVE ZERO TO WTOP6.
           move zeroes to ejcle.
           MOVE WNPSJ TO EJNAS.
      *****READ ECRJO INVALID GO TO INVA16B.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO INVA16B.
      *****READ ECRJO COMPL AT END GO TO T178.
           perform n-ecrcptjo.
           if file-status not = zero
                GO TO T178.
           if ejnas not = wnpsj go to t178.
      *
      **** MAJ PERCOMPT ****
      *
      *----> M0797 (D)
      *T170A.
      *    MOVE ZERO    TO WTOP2.
      *    MOVE EJETS4  TO PCETS2.
      *    MOVE EJGRP4  TO PCGRP2.
      *    MOVE EJNCPT4 TO PCNCPT2.
      *T170AA.
      *****READ PERCO INVALID GO TO CREAP2.
      *    perform r-percoel2.
      *    if file-status not = zero
      *          GO TO CREAP2.
      *T170B.
      *    IF PCGRP2 NOT = 3 GO TO T172.
      *    IF EJCSUC > PCCSCL MOVE EJCSUC TO PCCSCL.
      *    IF EJCSICF NOT = PCSIM1 GO TO T170C.
      *    ADD EJCSOCF TO PCSOM1.
      *    GO TO T172.
      *T170C.
      *    IF PCSOM1 > EJCSOCF SUBTRACT EJCSOCF FROM PCSOM1
      *      ELSE              SUBTRACT PCSOM1  FROM EJCSOCF
      *                                 GIVING PCSOM1
      *                        MOVE EJCSICF TO PCSIM1.
      *T172.
      *    IF EJCSIGN NOT = PCSICO GO TO T172A.
      *    ADD EJCMONT TO PCSOCO.
      *    GO TO T175.
      *T172A.
      *    IF PCSOCO > EJCMONT SUBTRACT EJCMONT FROM PCSOCO
      *      ELSE              SUBTRACT PCSOCO  FROM EJCMONT
      *                                 GIVING PCSOCO
      *                        MOVE EJCSIGN TO PCSICO.
      *T175.
      *    ADD 1 TO PCCCON.
      *    IF WTOP2 = ZERO GO TO T175A.
      *****WRITE ENRPCT INVALID MOVE 10 TO WTOP8 GO TO INVA18.
      *    perform w-percoel2.
      *    if file-status not = zero
      *         MOVE 10 TO WTOP8 GO TO INVA18.
      *    GO TO T175B.
      *T175A.
      *****REWRITE ENRPCT INVALID GO TO INVA3C.
      *    perform rw-percoel2.
      *    if file-status not = zero
      *         GO TO INVA3C.
      *----> M0797 (F)
      *
      **** MAJ ECRITURES DU JOUR ****
      *
       T175B.
      **** On a le CA HT de la facture dans EJCSOCF
      **** On l'y laisse pour la nouvelle compta (7/97)
           IF EJGRP4 NOT = 3 GO TO T175C.
      *    MOVE EJCMONT TO EJCSOCF.
      *    MOVE EJCSIGN TO EJCSICF.
       T175C.
           EXIT.
       T176.
           MOVE 1 TO EJCCL1 EJCVA.
           IF WTOP6 = ZERO GO TO T176A.
      *****REWRITE ENRECJ INVALID GO TO INVA17.
           perform rw-ecrcptjo.
           if file-status not = zero
                GO TO INVA17.
           GO TO T180.
       T176A.
      *****REWRITE COMPL ENRECJ.
           perform rw-ecrcptjo.
           if file-status not = zero
              if mmdt-langue = "FR"
                   display "REWR.INV/EC ECRJ : NON PREVU"
                   GO TO INVA17
              else
                   display "REWRITING ECRCPTJO KO"
                   GO TO INVA17
              end-if
           end-if
           .
       T176B.
      *****READ ECRJO COMPL AT END GO TO T178.
           perform n-ecrcptjo.
           if file-status not = zero
                GO TO T178.
           if ejnas not = wnpsj go to t178.
      *----> M0797 (D)
      *    GO TO T170A.
           go to t175b.
      *----> M0797 (F)
       T178.
           move zeroes to ejcle.
           MOVE WNPSJ  TO EJNAS.
      *****READ ECRJO INVALID GO TO INVA16C.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO INVA16C.
           MOVE 1 TO WTOP6.
      *----> M0797 (D)
      *    PERFORM T170A.
      *****READ PERCO INVALID GO TO T179.
      *    perform r-percoel2.
      *    if file-status not = zero
      *          GO TO T179.
      *    GO TO T170B.
      *T179.
      *    PERFORM CREAP2.
      *    IF EJGRP4 NOT = 3 MOVE "CREE PAR ORDINATEUR" TO PCLIBE
      *                     GO TO T170B.
      *****READ FCLIENTS INVALID GO TO INVA19.
      *    perform r-fclients.
      *    if file-status not = zero
      *          GO TO INVA19.
      *    GO TO CREAP22.
      *CREAP2.
      *    MOVE 1 TO WTOP2.
      *    MOVE zeroes    TO ENRPCT2.
      *    MOVE ZERO TO PCGRP2 PCTYEL2 PCCHIS PCCCEN PCTYCPT PCGRAT
      *                 PCRAAT  PCNRAT PCCSCL PCCVCL.
      *    MOVE SPACES TO PCSIAT PCFRAT PCSIM2 PCSIM1 PCSIMO PCSICO
      *                   PCLETTR PCLIBE PCFIL6.
      *    MOVE 0 TO PCCCON.
      *    MOVE EJGRP4  TO PCGRP2.
      *    MOVE EJETS4  TO PCETS2.
      *    MOVE EJNCPT4 TO PCNCPT2.
      *    MOVE 2       TO PCTYEL2.
      *    MOVE EJNCPT4 TO WCOMPT9.
      *    MOVE WNCL    TO CLNCL.
      *    MOVE "D"     TO PCSIM2 PCSIM1 PCSIMO PCSICO.
      *    MOVE "A"     TO PCLET.
      *    MOVE 01      TO PCRLE.
      *CREAP21.
      *    IF EJGRP4 NOT = 3 MOVE "CREE PAR ORDINATEUR" TO PCLIBE
      *                     GO TO T170B.
      *****READ FCLIENTS INVALID GO TO INVA19.
      *    perform r-fclients.
      *    if file-status not = zero
      *          GO TO INVA19.
      *CREAP22.
      *    IF CLGEO = ZERO MOVE 1 TO PCRAAT
      *      ELSE          MOVE 3 TO PCRAAT.
      *    IF CLNOM NOT = SPACES MOVE CLNOM TO PCLIBE
      *      ELSE                MOVE CLRSS TO PCLIBE.
      *    MOVE CLCSU TO PCCSCL.
      *    MOVE CLCAV TO PCCVCL.
      *    GO TO T170B.
           go to t175b.
      *----> M0797 (F)
      *
      **** FIN PHASE E ****
      *
       T180.
           MOVE "UTFACTC025" TO PHCLE.
      *****READ PARBATCH INVALID GO TO INVA1.
           perform r-parbatch.
           if file-status not = zero
                 GO TO INVA1.
           MOVE ALL "0" TO PHBAR.
      *****REWRITE ENRBGP INVALID GO TO INVA4.
           perform rw-parbatch.
           if file-status not = zero
                GO TO INVA4.
           GO TO FIN.
      *
      *****************************************************************
      *                    TRAITEMENT  REPRISES
      *****************************************************************
      *
      *
      **** REPRISE PHASE A ****
      *
       REPRISEA.
           MOVE PHINEC TO WNPSJ.
           MOVE PHIDJO TO WDAT9.
           IF PHIPHA = 1 GO TO REPRISEB.
           move zeroes to ejcle.
           MOVE WNPSJ  TO EJNAS.
      *****READ ECRJO INVALID GO TO REPRIS1A.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO REPRIS1A.
      *****DELETE ECRJO INVALID DISPLAY "ANNUL. ECRIT. DU JOUR IMPOSS:
      *****-    "REPRISE A"
      *****                     GO TO FIN.
       suisupej.
           perform d-ecrcptjo.
           if file-status not = zero
              if mmdt-langue = "FR"
                   DISPLAY "ANNUL. ECRIT. DU JOUR IMPOSS: REPRISE A"
                   GO TO FIN
              else
                   DISPLAY "DELETION ECRCPTJO IMPOSSIBLE: RESUMPTION A"
                   GO TO FIN
              end-if
           end-if
           perform n-ecrcptjo.
           if file-status not = zero go to repris1a.
           if ejnas not = wnpsj      go to repris1a.
           go to suisupej.
       REPRIS1A.
           GO TO T30.
      *----> M0797 (D)
      *--    lignes de repris2a a fin t187 seront a supprimees
      *REPRIS2A.
      *    MOVE 0 TO WCMONTD WCMONTC.
      *    MOVE EXCMONT TO WEXMT1.
      *    IF WEXMT1 NOT = WMONTECC GO TO REPRIS3A.
      *****READ EXTRA COMPL AT END GO TO T75.
      *    perform n-extracpt.
      *    if file-status not = zero
      *         GO TO T75.
      *    if excnt  not = wexcnt2 go to t75.
      *    IF EXNECR NOT = FBNFA GO TO REPRIS3A.
      *    GO TO T77.
      *REPRIS3A.
      *    MOVE EXCMONT TO WEXMT1.
      *    IF EXCSIGN = "D" ADD WEXMT1 TO WCMONTC
      *      ELSE           ADD WEXMT1 TO WCMONTD.
      *    IF EXNECR = FBNFA GO TO T77.
      *****READ EXTRA COMPL AT END GO TO REPRIS4A.
      *    perform n-extracpt.
      *    if file-status not = zero
      *         GO TO REPRIS4A.
      *    if excnt  not = wexcnt2 go to repris4a.
      *    GO TO REPRIS3A.
      *REPRIS4A.
      *    IF WCMONTD > WCMONTC SUBTRACT WCMONTC FROM WCMONTD
      *                                  GIVING WCMONT
      *      ELSE               SUBTRACT WCMONTD FROM WCMONTC
      *                                  GIVING WCMONT.
      *REPRIS5A.
      *    move wexcle2 to excle.
      *    perform r-extracpt.
      *****READ EXTRA PRIMARY.
      *    IF file-status NOT = "00" GO TO INVA20.
      *    MOVE ENREXC TO WENREXC.
      *    MOVE EXCMONT TO WEXMT1.
      *    IF WEXMT1 NOT = WCMONT GO TO T75.
      *    GO TO T77.
      *REPRIS6A.
      *    MOVE ZERO TO WTOP3.
      *    MOVE WDATEC9 TO WAED.
      *    MOVE FBE1F TO WMONTECF.
      *T185.
      *    IF TCTYE NOT = 4 GO TO T186.
      *    IF TCNFA NOT = FBNFA GO TO T186.
      *    IF FBREG = 7 OR FBREG = 13 GO TO T185A.
      *****READ FTRAITES COMPL AT END GO TO T95A.
      *    perform n-ftraites.
      *    if file-status not = zero
      *         GO TO T95A.
      *    if tcclp not = wncl go to t95a.
      *T185A.
      *    SUBTRACT 1 FROM WNEC3.
      *    IF WNEC3 NOT > 1 GO TO T185C.
      *    IF WTOP4 NOT = ZERO GO TO T185B.
      *    MOVE FBD2J TO WAEDJ.
      *    MOVE FBD2M TO WAEDM.
      *    MOVE FBD2A TO WAEDA.
      *    MOVE FBE2F TO WMONTECF.
      *    MOVE 2 TO WTOP4.
      *    MOVE 3 TO WNEC3.
      *    GO TO T186.
      *T185B.
      *    MOVE FBD3J TO WAEDJ.
      *    MOVE FBD3M TO WAEDM.
      *    MOVE FBD3A TO WAEDA.
      *    MOVE FBE3F TO WMONTECF.
      *    MOVE 1 TO WNEC3.
      *    GO TO T186.
      *T185C.
      *    IF WNEC3 NOT > 0 GO TO T98.
      *    MOVE FBD2J TO WAEDJ.
      *    MOVE FBD2M TO WAEDM.
      *    MOVE FBD2A TO WAEDA.
      *    MOVE FBE2F TO WMONTECF.
      *T186.
      *****READ FTRAITES COMPL AT END GO TO T187.
      *    perform n-ftraites.
      *    if file-status not = zero
      *         GO TO T187.
      *    if tcclp not = wncl go to t187.
      *    GO TO T185.
      *T187.
      *    IF WNEC3 > 0 PERFORM CREATIO3
      *                  GO TO T95.
      *    GO TO T98.
      *----> M0797 (F)
      *
      **** REPRISE B ****
      *
       REPRISEB.
           IF PHIPHB = 1 GO TO REPRISEC.
           move zeroes to ejcle.
           MOVE WNPSJ  TO EJNAS.
      *****READ ECRJO INVALID GO TO T40.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO T40.
      *****DELETE ECRJO INVALID DISPLAY "ANNUL. ECRIT. DU JOUR IMPOS:
      *****-    "REPRISE B"
      *****                    GO TO FIN.
       suisupej1.
           perform d-ecrcptjo.
           if file-status not = zero
              if mmdt-langue = "FR"
                   DISPLAY "ANNUL. ECRIT. DU JOUR IMPOSS: REPRISE B"
                   GO TO FIN
              else
                   DISPLAY "DELETION ECRCPTJO IMPOSSIBLE: RESUMPTION B"
                   GO TO FIN
              end-if
           end-if
           perform n-ecrcptjo.
           if file-status not = zero go to t40.
           if ejnas not = wnpsj      go to t40.
           go to suisupej1.
      *    GO TO T40.
      *
      **** REPRISE PHASE C ****
      *
       REPRISEC.
           IF PHIPHC = 1 GO TO REPRISED.
           PERFORM T30.
       T190.
           IF CPCD2 = "0" AND CPCD3 = "0" GO TO T150A.
      *****READ PARAM NEXT AT END GO TO T155.
           perform n-paramcpt.
           if file-status not = zero
                GO TO T155.
           GO TO T190.
      *
      **** REPRISE PHASE D ****
      *
       REPRISED.
           IF PHIPHD = 1 GO TO REPRISEE.
           move zeroes to ejcle.
           MOVE WNPSJ  TO EJNAS.
      *****READ ECRJO INVALID GO TO INVA16D.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO INVA16D.
      *
      * REPRISE CONTROLE ECRITURES ET PERCOMPT
      *
       T200.
           MOVE 0 TO WMONTC WMONTD.
           PERFORM T161 THRU T162.
           IF WMONTC NOT = WMONTD GO TO ERREUR.
           PERFORM T163.
      *****READ PERCO INVALID GO TO INVA15C.
           perform r-percogr0.
           if file-status not = zero
                 GO TO INVA15C.
           IF PCTYEL NOT = 1 GO TO INVA15D.
           IF PCCCR = 0 GO TO T210.
           IF PCCCR < WMONTCC GO TO T210
             ELSE            GO TO T220.
       T210.
           ADD WMONTCC TO PCCCR.
       T220.
           IF PCCDT = 0 GO TO T230.
           IF PCCDT < WMONTDC GO TO T230
             ELSE            GO TO T240.
       T230.
           ADD WMONTDC TO PCCDT.
       T240.
      *****REWRITE ENRPCT INVALID GO TO INVA3D.
           perform rw-percogr0.
           if file-status not = zero
                GO TO INVA3D.
           GO TO T165.
      *
      **** REPRISE PHASE E ****
      *
       REPRISEE.
           move zeroes to ejcle.
           MOVE WNPSJ  TO EJNAS.
      *****READ ECRJO INVALID GO TO INVA16E.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO INVA16E.
       T300.
           IF EJCVA NOT = ZERO GO TO T180.
           MOVE ZERO TO WTOP6.
       T310.
      *****READ ECRJO COMPL AT END GO TO T315.
           perform n-ecrcptjo.
           if file-status not = zero
                GO TO T315.
           IF EJCVA NOT = ZERO GO TO T310.
           if ejnas not = wnpsj go to t315.
      *----> M0797 (D)
      *    PERFORM T170A.
      *****READ PERCO INVALID GO TO CREAP2.
      *    perform r-percoel2.
      *    if file-status not = zero
      *          GO TO CREAP2.
      *    IF PCCCON = ZERO GO TO T170B.
      *----> M0797 (F)
           PERFORM T175B THRU T175C.
           MOVE 1 TO EJCCL1 EJCVA.
      *****REWRITE COMPL ENRECJ.
           perform rw-ecrcptjo.
           IF file-status NOT = "00" GO TO INVA17B.
           GO TO T310.
       T315.
           MOVE 1 TO WTOP6.
           move zeroes to ejcle.
           MOVE WNPSJ  TO EJNAS.
      *****READ ECRJO INVALID GO TO INVA16F.
           perform r-ecrcptjo.
           if file-status not = zero
                 GO TO INVA16F.
      *----> M0797 (D)
      *    PERFORM T170A.
      *****READ PERCO INVALID GO TO T179.
      *    perform r-percoel2.
      *    if file-status not = zero
      *          GO TO T179.
      *    IF PCCCON = ZERO GO TO T170B.
      *----> M0797 (F)
           GO TO T175B.
      *
       TITR3.
           MOVE SPACES TO LIGN3.
           WRITE LIGN3 BEFORE PAGE.

DD0370*       ajout ligne titre
           move wlabel-etat2 to immlp-titr-etat
           move wnom-prog   to immlp-titr-prog
           call 'mmlp-titr1' using mmlp-titr adl-art
           move ommlp-titr-titr to ligne
           write LIGNE BEFORE 1
           move spaces to ligne

           MOVE "          ETAT DES NUMEROS INEXISTANTS AU FICHIER CLIEN
      -    "TS" TO LIGN3.
           WRITE LIGN3 BEFORE 1.
           MOVE SPACES TO LIGN3.
           MOVE "               RACINE INITIALISEE A 1" TO LIGN3.
           WRITE LIGN3 BEFORE 3.
           MOVE SPACES TO LIGN3.
           MOVE "     NUMERO CLIENT" TO LIGN3.
           WRITE LIGN3 BEFORE 2.
           MOVE SPACES TO LIGN3.
           MOVE ZERO TO WCTL.
       FTITR3.
           EXIT.
      *****************************************************************
      *                  MESSAGES  D' ERREURS
      *****************************************************************
      *
       INVA1.
           DISPLAY "EL. UTFACTC025 INEXISTANT: " PHCLE.
           GO TO FININV.
       INVA2.
           MOVE PCETS TO WEXETS2.
           MOVE PCNCPT TO WEXNCPT2.
           DISPLAY "EL. NUMEROTATION INEXISTANT: " WEXETS2 PCGRP
           WEXNCPT2.
           GO TO FININV.
       INVA3.
           MOVE PCETS TO WEXETS2.
           MOVE PCNCPT TO WEXNCPT2.
           DISPLAY "REECRITURE PERCOMPT IMPOSSIBLE: " WEXETS2 PCGRP
           WEXNCPT2.
       INVA3A.
           DISPLAY "MAJ EL. NUMEROTATION: DEBUT TRT".
           GO TO FININV.
       INVA3B.
           PERFORM INVA3.
           DISPLAY "MAJ JOURNAL 11: PHASE D".
           GO TO FININV.
       INVA3C.
           PERFORM INVA3.
           DISPLAY "MAJ CA: PHASE E".
           GO TO FININV.
       INVA3D.
           PERFORM INVA3.
           DISPLAY "MAJ JOURNAL 11: REPRISE D".
           GO TO FININV.
       INVA4.
           DISPLAY "REECRITURE PARBATCH IMPOSSIBLE: " PHCLE.
           GO TO FININV.
       INVA5.
           DISPLAY "EL. PARAMCPT INEXISTANT: TARIF: " WVT (1) " TAXE: "
DD0422*    FBTAX " PAYS: " FBPMP.
           FBTAX " PAYS: " FBPML.
           GO TO FININV.
       INVA6.
           DISPLAY "REECRITURE PARAMCPT IMPOSSIBLE: " CPCLE.
       INVA6A.
           DISPLAY "RAZ MONTANTS: DEBUT TRT".
           GO TO FININV.
       INVA6B.
           PERFORM INVA6.
           DISPLAY "MAJ ZONES JOUR: PHASE A".
           GO TO FININV.
       INVA6C.
           PERFORM INVA6.
           DISPLAY "MAJ CODE CPCD1: PHASE B".
           GO TO FININV.
       INVA6D.
           PERFORM INVA6.
           DISPLAY "MAJ ZONES MOIS: PHASE C".
           GO TO FININV.
       INVA7.
           DISPLAY "INV-7 : ESSAI D'ECRITURE: " WTOP8.
           MOVE WEJNAS TO WEJNAS1.
           IF file-status = "24" DISPLAY "FICH. ECRIT. DU JOUR PLEIN"
           GO TO FININV.
           IF file-status = "22" DISPLAY "CLE EN DOUBLE: NO: " WEJNAS1
           GO TO FININV.
           DISPLAY "AUTRE STATUS: " file-status.
           GO TO FININV.
      *----> M0797 (D)
      *INVA8.
      *    MOVE EXETS TO WEXETS2.
      *    MOVE EXNCPT TO WEXNCPT2.
      *    MOVE EXECHC TO WEXECHC2.
      *    DISPLAY "REECRITURE EXTRACPT IMPOSSIBLE: " WEXETS2 EXGRP
      *    WEXNCPT2 WEXECHC2.
      *    GO TO FININV.
      *INVA9.
      *    DISPLAY "INV-9 : ESSAI D'ECRITURE: " WTOP8.
      *    MOVE WEXETS TO WEXETS2.
      *    MOVE WEXNCPT TO WEXNCPT2.
      *    MOVE WEXECHC TO WEXECHC2.
      *    IF file-status = "24" DISPLAY "FICH. EXTRACPT PLEIN"
      *    GO TO FININV.
      *    IF file-status = "22" DISPLAY "CLE EN DOUBLE : ETS: "
      *          WEXETS2 " GRP: " WEXGRP  " COMPTE: " WEXNCPT2
      *                          GO TO FIN.
      *    DISPLAY "AUTRE STATUS: " file-status.
      *    GO TO FININV.
      *INVA10.
      *    DISPLAY "EL. REGLEMENT PARAMGPI INEXISTANT: " WCLEGPI.
      *    GO TO FININV.
      *INVA12.
      *    DISPLAY "INV-12 : ESSAI D'ECRITURE: " WTOP8.
      *    MOVE WNCL TO WNCL3.
      *    IF file-status = "24" DISPLAY "FICH. FTRAITES PLEIN"
      *    GO TO FININV.
      *    IF file-status = "22" DISPLAY "CLE EN DOUBLE: NO: " WNCL3
      *    GO TO FININV.
      *    DISPLAY "AUTRE STATUS: " file-status.
      *    GO TO FININV.
      *----> M0797 (F)
       INVA14.
           DISPLAY "REECRITURE FFACTURE IMPOSSIBLE: " WNFACT.
           GO TO FININV.
       INVA15.
           MOVE PCETS TO WEXETS2.
           MOVE PCNCPT TO WEXNCPT2.
           DISPLAY "EL. JOURNAL 11 DE PERCOMPT INEXISTANT: " WEXETS2
           PCGRP WEXNCPT2.
       INVA15A.
           DISPLAY "PHASE D".
           GO TO FININV.
       INVA15B.
           DISPLAY "TYPE EL. JOURNAL 11 NON = 1: " PCTYEL " PHASE D".
           GO TO FININV.
       INVA15C.
           PERFORM INVA15.
           DISPLAY "REPRISE D".
           GO TO FININV.
       INVA15D.
           DISPLAY "TYPE EL. JOURNAL 11 NON = 1: " PCTYEL " REPRISE D".
           GO TO FININV.
       INVA16.
           MOVE EJNAS TO WEJNAS1.
           DISPLAY "RELECTURE ECRIT. DU JOUR IMPOSSIBLE: " WEJNAS1.
       INVA16A.
           DISPLAY "CALCUL CENTRALISATEURS: PHASE D".
           GO TO FININV.
       INVA16B.
           PERFORM INVA16.
           DISPLAY "MAJ CA DANS PERCOMPT: PHASE E".
           GO TO FININV.
       INVA16C.
           DISPLAY "MAJ ECRCPTJO: PHASE E".
           GO TO FININV.
       INVA16D.
           PERFORM INVA16.
           DISPLAY "REPRISE D".
           GO TO FININV.
       INVA16E.
           PERFORM INVA16.
           DISPLAY "REPRISE E".
           GO TO FININV.
       INVA16F.
           PERFORM INVA16.
           DISPLAY "REPRISE E: MAJ AP".
           GO TO FININV.
       INVA17.
           MOVE EJNAS TO WEJNAS1.
           DISPLAY "REECRITURE ECRIT. DU JOUR IMPOSSIBLE : " WEJNAS1.
       INVA17A.
           DISPLAY "MAJ ECRCPTJO: PHASE E".
           GO TO FININV.
       INVA17B.
           PERFORM INVA17.
           DISPLAY "MAJ ECRCPTJO: REPRISE E".
           GO TO FININV.
      *----> M0797 (D)
      *INVA18.
      *    DISPLAY "INV-18 : ESSAI D'ECRITURE: " WTOP8.
      *    MOVE PCETS2  TO WEXETS2.
      *    MOVE PCNCPT2 TO WEXNCPT2.
      *    IF file-status = "24" DISPLAY "FICH. PERCOMPT PLEIN"
      *    GO TO FININV.
      *    IF file-status = "22" DISPLAY "CLE EN DOUBLE: ETS: " WEXETS2 " GR
      *-    "P: " PCGRP2 " COMPTE: " WEXNCPT2
      *    GO TO FININV.
      *    DISPLAY "AUTRE STATUS: " file-status.
      *    GO TO FININV.
      *INVA19.
      *    IF WTEST = ZERO MOVE 1 TO WTEST.
      *    IF WCTL > 55 PERFORM TITR3 THRU FTITR3.
      *    MOVE WNCL TO LNCPT.
      *    WRITE LIGN3 BEFORE 1.
      *    MOVE SPACES TO LIGN3.
      *    MOVE EJCSUC TO PCCSCL.
      *    MOVE 1 TO PCRAAT.
      *    GO TO T170B.
      *INVA20.
      *    MOVE EXETS  TO WEXETS2.
      *    MOVE EXNCPT TO WEXNCPT2.
      *    MOVE EXECHC TO WEXECHC2.
      *    DISPLAY "RELECTURE EXTRACPT IMPOSSIBLE: " WEXETS2 EXGRP
      *    WEXNCPT2 WEXECHC2.
      *    GO TO FININV.
      *INVA21.
      *    MOVE WNCL TO WNCLL.
      *    DISPLAY "REECRITURE FTRAITES IMPOSSIBLE: " WNCLL.
      *    GO TO FININV.
      *----> M0797 (F)
       ERREUR.
           DISPLAY "DEBIT NON EGAL CREDIT / ECRITURE JOUR".
           GO TO FININV.
       F1.
           DISPLAY "AUCUNE FACTURE A TRAITER".
           MOVE "UTFACTC025" TO PHCLE.
      *****READ PARBATCH INVALID GO TO INVA1.
           perform r-parbatch.
           if file-status not = zero
                 GO TO INVA1.
           MOVE ALL "0" TO PHBAR.
      *****REWRITE ENRBGP INVALID GO TO INVA4.
           perform rw-parbatch.
           if file-status not = zero
                GO TO INVA4.
       fininv.
         if mmdt-langue = "FR"
           display "ATTENTION : VOTRE PROGRAMME S'EST MAL TERMINE"
           display "(SAUF CAS : PAS DE FACTURES A TRAITER)"
           display "VALIDER POUR TERMINER LE PROGRAMME"
         else
           display "ATTENTION: YOUR PROGRAM BADLY ENDED"
           display "(EXCEPT CASE: NO INVOICES TO BE TREATED)"
           display "ENTER TO END THE PROGRAM"
         end-if
DD0351   perform env-mail
           accept wreponse.
       FIN.
           IF WTEST NOT = ZERO
              if mmdt-langue = "FR"
                 DISPLAY "VOIR ETAT ANOMALIES"
              else
                 DISPLAY "SEE ABNORMALITIES STATE"
              end-if
           END-IF
           IF WTOP7 = 1 GO TO FIN1.
           CLOSE ETAT1.
       FIN1.
           CLOSE etat2.
           perform cl-percogr0.
      *----> M0797 (D)
      *    perform cl-percoel2.
      *    perform cl-paramgpi.
      *    perform cl-extracpt.
      *    perform cl-ftraites.
      *    perform cl-paramcpt.
      *----> M0797 (F)
           perform cl-ffacture.
           perform cl-ecrcptjo.
      *****      FCLIENTS ANO PARBATCH.
           STOP RUN.

      *=========================================================================
      *                          FONCTIONS LOCALES
      *=========================================================================

       cal-francs section.
DDE089**** appel fonction conversion des devises
           move 62 to weu-adev
           move 00    to weu-ndev
DD0351     if mmdt-societe = "SLOVAQ"
  -           move 50 to weu-adev
  -           move 62 to weu-ndev
DD0351     end-if
           call 'mmca-devi1' using wmmca-devi adl-art
           if weu-err not = spaces
              display weu-err
              move zero to weu-pht
           end-if.

GPICMT* GPIMAIL envoi erreur par mail
        env-mail section.
           move cmmlp-mail-type-oo to immlp-mail-type(1)
           move "EDITION DU JOURNAL DES VENTES"
                   to immlp-mail-data(1)
           move cmmlp-mail-type-o to immlp-mail-type(2)
           string "Voir erreur factc025 dans $GPILOG"
                     delimited by size into immlp-mail-data(2)
           string "PLANTAGE JOURNAL DES VENTES"
                   delimited by size into immlp-mail-objet
           move cmmlp-mail-trt-notif to immlp-mail-trt
           move spaces to immlp-mail-destg
           move spaces to immlp-mail-groupe
DD0351*    move "elgu micn door" to immlp-mail-dest
DD0351     move "anes micn"  to immlp-mail-dest
           move wnom-prog to immlp-mail-pgm
           call 'mmlp-mail1' using mmlp-mail adl-art
            .

GPICMT* creation commande fournisseur
DD0394 cre-cdefour section.
           string "cmcdccdf.sh"  " F," fbcle ","
                   X'00'
                delimited by size into sys-var
           call "systcc" using sys-var syst-rtn
           .

       pro section.
           copy "../copy/pro-ecrcptjo".
           copy "../copy/pro-ffacture-cdesup".
           copy "../copy/pro-paramcpt".
      *----> M0797 (D)
      *    copy "../copy/pro-ftraites".
      *    copy "../copy/pro-extracpt".
      *    copy "../copy/pro-paramgpi".
      *----> M0797 (F)
           copy "../copy/pro-percogr0".
      *----> M0797 (D)
      *    copy "../copy/pro-percoel2".
      *    copy "../copy/pro-fclients".
      *----> M0797 (F)
           copy "../copy/pro-parbatch".
