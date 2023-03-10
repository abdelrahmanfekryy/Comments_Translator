      *DD0448 15/12/11 elgu si adresse livraison prise dans l'entete ne pas prendre le commande par
      *                     pour le controle code taxe, adresse livree correcte dans cmcd-gest1
      *DD0503 10/10/11 elgu gestion imprimmeur
      *DD0448 20/07/11 elgu pour facture ourillage editer le numero tva client
      *DD0448 11/07/11 elgu  controle code taxe
      *DD0448 04/07/11 elgu cntrole code taxe
      *DD0350 21/06/11 elgu coorection edition commentaires lignes
      *ELGU17 05/05/11 elgu pour les proforma forcer le nombre d'exemplaire a 1
      *                     ajout edition nomenclature douaniere pour export hors cee
      *DD0448 14/04/11 elgu traitement code taxe par focntion avec edition texte
      *DD0221 13/01/11 anes allongement reference commande client
      *DD0351 07/01/11 elgu ne plus editer le rib client
      *DD0444 02/12/10 elgu maj data ware evenement facturation
      *DD0465 19/08/10 elgu remplacer FBFIN par FBIMPRIM
      *DD0444 10/05/10 micn maj dataware en xml
      *DD0351 06/04/10 elgu nouvelles coordonnees bancaire pour tous les clients 
      *DD0448 08/02/10 elgu prendre le numero intracom du client payeur
      *DD0351 07/01/10 elgu prendre les conditions de reglement du pieds de facture en cas de reedition
      *DD0444 24/12/09 elgu appel maj commande en dataware apres w-ffacture, car fait dans la gf avant la craetion de ffacture donc perte des infos pieds
      *DD0438 20/08/09 micn modif appel calcul echeance + appel controle des cond. de reglement
      *DD0351 24/06/09 elgu deblocage fcommac4 et ffacturea et fcommaap
      *DD0424 25/03/09 micn suppression de la zone releve de la fiche client
      *DD0422 28/01/09 elgu pour controle paramcpt prendre le code geo du livrea
      *DD0351 22/12/08 micn  modif conditions de reglement
      *DD0351 22/07/08 micn ajout texte pour conditions de ventes
      *DD0358 18/03/08 elgu ajout d'une 4eme banque
      *DD0394 19/02/08 elgu ajout du type de commande
      *DD0350 16/01/08 elgu modification taux de change pour Slovaquie
      *DD0387 04/01/08 elgu initialisation choix pour appel mmpa-regl1
      *                     traitement calcul echeance par fonction
      *DD0350 02/01/08 elgu ne pas creer de trace de facturation si edition arc
      *DD0350 19/07/07 micn recup du message erreur au retour edition facture/arc
      *DD0362 16/07/07 micn edition PDF
      *DD0350 13/07/07 micn 
      *DD0350 12/07/07 elgu suppression des open input et changement niveau
      *                     suite a edition des arc en pdf (blocage SLIV)
      *DD0350 10/07/07 micn edition laser au moment de la creation du pied de facture
      *DD0362 02/04/07 micn Edition arc laser chiffre + correction proforma
      *DD0351 19/03/07 elgu ne plus faire de cumul sur reference, editer chaque ligne
      *DD0350 29/01/07 elgu correction edition des commentaires lignes ; les commentaires sont edites avant la reference
      *                     ==> qd plusieurs lignes references avec plusieurs commentaires c'est illisible ==> un interligne 
      *                         avant edition commentaire
      *DD0358 27/12/06 elgu ajout infos banques (IBAN/BIC) sur 2 lignes
      *DD0350 26/10/06 micn ne pas mettre le com. escompte pour favotex
      *DD9999 09/10/06 elgu ajout commentaire escompte pour toutes les factures
      *DD0338 31/08/06 elgu edition a la demande pour assistante commerciale
      *DD0337 10/08/06 elgu edition laser pour slovaquie
      *DD9999 31/07/06 elgu corection edition contre partie en FRF pour BL regroupes
      *DD0326 21/06/06 elgu
      *DD0316 29/04/06 door alongement node cde
      *DD0316 18/04/06 elgu nlle wor-ffacture.mod wor-fcommac1
      *DD9999 02/06/06 elgu correction edition refgroupee de 2 memes ref avec
      *                     prix different
      *DD9999 25/01/06 elgu ne pas ouvrir paramgpi et mettre en niveau 3
      *                     si facture proforma demande a la saisie de
      *                     commande le fichier s'ouvrait en input
      *                     et plantage sur la saisie de la commande suivante
      *DD9999 25/11/05 elgu correction creation pied fe facture si facture
      *                     proforma regroupee
      *DD9999 18/10/05 elgu controle existence paramcpt pour ventilation compta
      *DD9999 27/07/05 elgu correction edition ref commande client pour BL
      *                     regroupes
      *DD9999 22/06/05 elgu prendre le code reglement du regle par
      *DDE175 02/03/05 elgu ne plus creer intracom
      *DDE153 10/01/05 elgu correction reedition facture avec BL regroupes
      *DD0062 29/12/04 micn changement code action trace
      *V30002 15/12/04 elgu
      *V20002 10/12/04 elgu
      *DDE153 26/11/04 elgu regroupement plusieurs bl sur une seule facture
      *V10002 27/09/04 elgu
      *DD0002 04/08/04 elgu activer le bloc adresse client a la place
      *                     du code nature de filiation
      *DD9999 12/08/04 elgu controle coherence des codes avec affichage erreur
      *                     et ctrl numero intracommunautaire
      *DD0188 28/06/04 elgu trt adresse export
      *DD0122 elgu le 11/06/04 sup DD0122 suite a nouvel imprime
      *DD0122 13/10/03 elgu edition nlle denomination et numero TVA
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cilp-fact1.
      *
      *********************************************
      * GPICMT EDITION   DES    FACTURES INDUSTRIE*
      *********************************************
      *
      *DDE046
      *DDE130 11/09/02 elgu nlle mise en page entete facture pour agrandissement
      *                     de la reference commande du client
      *                     editer les lignes factures a rupture ref/sous ref
      *DDE089: ajout controle validite de la devise
      *DDE069: numerotation lignes des commentaires idem DINAC
      *M210601 correction erreur calcul montant ligne
      *DDE089: ajout trt facture a la demande et modif pour compta en euro
      *DDE011: ne plus controler la devise avec le code facture dematerialisee
      *DDE043: trt el 5 (fcommac4) traiter le signe meme si montant non
      *        renseigne
      *DDE027: lecture fcommaap sur cle 3 afac/fac/cde pour ne lire que les
      *        commandes a facturer
      *M0299 : agrandiss. zones wcale et wzca, passe de 6 a 9 (pb lire et peset)
      *M1298 : ajout test du code decalage de l'echeance si date > 24
      *M1198 : Modifs pour passage a l' EURO
      *      : modif pour ne pas editer les factures dematerialisees
      *M0997 : mettre 1 dans fbetr (nlle compta)
      *M0797 : editer les textes permanents en fin de factures(apres remises)
      *        editer ffr si devise = 00 and wgeol # 0 mettre "FRANC FRANCAIS"
      *        dans le libelle de la devise
      *        enlever les chmod de fcomma*
      *M0197: si numero intracommunautaire du livre a = spaces prendre celui du
      *       facture a
      *M1196 : modif move fcdop --> iclia au lieu de fclia
      * edition n.outillage pour les factures d outillage 05/96
      * raz fclig car ajout fclig dans cle 3 ds fcommac2 07/96
      * modif pour prendre le num intrac. du client livre a trouve ds fcoadcli
      * au lieu de celui du client trouve ds la cde. M1096
      *  EP
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DPS-4.
       OBJECT-COMPUTER. DPS-4.
      *SPECIAL-NAMES.
      *    LN010 IS L10
      *    LN019 IS L19
      *    LN030 IS L30
      *    LN060 IS L60.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    SELECT FCOMMDES ASSIGN TO FCO-MSD300
      *
      *    SELECT PARAMGPI ASSIGN TO PAR-MSD300
      *
      *    SELECT FFACTURE ASSIGN TO FFA-MSD300
      *
      *    SELECT FCLIENTS ASSIGN TO FCL-MSD300
      *
      *    SELECT FCOADCLI ASSIGN TO CAD-MSD300
      *
      *    SELECT CLISUITE ASSIGN TO CLS-MSD300
      *
      *    SELECT PARBATCH ASSIGN TO PAB-MSD300
      *    SELECT DEVI     ASSIGN TO DEV-MS
      *
      *         SELECT INTCO ASSIGN TO INC
      *
           SELECT ETAT ASSIGN TO wlabel-etat
                       organization line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ETAT
                LINAGE IS 72
                DATA RECORD LIGNE.
       01  LIGNE             PIC X(82).
       01  L1.
           02 FILLER         PIC X.
           02 LNOMLIV.
             03 FILLER       PIC X(17).
             03 LSUITE       PIC X(5).
             03 FILLER       PIC X(5).
           02 LCOND          PIC X(12).
DD0326     02 lnfact redefines lcond pic bb9(7)bbb.
           02 FILLER         PIC XX.
           02 LNOMPAY        PIC X(26).
           02 FILLER         PIC X(14).
       01  L2.
           02 FILLER         PIC X.
DD0188     02 lbureaul.
            03 LCODP1        PIC 9(5)            BLANK ZERO.
            03 FILLER        PIC X.
            03 LXBD1         PIC X(26).
            03 FILLER        PIC X(3).
           02 LNBEX          PIC Z9.
           02 FILLER         PIC X(4).
DD0188* agrandissement zone pour adresse export
DD0188     02 lbureau.
            03 LCODP2         PIC 9(5)            BLANK ZERO.
            03 FILLER         PIC X.
            03 LXBD2          PIC X(29).
DD0188     02 FILLER         PIC X(5).
       01  L3.
           02 FILLER         PIC X.
DDE046     02 LNREF          PIC x(7).
           02 LVREF          PIC X(8).
           02 LLIB.
             03 LDESAR.
               04 LJE        PIC 99.
               04 LSL1       PIC X.
               04 LME        PIC 99.
               04 LSL2       PIC X.
               04 LAE        PIC 99.
               04 FILLER     PIC X(6).
               04 LMTECH     PIC Z(6)V,99        BLANK ZERO.
               04 LMODIF     PIC X(7).
             03 LUV          PIC Z9.
             03 LNBTES       PIC ZZZ9.
             03 FILLER REDEFINES LNBTES.
               04 FILLER     PIC XXX.
               04 LNBTESX    PIC X.
           02 LQTEFA         PIC Z(8)9V99B       BLANK ZERO.
           02 FILLER REDEFINES LQTEFA.
             03 LQTEUN       PIC ZZZ9            BLANK ZERO.
             03 LQTEFAU      PIC Z(4)9V99B       BLANK ZERO.
             03 FILLER REDEFINES LQTEFAU.
               04 FILLER     PIC X(5).
               04 LQTEFAD    PIC XX.
               04 FILLER     PIC X.
           02 LPU            PIC Z(5)9V99        BLANK ZERO.
           02 FILLER REDEFINES LPU.
             03 LUCM         PIC X.
             03 FILLER       PIC X(7).
           02 LMONT          PIC Z(6)9V99        BLANK ZERO.
           02 LSIG           PIC X.
       01  L4.
           02 FILLER         PIC X.
           02 LNBCOL         PIC Z9B             BLANK ZERO.
           02 LPDSTO         PIC ZZZ9V,9         BLANK ZERO.
           02 LNETMA         PIC Z(5)9V99B       BLANK ZERO.
           02 LTHT           PIC Z(5)9V99        BLANK ZERO.
           02 LPORTT         PIC ZZZ9V99B        BLANK ZERO.
           02 LTAU1          PIC Z9V99B          BLANK ZERO.
           02 LMTTVA1        PIC Z(5)9V99B       BLANK ZERO.
           02 LTAU2          PIC Z9V99B          BLANK ZERO.
           02 LMTTVA2        PIC Z(5)9V99        BLANK ZERO.
           02 LAFFRAN        PIC ZZZ9V99         BLANK ZERO.
      *----> M0797 (D)
      *    02 LNETPAY        PIC Z(9)9V,99      BLANK ZERO.
      *    02 LSIGN          PIC X.
      *    02 FILLER         PIC X.
           02 lffr           pic xxxx.
           02 LNETPAY        PIC Z(6)9V,99      BLANK ZERO.
           02 LSIGN          PIC X.
      *----> M0797 (F)
       01  L5.
           02 FILLER         PIC X.
           02 LR5.
            03 LAD.
             04 LCODBQ       PIC X(6).
             04 LCODGU       PIC X(6).
             04 LCOMPB       PIC X(12).
             04 LRIB         PIC XX.
            03 FILLER        PIC XXX.
            03 LLIB2         PIC X(14).
           02  LREGL REDEFINES LR5.
            03 LRLI          PIC X(30).
            03 FILLER        PIC XX.
DD0351      03 lcondx.
             04 LRJO          PIC 99B.
             04 LRJL          PIC X(6).
             04 LRLE          PIC 99.
           02 LJEE           PIC 99.
           02 FILLER         PIC X.
           02 LMEE           PIC 99.
           02 FILLER         PIC X.
           02 LAEE           PIC 99.
           02 FILLER         PIC X(15).
           02 LPAPI.
             03 LNETPAYP.
      *----> M0797 (D)
      *        04 LNETPAY9   PIC Z(9)9V,99.
               04 lffrp      pic x(4).
               04 LNETPAY9   PIC Z(6)9V,99.
      *----> M0797 (F)
               04 LPASIG     PIC X.
             03 FILLER REDEFINES LNETPAYP.
      *----> M0797 (D)
      *        04 FILLER     PIC X(7).
               04 FILLER     PIC X(8).
      *----> M0797 (F)
               04 LNCLIP     PIC 9(6)B.
             03 FILLER REDEFINES LNETPAYP.
      *----> M0797 (D)
      *        04 FILLER     PIC X(5).
               04 FILLER     PIC X(6).
      *----> M0797 (F)
               04 LJP        PIC 99.
               04 LSL1P      PIC X.
               04 LMP        PIC 99.
               04 LSL2P      PIC X.
               04 LAP        PIC 99.
               04 FILLER     PIC X.
             03 FILLER REDEFINES LNETPAYP.
      *----> M0797 (D)
      *        04 FILLER     PIC X(8).
DD0316         04 FILLER     PIC X(7).
      *----> M0797 (F)
DD0316         04 LNFACTP    PIC 9(7)B.
             03 FILLER REDEFINES LNETPAYP.
      *----> M0797 (D)
      *        04 FILLER     PIC X(6).
DD0316         04 FILLER     PIC X(6).
      *----> M0797 (F)
DD0316         04 LNCDE      PIC 9(7).
               04 LNIN       PIC 9B.
      *----> M0797 (D)
      *    02 FILLER         PIC X.
      *----> M0797 (F)
       01  L6.
           02 FILLER         PIC X(16).
           02 LIBCA13.
             03 LIBCA12      PIC X(30).
             03 LTAUX        PIC Z9V,99B.
             03 LPOURC       PIC X.
             03 FILLER       PIC X(14).
             03 LMT          PIC Z(9)9V,99       BLANK ZERO.
             03 FILLER       PIC XX.
       01  L7.
           02 FILLER         PIC X(16).
           02 LFIN.
             03 LIBF         PIC X(15).
             03 LJF          PIC 99B.
             03 LSL5         PIC XX.
             03 LMF          PIC 99B.
             03 LSL6         PIC XX.
             03 LAF          PIC 99.
           02 FILLER         PIC X(21).
           02 LDEV           PIC X(18).
       01  L8.
           02 FILLER         PIC X(16).
           02 LPAY           PIC XX.
           02 FILLER         PIC XX.
           02 LCLE           PIC XX.
           02 FILLER         PIC XX.
           02 LSIR           PIC X(11).
           02 FILLER         PIC X(5).
           02 LEXP.
             03 LNV          PIC X(6).
             03 LTXT         PIC X(36).
      *
       WORKING-STORAGE SECTION.
           copy "../copy/wor-intracom".
DD9999     copy "../copy/wor-paramcpt".
           copy "../copy/wor-artdevc1".
           copy "../copy/wor-parbatch".
           copy "../copy/wor-clisuite".
DD0002*    copy "../copy/wor-fcoadcli".
           copy "../copy/wor-fclients".
           copy "../copy/wor-ffacture".
           copy "../copy/wor-paramgpi".
           copy "../copy/wor-fcommaap".
           copy "../copy/wor-fcommac1".
           copy "../copy/wor-fcommac2".
           copy "../copy/wor-fcommac3".
           copy "../copy/wor-fcommac4".
           copy "../copy/wor-fcommac5".
           copy "../copy/wor-artdevc2".
           copy "../copy/wor-numdevis".
DDE089     copy '../copy/tarifqt.com'.                                  *GPICMT
DDE089     copy "../copy/mmcp-devb.com".                                *GPICMT
DDE089     copy "../copy/mmdt-parb.com".                                *GPICMT
DDE089     copy "../copy/mmaf-vali.com".                                *GPICMT
DDE089     copy "../copy/mmti-date.com".                                *GPICMT
DDE089     copy "../copy/mmpa-regl.com".                                *GPICMT
DDE089     copy "../copy/mmpa-devi.com".                                *GPICMT
DDE089     copy "../copy/mmpa-upri.com".                                *GPICMT
DDE089     copy "../copy/mmtr-trac.com".                                *GPICMT
DDE089     copy "../copy/mmca-date.com".                                *GPICMT
DD0002     copy "../copy/cmcd-gest.com".                                *GPICMT
DD0337     copy "../copy/mmlp-hfac.com".                                *GPICMT
DD0358     copy "../copy/mmpa-pays.com".
DD0358     copy "../copy/mmpa-soci.com".
DD0387     copy "../copy/mmca-eche.com".
DD0387     copy "../copy/mmaf-vrep.com".
DD0444*    copy '../copy/cmex-cdes.com'.                                *GPICMT
DD0444     copy '../copy/cmcd-mjdw.com'.                                *GPICMT
DD0444     copy '../copy/cmcd-lect.com'.                                *GPICMT
DD0448     copy '../copy/mmpa-vtax.com'.                                *GPICMT
DD0448     copy '../copy/cmpa-tycd.com'.                                *GPICMT
DD0316 77  WNFANA            PIC 9(7).
DD0316 77  WNFACA            PIC 9(7).
       77  WTOTFA            PIC S9(7)V99.
       77  WTOTAV            PIC S9(7)V99.
       77  WCPTR             PIC s999.
DD0358 77  WCPTR-banque      PIC s999.
DD0358 77  WCPTR-tot         PIC s999.
       77  WCPTR7            PIC s999.
       77  WWCPTR            PIC s999.
       77  WQTEFA            PIC S9(10)V99.
       77  WMONT             PIC S9(9)V99.
       77  WHTD              PIC S9(7)V99.
       77  WNPF              PIC S9(7)V99.
       77  WMT               PIC S9(9)V99.
       77  WHTDF             PIC S9(7)V99.
       77  WTXF              PIC S9(7)V99.
       77  WWNBF             PIC s9.
       77  WNBF              PIC s9.
       77  I                 PIC s9.
       77  J                 PIC s9.
       77  WHT1              PIC S9(9)V99.
       77  WREM              PIC S9(9)V99.
       77  WFAEF             PIC S9(7)V99.
       77  WCAL              PIC S9(9)V99.
       77  wnbl              PIC 99.
      *
       01  wlabel-etat pic x(64) value space.
       01  var-name pic x(64).
       01  var-data pic x(64).
       01  syst-rtn   pic s9(4) comp.
DD0444 01  sys-var         PIC X(200).
DD0337 01  sys-rtn         PIC 9.
       01  syst-zone.
           02  syst-data1         pic x(40).
           02  syst-data2         pic x(40).
           02  filler             pic x(4)   value " -d.".
           02  filler             pic x      value x"00".

       01  zon-cde.
           02    cde-data    pic x(80).
           02    filler      pic x              value x'00'.
       01  ZONES.
DDE153     02 wtaro.
             03 wt1          pic x.
             03 filler       pic xx.
DDE153* memo ht par commande
           02 w-fbht1f             pic s9(11)v99.
           02 w-fbht2f             pic s9(11)v99.
           02 w-fbnmf              pic s9(11)v99.
DDE153* memo cle cde traitee pour rupture regroupement
           02 w-cleregroup.
              03 w-facturea        pic 9(6).
              03 w-reglepar        pic 9(6).
              03 w-regrfa          pic x.
              03 w-dev             pic 99.
              03 w-geo             pic x.
              03 w-taxe            pic x.
              03 w-foa             pic x.
DDE153* memo type trt regroupement et memo creation facture
           02 w-regroup            pic x.
           02 w-creat              pic x.
DDE153* memo code reglement
           02 w-regl               pic 99.
DD0351* memo reglement et condition pieds de facture
           02 w-fbreg              pic 99.
           02 w-fbcjl              pic 9(5).
DDE153* memo code surveillance et gencod client regle et representant
           02 w-suc                pic 9.
           02 w-gen                pic 9.
           02 w-repr               pic 9(3).
DD0394* type commande
DD0394     02 w-type               pic x.
DD0350* flag pour edition facture laser
           02 w-flag-edit          pic 9.

DD0316     02 wlnfac  pic 9(7).
      *DDE089
      * nom pgm pour vali1
           02  wnom-prog                PIC X(10) value 'cilpfact1'.
ELGU17     02  wgeo-livrea              pic x.
ELGU17     02  wgeo-facturea            pic x.
      * memo code douanier
ELGU17     02  w-FCNPO                  pic x(9).
      * memo date du jour
           02 wdatej              pic 9(6).
           02 filler redefines wdatej.
             03 waj               pic 99.
             03 wmj               pic 99.
             03 wjj               pic 99.
      * numero de commande a la demande
           02 wcdex.
DD0316        03 wcde        pic 9(7).
DDE153        03 wind        pic 9.
DD0338* numero de client a facturer
DD0338     02 wclientx.
DD0338        03 wclient     pic 9(6).
      * trt des erreur avec reponse par accept
           02 wtrt           pic x.

      *----> M1298 (D)
           02 wcec           pic x.
      *----> M1298 (F)

      *----> M1198 (D)
      ***************** pour l'arrondi sur la 3eme decimale
           02 t4                pic 9(5) value 00050.
           02 ta4 redefines t4.
             03 ar              pic 9v9(4).
           02 wcale             pic 9(9)v9(4).
           02 wzca              pic 9(9)v99.
           02 wfbnpf         pic s9(7)v99.
           02 wpgbceu        pic x.
           02 wpgbteu        pic 9(5)v9(6).
           02 wpgbt62        pic 9(5)v9(6).
      *----> M1198 (F)
           02 WDEV           PIC 99.
           02 WLDE           PIC X(15).
           02 WDATE.
             03 WJ           PIC 99.
             03 WM           PIC 99.
             03 WA           PIC 99.
           02 WDATE9  REDEFINES WDATE  PIC 9(6).
           02 WENRTVA1.
             03 WTAUPA1      PIC 99V999.
             03 WTVAX1.
               04 WTVA1      OCCURS 5  PIC  99V999.
           02 WENRTVA2.
             03 WTAUPA2      PIC 99V999.
             03 WTVAX2.
               04 WTVA2      OCCURS 5  PIC 99V999.
           02 WAD.
             03 WAD1         PIC X(26).
             03 WAD2         PIC X(26).
             03 WAD3         PIC X(26).
             03 WAD4         PIC X(26).
             03 WAD5.
               04 WCODP5     PIC 9(5).
DD0188         04 filler     pic x.
DD0188         04 WBD5       PIC X(30).
             03 WAD6         PIC X(26).
             03 WAD7.
               04 WCODP7     PIC 9(5).
DD0188         04 filler     pic x.
DD0188         04 WBD7       PIC X(30).
           02 WORIDUP        PIC X(12).
           02 WFACAVO        PIC X(12).
           02 WPROCON        PIC X(12).
           02 WDAE           PIC 9(6).
      * wtest1 ==> test traitement pied de facture (pas de pied pour les
      *            conditionnelles et proforma
           02 WTEST1         PIC 9.
           02 WTEST2         PIC 9.
           02 WTEST3         PIC 9.
           02 WTEST4         PIC 9.
           02 WTEST5         PIC 9.
           02 WTEST6         PIC 9.
           02 WRCL.
             03 WDA          PIC X(4).
DD0221*      03 WREF         PIC X(9).
DD0221       03 WREF         PIC X(15).
           02 WLREF.
             03 FILLER       PIC X.
             03 WDATC        PIC X(4).
             03 FILLER       PIC X.
DDE130*      03 WNCDE        PIC X(9).
DDE130       03 WNCDE        PIC X(17).
             03 FILLER       PIC X.
             03 WCOTA        PIC 9B.
             03 WTARI        PIC 999B.
             03 WPORT        PIC X(8).
             03 FILLER       PIC X.
             03 WEXPE        PIC X(14).
DD0316       03 FILLER       PIC X(3).
DD0316       03 WLNUM        PIC 9(7).
             03 WLNIN        PIC 9B.
             03 WSECT        PIC 999BB.
DDE130*      03 WNCL         PIC 9(6)B.
DDE130       03 WNCL         PIC 9(6).
DDE130*      03 WDATEF       PIC X(6).
DDE130       03 WDATEF.
DDE130          04 WDATJ       PIC 99.
DDE130          04 wdatm       pic 99.
DDE130          04 wdata       pic 99.
DDE130*      03 FILLER       PIC X.
DDE130*      03 WLNFAC       PIC 9(5).
DDE130*      03 FILLER       PIC X.
           02 WNCLP          PIC 9(6).
           02 WGEOP          PIC 9.
           02 WGEOL          PIC 9.
           02 WCODTAX1       PIC 99.
           02 FILLER         REDEFINES WCODTAX1.
             03 WPAR1        PIC 9.
             03 WTV1         PIC 9.
           02 WCODTAX2       PIC 99.
           02 FILLER         REDEFINES WCODTAX2.
             03 WPAR2        PIC 9.
             03 WTV2         PIC 9.
           02 WWCLE.
             03 WNEL         PIC 99.
             03 WNLIG        PIC 99.
DDE046       03 WART1        PIC x(7).
DDE130       03 wsref        pic x(2).
DD9999* ajout du prix
DD9999       03 wpri1        pic 9(6)v99.
           02 WWWCLE.
             03 WWNEL        PIC 99.
             03 WWNLIG       PIC 99.
DDE046       03 WWART1       PIC x(7).
DDE130       03 wwsref       pic x(2).
DD9999* ajout du prix
DD9999       03 wwpri1       pic 9(6)v99.
           02 wadcnt.
             03 wadncl       pic 9(6).
DDE046       03 wadnar       pic x(7).
DDE046       03 wadsrf       pic x(2).
           02 wart2          pic 99.
DD0424*    02 WCRE           PIC 9.
           02 WDES           PIC X(30).
           02 WMES           PIC 99.
           02 WPRX           PIC 9.
           02 WQPB           PIC 9(4).
           02 WPHT           PIC 9(6)V99.
           02 WRE1           PIC X(8).
           02 WQTELIV        PIC 9(10)V99.
           02 FILLER         REDEFINES WQTELIV.
             03 WENT         PIC 9(10).
             03 WDEC         PIC 99.
           02 WCLE.
DD0316       03 WNUM         PIC 9(7).
             03 WNIN         PIC 9.
           02 XCLE.
DD0316       03 XNUM         PIC 9(7).
             03 XTIR         PIC X.
             03 XNIN         PIC 9.
           02   WPARAM.
             03 WPARP        PIC X(8)           OCCURS 9.
       01  ZONES2.
           02 WMT4           PIC S9(7)V99       OCCURS 2.
           02 WMT5           PIC S9(7)V99       OCCURS 2.
           02 WMT6           PIC S9(7)V99       OCCURS 2.
           02 WMT8           PIC S9(7)V99.
           02 WMT9           PIC S9(7)V99       OCCURS 2.
       01  ZONES3.
DDE153*M280302 memo code transport pour edition entete + code demat
           02 wfcvia           pic x.
           02 wfcfdem          pic x.

           02 WTAUX          PIC 9(6)V99.
           02 WTEC.
             03 WEEC         OCCURS 3.
               04 WDECH      PIC 9(6).
               04 WEECX      REDEFINES  WDECH.
                 05 WCDEJ    PIC 99.
                 05 WCDEM    PIC 99.
                 05 WCDEA    PIC 99.
               04 WTXE       PIC 99V99.
           02 WCONX.
DD0438       03 WCON1        PIC 9(3).
             03 WCON2        PIC 99.
DD0438     02 WCON           REDEFINES WCONX  PIC 9(5).
           02 WREPONS.
             03 WREPONS1.
               04 WREP1      PIC 99.
             03 WREP2X.
               04 WREP21     PIC 99.
               04 WREP22     PIC 99.
             03 WREP2        REDEFINES WREP2X  PIC 9(4).
           02 WDATECH9       PIC 9(6).
           02 FILLER         REDEFINES  WDATECH9.
             03 WLJ          PIC 99.
             03 WLM          PIC 99.
             03 WLA          PIC 99.
           02 WDOM.
             03 WCOB         PIC X(5).
             03 WCOG         PIC X(5).
             03 WNCB         PIC X(11).
             03 WKRI         PIC XX.
           02 WDOA           PIC X(24).
           02 WPGCLE.
             03 WPGCLE1      PIC X(8).
             03 WPGCLE2      PIC 99.
           02 WREPX.
      *----> M1198 (D)
      *      03 WREP3        PIC 99V9(6).
             03 WREP3        PIC 9(5)V9(6).
      *----> M1198 (f)
      *****02 WSTAT          PIC XX.
           02 WBTED          PIC X(17).
           02 WNEL9          PIC 99.
           02 WCPT7          PIC 9.
           02 WREMX.
             03 WREM7        OCCURS 5.
               04 WLIB7      PIC X(30).
               04 WQUI       PIC S9(6)V99.
DD0337         04 wbas7      pic 9(6)v99.
  -            04 wmon7      pic 9(6)v99.
DD0337         04 wcle7      pic x(13).
DD0316     02 WNFA9          PIC 9(7).
           02 WREM1          PIC S9(9)V99.
           02 WREM2          PIC S9(9)V99.
           02 WWNCL          PIC 9(6).
      *
           02 WDNEL          PIC 99.
DD0316     02 WDNUM          PIC 9(7).
           02 WDNIN          PIC 9.
           02 WCOREG         PIC 9(4).
           02 WDIREG         PIC 9(4).
           02 WMUREG         PIC 9(4).
           02 WRESTE         PIC 99.
           02 WITC           PIC 9.
           02 WNAF           PIC 9.
           02 WNOF           PIC 9(6).
      ***  AJOUTS POUR TT INTRACOMMUNAUTAIRE                ***
           02 WNIC.
             03 WPAY         PIC XX.
             03 WCLN         PIC XX.
             03 WSIR         PIC X(11).
           02 WPDU           PIC 9(6).
           02 WPDX           REDEFINES      WPDU.
             03 WPDS         PIC 999V999.
       01       xart.
DDE046     02   xnar         pic x(7).
DDE046     02   xsre         pic x(2).
DD0350 01  wnlg              pic 9(4).
DD0350 01  wsaut3            pic 9.
       01       wver         pic xxx.
       01  wdopx.
           02 wdop1             pic x(16).
           02 wdop2x.
DDE046        03 wdop2          pic x(7).
              03 filler         pic x.
           02 wdop3             pic 99.
DDE046 01  wnart                pic x(7).
DDE046 01  wnsrf                pic x(2).
       01  wi                   pic 9.
       01  wgdes.
           02 wgtyp             pic 9.
           02 wgnou             pic 9(6).
      *M1096 -----------------------------------------------------------------
       01  walliv               pic 9(6).
      *M1096 -----------------------------------------------------------------
      *M0197 -----------------------------------------------------------------
       01  walfac               pic 9(6).
      *M0197 -----------------------------------------------------------------
 

DD0316 01  wtout.
      *       memo enreg pied de facture pour controle identique qd reedition
DD0316   copy "../copy/wor-ffacture.mod" replacing ==(pref)== by ==ww==.
      *
       LINKAGE SECTION.
           copy '../copy/cilp-fact.com'.                                *GPICMT
           copy "/usr/action/ADL/copy/wor-adl".
      *
       PROCEDURE DIVISION using cilp-fact adl-art.
       DEB SECTION.
       D10.

DD0358* recherche infos societe
  -        move mmdt-societe to immpa-soci-societe
DD0358     call 'mmpa-soci1' using mmpa-soci adl-art


      *DDE089
      * reedition terminee ==> ferm = 'F' ==> fermeture des fichiers
           move spaces to immaf-vali-tit.
           if icilp-fact-ferm = 'F' go to fin.
       D10.

      *DDE089
           move spaces to wtrt
           move '0' to ocilp-fact-rtn
           move spaces to ocilp-fact-liberr.
      * recuperation date du jour                                       *GPICMT
           move 'D' to immti-date-taj
           call 'mmti-date1' using mmti-date adl-art
           if ommti-date-rtn not = '0'
              move ommti-date-rtn    to ocilp-fact-rtn
              move ommti-date-liberr to ocilp-fact-liberr
              perform erreur
              go to fin
           end-if
           move wmmti-date-amj to wdatej
           .


      *----> M0797 (D)
      *    move "chmod 766 fcomma*" to cde-data.
      *    call 'systcc' using zon-cde sysrtn.
      *    if sysrtn not = 0 display "ERREUR/Chmod 766 FCOMMA..".
      *----> M0797 (F)
DD0337     move 'W' to gfkey.
           perform op-fcommac4.
           move 'W' to gfkey.
           perform op-parbatch.
           move 'W' to gfkey.
           perform op-ffacture.
           move 'W' to gfkey.
           perform op-fcommaap.
      *----> M0797 (D)
      *    move "chmod 444 fcomma*" to cde-data
      *    call 'systcc' using zon-cde sysrtn.
      *    if sysrtn not = 0 display "ERREUR/Chmod 444 FCOMMA..".
      *----> M0797 (F)
      *****OPEN EXTEND INTCO.
           move 'E' to gfkey.
           perform op-intracom.

      *DDE089 ouverture etat une seule fois en cas de reedition
           IF icilp-fact-ferm = spaces
              string 'ADLPID' x'00' delimited by size
                  into var-name
              move space to var-data
              call 'genvcc' using var-name var-data
DDE089        if icilp-fact-e1dem = 'D'
                 string 'cilpfacd' mmdt-lieu '.'
                      var-data delimited by ' '
                                     into wlabel-etat
              else
                 string 'cilpfact' mmdt-lieu '.'
                      var-data delimited by ' '
                                     into wlabel-etat
              end-if
              OPEN OUTPUT ETAT
           END-IF.

      *
      *---------------------------------------------                    *GPICMT
      * controle si facturation en lot ou a la demande                  *GPICMT

GPICMT* pour edition par assistante demande numero de client et numero de commande
DD0338     if icilp-fact-e1red = 'A'                                    *GPICMT
  -   * on met "#" ds code trt de vali1 pour attendre une reponse au    *GPICMT
  -   * clavier en cas d'erreur par vali1                               *GPICMT
  -           move '#' to wtrt                                          *GPICMT
  -           perform saisie                                            *GPICMT
  -           if ocilp-fact-rtn not = cmmdt-envi-rtn-ok                 *GPICMT
  -              go to fin
  -           end-if
DD0338     end-if

           if icilp-fact-e1nocdecx = zero or = spaces                   *GPICMT
              go to trt-console                                         *GPICMT
           else
              move icilp-fact-e1nocdecx to wcdex
           end-if
      * facture a la demande: verification date si non donnee on force  *GPICMT
      *                       avec la date du jour                      *GPICMT
      *---------------------------------------------                    *GPICMT
           if icilp-fact-e1date = zero or = spaces
              move 'D' to immti-date-taj
           else
              move 'J' to immti-date-taj
           end-if
           move icilp-fact-e1date to wmmti-date-jma
           call 'mmti-date1' using mmti-date adl-art
           if ommti-date-rtn not = '0'
              move ommti-date-liberr to immaf-vali-tit
              move ommti-date-rtn    to ocilp-fact-rtn
              perform erreur
              go to fin
           end-if
           move wmmti-date-jjour to wj
           move wmmti-date-jmois to wm
           move wmmti-date-janne to wa
           go to trt-deb.

      *
       trt-console.
DDE089* on met "#" ds code trt de vali1 pour attendre une reponse au    *GPICMT
      * clavier en cas d'erreur par vali1                               *GPICMT
           move '#' to wtrt.

           move spaces to wcdex
           DISPLAY "EDITION DES FACTURES,T=toutes,99999999=N.cde,F=FIN".
           accept wcdex
           if wcdex = 'F' or = 'f'
              go to fin
           end-if
           if wcdex = 'T' or = 't'
              move zero to wcdex
              go trt-console-s
           end-if
           if wcdex = spaces or = zero or wcdex not numeric
              go to trt-console
           end-if
           .
       trt-console-s.
           MOVE ZERO TO WJ WM WA.
           DISPLAY "ENTRER DATE DE FACTURE: <JJMMAA>,F=FIN".
           ACCEPT WDATE.
           if wdate = 'f' or 'F' go to fin.

      *DDE089 ajout controle date 5 jour avant ou apres si # ==> message
      *       avec demande confiramtion par reponse HOR
      * date facture doit etre > date jour -5
           move 'S'               to immca-date-trt
           move waj to immca-date-janne
           move wmj to immca-date-jmois
           move wjj to immca-date-jjour
           move 5   to immca-date-nbj
           call 'mmca-date1' using mmca-date adl-art
           IF ommca-date-rtn = "0"                                      *GPICMT
              move tmmca-date-amj to immti-date-damj                    *GPICMT
              move wa             to immti-date-faanne
              move wm             to immti-date-famois
              move wj             to immti-date-fajour
              move "< "           to immti-date-trt                     *GPICMT
              move "N"            to immti-date-zer
              call 'mmti-date0' using mmti-date adl-art
              if ommti-date-rtn not = "0"                               *GPICMT
                 go to trt-hors
              else
      * date facture doit etre < date jour +5
                 move 'A' to immca-date-trt
                 move waj to immca-date-janne
                 move wmj to immca-date-jmois
                 move wjj to immca-date-jjour
                 move 5   to immca-date-nbj
                 call 'mmca-date1' using mmca-date adl-art
                 if ommca-date-rtn = "0"                                *GPICMT
                    move tmmca-date-amj to immti-date-damj              *GPICMT
                    move wa             to immti-date-faanne
                    move wm             to immti-date-famois
                    move wj             to immti-date-fajour
                    move "> "           to immti-date-trt               *GPICMT
                    move "N"            to immti-date-zer
                    call 'mmti-date0' using mmti-date adl-art
                    if ommti-date-rtn not = "0"                         *GPICMT
                       go to trt-hors
                    end-if
                 else
                    go to trt-hors
                 end-if
              end-if
           else
              go to trt-hors
           END-IF

      * date controle OK
           go to trt-console-s1.

       trt-hors.
           display '************************************************'
           display 'DATE FACTURE SAISIE HORS PERIODE !!!!!'
           display '************************************************'
           display "Date = " wj "-" wm "-" wa " - <hor> si ok"
           accept wver
           if wver not = "hor" and wver not = "HOR"
              go to trt-console
           else
              go to trt-console-s2
           end-if
           .

       trt-console-s1.
           display "Date = " wj "-" wm "-" wa " - <oui> si ok"
           accept wver
           if wver not = "oui" and wver not = "OUI"
              go to trt-console
           end-if
           .

       trt-console-s2.
           if wcdex = zero
              display 'Facturation de toutes les commandes a facturer'
           else
              display 'Facturation de la commande: ' wcdex
           end-if
           .
      *
      **** DEBUT TRAITEMENT ****
      *
       trt-deb.

      *DDE089 appel fonction recherche si parametre bloque
           move "FACTURE000" to immdt-parb-cle
           call 'mmdt-parb1' using mmdt-parb adl-art
           if ommdt-parb-rtn not = '0'
              move ommdt-parb-liberr to immaf-vali-tit
DD0350        move ommdt-parb-liberr to ocilp-fact-liberr
DD0350        move cmmdt-envi-rtn-err to ocilp-fact-rtn
DD0350*       perform erreur
              go to fin
           end-if

DDE089* recup devise de base en compta
           call 'mmcp-devb1' using mmcp-devb adl-art
           if ommcp-devb-rtn not = '0'
              move ommcp-devb-liberr to immaf-vali-tit
              move ommcp-devb-rtn    to ocilp-fact-rtn
              perform erreur
              go to fin
           .

      *----> M1198 (D)
      ****************** ajout lecture devise EURO (62)
      * appel fonction lecture devise
           move 'f' to immpa-devi-trt
           move 00  to wmmpa-devi-cdev9
           move 'C' to immpa-devi-tfc
DDE089     move 'L' to immpa-devi-aff
           call 'mmpa-devi1' using mmpa-devi adl-art
           if ommpa-devi-rtn not = '0'
              move ommpa-devi-liberr to immaf-vali-tit
              move ommpa-devi-rtn    to ocilp-fact-rtn
              perform erreur
              go to fin
           else
      * controle des taux
              if ommpa-devi-tcd = zero or ommpa-devi-teu = zero
                 move '2' to ocilp-fact-rtn
                 move "PARAMETRE DEVISE EURO (50) INCORRECT"
                          to immaf-vali-tit
                 move '2' to ocilp-fact-rtn
                 perform erreur
                 go to fin
              end-if
           end-if
           MOVE ommpa-devi-teu TO wpgbt62.
      *----> M1198 (F)


           if icilp-fact-e1red not = 'R'
DD0362        and icilp-fact-arc = spaces
              MOVE "FACTURE000" TO PHCLE
              perform r-parbatch
              if file-status not = zero
                 move "EL.FACTURE000 ABSENT" to immaf-vali-tit
                 move '3'                    to ocilp-fact-rtn
                 perform erreur
                 go to fin
              end-if
              MOVE PHANOC (1) TO WNFANA
              MOVE PHANOC (2) TO WNFACA
           END-IF.

           MOVE "PARAFITVA1" TO PGCLE.
           perform rnl-paramgpi.
           if file-status not = zero
              move "EL.TVA1 ABSENT" to immaf-vali-tit
              move '3'              to ocilp-fact-rtn
              perform erreur
              go to fin.
           MOVE PGFZON TO WENRTVA1.
           MOVE "PARAFITVA2" TO PGCLE.
           perform rnl-paramgpi.
           if file-status not = zero
              move "EL.TVA2 ABSENT" to immaf-vali-tit
              move '3'              to ocilp-fact-rtn
              perform erreur
              go to fin.
           MOVE PGFZON TO WENRTVA2.
           MOVE SPACES TO WPARAM.
           MOVE "AVANCE" TO WPARP (1) WPARP (3) WPARP (4) WPARP (5)
                            WPARP (6) WPARP (7) WPARP (8) WPARP (9).
           MOVE "FRANCO" TO WPARP (2).
           MOVE "CLIENPORT1"  TO PGCLE.
           perform rnl-paramgpi.
           if file-status not = zero
                     GO TO D30.
       D25.
           MOVE PGZON3 (3) TO WPARP (PGCL2n).
      *****READ PARAMGPI NEXT AT END    GO TO D30.
           perform nnl-paramgpi.
           if file-status not = zero
                   GO TO D30.
           IF PGCL1 NOT = "CLIENPORT"  GO TO D30.
           IF PGCL2 NOT NUMERIC         GO TO D30.
           GO TO D25.
      *
      **** SEANCE DE CADRAGE ****
      *
       D30.
      *DDE089 pour une facture a la demande ==> pas de cadrage
           if wcdex not = zero and not = spaces
              go to t10
           end-if
           .
       d31.

           MOVE SPACE TO LIGNE.
           compute wnbl = 10 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L10.
           MOVE ALL "*" TO LNOMPAY.
           compute wnbl = 19 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L19.
           MOVE SPACE TO LIGNE.
           MOVE ALL "X" TO WDATC WNCDE WDATEF.
           WRITE LIGNE FROM WLREF BEFORE 3.
           MOVE SPACE TO LIGNE.
           MOVE 9999999 TO LNREF.
           MOVE 999999 TO LUV LNBTES LMONT.
           compute wnbl = 60 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L60.
           MOVE SPACE TO LIGNE.
           MOVE 999999 TO LNETMA LTAU1 LMTTVA1 LNETPAY.
           WRITE LIGNE BEFORE 3.
           MOVE SPACE TO LIGNE.
           MOVE 999999 TO LJEE LMEE LAEE LNCLIP.
      *    WRITE LIGNE BEFORE 1.
           WRITE LIGNE BEFORE page.
           MOVE SPACES TO LIGNE.
       D40.
           PERFORM D31.
      *
      **** TRAITEMENT FACTURATION ****
      *
       T10.
           MOVE 0 TO WTOTAV WTOTFA.
DD0316     MOVE ZERO TO FCCLE-cdesup

      *DDE089 lecture directe de la commande qd reedition avec controle
      *       deja facturee
           if icilp-fact-e1red = 'R'
              and icilp-fact-arc = spaces
DDE153* on lit les commandes des sur cle facture/numero cde
DD0316        move spaces to fccle-cdesup
DD0316        move wcde to fcnfa-cdesup
DDE153        perform snlsk4-fcommaap
DDE153        if file-status = zero
DD0351           perform nnl-fcommaap
DDE153        end-if
DD0316        if file-status not = zero or fcnfa-cdesup not = wcde
DDE153           string 'FACTURE NON TROUVEE: ' wcde
                     delimited size into immaf-vali-tit
                 move '3'               to ocilp-fact-rtn
                 perform erreur
                 go to fin
              end-if
DD0316        move fcnfa-cdesup to wlnfac
DDE153        move zero to wcdex
DDE153        perform ini-fac
DDE153        perform ini-cde
DDE153        go to t20a
           END-IF.

      *----> DDE027 (D)
      * lecture fcommaap sur cle 3 ==> seulement les a facturer
      * fcafa = 1 ==> a facturer
      * fcfac = 0 ==> pas facturer apres facturation passe a 1
DDE153* si trt BL non regroupee ou pour un BL donne lecture sur cle 3
      * sinon lecture sur cle 5 dans l'ordre des clients
          if icilp-fact-arc = "X"
             move 0 to fcafa fcfac
          else
             move 1 to fcafa 
             move 0 to fcfac 
          end-if
DD0316     move wcdex to fccle-cdesup.
           if icilp-fact-e1regroup not = '1'
              perform snlsk3-fcommaap
           ELSE
              move 1 to fcafa
              move 0 to fcfac fcdev fcgeo fcfoa fcdi2
DD0316        move spaces to fcclefac fccle-cdesup
              perform snlsk5-fcommaap
           END-IF

           if file-status not = zero
              string 'RIEN A FACTURER, status: (' file-status ' )'
                     delimited size into immaf-vali-tit
              move '3'            to ocilp-fact-rtn
              perform erreur
              go to fin.
      *----> DDE027 (F)

GPICMT* lecture non blocante si reeditiion
DD0351     if icilp-fact-e1red = 'R'
              and icilp-fact-arc = spaces
              perform nnl-fcommaap
           else
              perform n-fcommaap
           end-if
           if file-status not = zero
              string 'RIEN A FACTURER, status: (' file-status ' )'
                     delimited size into immaf-vali-tit
              move '3'            to ocilp-fact-rtn
              perform erreur
              go to fin.
      * controle facture a la demande
           if wcdex not = spaces and not = zero
DD0316        if fccle-cdesup not = wcdex
                 string 'Commande inexistante ou facturee : 'wcdex
                    delimited size into immaf-vali-tit
                 move '2' to ocilp-fact-rtn
                 perform erreur
                 go to fin
              end-if
           END-IF.
      *
      **** DEBUT FACTURE ****
      *
       T20.

      *-------------------------------------                            *GPICMT
      * si fcafa > 1 ==> fini
      * si fcfac > 0 ==> fini
           if fcafa > 1 or fcfac > 0 go to fin1.                        *DDE027

DDE153* init debut trt d'une facture
           perform ini-fac.

DDE153* test si factures a regrouper demande
           IF icilp-fact-e1regroup = '1'
              if fcregrfa not = '1'
                 go to t420
              end-if
           ELSE
              if fcregrfa = '1'
                 go to t420
              end-if
           END-IF


      * controle facture a dematerialiser si demander sauf si facture a *GPICMT
      *                                               la demande        *GPICMT
           if wcdex = zero                                              *GPICMT
              if fcfdem = "1"                                           *GPICMT
                 if icilp-fact-e1dem not = 'D'                          *GPICMT
                 go to t420
                 end-if
              else
                 if icilp-fact-e1dem = 'D'
                    go to t420
                 end-if
              end-if
           END-IF
           .
      *-------------------------------------                            *GPICMT
DDE153 t20-s.
           perform ini-cde.

DD0362   if icilp-fact-arc not = spaces
            go to t20a
         end-if
           IF FCAFA NOT = 1 GO TO T420.
           IF FCLIV NOT = 2 GO TO T420.
           IF FCFAC NOT = ZERO GO TO T420.
           IF FCICP NOT = ZERO GO TO T420.

       t20a.
      *DDE089 controle code reglement                                   *GPICMT
           move 'C' to immpa-regl-cof
           move fcreg to wmmpa-regl-regl
DD0387     move spaces to immpa-regl-choix
           call 'mmpa-regl1' using mmpa-regl adl-art
           if ommpa-regl-rtn not = '0'
              move ommpa-regl-rtn    to ocilp-fact-rtn
DD0316        string 'Cde: ' fccle-cdesup ' ' ommpa-regl-liberr
                   delimited size into immaf-vali-tit
              perform erreur
              go to t420
           end-if
      *
      * controle Taux de la devise de la commande
      * appel fonction lecture devise
           move 'f' to immpa-devi-trt
           move fcdev  to wmmpa-devi-cdev9
           move 'C' to immpa-devi-tfc
DDE089     move ' ' to immpa-devi-aff
           call 'mmpa-devi1' using mmpa-devi adl-art
           if ommpa-devi-rtn not = '0'
              string 'Cde: ' fccle-cdesup ' ' ommpa-devi-liberr
                   delimited size into immaf-vali-tit
              move ommpa-devi-rtn    to ocilp-fact-rtn
              perform erreur
              go to t420
           else
      ***************** si taux en euro = zero on ne traite pas la commande
              if ommpa-devi-teu = zero
                 string 'Cde: ' fccle-cdesup ' ' "TAUX DEVISE ???? :  "
                   delimited size into immaf-vali-tit
                 move '2' to ocilp-fact-rtn
                 perform erreur
                 go to t420
              end-if
           END-IF.
           move ommpa-devi-teu  to wpgbteu.
           move ommpa-devi-ceu  to wpgbceu.
           move ommpa-devi-ldev to wlde.

      *----> M1298 (D)
           move spaces to wcec.
           move fcncl to clncl.
           perform rnl-fclients.
           if file-status not = zero
              string 'Cde: ' fccle-cdesup ' Client: ' fcncl
                     ' inexistant (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocilp-fact-rtn
              perform erreur
              go to t420
           end-if

DD0002*GPICMT on prend le code calcul echeance sur le client payeur
DD0002*    if clnaf = zero or clnaf = 3 or clnuf = zero
DD0002*                              move clcec to wcec
DD0002*                              go to  t20f.
DD0002*    move clnuf to clncl.
DD0002*    perform rnl-fclients.
DD0002*    if file-status not = zero
DD0002*       string 'Cde: ' fccle-cdesup ' Client: ' fcncl ' inexistant'
DD0002*              ' (' file-status ')'
DD0002*              delimited size into immaf-vali-tit
DD0002*       move '3' to ocilp-fact-rtn
DD0002*       perform erreur
DD0002*       go to t420
DD0002*    end-if

DD0002*    move clcec to wcec.
DD0002*t20f.
      *----> M1298 (F)
           MOVE ZERO TO WNCLP.
DD0316     MOVE FCCLE-cdesup TO WCLE.
DD9999*    MOVE ZERO TO WTEST1.
           MOVE FCNBF TO WNBF WWNBF.
ELGU17* pour les proformat on force le nombre d'eemplaire a 1
  -        if fcfoa = 9
  -           move 1 to WNBF WWNBF
EGGU17     end-if
           MOVE FCDAE TO WDAE.
           MOVE FCITC TO WITC.
           MOVE FCNAF TO WNAF.
           MOVE FCNOF TO WNOF.
           MOVE FCGEO TO WGEOL.
DDE153     move fcreg to w-regl
GPICMT* en reedition on prend le mode de reglement et les conditions dans ffacture
DD0351     if icilp-fact-e1red = 'R'
              and icilp-fact-arc = spaces
  -           move w-fbreg to w-regl
  -           move w-fbcjl to WCON
DD0351     end-if
      *
      ** MISE EN WSS DE L'ENTETE **
      *
           MOVE SPACE TO WAD WORIDUP WFACAVO WPROCON.
           MOVE ZERO TO WCODP5 WCODP7.
      ***  TT DES NOUVEAUX BLOCS ADRESSES  (JUIN 90)                 ***
DDE153     move fcfacturea to walfac.
      *M1096 -----------------------------------------------------------------
           move zero to walliv.
      *M1096 -----------------------------------------------------------------
      *M0197 -----------------------------------------------------------------
DDE153*    move zero to walfac.
      *M1096 -----------------------------------------------------------------
DDE153     move fcfacturea to walfac.
      *****READ FCOADCLI INVALID GO TO T25.
DD0002*    perform r-fcoadcli.
DD0002*    if file-status not = zero
DD0002*          GO TO T25.
      *M1096 -----------------------------------------------------------------
DD0002*    move alliv to walliv.
      *M1096 -----------------------------------------------------------------
      *M0197 -----------------------------------------------------------------
DD0002*    move alfac to walfac.
      *M0197 -----------------------------------------------------------------
DD0002*    IF ALADC = "C"        GO TO T25.
      ***  TT DU LIVRE A                                             ***
DD0002*    MOVE ALLIV TO CLNCL.
DD0002* appel fct bloc adresse commande
DD0316     move fccle-cdesup to icmcd-gest-numcde
ELGU17     move '0' to wgeo-facturea wgeo-livrea
           call "cmcd-gest1" using cmcd-gest adl-art
           if ocmcd-gest-rtn = cmmdt-envi-rtn-ok
V20002           MOVE ocmcd-gest-livrea-nom TO WAD6
V20002           MOVE ocmcd-gest-livrea-bureau TO wad7
V30002           move ocmcd-gest-livrea to walliv
V30002           MOVE ocmcd-gest-facturea-nom TO WAD1
V30002           MOVE ocmcd-gest-facturea-raison TO WAD2
V30002           MOVE ocmcd-gest-facturea-rue TO WAD3
V30002           MOVE ocmcd-gest-facturea-ville TO WAD4
V30002           MOVE ocmcd-gest-facturea-bureau TO WAD5
GPICMT* recherche code geographique du livre a
ELGU17           perform rech-geo
GPICMT* controle validite code taxe avec code pays
DD0448        if mmdt-societe not = 'SLOVAQ' and not = 'CHINE'
  -              perform ctrl-taxe
  -              if ommpa-vtax-rtn not = cmmdt-envi-rtn-ok
  -                 perform erreur
  -                 go to t420
  -              end-if
DD0448        end-if
           else
              string 'Cde: ' fccle-cdesup ' Client Livre Inexistant'
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocilp-fact-rtn
              perform erreur
              go to t420
           end-if
           .

      ***  TT DU FACTURE A                                           ***
DD0002*    IF ALFAC = CLNCL GO TO T21.
DD0002*    MOVE ALFAC TO CLNCL.
DD0002     IF walfac = CLNCL GO TO T21.
DD0002     MOVE walfac TO CLNCL.
           perform r-fclients.
           if file-status not = zero
DD0316        string 'Cde: ' fccle-cdesup ' Client: ' walfac
                     ' inexistant (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocilp-fact-rtn
              perform erreur
              go to t420
           end-if
           .
       T21.

DDE153* si regroupement on prend le nbre de facture et- le numero de tarif
DDE153* du facture a et non celui de la commande en cours
DDE153     if fcregrfa = '1'
              move clnbf to wwnbf
ELGU17* pour les proformat on force le nombre d'eemplaire a 1
  -           if fcfoa = 9
  -              move 1 to WWNBF
EGGU17        end-if
              move cltaro to wtaro
           end-if

      ***  TT DU REGLE PAR                                           ***
DD0002*    IF ALREG = CLNCL GO TO T22.
DD0002*    MOVE ALREG TO CLNCL.
DD0002     IF fcreglepar = CLNCL GO TO T22.
DD0002     MOVE fcreglepar TO CLNCL.
           perform r-fclients.
           if file-status not = zero
DD0316        string 'Cde: ' fccle-cdesup ' Client: ' fcreglepar
                     ' inexistant'
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocilp-fact-rtn
              perform erreur
              go to t420
           end-if
           .

       T22.
           MOVE CLNCL TO WNCLP.
           MOVE CLGEO TO WGEOP.
DD0424*    MOVE CLCRE TO WCRE.
           MOVE CLCON TO WCON.
           MOVE CLDOM TO WDOM.
           MOVE CLDOA TO WDOA.
DD0002     move clcec to wcec.
DD9999     move clcrt to w-regl
GPICMT* en reedition on prend le mode de reglement et les conditions dans ffacture
DD0351     if icilp-fact-e1red = 'R'
              and icilp-fact-arc = spaces
  -           move w-fbreg to w-regl
  -           move w-fbcjl to WCON
DD0351     end-if

           move clcsu to w-suc
           move clgen to w-gen
           .
       T40.
      * DDE089 controle conditions de reglement
DD0438     move 'C' to immpa-regl-choix
DD0438     move wcon to wmmpa-regl-con9
           call 'mmpa-regl1' using mmpa-regl adl-art
           if ommpa-regl-rtn not = '0'
              string 'Cde: ' fccle-cdesup ' Cond. de reglement ???'
                             delimited size into immaf-vali-tit
              perform erreur
              go to t420
           end-if

DDE153     if w-regroup = '1' go to lececc1.
DDE153     move fcregrfa to w-regroup.

           IF FCFOA = 5 OR FCFOA = 6 OR FCFOA = 7
                      MOVE "   AVOIR" TO WFACAVO  ELSE
                      MOVE "  FACTURE" TO WFACAVO.
           IF FCFOA NOT = 1 AND FCFOA NOT = 6 GO TO T50.
           MOVE "CONDITIONNEL" TO WPROCON.
           MOVE 1 TO WTEST1.
           GO TO T60.
       T50.
           IF FCFOA NOT = 9 GO TO T60.
           MOVE " PRO-FORMAT" TO WPROCON.
           MOVE 1 TO WTEST1.
       T60.
           MOVE "*        *" TO WORIDUP.
      *
      ** MISE EN WSS LIGNE REFERENCE **
      *
           MOVE FCRCL TO WRCL.
DDE153     move spaces to wlref
DDE153     IF w-regroup not = '1'
              MOVE WDA TO WDATC
              MOVE WREF  TO WNCDE
              MOVE FCDI2 TO WCOTA
              MOVE FCTVE TO WTARI
              MOVE FCNTR TO WEXPE
DD0316*       MOVE FCNUM TO WLNUM
DD0316        MOVE fccle-cdesup(1:7) TO WLNUM
              MOVE FCNIN TO WLNIN
              MOVE FCREP TO WSECT
              MOVE FCNCL TO WNCL
              if FCFRA NOT = ZERO
                 MOVE WPARP (FCFRA) TO WPORT
              else
                 MOVE "AVANCE" TO WPORT
              end-if
           END-IF
           MOVE WDA TO WDATC.
           MOVE WREF TO WNCDE.
           MOVE FCDI2 TO WCOTA.
           MOVE FCTVE TO WTARI.
           IF FCFRA NOT = ZERO  MOVE WPARP (FCFRA) TO WPORT
                          ELSE  MOVE "AVANCE" TO WPORT.
           MOVE FCNTR TO WEXPE.
DD0316*    MOVE FCNUM TO WLNUM.
DD0316        MOVE fccle-cdesup(1:7) TO WLNUM
           MOVE FCNIN TO WLNIN.
           MOVE FCREP TO WSECT.
           MOVE FCNCL TO WNCL.
DDE130*    MOVE WDATE TO WDATEF.
DDE130     MOVE WJ TO WDATJ.
DDE130     MOVE WM TO WDATM.
DDE130     MOVE WA TO WDATA.

DDE089     IF icilp-fact-e1red not = 'R'
DD0362        and icilp-fact-arc = spaces
              if WTEST1 = 1 MOVE WNFACA TO WLNFAC
                            ADD 1 TO WNFACA
              else          MOVE WNFANA TO WLNFAC
                            ADD 1 TO WNFANA
              end-if
           END-IF.

           MOVE FCDEV  TO WDEV
      *----> M0797 (D)
           if fcdev = 00 and wgeol not = zero
                         move "FRANC FRANCAIS" to wlde
                         go to t61.
      *----> M0797 (F)
           IF FCDEV = 00 MOVE SPACES TO WLDE
                         GO TO T61.
           MOVE "DEVISE00" TO PGBRAC.
           MOVE FCDEV      TO PGBCOD.
      *****READ PARAMGPI INVALID MOVE ALL "?" TO PGBLDE.
           perform rnl-paramgpi.
           if file-status not = zero
                 MOVE ALL "?" TO PGBLDE.
           MOVE PGBLDE     TO WLDE.
      *----> M1198 (D)
      ***************** si taux en euro = zero on ne traite pas la commande
           if file-status not = zero or pgbteu = zero display
             "PARAMETRE DEVISE INCOMPLET :  "  fcdev "  COMMANDE N.:  "
                                               fccle-cdesup  go to t420.
           move pgbteu to wpgbteu.
           move pgbceu to wpgbceu.
      *----> M1198 (F)
      *
      ** INITIALISATION PIED DE FACTURE **
      *
       T61.
DDE153     move 1 to w-creat
DD0316     MOVE spaces TO wor-ffacture.
DD0316     MOVE WLNFAC TO fbcle-cdesup.
DD0316* pour pouvoir reediter une ancienne facture leau je charge
DD0316* la facture demandee
DD0316     if icilp-fact-e1red = 'R'
DD0316        move fcnfa-cdesup to fbcle-cdesup
DD0316     end-if
           MOVE WDATE9 TO FBDAF.
           MOVE FCFOA  TO FBCFA.
DD0316     MOVE FCCLE-cdesup  TO FBNCD-cdesup.
           MOVE WGEOL  TO FBPML.
           MOVE FCNCL  TO FBNCL.
           MOVE WGEOP  TO FBPMP.
           MOVE WNCLP  TO FBNCP.
DDE153*    MOVE FCREP  TO FBNRH.
DDE153     MOVE w-repr TO FBNRH.
DD0394     MOVE w-type TO FBTYP.
DD0424*    MOVE WCRE   TO FBREL.
           MOVE 9      TO FBIG9.
           MOVE FCDTR  TO FBDTR.
           MOVE FCTVE  TO FBTVE.
DDE153     MOVE FCTVE  TO wtaro.
           MOVE FCNAF  TO FBFIL.
           MOVE FCFRA  TO FBFRA.
           MOVE FCCTA  TO FBCOT.
DDE153*    MOVE FCREG  TO FBREG.
DDE153     MOVE w-regl TO FBREG.
           MOVE FCCSC  TO FBSUC.
           MOVE FCCOK  TO FBCOK.
           MOVE FCGEN  TO FBGEN.
           MOVE FCNCO  TO FBNCO.
           MOVE FCNUT  TO FBCNUT.
           MOVE FCPBR  TO FBPBR.
           MOVE FCCPA  TO FBCPA.
DDE153*    MOVE FCTQL  TO FBQTL.
           MOVE FCLIP  TO FBLIP.
           MOVE FCTHA  TO FBTRR.
           MOVE FCLIA  TO FBLIA.
           MOVE FCDEV  TO FBDEV.
           MOVE FCDI2  TO FBTAX.
           move fclpr  to fblpr.
           MOVE ZERO   TO FBCCO FBETR FBTRM FBJVT.
           MOVE ZERO   TO FBIL1 FBIL2 FBNEC FBTCO1 FBTCO2 FBPCO1 FBPCO2
                          FBRTD.
DD0465     MOVE spaces TO FBIMPRIM.
      *----> M0997 (D)
           move 1 to fbetr.
      *----> M0997 (F)
DDE153* init des zones propres a une commande si regroupement
           if w-regroup = '1'
DD0326*       move zero to fbncd fbfil fbcot fbtrr
DD0326        move zero to fbncd-cdesup fbfil fbcot fbtrr
              move walfac to fbncl
              move wtaro to fbtve
              move w-suc to fbsuc
              move w-gen to fbgen
              move spaces to fblia
           end-if
           .
       T62.
      ***** Instruction ajoutee sous UNIX ici        **********
      ***** Doit convenir pour Fact+Duplicata        **********
           move zeroes to wwwcle.
           MOVE 90     TO WCPTR.
           PERFORM TITRE THRU FTITRE.
           MOVE ZERO   TO WTEST3 WTEST4 WTEST5 WCPT7.
           MOVE ZERO   TO WCODTAX1 WCODTAX2.
           MOVE 0      TO FBNMF FBHT1F FBBF1F FBTX1F FBTP1F FBHT2F
                          FBBF2F FBTX2F FBTP2F FBPOF.
           MOVE 0      TO FBREF FBCOF FBAFF FBNPF FBE1F FBE2F FBE3F
                          FBNMD  FBHT1D FBBF1D.
           MOVE 0      TO FBTX1D FBTP1D FBHT2D FBBF2D FBTX2D FBTP2D
                          FBPOD  FBRED FBCOD FBAFD.
           MOVE 0      TO FBNPD FBE1D FBE2D FBE3D.
           MOVE ZERO   TO WMT4 (1) WMT4 (2).
           MOVE 0      TO WMT5 (1) WMT5 (2) WMT6 (1) WMT6 (2)
                          WMT8 WMT9 (1) WMT9 (2).
           MOVE zeroes TO WREMX.
           MOVE SPACE  TO WLIB7 (1) WLIB7 (2) WLIB7 (3) WLIB7 (4)
                          WLIB7 (5).

DDE153     MOVE zero   TO FBNCO.
DDE153     MOVE zero   TO FBPBR.
DDE153     MOVE zero   TO FBQTL.
      *
      **** AJOUT TRAITEMENT INTRACOMMUNAUTAIRE    ***
       T63.
           MOVE SPACES TO WNIC.
DD0448     MOVE fcreglepar  TO CLSNCL.
           perform r-clisuite.
           if file-status = zero
              move clsntv to wnic
           end-if
DD0448*    IF WGEOL NOT = 1 GO TO lececc1.
DD0448     IF WGEOL NOT = 1 and wgeop not = 1 GO TO edit-ltaxe.
      ***  Il ne faut pas creer intracom si pro-forma,mais on edite ***
      ***  les numeros gpi et client. (d'ou supp. ligne suivante    ***
      *    if fcfoa     = 9 go to lececc1.
           IF WTEST1    = 1 GO TO lececc1.
GPICMT* on prend le numero intracommunautaire du client payeur
DD0448*    MOVE FCNCL  TO CLSNCL.
      *M1096 -----------------------------------------------------------------
DD0448*    if walliv not = zero move walliv to clsncl.
      *M1096 -----------------------------------------------------------------
DD0448*    perform r-clisuite.
DD0448*    if file-status not = zero
DD0448*          go to t63a.
DD0448*    if clsntv not = spaces move clsntv to wnic go to t64.
DD0448*t63a.
DD0448*    move fcncl to clsncl.
DD0448*    if walfac not = zero move walfac to clsncl.
      * on doit editer le code intracom du reglepar si so pays = celui du livre
      * sinon on edite celui du livrea sauf pour les factures d'utillage ou le livre est gpi
DD0448     MOVE fcreglepar  TO CLSNCL
DD0448     if ocmcd-gest-reglepar-pays not = ocmcd-gest-livrea-pays
DD0448        and fcfeo not = ccmpa-tycd-typ-outil-ei(1:1)
DD0448        and fcfeo not = ccmpa-tycd-typ-outil-pr(1:1)
              move fclivrea to CLSNCL
           end-if
           perform r-clisuite.
           if file-status not = zero go to t64.
           move clsntv to wnic.
      *M0197 -----------------------------------------------------------------
       T64.
GPICMT* message si numero intracommunautaire vide
DD9999     if wnic = spaces
DD0316        string "Numero Intracom Vide,  Commande " fccle-cdesup
                     " traitee mais a reediter"
                    delimited size into immaf-vali-tit
              perform erreur
           end-if

           MOVE "FR"   TO LPAY.
DD0122*    MOVE "55"   TO LCLE.
DD0122*    MOVE "762200277" TO LSIR.
DD0122     MOVE "65"   TO LCLE.
DD0122     MOVE "379622160" TO LSIR.
           MOVE "NOTRE NUMERO INDIVIDUEL INTRACOMMUNAUTAIRE" TO LEXP.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           ADD 1       TO WCPTR.
           MOVE WPAY   TO LPAY.
           MOVE WCLN   TO LCLE.
           MOVE WSIR   TO LSIR.
           MOVE "VOTRE NUMERO INDIVIDUEL INTRACOMMUNAUTAIRE" TO LEXP.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           ADD  1      TO WCPTR.
DD0448 edit-ltaxe.
  -        evaluate fbtax
  -          when 6
  -           string "     "
  -                  "EXONERATION DE TVA EN FRANCE - ARTICLE 196 DE LA "
  -                  "DIRECTIVE 2006/112/CE" delimited size into ligne
  -           WRITE LIGNE BEFORE 2
  -           MOVE SPACES TO LIGNE
  -           ADD  2      TO WCPTR
  -          when 4
  -           string "     "
  -                  "EXONERATION DE TVA EN FRANCE - ARTICLE 262 TER.1 "
  -                "DU CGI " delimited size into ligne 
  -           WRITE LIGNE BEFORE 2
  -           MOVE SPACES TO LIGNE
  -           ADD  2      TO WCPTR
  -        evaluate fbtax
  -          when 3
  -             string "     "
  -                  "MARCHANDISE DEBITEE EN EXONERATION DE TAXES "
  -                  "DESTINEE A L'EXPORTATION" 
  -                       delimited size into ligne
  -             WRITE LIGNE BEFORE 2
  -             MOVE SPACES TO LIGNE
  -             ADD  2      TO WCPTR
  -          when 7
  -             string "     "
  -                    "MARCHANDISE DEBITEE EN EXONERATION DE TAXES"
  -                  delimited size into ligne
  -             WRITE LIGNE BEFORE 2
  -             MOVE SPACES TO LIGNE
  -             ADD  2      TO WCPTR
  -          when 8
  -             string "     "
  -                    "LIVRAISONS DE DECHETS NEUFS D'INDUSTRIE ET "
  -                    "MATIERES DE RECUPERATION"
                          delimited size into ligne
  -             WRITE LIGNE BEFORE 1
  -             MOVE SPACES TO LIGNE
  -             string "     "
  -                    "ARTICLE 283-2 SEXIES DU CGI TVA ACQUITTEE PAR "
  -                    "LE DESTINATAIRE " delimited size into ligne
  -             WRITE LIGNE BEFORE 1
  -             MOVE SPACES TO LIGNE
  -             string "     " wnic delimited size into ligne
  -             WRITE LIGNE BEFORE 2
  -             MOVE SPACES TO LIGNE
  -             ADD  4      TO WCPTR
DD0448     end-evaluate
           .
      ***  FIN 2 LIGNES EN TETE DE FACTURE      ***
      **** TRAITEMENT LIGNES DETAIL ****
      *
       LECECC1.
DDE153     if w-regroup = '1'
              perform entete-cde
           end-if

      ********     lecture    des elements 01 : libelles  ************
DD0316     move fccle-cdesup  to pfjcle1
           perform r-fcommac1.
           if file-status not = zero
                GO TO lecc2.
DD0316     if fccle-cdesup not = pfjcle1 go to lecc2.
      *T65.
      *****READ FCOMMDES COMPL AT END GO TO T420.
      *    perform n-fcommaap.
      *    if file-status not = zero
      *         GO TO T420.
       T70.
      *    IF FCNEL1 NOT = 3 GO TO T90.
      *
      ** TRT EL.3 **
      *
           IF pfjli1 = SPACE AND pfjli2 = SPACE
                     AND pfjli3 = SPACE AND pfjli4 = SPACE GO TO lecc2.
           MOVE ZERO TO I.
       T70A.
      *    ADD 1 TO I.
      *    IF I > 3 GO TO T70B.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           IF pfjli1 = SPACES GO TO T70A2.
           MOVE pfjli1  TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       t70a2.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           IF pfjli2 = SPACES GO TO T70A3.
           MOVE pfjli2  TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       t70a3.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           IF pfjli3 = SPACES GO TO T70B.
           MOVE pfjli3  TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
      *    GO TO T70A.
       T70B.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           IF pfjli4 = SPACE GO TO T70C.
           MOVE pfjli4 TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T70C.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
      *    GO TO T190.
      *
       lecc2.
      ********  lecture    des elements 04 : lignes articles   *******
           move fccle-cdesup  to fcnoc2-cdesup.
      *07/96----------------------------------------------------------->
           move zero to fclig.
      *07/96----------------------------------------------------------->
           move spaces to fcart.
           perform snlsk3-fcommac2.
           if file-status not = zero
                GO TO lecc4.
       lec2.
           perform n-fcommac2.
           if file-status not = zero move all "9" to wwwcle
                GO TO lecc4.
           if fcnoc2-cdesup not = fccle-cdesup go to lecc4.
      ** TRT EL.4 **
      *
       T90.
           move fcart to xart.
DD0350     move fcnlg to wnlg
           if wwwcle = all "9" go to lecc4.
      *    IF FCNEL2 NOT = 4 GO TO T120.
      *
      * TYPE DE LIGNE = 1 *
      *
       T92.
           IF FCLIG NOT = 1 GO TO T100.
           PERFORM TEST2b THRU FTEST2b.
           IF WTEST2 = 3 GO TO DISP.
      *
      **** DEBUT ARTICLE ****
      *
           MOVE ZERO   TO WQTELIV wta-qte.                              *M210601
           MOVE FCNEL2 TO WNEL.
           MOVE FCLIG  TO WNLIG.
           MOVE FCNAR  TO WART1.
           move fcsre  to wart2.
DDE130     move fcsre  to wsref.
DD9999     MOVE fcpht  TO wpri1.
           MOVE FCPDU  TO WPDU.
           MOVE FCDES  TO WDES.
           MOVE FCMES  TO WMES.
ELGU17     move FCNPO  TO w-FCNPO
           MOVE FCPRX  TO WPRX.
           MOVE FCQPB  TO WQPB.
           MOVE FCPHT  TO WPHT.
DD0362     if icilp-fact-arc = spaces
              IF FCQTL = ZERO
                 GO TO T92B 
              end-if
           end-if
      *
      ** MVTS ARTICLE **
      *
         .
       T92A.
DD0362     if icilp-fact-arc = spaces
              ADD FCQTL TO WQTELIV wta-qte                              *M210601
           else
              ADD FCQTc TO WQTELIV wta-qte
           end-if
         .
       T92B.
      *****READ FCOMMDES COMPL AT END MOVE ALL "9" TO WWWCLE
           perform n-fcommac2.
           if file-status not = zero
                MOVE ALL "9" TO WWWCLE
                                      GO TO lecc3.
      **** le test etait avant les  move : to lecc3.
           MOVE FCNEL2 TO WWNEL.
           MOVE FCLIG  TO WWNLIG.
           MOVE FCNAR  TO WWART1.
DDE130     move fcsre  to wwsref.
DD9999     MOVE fcpht  TO wwpri1.
           if fcnoc2-cdesup not = fccle-cdesup move all "9" to wwwcle
                                 go to lecc3.
DD0351*    IF WWCLE = WWWCLE GO TO T92A.
      *
       lecc3.
      ********  lecture    des elements 04 : libelles complem. *******
           move zeroes to fccle3-cdesup.
DD0316     move fccle-cdesup  to fccle3-cdesup
           move 04     to fcnel3.
           move xart   to fcart3.
           move 10     to fclig3.
DD0350     move zero to wsaut3
           perform snl-fcommac3.
           if file-status not = zero
                GO TO t92c.
       lec3.
           perform nnl-fcommac3.
           if file-status not = zero
                GO TO t92c.
DD0316     if fcnum3 not = fcnum or fcnin3 not = fcnin or
DD0316        fccle3-newcde not = fccle-newcde or
DD0316        fccle3-newsoc not = fccle-newsoc
              go to t92c
           end-if
           if fcnar3 not = xnar go to t92c.
DDE069*    if fcnlg3 not = fcnlg go to lec3.
DD0350     if fcnlg3 not = wnlg go to lec3.
           perform t110 thru t110e.
           go to lec3.
      **** FIN ARTICLE ****
      *
       T92C.
      ***  MODIF DEMANDEE PAR MR. COMBAZ LE 26/10/92            ***
      ***        NE PLUS IMPRIMER LES RUPTURES SUR FACTURE      ***
      ***  IF WQTELIV = 0 GO TO T97.
           IF WQTELIV = 0 GO TO T95g.
      ***               ---    FIN MODIF   ---                  ***
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE ZERO TO WTEST6.
           IF WNLIG = 2 MOVE WRE1 TO LVREF.
           MOVE WART1 TO LNREF.
           MOVE WDES TO LDESAR.
           MOVE WMES TO LUV.
           IF WDEC NOT = ZERO AND WQPB > 1 MOVE 1 TO WTEST3 WTEST6.
      ***MODIF DE LA FACTURATION 4/87 ***( GO TO T92D. NON REPRIS ICI)
           IF WMES > 3 AND WMES < 7 GO TO T92D.
           GO TO T92E.
       T92D.
           IF WMES = 06 MULTIPLY 1000 BY WQTELIV
                   ELSE MULTIPLY  100 BY WQTELIV.
       T92E.
           IF WTEST6 = 1 MOVE "*" TO LNBTESX GO TO T92EA.
           IF WQPB > 1   MOVE WENT TO LNBTES.
       T92EA.
           IF WQPB   > 1 MULTIPLY WQPB BY WQTELIV
                         MOVE     WQPB TO LQTEUN.
           MOVE WQTELIV  TO WQTEFA.
           IF WQTEFA > 99999 MOVE WQTEFA TO LQTEFA
             ELSE            MOVE WQTEFA TO LQTEFAU.
           IF LQTEFAD = "00" MOVE SPACE TO LQTEFAD.
           MOVE WPHT TO LPU.

      * calcul montant ligne ==> wta-resu
      *M210601 wta-qte   n'est plus chargee ici mais en meme temps que wqteliv
      *        car wqtefa etait deja multipliee par la qte par boite ==>
      *        la qte etait ensuite recalculee avec la qte par boite ds tarifqt3
      *        d'ou mauvais calcul du montant ligne
      *    move wqtefa to wta-qte.
           move wmes  to wta-fcmes.
           move wprx  to wta-fcprx.
           move wpht  to wta-prix.
           move wqpb  to wta-fcqpb.
           call "tarifqt3" using wtarifqte adl-art.
           if wprx not = 1
              move 'F'   to immpa-upri-trt
              move "A" to immpa-upri-fic
              move wprx to wmmpa-upri-mes
              call 'mmpa-upri1' using mmpa-upri adl-art
      * GPIWARNING pour autoriser un prix a zero on doit saisir Z dans l'unite
      *            de prix mais ne pas l'editer sur la facture
              if wmmpa-upri-lib not = 'Z'
                 move wmmpa-upri-lib to lucm
              else
                 move space to lucm
              end-if
           end-if.
       T95.
           ADD wta-resu TO WMT4 (WTEST2).
           MOVE wta-resu TO LMONT.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
GPICMT* edition nomenclature douaniere pour export hors CEE
ELGU17     if fcfoa < 5 and w-FCNPO not = '999999999' and not = spaces
  -           if (wgeo-livrea not = '0' and not = '1')
  -              or
  -              (wgeo-facturea not = '0' and not = '1')
  -              string 'Douane : ' w-FCNPO delimited size into LLIB
  -              write ligne before 1
  -              add 1 to wcptr
  -              move spaces to ligne
  -              end-if
ELGU17     end-if
           .
      ***  TT INTRACOMMUNAUTAIRE POUR LES ELT 4     ***
       T95A.
      *    IF WGEOL NOT = 1 GO TO T97.
           if fcfoa     = 9 go to t95g.
           IF WGEOL NOT = 1 GO TO T95G.
       T95B.
           MOVE SPACES TO ENRICO.
           MOVE FBNCL  TO ICCLF.
DD0316     MOVE fbcle-cdesup  TO ICNFA-cdesup.
           MOVE FBCFA  TO ICCFA.
           MOVE FBTAX  TO ICCTA.
           MOVE FBDEV  TO ICCDE.
           MOVE "L"    TO ICCIL.
           MOVE WNIC   TO ICNIC.
           MOVE WDATE9 TO ICDFA.
           MOVE WAD1   TO ICRCF.
DD0316*    MOVE WNUM   TO ICNOC.
DD0316*    MOVE WNIN   TO ICNIN.
DD0316     MOVE wcle   TO ICNCD-cdesup
           MOVE WPORT  TO ICPOR.
       T95C. EXIT.
       T95D.
           MOVE WART1  TO ICNAR.
           move wart2  to icsrf.
           MOVE WQPB   TO ICQPB.
           MOVE WQTEFA TO ICQTL.
           MOVE WPHT   TO ICPU.
           MOVE wta-resu TO ICMON.
           MOVE WMES   TO ICUM.
           MOVE WPRX   TO ICUP.
           MOVE WPDU   TO ICPDS.
           IF WPDU   = 0  GO TO T95E.
           MULTIPLY WPDS BY WQTEFA GIVING ICPTO ROUNDED.
       T95E.
           MOVE WDES   TO ICLIA.
       T95F.
      ***  Demande d'Yvonne le 16/1/97 : Vu Mme Large (ok)      ***
      ***  mais rechercher ce mot, il y est n (fois avec # )    ***
      *    IF WORIDUP = "*DUPLICATA*" GO TO T95G.
           IF WORIDUP = "*         *" GO TO T95G.
      ***  POUR NE PAS RE CREER LES LIGNES A CHAQUE DUPLICATA   ***
      *****WRITE ENRICO.

DDE089     if icilp-fact-e1red = 'R' go to t95g.

DDE175*    perform w-intracom.
DDE175*    IF file-status NOT = "00"
DDE175*       string 'Cde: ' fccle-cdesup ' ANOMALIE / INTRACOM, ST: '
DDE175*                               file-status
DDE175*                        delimited size into immaf-vali-tit
DDE175*       move '3' to ocilp-fact-rtn
DDE175*       perform erreur
DDE175*       perform subnum
DDE175*       go to fin1.
       T95G. EXIT.
      ***  FIN INTRACOMMUNAUTAIRE : EL 4 + COMMUN  ***
      *T96.
      *    EXIT.
       T97.
      *    IF WWWCLE NOT = ALL "9" GO TO T70.
           IF WWWCLE NOT = ALL "9" GO TO T90.
           go to lecc4.
      *    GO TO T200.
      *
      * TRT LIGNE = 2 *
      *
       T100.
           IF FCLIG NOT = 2 MOVE ZERO TO J
                            GO TO T110.
      *    IF FCQTL = ZERO GO TO T190.
           IF FCQTL = ZERO GO TO lec2.
      *
      *  RECHERCHE TAUX COMMISSION DANS ART./DEVIS
           IF WITC NOT = 9  GO TO T102F.
           MOVE FBNCL TO ADNCL1 wadncl.
           MOVE FCNAR TO ADNAR1 wadnar.
           MOVE FCSRE TO ADSRF1 wadsrf.
           move 02    to adtye1.
           move 00    to adunix1.
      *****READ DEVI INVALID KEY GO TO T102D.
           perform r-artdevc1.
           if file-status not = zero
                     GO TO T102D.
       T102.
      *****READ DEVI COMPL AT END   GO TO T102F.
      *    perform n-artdevc1.
      *    if file-status not = zero
      *           GO TO T102F.
           if adcnt1 not = wadcnt    go to t102f.
           IF ADTYE1 NOT = 2         GO TO T102F.
           IF ADTRE = ZERO AND ADTAG = ZERO  GO TO T102F.
           MOVE ADTRE TO FBTRH.
           MOVE ADTAG TO FBTRA.
           MOVE ZERO  TO WITC.
           GO TO T102F.
       T102D.
DD0002*    IF WNAF = ZERO OR WNOF = ZERO  GO TO T102F.
DD0002     IF WNOF = ZERO  GO TO T102F.
           MOVE WNOF  TO ADNCL1 wadncl.
           MOVE FCNAR TO ADNAR1 wadnar.
           MOVE FCSRE TO ADSRF1 wadsrf.
           move 02    to adtye1.
           move 00    to adunix1.
      *****READ DEVI INVALID KEY GO TO T102F.
           perform r-artdevc1.
           if file-status not = zero
                     GO TO T102F.
           GO TO T102.
      *
       T102F.
           MOVE FCLIG TO WNLIG.
           PERFORM TEST2b THRU FTEST2b.
           IF WTEST2 = 3 GO TO DISP.
           MOVE ZERO   TO WQTELIV wta-qte.                              *M210601
           MOVE FCNEL2 TO WNEL.
           MOVE FCNAR  TO WART1.
           move fcsre  to wart2.
DDE130     move fcsre  to wsref
DD9999     MOVE fcpht  TO wpri1.
           MOVE FCPDU  TO WPDU.
           MOVE FCDES  TO WDES.
           MOVE FCMES  TO WMES.
           MOVE FCPRX  TO WPRX.
           MOVE FCPHT  TO WPHT.
           MOVE FCQPB  TO WQPB.
           MOVE FCRE1  TO WRE1.
       T105.
           ADD FCQTL   TO WQTELIV wta-qte.                              *M210601
      *****READ FCOMMDES COMPL AT END MOVE ALL "9" TO WWWCLE
           perform n-fcommac2.
           if file-status not = zero
                MOVE ALL "9" TO WWWCLE
                                      GO TO lecc3b.
      *****Ici aussi le test etait avant mise en ww
           MOVE FCNEL2 TO WWNEL.
           MOVE FCLIG  TO WWNLIG.
           MOVE FCNAR  TO WWART1.
DDE130     move fcsre  to wwsref
DD9999     MOVE fcpht  TO wwpri1.
           if fcnoc2-cdesup not = fccle-cdesup move all "9" to wwwcle
                                 GO TO lecc3b.
DD0351*    IF WWCLE = WWWCLE AND FCPHT = WPHT  GO TO T105.
       lecc3b.
      ********  lecture    des elements 04 : libelles complem. *******
           move zeroes to fccle3-cdesup
           move fccle-cdesup to fccle3-cdesup
           move 04     to fcnel3.
           move xart   to fcart3.
           move 10     to fclig3.
DD0350     move zero to wsaut3
           perform snl-fcommac3.
           if file-status not = zero
                GO TO t92c.
       lec3b.
           perform n-fcommac3.
           if file-status not = zero
                GO TO t92c.
DD0316     if fcnoc3 not = fccle or fccle3-newcde not = fccle-newcde or
DD0316        fccle3-newsoc not = fccle-newsoc
              or  fcart3 not = xart go to t92c.
           perform t110 thru t110e.
DD0350     if fcnlg3 not = wnlg go to lec3b.
           go to lec3b.
      *    GO TO t90.
      *
      * TRT LIGNE > 9 *
      *
       T110.
      *    ADD 1 TO J.
      *    IF J > 3 GO TO T190.
           IF FCCL1 NOT = "G" AND FCCL1 NOT = "F" GO TO T110b.
DD0122*    IF WCPTR > 34 write ligne before page
DD0350     perform saut3
DD0122     perform saut34
           MOVE FCLC1 TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T110b.
           IF FCCL2 NOT = "G" AND FCCL2 NOT = "F" GO TO T110c.
DD0122*    IF WCPTR > 34 write ligne before page
DD0350     perform saut3
DD0122     perform saut34
           MOVE FCLC2 TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T110c.
           IF FCCL3 NOT = "G" AND FCCL3 NOT = "F" GO TO t110e.
DD0122*    IF WCPTR > 34 write ligne before page
DD0350     perform saut3
DD0122     perform saut34
           MOVE FCLC3 TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       t110e. exit.
       t110f.
           go to lec3b.
      *    GO TO T110.
       lecc4.
      ********  lecture    des elements 05 A 10                *******
DD0316     move zeroes to fccle4-cdesup
DD0316     move fccle-cdesup  to fcnoc4-cdesup
           move 05     to fcnel4.
           move zeroes to fcunix4.
           move zero   to wnart wnsrf.
           perform snl-fcommac4.
           if file-status not = zero
                GO TO flec4.
       lec4.
           perform n-fcommac4.
           if file-status not = zero
                GO TO flec4.
DD0316     if fcnoc4-cdesup not = fccle-cdesup
DD0351        perform rw-fcommac4 go to flec4.
      *
      ** TRT EL.5 **
      *
       T120.
           IF FCNEL4 NOT = 5 GO TO T130.
           PERFORM TEST2 THRU FTEST2.
           IF WTEST2 = 3 GO TO DISP.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FCDOP TO LLIB.
           MOVE FCQUL TO LQTEFA.
           IF LQTEFAD = "00" MOVE SPACE TO LQTEFAD.
           MOVE FCPUH TO LPU.
           IF FCMON NOT = ZERO GO TO T120A.
           MULTIPLY FCQUL BY FCPUH GIVING WMONT.
GPICMT* on met a jour fcmon
DD0337     move wmont to fcmon
DD0337     perform rw-fcommac4

      * trt du signe
           if fcsig = "-" multiply -1 by wmont                          *DDE043
                          move "-" to lsig.                             *DDE043

           GO TO T120B.
       T120A.
           MOVE 0 TO WMONT.
           IF FCSIG = "-" MOVE "-" TO LSIG
                          SUBTRACT FCMON FROM WMONT
             ELSE         ADD      FCMON TO   WMONT.
       T120B.
           ADD WMONT TO WMT5 (WTEST2).
           MOVE WMONT TO LMONT.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
           PERFORM T170A THRU T170D.
      ***  TT INTRACOMM. EL 5    ***
           IF WGEOL NOT = 1 GO TO T120C.
           if fcfoa     = 9 go to t120c.
           PERFORM T95B THRU T95C.
           MOVE FCQUL   TO ICQTL.
           MOVE FCPUH   TO ICPU.
           MOVE WMONT   TO ICMON.
           MOVE FCDOP   TO ICLIA.
           PERFORM T95F THRU T95G.
      ***  FIN TT INTRACOMM. EL 5   ***
       T120C.
           GO TO lec4.
      *
      ** TRT EL.6 **
      *
       T130.
           IF FCNEL4 NOT = 6 GO TO T140.
           PERFORM TEST2 THRU FTEST2.
DDE153     if fcfra = 2 and w-regroup = '1' go to lec4.
           IF WTEST2 = 3 GO TO DISP.
           IF FCSIG = "-" SUBTRACT FCMON FROM WMT6 (WTEST2)
             ELSE         ADD      FCMON TO   WMT6 (WTEST2).
           IF WGEOL = 1 GO TO T150A.
           GO TO lec4.
      *
      ** TRT EL.7 **
      *
       T140.
           IF FCNEL4 NOT = 7 GO TO T150.
           ADD 1 TO WCPT7.
           MOVE FCDOP TO WLIB7 (WCPT7).
DD0362     if icilp-fact-arc = spaces
              MOVE FCQUL TO WQUI (WCPT7)     
           else
              MOVE FCQUi TO WQUI (WCPT7)
           end-if
DD0337     move fccle4-cdesup to wcle7(wcpt7).
           IF FCSIG = "-" MULTIPLY -1 BY WQUI (WCPT7).
           IF WGEOL = 1  GO TO T150A.
           GO TO lec4.
      *
      ** TRT EL.8 **
      *
       T150.
           IF FCNEL4 NOT = 8 GO TO T160.
DDE153     if fcfra = 2 and w-regroup = '1' go to lec4.
           IF FCSIG = "-" SUBTRACT FCMON FROM WMT8
             ELSE         ADD      FCMON TO   WMT8.
           IF WGEOL NOT = 1 GO TO lec4.
       T150A.
           if fcfoa     = 9 go to lec4.
           PERFORM T95B THRU T95C.
           MOVE FCDOP   TO ICLIA.
           MOVE FCMON   TO ICMON.
           IF FCSIG = "-" MULTIPLY -1 BY ICMON.
           IF FCNEL4 = 07  MOVE FCQUL TO ICQTL.
           PERFORM T95F THRU T95G.
           GO TO lec4.
      *
      ** TRT EL.9 **
      *
       T160.
      *----> M0797 (D)
      *    IF FCNEL4 NOT = 9 GO TO T170.
           IF FCNEL4 NOT = 9 GO TO lec4.
      *----> M0797 (F)
           if fcfeo not = 4 and fcfeo not = 5 go to t161.
           move fcdop to wdopx.
           if wdop1 = "REFERENCE GPI :" move wdop2 to wnart
                                        move wdop3 to wnsrf.
       t161.
           PERFORM TEST2 THRU FTEST2.
           IF WTEST2 = 3 GO TO DISP.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FCDOP TO LLIB.
           MOVE FCQUL TO LQTEFA.
           IF LQTEFAD = "00" MOVE SPACE TO LQTEFAD.
           MOVE FCPUH TO LPU.
           MOVE FCMON TO LMONT.
           IF FCSIG = "-" MOVE "-" TO LSIG
                          SUBTRACT FCMON FROM WMT9 (WTEST2)
             ELSE         ADD      FCMON TO   WMT9 (WTEST2).
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
           PERFORM T170A THRU T170D.
       T160A.
           IF WGEOL NOT = 1 GO TO lec4.
           if fcfoa     = 9 go to lec4.
           PERFORM T95B THRU T95C.
           MOVE FCDOP   TO ICLIA.
           MOVE FCQUL   TO ICQTL.
           MOVE FCPUH   TO ICPU.
           MOVE FCMON   TO ICMON.
           IF FCSIG = "-" MULTIPLY -1 BY ICMON.
           PERFORM T95F THRU T95G.
           GO TO lec4.
      *
      ** TRT EL.10 **
      *
       T170.
      *    IF FCNEL4 NOT = 10 GO TO T180.
           IF FCNEL4 NOT = 10 GO TO lec4.
           IF FCDOP = SPACE GO TO T170A.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FCDOP TO LLIB.
           PERFORM T170C.
       T170A.
           IF FC1LC = SPACE GO TO T170B.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FC1LC TO LLIB.
           PERFORM T170C.
       T170B.
           IF FC2LC = SPACE GO TO T170D.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FC2LC TO LLIB.
       T170C.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T170D.
           EXIT.
       T170E.
           GO TO lec4.
      *
      ***** edition libelles des outillages factures trouves dans ARTDEVC2
      *
       flec4.
           if wnart = zero and wnsrf = zero go to lecc5.
           move wnart to ndnar.
           move wnsrf to ndsrf.
           perform rnl-numdevis.
           if file-status not = zero display
             "ERREUR : DEVIS INEXISTANT, ARTICLE: " wnart "  " wnsrf
                                     go to lecc5.
           MOVE NDNCL to adncl2.
           MOVE NDNAR to adnar2.
           MOVE NDSRF to adsrf2.
           move zero to adunix2.
           move 3 to adtye2.
           perform snl-artdevc2.
           if file-status not = zero   go to lecc5.
       flec41.
           perform nnl-artdevc2.
           if file-status not = zero   go to lecc5.
           if adncl2 not = ndncl or adnar2 not = ndnar or adsrf2
                     not = ndsrf        go to lecc5.
           IF adtye2 NOT = 3  GO TO flec41.
           IF ADZOU = zero   GO TO flec41.
           move 1 to wi.
       flec42.
           if adnou (wi) = zero go to flec43.
           if fcfeo = 4 and adcps (wi) not = "P" go to flec43.
           if fcfeo = 5 and adcps (wi) not = "S"     go to flec43.
           if adcaf(wi) not = "1" and adcaf(wi) not = "4" go to flec43.
           MOVE ADCOU (WI) TO WGTYP.
           MOVE ADNOU (WI) TO WGNOU.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE WGDES TO LLIB.
           if adcaf(wi) = 4 move "(modif)" to lmodif.
           PERFORM T170C.
       flec43.
           if wi < 3 add 1 to wi go to flec42.
           go to flec41.
      *
      ** TRT EL.99 **
       lecc5.
      ********  lecture    des elements 99                     *******
DD0316     move zeroes to fccle5-cdesup
DD0316     move fccle-cdesup  to fccle5-cdesup
           move 99     to fcnel5.
           perform r-fcommac5.
           if file-status not = zero
                GO TO t200.
DD0316*    if fcnoc5 not = fccle go to t200.
DD0316     if fcnum5 not = fcnum or fcnin5 not = fcnin or
DD0316        fccle-newcde not = fccle5-newcde or
DD0316        fccle-newsoc not = fccle5-newsoc go to t200.
      *
       T180.
           IF FCNEL5 NOT = 99
              MOVE FCNEL5 TO WDNEL
DD0316*       MOVE FCNUM TO WDNUM
DD0316        MOVE fccle-cdesup(1:7) TO WDNUM
              MOVE FCNIN TO WDNIN
              string 'Cde: ' fccle-cdesup ' Type elt 5 inconnu: ' wdnel
                     delimited size into immaf-vali-tit
              perform erreur
              GO TO lecc5.

           MOVE FCTEC TO WTEC.
           MOVE 1 TO WTEST4.
      *
      **** FIN FACTURE ****
      *
      *
      * CALCUL NET MARCH., HORS TAXES  AVANT REMISES
      *
       T200.
DDE153*    ADD WMT4 (1) WMT5 (1) TO FBNMF.
DDE153     ADD WMT4 (1) WMT5 (1) TO w-FBNMF.
DDE153*    ADD FBNMF WMT9 (1)    TO FBHT1F.
DDE153     ADD w-FBNMF WMT9 (1)    TO w-FBHT1F.
DDE153*    IF FBFRA NOT = 2 ADD WMT6 (1) TO FBHT1F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (1) TO w-FBHT1F.
DDE153*    MOVE WMT6 (1) TO FBPOF.
DDE153     add  WMT6 (1) TO FBPOF.
DDE153*    MOVE WMT8     TO FBAFF.
DDE153     add  WMT8     TO FBAFF.
           MOVE WCODTAX1 TO FBC1F.
           IF WTEST5 NOT = 2 GO TO T200A.
DDE153*    ADD WMT4 (2) WMT5 (2) TO FBNMF.
DDE153     ADD WMT4 (2) WMT5 (2) TO w-FBNMF.
DDE153*    ADD WMT4 (2) WMT5 (2) WMT9 (2) TO FBHT2F.
DDE153     ADD WMT4 (2) WMT5 (2) WMT9 (2) TO w-FBHT2F.
DDE153*    IF FBFRA NOT = 2 ADD WMT6 (2)  TO FBHT2F.
DDE153     IF FBFRA NOT = 2 ADD WMT6 (2)  TO w-FBHT2F.
           ADD WMT6 (2) TO FBPOF.
           MOVE WCODTAX2 TO FBC2F.
      *
      * CALCUL ET EDITION REMISES
      *
       T200A.
           IF WCPT7 = ZERO GO TO T210.
           MOVE ZERO TO I.
DDE153*    ADD FBHT1F FBHT2F GIVING WHT1.
DDE153     ADD w-FBHT1F w-FBHT2F GIVING WHT1.
      ***  AJOUT 4/11/86 PAS FAIRE DE REM./PORT SI AVANCE (3 LIGNES)
DDE153*    IF FBFRA = 2 GO TO T205.
DDE153     IF FCFRA = 2 GO TO T205.
           SUBTRACT WMT6 (1) FROM WHT1.
           SUBTRACT WMT6 (2) FROM WHT1.
       T205.
           ADD 1 TO I.
           IF I > WCPT7 GO TO T210.
DD0122*    IF WCPTR > 31 write ligne before page
DD0122     perform saut31
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 1.
           MOVE "BASE HORS TAXES AVANT REMISE" TO LIBCA13.
           MOVE WHT1 TO LMONT.
           IF WHT1 < 0 MOVE "-" TO LSIG.
DD0337     if wht1 < 0 multiply wht1 by -1 giving wbas7(i)
DD0337       else      move wht1           to     wbas7(i).
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MULTIPLY WQUI (I) BY WHT1 GIVING WREM.
           DIVIDE 100 INTO WREM ROUNDED.
           ADD WREM TO FBREF.
           ADD WREM TO WHT1.
      ***  SUITE MODIF (4 LIGNES SUIVANTES)
           MOVE WREM TO WREM2.
           IF FBFRA = 2 GO TO T205A.
DDE153     SUBTRACT WMT6 (1) FROM w-FBHT1F.
       T205A.
DDE153     MULTIPLY WQUI (I) BY w-FBHT1F GIVING WREM1.
           DIVIDE 100 INTO WREM1 ROUNDED.
DDE153     ADD WREM1 TO w-FBHT1F.
      ***  SUITE MODIF (2 LIG)
DDE153     SUBTRACT w-FBHT1F FROM WHT1 GIVING w-FBHT2F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (1) TO w-FBHT1F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (2) TO w-FBHT2F.
      ***
           MOVE WLIB7 (I) TO LLIB.
           MOVE WQUI (I) TO LQTEFA.
           MOVE WREM TO LMONT.
DD0337     move wrem to wmon7(i).
           IF WREM < 0 MOVE "-" TO LSIG.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           ADD 3 TO WCPTR.
      ***  LA REMISE GLOBALE CONCERNE LE NET MARCHANDISE
      ***  LES 2 LIGNES SUIVANTES AVAIENT ETE SUPPRIMEES AVEC LA
      ***  MODIFICATION DE 1986 : CAS DES ETIQUETTES (14/1/93)
      ***  DONC ON RETABLIT:TOT NM - % REM = NOUVEAU TOT NET MARCHANDISE
DDE153     MULTIPLY WQUI (I) BY w-FBNMF GIVING WREM2.
           DIVIDE 100 INTO WREM2 ROUNDED.
DDE153     ADD WREM2 TO w-FBNMF.
       T205B.
           IF WGEOL NOT = 1 GO TO T205.
           if fcfoa     = 9 go to t205.
           PERFORM T95B THRU T95C.
           MOVE WLIB7 (I) TO ICLIA.
           MOVE WREM    TO ICMON.
           PERFORM T95F THRU T95G.
           GO TO T205.
      *
      * CALCUL NET MARCH. APRES REMISES
      * CALCUL BASES ET TAXES PARAFISCALES
      *
       T210.
      *----> M0797 (D)
      *
      ** TRT EL.10 **
      *
           move zeroes to fccle4-cdesup.
           move fccle-cdesup  to fcnoc4-cdesup.
           move 10     to fcnel4.
           move zeroes to fcunix4.
           perform snl-fcommac4.
           if file-status not = zero
                GO TO t213.
       t211.
           perform n-fcommac4.
           if file-status not = zero
                GO TO t213.
           if fcnoc4-cdesup not = fccle-cdesup
DD0351        perform rw-fcommac4 go to t213.
           IF FCNEL4 NOT = 10 GO TO t211.
           IF FCDOP = SPACE GO TO t212.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FCDOP TO LLIB.
           PERFORM T170C.
       t212.
           perform t170a thru t170d.
           go to t211.
       t213.
      *----> M0797 (F)
      *
      **** REECRITURE EP FCOMMDES ****
      *
       T410.
DDE153     perform cum-cde.
           IF WORIDUP = "*         *" GO TO T410A.
DD0362* pas de mise a jour si edition arc
DD0362     if icilp-fact-arc = spaces
DD0316        MOVE fbcle-cdesup TO FCNFA-cdesup   
              MOVE 1 TO FCFAC 
           end-if

DDE089     if icilp-fact-e1red not = 'R'
              perform rw-fcommaap
              if file-status not = zero
                 string 'Reecriture FCOMMAAP impossible: ' fccle-cdesup
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                 move '3' to ocilp-fact-rtn
                 perform erreur
                 go to fin1
              end-if
           END-IF.

      *---------------------
      * Ecriture de la trace
      *---------------------
           IF icilp-fact-e1red not = 'R'
DD0350        and icilp-fact-arc = spaces
              move space to wmmtr-trac
              move "C"   to immtr-trac-type
DD0316        string fccle-cdesup delimited by size into immtr-trac-num
DD0062        move "M"   to immtr-trac-action
              move wnom-prog to immtr-trac-prog
              string 'Facturation de la commande: ' fccle-cdesup
                     '  sous le n.: ' fbcle-cdesup
                      delimited by size into immtr-trac-commentaire
              call 'mmtr-trac1' using mmtr-trac adl-art
           END-IF.

      *
      * TEST DUPLICATA *
      *
       T410A.
ELGU17* pour edition laser on ne fait pas plusieurs exemplaire
ELGU17     if mmdt-societe = "SLOVAQ"
DD0516        or mmdt-societe = "GERGONNE"
  -           or icilp-fact-arc not = spaces
  -           or icilp-fact-pdf = "O"
  -           go to T419
ELGU17     end-if
           SUBTRACT 1 FROM WWNBF.
DD0337*    IF WWNBF < 1 GO TO T420.
DD0337     IF WWNBF < 1 GO TO T419.
DDE153* GPIWARNING pas de duplicata pour cde regroupee
           if w-regroup = '1'
              string 'Duplicata non édité pour facture ' fbcle-cdesup
                   delimited size into immaf-vali-tit
              perform erreur
DD0337*       go to t420
DD0337        go to t419
           end-if

DDE153     if w-creat = '1'
              perform pied
GPICMT* appel maj de toutes les commandes de la facture  dans la dataware 
DD0444        perform maj-dataware
           end-if
 
           perform ini-cde

      *    MOVE "*DUPLICATA*" TO WORIDUP.
           MOVE "*         *" TO WORIDUP.
           GO TO T62.
      *
DD0337*********** reecriture base et montant remise pour les elmnts 7
      *
       t419.
           if wcpt7 = zero go to t420.
           move 1 to i.
       t419a.
           if i > wcpt7 go to t420.
           move wcle7(i) to fccle4-cdesup.
GPICMT* lecture non blocante si reedition
DD0351     if icilp-fact-e1red not = 'R'
              perform r-fcommac4
           else
              perform rnl-fcommac4
           end-if
           if file-status not = zero go to t419b.
           move wbas7(i) to fcpuh.
           move wmon7(i) to fcmon.

           IF icilp-fact-e1red not = 'R'
              perform rw-fcommac4
              if file-status not = zero
                 string 'Reecriture FCOMMAC4 impossible: ' wcle7(i)
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                 move '3' to ocilp-fact-rtn
                 perform erreur
                 go to fin1
              end-if
            END-IF
           .

       t419b.
           add 1 to i.
           if i > zero go to t419a.
      *----> M1297 (F)
      *
      **** LECTURE EP SUIVANT ****
      *
       T420.
      *DDE089 si facture a la demande ==> fini
DDE153*    if wcdex not = zero go to fin1.
DDE153     if wcdex not = zero go to t420-f.



GPICMT* mecture non blcante si reedition
DD0351     if icilp-fact-e1red = 'R'
              and icilp-fact-arc = spaces
              perform nnl-fcommaap
           else
              perform n-fcommaap
           end-if
           if file-status not = zero
                GO TO FIN1.

DDE153     if icilp-fact-e1red = 'R'
DD0316        if fcnfa-cdesup = fbcle-cdesup
DD9999           if fcfoa = 9
                    go to t420
                 end-if
                 perform ini-cde
                 go to t20a
              else
                 go to t420-f
              end-if
           end-if
DDE153     if fcafa > 1 or fcfac > 0 go to t420-f.

           IF w-regroup = '1'
              if fcfacturea = w-facturea
                 and fcreglepar = w-reglepar
                 and fcregrfa   = w-regrfa
                 and fcdev      = w-dev
                 and fcgeo      = w-geo
                 and fcdi2      = w-taxe
                 and fcfoa      = w-foa
                 go to t20-s
              end-if
           END-IF
           .
       t420-f.
           if w-creat = '1'
              perform pied
GPICMT* appel maj de toutes les commandes de la facture  dans la dataware 
DD0444        perform maj-dataware
           end-if

DDE089     if icilp-fact-e1red = 'R'
               go to fin1
           end-if
           if wcdex not = zero go to fin1.

           GO TO T20.
       FIN1.
DDE089     if icilp-fact-e1red = 'R' go to fini.

DD0362     if icilp-fact-arc not = spaces
              go to fin
           end-if

           MOVE "FACTURE000" TO PHCLE.
           perform r-parbatch.
           if file-status not = zero
              string 'Relecture PARBATCH impossible: ' PHCLE
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocilp-fact-rtn
              perform erreur
              go to fin
           end-if
           MOVE WNFANA TO PHANOC (1).
           MOVE WNFACA TO PHANOC (2).
      *****REWRITE ENRBGP INVALID GO TO INVAL6.
           perform rw-parbatch.
           if file-status not = zero
              string 'Reecriture PARBATCH impossible: ' PHCLE
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocilp-fact-rtn
              perform erreur
           end-if
           GO TO FIN.

       TEST2.
           IF WCODTAX1 = ZERO AND WTEST5 = ZERO MOVE FCTVP4 TO WCODTAX1
                                                MOVE 1 TO WTEST5.
           IF WCODTAX1 = FCTVP4 MOVE 1 TO WTEST2
                                GO TO FTEST2.
           IF WCODTAX2 = ZERO AND WTEST5 = 1 MOVE FCTVP4 TO WCODTAX2
                                             MOVE 2 TO WTEST5.
           IF WCODTAX2 = FCTVP4 MOVE 2 TO WTEST2
                                GO TO FTEST2.
           MOVE 3 TO WTEST2.
       FTEST2.
           EXIT.
       TEST2b.
           IF WCODTAX1 = ZERO AND WTEST5 = ZERO MOVE FCTVP TO WCODTAX1
                                                MOVE 1 TO WTEST5.
           IF WCODTAX1 = FCTVP MOVE 1 TO WTEST2
                               GO TO FTEST2b.
           IF WCODTAX2 = ZERO AND WTEST5 = 1 MOVE FCTVP TO WCODTAX2
                                             MOVE 2 TO WTEST5.
           IF WCODTAX2 = FCTVP MOVE 2 TO WTEST2
                               GO TO FTEST2b.
           MOVE 3 TO WTEST2.
       FTEST2b.
           EXIT.
       DISP.
DD0316     MOVE fbcle-cdesup TO WNFA9.
      *DDE089 decrementation numerotation
           perform subnum.

           MOVE SPACE TO LIGNE.
           MOVE "ATTENTION! FACTURE NON TRAITEE ENTIEREMENT, CAR IL
      -    "EXISTE PLUS DE 2 CODES TAXE." TO LIGNE.
      *    WRITE LIGNE BEFORE 1.
           WRITE LIGNE BEFORE page.
           MOVE SPACE TO LIGNE.
           string "FACTURE: " WNFA9 " NON TRAITEE, NBRE DE TAXE > 2"
                  delimited size into immaf-vali-tit
           perform erreur
           GO TO T420.
      *
      **** EDITION DE L'ENTETE ****
      *
       TITRE.
           MOVE SPACE TO LIGNE.
           compute wnbl = 10 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L10.
           IF WCPTR NOT = 90 MOVE "SUITE" TO LSUITE.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE WAD1 TO LNOMPAY.
           MOVE WORIDUP TO LCOND.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE WAD2 TO LNOMPAY.
           MOVE WFACAVO TO LCOND.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DD0316     MOVE fbcle-cdesup TO LNFACT.
           MOVE WAD3 TO LNOMPAY.
DDE130*    MOVE WPROCON TO LCOND.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DDE130*    MOVE WNBF TO LNBEX.
DDE130     MOVE WPROCON TO LCOND.
           MOVE WAD4 TO LNOMPAY.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DDE130     MOVE WNBF TO LNBEX.
DD0188*    MOVE WCODP5 TO LCODP2.
DD0188*    MOVE WBD5   TO LXBD2.
DD0188     move wad5 to lbureau
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DDE153*    MOVE WAD6 TO LNOMLIV.
DDE153     if w-regroup not = '1'
              MOVE WAD6 TO LNOMLIV
           end-if
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DD0188*    MOVE WCODP7 TO LCODP1.
DD0188*    MOVE WBD7 TO LXBD1.
DDE153     if w-regroup not = '1'
DD0188        move wad7 to lbureaul
DDE153     end-if
           compute wnbl = 19 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L19.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE FROM WLREF BEFORE 3.
           MOVE SPACE TO LIGNE.
           MOVE ZERO TO WCPTR.
      *----> M0797 (D)
      *    IF WDEV = 00 GO TO FTITRE.
           IF wlde = spaces GO TO FTITRE.
      *----> M0797 (F)
           MOVE WLDE TO LDEV.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           ADD 2 TO WCPTR.
       FTITRE.
           EXIT.
      *----> M1198 (D)
      ******    conversion d'un montant devise en EURO
      ******    suivant que la devise est europeenne ou non on multiplie ou on
      ******    divise par le coefficient (methode annulee le 070199 apres
      ******    confirmation des taux de change par les banques)
       eurb.
      *    if wpgbceu = "O" divide   wzca by wpgbteu giving wcale
      *      else           multiply wzca by wpgbteu giving wcale.
           divide wzca by wpgbteu giving wcale.
           add ar to wcale.
       eurc.
      ******    conversion d'un montant EURO en FRANC FRANCAIS
           multiply wzca by wpgbt62 giving wcale.
           add ar to wcale.
      *----> M1198 (F)
       FIN.
           CLOSE ETAT.

GPICMT* si edition laser on supprime l'etat qui a ete cree (en attendant de
GPICMT* reecrire la creation de la facture sans l'edition)
DD0337     if mmdt-societe = "SLOVAQ"
DD0516        or mmdt-societe = "GERGONNE"
DD0362        or icilp-fact-arc not = spaces
DD0362        or icilp-fact-pdf = "O"
              string 'rm -f ' wlabel-etat x'00' delimited by size
                       into sys-var
              call 'systcc' using sys-var sys-rtn
           else
              move "spooladl"  to syst-data1
              move wlabel-etat to syst-data2
              call "systcc" using syst-zone syst-rtn
           end-if

GPICMT* on ferme les fichiers seumlement si direct 3 suit au pb edition ARC
         perform cl-parbatch
DD0350   if icilp-fact-direct not = 3
           perform cl-fclients
           perform cl-ffacture
           perform cl-paramgpi
           perform cl-fcommaap
           perform cl-fcommac1
           perform cl-fcommac2
           perform cl-fcommac3
           perform cl-fcommac4
           perform cl-fcommac5
           perform cl-intracom
           perform cl-artdevc1
           perform cl-clisuite
DD0002*    perform cl-fcoadcli
         end-if
         .
       fini.
           exit program.

      *=========================================================================
      *                              FONCTIONS LOCALES
      *=========================================================================


      *DDE089 affichage fenetre d'erreur
       ERREUR section.
           move wnom-prog to immaf-vali-pgm
           move "V=Validation" to immaf-vali-act
           move "V" to wmmaf-vali-trt
           move "B" to immaf-vali-pos
           move wtrt to wmmaf-vali-trt.
           call 'mmaf-vali1' using mmaf-vali adl-art.

      * decrementation numero de facture
       subnum section.
           IF WPROCON = "CONDITIONNEL" SUBTRACT 1 FROM WNFACA
             ELSE                      SUBTRACT 1 FROM WNFANA.

DDE153* ecriture entete commande si regroupee
       entete-cde section.
DD0122     perform saut34
           write ligne before 1
           string '    Notre Reference : ' fccle-cdesup
             '  Representant : '
            fcrep '  Livre A : ' walliv '  ' wad6 ' '
             delimited size into ligne
           write ligne before 1
           add 2 to wcptr
           move spaces to ligne
           string '    Votre Reference : ' wref(2:8)
                  '                                     '
                                           wcodp7 ' ' wbd7
             delimited size into ligne
           write ligne before 1
           add 1 to wcptr
           move spaces to ligne
           .
DDE153* edition et creation pied de facture
       pied section.

           IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T220.
           IF WPAR1 = ZERO AND  WPAR2 = ZERO GO TO T215.
           MOVE ZERO TO WHT1.
           IF WPAR1 = 1 MOVE FBHT1F TO FBBF1F
                        ADD FBBF1F TO WHT1.
           IF WPAR2 = 1 MOVE FBHT2F TO FBBF2F
                        ADD FBBF2F TO WHT1.
           IF FBTAX = 1 MULTIPLY WTAUPA1 BY WHT1
             ELSE       MULTIPLY WTAUPA2 BY WHT1.
           DIVIDE 100 INTO WHT1 ROUNDED.
           IF FBTAX = 1 MULTIPLY WTAUPA1 BY FBBF1F GIVING WCAL
             ELSE       MULTIPLY WTAUPA2 BY FBBF1F GIVING WCAL.
           DIVIDE 100 INTO WCAL   ROUNDED.
           MOVE WCAL TO FBTP1F.
           SUBTRACT FBTP1F FROM WHT1 GIVING FBTP2F.
           ADD FBTP1F TO FBHT1F.
           ADD FBTP2F TO FBHT2F.
      *
      * CALCUL TVA
      *
       T215.
           IF WTV1 = ZERO GO TO T215A.
           IF WTV1 = 9 MOVE 5    TO I
             ELSE           MOVE WTV1  TO I.
           IF FBTAX = 1 MULTIPLY FBHT1F BY WTVA1 (I) GIVING WCAL
             ELSE       MULTIPLY FBHT1F BY WTVA2 (I) GIVING WCAL.
           DIVIDE 100 INTO WCAL   ROUNDED.
           MOVE WCAL TO FBTX1F.
       T215A.
           IF WTV2 = ZERO GO TO T220.
           IF WTV2 = 9 MOVE 5    TO I
             ELSE           MOVE WTV2  TO I.
           IF FBTAX = 1 MULTIPLY FBHT2F BY WTVA1 (I) GIVING WCAL
             ELSE       MULTIPLY FBHT2F BY WTVA2 (I) GIVING WCAL.
           DIVIDE 100 INTO WCAL   ROUNDED.
           MOVE WCAL TO FBTX2F.
      *
      * CALCUL NET A PAYER
      *
       T220.
           ADD FBHT1F FBTX1F FBHT2F FBTX2F TO FBNPF.
DDE153* on teste la valeur des montants port ici afin d'affecter le bon code
      * port a la facture et pour prendre en compte l'affranchissemnt postal
      * sinon en cas de regroupement le code etatit aleatoire suivant celui
      * de la derniere commande traitee, pourra se resoudre en ajoutant le code
      * port dans la cle de regrroupement des commandes
DDE153     if w-regroup = '1'
              if fbaff = zero and fbpof = zero
                 move 2 to fbfra
              else
                 move 1 to fbfra
              end-if
           end-if

           IF FBFRA NOT = 2 ADD FBAFF TO FBNPF.
      *
      **** MISE EN FORME DATE ECHEANCE ****
      *
       T280.
           IF WTEST4 NOT = 1 GO TO T290.
      *
      * ECHEANCES PROPORTIONNELLES *
      *
           MOVE WDECH (1) TO FBDE1.
           MULTIPLY FBNPF BY WTXE (1) GIVING WMT.
           DIVIDE WMT BY 100 GIVING FBE1F.
           MOVE WDECH (2) TO FBDE2.
           IF WTXE (3) = ZERO SUBTRACT FBE1F FROM FBNPF GIVING FBE2F
                              MOVE 2 TO FBNEC
                              GO TO T330.
           MOVE WDECH (3) TO FBDE3.
           MULTIPLY FBNPF BY WTXE (2) GIVING WMT.
           DIVIDE WMT BY 100 GIVING FBE2F.
           ADD FBE1F FBE2F GIVING WNPF.
           SUBTRACT WNPF FROM FBNPF GIVING FBE3F.
           MOVE 3 TO FBNEC.
           GO TO T330.
      *
      * ECHEANCE IMPOSEE *
      *
       T290.
           IF WDAE = ZERO GO TO T300.
           MOVE WDAE TO FBDE1.
           MOVE 9 TO FBNEC.
           MOVE FBNPF TO FBE1F.
           GO TO T330.
      *
      * ECHEANCE CALCULEE *
      *
       T300.
DD0387     move FBDAF to wmmca-eche-date-ori9
  -        move wcon to wmmca-eche-con
  -        move fbreg to wmmca-eche-regl
  -        move wcec to wmmca-eche-decal
DD0438     call 'mmca-eche1' using mmca-eche mmaf-vrep adl-art
  -        if ommca-eche-rtn not = cmmdt-envi-rtn-ok
  -           string 'Facture ' FBCLE-CDESUP ' erreur calcul echeance '
  -                       ommca-eche-liberr
  -                  delimited size into immaf-vali-tit
  -           move '3' to ocilp-fact-rtn
  -           perform erreur
  -           go to fin1
  -        end-if
GPICMT* si l'echeance n'est pas calculee le chergement de l'echeance se fera au moment
GPICMT* de l'ecriture du pieds
  -        if ommca-eche-calculee = spaces
  -           go to t330
DD0387     end-if

DD0387*    MOVE WDATECH9 TO FBDE1.
DD0387     MOVE WCON TO FBCJL
DD0387     MOVE ommca-eche-date-eche9 TO FBDE1.
           MOVE FBNPF TO FBE1F.
           MOVE 1 TO FBNEC.
      *
      * COMPTAGE LIGNES PARTICULIERES
      *
       T330.
           MOVE ZERO TO WCPTR7.
           IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T330B.
           IF WPAR1 = ZERO AND WPAR2 = ZERO GO TO T330A.
           ADD 4 TO WCPTR7.
       T330A.
           IF WTEST5 NOT = 2 GO TO T330B.
           IF WTV1 = WTV2 GO TO T330B.
           ADD 4 TO WCPTR7.
       T330B.
           IF WTEST4 = 1 ADD FBNEC TO WCPTR7
                         ADD 4     TO WCPTR7.
      *
      * EDITION LIGNES PARTICULIERES *
      *
       T340.
           IF WCPTR7 NOT > ZERO GO TO T350.
           ADD WCPTR7 WCPTR GIVING WWCPTR.
      **** POUR AVOIR NB.LIGNES A JOUR APRES EDIT. LIGNES SPECIALES **
           MOVE WWCPTR TO WCPTR.
DD0122*    IF WWCPTR > 34 write ligne before page
DD0122     perform saut34
           IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T340B.
           IF WPAR1 = ZERO AND WPAR2 = ZERO GO TO T340A.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 2.
           MOVE "BASE HORS-TAXES AVANT TAXE PARAFISCALE" TO LIBCA13.
           ADD FBBF1F FBBF2F GIVING LMT.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE "TAXE   PARAFISCALE   DE" TO LIBCA12.
           IF FBTAX = 1 MOVE WTAUPA1 TO LTAUX
             ELSE       MOVE WTAUPA2 TO LTAUX.
           ADD FBTP1F FBTP2F GIVING LMT.
           MOVE "%" TO LPOURC.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
       T340A.
           IF WTV1 = WTV2 GO TO T340B.
           IF WTV1 = ZERO OR WTV2 = ZERO GO TO T340B.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 2.
           MOVE "BASE HORS-TAXES AVANT TVA" TO LIBCA12.
           IF FBTAX = 1 MOVE WTVA1 (WTV1) TO LTAUX
             ELSE       MOVE WTVA2 ( WTV1) TO LTAUX.
           MOVE "%" TO LPOURC.
           MOVE FBHT1F TO LMT.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE "BASE HORS-TAXES AVANT TVA" TO LIBCA12.
           IF FBTAX = 1 MOVE WTVA1 (WTV2) TO LTAUX
             ELSE       MOVE WTVA2 (WTV2) TO LTAUX.
           MOVE "%" TO LPOURC.
           MOVE FBHT2F TO LMT.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
       T340B.
           IF WTEST4 NOT = 1 GO TO T350.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 2.
           MOVE "ECHEANCES FRACTIONNEES :" TO LLIB.
           WRITE LIGNE BEFORE 2.
           MOVE SPACE TO LIGNE.
           MOVE FBD1J TO LJE.
           MOVE FBD1M TO LME.
           MOVE FBD1A TO LAE.
           MOVE "/" TO LSL1 LSL2.
           MOVE FBE1F TO LMTECH.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE FBD2J TO LJE.
           MOVE FBD2M TO LME.
           MOVE FBD2A TO LAE.
           MOVE "/" TO LSL1 LSL2.
           MOVE FBE2F TO LMTECH.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           IF FBDE3 = ZERO GO TO T350.
           MOVE FBD3J TO LJE.
           MOVE FBD3M TO LME.
           MOVE FBD3A TO LAE.
           MOVE "/" TO LSL1 LSL2.
           MOVE FBE3F TO LMTECH.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
      *
      * EDITION PIED DE FACTURE *
      *
       T350.
DD9999* ajout commentaire pour l'escompte
           perform finleau
      **** AJOUT DU TEXTE FIN DE FACTURE : SEPTEMBRE 1993     ****
           compute wnbl = 60 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L60.
           MOVE FBNCO TO LNBCOL.
           MOVE FBPBR TO LPDSTO.
           MOVE FBNMF TO LNETMA.
           ADD FBHT1F FBHT2F GIVING LTHT.
           IF FBFRA NOT = 2 MOVE FBPOF TO LPORTT.
           IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T350B.
           IF WTV1 = ZERO GO TO T350A.
           IF FBTAX = 1 MOVE WTVA1 (WTV1) TO LTAU1 FBTT1
             ELSE       MOVE WTVA2 (WTV1) TO LTAU1 FBTT1.
           MOVE FBTX1F TO LMTTVA1.
       T350A.
           IF WTV2 = ZERO GO TO T350B.
           IF FBTAX = 1 MOVE WTVA1 (WTV2) TO LTAU2 FBTT2
             ELSE       MOVE WTVA2 (WTV2) TO LTAU2 FBTT2.
           MOVE FBTX2F TO LMTTVA2.
       T350B.
           IF FBFRA NOT = 2 MOVE FBAFF TO LAFFRAN.
      *----> M0797 (D)
      *    if fcdev = 00 and wgeol not = zero move "  F.FR" to lffr.
      *----> M0797 (F)
           MOVE FBNPF TO LNETPAY.
      *----> M1198 (D)
      *    if fcdev = 62 move "  EURO" to lffr.
      * on edite systematiquement le sigle de la devise
           move wmmpa-devi-cdev to lffr.
      *----> M1198 (F)
           IF WFACAVO = "   AVOIR" AND FBNPF > ZERO MOVE "-" TO LSIGN.
           IF WFACAVO = "  FACTURE" AND FBNPF < ZERO MOVE "-" TO LSIGN.
           WRITE LIGNE BEFORE 2.
           MOVE SPACE TO LIGNE.
      *
      * EDITION MODE DE REGLEMENT *
      *
       T360.
           IF FBREG > 5 GO TO T360A.
           IF FBREG = 5 MOVE "EN V/REGLEMENT PAR B.O" TO LAD GO TO T370.
           MOVE "EN V/REGLEMENT" TO LAD.
           GO TO T370.
       T360A.
           IF FBREG > 10 GO TO T360B.
           IF FBREG = 6 MOVE "TRT. NORMALE" TO LLIB2.
           IF FBREG = 7 MOVE "TRT. CLIENT"  TO LLIB2.
           IF FBREG = 8 MOVE "TRT. BANQUE"  TO LLIB2.
           IF FBREG = 9 MOVE "TRT A PRECIS" TO LLIB2.
GPICMT* on n'edite plus la domiciliation bancaire du client
DD0351*    MOVE WDOM TO LAD.
           GO TO T370.
       T360B.
GPICMT* on n'edite plus le rib client
DD0351     go to T360C
           IF FBREG > 15 GO TO T360C.
           MOVE WCOB TO LCODBQ.
           MOVE WCOG TO LCODGU.
           MOVE WNCB TO LCOMPB.
           MOVE WKRI TO LRIB.
           GO TO T370.
       T360C.
           IF FBREG = 16  MOVE "CONTRE-REMBOURSEMENT" TO LAD GO TO T370.
           IF FBREG = 18  MOVE "ECHEANCE NETTE"       TO LAD GO TO T370.
           IF FBREG = 19  MOVE "CONTRE DOCUMENTS"     TO LAD GO TO T370.
       T360D.
           MOVE "REGLEMENT COMPTANT" TO LAD.
       T370.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           IF FBDE1 = ZERO GO TO T370A.
           MOVE FBD1J TO LJEE.
           MOVE FBD1M TO LMEE.
           MOVE FBD1A TO LAEE.
       T370A.
GPICMT* on n'edite plus la domiciliation bancaire du client
DD0351*    IF FBREG > 5 AND FBREG < 16 MOVE WDOA TO LAD.
           MOVE WNCLP TO LNCLIP.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE "REGLTCLI" TO PGHCL1.
           MOVE  FBREG     TO PGHCL2.
           perform rnl-paramgpi.
           if file-status not = zero
                     GO TO T370F.
           MOVE PGHLIB     TO LRLI.
           IF FBCJL = ZERO           GO TO T370F.
DD0387     if FBLE = zero
  -           move spaces to lcondx
  -           string fbjou ' Jrs Net'
  -               delimited size into lcondx
  -        else
              if FBLE = 30
                 string fbjou " J. F.M."
  -               delimited size into lcondx
              else
                 MOVE FBJOU      TO LRJO
                 MOVE FBLE       TO LRLE
                 MOVE "J. FM "   TO LRJL
              end-if
DD0387     end-if
           .
       T370F.
           MOVE WJ TO LJP.
           MOVE WM TO LMP.
           MOVE WA TO LAP.
           MOVE "/" TO LSL1P LSL2P.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DD0316     MOVE fbcle-cdesup TO LNFACTP.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DDE153     if w-regroup not = '1'
              MOVE WNUM TO LNCDE
              MOVE WNIN TO LNIN
           end-if
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
      ***  MOVE "SERVICE COMPTABILITE - TEL: 74.73.39.30" TO LREGL. ***
      *----> M0797 (D)
      *    if fcdev = 00 and wgeol not = zero move "  F.FR" to lffrp.
      *----> M0797 (F)
           MOVE FBNPF TO LNETPAY9.
      *----> M1198 (D)
      *    if fcdev = 62 move "  EURO" to lffrp.
      * on edite systematiquement le sigle de la devise
           move wmmpa-devi-cdev to lffrp.
      *----> M1198 (F)
           IF WFACAVO = "   AVOIR" AND FBNPF > ZERO MOVE "-" TO LPASIG.
           IF WFACAVO = "  FACTURE" AND FBNPF < ZERO MOVE "-"
                                                             TO LPASIG.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
      *----> M1198 (D)
DD9999*    if fcdev not = 00 and fcdev not = 50 and fcdev not = 62
DD9999     if w-dev not = 00 and w-dev not = 50 and w-dev not = 62
                                go to t370-a.
           move fbnpf to wzca.
DD9999*    if fcdev = 00 or fcdev = 50
DD9999     if w-dev = 00 or w-dev = 50
                          divide wzca by wpgbt62 giving wcale
                          move "EUR" to lffrp
             else         multiply wzca by wpgbt62 giving wcale
                          move "FRF" to lffrp.
           add ar to wcale.
           move wcale to lnetpay9.
           IF WFACAVO = "   AVOIR" AND FBNPF > ZERO MOVE "-" TO LPASIG.
           IF WFACAVO = "  FACTURE" AND FBNPF < ZERO MOVE "-"
                                                             TO LPASIG.
           write ligne before 1.
           move spaces to ligne.
       t370-a.
      *----> M1198 (F)
DD0122* on edite plus boites detaillees sert a rien et pose pb pour cette
      * modif
DD0122*    IF WTEST3 NOT = 1 GO TO T370B.
DD0122     go to t370b.
       T370B.
      ***** Verifier que tte fin de fact. passe par ici *****
DD0122*    perform identifiant
           WRITE LIGNE BEFORE page.
           MOVE SPACE TO LIGNE.
      *    IF WORIDUP = "*DUPLICATA*" GO TO T410.
DDE153*    IF WORIDUP = "*         *" GO TO T410.
DDE153     IF WORIDUP = "*         *" GO TO pied-f.
           IF WPROCON = " PRO-FORMAT" GO TO T380.
      *
      **** CUMUL FIN DE FACTURE ****
      *
           IF WFACAVO = "   AVOIR" ADD FBNPF TO WTOTAV
             ELSE                  ADD FBNPF TO WTOTFA.
      *
      * TEST DEVISE *
      *
       T380.
DDE153*    IF WTEST1 = 1 GO TO T410.
ELGU17*    IF WTEST1 = 1 GO TO pied-f.
      *----> M1198 (D)

      *DDE089 si devise = devise base comptable pas de conversion des   *GPICMT
      *                   zones montants du pieds de facture            *GPICMT
      *    IF FBDEV = ZERO or fbdev = 50 GO TO T400.
           if ommcp-devb-code = wmmpa-devi-cdev go to t400.
           move wpgbteu to wrep3.
      *----> M1198 (F)
       T380A.
           MOVE FBPFF TO FBPFD.
      *----> M1198 (D)
      *------------------------------------------                       *GPICMT
      *DDE089 ce traitement ne se fait que si la facture n'est pas dans *GPICMT
      *       le devise de base comptable                               *GPICMT
      *       si tarif base comptable euro ==> conversion des montants  *GPICMT
      *       en EURO ==> fini                                          *GPICMT
      *       si tarif base comptable francs ==> conversion des montants*GPICMT
      *       en EURO ==> puis en francs (/par 6,55957)                 *GPICMT
      *------------------------------------------                       *GPICMT
           if ommcp-devb-cod9 not = 62
              if fbdev = 62
                 move 1 to fbtch
                 go to t390e-1
              end-if
           END-IF.
      *----> M1198 (F)
           MOVE ZERO TO FBC1F FBC2F.
           MOVE 0 TO FBNMF FBHT1F FBBF1F FBTX1F FBTP1F FBHT2F FBBF2F
                     FBTX2F FBTP2F FBPOF.
           MOVE 0 TO FBREF FBCOF FBAFF FBNPF FBE1F FBE2F FBE3F.
      *
      * TEST DU NET A PAYER *
      *
       T390.
           MOVE FBC1D TO FBC1F.
           MOVE FBC2D TO FBC2F.
           ADD FBHT1D FBHT2D GIVING WHTD.
      *----> M1198 (D)
      *    MULTIPLY WREP3 BY FBNPD GIVING FBNPF.
      *    IF FBAFD NOT = ZERO MULTIPLY WREP3 BY FBAFD GIVING FBAFF.
           move fbnpd to wzca.
           perform eurb.
           move wcale to fbnpf.
           if fbafd not = zero move fbafd to wzca
                               perform eurb
                               move wcale to fbaff.
      *----> M1198 (F)
           IF FBNPD = WHTD MOVE FBNPF TO WHTDF
                           GO TO T390A.
      *----> M1198 (D)
      *    MULTIPLY WREP3 BY FBTX1D GIVING FBTX1F.
      *    MULTIPLY WREP3 BY FBTX2D GIVING FBTX2F.
           move fbtx1d to wzca.
           perform eurb.
           move wcale to fbtx1f.
           move fbtx2d to wzca.
           perform eurb.
           move wcale to fbtx2f.
      *----> M1198 (F)
           ADD FBTX1F FBTX2F GIVING WTXF.
           IF FBFRA NOT = 2 ADD FBAFF TO WTXF.
           SUBTRACT WTXF FROM FBNPF GIVING WHTDF.
       T390A.
      *----> M1198 (D)
      *    MULTIPLY WREP3 BY FBHT1D GIVING FBHT1F.
      *    SUBTRACT FBHT1F FROM WHTDF GIVING FBHT2F.
           if fbht2f = zero move whtdf to fbht1f
             else           move fbht1d to wzca
                            perform eurb
                            move wcale to fbht1f
                            SUBTRACT FBHT1F FROM WHTDF GIVING FBHT2F.
      *----> M1198 (F)
           IF WHTD = FBNMD MOVE WHTDF TO FBNMF ELSE
      *----> M1198 (D)
      *                    MULTIPLY WREP3 BY FBNMD GIVING FBNMF.
                           move fbnmd to wzca
                           perform eurb
                           move wcale to fbnmf.
      *----> M1198 (F)
           IF FBBF1D = ZERO AND FBBF2D = ZERO GO TO T390B.
           ADD FBBF1D FBBF2D GIVING FBBF2F.
           IF FBBF2F = FBNPD MOVE FBNPF TO FBBF2F ELSE
      *----> M1198 (D)
      *                  MULTIPLY WREP3 BY FBBF2F GIVING FBBF2F.
      *    MULTIPLY WREP3 BY FBBF1D GIVING FBBF1F.
                         move fbbf2f to wzca
                         perform eurb
                         move wcale to fbbf2f.
           move fbbf1f to wzca.
           perform eurb.
           move wcale to fbbf1f.
      *----> M1198 (F)
           SUBTRACT FBBF1F FROM FBBF2F.
           SUBTRACT FBBF1F FROM FBHT1F GIVING FBTP1F.
           SUBTRACT FBBF2F FROM FBHT2F GIVING FBTP2F.
       T390B.
           IF FBNEC = ZERO GO TO T390E.
           IF FBNEC NOT = 1 GO TO T390C.
           IF FBE1D = FBNPD MOVE FBNPF TO FBE1F ELSE
      *----> M1198 (D)
      *                    MULTIPLY WREP3 BY FBE1D GIVING FBE1F.
                           move fbe1d to wzca
                           perform eurb
                           move wcale to fbe1f.
      *----> M1198 (F)
           GO TO T390E.
       T390C.
           IF FBNEC NOT = 2 GO TO T390D.
           ADD FBE1D FBE2D GIVING FBE2F.
           IF FBE2F = FBNPD MOVE FBNPF TO FBE2F ELSE
      *----> M1198 (D)
      *                    MULTIPLY WREP3 BY FBE2F GIVING FBE2F.
      *    MULTIPLY WREP3 BY FBE1D GIVING FBE1F.
                           move fbe2d to wzca
                           perform eurb
                           move wcale to fbe2f.
           move fbe1d to wzca.
           perform eurb.
           move wcale to fbe1f.
      *----> M1198 (F)
           SUBTRACT FBE1F FROM FBE2F.
           GO TO T390E.
       T390D.
           ADD FBE1D FBE2D FBE3D GIVING FBE3F.
           IF FBE3F = FBNPD MOVE FBNPF TO FBE3F ELSE
      *----> M1198 (D)
      *                    MULTIPLY WREP3 BY FBE3F GIVING FBE3F.
      *    MULTIPLY WREP3 BY FBE2D GIVING FBE2F.
      *    MULTIPLY WREP3 BY FBE1D GIVING FBE1F.
                           move fbe3f to wzca
                           perform eurb
                           move wcale to fbe3f.
           move fbe2d to wzca.
           perform eurb.
           move wcale to fbe2f.
           move fbe1d to wzca.
           perform eurb.
           move wcale to fbe1f.
      *----> M1198 (F)
           ADD FBE1F FBE2F GIVING WFAEF.
           SUBTRACT WFAEF FROM FBE3F.
       T390E.
      *----> M1198 (D)
      *    IF FBPOD  NOT = ZERO MULTIPLY WREP3 BY FBPOD GIVING FBPOF.
      *    IF FBRED  NOT = ZERO MULTIPLY WREP3 BY FBRED GIVING FBREF.
      *    IF FBCOD  NOT = ZERO MULTIPLY WREP3 BY FBCOD GIVING FBCOF.
      *    MOVE WREP3 TO FBTCH.
           if fbpod not = zero move fbpod to wzca
                               perform eurb
                               move wcale to fbpof.
           if fbred not = zero move fbred to wzca
                               perform eurb
                               move wcale to fbref.
           if fbcod not = zero move fbcod to wzca
                               perform eurb
                               move wcale to fbcof.
           MOVE wpgbteu TO FBTCH.
      *DDE089 si devise comptable euro on arrete la conversion
           if ommcp-devb-cod9 = 62 go to t400.

       t390e-1.
           ADD FBHT1F FBHT2F GIVING WHTD.
           move fbnpf to wfbnpf.
           MOVE FBNPF to wzca.
           perform eurc.
           move wcale to fbnpf.
           move fbaff to wzca. perform eurc. move wcale to fbaff.
           IF wfbnpf = WHTD MOVE FBNPF to whtdf
                            GO TO T390F.
           move fbtx1f to wzca. perform eurc. move wcale to fbtx1f.
           move fbtx2f to wzca. perform eurc. move wcale to fbtx2f.
           ADD FBTX1F FBTX2F GIVING WTXF.
           IF FBFRA NOT = 2 ADD FBAFF TO WTXF.
           SUBTRACT WTXF FROM FBNPF GIVING WHTDF.
       T390F.
           if fbht2f = zero move whtdf to fbht1f
             else           move fbht1f to wzca
                            perform eurc
                            move wcale to fbht1f
                            SUBTRACT FBHT1F FROM WHTDF GIVING FBHT2F.
           IF WHTD = fbnmf  MOVE WHTDF TO FBNMF ELSE
                            move fbnmf to wzca perform eurc
                            move wcale to fbnmf.
           IF FBBF1F = ZERO AND FBBF2F = ZERO GO TO T390G.
           ADD FBBF1F to FBBF2F.
           IF FBBF2F = WFBNPF MOVE FBNPF TO FBBF2F ELSE
                              move fbbf2f to wzca perform eurc
                              move wcale to fbbf2f.
           move fbbf1f to wzca. perform eurc. move wcale to fbbf1f.
           SUBTRACT FBBF1F FROM FBBF2F.
           SUBTRACT FBBF1F FROM FBHT1F GIVING FBTP1F.
           SUBTRACT FBBF2F FROM FBHT2F GIVING FBTP2F.
       T390G.
           IF FBNEC = ZERO GO TO T390Z.
           IF FBNEC NOT = 1 GO TO T390H.
           IF FBE1F = WFBNPF MOVE FBNPF TO FBE1F ELSE
                             move fbe1f to wzca perform eurc
                             move wcale to fbe1f.
           GO TO T390Z.
       T390H.
           IF FBNEC NOT = 2 GO TO T390I.
           ADD FBE1F to FBE2F.
           IF FBE2F = WFBNPF MOVE FBNPF TO FBE2F ELSE
                             move fbe2f to wzca perform eurc
                             move wcale to fbe2f.
           move fbe1f to wzca. perform eurc. move wcale to fbe1f.
           SUBTRACT FBE1F FROM FBE2F.
           GO TO T390Z.
       T390I.
           ADD FBE1F FBE2F to FBE3F.
           IF FBE3F = WFBNPF MOVE FBNPF TO FBE3F ELSE
                             move fbe3f to wzca perform eurc
                             move wcale to fbe3f.
           move fbe2f to wzca. perform eurc. move wcale to fbe2f.
           move fbe1f to wzca. perform eurc. move wcale to fbe1f.
           ADD FBE1F FBE2F GIVING WFAEF.
           SUBTRACT WFAEF FROM FBE3F.
       T390Z.
           IF FBPOF  NOT = ZERO move fbpof to wzca perform eurc
                                move wcale to fbpof.
           IF FBREF  NOT = ZERO move fbref to wzca perform eurc
                                move wcale to fbref.
           IF FBCOF  NOT = ZERO move fbcof to wzca perform eurc
                                move wcale to fbcof.
      *----> M1198 (F)
      *
      **** ECRITURE EL. PIED DE FACTURE ****
      *
       T400.
           IF FBNEC NOT = ZERO GO TO T405.
           MOVE 1 TO FBNEC.
           MOVE FBNPF TO FBE1F.
           MOVE FBNPD TO FBE1D.
           IF WM = 2 MOVE 28 TO FBD1J
             ELSE    MOVE 30 TO FBD1J.
           MOVE WM TO FBD1M.
           MOVE WA TO FBD1A.
       T405.
      **** Ajout des 3 lignes qui suivent pour avoir le MT HT  ****
      **** reellement facture dans la commande, meme si le     ****
      **** fichier facture a disparu entre temps               ****
           move 0       to fchtf.
           add  fbht1f  to fchtf.
           add  fbht2f  to fchtf.

      *DDE089 si reedition on controle que le pied de facture existant est
      *      identique a celui recalcule sinon cela signifie que la commande ou
      *      le pieds de facture a ete modifie ==> message d'erreur sur
      *       la facture
           if icilp-fact-e1red = 'R'
DD0316        move wor-ffacture to wwwor-ffacture2
DD0351        perform rnl-ffacture
      * on compare l'egalite entre la devise et les net a payer (devise / franc)
      * pour etre sur de la validite du pied de facture et de la facture editee
      * pour palier au cas de modif de commande
              if file-status not = zero or
                    wwfbdev not = fbdev or
                    wwfbnpf not = fbnpf or
                    wwfbnpd not = fbnpd
                 string '* ATTENTION !!! LA FACTURE : ' fbcle-cdesup
                        ' EST DIFFERENTE DE SON PIED'
                        delimited size into immaf-vali-tit
                 perform erreur
                 move zero to wcptr
                 move spaces to woridup
                 perform titre thru ftitre
                 move all '*' to ligne
                 write ligne before 1
                 string '*     ATTENTION !!! LA FACTURE : ' fbcle-cdesup
                        ' EST DIFFERENTE DE SON PIEDS DE FFACTURE'
                        delimited size into ligne
                 write ligne before 1
                 string '*      VOUS DEVEZ DECHIRER CETTE FACTURE'
                        delimited size into ligne
DD0122*          perform identifiant
                 write ligne before page
DDE153*          go to t420
DDE153           go to pied-f
              end-if
           END-IF

           IF icilp-fact-e1red not = 'R'
DD0362        and icilp-fact-arc = spaces
ELGU17        and wtest1 = zero
GPICMT* pour la slovaquie on recalcul le taux de chnage montant devise/montant SK
DD0351        if mmdt-societe = 'SLOVAQ'
  -              divide fbnpd by fbnpf giving fbtch
DD0351        end-if
GPICMT* pour gERGONNE on met la facture a envoyer par l'imprimeur si:
GPICMT*       facture normal avec destockage 
GPICMT*       TTC different de zero
GPICMT*       facture non dematerialisee
DD0465        if mmdt-societe = 'GERGONNE'
  -              if FBCFA = 0
  -                 and FBNPF not = zero
  -                 and wfcfdem not = '1'
DD0516              and witc not = 1
  -                 move 1 to FBIMPRIM
                 else
                    if (witc = 1 or FBCFA not = 0) and FBNPF not = zero
                       move 2 to FBIMPRIM
                    end-if
                 end-if
DD0465        end-if
999999  display "trace cilp-fact1 w-ffacture " 
              perform w-ffacture
              if file-status not = zero
                 string 'Cde: ' fccle-cdesup
DD0316               ' Ecriture facture impos.: ' fbcle-cdesup
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                 move '3' to ocilp-fact-rtn
                 perform erreur
                 go to fin1
              end-if
GPICMT* lecture paramcpt afin de modifier le pieds de facture avant de lancer
GPICMT* le jnl des ventes (factc025) si paramcpt n'existe pas
              MOVE fbtve(1:1) TO CPTARIF
              MOVE FBTAX TO CPTAXE
DD0448* pour code taxe 6 ==> on charge 4
DD0448* pour code taxe > 6 on charge 3, pour ne pas planter le joural des ventes, qui ne devrait plus servir, car remplace par amcp-jnlv, extraction des ca par secteur et taxe
DD0448        if CPTAXE = 6 
  -              move 4 to CPTAXE
  -           else
  -              if CPTAXE > 6
  -                 move 3 to CPTAXE
  -              end-if
DD0448        end-if
  
              IF FBPML > 2 AND FBPML < 9
                 MOVE 2 TO CPPAYS
              ELSE
                 MOVE FBPML TO CPPAYS
              END-IF
              perform rnl-paramcpt
              if file-status not = zero
DD0316           string 'Facture: ' fbcle-cdesup ' PARAMCPT Inexistant '
                        'modifier pieds de facture'
                     delimited size into immaf-vali-tit
                 perform erreur
              end-if
           END-IF
           .
       pied-f.
GPICMT* edition laser 
DD0337     if mmdt-societe = "SLOVAQ"
DD0516        or mmdt-societe = "GERGONNE"
DD0362        or icilp-fact-arc not = spaces
DD0362        or icilp-fact-pdf = "O"

DD0350        IF icilp-fact-e1red = 'R'
                 and w-flag-edit = 1
                 go to pied-f-edit
              end-if
              move 1 to w-flag-edit
              initialize wmmlp-hfac
DD0362* type de facture
              if fbcfa = 5 OR = 6 OR = 7
                 move cmmlp-hfac-type-avoir to immlp-hfac-type
              else
                 move cmmlp-hfac-type-fact to immlp-hfac-type
              end-if
              if fbcfa = 9
                 move cmmlp-hfac-type-prof to immlp-hfac-type
              end-if
              if icilp-fact-arc not = spaces
                 move cmmlp-hfac-type-arc to immlp-hfac-type
                 move fccle-cdesup to immlp-hfac-cdex
              end-if
              move fbcle-cdesup to immlp-hfac-hfac
DD0362        move wor-ffacture to fwor-ffacture2
              move 3 to immlp-hfac-direct
              call "mmlp-hfac1" using mmlp-hfac adl-art
              if ommlp-hfac-rtn not = cmmdt-envi-rtn-ok
                 move ommlp-hfac-liberr to immaf-vali-tit
                 move '3' to ocilp-fact-rtn
                 perform erreur
                 go to fin1
              end-if
           end-if
           .
       pied-f-edit.
           exit.

DDE153* initialisation zone facture
       ini-fac section.
DD9999     move zero to wtest1
DD0350     move 0 to w-flag-edit
           move spaces to w-regroup w-creat

GPICMT* en reedition on lit le pieds de facture
           if icilp-fact-e1red = 'R'
              and icilp-fact-arc = spaces
              move fcnfa-cdesup to fbcle-cdesup
              perform rnl-ffacture
              if file-status = zero
                 move fbreg to w-fbreg
                 move fbcjl to w-fbcjl
              else
                 move zero to w-fbreg w-fbcjl
              end-if
           end-if

           move fcfacturea to w-facturea
           move fcreglepar to w-reglepar
           move fcregrfa   to w-regrfa
           move fcdev      to w-dev
           move fcgeo      to w-geo
           move fcdi2      to w-taxe
           move fcfoa      to w-foa
           move fcrep      to w-repr
DD0394     move fcfeo      to w-type
           .

DDE153* initialisation des zones par cde a traiter
       ini-cde section.
           move zeroes to wwwcle.
           move zero to w-fbht1f w-fbht2f w-fbnmf.
           MOVE ZERO   TO WTEST4 WTEST5 WCPT7.
           MOVE ZERO   TO WMT4 (1) WMT4 (2).
           MOVE 0      TO WMT5 (1) WMT5 (2) WMT6 (1) WMT6 (2)
                          WMT8 WMT9 (1) WMT9 (2).
           MOVE zeroes TO WREMX.
           MOVE SPACE  TO WLIB7 (1) WLIB7 (2) WLIB7 (3) WLIB7 (4)
                          WLIB7 (5).
      * memo reference commande client
DD9999     MOVE FCRCL TO WRCL.

DDE153* cummul commande ds pied de facture
       cum-cde section.
DDE153* cumul poids/volume/qte et raz zones specifiques a une commande
           add fcnco to fbnco
           add fcpbr to fbpbr
           add fctql to fbqtl
           add w-fbht1f to fbht1f
           add w-fbht2f to fbht2f
           add w-fbnmf to fbnmf
           .

DD0122*GPICMT edition nouveau siret et Identifiant CEE
       identifiant section.
           compute wnbl = 69 - linage-counter of etat
           write ligne before wnbl
           move ' _____________________________________________'
               to ligne
           compute wnbl = 70 - linage-counter of etat
           write ligne before wnbl
           move
             '379 622 160 RCS BOURG/SIRET 379 622 160 00016/APE 252H'
                  to ligne
           compute wnbl = 71 - linage-counter of etat
           write ligne before wnbl
           move spaces to ligne
           move '                       Ident.CEE:FR65379622160'
                   to ligne
           .

DD0122* GPICMT saut de page > 34 lignes
       saut34 section.
           IF WCPTR > 34
              write ligne before page
              PERFORM TITRE THRU FTITRE
           END-IF
           .

DD0122* GPICMT saut de page > 31 lignes
       saut31 section.
           IF WCPTR > 31
              write ligne before page
              PERFORM TITRE THRU FTITRE
           END-IF
           .

GPICMT* erreur coherence des codes
DD9999 erreur-codes section.
           string "CODES  Commande " fccle-cdesup
                  " incoherents a verifier"
                    delimited size into immaf-vali-tit
           move '2' to ocilp-fact-rtn
           perform erreur
           .
GPICMT saisie section.
           move spaces to wclientx
           DISPLAY "CLIENT A FACTURER : 999999,F=FIN".
           accept wclientx
           if wclientx = 'F' or = 'f'
              go to fin
           end-if
           move spaces to icilp-fact-e1nocdecx
           DISPLAY "NUMERO DE COMMANDE A FACTURER : 99999999,F=FIN".
           accept icilp-fact-e1nocdecx
           if icilp-fact-e1nocdecx = 'F' or = 'f'
              go to fin
           end-if
           move icilp-fact-e1nocdecx to fccle-cdesup
           perform r-fcommaap
           if file-status not = zero
              string 'Commande ' icilp-fact-e1nocdecx ' Inexistante'
                    delimited size into immaf-vali-tit
              move '2' to ocilp-fact-rtn
              perform erreur
              go to saisie-fin
           end-if
           if fcncl not = wclient
              string 'Client saisi ' wclient
                     ' different de la commande ' icilp-fact-e1nocdecx
                    delimited size into immaf-vali-tit
              move '2' to ocilp-fact-rtn
              perform erreur
              go to saisie-fin
           end-if
GPICMT* on cntrole la validite des codes deja facturee et preparee
           IF FCLIV NOT = 2
              string 'Commande ' icilp-fact-e1nocdecx ' non expediee'
                    delimited size into immaf-vali-tit
              move '2' to ocilp-fact-rtn
              perform erreur
              go to saisie-fin
           end-if
           IF FCFAC NOT = ZERO
              string 'Commande ' icilp-fact-e1nocdecx ' deja facturee'
                    delimited size into immaf-vali-tit
              move '2' to ocilp-fact-rtn
              perform erreur
              go to saisie-fin
           end-if
GPICMT* on force le code piege a zero et le cod a facturer a 1
           move 1 to fcafa
           move zero to fcicp
           perform rw-fcommaap
           if file-status not = zero
              string 'Commande ' icilp-fact-e1nocdecx
                     ' Maj code fcafa et fccip KO'
                    delimited size into immaf-vali-tit
              move '2' to ocilp-fact-rtn
              perform erreur
              go to saisie-fin
           end-if

           .
       saisie-fin.

GPICMT* ajout d'un commentaire pour l'escompte
DD9999 finleau section.
GPICMT* recherche si infos banque a editer
DD0358        perform cpt-banque
GPICMT* pour favotex on edite pas le commentaire d'escompte
  -           if mmdt-societe not = "GERGONNE"
  -              and wcptr-banque = zero
  -              go to finleau-fin
DD0358        end-if
DD0358        move wcptr-banque to wcptr-tot
GPICMT* ajout lignes pour escompte
  -           if mmdt-societe = "GERGONNE"
  -              add 4 to wcptr-tot
DD0358        end-if
              perform saut29
DD0358*       compute wnbl = 56 - linage-counter of etat
DD0358        compute wnbl =
DD0358              ( 58 - wcptr-tot) - linage-counter of etat
              move spaces to ligne
              write ligne before wnbl
DD0358        if wcptr-banque not = zero
  -              perform infos-banque thru infos-banque-fin
DD0358        end-if
DD0351* Ajout infos ventes
              string "Toutes nos ventes sont effectuees selon nos "
                "conditions generales de ventes figurant"
                   delimited by size into ligne
              write ligne before 1
              move spaces to ligne
              string "au verso "
                   delimited by size into ligne
              write ligne before 1
              move spaces to ligne
              STRING "En cas d'escompte pour paiement comptant celui-ci"
                     " sera deduit de notre chiffre d'affaires taxable."
                 DELIMITED SIZE INTO LIGNE
              WRITE LIGNE BEFORE 1
              MOVE SPACES TO LIGNE
              STRING "Le montant de la TVA deductible"
                    " doit etre diminue de la taxe sur l'escompte"
                 DELIMITED SIZE INTO LIGNE
              WRITE LIGNE BEFORE 1
              MOVE SPACES TO LIGNE
              .
       finleau-fin.
           exit.
              .

GPICMT* edition infos banque pour regle par export
DD0358 infos-banque section.
         if ocmcd-gest-reglepar-pays = "BE"
            go to infos-banque-BE
         end-if
         if ocmcd-gest-reglepar-pays = "DE"
            go to infos-banque-DE
         end-if
         .
       infos-banque-standard.
         if ommpa-soci-banq1 not = spaces
            string ommpa-soci-dom1 ' IBAN: ' ommpa-soci-iban1
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
            string '                                 BIC: '
                   ommpa-soci-bic1
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq2 not = spaces
            string ommpa-soci-dom2 ' IBAN: ' ommpa-soci-iban2
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
            string '                                 BIC: '
                   ommpa-soci-bic2
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq3 not = spaces
            string ommpa-soci-dom3 ' IBAN: ' ommpa-soci-iban3
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
            string '                                 BIC: '
                   ommpa-soci-bic3
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq4 not = spaces
            string ommpa-soci-dom4 ' IBAN: ' ommpa-soci-iban4
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
            string '                                 BIC: '
                   ommpa-soci-bic4
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         .
       infos-banque-standard-fin.
         go to infos-banque-fin
         .
       infos-banque-BE.
         if ommpa-soci-banq-be = spaces
            perform infos-banque-standard
         else
            string ommpa-soci-dom-be ' IBAN: ' ommpa-soci-iban-be
                   delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
            string '                                 BIC: '
                   ommpa-soci-bic-be
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         go to infos-banque-fin
            .

GPICMT* recherche nombre de ligne banque a editer
DD0358 infos-banque-DE.
         if ommpa-soci-banq-de = spaces
            perform infos-banque-standard
         else
            string ommpa-soci-dom-de ' IBAN: ' ommpa-soci-iban-de
                   delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
            string '                                 BIC: '
                   ommpa-soci-bic-de
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         go to infos-banque-fin
         .
       infos-banque-fin.
         exit
         .

       cpt-banque section.
         move zero to wcptr-banque
         move "L"      to immpa-pays-fic
         move ocmcd-gest-reglepar-pays to wmmpa-pays-pays
         call 'mmpa-pays1' using mmpa-pays adl-art
DD0351*  if ommpa-pays-cgeo = zero
         if ommpa-soci-banquex = spaces
            go to cpt-banque-fin
         end-if
         if ocmcd-gest-reglepar-pays = "BE"
            go to cpt-banque-BE
         end-if
         if ocmcd-gest-reglepar-pays = "DE"
            go to cpt-banque-DE
         end-if
         .
       cpt-banque-standard.
         if ommpa-soci-banq1 not = spaces
            add 2 to wcptr-banque
         end-if
         if ommpa-soci-banq2 not = spaces
            add 2 to wcptr-banque
         end-if
         if ommpa-soci-banq3 not = spaces
            add 2 to wcptr-banque
         end-if
         if ommpa-soci-banq4 not = spaces
            add 2 to wcptr-banque
         end-if
         .
       cpt-banque-standard-fin.
         go to cpt-banque-fin
         .
       cpt-banque-BE.
         if ommpa-soci-banq-be = spaces
            perform cpt-banque-standard
         else
            add 2 to wcptr-banque
         end-if
         go to cpt-banque-fin
            .
       cpt-banque-DE.
         if ommpa-soci-banq-de = spaces
            perform cpt-banque-standard
         else
            add 2 to wcptr-banque
         end-if
         go to cpt-banque-fin
         .
       cpt-banque-fin.
         exit
         .

DD9999* GPICMT saut de page > 35 lignes
       saut29 section.
DD0337*    IF WCPTR > 35
DD0358     IF WCPTR > ( 35 - wcptr-tot)
              write ligne before page
              PERFORM TITRE THRU FTITRE
           END-IF
           .
GPICMT* edition interligne avant 1er commentaire ligne 
       saut3 section.
DD0350     if wsaut3 = zero
  -           write ligne before 1
  -           add 1 to wcptr
  -           move 1 to wsaut3
DD0350     end-if
           .

GPICMT* lecture de toutes les commandes de la facture pour commandes regroupees
DD0444 maj-dataware section.
GPICMT   move 1 to icmcd-lect-raz
GPICMT   move spaces to wcmcd-lect-e1cdexx
         .

DD0444* ELGU Mise a jour MySQL pour GPI
GPICMT maj-dataware-s.
GPICMT   move ccmcd-lect-trt-numfac to icmcd-lect-trt
GPICMT   move ccmcd-lect-e1facavo-t to wcmcd-lect-e1facavo
GPICMT   move ccmcd-lect-e1type-e to wcmcd-lect-e1type
GPICMT   move fbcle-cdesup      to icmcd-lect-numfac
GPICMT   call 'cmcd-lect1' using cmcd-lect adl-art
GPICMT   if ocmcd-lect-rtn = cmmdt-envi-rtn-ok
GPICMT      string "cmcdmjdw.sh " ccmcd-mjdw-trt-entete 
GPICMT        "," ccmcd-mjdw-even-FACTURATION
GPICMT        ","  lfccle-cdesup  X'00'
GPICMT           delimited by size into sys-var
GPICMT      call "systcc" using sys-var syst-rtn
GPICMT      go to maj-dataware-s
GPICMT   end-if
         .
       maj-dataware-f.
         exit.

GPICMT* recherche code geo du livrea et facturea
ELGU17 rech-geo section.
         move zero to wgeo-livrea
         move "L"      to immpa-pays-fic
         move ocmcd-gest-livrea-pays to wmmpa-pays-pays
         call 'mmpa-pays1' using mmpa-pays adl-art
         move ommpa-pays-cgeo to wgeo-livrea
         move zero to wgeo-facturea
         move "L"      to immpa-pays-fic
         move ocmcd-gest-facturea-pays to wmmpa-pays-pays
         call 'mmpa-pays1' using mmpa-pays adl-art
         move ommpa-pays-cgeo to wgeo-facturea
         .

DD0448* GPICMT* controle code taxe
       ctrl-taxe section.
           move fcgeo to wmmpa-vtax-cgeo
           move fcdi2 to wmmpa-vtax-vtax
           move ocmcd-gest-livrea to wmmpa-vtax-ncl
           if ocmcd-gest-livrea-niveau = 'T'
DD0448*       or ocmcd-gest-saisie = 'C'
              move ocmcd-gest-cdepar-pays to wmmpa-pays-pays
              move ocmcd-gest-cdepar to wmmpa-vtax-ncl
           end-if
           call 'mmpa-vtax1' using mmpa-vtax adl-art
           if ommpa-vtax-rtn not = cmmdt-envi-rtn-ok   
             if mmdt-langue = "FR"
                string fccle-cdesup ' Code taxe/Pays invalide'
                       delimited size into immaf-vali-tit
             else
                string fccle-cdesup 'Tax/Country Code not correct' 
                       delimited size into immaf-vali-tit
             end-if
           end-if
           .

       pro section.

           copy "../copy/pro-fcommaap".
           copy "../copy/pro-fcommac1".
           copy "../copy/pro-fcommac2".
           copy "../copy/pro-fcommac3".
           copy "../copy/pro-fcommac4".
           copy "../copy/pro-fcommac5".
           copy "../copy/pro-paramgpi3".
           copy "../copy/pro-ffacture".
           copy "../copy/pro-fclients".
DD0002*    copy "../copy/pro-fcoadcli".
           copy "../copy/pro-clisuite".
           copy "../copy/pro-parbatch".
           copy "../copy/pro-artdevc1".
           copy "../copy/pro-intracom".
           copy "../copy/pro-artdevc2".
           copy "../copy/pro-numdevis".
DD9999     copy "../copy/pro-paramcpt".
