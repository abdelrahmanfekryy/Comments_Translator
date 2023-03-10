      *DD0351 11/10/12 pase Changement destinataire de mail 
      *DD0351 29/02/12 micn  ajout refenece conrat pour BBJ
      *DD0351 23/02/12 micn Suppr commentaire changement de tarif (DIJE)
      *DD0448 15/12/11 elgu si adresse livraison prise dans l'entete ne pas prendre le commande par
      *                     pour le controle code taxe, adresse livree correcte dans cmcd-gest1
      *DD0351 12/10/11 micn ajout commentaire changement de tarif (DIJE)
      *DD0351 29/08/11 elgu edition code douanier hors france sur test client livrea sauf transitaire
      *DD0448 04/07/11 elgu cntrole code taxe et envoi erreur par mail
      *ELGU17 05/05/11 elgu pour les proforma forcer le nombre d'exemplaire a 1
      *DD0448 14/04/11 elgu traitement code taxe par focntion avec edition texte
      *DD0221 22/03/11 elgu editer la reference commande client en entete
      *DD0351 28/02/11 elgu modif calcul montant ligne avec arrondi sur la 3eme decimale
      *DD0221 12/01/11 anes allongement reference commande client
      *DD0354 28/01/11 elgu suppression commentaire pour hausse de tarif
      *DD0444 02/12/10 elgu ajout maj dataware
      *DD0354 28/10/10 elgu ajout commentaire pour hausse de tarif
      *DD0465 25/08/10 elgu renommer FBFIN en FBIMPRIM
      *DD0448 08/02/10 elgu prendre le numero intracom du client payeur
      *DD0351 07/01/10 elgu prendre les conditions de reglement du pieds de facture en cas de reedition
      *DD0438 10/11/09 micn modif zone edition des conditions de reglement pour 120 jrrs
      *DD0351 18/10/09 elgu controle paramcpt pour validation code taxe par lecture paramcpt
      *DD0438 18/10/09 micn modif longueur WCON
      *DD0438 20/08/09 micn modif appel calcul echeance
      *DD0351 24/06/09 elgu deblocage fcommac4 et ffacturea et fcommaap
      *DD0424 20/04/09 elgu correction adresse livraison et suppression commentaire hausse tarif
      *DD0424 02/04/09 micn suppression du code releve
      *DD0424 13/03/09 elgu editer le libelle pays dans les adresses
      *DD0400 02/01/09 elgu calcul echeance par fonction comme gpi
      *DD0354 13/09/08 elgu commentaire pour augmentation tarif
      *DD0394 19/02/08 elgu ajout du type de commande dans pieds de facture
      *DD0387 04/01/08 elgu initialisation choix pour appel mmpa-regl1
      *DD0350 02/01/08 elgu ne pas creer de trace de facturation si edition arc
      *DDE354 13/11/07 elgu suppression commentaire pour salon BATIMAT
      *DD0354 20/09/07 elgu ajout commentaire pour salon BATIMAT
      *DD0350 19/07/07 micn recup du message d'erreur au retour edition facture/arc
      *DDE0350 13/07/07 elgu 
      *DD0350 13/07/07 elgu suppression des open input et changement niveau
      *                     suite a edition des arc en pdf (blocage SLIV)
      *DD0362 10/07/07 micn edition laser au moment de la creaton du pied de facture
      *DD0362 02/04/07 micn  edition arc chiffre + correction proforma
      *DD0354 02/02/07 elgu suppression DD0354 precedente
      *DDE0358 27/12/06 elgu ajout infos banques (IBAN/BIC)
      *DDE0350 17/11/06 elgu correction reedition facture avec BL regroupes, manquait pied
      *DD0354 31/10/06 elgu ajout commentaire pour les Neerlandophones
      *DD0354 20/10/06 elgu sur demande dije ajout commentaire changement de tarif
      *DDE0326 28/06/06 elgu 
      *DDE0316 03/05/06 elgu correction pb reedition factures avec BL regroupes
      *DD0316 01/05/06 door alongement no cde
      *DD0316 26/04/06 elgu nlle wor-ffacture
      *DD0316 18/04/06 elgu agrandissement et nlle wor-ffacture.mod/fcommac1
      *DD9999 25/01/06 elgu ne pas ouvrir paramgpi et mettre en niveau 3
      *                     si facture proforma demande a la saisie de
      *                     commande le fichier s'ouvrait en input
      *                     et plantage sur la saisie de la commande suivante
      *DD9999 25/11/05 elgu correction creation pied fe facture si facture
      *                     proforma regroupee
      *DD9999 08/11/05 elgu correction edition echeance imposee
      *DDE999 11/10/05 elgu correction client facture
      *DD9999 04/08/05 elgu ne modifier le code reglement que si = zero
      *DD9999 22/06/05 elgu prendre code reglement du relgle par
      *DD0062 29/12/04 micn changement code action trace
      *DDE153 10/01/05 elgu correction reedition facture avec plusieurs BL
      *V30002 15/12/04 elgu
      *DD9999 12/08/04 elgu controle coherence des codes avec affichage erreur
      *                     et ctrl numero intracommunautaire
      *DD0188 28/06/04 elgu trt adresse export
      *DD0002 04/08/04 elgu activer le bloc adresse fiche client
      *DDE999 02/12/02 elgu correction edition mode port sue 2eme exemeplaire
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cglp-fact1.
      *
      ***************************************************************
      * GPICMT EDITION   DES    FACTURES DINAC  REECRIT EN FOCNTION *
      * GPICMT PERMET L EDITION D UNE FACTURE A LA DEMANDE          *
      ***************************************************************
      *
      * GPICMT ****  specifique DINAC
      *
      *DDE999 02/12/02 elgu correction chargement num cde dans la trace
      *DDE999 31/10/02 elgu  ajout demande reponse clavier si erreur
      *DDE284 29/10/02 elgu
      *DDE153 suite 09/10/02 elgu  modif trt port si regroupement BL
      *DDE187 ajout edition libelle langue si code langue client livre # zero
      *M280302 correction edition entete apres saut de page
      *M250302 elgu suppression edition contrevaleur
      *DDE153 ajout regroupement des cdes sur une meme facture
      *DDE109
      *DDE089: edition contre valeur en FRF pour facture EURO suivant code
      *        clcvfrf du client facture
      *        ajout controle validite de la devise
      *DDE045: modification recherche tarif
      *DDE027: lecture fcommaap sur cle 3 afac/fac/cde pour ne lire que les
      *        commandes a facturer
      *M0799 : ajout trt tarif gamme pro
      *M0399a: trt code reglement 11 idem 3
      *M0299 : agrandiss. zones wcale et wzca, passe de 6 a 9 (pb lire et peset)
      *M1298 : ajout test du code decalage de l'echeance si date > 24
      *M1198 : Modifs pour passage a l' EURO
      *M1098 : modif comptage wcptr suite a modif M0698
      *M0798 : mise a jour montant remise et base remise pour les elmts 7 pour
      *        la dematerialisation de la facture
      *M0698 : ajout edition nom douaniere pour les factures avec code taxe
      *        3 ou 4
      *M1197 : mettre 1 dans fbetr (nlle compta)
      *M0697 : ne pas editer transporteur impose 96
      *M0497b: calculer une echeance pour les paiements comptants
      *M0497a: correction mauvais saut de page
      *        mettre 2 lib complementaires sur la meme ligne
      *M0497: si devise = 00 et wgeol # zero mettre "FRANC FRANCAIS" dans le
      *       libelle de la devise
      *M0397: ne pas editer les lignes avec qtl = zero, edition "* RELIQUAT *"
      *M0297: ajout trt code TAXE = 5/6 pour utiliser la tva a 8% pour la CORSE
      *M0197: si numero intracommunautaire du livre a = spaces prendre celui du
      *       facture a
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
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ETAT ASSIGN TO wlabel-etat
                       organization line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ETAT
           DATA RECORD LIGNE
           LINAGE is 48.
      *
       01  LIGNE                       pic x(118).
       01  l1.
            03 filler                  pic xx.
            03 elib                    PIC X(15).
            03 EFAC.
              04 filler                pic x(12).
              04 enliv                 pic 9(6).
              04 filler                pic xxx.
            03 FILLER                  PIC X.
DD0188* agrandissement pour adresse export
DD0188*     03 eliv                    pic x(32).
DD0188      03 eliv.
DD0188       04 filler                 pic x(32).
             04 enfac.
              05 filler                pic x(4).
              05 ecnut                 pic xxx.
            03 filler                  pic x.
            03 ecnufx.
               04 ecnud                pic xxx.
               04 ecnudx.
                 05 filler             pic x.
                 05 ecid               pic x(5).
                 05 filler             pic x.
DD0221           05 erd                pic x(8).
DD0221*          05 erd                pic x(15).
                 05 filler             pic x.
               04 ecnudlx redefines ecnudx.
                 05 ecnul              pic xxx.
                 05 filler             pic x.
                 05 ecil               pic x(5).
                 05 filler             pic xxx.
                 05 ecnur              pic xxx.
DD0221           05 filler             pic x.
DD0221*          05 filler             pic x(8).
               04 ecnuf                pic x(5).
               04 filler               pic x.
               04 ecif                 pic xxx.
               04 erfx.
                 05 filler             pic x.
                 05 erf                pic x(8).
                 05 edatfx redefines erf pic bb9(6).
               04 filler               pic xx.
            03 ecde redefines ecnufx.
               04 filler               pic x(35).
               04 epag                 pic z9 blank zero.
DD0221         04 filler               pic xx.
DD0221*        04 filler               pic x(9).
       01  l2.
            03 filler                  pic x.
            03 enlig                   pic z(4).
            03 filler                  pic x.
            03 edes.
               04 LJE        PIC 99.
               04 LSL1       PIC X.
               04 LME        PIC 99.
               04 LSL2       PIC X.
               04 LAE        PIC 99.
               04 FILLER     PIC X(6).
               04 LMTECH     PIC Z(6)V,99        BLANK ZERO.
               04 ere1.
                 05 LMODIF   PIC X(8).
            03 filler                  pic x(3).
            03 edes2.
               04 eput                 pic z(5)9v,99b blank zero.
               04 eprem                pic z9v,99bb blank zero.
               04 ecnura               pic xxx.
               04 filler               pic x.
               04 edin                 pic x.
               04 ecnup                pic x(5).
               04 filler               pic x.
               04 ecip                 pic x(6).
            03 filler                  pic x.
            03 ecle                    pic x.
            03 filler                  pic x.
            03 evl                     pic xx.
            03 filler                  pic x.
            03 eva                     pic xx.
            03 filler                  pic xx.
            03 eqte                    pic -(5)9v,99b blank zero.
            03 epun                    pic z(5)9v,99 blank zero.
            03 etva                    pic z9 blank zero.
            03 filler                  pic x.
            03 emht                    pic z(5)9v,99bbb blank zero.
       01  l3.
            03 filler                  pic xx.
            03 ettva                   pic z9v,99 blank zero.
            03 ectva                   pic z9.
            03 filler                  pic x.
            03 etht                    pic z(6)9v,99b blank zero.
            03 emtva                   pic z(5)9v,99 blank zero.
            03 filler                  pic x(4).
            03 ereglx.
               04 ecnurp               pic xxx.
               04 filler               pic x.
               04 edate                pic x(6).
               04 filler               pic xx.
               04 emod                 pic xx.
               04 filler               pic xx.
               04 etypd                pic x(5).
               04 filler               pic x(4).
               04 edateb               pic x(6).
             03 filler                 pic x(5).
             03 etoht                  pic z(5)9v,99bb blank zero.
             03 etotva                 pic z(4)9v,99 blank zero.
             03 ecumx                  pic x(15).
             03 enettx.
               04 enet                 pic z(6)9v,99b blank zero.
               04 esign                pic x.
               04 filler               pic xx.
       01  L4.
           02 FILLER         PIC X(16).
           02 libca13.
             03 lexp           pic x(27).
             03 LPAY           PIC XX.
             03 FILLER         PIC XX.
             03 LCLE           PIC XX.
             03 FILLER         PIC XX.
             03 LSIR           PIC X(11).
             03 FILLER         PIC X(10).
       01 l5.
DDE089*    02 filler           pic x(73).
DDE089     02 filler           pic x(58).
DDE089     02 ldemat           pic x(15).
           02 lavoir           pic x(12).
           02 lxx              pic x(11).
           02 filler           pic x(22).
      *----> M0698 (D)
       01 l6.
           02 filler           pic x(6).
           02 lcustx           pic x(13).
           02 lnom             pic x(10).
      *----> M0698 (F)
      *
       WORKING-STORAGE SECTION.
           copy "../copy/wor-intracom".                                 *GPICMT
           copy "../copy/wor-artdevc1".                                 *GPICMT
           copy "../copy/wor-parbatch".                                 *GPICMT
           copy "../copy/wor-clisuite".                                 *GPICMT
DD0002*    copy "../copy/wor-fcoadcli".                                 *GPICMT
           copy "../copy/wor-fclients".                                 *GPICMT
           copy "../copy/wor-ffacture".                                 *GPICMT
           copy "../copy/wor-paramgpi".                                 *GPICMT
           copy "../copy/wor-fcommaap".                                 *GPICMT
           copy "../copy/wor-fcommac1".                                 *GPICMT
           copy "../copy/wor-fcommac2".                                 *GPICMT
           copy "../copy/wor-fcommac3".                                 *GPICMT
           copy "../copy/wor-fcommac4".                                 *GPICMT
           copy "../copy/wor-fcommac5".                                 *GPICMT
           copy "../copy/wor-artdevc2".                                 *GPICMT
           copy "../copy/wor-numdevis".                                 *GPICMT
           copy "../copy/wor-filieres".                                 *GPICMT
           copy "../copy/wor-fartusac".                                 *GPICMT
           copy "../copy/wor-trpntran".                                 *GPICMT
DDE187     copy "../copy/wor-languear".
DD0351     copy "../copy/wor-paramcpt".                                 *GPICMT
      *----> M0698 (D)
           copy "../copy/wor-fartusap".                                 *GPICMT
      *----> M0698 (F)
DD0362     copy "../copy/mmlp-hfac.com".                                *GPICMT
DDE089     copy "../copy/mmcp-devb.com".                                *GPICMT
DDE089     copy "../copy/mmdt-parb.com".                                *GPICMT
DDE089     copy "../copy/mmaf-vali.com".                                *GPICMT
DDE089     copy "../copy/mmti-date.com".                                *GPICMT
DDE089     copy "../copy/mmpa-regl.com".                                *GPICMT
DDE089     copy "../copy/mmpa-devi.com".                                *GPICMT
DDE089     copy "../copy/mmca-date.com".                                *GPICMT
DDE069     copy "../copy/mmtr-trac.com".                                *GPICMT
DD0002     copy "../copy/cmcd-gest.com".                                *GPICMT
DD0358     copy "../copy/mmpa-pays.com".
DD0358     copy "../copy/mmpa-soci.com".
DD0400     copy "../copy/mmca-eche.com".
DD0400     copy "../copy/mmaf-vrep.com".
DD0444     copy "../copy/cmcd-lect.com".
DD0444     copy "../copy/cmcd-mjdw.com".
DD0448     copy "../copy/mmpa-vtax.com".
DD0351     copy '../copy/mmlp-mail.com'.                                *GPICMT
       77  cp                pic 99.
DD0326 77  WNFANA            PIC 9(7).
DD0326 77  WNFACA            PIC 9(7).
       77  WTOTFA            PIC S9(7)V99.
       77  WTOTAV            PIC S9(7)V99.
       77  WCPTR             PIC s999.
GPICMT* compteur ligne de banque a editer
DD0358 77  WCPTR-banque      PIC s999.
DD0358 77  wnbl              PIC 99.
       77  wctl              pic 999.
       77  WMONTX            PIC S9(11)V999.
       77  WMONT2            PIC S9(11)V99.
       77  WMONT             PIC S9(9)V99.
       77  WHTD              PIC S9(7)V99.
       77  WNPF              PIC S9(7)V99.
       77  WMT               PIC S9(9)V99.
       77  WHTDF             PIC S9(7)V99.
       77  WTXF              PIC S9(7)V99.
       77  WWNBF             PIC s9.
       77  I                 PIC s9.
       77  WHT1              PIC S9(9)V99.
       77  WREM              PIC S9(9)V99.
       77  WFAEF             PIC S9(7)V99.
       77  WCAL              PIC S9(9)V99.
       77  wcumul            pic s9(9)v99.
      *
DD0337 01  sys-var         PIC X(80).
DD0337 01  sys-rtn         PIC 9.
       01  syst-zone.
           02  syst-data1          pic x(40).
           02  syst-data2          pic x(40).
           02  filler              pic x(4)   value " -d.".
           02  filler              pic x      value x"00".
       01  syst-rtn                pic s9(4)  comp.
      *
       01  wlabel-etat pic x(64) value space.
       01  var-name pic x(80).
       01  var-data pic x(80).
       01  zon-cde.
           02    cde-data    pic x(80).
           02    filler      pic x              value x'00'.
       01  ZONES.
DD9999* memo date echeance impose
            02 w-fcdae pic 9(6).
DDE999* trt des erreur avec reponse par accept
            02 wtrt           pic x.
DDE187* code langue client facture
            02 wlngfac              pic 99.
DD0351* code groupement              
            02 wclrdi               pic x(3).

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
DDE153* memo code surveillance et gencod client regle
           02 w-suc                pic 9.
           02 w-gen                pic 9.
DD0394* type commande
DD0394     02 w-type               pic x.
DD0350* flag reedition facture laser
           02 w-flag-edit          pic 9.
DDE045* memo type du client facture
DDE045     02 wtypfac        pic x.
DD0354* memo famille stat du client facture
DD0354     02 wfamstat       pic x(5).
DD0354* memo pays du client facture
DD0354     02 wpays          pic x(2).

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

      *----> M0698 (D)
           02 wnarx.
             03 w90          pic 99.
             03 filler       pic x(5).
      *----> M0698 (F)
           02 cinq           pic 99             value 22.
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
           02 WORIDUP        PIC X(12).
           02 WFACAVO        PIC X(12).
           02 WPROCON        PIC X(12).
      * wtest1 ==> test traitement pied de facture (pas de pied pour les 
      *            conditionnelles et proforma
           02 WTEST1         PIC 9.
           02 WTEST2         PIC 9.
           02 WTEST4         PIC 9.
           02 WTEST5         PIC 9.
           02 WNCLP          PIC 9(6).
           02 WGEOP          PIC 9.
           02 WGEOL          PIC 9.
      *----> M0297 D
           02 wegtvp         PIC 99.
           02 FILLER         REDEFINES wegtvp.
             03 filler       PIC 9.
             03 wegtv1       PIC 9.
      *----> M0297 F
           02 WCODTAX1       PIC 99.
           02 FILLER         REDEFINES WCODTAX1.
             03 WPAR1        PIC 9.
             03 WTV1         PIC 9.
           02 WCODTAX2       PIC 99.
           02 FILLER         REDEFINES WCODTAX2.
             03 WPAR2        PIC 9.
             03 WTV2         PIC 9.
           02 wadcnt.
             03 wadncl       pic 9(6).
             03 wadnar       pic 9(7).
             03 wadsrf       pic 99.
DD0424*    02 WCRE           PIC 9.
           02 WQTELIV        PIC 9(10)V99.
           02 WCLE.
DD0326       03 WNUM         PIC 9(7).
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

      * DDE089 code edition contre valeur en francs
           02 wclcvfrf       pic x.
      * numero de commande a la demande
           02 wcdex.
DD0316        03 wcde        pic 9(7).
DDE153        03 wind        pic 9.
      * date du jour pour trace
           02 wdatej              pic 9(6).
           02 filler redefines wdatej.
             03 wjj               pic 99.
             03 wmj               pic 99.
             03 waj               pic 99.
      * nom pgm pour vali1
           02  wnom-prog                PIC X(10) value 'cglpfact1'.
DD0448     02  wgeo-livrea              pic x.

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
           02 WCON           REDEFINES WCONX  PIC 9(5).
DD0400*    02 wlcon.
  -   *       03 wcon1e      pic 99 blank zero.
  -   *       03 wlje        pic x(9) value " J FM LE ".
  -   *       03 wlje        pic x(9).
DD0400*       03 wcon2e      pic 99 blank zero.
DD0438     02 wlcon          pic x(21).
      *----> M0497b (D)
           02 wlconc.
              03 filler      pic x(5) value "SOUS ".
DD0438        03 wcon2c      pic 9(3) blank zero.
              03 filler      pic x(6) value " JOURS".
      *----> M0497b (F)
           02 weanc.
             03 windc        pic xxx.
             03 wcnudc       pic x(3).
             03 wcidc        pic x(5).
           02 weanl.
             03 windl        pic xxx.
             03 wcnudl       pic x(3).
             03 wcidl        pic x(5).
           02 WFCRCL                   PIC X(19).
           02 WF                       REDEFINES      WFCRCL.
            03 WRFJ                    PIC XX.
            03 WRFM                    PIC XX.
DD0221*     03 filler                  pic x.
DD0221*     03 WRF2                    PIC X(8).
DD0221      03 WRF2                    PIC X(15).
           02 wedreg         pic 9.
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
           02 WPGCLE.
             03 WPGCLE1      PIC X(8).
             03 WPGCLE2      PIC 99.
           02 WREPX.
      *----> M1198 (D)
      *      03 WREP3        PIC 99V9(6).
             03 WREP3        PIC 9(5)V9(6).
      *----> M1198 (F)
           02 WCPT7          PIC 9.
           02 WREMX.
             03 WREM7        OCCURS 5.
               04 WLIB7      PIC X(30).
               04 WQUI       PIC S9(6)V99.
      *----> M0798 (D)
               04 wbas7      pic 9(6)v99.
               04 wmon7      pic 9(6)v99.
               04 wcle7      pic x(13).
      *----> M0798 (F)

      *----> M0799 (D)
           02 wtaro.
             03 wt1          pic x.
             03 filler       pic xx.
      *----> M0799 (F)
DD0316     02 WNFA9          PIC 9(7).
           02 WREM1          PIC S9(9)V99.
           02 WREM2          PIC S9(9)V99.
           02 WWNCL          PIC 9(6).
      *
           02 WDNEL          PIC 99.
           02 WDNUM          PIC 9(5).
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
           02   wver         pic xxx.
           02  wdopx.
             03 wdop1        pic x(16).
             03 wdop2x.
               04 wdop2      pic 9(7).
               04 filler     pic x.
             03 wdop3        pic 99.
           02  wnart         pic 9(7).
           02  wnsrf         pic 99.
           02  wi            pic 9.
           02  wgdes.
             03 wgtyp        pic 9.
             03 wgnou        pic 9(6).
           02 WPORT          PIC X(8).
DD0326     02 WLNUM          PIC 9(7)B.
DD0326     02 WLNFAC         PIC 9(7).
           02 wad1           pic x(26).
           02 wclcde         pic 9(6).
      *----> M0497a (D)
           02 wmep           pic 9.
      *----> M0497a (F)
      *
      *----> M0497b (D)
           02 wmoisx.
              03 wmois occurs 12 pic 99.
      *----> M0497b (F)
       01   LINALP.
DD0424      02 WFCADL OCCURS 6         PIC X(32).
            02 wfcbdix.
              03 wfcccp                  pic 9(5)b.
DD0188        03 WFCBDI                  PIC X(29).
            02 WFCNTR                  PIC X(14).
            02 wfcnud                  pic xxx.
            02 wfccid                  pic x(5).
            02 wclliv                  pic 9(6).
       01   LINALF.
DD0424      02 wfcadf occurs 6         PIC X(32).
            02 wbdifx.
              03 wccpf                   pic 9(5)b.
DD0188        03 WBDIF                   PIC X(29).
            02 wclfac                    pic 9(6).

      *       memo enreg pied de facture pour controle identique qd reedition
DD0316   copy "../copy/wor-ffacture.mod" replacing ==(pref)== by ==ww==.
      *
       LINKAGE SECTION.
           copy '../copy/cglp-fact.com'.                                *GPICMT
           copy "/usr/action/ADL/copy/wor-adl".
      *
       PROCEDURE DIVISION using cglp-fact adl-art.
       DEB SECTION.

DD0358* recherche infos societe
  -        move mmdt-societe to immpa-soci-societe
DD0358     call 'mmpa-soci1' using mmpa-soci adl-art

      *DDE089
      * reedition terminee ==> ferm = 'F' ==> fermeture des fichiers
           move spaces to immaf-vali-tit.
           if icglp-fact-ferm = 'F' go to fin.
       D10.
      *DDE089
DDE999     move spaces to wtrt
           move '0' to ocglp-fact-rtn
           move spaces to ocglp-fact-liberr.
      * recuperation date du jour                                       *GPICMT
           move 'D' to immti-date-taj
           call 'mmti-date1' using mmti-date adl-art
           if ommti-date-rtn not = '0'
              move ommti-date-rtn    to ocglp-fact-rtn
              move ommti-date-liberr to ocglp-fact-liberr
              perform erreur
              go to fin
           end-if
           move wmmti-date-jma to wdatej

      *----> M0798 (D)
      *    move 'I' to gfkey.
           move 'W' to gfkey.
      *----> M0798 (F)
           perform op-fcommac4.
           move 'W' to gfkey.
           perform op-parbatch.
           move 'W' to gfkey.
           perform op-ffacture.
           move 'W' to gfkey.
           perform op-fcommaap.
           move 'E' to gfkey.
           perform op-intracom.


      *DDE089 ouverture etat une seule fois en cas de reedition
           if icglp-fact-ferm = spaces
              string 'ADLPID' x'00' delimited by size
                  into var-name
              move space to var-data
              call 'genvcc' using var-name var-data
DDE089        if icglp-fact-e1dem = 'D'
                 string 'cglpfacd' mmdt-lieu '.'
                  var-data delimited by ' '
                                     into wlabel-etat
              else
                 string 'cglpfact' mmdt-lieu '.'
                  var-data delimited by ' '
                                     into wlabel-etat
              end-if
              OPEN OUTPUT ETAT
           END-IF.

      *----> M0497b (D)
           move "312831303130313130313031" to wmoisx.
      *----> M0497b (F)
      *---------------------------------------------                    *GPICMT
      * controle si facturation en lot ou a la demande                  *GPICMT
           if icglp-fact-e1nocdecx = zero or = spaces                   *GPICMT
              go to trt-console                                         *GPICMT
           else
              move icglp-fact-e1nocdecx to wcdex
           end-if
      * facture a la demande: verification date si non donnee on force  *GPICMT
      *                       avec la date du jour                      *GPICMT
      *---------------------------------------------                    *GPICMT
           if icglp-fact-e1date = zero or = spaces
              move 'D' to immti-date-taj
           else
              move 'J' to immti-date-taj
           end-if
           move icglp-fact-e1date to wmmti-date-jma
           call 'mmti-date1' using mmti-date adl-art
           if ommti-date-rtn not = '0'
              move ommti-date-liberr to immaf-vali-tit
              move ommti-date-rtn    to ocglp-fact-rtn
              perform erreur
              go to fin
           end-if
           move wmmti-date-jjour to wj
           move wmmti-date-jmois to wm
           move wmmti-date-janne to wa
           go to trt-deb.

      *
      **** TRAITEMENT CONSOLE ****
      *
       trt-console.
DDE999* on met "#" ds code trt de vali1 pour attendre une reponse au    *GPICMT
      * clavier en cas d'erreur par vali1                               *GPICMT
           move '#' to wtrt.

           if icglp-fact-direct not = '3'
              string 'TRAITEMENT CONSOLE IMPOSSIBLE EN MODE '
                    icglp-fact-direct
                    delimited size into immaf-vali-tit
              perform erreur
              go to fin.

           move spaces to wcdex
           DISPLAY "EDITION DES FACTURES,T=toutes ,999999=N.cde, F=FIN".
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
      *----> M1198 (D)

      *DDE089 appel fonction recherche si parametre bloque
           move "FACTURE000" to immdt-parb-cle
           call 'mmdt-parb1' using mmdt-parb adl-art
           if ommdt-parb-rtn not = '0'
              move ommdt-parb-liberr to immaf-vali-tit
DD0350        move ommdt-parb-liberr to ocglp-fact-liberr
DD0350        move cmmdt-envi-rtn-err to ocglp-fact-rtn
DD0350*       perform erreur
              go to fin
           end-if

DDE089* recup devise de base en compta
           call 'mmcp-devb1' using mmcp-devb adl-art
           if ommcp-devb-rtn not = '0'
              move ommcp-devb-liberr to immaf-vali-tit
              move ommcp-devb-rtn    to ocglp-fact-rtn
              perform erreur
              go to fin
           .

      ****************** ajout lecture devise EURO (62)
      * appel fonction lecture devise
           move 'f' to immpa-devi-trt
           move 00  to wmmpa-devi-cdev9
           move 'C' to immpa-devi-tfc
DDE089     move 'L' to immpa-devi-aff
           call 'mmpa-devi1' using mmpa-devi adl-art
           if ommpa-devi-rtn not = '0'
              move ommpa-devi-liberr to immaf-vali-tit
              move ommpa-devi-rtn    to ocglp-fact-rtn
              perform erreur
              go to fin
           else
      * controle des taux
              if ommpa-devi-tcd = zero or ommpa-devi-teu = zero
                 move '2' to ocglp-fact-rtn
                 move "PARAMETRE DEVISE EURO (50) INCORRECT"
                          to immaf-vali-tit
                 move '2' to ocglp-fact-rtn
                 perform erreur
                 go to fin
              end-if
           end-if
           MOVE ommpa-devi-teu TO wpgbt62.
      *----> M1198 (F)
           if icglp-fact-e1red not = 'R'
DD0362        and icglp-fact-arc = spaces
              MOVE "FACTURE000" TO PHCLE
              perform r-parbatch
              if file-status not = zero
                 move "EL.FACTURE000 ABSENT" to immaf-vali-tit
                 move '3'                    to ocglp-fact-rtn
                 perform erreur
                 go to fin
              end-if
              MOVE PHANOC (1) TO WNFANA
              MOVE PHANOC (2) TO WNFACA
           END-IF.

           MOVE "PARAFITVA1" TO PGCLE.
DDE069     perform rnl-paramgpi.
           if file-status not = zero
              move "EL.TVA1 ABSENT" to immaf-vali-tit
              move '3'              to ocglp-fact-rtn
              perform erreur
              go to fin.
           MOVE PGFZON TO WENRTVA1.
           MOVE "PARAFITVA2" TO PGCLE.
DDE069     perform rnl-paramgpi.
           if file-status not = zero
              move "EL.TVA2 ABSENT" to immaf-vali-tit
              move '3'              to ocglp-fact-rtn
              perform erreur
              go to fin.
           MOVE PGFZON TO WENRTVA2.
           MOVE SPACES TO WPARAM.
           MOVE "AVANCE" TO WPARP (1) WPARP (3) WPARP (4) WPARP (5)
                            WPARP (6) WPARP (7) WPARP (8) WPARP (9).
           MOVE "FRANCO" TO WPARP (2).
           MOVE "CLIENPORT1"  TO PGCLE.
DDE069     perform rnl-paramgpi.
           if file-status not = zero
                     GO TO t10.
       D25.
           MOVE PGZON3 (3) TO WPARP (PGCL2n).
DDE069     perform nnl-paramgpi.
           if file-status not = zero
                   GO TO t10.
           IF PGCL1 NOT = "CLIENPORT"  GO TO t10.
           IF PGCL2 NOT NUMERIC         GO TO t10.
           GO TO D25.
      *
      **** TRAITEMENT FACTURATION ****
      *
       T10.
           MOVE 0 TO WTOTAV WTOTFA.
DD0316*    MOVE ZERO TO FCNUM FCNIN.
DD0316     MOVE ZERO TO fccle-cdesup
      *DDE089 lecture directe de la commande qd reedition avec controle
      *       deja facturee
           if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
DDE153* on lit les commandes des sur cle facture/numero cde
DDE153*       move wcdex to fccle
DDE153*       perform r-fcommaap
DDE153        move spaces to fccle-cdesup
DDE153        move wcde to fcnfa-cdesup
DDE153        perform snlsk4-fcommaap
DDE153        if file-status = zero
DD0351           perform nnl-fcommaap
DDE153        end-if
DDE153        if file-status not = zero or fcnfa-cdesup not = wcde
                 string 'FACTURE NON TROUVEE: ' wcde
                     delimited size into immaf-vali-tit
              move '3'               to ocglp-fact-rtn
              perform erreur
              go to fin
              end-if
DD0326     move fcnfa-cdesup to wlnfac
DDE153     move zero to wcdex
           perform ini-fac
           perform ini-cde
           go to t20a
           END-IF.

      *----> DDE027 (D)
      * lecture fcommaap sur cle 3 ==> seulement les a facturer
      * fcafa = 1 ==> a facturer
      * fcfac = 0 ==> pas facturer apres facturation passe a 1
DDE153* si trt BL non regroupee ou pour un BL donne lecture sur cle 3
      * sinon lecture sur cle 5 dans l'ordre des clients
DD0362     if icglp-fact-arc = "X"
              move 0 to fcafa fcfac
           else
              move 1 to fcafa
              move 0 to fcfac
           end-if
DD0316     move wcdex to fccle-cdesup
           if icglp-fact-e1regroup not = '1'
              perform snlsk3-fcommaap
           ELSE
              move 1 to fcafa
              move 0 to fcfac fcdev fcgeo fcfoa fcdi2
              move spaces to fcclefac fccle-cdesup
              perform snlsk5-fcommaap
           END-IF

           if file-status not = zero
              move "RIEN A FACTURER" to immaf-vali-tit
              move '3'               to ocglp-fact-rtn
              perform erreur
              go to fin.
      *----> DDE027 (F)

GPICMT* lecture non blocante si reeditiion
DD0351     if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
              perform nnl-fcommaap
           else
              perform n-fcommaap
           end-if
           if file-status not = zero
              string 'FICHIER VIDE, status: (' file-status ' )'
                     delimited size into immaf-vali-tit
              move '3'            to ocglp-fact-rtn
              perform erreur
              go to fin.
      * controle facture a la demande
           if wcdex not = spaces and not = zero
DD0316        if fccle-cdesup not = wcdex
                 string 'Commande inexistante ou facturee : 'wcdex
                    delimited size into immaf-vali-tit
                 move '2' to ocglp-fact-rtn
                 perform erreur
                 go to fin
              end-if
           END-IF.

      *
      **** DEBUT FACTURE ****
      *
       T20.

      *-------------------------------------                            *GPICMT
      * si fcafa > 1 ==> fini                                           *GPICMT
      * si fcfac > 0 ==> fini                                           *GPICMT
           if fcafa > 1 or fcfac > 0 go to fin1.                        *DDE027

DDE153* init debut trt d'une facture
           perform ini-fac.

DDE153* test si factures a regrouper demande
           IF icglp-fact-e1regroup = '1'
              if fcregrfa not = '1'
                 go to t520
              end-if
           ELSE
              if fcregrfa = '1'
                 go to t520
              end-if
           END-IF


      * controle facture a dematerialiser si demander sauf si facture a *GPICMT
      *                                               la demande        *GPICMT
           if wcdex = zero                                              *GPICMT
              if fcfdem = "1"                                           *GPICMT
                 if icglp-fact-e1dem not = 'D'                          *GPICMT
                 go to t520
                 end-if
              else
                 if icglp-fact-e1dem = 'D'
                    go to t520
                 end-if
              end-if
           END-IF
           .
      *-------------------------------------                            *GPICMT
DDE153 t20-s.
           perform ini-cde.

DD0362     if icglp-fact-arc not = spaces
              go to t20a
           end-if
           IF FCAFA NOT = 1 GO TO T520.
DD9999     IF FCLIV NOT = 2 perform erreur-codes GO TO T520.
DD9999     IF FCFAC NOT = ZERO perform erreur-codes GO TO T520.
DD9999     IF FCICP NOT = ZERO perform erreur-codes GO TO T520.

       t20a.
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
              move ommpa-devi-rtn    to ocglp-fact-rtn
              perform erreur
              go to t520
           else
      ***************** si taux en euro = zero on ne traite pas la commande
              if ommpa-devi-teu = zero
                 string 'Cde: ' fccle-cdesup ' ' "TAUX DEVISE ???? :  "
                   delimited size into immaf-vali-tit
                 move '2' to ocglp-fact-rtn
                 perform erreur
                 go to t520
              end-if
           END-IF.
           move ommpa-devi-teu  to wpgbteu.
           move ommpa-devi-ceu  to wpgbceu.
           move ommpa-devi-ldev to wlde.

DD0316     MOVE fccle-cdesup TO WCLE.
DD9999*    MOVE ZERO TO WTEST1.
           move fcnbf to wwnbf
ELGU17* pour les proformat on force le nombre d'eemplaire a 1
  -        if fcfoa = 9
  -           move 1 to WWNBF
EGGU17     end-if
           MOVE FCITC TO WITC.
           MOVE FCNAF TO WNAF.
           MOVE FCNOF TO WNOF.
           MOVE FCGEO TO WGEOL.
DDE153     move fcreg to w-regl
      *ne pas editer devise
           if fcdev = 00 and wgeol = zero move spaces to wlde.

           IF FCFRA NOT = ZERO  MOVE WPARP (FCFRA) TO wport
                          ELSE  MOVE "PORT AVANCE" TO   wport.
           move fcrcl to wfcrcl.
           MOVE SPACES TO LINALP linalf.
           move fcncl to wclliv wclcde wclfac.
      *
      ** MISE EN WSS DE L'ENTETE **
      *
           MOVE SPACE TO WORIDUP WFACAVO WPROCON wad1.
DD9999* memo date echeance imposee
           move fcdae to w-fcdae
      ***  TT DES NOUVEAUX BLOCS ADRESSES  (JUIN 90)                 ***
           move zero to wcon.
           move spaces to weanc weanl wfcntr.
      ****** ADRESSE DE LIVRAISON = CELLE DE FCOMMAAP
DD0316     move fccle-cdesup to icmcd-gest-numcde
DD0448     move '0' to wgeo-livrea
           call "cmcd-gest1" using cmcd-gest adl-art
           if ocmcd-gest-rtn = cmmdt-envi-rtn-ok
              move ocmcd-gest-livrea to wclliv
              move ocmcd-gest-cdepar to wclcde
GPICMT* mep adresse de livraison
V30002        perform adr-liv
V30002* mep adresse facturation
DD9999        move ocmcd-gest-facturea to wclfac
              perform adr-fact
GPICMT* recherche code geographique du livre a
ELGU17        perform rech-geo
GPICMT* controle validite code taxe avec code pays
DD0448        if mmdt-societe not = 'SLOVAQ' and not = 'CHINE'
  -              perform ctrl-taxe
  -              if ommpa-vtax-rtn not = cmmdt-envi-rtn-ok
  -                 perform erreur
  -                 go to t520
  -              end-if
DD0448        end-if
           else
              string 'Bloc Adresse Commande ' fccle-cdesup ' Incorrect'
                   delimited size into immaf-vali-tit
              move cmmdt-envi-rtn-err to ocglp-fact-rtn
              perform erreur
              go to t520
           end-if
           .

       t11.
      ****** recherche cnud et cid client commande
           move wclcde to fincl.
           perform rnlsk1-filieres.
           if file-status = zero move fiean to  weanl weanc.
      ***  TT DU FACTURE A                                           ***
           MOVE wclfac TO CLNCL.
DDE069     perform rnl-fclients.
           if file-status not = zero
              string 'Cde: ' fccle-cdesup ' Client: ' wclfac 
                     ' inexistant' ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocglp-fact-rtn
              perform erreur
              go to t520
           end-if

DD0351* on recupere le groupement pour mettre une refernce de contrat
           move clrdi to wclrdi
DDE187     move cllng to wlngfac
DDE045     move cltype to wtypfac.
DD0354     move clfamstat to wfamstat
DD0354     move clpays to wpays
DDE153     if fcregrfa = '1'
              move clnbf to wwnbf
ELGU17* pour les proformat on force le nombre d'eemplaire a 1
  -           if fcfoa = 9
  -              move 1 to WWNBF
EGGU17        end-if
           end-if

      *----> M0799 (D)
           move cltaro to wtaro.
      *----> M0799 (F)
DDE089     move clcvfrf to wclcvfrf.

           move clcnu to wfcnud.
           move clcid to wfccid.
           move clnom to wad1.
       t11a.
           exit.
      ***  TT DU REGLE PAR                                           ***
       t15.
DDE153     IF fcreglepar = CLNCL GO TO T22.
DDE153     MOVE fcreglepar TO CLNCL.
      *****READ FCLIENTS INVALID PERFORM INVAL8 THRU INVAL9
DDE069     perform rnl-fclients.
           if file-status not = zero
              string 'Cde: ' fccle-cdesup ' Client: ' fcreglepar 
                     ' inexistant' ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocglp-fact-rtn
              perform erreur
              go to t520
           end-if
           .
       T22.
           MOVE CLNCL TO WNCLP.
           MOVE CLGEO TO WGEOP.
DD0424*    MOVE CLCRE TO WCRE.
           MOVE CLCON TO WCON.
DD9999     if w-regl = zero
              move clcrt to w-regl
           end-if
GPICMT* en reedition on prend le mode de reglement et les conditions dans ffacture
DD0351     if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
  -           move w-fbreg to w-regl
  -           move w-fbcjl to WCON
DD0351     end-if
           move clcsu to w-suc
           move clgen to w-gen
           .

       T40.

      *DDE089 controle code reglement                                   *GPICMT
           move 'C' to immpa-regl-cof
DDE153     move w-regl to wmmpa-regl-regl
DD0387     move spaces to immpa-regl-choix
           call 'mmpa-regl1' using mmpa-regl adl-art
           if ommpa-regl-rtn not = '0'
              move ommpa-regl-rtn    to ocglp-fact-rtn
              string 'Cde: ' fccle-cdesup ' ' ommpa-regl-liberr
                   delimited size into immaf-vali-tit
              perform erreur
              go to t520
           end-if

      * DDE089 controle conditions de reglement
           move 'C' to immpa-regl-choix
           move wcon to wmmpa-regl-con9
           call 'mmpa-regl1' using mmpa-regl adl-art
           if ommpa-regl-rtn not = '0'
              string 'Cde: ' fccle-cdesup ' Cond. de reglement ???'
                             delimited size into immaf-vali-tit
              perform erreur
              go to t520
           end-if

           if wclliv = wclcde go to t40b.
           move spaces to weanl.
      ****** recherche cnud et cid du livre a
           move wclliv to fincl.
           perform rnlsk1-filieres.
           if file-status not = zero go to t40b.
           move fiean to weanl.
       t40b.
      ****** SAUVEGARDE TRANSPORTEUR RETENU OU IMPOSE OU HABIRUEL
           IF FCNTR NOT = SPACES MOVE FCNTR TO wfcntr GO TO t41.
      *----> M0697 (D)
      *    IF FCTRS NOT = ZERO move fctrs to tnntr
           if fctrs not = zero and fctrs not = 96
                               MOVE FCTRS TO tnntr
      *----> M0697 (F)
             ELSE              MOVE FCTHA TO tnntr.
           perform rnl-trpntran.
           if file-status = zero move tnnom to wfcntr.
       t41.
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
DD0326*    MOVE FCNUM TO WLNUM.
DD0326     MOVE fccle-cdesup(1:7) TO WLNUM.

           if icglp-fact-e1red not = 'R'
DD0362        and icglp-fact-arc = spaces
              IF WTEST1 = 1 MOVE WNFACA TO WLNFAC
                 ADD 1 TO WNFACA
              ELSE        MOVE WNFANA TO WLNFAC
                 ADD 1 TO WNFANA
              end-if
           END-IF.
      *----> M0497 (D)
      *
      ** INITIALISATION PIED DE FACTURE **
      *
       T61.

DDE153      move 1 to w-creat
DD0316     MOVE spaces TO wor-ffacture.
DD0326*    MOVE WLNFAC TO FBNFA.
DD0326     MOVE WLNFAC TO fbcle-cdesup.
DD0316* pour pouvoir reediter une ancienne facture regroupee
DD0316* la facture demandee
DD0316     if icglp-fact-e1red = 'R'
DD0316        move fcnfa-cdesup to fbcle-cdesup
DD0316     end-if
           MOVE WDATE9 TO FBDAF.
           MOVE FCFOA  TO FBCFA.
DD0316     MOVE fccle-cdesup  TO FBNCD-cdesup.
           MOVE WGEOL  TO FBPML.
           MOVE FCNCL  TO FBNCL.
           MOVE WGEOP  TO FBPMP.
           MOVE WNCLP  TO FBNCP.
           MOVE FCREP  TO FBNRH.
DD0394     MOVE w-type TO FBTYP.
DD0424*    MOVE WCRE   TO FBREL.
DD0424     MOVE zero   TO FBREL.
           MOVE 9      TO FBIG9.
           MOVE FCDTR  TO FBDTR.
           MOVE FCTVE  TO FBTVE.
           MOVE FCNAF  TO FBFIL.
           MOVE FCFRA  TO FBFRA.
           MOVE FCCTA  TO FBCOT.
DDE153     MOVE w-regl TO FBREG.
           MOVE FCCSC  TO FBSUC.
           MOVE FCCOK  TO FBCOK.
           MOVE FCGEN  TO FBGEN.
           MOVE FCNUT  TO FBCNUT.
           MOVE FCCPA  TO FBCPA.
           MOVE FCLIP  TO FBLIP.
           MOVE FCTHA  TO FBTRR.
           MOVE FCLIA  TO FBLIA.
           MOVE FCDEV  TO FBDEV.
           MOVE FCDI2  TO FBTAX.
           move fclpr  to fblpr.
           move fcvia  to wfcvia                                        *M280302
           move fcfdem to wfcfdem                                       *M280302
           MOVE ZERO   TO FBCCO FBETR FBTRM FBJVT.
           MOVE ZERO   TO FBIL1 FBIL2 FBNEC FBTCO1 FBTCO2 FBPCO1 FBPCO2
                          FBRTD.
           MOVE SPACE  TO FBIMPRIM.
      *----> M0997 (D)
           move 1 to fbetr.
      *----> M0997 (F)
DDE153* init des zones propres a une commande si regroupement
           if w-regroup = '1'
DD0326*       move zero to fbncd fbnrh fbfil fbcot fbtrr
DD0326        move zero to fbncd-cdesup fbnrh fbfil fbcot fbtrr
              move wclfac to fbncl
              move wtaro to fbtve
              move w-suc to fbsuc
              move w-gen to fbgen
              move spaces to fblia
           end-if
           .
       T62.
      ***** Instruction ajoutee sous UNIX ici        **********
      ***** Doit convenir pour Fact+Duplicata        **********
           MOVE zero   TO WCPTR.
           PERFORM TITRE THRU FTITRE.
           MOVE ZERO   TO WCODTAX1 WCODTAX2.
           MOVE 0      TO FBNMF FBHT1F FBBF1F FBTX1F FBTP1F FBHT2F
                          FBBF2F FBTX2F FBTP2F FBPOF.
           MOVE 0      TO FBREF FBCOF FBAFF FBNPF FBE1F FBE2F FBE3F
                          FBNMD  FBHT1D FBBF1D.
           MOVE 0      TO FBTX1D FBTP1D FBHT2D FBBF2D FBTX2D FBTP2D
                          FBPOD  FBRED FBCOD FBAFD.
           MOVE 0      TO FBNPD FBE1D FBE2D FBE3D.
           move zero to wcumul.
DDE153     MOVE zero   TO FBNCO.
DDE153     MOVE zero   TO FBPBR.
DDE153     MOVE zero   TO FBQTL.
      *
      **** AJOUT TRAITEMENT INTRACOMMUNAUTAIRE    ***
       T63.
           MOVE SPACES TO WNIC.
DD0448*    IF WGEOL NOT = 1 GO TO lececc1.
DD0448     IF WGEOL NOT = 1 GO TO edit-ltaxe.
      ***  Il ne faut pas creer intracom si pro-forma,mais on edite ***
      ***  les numeros gpi et client. (d'ou supp. ligne suivante    ***
      *    if fcfoa     = 9 go to lececc1.
           IF WTEST1    = 1 GO TO lececc1.
GPICMT* on prend le numero intracommunautaire du client payeur
DD0448*    MOVE FCNCL  TO CLSNCL.
      *M1096 -----------------------------------------------------------------
DD0448*    if wclliv not = zero move wclliv to clsncl.
      *M1096 -----------------------------------------------------------------
DD0448*    perform rnl-clisuite.
DD0448*    if file-status not = zero
      *M0197 -----------------------------------------------------------------
      *          GO TO T64.
      *    MOVE CLSNTV TO WNIC.
DD0448*          go to t63a.
DD0448*    if clsntv not = spaces move clsntv to wnic go to t64.
DD0448*t63a.
DD0448*    move fcncl to clsncl.
DD0448*    if wclfac not = zero move wclfac to clsncl.
DD0448     MOVE fcreglepar  TO CLSNCL.
DDE069     perform rnl-clisuite.
           if file-status not = zero go to t64.
           move clsntv to wnic.
      *M0197 -----------------------------------------------------------------
       T64.
GPICMT* message si numero intracommunautaire vide
DD9999     if wnic = spaces
              string "Numero Intracom Vide,  Commande " fccle-cdesup
                     " traitee mais a reediter"
                    delimited size into immaf-vali-tit
              perform erreur
           end-if

           MOVE SPACES TO LIGNE.
      *----> M1098 (D)
      *    ADD 1       TO WCPTR.
      *----> M1098 (F)
           MOVE WPAY   TO LPAY.
           MOVE WCLN   TO LCLE.
           MOVE WSIR   TO LSIR.
           MOVE "VOTRE IDENTIFICATION CEE : " TO LEXP.
           WRITE LIGNE BEFORE 1.
           MOVE SPACES TO LIGNE.
           ADD  1      TO WCPTR.
DD0448     evaluate fbtax
  -          when 6
  -           string "     "
  -                  "EXONERATION DE TVA EN FRANCE - ARTICLE 196 DE LA "
  -                  "DIRECTIVE 2006/112/CE" delimited size into ligne
  -          when other
  -           string "     "
  -                  "EXONERATION DE TVA EN FRANCE - ARTICLE 262 TER.1 "
  -                "DU CGI " delimited size into ligne 
  -        end-evaluate
  -        WRITE LIGNE BEFORE 2.
  -        MOVE SPACES TO LIGNE.
  -        ADD  2      TO WCPTR.
  -        go to LECECC1.
DD0448 edit-ltaxe.
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
      ********     lecture    des elements 01 : libelles  ************
           if w-regroup = '1'
              perform entete-cde
           end-if

DD0316     move fccle-cdesup  to pfjcle1.
DDE069     perform rnl-fcommac1.
           if file-status not = zero
                GO TO lecc2.
DD0316     if fccle-cdesup not = pfjcle1 go to lecc2.
       T70.
      *
      ** TRT EL.3 **
      *
           IF pfjli1 = SPACE AND pfjli2 = SPACE
                     AND pfjli3 = SPACE AND pfjli4 = SPACE GO TO lecc2.
           MOVE ZERO TO I.
       T70A.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           IF pfjli1 = SPACES GO TO T70A2.
           MOVE pfjli1  TO edes.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       t70a2.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           IF pfjli2 = SPACES GO TO T70A3.
           MOVE pfjli2  TO edes.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       t70a3.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           IF pfjli3 = SPACES GO TO T70B.
           MOVE pfjli3  TO edes.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T70B.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           IF pfjli4 = SPACE GO TO T70C.
           MOVE pfjli4 TO edes.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T70C.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
      *
       lecc2.
      ********  lecture    des elements 04 : lignes articles   *******
           move fccle-cdesup  to fcnoc2-cdesup.
           move zero to fcnlg.
           perform snlsk4-fcommac2.
           if file-status not = zero
                GO TO lecc4.
       lec2.
DDE069     perform nnl-fcommac2.
           if file-status not = zero
                GO TO lecc4.
           if fcnoc2-cdesup not = fccle-cdesup go to lecc4.

           if icglp-fact-arc = spaces
              if fcqtl = zero 
                 go to lec2 
              end-if
           end-if
        .

      ** TRT EL.4 **
      *
       T92.
      *
      * TYPE DE LIGNE = 1 *
      *
           IF FCLIG NOT = 1 GO TO T100.
       t92aa.
           PERFORM TEST2b THRU FTEST2b.
           IF WTEST2 = 3 GO TO DISP.
           if fclig = 2 and fcre1 not = spaces add 1 to wcptr.
      *----> M1098 (D)  suppression de la ligne de modif M0698
      *----> M0698 (D)
      *    if fbtax = 3 or fbtax = 4 add 1 to wcptr.
      *----> M0698 (F)
      *----> M1098 (D)
           if wcptr > 18 perform titre thru ftitre.
      *----> M1198 (D)
           if fclig = 2 and fcre1 not = spaces subtract 1 from wcptr.
      *----> M1198 (F)
           move fcnlg to enlig.
           MOVE FCDES TO EDES.

DDE187* edition libelle en langue suivant code langue client facture
           IF wlngfac not = zero
              move wlngfac to lglng
              move fcnar to lgnar
              perform rnl-languear
              if file-status = zero
                 move lglia to edes
              end-if
           END-IF
           move fcpht to eput.

      *----> M0799 (D)
      *--*             edition prix avant remise et % remise pour les clients
      *--*             avec numero de tarif commencant par 5 et article gde cl5
           if wt1 = "5" and fctgc = 5 move fcpth to eput
                                      move fctrem to eprem.
      *----> M0799 (F)
           move 106   to ecnura.
DDE284*    move 3     to edin.
           move fcnar to fanar1.
           move fcsre to fansr1.
           move 01    to fanma1.
           perform rnl-fartusac.
           if file-status = zero move facnu to ecnup
DDE284                           move fapay to edin
                                 move facip to ecip
                                 move facle to ecle
                                 move fava  to eva
                                 move favl  to evl.
           move fcqtl to eqte.
           move fcpht to epun.
           move fctvp to etva.
           if icglp-fact-arc = spaces
              move fcqtl to wqteliv 
           else
              move fcqtc to wqteliv
           end-if
           IF FCMES > 3 AND FCMES < 7 GO TO T92a.
           GO TO T92b.
       T92a.
           IF FCMES = 06 MULTIPLY 1000 BY wqteliv
                   ELSE MULTIPLY  100 BY wqteliv.
       t92b.
           multiply fcpht by wqteliv giving wmontx.
           IF FCPRX = 2 GO TO T92c.
           IF FCPRX = 3 GO TO T92e.
           GO TO T92g.
       T92c.
           DIVIDE 100 INTO wmontx.
           GO TO T92g.
       T92e.
           DIVIDE 1000 INTO wmontx.
       T92g.
DD0351     multiply wmontx by 1 giving wmont2 rounded
           move wmont2 to emht.
           ADD wmont2 TO WMT4 (WTEST2) wcumul.
           write ligne before 1.
           add 1 to wcptr.
           move spaces to ligne.
           if fclig = 2 and fcre1 not =spaces
                   move "VOTRE REFERENCE : " to edes
                   move fcre1                to ere1
                   write ligne before 1
      *----> M0497a (D)
                   add 1 to wcptr
      *----> M0497a (F)
                   move spaces to ligne.
      *----> M0698 (D)
DD0448*    if fbtax not = 3 and fbtax not = 4 go to t92h.
DD0448*    move fcnar to wnarx.
           if w90 = 90 go to t92h.
DD0448     if fcfoa < 5 and FCNPO not = '999999999' and not = spaces
  -           if wgeo-livrea = '0'
  -              go to t92h
  -           end-if
  -           if wcptr > 18
  -              perform titre thru ftitre
  -           end-if
  -           move "CUSTOMS REF: " to lcustx
DD0448*       move fcnar to fanar.
DD0448*       move 01    to fanma.
DD0448*       perform rnl-fartusap.
DD0448*       if file-status = zero move facod to lnom.
  -           move FCNPO to lnom
  -           write ligne before 1
  -           add 1 to wcptr
  -           move spaces to ligne
DD0448     end-if
           .
      *----> M0698 (F)
       t92h.
           exit.
       lecc3.
      ********  lecture    des elements 04 : libelles complem. *******
           move zeroes to fccle3-cdesup.
           move fccle-cdesup  to fcnoc3-cdesup.
           move 04     to fcnel3.
           move fcart  to fcart3.
           move 10     to fclig3.
      *----> M0497a (D)
           move zero to wmep.
      *----> M0497a (F)
           perform snl-fcommac3.
           if file-status not = zero
                GO TO t93.
       lec3.
DDE069     perform nnl-fcommac3.
           if file-status not = zero
                GO TO t93.
           if fcnoc3-cdesup not = fccle-cdesup
                    or  fcart3 not = fcart go to t93.
           if  fcnlg3 not = fcnlg go to lec3.
           perform t110 thru t110e.
           go to lec3.
       T93.
      *----> M0497a (D)
           if wmep = 1 WRITE LIGNE BEFORE 1
                       move zero to wmep
                       ADD 1 TO WCPTR
                       MOVE SPACE TO LIGNE.
      *----> M0497a (F)
      ***  TT INTRACOMMUNAUTAIRE POUR LES ELT 4     ***
           if fcfoa     = 9 go to t95g.
           IF WGEOL NOT = 1 GO TO T95G.
       T95B.
           MOVE SPACES TO ENRICO.
           MOVE FBNCL  TO ICCLF.
DD0316*    MOVE FBNFA  TO ICNFA.
DD0316     MOVE fbcle-cdesup  TO ICNFA-cdesup.
           MOVE FBCFA  TO ICCFA.
           MOVE FBTAX  TO ICCTA.
           MOVE FBDEV  TO ICCDE.
           MOVE "L"    TO ICCIL.
           MOVE WNIC   TO ICNIC.
           MOVE WDATE9 TO ICDFA.
           MOVE wad1   TO ICRCF.
DD0316*    MOVE WNUM   TO ICNOC.
DD0316*    MOVE WNIN   TO ICNIN.
DD0316     MOVE wcle   TO ICNCD-cdesup.
           MOVE WPORT  TO ICPOR.
       T95C. EXIT.
       T95D.
           MOVE fcnar  TO ICNAR.
           move fcsre  to icsrf.
           MOVE fcqpb  TO ICQPB.
           MOVE fcqtl  TO ICQTL.
           MOVE fcpht  TO ICPU.
           MOVE WMONT2 TO ICMON.
           MOVE fcmes  TO ICUM.
           MOVE fcprx  TO ICUP.
           MOVE fcpdu  TO ICPDS wpdu.
           IF fcpdu  = 0  GO TO T95E.
           MULTIPLY wpds  BY fcqtl  GIVING ICPTO ROUNDED.
       T95E.
           MOVE fcdes  TO ICLIA.
       T95F.
           IF WORIDUP = "*DUPLICATA*" GO TO T95G.
      ***  POUR NE PAS RE CREER LES LIGNES A CHAQUE DUPLICATA   ***
      *****WRITE ENRICO.

DDE089     if icglp-fact-e1red = 'R' go to t95g.

           perform w-intracom.
           IF file-status NOT = "00"
              string 'Cde: ' fccle-cdesup ' ANOMALIE / INTRACOM, ST: '
                                      file-status
                               delimited size into immaf-vali-tit
              move '3' to ocglp-fact-rtn
              perform erreur
              perform subnum
              go to fin1.
       T95G. EXIT.
      ***  FIN INTRACOMMUNAUTAIRE : EL 4 + COMMUN  ***
      *T96.
      *    EXIT.
       T97.
           go to lec2.
      *
      * TRT LIGNE = 2 *
      *
       T100.
           IF FCLIG NOT = 2
              string 'Cde: ' fccle-cdesup ' type ligne #1/2, TL: ' 
                      fclig delimited size into immaf-vali-tit
              perform erreur
              go to t92aa.
      *
      *  RECHERCHE TAUX COMMISSION DANS ART./DEVIS
           IF WITC NOT = 9  GO TO T102F.
           MOVE FBNCL TO ADNCL1 wadncl.
           MOVE FCNAR TO ADNAR1 wadnar.
           MOVE FCSRE TO ADSRF1 wadsrf.
           move 02    to adtye1.
           move 00    to adunix1.
DDE069     perform rnl-artdevc1.
           if file-status not = zero
                     GO TO T102D.
       T102.
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
DDE069     perform rnl-artdevc1.
           if file-status not = zero
                     GO TO T102F.
           GO TO T102.
      *
       T102F.
           PERFORM TEST2b THRU FTEST2b.
           IF WTEST2 = 3 GO TO DISP.
           go to t92aa.
      *
      * TRT LIGNE > 9 *
      *
       T110.
      *    ADD 1 TO J.
      *    IF J > 3 GO TO T190.
           IF FCCL1 NOT = "G" AND FCCL1 NOT = "F" GO TO T110b.
      *----> M0497a (D)
      *    IF WCPTR > 34 write ligne before page
      *                  PERFORM TITRE THRU FTITRE.
      *    MOVE FCLC1 TO edes.
           if wmep > 1 move zero to wmep.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           add 1 to wmep.
           if wmep = 1 MOVE FCLC1 TO edes go to t110b
             else      move fclc1 to edes2.
      *----> M0497a (F)
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T110b.
           IF FCCL2 NOT = "G" AND FCCL2 NOT = "F" GO TO T110c.
      *----> M0497a (D)
      *    IF WCPTR > 34 write ligne before page
      *                  PERFORM TITRE THRU FTITRE.
      *    MOVE FCLC2 TO edes.
           if wmep > 1 move zero to wmep.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           add 1 to wmep.
           if wmep = 1 MOVE FCLC2 TO edes go to t110c
             else      move fclc2 to edes2.
      *----> M0497a (F)
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T110c.
           IF FCCL3 NOT = "G" AND FCCL3 NOT = "F" GO TO t110e.
      *----> M0497a (D)
      *    IF WCPTR > 34 write ligne before page
      *                  PERFORM TITRE THRU FTITRE.
      *    MOVE FCLC3 TO edes.
           if wmep > 1 move zero to wmep.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           add 1 to wmep.
           if wmep = 1 MOVE FCLC3 TO edes go to t110e
             else      move fclc3 to edes2.
      *----> M0497a (F)
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       t110e. exit.
       lecc4.
      ********  lecture    des elements 05 A 10                *******
           move zeroes to fccle4-cdesup.
           move fccle-cdesup  to fcnoc4-cdesup.
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
           if fcnoc4-cdesup not = fccle-cdesup
DD0351        perform rw-fcommac4 go to flec4.
      *
      ** TRT EL.5 **
      *
       T120.
           IF FCNEL4 NOT = 5 GO TO T130.
           PERFORM TEST2 THRU FTEST2.
           IF WTEST2 = 3 GO TO DISP.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           MOVE FCDOP TO edes.
           MOVE FCQUL TO eqte.
           MOVE FCPUH TO epun.
           IF FCMON NOT = ZERO GO TO T120A.
           MULTIPLY FCQUL BY FCPUH GIVING WMONT.
           if fcsig = "-" multiply -1 by wmont.
           GO TO T120B.
       T120A.
           MOVE 0 TO WMONT.
           IF FCSIG = "-" MOVE "-" TO esign
                          SUBTRACT FCMON FROM WMONT
             ELSE         ADD      FCMON TO   WMONT.
       T120B.
           ADD WMONT TO WMT5 (WTEST2) wcumul.
           MOVE WMONT TO emht.
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
DDE153     if fcfra = 2 go to lec4.
           PERFORM TEST2 THRU FTEST2.
           IF WTEST2 = 3 GO TO DISP.
           if wcptr > 18 perform titre thru ftitre.
           IF FCSIG = "-" SUBTRACT FCMON FROM WMT6 (WTEST2)
                          subtract fcmon from wcumul
             ELSE         ADD      FCMON TO   WMT6 (WTEST2)
                          add      fcmon to   wcumul.
           MOVE FCDOP TO edes.
           IF FCSIG = "-" MOVE "-" TO esign.
           MOVE FCMON TO emht.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
           PERFORM T170A THRU T170D.
           IF WGEOL = 1 GO TO T150A.
           GO TO lec4.
      *
      ** TRT EL.7 **
      *
       T140.
           IF FCNEL4 NOT = 7 GO TO T150.
           ADD 1 TO WCPT7.
           MOVE FCDOP TO WLIB7 (WCPT7).
DD0362     if icglp-fact-arc = spaces
              MOVE FCQUL TO WQUI (WCPT7) 
           else
              move fcqui TO WQUI (WCPT7)
           end-if
      *----> M0798 (D)
           move fccle4-cdesup to wcle7(wcpt7).
      *----> M0798 (F)
           IF FCSIG = "-" MULTIPLY -1 BY WQUI (WCPT7).
           IF WGEOL = 1  GO TO T150A.
           GO TO lec4.
      *
      ** TRT EL.8 **
      *
       T150.
           IF FCNEL4 NOT = 8 GO TO T160.
DDE153     if fcfra = 2 go to lec4.
           IF FCSIG = "-" SUBTRACT FCMON FROM WMT8
                          subtract fcmon from wcumul
             ELSE         ADD      FCMON TO   WMT8
                          add      fcmon to   wcumul.
           MOVE FCDOP TO edes.
           IF FCSIG = "-" MOVE "-" TO esign.
           MOVE FCMON TO emht.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
           PERFORM T170A THRU T170D.
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
           IF FCNEL4 NOT = 9 GO TO T170.
           if fcfeo not = 4 and fcfeo not = 5 go to t161.
           move fcdop to wdopx.
           if wdop1 = "REFERENCE GPI :" move wdop2 to wnart
                                        move wdop3 to wnsrf.
       t161.
           PERFORM TEST2 THRU FTEST2.
           IF WTEST2 = 3 GO TO DISP.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           MOVE FCDOP TO edes.
           MOVE FCQUL TO eqte.
           MOVE FCPUH TO epun.
           MOVE FCMON TO emht.
           IF FCSIG = "-" MOVE "-" TO esign
                          SUBTRACT FCMON FROM WMT9 (WTEST2)
                          subtract fcmon from wcumul
             ELSE         ADD      FCMON TO   WMT9 (WTEST2)
                          add      fcmon to   wcumul.
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
           IF FCNEL4 NOT = 10 GO TO lec4.
           IF FCDOP = SPACE GO TO T170A.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           MOVE FCDOP TO edes.
           PERFORM T170C.
       T170A.
           IF FC1LC = SPACE GO TO T170B.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           MOVE FC1LC TO edes.
           PERFORM T170C.
       T170B.
           IF FC2LC = SPACE GO TO T170D.
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           MOVE FC2LC TO edes.
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
           if file-status not = zero
              string 'Cde: ' fccle-cdesup ' Erreur devis, ref: ' 
                      wnart wnsrf
                     delimited size into immaf-vali-tit
                                     perform erreur
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
           IF WCPTR > 18 PERFORM TITRE THRU FTITRE.
           MOVE WGDES TO edes.
           if adcaf(wi) = 4 move "(modif)" to lmodif.
           PERFORM T170C.
       flec43.
           if wi < 3 add 1 to wi go to flec42.
           go to flec41.
      *
      ** TRT EL.99 **
       lecc5.
      ********  lecture    des elements 99                     *******
           move zeroes to fccle5-cdesup.
           move fccle-cdesup  to fccle5-cdesup.
           move 99     to fcnel5.
DDE069     perform rnl-fcommac5.
           if file-status not = zero
                GO TO t200.
           if fccle5-cdesup not = fccle-cdesup go to t200.
      *
       T180.
           IF FCNEL5 NOT = 99
              MOVE FCNEL5 TO WDNEL  MOVE FCNUM TO WDNUM
              MOVE FCNIN TO WDNIN
              string 'Cde: ' fccle-cdesup ' *** ELT: ' wdnel ' inconnu'
                 delimited size into immaf-vali-tit
              perform erreur
              GO TO lecc5.
           MOVE FCTEC TO WTEC.
           MOVE 1 TO WTEST4.
       T200.
           ADD WMT4 (1) WMT5 (1) TO w-FBNMF.
           ADD w-FBNMF WMT9 (1)    TO w-FBHT1F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (1) TO w-FBHT1F.
DDE153*    MOVE WMT6 (1) TO FBPOF.
DDE153     add  WMT6 (1) TO FBPOF.
DDE153*    MOVE WMT8     TO FBAFF.
DDE153     add  WMT8     TO FBAFF.
           MOVE WCODTAX1 TO FBC1F.
           IF WTEST5 NOT = 2 GO TO T200A.
           ADD WMT4 (2) WMT5 (2) TO w-FBNMF.
           ADD WMT4 (2) WMT5 (2) WMT9 (2) TO w-FBHT2F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (2)  TO w-FBHT2F.
           ADD WMT6 (2) TO FBPOF.
           MOVE WCODTAX2 TO FBC2F.
      *
      * CALCUL ET EDITION REMISES
      *
       T200A.
           IF WCPT7 = ZERO GO TO T510.
           MOVE ZERO TO I.
           ADD w-FBHT1F w-FBHT2F GIVING WHT1.
      ***  AJOUT 4/11/86 PAS FAIRE DE REM./PORT SI AVANCE (3 LIGNES)
DDE153     IF FCFRA = 2 GO TO T205.
           SUBTRACT WMT6 (1) FROM WHT1.
           SUBTRACT WMT6 (2) FROM WHT1.
       T205.
           ADD 1 TO I.
           IF I > WCPT7 GO TO T510.
           IF WCPTR > 16 PERFORM TITRE THRU FTITRE.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 1.
           MOVE "BASE HORS TAXES AVANT REMISE" TO LIBCA13.
           MOVE WHT1 TO emht.
           IF WHT1 < 0 MOVE "-" TO esign.
      *----> M0798 (D)
           if wht1 < 0 multiply wht1 by -1 giving wbas7(i)
             else      move wht1           to     wbas7(i).
      *----> M0798 (F)
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MULTIPLY WQUI (I) BY WHT1 GIVING WREM.
           DIVIDE 100 INTO WREM ROUNDED.
           ADD WREM TO FBREF.
           ADD WREM TO WHT1.
           MOVE WREM TO WREM2.
DDE153     IF FCFRA = 2 GO TO T205A.
           SUBTRACT WMT6 (1) FROM w-FBHT1F.
       T205A.
           MULTIPLY WQUI (I) BY w-FBHT1F GIVING WREM1.
           DIVIDE 100 INTO WREM1 ROUNDED.
           ADD WREM1 TO w-FBHT1F.
           SUBTRACT w-FBHT1F FROM WHT1 GIVING w-FBHT2F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (1) TO w-FBHT1F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (2) TO w-FBHT2F.
      ***
           MOVE WLIB7 (I) TO edes.
           MOVE WQUI (I)  TO eqte.
           MOVE WREM      TO emht.
DDE153     add wrem to wcumul
      *----> M0798 (D)
           move wrem to wmon7(i).
      *----> M0798 (F)
           IF WREM < 0 MOVE "-" TO esign.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           ADD 3 TO WCPTR.
      ***  LA REMISE GLOBALE CONCERNE LE NET MARCHANDISE
      ***  LES 2 LIGNES SUIVANTES AVAIENT ETE SUPPRIMEES AVEC LA
      ***  MODIFICATION DE 1986 : CAS DES ETIQUETTES (14/1/93)
      ***  DONC ON RETABLIT:TOT NM - % REM = NOUVEAU TOT NET MARCHANDISE
           MULTIPLY WQUI (I) BY w-FBNMF GIVING WREM2.
           DIVIDE 100 INTO WREM2 ROUNDED.
           ADD WREM2 TO w-FBNMF.
       T205B.
           IF WGEOL NOT = 1 GO TO T205.
           if fcfoa     = 9 go to t205.
           PERFORM T95B THRU T95C.
           MOVE WLIB7 (I) TO ICLIA.
           MOVE WREM    TO ICMON.
           PERFORM T95F THRU T95G.
           GO TO T205.
      *
      **** REECRITURE EP FCOMMDES ****
      *
       T510.

DDE153     perform cum-cde.
           IF WORIDUP = "*DUPLICATA*" GO TO T510A.
DD0362* pas de mise a jour code facture si edition arc
           if icglp-fact-arc = spaces
DD0316        MOVE fbcle-cdesup TO FCNFA-cdesup 
              MOVE 1 TO FCFAC 
           end-if

           if icglp-fact-e1red not = 'R'
              perform rw-fcommaap
              if file-status not = zero
                 string 'Reecriture FCOMMAAP impossible: ' fccle-cdesup
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                 move '3' to ocglp-fact-rtn
                 perform erreur
                 go to fin1
              end-if
           END-if.

      *DDE089 creation trace                                            *GPICMT
DDE069*    creation trace par fonction
           IF icglp-fact-e1red not = 'R'
DD0350        and icglp-fact-arc = spaces
              move space to wmmtr-trac
DD0062        move 'M' to immtr-trac-action
              string wcle delimited by size into immtr-trac-num
              if icglp-fact-comptoir = 'C'
DDE999           string 'facturation comptoir de la commande: ' wcle
                     '  sous le n.: ' fbcle-cdesup
                     delimited by size into immtr-trac-commentaire
              else
DDE999           string 'facturation de la commande: ' wcle
                     '  sous le n.: ' fbcle-cdesup
                  delimited by size into immtr-trac-commentaire
              end-if

              perform cre-trace
           END-IF.
      *
      * TEST DUPLICATA *
      *
       T510A.
ELGU17* pour edition laser on ne fait pas plusieurs exemplaire
ELGU17     if icglp-fact-arc not = spaces
  -           or icglp-fact-pdf = "O"
  -           go to T519
ELGU17     end-if
           SUBTRACT 1 FROM WWNBF.
      *----> M0798 (D)
      *    IF WWNBF < 1 GO TO T520.
           IF WWNBF < 1 GO TO T519.
DDE153* GPIWARNING pas de duplicata pour cde regroupee
           if w-regroup = '1'
              string 'Duplicata non édité pour facture ' fbcle-cdesup
                   delimited size into immaf-vali-tit
              perform erreur
              go to t519
           end-if

      *----> M0798 (F)
DDE153     if w-creat = '1'
              perform pied
GPICMT* appel maj de toutes les commandes de la facture  dans la dataware 
DD0444        perform maj-dataware
           end-if

           perform ini-cde

           MOVE "*DUPLICATA*" TO WORIDUP.
           GO TO T62.
      *----> M0798 (D)
       T519.
      *
      *********** reecriture base et montant remise pour les elmnts 7
      *
           if wcpt7 = zero go to t520.
           move 1 to i.
       t519a.
           if i > wcpt7 go to t520.
           move wcle7(i) to fccle4-cdesup
GPICMT* lecture non blocante si reedition
DD0351     if icglp-fact-e1red not = 'R'
              perform r-fcommac4
           else
              perform rnl-fcommac4
           end-if
           if file-status not = zero go to t519b.
           move wbas7(i) to fcpuh.
           move wmon7(i) to fcmon.

           if icglp-fact-e1red not = 'R'
              perform rw-fcommac4
              if file-status not = zero
                 string 'Reecriture FCOMMAC4 impossible: ' wcle7(i)
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                 move '3' to ocglp-fact-rtn
                 perform erreur
                 go to fin1
              end-if
           END-IF.

       t519b.
           add 1 to i.
           if i > zero go to t519a.
      *----> M0798 (F)
      *
      **** LECTURE EP SUIVANT ****
      *
       T520.
      *DDE089 si facture a la demande ==> fini
           if wcdex not = zero go to t520-f.

GPICMT* mecture non blcante si reedition
DD0351     if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
              perform nnl-fcommaap
           else
              perform n-fcommaap
           end-if
           if file-status not = zero
DD0350*         GO TO FIN1.
DD0350          GO TO t520-f.

DDE153     if icglp-fact-e1red = 'R'
              if fcnfa-cdesup= fbcle-cdesup
                 perform ini-cde
                 go to t20a
              else
                 go to t520-f
              end-if
           end-if
DDE153     if fcafa > 1 or fcfac > 0 go to t520-f.

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
       t520-f.
           if w-creat = '1'
              perform pied
GPICMT* appel maj de toutes les commandes de la facture  dans la dataware 
DD0444        perform maj-dataware
           end-if

DDE153     if icglp-fact-e1red = 'R'
              go to fin1
           end-if

           if wcdex not = zero go to fin1.

           GO TO T20
           .
      *reecriture parametre numerotation des factures en fin de programm*GPICMT
       FIN1.
           if icglp-fact-e1red = 'R' go to fini.

DD0362     if icglp-fact-arc not = spaces
              go to fin
           end-if

           MOVE "FACTURE000" TO PHCLE.
           perform r-parbatch.
           if file-status not = zero
              string 'Relecture PARBATCH impossible: ' PHCLE
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocglp-fact-rtn
              perform erreur
              go to fin
           end-if
           MOVE WNFANA TO PHANOC (1).
           MOVE WNFACA TO PHANOC (2).

           perform rw-parbatch
           if file-status not = zero
              string 'Reecriture PARBATCH impossible: ' PHCLE
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
              move '3' to ocglp-fact-rtn
              perform erreur
           end-if

           GO TO FIN.
       TEST2.
      *----> M0297 D
DD0448*    if fbtax = 5 or fbtax = 6 move fctvp4 to wegtvp
DD0448     if fbtax = 5              move fctvp4 to wegtvp
                                     move 1      to wegtv1
                                     move wegtvp to fctvp4.
      *----> M0297 F
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
      *----> M0297 D
DD0448*    if fbtax = 5 or fbtax = 6 move fctvp  to wegtvp
DD0448     if fbtax = 5              move fctvp  to wegtvp
                                     move 1      to wegtv1
                                     move wegtvp to fctvp.
      *----> M0297 F
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
           MOVE fbcle-cdesup TO WNFA9.
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
           GO TO T520.
      *
      **** EDITION DE L'ENTETE ****
      *
       titre.
           move wcptr to wctl.
           if wcptr = zero go to titr1.
           SUBTRACT      wcptr FROM CINQ GIVING wcptr.
           write ligne before wcptr.
           move "  CUMUL PARTIEL" TO ECUMX.
      *    add fbht1f fbht2f fbtx1f fbtx2f fbaff giving etoht.
           move wcumul to etoht.
           write ligne before page.
           move spaces to ligne.
       titr1.
           MOVE SPACES  TO LIGNE.
           write ligne before 3.
           move spaces to ligne.
           move 025    to ecnut.
DDE153     if w-regroup not = '1'
              move wcnudc to ecnud
              move wcidc  to ecid
              move wrf2   to erd
           end-if
           move 23356  to ecnuf.
           move 001    to ecif.
           move wlnfac to erf.
           write ligne before 3.
           move spaces to ligne.
DDE153     if w-regroup not = '1'
              move wcnudl to ecnul
              move wcidl  to ecil
           end-if
           move 123    to ecnur.
           move 23356  to ecnuf.
           move 001    to ecif.
DDE153     if w-regroup not = '1'
DDE153*M280302 editer zone memo sinon edition cde suivante
      *       move fccle to erf
              move wcle  to erf
           end-if
           write ligne before 4.
           move spaces to ligne.
           move wdate  to edatfx.

      *-----------------------------------------                        *GPICMT
      *DDE089 ajout libelle pour facture demateria lise                 *GPICMT
DDE153*M280302 editer zone memo sinon edition cde suivante
      *    if fcfdem = '1'                                              *GPICMT
           if wfcfdem = '1'                                             *GPICMT
              move "*   DEMAT  *" to ldemat                             *GPICMT
           end-if
      *-----------------------------------------                        *GPICMT

DDE153*M280302 editer zone memo sinon edition cde suivante
      *    if fcfoa = 5 or fcfoa = 6 or fcfoa = 7 move "***AVOIR***"
           if fbcfa = 5 or fbcfa = 6 or fbcfa = 7 move "***AVOIR***"
                               to lavoir          move all "X" to lxx.
           write ligne before 1.
           move spaces to ligne.
           move wprocon to lavoir.
DDE153     if w-regroup not = '1'
              move "adresse de livraison" to eliv
              move wclliv to enliv
           end-if
           move wclfac to enfac.
DD0424     write ligne before 1.
           move spaces to ligne.
DDE153     if w-regroup not = '1'
              move wfcadl(1) to eliv
           end-if
           MOVE wfcadf(1) to ecde.
           write ligne before 1.
           move spaces to ligne.
DDE153     if w-regroup not = '1'
              move "Representant : " to elib
DDE153*M280302 editer zone memo sinon edition cde suivante
      *       move fcrep to efac
              move fbnrh to efac
              move wfcadl(2) to eliv
           end-if
           move wfcadf(2) to ecde.
           write ligne before 1.
           move spaces to ligne.
DDE153     if w-regroup = '1' go to titr2.

           move "Livraison    : " to elib.
DDE153*M280302 editer zone memo sinon edition cde suivante
      *    if fcvia < 2 or fcvia > 5
           if wfcvia < 2 or wfcvia > 5
                        move "PAR ROUTE"         to efac go to titr2.
           if wfcvia = 2 move "PAR AVION"         to efac go to titr2.
           if wfcvia = 3 move "PAR BATEAU"        to efac go to titr2.
           if wfcvia = 4 move "LAISSER SUR PLACE" to efac go to titr2.
           if wfcvia = 5 move "PAR TRAIN"         to efac.
       titr2.
DDE153     if w-regroup not = '1'
              move wfcadl(3) to eliv
           end-if
           move wfcadf(3) to ecde.
           write ligne before 1.
           move spaces to ligne.
DDE153     if w-regroup not = '1'
              move "Transporteur : " to elib
              move wfcntr to efac
              move wfcadl(4) to eliv
           end-if
           move wfcadf(4) to ecde.
           write ligne before 1.
           move spaces to ligne.
DDE153     IF w-regroup not = '1'
              move "Port         : " to elib
DDE153        if FBFRA NOT = ZERO
DDE153           MOVE WPARP (FBFRA) TO efac
              else
                 MOVE "PORT AVANCE" TO  efac
              end-if
              move wfcadl(5) to eliv
           END-IF
           move wfcadf(5) to ecde.
DD0424     write ligne before 1
  -        move spaces to ligne
  -        if w-regroup not = '1'
  -           move wfcadl(6) to eliv
  -        end-if
DD0424     move wfcadf(6) to ecde.
           ADD 1 TO CP.
           MOVE CP TO EPAG.
      *----> M0397 (D)
      *    write ligne before 4.
DDE153*M280302 editer zone memo sinon edition cde suivante
      *    if fcnin = zero write ligne before 4
           if wnin = zero write ligne before 4
             else          write ligne before 1
                           move spaces to ligne
                           move "* RELIQUAT *" to eliv
                           write ligne before 3.
      *----> M0397 (F)
           move spaces to ligne.
           move zero to wcptr.
           if wctl not = zero move "                   REPORT" TO EDES
                   move wlde                                   to eliv
      *            add fbht1f fbht2f fbtx1f fbtx2f fbaff giving emht
                   move wcumul to emht
                   write ligne before 1
                   move spaces to ligne add 1 to wcptr go to ftitre.
           if wlde not = spaces move wlde to eliv
                                write ligne before 1
                                move spaces to ligne
                                add 1 to wcptr.
DD0221* edition reference commande client si non regroupee
DD0221     if w-regroup not = '1'
              string "      VOTRE REFERENCE COMMANDE : " wrf2
                  delimited size into ligne
              write ligne before 1
              add 1 to wcptr
              move spaces to ligne
           end-if
           .

       ftitre.
           exit.

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
      ******    conversion d'un montant francs francais en EURO
           multiply wzca by wpgbt62 giving wcale.
           add ar to wcale.
      *----> M1198 (F)
       FIN.
           move spaces to ligne
      *    write ligne before page.
           CLOSE ETAT.

GPICMT* si edition ARC on supprime l'etat qui a ete cree (en attendant de
GPICMT* reecrire la creation de la facture sans l'edition)
DD0362     if icglp-fact-arc not = spaces
              or icglp-fact-pdf = "O"
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
DD0350   if icglp-fact-direct not = 3
           perform cl-ffacture
           perform cl-fcommaap
           perform cl-fcommac4
           perform cl-intracom
         end-if
         .
       fini.
           exit program.

      *=========================================================================
      *                              FONCTIONS LOCALES
      *=========================================================================


      *DDE089 affichage fenetre d'erreur
       ERREUR section.
DD0351     perform env-mail
           move wnom-prog to immaf-vali-pgm
           move "V=Validation" to immaf-vali-act
           move "V" to wmmaf-vali-trt
           move "B" to immaf-vali-pos
DDE999     move wtrt to wmmaf-vali-trt.
           call 'mmaf-vali1' using mmaf-vali adl-art.

      * decrementation numero de facture
       subnum section.
           IF WPROCON = "CONDITIONNEL" SUBTRACT 1 FROM WNFACA
             ELSE                      SUBTRACT 1 FROM WNFANA.


      *---------------------
      * Ecriture de la trace
      *---------------------
       cre-trace section.
           move "C"   to immtr-trac-type
           move wnom-prog to immtr-trac-prog
           call 'mmtr-trac1' using mmtr-trac adl-art
           .
DDE153* ecriture entete commande si regroupee
       entete-cde section.
           if wcptr > 14
              PERFORM TITRE THRU FTITRE
           end-if
           write ligne before 1
           string '    Notre Reference : ' fccle-cdesup 
                 '  Representant : '
            fcrep '  Adresse de Livraison : ' wclliv '  ' wfcadl(1)
             delimited size into ligne
           write ligne before 1
           add 2 to wcptr
           move spaces to ligne
DD0221*    string '    Votre Reference : ' wrf2 '                 '
DD0221     string '    Votre Reference : ' wrf2 '          '
                  '                                  ' wfcadl(2)
             delimited size into ligne
           write ligne before 1
           add 1 to wcptr
           move spaces to ligne
           if wfcadl(3) not = space
           string '                      ' '        ' '              '
                  '                                     ' wfcadl(3)
             delimited size into ligne
           write ligne before 1
           add 1 to wcptr
           move spaces to ligne
           end-if
           if wfcadl(4) not = space
           string '                      ' '        ' '              '
                  '                                     ' wfcadl(4)
             delimited size into ligne
           write ligne before 1
           add 1 to wcptr
           move spaces to ligne
           end-if
           if wfcadl(5) not = space
           string '                      ' '        ' '              '
                  '                                     ' wfcadl(5)
             delimited size into ligne
           write ligne before 1
           add 1 to wcptr
           move spaces to ligne
           end-if
GPICMT* edition libelle pays
DD0424     if wfcadl(6) not = space
  -        string '                      ' '        ' '              '
  -               '                                     ' wfcadl(6)
  -          delimited size into ligne
  -        write ligne before 1
  -        add 1 to wcptr
  -        move spaces to ligne
DD0424     end-if
           .
DDE153* edition et creation pied de facture
       pied section.
      *
      * CALCUL NET MARCH. APRES REMISES
      * CALCUL BASES ET TAXES PARAFISCALES
      *

      *DDE045 edition texte pour client partenaire
           if wtypfac = 'A'
              if wcptr > 16
                 perform titre thru ftitre
              end-if

              move "                 Le Prix Net facture tient compte de
      -    " la remise commerciale appliquee par famille" to ligne
              write ligne before 1
              move "                 et de la remise additionnelle PARTE
      -    "NAIRE" to ligne
              write ligne before 1
              add 2 to wcptr
              move spaces to ligne
           END-IF.

DD0351* edition reference du contrat groupment LCL                     
DD0351     if wclrdi = "BBJ"                            
              if wcptr > 16
                 perform titre thru ftitre
              end-if
              move "Ref. Condition : 2012-04953-45" to ligne
              write ligne before 1
              move spaces to ligne
              write ligne before 1
              add 2 to wcptr
           END-IF
DD0351* edition commentaire hausse de tarif pour client Pro France
DD0351*    if wfamstat(1:1) = 'P' and (wpays = 'FR')
      *       if wcptr > 16
      *          perform titre thru ftitre
      *       end-if
      *       move "Hausse de prix modulee suivant matieres de 3% a 7% a
      *    " compter du 02/01/2012." to ligne
      *       write ligne before 1
      *       move spaces to ligne
      *       write ligne before 1
      *       add 2 to wcptr
      *    END-IF

DD0358* ajout infos banque (IBAN/BIC)
DD0358     perform cpt-banque
  -        if wcptr-banque not = zero
      * calcul nombre de lignes possible avant pied de facture
  -           compute wnbl =
  -                 42 - linage-counter of etat
              if wcptr-banque > wnbl
  -              perform titre thru ftitre
  -           end-if
  -           perform infos-banque thru infos-banque-fin
DD0358     end-if

      *----> M0297 D
      *    IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T220.
           IF FBTAX NOT = 1 AND FBTAX NOT = 2 and fbtax not = 5
DD0448*                     and fbtax not = 6 GO TO T220.
DD0448                                        GO TO T220.
      *----> M0297 F
DDE153     IF WPAR1 = ZERO AND  WPAR2 = ZERO GO TO pied-tva.
           MOVE ZERO TO WHT1.
           IF WPAR1 = 1 MOVE FBHT1F TO FBBF1F
                        ADD FBBF1F TO WHT1.
           IF WPAR2 = 1 MOVE FBHT2F TO FBBF2F
                        ADD FBBF2F TO WHT1.
      *----> M0297 D
      *    IF FBTAX = 1 MULTIPLY WTAUPA1 BY WHT1
      *      ELSE       MULTIPLY WTAUPA2 BY WHT1.
      *    DIVIDE 100 INTO WHT1 ROUNDED.
      *    IF FBTAX = 1 MULTIPLY WTAUPA1 BY FBBF1F GIVING WCAL
      *      ELSE       MULTIPLY WTAUPA2 BY FBBF1F GIVING WCAL.
           IF FBTAX = 1 or fbtax = 5 MULTIPLY WTAUPA1 BY WHT1
             else                    MULTIPLY WTAUPA2 BY WHT1.
           DIVIDE 100 INTO WHT1 ROUNDED.
           IF FBTAX = 1 or fbtax = 5
                        MULTIPLY WTAUPA1 BY FBBF1F GIVING WCAL
             else       MULTIPLY WTAUPA2 BY FBBF1F GIVING WCAL.
      *----> M0297 F
           DIVIDE 100 INTO WCAL   ROUNDED.
           MOVE WCAL TO FBTP1F.
           SUBTRACT FBTP1F FROM WHT1 GIVING FBTP2F.
           ADD FBTP1F TO FBHT1F.
           ADD FBTP2F TO FBHT2F.
      *
      * CALCUL TVA
      *
       pied-tva.
           IF WTV1 = ZERO GO TO T215A.
           IF WTV1 = 9 MOVE 5    TO I
             ELSE           MOVE WTV1  TO I.
      *----> M0297 D
      *    IF FBTAX = 1 MULTIPLY FBHT1F BY WTVA1 (I) GIVING WCAL
      *      ELSE       MULTIPLY FBHT1F BY WTVA2 (I) GIVING WCAL.
           IF FBTAX = 1 or fbtax = 5
                        MULTIPLY FBHT1F BY WTVA1 (I) GIVING WCAL
             else       MULTIPLY FBHT1F BY WTVA2 (I) GIVING WCAL.
      *----> M0297 F
           DIVIDE 100 INTO WCAL   ROUNDED.
           MOVE WCAL TO FBTX1F.
       T215A.
           IF WTV2 = ZERO GO TO T220.
           IF WTV2 = 9 MOVE 5    TO I
             ELSE           MOVE WTV2  TO I.
      *----> M0297 D
      *    IF FBTAX = 1 MULTIPLY FBHT2F BY WTVA1 (I) GIVING WCAL
      *      ELSE       MULTIPLY FBHT2F BY WTVA2 (I) GIVING WCAL.
           IF FBTAX = 1 or fbtax = 5
                        MULTIPLY FBHT2F BY WTVA1 (I) GIVING WCAL
             else       MULTIPLY FBHT2F BY WTVA2 (I) GIVING WCAL.
      *----> M0297 F
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
DD9999*    IF FCDAE = ZERO GO TO T300.
  "   *    MOVE FCDAE TO FBDE1.
  "        IF w-fcdae = ZERO GO TO T300.
DD9999     MOVE w-fcdae TO FBDE1.
           MOVE 9 TO FBNEC.
           MOVE FBNPF TO FBE1F.
           GO TO T330.
      *
      * ECHEANCE CALCULEE *
      *
       T300.
DD0400     move FBDAF to wmmca-eche-date-ori9
  -        move wcon to wmmca-eche-con
  -        move fbreg to wmmca-eche-regl
  -        move wcec to wmmca-eche-decal
DD0438     call 'mmca-eche1' using mmca-eche mmaf-vrep adl-art
  -        if ommca-eche-rtn not = cmmdt-envi-rtn-ok
             if mmdt-langue = "FR"
  -           string 'Facture ' FBCLE-CDESUP ' erreur calcul echeance '
  -                       ommca-eche-liberr
  -                  delimited size into immaf-vali-tit
             else
  -           string 'Invoic ' FBCLE-CDESUP
                     ' error counting expiry date '
  -                       ommca-eche-liberr
  -                  delimited size into immaf-vali-tit
             end-if
  -           move '3' to ocglp-fact-rtn
  -           perform erreur
  -           go to fin1
  -        end-if
GPICMT* si l'echeance n'est pas calculee le chargement de l'echeance se fera au moment
GPICMT* de l'ecriture du pieds
  -        if ommca-eche-calculee = spaces
  -           go to t330
  -        end-if
  -        MOVE WCON TO FBCJL
DD0400     MOVE ommca-eche-date-eche9 TO FBDE1.

           MOVE FBNPF TO FBE1F.
           MOVE 1 TO FBNEC.
       t330.
           IF WTEST4 NOT = 1 GO TO T360.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 2.
           add 7 to wcptr.
      *----> M1198 (D)
      *    if wcptr > 18 perform titre thru ftitre
           if wcptr > 18 subtract 5 from wcptr
                         perform titre thru ftitre
      *----> M1198 (F)
             else subtract 5 from wcptr.
           MOVE "ECHEANCES FRACTIONNEES :" TO edes.
           WRITE LIGNE BEFORE 2.
           MOVE SPACE TO LIGNE.
      *----> M1198 (D)
           add 2 to wcptr.
      *----> M1198 (F)
           MOVE FBD1J TO LJE.
           MOVE FBD1M TO LME.
           MOVE FBD1A TO LAE.
           MOVE "/" TO LSL1 LSL2.
           MOVE FBE1F TO LMTECH.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           add 1 to wcptr.
           MOVE FBD2J TO LJE.
           MOVE FBD2M TO LME.
           MOVE FBD2A TO LAE.
           MOVE "/" TO LSL1 LSL2.
           MOVE FBE2F TO LMTECH.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           add 1 to wcptr.
           IF FBDE3 = ZERO GO TO T360.
           MOVE FBD3J TO LJE.
           MOVE FBD3M TO LME.
           MOVE FBD3A TO LAE.
           MOVE "/" TO LSL1 LSL2.
           MOVE FBE3F TO LMTECH.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
      *----> M0497a (D)
           add 1 to wcptr.
      *----> M0497a (F)
      *
      * EDITION PIED DE FACTURE *
      *
       T360.
           SUBTRACT     wcptr      FROM CINQ GIVING wcptr.
           move spaces to ligne.
           write ligne before wcptr.
           move zero to wedreg.
      *----> M0297 D
      *    IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO t400.
           IF FBTAX NOT = 1 AND FBTAX NOT = 2 and fbtax not = 5
DD0448*                     and fbtax not = 6 GO TO t400.
DD0448                                        GO TO t400.
      *----> M0297 F
           if wtv1 = zero go to t360a.
           move wtv1 to ectva.
      *----> M0297 D
      *    IF FBTAX = 1 MOVE WTVA1 (WTV1) TO ettva FBTT1
      *      ELSE       MOVE WTVA2 (WTV1) TO ettva FBTT1.
           IF FBTAX = 1 or fbtax = 5 MOVE WTVA1 (WTV1) TO ettva FBTT1
             else                    MOVE WTVA2 (WTV1) TO ettva FBTT1.
      *----> M0297 F
           MOVE FBTX1F TO emtva.
           move fbht1f to etht.
           perform t370.
       t360a.
           if wtv2 = zero go to t400.
      *----> M0297 D
      *    IF FBTAX = 1 MOVE WTVA1 (WTV2) TO ettva FBTT2
      *      ELSE       MOVE WTVA2 (WTV2) TO ettva FBTT2.
           IF FBTAX = 1 or fbtax = 5 MOVE WTVA1 (WTV2) TO ettva FBTT2
             else                    MOVE WTVA2 (WTV2) TO ettva FBTT2.
      *----> M0297 F
           MOVE FBTX2F TO emtva.
           move wtv2   to ectva.
           move fbht2f to etht.
           if wedreg = zero perform t370 go to t400.
           if wedreg = 1    perform t390.
           go to t400.
      *
      * edition echeance *
      *
       T370.
           if fbde1 not = zero MOVE fbde1 TO edate
                               move wdate to edateb.
           add fbht1f fbht2f fbaff giving etoht.
           add fbtx1f fbtx2f giving etotva.
           add 1 to wedreg.
           move 108      to ecnurp.
           move fbreg    to emod.
           write ligne before 1.
           move spaces to ligne.
      *----> M0497a (D)
           add 1 to wcptr.
      *----> M0497a (F)
      *
      * EDITION MODE DE REGLEMENT *
      *
       T390.
           MOVE "REGLTCLI" TO PGHCL1.
           MOVE  FBREG     TO PGHCL2.
DDE069     perform rnl-paramgpi.
           if file-status = zero MOVE PGHLIB     TO ereglx.
           move fbnpf to enet.
DDE153*    IF FCFOA = 5 OR FCFOA = 6 OR FCFOA = 7
           IF FBCFA = 5 OR FBCFA = 6 OR FBCFA = 7
                      MOVE "-" to esign.
           write ligne before 1.
           move spaces to ligne.
      *----> M0497a (D)
           add 1 to wcptr.
      *----> M0497a (F)
           add 1 to wedreg.
       t400.
           if wedreg = zero perform t370.
           if wedreg = 1    perform t390.
           if wcon = zero and wlde = spaces go to t410.
GPICMT* edition conditions de reglement
DD0400     move spaces to wlcon
  -        if FBLE = zero
  -           string fbjou ' Jrs Net'
  -               delimited size into wlcon
  -        else
  -           if FBLE = 30
  -              string fbjou " J. FIN MOIS" 
  -               delimited size into wlcon
  -           else
  -              string fbjou " J. FIN MOIS LE " FBLE    
  -               delimited size into wlcon
  -           end-if
DD0400     end-if
           move wlcon to ereglx.
       t410.
           move wlde to enettx.
      *----> M1198 (D)
           if fcdev not = 00 and fcdev not = 50 and fcdev not = 62
                                go to t410-a.
      * DDE089 on edite la valeur en franc suivant code wclcvfrf
      * avant nlle fiche client le code tester etair '0' maintenant 'N'
      *le 250302 suppression edition contre valeur
      *    if wclcvfrf = "0" or = "N"   go to t410-a.
           go to t410-a.

           write ligne before 1.
           move spaces to ligne.
           move fbnpf to wzca.
           if fcdev = 00 or fcdev = 50
                          divide wzca by wpgbt62 giving wcale
                          move "        EURO" to ecumx
             else         multiply wzca by wpgbt62 giving wcale
                          move "        F.FR" to ecumx.
           add ar to wcale.
           move wcale to enet.
           IF WFACAVO = "   AVOIR" AND FBNPF > ZERO MOVE "-" TO esign.
           IF WFACAVO = "  FACTURE" AND FBNPF < ZERO MOVE "-"
                                                             TO esign.
       t410-a.
      *----> M1198 (F)
           write ligne before page.
           move spaces to ligne.
           MOVE SPACE TO LIGNE.
           IF WORIDUP = "*DUPLICATA*" GO TO pied-f.
           IF WPROCON = " PRO-FORMAT" GO TO T480.
      *
      **** CUMUL FIN DE FACTURE ****
      *
           IF WFACAVO = "   AVOIR" ADD FBNPF TO WTOTAV
             ELSE                  ADD FBNPF TO WTOTFA.
      *
      * TEST DEVISE *
      *
       T480.
ELGU17*    IF WTEST1 = 1 GO TO pied-f.
      *----> M1198 (D)
      *    IF FBDEV = ZERO GO TO T500.

      *DDE089 si devise = devise base comptable pas de conversion des   *GPICMT
      *                   zones montants du pieds de facture            *GPICMT
      *    IF FBDEV = ZERO or fbdev = 50 GO TO T500.
           if ommcp-devb-code = wmmpa-devi-cdev go to t500.
           move wpgbteu to wrep3.
      *----> M1198 (F)
       T480A.
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
                 go to t490e-1
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
       T490.
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
                           GO TO T490A.
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
       T490A.
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
           IF FBBF1D = ZERO AND FBBF2D = ZERO GO TO T490B.
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
       T490B.
           IF FBNEC = ZERO GO TO T490E.
           IF FBNEC NOT = 1 GO TO T490C.
           IF FBE1D = FBNPD MOVE FBNPF TO FBE1F ELSE
      *----> M1198 (D)
      *                    MULTIPLY WREP3 BY FBE1D GIVING FBE1F.
                           move fbe1d to wzca
                           perform eurb
                           move wcale to fbe1f.
      *----> M1198 (F)
           GO TO T490E.
       T490C.
           IF FBNEC NOT = 2 GO TO T490D.
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
           GO TO T490E.
       T490D.
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
       T490E.
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
           if ommcp-devb-cod9 = 62 go to t500.

       t490e-1.
           ADD FBHT1F FBHT2F GIVING WHTD.
           move fbnpf to wfbnpf.
           MOVE FBNPF to wzca.
           perform eurc.
           move wcale to fbnpf.
           move fbaff to wzca. perform eurc. move wcale to fbaff.
           IF wfbnpf = WHTD MOVE FBNPF to whtdf
                            GO TO T490F.
           move fbtx1f to wzca. perform eurc. move wcale to fbtx1f.
           move fbtx2f to wzca. perform eurc. move wcale to fbtx2f.
           ADD FBTX1F FBTX2F GIVING WTXF.
           IF FBFRA NOT = 2 ADD FBAFF TO WTXF.
           SUBTRACT WTXF FROM FBNPF GIVING WHTDF.
       T490F.
           if fbht2f = zero move whtdf to fbht1f
             else           move fbht1f to wzca
                            perform eurc
                            move wcale to fbht1f
                            SUBTRACT FBHT1F FROM WHTDF GIVING FBHT2F.
           IF WHTD = fbnmf  MOVE WHTDF TO FBNMF ELSE
                            move fbnmf to wzca perform eurc
                            move wcale to fbnmf.
           IF FBBF1F = ZERO AND FBBF2F = ZERO GO TO T490G.
           ADD FBBF1F to FBBF2F.
           IF FBBF2F = WFBNPF MOVE FBNPF TO FBBF2F ELSE
                              move fbbf2f to wzca perform eurc
                              move wcale to fbbf2f.
           move fbbf1f to wzca. perform eurc. move wcale to fbbf1f.
           SUBTRACT FBBF1F FROM FBBF2F.
           SUBTRACT FBBF1F FROM FBHT1F GIVING FBTP1F.
           SUBTRACT FBBF2F FROM FBHT2F GIVING FBTP2F.
       T490G.
           IF FBNEC = ZERO GO TO T490Z.
           IF FBNEC NOT = 1 GO TO T490H.
           IF FBE1F = WFBNPF MOVE FBNPF TO FBE1F ELSE
                             move fbe1f to wzca perform eurc
                             move wcale to fbe1f.
           GO TO T490Z.
       T490H.
           IF FBNEC NOT = 2 GO TO T490I.
           ADD FBE1F to FBE2F.
           IF FBE2F = WFBNPF MOVE FBNPF TO FBE2F ELSE
                             move fbe2f to wzca perform eurc
                             move wcale to fbe2f.
           move fbe1f to wzca. perform eurc. move wcale to fbe1f.
           SUBTRACT FBE1F FROM FBE2F.
           GO TO T490Z.
       T490I.
           ADD FBE1F FBE2F to FBE3F.
           IF FBE3F = WFBNPF MOVE FBNPF TO FBE3F ELSE
                             move fbe3f to wzca perform eurc
                             move wcale to fbe3f.
           move fbe2f to wzca. perform eurc. move wcale to fbe2f.
           move fbe1f to wzca. perform eurc. move wcale to fbe1f.
           ADD FBE1F FBE2F GIVING WFAEF.
           SUBTRACT WFAEF FROM FBE3F.
       T490Z.
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
       T500.
           IF FBNEC NOT = ZERO GO TO T505.
           MOVE 1 TO FBNEC.
           MOVE FBNPF TO FBE1F.
           MOVE FBNPD TO FBE1D.
           IF WM = 2 MOVE 28 TO FBD1J
             ELSE    MOVE 30 TO FBD1J.
           MOVE WM TO FBD1M.
           MOVE WA TO FBD1A.
       T505.
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
           if icglp-fact-e1red = 'R'
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
                 string '*  ATTENTION !!! LA FACTURE : ' fbcle-cdesup
                        ' EST DIFFERENTE DE SON PIEDS DE FFACTURE'
                        delimited size into ligne
                 write ligne before 1
                 string '*      VOUS DEVEZ DECHIRER CETTE FACTURE'
                        delimited size into ligne
                 write ligne before page
                 go to pied-f
              end-if
           END-IF

           if icglp-fact-e1red not = 'R'
ELGU17        and wtest1 = zero
DD0362        and icglp-fact-arc = spaces   
              perform w-ffacture
              if file-status not = zero
DDE999           string 'Cde: ' wcle ' Ecriture facture impos.: '
                      fbcle-cdesup ' (' file-status ')'
                     delimited size into immaf-vali-tit
                 move '3' to ocglp-fact-rtn
                 perform erreur
                 go to fin1
              end-if

GPICMT* elgu le 18/10/09 controle existence paramcpt
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
GPICMT* GPIWARNING pour les factures avec code geo 0 on peut par exemple pour les tansporteur (marchandises abimees)
GPICMT*    faire une facture exoneree et utiliser le code 4 ==> prevoir gestion de ce cas
GPICMT*    gpi met un code taxe 3 au lieu de 4 et ca passe mais on ne devrait pas utiliser ce code 3 qui est fait
GPICMT*    pour de la suspension de taxe avec un montant ht affacte
              perform rnl-paramcpt
              if file-status not = zero
                if mmdt-langue = "FR"
DD0316           string 'Facture: ' fbcle-cdesup ' PARAMCPT Inexistant '
                        'modifier pieds de facture'
                     delimited size into immaf-vali-tit
                else
DD0316           string 'Invoic: ' fbcle-cdesup ' PARAMCPT not found '
                        'change feet of invoic'
                     delimited size into immaf-vali-tit
                end-if
                 perform erreur
              end-if
           END-IF.

       pied-f.
GPICMT* edition ARC 
DD0362     if icglp-fact-arc not = spaces
              or icglp-fact-pdf = "O"

DD0448*       if icglp-fact-e1red = 'R'
DD0448*          and w-flag-edit = 1
DD0448*          go to pied-f-edit
DD0448*       end-if
DD0448        if w-flag-edit = 1
  -              go to pied-f-edit
DD0448        end-if
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
              if icglp-fact-arc not = spaces
                 move cmmlp-hfac-type-arc to immlp-hfac-type
                 move fccle-cdesup to immlp-hfac-cdex
              end-if
              move fbcle-cdesup to immlp-hfac-hfac
DD0362        move wor-ffacture to fwor-ffacture2
              move 3 to immlp-hfac-direct
              call "mmlp-hfac1" using mmlp-hfac adl-art
              if ommlp-hfac-rtn not = cmmdt-envi-rtn-ok
                 move ommlp-hfac-liberr to immaf-vali-tit
                 move '3' to ocglp-fact-rtn
                 perform erreur
                 go to fin1
              end-if
           end-if
           .
       pied-f-edit.
           exit.

DDE153* initialisation zone facture
       ini-fac section.
DD0350     move 0 to w-flag-edit
           move spaces to w-regroup w-creat
           move zero   to cp wtest1.

GPICMT* en reedition on lit le pieds de facture
           if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
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
DD0394     move fcfeo      to w-type
           .

DDE153* initialisation des zones par cde a traiter
       ini-cde section.
           move zero to w-fbht1f w-fbht2f w-fbnmf.
           MOVE ZERO   TO WTEST4 WTEST5 WCPT7.
           MOVE ZERO   TO WMT4 (1) WMT4 (2).
           MOVE 0      TO WMT5 (1) WMT5 (2) WMT6 (1) WMT6 (2)
                          WMT8 WMT9 (1) WMT9 (2).
           MOVE zeroes TO WREMX.
           MOVE SPACE  TO WLIB7 (1) WLIB7 (2) WLIB7 (3) WLIB7 (4)
                          WLIB7 (5).
DDE153* cummul commande ds pied de facture
       cum-cde section.
DDE153* cumul poids/volume/qte et raz zones specifiques a une commande
           add fcnco to fbnco
           add fcpbr to fbpbr
           add fcqtl to fbqtl
           add w-fbht1f to fbht1f
           add w-fbht2f to fbht2f
           add w-fbnmf to fbnmf
           .

GPICMT* erreur coherence des codes
DD9999 erreur-codes section.
           string "CODES  Commande " fccle-cdesup 
                  " incoherents a verifier"
                    delimited size into immaf-vali-tit
           move '2' to ocglp-fact-rtn
           perform erreur
           .

GPICMT* mep adresse de facturation
V30002 adr-fact section.
           move 1 to i
           if ocmcd-gest-facturea-nom  not = spaces
              MOVE  ocmcd-gest-facturea-nom TO WFCADF(I) ADD 1 TO I.
           if ocmcd-gest-facturea-raison not = spaces
              MOVE ocmcd-gest-facturea-raison TO WFCADF(I) ADD 1 TO I.
           if ocmcd-gest-facturea-rue not = spaces
              MOVE ocmcd-gest-facturea-rue TO WFCADF(I) ADD 1 TO I.
           if ocmcd-gest-facturea-ville not = spaces
              MOVE ocmcd-gest-facturea-ville TO WFCADF(I) ADD 1 TO I.
           if ocmcd-gest-facturea-bureau not = spaces
              MOVE ocmcd-gest-facturea-bureau TO WFCADF(I) ADD 1 TO I.
DD0424     if ocmcd-gest-facturea-lpays not = spaces
  -             and ocmcd-gest-facturea-pays not = ommpa-soci-pays
DD0424        MOVE ocmcd-gest-facturea-lpays TO WFCADF(I) ADD 1 TO I.

GPICMT* mep adresse de facturation
V30002 adr-liv section.
           move 1 to i
           if ocmcd-gest-livrea-nom  not = spaces
              MOVE  ocmcd-gest-livrea-nom TO WFCADL(I) ADD 1 TO I.
           if ocmcd-gest-livrea-raison not = spaces
              MOVE ocmcd-gest-livrea-raison TO WFCADL(I) ADD 1 TO I.
           if ocmcd-gest-livrea-rue not = spaces
              MOVE ocmcd-gest-livrea-rue TO WFCADL(I) ADD 1 TO I.
           if ocmcd-gest-livrea-ville not = spaces
              MOVE ocmcd-gest-livrea-ville TO WFCADL(I) ADD 1 TO I.
           if ocmcd-gest-livrea-bureau not = spaces
              MOVE ocmcd-gest-livrea-bureau TO WFCADL(I) ADD 1 TO I.
DD0424     if ocmcd-gest-livrea-lpays not = spaces
  -             and ocmcd-gest-livrea-pays not = ommpa-soci-pays
DD0424        MOVE ocmcd-gest-livrea-lpays TO WFCADL(I) ADD 1 TO I.

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
            string '   ' ommpa-soci-dom1 ' IBAN : ' ommpa-soci-iban1
                                   ' SWIFT/BIC: ' ommpa-soci-bic1
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq2 not = spaces
            string '   ' ommpa-soci-dom2 ' IBAN : ' ommpa-soci-iban2
                                   ' SWIFT/BIC: ' ommpa-soci-bic2
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq3 not = spaces
            string '   ' ommpa-soci-dom3 ' IBAN : ' ommpa-soci-iban3
                                   ' SWIFT/BIC: ' ommpa-soci-bic3
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
            string '   ' ommpa-soci-dom-be ' IBAN : ' ommpa-soci-iban-be
                                   ' SWIFT/BIC: ' ommpa-soci-bic-be
                   delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         go to infos-banque-fin
         .
       infos-banque-DE.
         if ommpa-soci-banq-de = spaces
            perform infos-banque-standard
         else
            string '   ' ommpa-soci-dom-de ' IBAN : ' ommpa-soci-iban-de
                                   ' SWIFT/BIC: ' ommpa-soci-bic-de
                   delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         .

       infos-banque-fin.
         exit
         .

GPICMT* recherche nombre de ligne banque a editer

       cpt-banque section.
         move zero to wcptr-banque
         move "L"      to immpa-pays-fic
         move ocmcd-gest-reglepar-pays to wmmpa-pays-pays
         call 'mmpa-pays1' using mmpa-pays adl-art
         if ommpa-pays-cgeo = zero
            or ommpa-soci-banquex = spaces
            go to cpt-banque-fin
         end-if
         move zero to wcptr-banque
         if ocmcd-gest-reglepar-pays = "BE"
            go to cpt-banque-BE
         end-if
         if ocmcd-gest-reglepar-pays = "DE"
            go to cpt-banque-DE
         end-if
         .
       cpt-banque-standard.
         if ommpa-soci-banq1 not = spaces
            add 1 to wcptr-banque
         end-if
         if ommpa-soci-banq2 not = spaces
            add 1 to wcptr-banque
         end-if
         if ommpa-soci-banq3 not = spaces
          add 1 to wcptr-banque
         end-if
         .
       cpt-banque-standard-fin.
         go to cpt-banque-fin
            .
       cpt-banque-BE.
         if ommpa-soci-banq-be = spaces
            perform cpt-banque-standard
         else
            add 1 to wcptr-banque
         end-if
         go to cpt-banque-fin
            .
       cpt-banque-DE.
         if ommpa-soci-banq-de = spaces
            perform cpt-banque-standard
         else
            add 1 to wcptr-banque
         end-if
         go to cpt-banque-fin
         .
       cpt-banque-fin.
         exit
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
GPICMT        ","  lfccle-cdesup               
GPICMT        X'00'
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
         if ocmcd-gest-livrea-niveau not = 'T'
            move ocmcd-gest-livrea-pays to wmmpa-pays-pays
         else
            move ocmcd-gest-cdepar-pays to wmmpa-pays-pays
         end-if
         call 'mmpa-pays1' using mmpa-pays adl-art
         move ommpa-pays-cgeo to wgeo-livrea
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

GPICMT* GPIMAIL envoi erreur par mail
        env-mail section.
           move cmmlp-mail-type-oo to immlp-mail-type(1)
           move "EDITION DES FACTURES"
                   to immlp-mail-data(1)
           move cmmlp-mail-type-o to immlp-mail-type(2)
           move immaf-vali-tit to immlp-mail-data(2)
           move cmmlp-mail-type-o to immlp-mail-type(3)
           move immaf-vali-tit to immlp-mail-data(3)
           string "PROBLEME EDITION DES FACTURES"
                   delimited by size into immlp-mail-objet
           move cmmlp-mail-trt-notif to immlp-mail-trt
           move spaces to immlp-mail-destg
           move spaces to immlp-mail-groupe
DD0351*    move "elgu micn door" to immlp-mail-dest
DD0351     move "anes micn"  to immlp-mail-dest
           move wnom-prog to immlp-mail-pgm
           call 'mmlp-mail1' using mmlp-mail adl-art
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
           copy "../copy/pro-filieres".
           copy "../copy/pro-fartusac".
           copy "../copy/pro-trpntran".
DDE187     copy "../copy/pro-languear".
      *----> M0698 (D)
           copy "../copy/pro-fartusap".
      *----> M0698 (F)
DD0351     copy "../copy/pro-paramcpt".
