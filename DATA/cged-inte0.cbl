      *DD2001 02/08/16 anes fcoadcli devient fjodcli (bloc adresse jour)
      *DD0525 29/09/11 door Demarrage GERGONNE
      *DD0221 13/01/11 anes 
      *DD0424 18/03/09 micn suppression de la zone depot livre de la fiche client
      *DD0420 08/12/08 elgu changement fichier sequentiel + recherche prix sans infocom
      *DD0394 08/02/08 elgu passer un parametre pour conversion des quantites
      *                     + type de commande
      *DD0394 28/12/07 elgu passer le type de commande
      *DD0282 27/06/2005 anes remplacement des "valide" par "etat"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cged-inte0.
      *
      * elgu 20/06/2005 DD0275
      * GPICMT integration commande EDI dans commande jour
      * GPICMT cas reprise de commandes client
      *
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ETAT ASSIGN TO wlabel-etat
                       organization line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ETAT
                DATA RECORD LIGNE
                LINAGE IS 64  LINES AT TOP 2.
       01  LIGNE                  PIC X(121).
       01  L1.
           02 LNCL                PIC 9(6).
           02 FILLER              PIC X(4).
           02 LRCC                PIC X(8).
           02 FILLER              PIC X(4).
           02 LNUM                PIC 9(5) blank zero.
           02 FILLER              PIC X(6).
           02 LLJJ                PIC 99.
           02 LS1                 PIC X.
           02 LLMM                PIC 99.
           02 LS2                 PIC X.
           02 LLAA                PIC 99.
           02 FILLER              PIC X(4).
           02 LART                PIC X(13).
           02 FILLER              PIC XXX.
           02 LLIB                PIC X(60).
      *
       WORKING-STORAGE SECTION.
           copy "/usr/action/ADL/copy/wor-adl".
           copy "../copy/wor-gencoent".
           copy "../copy/wor-gencolig".
           copy "../copy/wor-errlig".
           copy "../copy/wor-errent".
           copy "../copy/wor-fcommaap".
           copy "../copy/wor-multidat".
           copy "../copy/wor-fartusap".
           copy "../copy/wor-fartusac".
           copy "../copy/wor-fclients".
DD2001*    copy "../copy/wor-fcoadcli-cdesup".
DD2001     copy "../copy/wor-fjoadcli-cdesup".
           copy "../copy/wor-fcomjoap".
           copy "../copy/wor-fcomjoc1".
           copy "../copy/wor-fcomjoc2".
           copy "../copy/wor-fcomjoc4".
           copy "../copy/wor-paramgpi".

           copy '../copy/wor-guextmst'.
           copy '../copy/wor-ttfacmst-cdesup'.
           copy '../copy/wor-filieres'.
           copy '../copy/wor-cliartsp'.
           copy '../copy/wor-fcomjoc3'.
GPICMT* fichier de correspondance avec numero commande origine
           copy '../copy/wor-cdesrepr'.


      * ajout copy parametres appel mmcd-majo1                          *GPICMT
           copy '../copy/mmcd-majo.com'.                                *GPICMT

      * ajout trt fichier fcomjoc5 pour traiter l'annulation par une
      * fonction (ts les fichiers doivent etre ouverts)
           copy '../copy/wor-fcomjoc5'.
      * commentaires du jour integres
           copy '../copy/wor-fcomjoc6'.
      * fichier des avoirs pour appel fct annulation de cde
           copy '../copy/wor-avoircli'.
      * commentaires issus de l'EDI
           copy '../copy/wor-gencocom'.
      * commentaires pour commandes en erreur
           copy '../copy/wor-errcom'.
      * recap des cdes crees par assistante
           copy '../copy/wor-seqcom1'.
      * ajout copy parametre recherche assistnate commerciale           *GPICMT
           copy '../copy/cgcd-assi.com'.                                *GPICMT

      * parametre recherche du numero de commande a creer               *GPICMT
           copy '../copy/cgcd-nume.com'.                                *GPICMT

      * parametre pour annulation d'une commande                        *GPICMT
           copy '../copy/cgcd-annu.com'.                                *GPICMT

           copy '../copy/mgca-arti.com'.                                *GPICMT
           copy '../copy/cgca-mtht.com'.                                *GPICMT
           copy '../copy/mmpa-devi.com'.                                *GPICMT
           copy '../copy/cgta-rech.com'.                                *GPICMT
           copy '../copy/cgre-arcl.com'.                                *GPICMT
           copy '../copy/mmca-date.com'.                                *GPICMT
           copy "../copy/mmti-date.com".                                *GPICMT
           copy "../copy/mmtr-trac.com".                                *GPICMT
           copy '../copy/mgre-mtfr.com'.                                *GPICMT
           copy '../copy/mmca-qtes.com'.                                *GPICMT
           copy '../copy/mmpa-etat.com'.                                *GPICMT
           copy '../copy/mmpa-sect.com'.                                *GPICMT
           copy '../copy/mmdt-lieu.com'.                                *GPICMT
           copy '../copy/cmcd-majc.com'.                                *GPICMT
           copy '../copy/cmcd-gest.com'.                                *GPICMT
           copy '../copy/fcomjoc8.com'.                                 *GPICMT
           copy  '../copy/mmaf-finp.com'.
           copy '../copy/cged-inte.com'.                                 *GPICMT
       01  wlabel-etat pic x(64) value space.
       01  var-name pic x(64).
       01  var-data pic x(64).
       01  TOUT.
         02 wcalr           pic 9(6)v9(4).
DD0420* memo numero client lu pour affectation de la commande
         02     wclient pic 9(6).
         02     wentree pic x(80).
DD0462* facture ou avoir
         02     w-foa   pic  x.
DD0462* controle pcb si unite de saisie et fichier #
         02     w-conv  pic X.
DD0462* type de commande
         02     w-type  pic X.
DD0462* code etat article
         02     w-valid pic x.
DD0462* clients sur numero interene ou code ean
         02     w-ean  pic x.
      * memo livraison sur plateforme
           02 wplateforme pic x.
      * memo adresse livraison pour maj entete
           02   wnom              pic x(26).
           02   wrso              pic x(26).
           02   wrue              pic x(26).
           02   wbpo              pic x(26).
           02   wccp              pic 9(5).
           02   wbdi              pic x(35).
           02   wpays             pic xx.

      * memo gencod article
           02 wrlixar  pic x(13).
      * libelle pour reference non trouvee ou supprimee
           02 wlis.
              03 wlis1 pic x(17).
              03 wlis2 pic x(13).
      * libelle pour controle validite
           02 vlib             pic x(24).
      * indice table commentaire
           02 wind             pic 9.
      * cle fichier commentaires
           02   gencocom-key comp PIC 9(8).
      * code entete commande cree
           02 wecree           pic 9.
      * code commande ok
           02 wok              pic 9.
      * code categorie client
           02 wrdi             pic xxx.
      * reference commande client
           02 wrcl.
DD0221*       03 filler        pic x(5).
DD0221*       03 wrefcli       pic x(8).
DD0221        03 filler        pic x(4).
DD0221        03 wrefcli       pic x(15).
      * enreg recap commandes par assistante commerciale
           02 wenrass.
      * nom assistante
             03 wassist        pic x(4).
             03 filler         pic x value "|".
      * reference commande du client
DD0221*      03 wrecli         pic x(8).
DD0221       03 wrecli         pic x(15).
             03 filler         pic x value "|".
      * numero de commande gpi
             03 wcdegpi        pic x(6).
             03 filler         pic x value "|".
      * numero de client gpi
             03 wclien         pic 9(6).
             03 filler         pic x value "|".
      * code categorie client (BBO/MST)
             03 wtclien        pic xxx.
             03 filler         pic x value "|".
      * etat de la commande N=normal, E=en erreur
             03 wtcde          pic x.
             03 filler         pic x value "|".
      * commentaire C=commentaires
             03 wtcom          pic x.

      * memo lecture des commentaire
           02 wleccom          pic x.
      ** RANG 1ER COMMENTAIRE DE LA CDE
           02 wrancom    comp  PIC 9(8).
      * code creation des commentaires ds fcomjoc6 ou commande en erreur
           02 wtrtcom          pic x.
      * code creation des commentaires ds fcomjoc6 ou erreur
           02 wcrecom          pic x.
      * memo code regroupement client
           02 wcodrgt          pic xxx.
      * cle  entete de cde pour test commentaires
           02 wcleent.
             03 weanentc       pic 9(13).
             03 weanent        pic 9(13).
DD0420*      03 wcdeent        pic x(8).
DD0420       03 wcdeent        pic x(15).

      * cle  commentaire de cde pour test commentaires
           02 wclecom.
             03 weancomc       pic 9(13).
             03 weancom        pic 9(13).
DD0420*      03 wcdecom        pic x(8).
DD0420       03 wcdecom        pic x(15).
      * numero tarif ligne
           02   WNTAL             PIC 999.
           02   wntalx redefines wntal.
            03  wnta1l            pic 9.
            03  wnta2l            pic 99.
      * coef calcul prix ligne
           02   WCOKCAL           PIC 99V9(5).
      * meno numero de tarif de la fiche client
           02 wcltaro.
              03 wcltaro1       pic 9.
              03 wcltaro2       pic 99.
      * code categorie pour classe 3
           02 wzli              pic x.
      * table qte par reference pour classe 3 et categorie "B"
           02 wtab3x.
              03 wart3x occurs 500.
                 04 wart3xx.
                   05 wart3     pic 9(7).
                 04 wqte3       pic 9(8)v99.
      * memo nbre de poste maxi de la table
           02 wind3             pic 999 value 500.
      * indice d'utilisation de la table.
           02 windt             pic 999.
           02 wtba              pic x.
      ***************** pour l'arrondi sur la 3eme decimale
           02 t4                pic 9(5) value 00050.
           02 ta4 redefines t4.
             03 ar              pic 9v9(4).
           02 zeuro             pic 9(5)v9(6).
           02 wcale             pic 9(6)v9(4).
           02 wzca              pic 9(6)v99.
           02 wtrcd.
              03 wtj pic 99.
              03 wtm pic 99.
      ** CLIENT LIVRE - FRANCO - FILIATION - GDE CLASSE
           02   WLIV              PIC 9(6).
           02   WFRA              PIC 9.
           02   WMFR              PIC 9(6).
           02   WNAF              PIC 9.
           02   WNOF              PIC 9(6).

           02   wnofn             pic 9(6).

           02   wclpa             pic 9(6).
           02   wtgcx.
             03 WTGC              PIC 9.
           02   WCTA              PIC 9.
      *
      *
           02   WNUM              PIC 9(7).
           02   wfoa              pic 9.
           02   WCD               PIC 9(6).
           02   WRCD REDEFINES WCD.
            03  WA                PIC 99.
            03  WM                PIC 99.
            03  WJ                PIC 99.
           02   WFIN              PIC 9.
           02   WLIG              PIC 99.
      *
           02   WGENC.
            03  FILLER            PIC XXX.
            03  WNUD              PIC 999.
            03  WCID              PIC 9(5).
            03  FILLER            PIC XX.
      *
      **  1 A 20= EDITION/ 21 A 40=EDITION+CREATION DANS FICHIER ERREURS
      ** 41 A 60= EDITION+CREATION DANS FICH.ERREURS JUSQU'A LA FIN
           02   WERR              PIC 99.
           02   WSTO              PIC S9(7)V99.
           02   WELT              PIC 9.
           02   I                 PIC 99.
      ** 1 SI TOUTES LES CDES -> FICH.ERREURS JUSQU'A LA FIN
           02   TTFIN             PIC 9.
      ** RANG EN COURS - ENTETE ET LIGNE
           02   gencoent-key comp PIC 9(8).
           02   gencolig-key comp PIC 9(8).
      ** RANG 1ERE LIGNE DE LA CDE
           02   WRAN1        comp PIC 9(8).
      ** 1 SI REFERENCEMENT
           02   WREF              PIC 9.
      ** 1 NO.CENTRALE INEXISTANT
           02   WINVF             PIC 9.
      ** LIBELLE ENTETE
           02   WLENT.
            03  WLA               PIC X(10).
            03  WLB               PIC X(20).

      ** LIBELLE REFERENCE COMMANDE DU CLIENT POUR BRICORAMA            *M0600a
           02   WLENTB.                                                 *M0600a
            03  WLAB              PIC X(20).                            *M0600a
            03  WLBB              pic x(10).                            *M0600a

      ** 1 SI LIGNE CDE SUIVANTE DEJA LUE
           02   WLEC              PIC 9.
      ** REF. EAN ARTICLE
           02   WART.
            03  FILLER            PIC X(6).
            03  WCIP              PIC 9(6).
            03  FILLER            PIC X.
      ** CLE INDEX SEC. / GENCOD
           02   WCLE.
            03  WCLEN             PIC 9(6).
      ** CODE TAXE
           02   WTAX1X.
            03  WTAX1             PIC 9.
           02   WTAX2X.
            03  WTAX2             PIC 9.
           02   WFTVP             PIC 99.
           02   WTVPX REDEFINES WFTVP.
            03  WTVP1             PIC 9.
            03  WTVP2             PIC 9.
      ** QTE / QTE PAR BOITE
           02   WQTC              PIC 9(6)V999.
           02   WQTE REDEFINES WQTC.
            03  WENT              PIC 9(6).
            03  WDEC              PIC 99.
            03  WDEF              PIC 9.
           02   WCAL              PIC 9(6)V999.
      ** LECT.CENTRALE
           02   WMCE              PIC 9.
      ** CODE FRANCO CALCULE
           02   WCFRA             PIC 9.
      ** RECHERCHE COEF.K - NO TARIF
           02   WINDI             PIC 9.
           02   WRESU1            PIC 9.
           02   WRESU2            PIC 99.
           02   WCOK              PIC 99V9(5).
           02   WNTA              PIC 999.
           02   wntax redefines wnta.
            03  wnta1             pic 9.
            03  wnta2             pic 99.
      ** COMMENTAIRE
           02   WLCOMM.
            03  WL1               PIC X(30).
            03  WL2               PIC X(15).
            03  WL3               PIC X(30).
      ** CLIENT LIVRE + REF.CDE CLIENT
           02   WLC.
            03  WGLI              PIC 9(13).
DD0420*     03  WRCC              PIC X(8).
DD0420      03  WRCC              PIC X(15).
           02   WGCD              PIC 9(13).
      ** QTE - MONT COMMANDE
           02   WQUAN             PIC 9(8)V99.
           02   WMONT             PIC 9(8)V99.
           02   WMONTV            PIC Z(7)9V,99.
           02   WRESU             PIC 9(8)V99.
           02   renhtcdev         PIC Z(7)9V,99.
      ** DEBUT AC
           02   WACCOJ.
            03  WFJNUM            PIC 9(5).
            03  WFJNIN            PIC 9.
            03  WFJNEL            PIC 99.
            03  WFJunix2          PIC 999.
            03  WFJDLI            PIC 9(6).
            03  WFJILD REDEFINES WFJDLI.
             04 WFJAA             PIC 99.
             04 WFJMM             PIC 99.
             04 WFJJJ             PIC 99.
            03  WFJGEO            PIC 9.
            03  WFJRGT            PIC x(8).
            03  wfjnarx.
              04 WFJNAR           PIC 9(7).
            03  wfjsrex.
              04 WFJSRE           PIC 99.

            03  WFJLIG            PIC 99.
      ** MAJORATION - NB BARRIERES
           02   WENDB             PIC 9.
           02   WMAJO             PIC 999V99.
      ***  AJOUT JANVIER  93 POUR CALCUL 0 OU 5 DES PU TARIF   ***
           02 T5 PIC 999  VALUE 010.
           02 TA5 REDEFINES T5.
              03 ARD PIC 9V99.
           02 WMAN   PIC 9(4)V99.
           02 YMAN   REDEFINES WMAN.
              03 YMAD PIC 9(5).
              03 YMAF PIC 9.
      *
      *** gde classe 3 : rang 1er art./ no.art./ nb ref./ cumul qte art.
           02    wmgc.
            03   wrangc  pic 9(4).
            03   wartgcx.
              04 wartgc  pic 9(7).
            03   wnbgc   pic 999.
            03   wqtgc   pic 9(8)v99.
           02    wcumqt  pic 9(8)v99.
      *
           02    WDISPN  PIC 9(7).
      *
           02    wunix2  pic 9(3).
           02    wunix4  pic 9(3).
      *M1296 -------------------------------------------------------------------
           02    wcllia  pic 9(6).
      *M1296 -------------------------------------------------------------------
      * client facture
           02    wclfacturea  pic 9(6).
      * client regle
           02    wclreglepar  pic 9(6).

           02    wclcde  pic 9(6).
DD9999* numero client commande par du bloc adresse (fcoadcli
           02    w-alcde pic 9(6).

           02 wcalf             pic s9(8)v99.
           02 ztdv              pic 99v9(6).
           02 ztlig             pic 99.
           02 wcli              pic 9(6).
           02 wia               pic 999.
           02 wlib-comp.
              03 w-libelles.
      *          04 w-lib occurs 15 pic x(30).
      *    02 wt-lib-comp redefines wlib-comp occurs 5 pic x(90).
                 04 w-lib occurs 54 pic x(30).
           02 wt-lib-comp redefines wlib-comp occurs 18 pic x(90).
           02 wclref            pic x.
           02 zrendli        pic 9(6).
           02 filler redefines zrendli.
             03 zaa          pic 99.
             03 zmm          pic 99.
             03 zjj          pic 99.
           02 td-nolig          pic 9(4).
      *----> Escompte du client.
           02 td-clesc           pic 99v99.
           02  wnom-prog                PIC X(10) value 'prcde060'.
           02 td-top-prix        pic x.
           02 wpht pic z(5)9v,99.
           02 wapht pic z(5)9v,99.
       PROCEDURE DIVISION.
       PREM SECTION.
       T10.
DD0394     accept wentree
           unstring wentree delimited by ',' into
                w-foa w-conv w-type w-valid w-ean
      * recuperation date du jour                                       *GPICMT
           move 'D' to immti-date-taj
           call 'mmti-date1' using mmti-date adl-art
           move wmmti-date-amj to wcd.

      *****OPEN INPUT ENTETE LIGCDE CLIE REFER ARTI COMM MDAT.
           move 'I' to gfkey.
           perform op-fcommaap.
           move 'I' to gfkey.
           perform op-multidat.
           move 'I' to gfkey.
           perform op-fartusap.
           move 'I' to gfkey.
           perform op-fclients.
      *****open i-o entete.
           move 'W' to gfkey.
           perform op-gencoent.

           move 'I' to gfkey.
           perform op-guextmst.
           move 'W' to gfkey.
           perform op-ttfacmst.
           move 'I' to gfkey.
           perform op-filieres.

           move 'I' to gfkey.
           perform op-cliartsp.
           move 'W' to gfkey.
           perform op-fcomjoc3.
      *****                                go to fin.
       deb.
      *****open i-o ligcde.
           move 'W' to gfkey.
           perform op-gencolig.
      *****                                go to fin.
       debdeb.
      *****OPEN I-O   PARG COMJ FCOA.
           move 'W' to gfkey.
DD2001*    perform op-fcoadcli.
DD2001     perform op-fjoadcli.
           move 'W' to gfkey.
           perform op-fcomjoap.
           move 'W' to gfkey.
           perform op-fcomjoc1.
           move 'W' to gfkey.
           perform op-fcomjoc2.
           move 'W' to gfkey.
           perform op-fcomjoc4.

      * fichier echeances
           move 'W' to gfkey.
           perform op-fcomjoc5.

      * fichier commentaire cde du jour
           move 'W' to gfkey.
           perform op-fcomjoc6.
      * fichier commentaire issu cde EDI
           move 'I' to gfkey.
           perform op-gencocom.
      * fichier commentaires des commandes en erreur
           move 'E' to gfkey.
           perform op-errcom.
      * fichier recap des commandes par assistante
           move 'O' to gfkey.
           perform op-seqcom1.
      * fichier correspondance commande
           move "W" to gfkey
           perform op-cdesrepr

           move 'W' to gfkey.
           perform op-paramgpi.
      *****OPEN EXTEND ERRENT ERRLIG.
           move 'E' to gfkey.
           perform op-errlig.
           move 'E' to gfkey.
           perform op-errent.
           string 'ADLPID' x'00' delimited by size
                  into var-name.
           move space to var-data.
           call 'genvcc' using var-name var-data.
           string 'cgedinte.'
                  var-data delimited by ' '
                                     into wlabel-etat.

      *    recup des variables d'environnement
           call 'mmdt-envi1' using adl-art.

      * init memo lecture des commentaires
           move spaces to wleccom.

           OPEN OUTPUT ETAT.
           MOVE ZERO TO WFIN WLEC TTFIN.
           MOVE  90  TO WLIG.
           MOVE   1  TO gencoent-key gencolig-key WRAN1.

      * init rang commentaires
           move 0    to gencocom-key.
      * init cle entete et cle commentaire et fin fichier commentaire
           move spaces to wcleent wclecom wleccom.


       T20.
           MOVE SPACES TO WTAX1X WTAX2X.
           MOVE  ZERO  TO WTGCX WREF WQUAN.

      * init code commande OK code creation entete et rang 1er commentaire
           move 0 to wok.
           move 0 to wecree wrancom.

           move zeroes to wcok wmfr wmgc.

           move zero  to td-clesc td-nolig.
           move spaces to wlib-comp.
           move 1     to wcok.

      *****READ ENTETE INVALID KEY GO TO FIN.
           perform r-gencoent.
           if file-status not = zero
                     GO TO FIN.

      * memo ean cde, livre et reference commande client
           move RENGCD to weanentc.
           move rengli to weanent.
           move renrcc to wcdeent.
           move spaces to wplateforme
      * init num cde
           move zero to wnum.

      * RENTYP : type de commande EDI ALLEGRO                           *GPICMT
      *                       23=LEROY (message gencod)                 *GPICMT
      *                      105=CASTORAMA (message eancom)             *GPICMT
      *                      220=BRICORAMA (message eancom              *GPICMT
           IF RENTYP NOT = "023" and rentyp not = "105"
                              and rentyp not = "220 "MOVE 21 TO WERR
                              GO TO ERR1.
GPICMT* recherche code interne client si code ean dan interface, sinon numclient donne
           if w-ean not = 'I'
              move rengli to fiean
              perform rnl-filieres
              IF file-status not = zero
                 MOVE 22 TO WERR
                 go to ERR1
              END-IF
              move fincl to wclcde wclient w-alcde
           else
              move rengli to wclcde wclient w-alcde
           end-if
           .
       T25.
           IF WFIN = 1         MOVE 41 TO WERR  GO TO ERR1.
           IF WLEC = 0         GO TO T30.
           IF RENGLI = RLIGLI AND RENRCC = RLIRCC  and
              rengcd = rligcd GO TO T30.
           MOVE 32 TO WERR.
           GO TO ERR1.
       T30.
      *M1296 -------------------------------------------------------------------
           move zero to wcllia.
      *M1296 -------------------------------------------------------------------

DD0420     MOVE wclient TO CLNCL.

      * lecture client livre pour recuperer adresse de livraison
           perform rnl-fclients.
           if file-status not = zero
                    MOVE 22 TO WERR  GO TO ERR1.

      * recherche marche du client a partir de son secteur
           move clsecteur to wmmpa-sect-sect
           move ' ' to immpa-sect-trt
           call 'mmpa-sect1' using mmpa-sect adl-art

      * recherche lieu edition a partir du code magasin expediteur client
           move "L" to immdt-lieu-trt
           move clmli to immdt-lieu-lieu
           call "mmdt-lieu1" using wmmdt-lieu adl-art
           if lili-corlieu = spaces
              if mmdt-societe = "GPI" or = "GERGONNE"
                 move "3" to lili-corlieu
              else
                 move "1" to lili-corlieu
              end-if
           end-if


           move clnom to wnom.
           move clrss to wrso.
           move clrue to wrue.
           move clvil to wbpo.
           move clpays to wpays
           if clpays = "FR"
              move clcop to wccp
              move clbud to wbdi
           else
              if CLADREXPOR not = spaces
                 move CLADREXPOR to wbdi
                 move zero to wccp
              else
                 move clcop to wccp
                 move clbud to wbdi
              end-if
DD0279     end-if

DD0420*    move renncl to wclcde.
DD0420     move wclient to wclcde w-alcde.
           if rengcd = rengli go to t31.
           if w-ean not = 'I'
              move rengcd to fiean
              perform rnl-filieres
              if file-status not = zero
                 move 33 to werr
                 go to err1
              end-if
           else
              move rengcd to fincl
           end-if
      * GPICMT renncl correspond au client interne de l'ean livre
      * GPICMT dans le cas d'une livraison sur plateforme il faut relire
      * GPICMT mettre l'adresse du commande par dans l'entete
      * GPICMT at affecte la commande au commande par
DD0420     move wclient to clncl
           perform rnl-fclients
           if file-status not = zero
              MOVE 22 TO WERR
              GO TO ERR1
           else
              if clniveau = "K" or = "T" or = "D"
                 move fincl to wclcde clncl
              end-if
              if clniveau = "K"
                 move fincl to clncl
                 perform rnl-fclients
                 if file-status not = zero
                    MOVE 22 TO WERR
                    GO TO ERR1
                 else
                    move clnom to wnom
                    move clrss to wrso
                    move clrue to wrue
                    move clvil to wbpo
                    move clcop to wccp
                    move clbud to wbdi
                    move clpays to wpays
                    if clpays = "FR"
                       move clcop to wccp
                       move clbud to wbdi
                    else
                       if CLADREXPOR not = spaces
                          move CLADREXPOR to wbdi
                          move zero to wccp
                       else
                          move clcop to wccp
                          move clbud to wbdi
                       end-if
                    end-if
                    move "K" to wplateforme
                 end-if
              end-if
           end-if

           move fincl to w-alcde
DD0420*    move renncl to wcllia.
DD0420     move wclient to wcllia.
       t31.

      *****READ CLIE INVALID KEY MOVE 22 TO WERR  GO TO ERR1.
           perform r-fclients.
           if file-status not = zero
                     MOVE 22 TO WERR  GO TO ERR1.
      * init coeficient
           move 1 to wcok.
      * memo numero de tarif du client
      * si tarif espace on signale que le client n'est pas codifie multiclasse
      * on impose le tarif 100 et on traite la cde en multiclasse
           if cltaro = spaces move 100 to wcltaro
                              move 36 to werr
                              perform err1 thru errf
             else             move cltaro to wcltaro.
           move wcltaro to wnta.

           if cltrif = 9 move clrek to wcok.
      *chargement de l'escompte
           move clesc to td-clesc.

      *GPICMT memo code greoupement pour gestion zones specifiques
           move clrdi to wrdi.

           if wcllia = zero move cllia to wcllia.

      * memo client facture et regle du client de la commande
           move clfaa to wclfacturea
           move clrep to wclreglepar

      *
      ** ATTRIBUTION NO.COMMANDE

           perform rechnum.
           if ocgcd-nume-rtn not = spaces display ocgcd-nume-err
                                          move 43 to werr go to err1.

      ** VERIF. DATE LIVRAISON - CODE SURV. CLIENT
          MOVE RENDLI TO CADATE.
          move rendli to zrendli.
          if rendli > zero move rendli to cadate go to t51.

      *calcul delai par fonction
          move 'O' to immca-date-ouv
          move 'A' to immca-date-trt
          move 'N' to immca-date-zer
          move wj  to immca-date-jjour
          move wm  to immca-date-jmois
          move wa  to immca-date-janne
          move 5   to immca-date-nbj
          call 'mmca-date1' using mmca-date adl-art
          if ommca-date-rtn not = '0'
             move 1 to werr
             perform err1 thru errf
             go to t52.

          move ommca-date-jjour to zjj
          move ommca-date-jmois to zmm
          move ommca-date-janne to zaa
          move zrendli to cadate.
       t51.

           if CADATA > 50 move 19 to CADATS
             else         move 20 to CADATS.
           IF CADATA > 50 AND WA < 51
                           MOVE 1 TO WERR  PERFORM ERR1 THRU ERRF
                            GO TO T52.
           IF CADATA < 51 AND WA > 50 GO TO suit2000.
           IF CADATE < WCD MOVE  1 TO WERR  PERFORM ERR1 THRU ERRF
                            GO TO T52.
       suit2000.
      *****READ MDAT INVALID KEY MOVE  2 TO WERR  PERFORM ERR1 THRU ERRF
           perform r-multidat.
           if file-status not = zero
                     MOVE  2 TO WERR  PERFORM ERR1 THRU ERRF
                                 GO TO T52.
           IF CAVAJO NOT = 1     MOVE  3 TO WERR  PERFORM ERR1 THRU ERRF
                                 GO TO T52.
       T52.
           IF CLCSU NOT = ZERO   MOVE  4 TO WERR PERFORM ERR1 THRU ERRF.

      * init tables qte classe 3
           move spaces to wtab3x.

      *
      ** CREATION AP COMMANDE
      * elgu le 06/05/03 initialisation fcomjoap car fcnfa non initialise
      *                  a zero ==> manque element lecture sur cle 4
      *    MOVE spaces    TO wor-fcomjoap WACCOJ.
      *    INITIALIZE wor-fcomjoap WACCOJ
           INITIALIZE apcoj WACCOJ

           MOVE ZERO TO FJGEO FJNIN FJFOA FJCTA FJFRA FJNBF FJNAF FJCCP
                        fjnumr fjninr.
           MOVE ALL "0" TO    FJGCO.
           MOVE SPACES  TO    FJDTR FJRCL FJPTT FJTAR FJACS FJNTR FJNOM
                  FJRSO FJRUE FJBPO FJBDI FJQUA FJCOP FJCQU FJCCO.
      *elgu le 22/10/02 on met EDI ds qui saisit permettra de reconnaitre
      *     les commandes passant par edi sans risque d'avoir un login AUTO
      *   move "AUTO" to fjqsa.
          move "EDI " to fjqsa.

      * on initialise le marche a GDPU si non parametre
           if ommpa-sect-marche not = spaces
              move ommpa-sect-marche to fjmarche
           else
              move 'GDPU' to fjmarche
           end-if

           MOVE ZERO TO WQUAN WMONT.


          if rendli > zero MOVE RENDLI TO FJDLI WFJDLI
            else           move zrendli to fjdli wfjdli.
           MOVE CLGEO  TO FJGEO WFJGEO.
           MOVE WNUM   TO FJNUM WFJNUM.
      *    move wtba   to fjtba.
           move wfoa   to fjfoa.
DD0394     move w-type  to fjfeo
           MOVE ZERO   TO FJNIN WFJNIN wunix2 wunix4.
           MOVE wclcde TO FJNCL.
      *on chagre la date du jour ds la date de reference tarif          *GPICMT
           move wcd    to fjdatetarif9
           MOVE WA     TO FJANN.
           MOVE WM     TO FJMOI.
           MOVE WJ     TO FJJOU.
           MOVE CLNRE  TO FJREP.
           MOVE "D"    TO FJDTR.
GPICMT* la zone renrcl contient le numero de commande origine, la ref cde client
GPICMT* a ete mise dans la zone renbtrans
      *    MOVE RENRCL TO FJRCL.
           string renrcd renrck renbtrans
                 delimited size into FJRCL.
           MOVE CLTRIF TO FJCTA WCTA.

      * chargt tarif fiche client
           move wcltaro to fjtaro.

           IF CLCFR NOT = ZERO  MOVE CLCFR TO FJFRA WFRA
                          ELSE  MOVE 1     TO FJFRA WFRA.
      * recherche montant franco du client
      *    MOVE CLMFR  TO WMFR.
           move zero to wmfr
           move clncl to imgre-mtfr-ncl
           move wcltaro1 to imgre-mtfr-sufa
           call 'mgre-mtfr1' using mgre-mtfr adl-art
           IF omgre-mtfr-rtn = '0'
              move omgre-mtfr-mfr to wmfr
           END-IF
           .

           MOVE 1      TO FJDLR.
      *    MOVE 0      TO FJDLE.
DD0424*    MOVE cldli  TO FJDLE.
DD0424     MOVE zero   TO FJDLE.
           MOVE CLCSU  TO FJCSC.
           MOVE 1      TO FJIEF.
           MOVE CLTRH  TO FJTHA.
      *anciennement zone sernam plus uitilisee
      *    MOVE CLACS  TO FJACS.
           MOVE spaces TO FJACS.
           MOVE RENGCD TO WGENC.
           MOVE WNUD   TO FJNUD.
           MOVE WCID   TO FJCID.
           IF RENRCC NUMERIC  MOVE RENRD TO FJRD.
           MOVE RENGLI TO WGENC.
           MOVE WNUD   TO FJZO1.
           MOVE WCID   TO FJZO2.
           MOVE CLNAF  TO FJNAF WNAF.
           MOVE CLNUF  TO FJNOF WNOF.

           move zero to wnofn.

           move clnum  to wclpa.

           move td-clesc to fjesc.

      * maj adresse client livre de l'entete depuis client livre
           MOVE wnom   TO FJNOM.
           MOVE wrso   TO FJRSO.
           MOVE wrue   TO FJRUE.
           MOVE wbpo   TO FJBPO.
           move wpays to fjpays
           if wpays = 'FR'
              MOVE wccp   TO FJCCP
              MOVE wbdi   TO FJBDI
           else
              move wbdi to FJBDI
              move zero to FJCCP
           end-if

           MOVE CLCQU  TO FJCQU.
           MOVE CLCCO  TO FJCCO.
           MOVE ZERO TO WREF.
       T54D.
      * lecture de la filiation du client afin de memoriser sa propre filiation
      * pour la recherche des libelles complementaires articles
           MOVE CLNUF TO CLNCL.
           MOVE ZERO TO WINVF.
      *****READ CLIE INVALID KEY MOVE 1 TO WINVF  GO TO T54F.
           perform r-fclients.
           if file-status not = zero
                     MOVE 1 TO WINVF  GO TO T54F.
           move clnuf to wnofn.

      * lecture client payeur pour prendre les conditions de reglement
           MOVE wclreglepar TO CLNCL.
           MOVE ZERO TO WINVF.
           perform r-fclients.
           if file-status not = zero
                     MOVE 1 TO WINVF  GO TO T54F.
           MOVE CLCRT TO FJREG.

DD0002* lecture client facture pour prendre les donnees de facturation
DD9999     MOVE wclfacturea TO CLNCL.
           MOVE ZERO TO WINVF.
           perform r-fclients.
           if file-status not = zero
                     MOVE 1 TO WINVF  GO TO T54F.
       T54F.
           EXIT.
       T55.
           move clregrfa to fjregrfa
           MOVE CLNBF TO FJNBF.
           MOVE CLCDD TO FJDEV PGBCOD.
           move cltaxe to fjdi2
           move 1 to ztdv.
           IF CLCDD = 0  GO TO T55F.
           MOVE "DEVISE00" TO  PGBRAC.
      *****READ PARG INVALID KEY GO TO T55F.
           perform rnl-paramgpi.
           if file-status not = zero
                     GO TO T55F.
           move pgbtcd to ztdv.
           IF PGBLNG = ZERO      GO TO T55F.
           IF PGBLNG = PGBCOD MOVE PGBLNG TO FJLNG  GO TO T55F.
           MOVE PGBLNG TO PGBCOD.
      *****READ PARG INVALID KEY GO TO T55F.
           perform rnl-paramgpi.
           if file-status not = zero
                     GO TO T55F.
           IF PGBLNG = PGBCOD MOVE PGBLNG TO FJLNG.
       T55F.
           EXIT.
       T60.
      * ajout maj codes lieux prod/expe/edition
           move lili-corlieu to fjlpr fjlli fjled
           move wplateforme to fjplateforme

           perform w-fcomjoap.
           if file-status not = zero
                    GO TO ERRAP.

      * GPICMT creation donnees specifique commande client
           if renlib1 not = spaces
              move fjcle to icmcd-majc-e1numcde
              move "C"   to icmcd-majc-e1action
              move ccmcd-gest-trt-jour to icmcd-majc-e1trt
              move ccmcd-majc-e1type-c8 to icmcd-majc-e1type
              move renlib1 to icmcd-majc-e2texte
              move 3 to icmcd-majc-direct
              call 'cmcd-majc1' using cmcd-majc adl-art
              if ocmcd-majc-rtn not = cmmdt-envi-rtn-ok
                 move 40 to werr
                 perform err1 thru errf
              end-if
           end-if


      * maj code entete de commande cree
           move 1 to wecree.

      *
      ** ELT 3 - LIBELLE ENTETE
      *****MOVE WACCOJ TO EC1COJ.
           move wfjnum to fjnum1.
           move wfjnin to fjnin1.
           MOVE  3     TO FJNEL1.
           move wfjdli to fjdli1.
           move wfjgeo to fjgeo1.
           move zeroes to fjzog.
           MOVE SPACES TO FJLI1 FJLI2 FJLI3 FJLI4.
           MOVE "CDE PAR :" TO WLA.
           MOVE  RENGCD     TO WLB.
           MOVE  WLENT      TO FJLI1.

      * memo reference commande BRICORAMA
           if renlib1 not = spaces
              string "COMMANDE " mmdt-societe
                 delimited size into wlab
              move renlib1              to wlbb
              move wlentb to fjli2
           end-if
      * GPICMT chargement contremarque
           if wrdi = "MST" and renlib1 not = spaces
              move renlib1 to wfcomjoc8
              if fcomjoc8-MST-ctmarque not = spaces
                 string "CM: " fcomjoc8-MST-ctmarque
                    delimited size into fjli2
                 if wok = zero
                    move 8 to wok
                 end-if
              else
                 move spaces to fjli2
              end-if
           end-if

      *****WRITE COMPL ENRCOJ.(en-tete=fcomjoc1)***
           perform w-fcomjoc1.
           IF file-status not = "00"
                          DISPLAY "** FICHIER en-tete JOUR PLEIN **"
                          " - STATUS = " file-status
                          MOVE 45 TO WERR  GO TO ERR1.
      *
      ** CREATION FCOADCLI (4 NOS CLIENTS) - VERIF. NOS EXISTANTS
       T70.
      *    MOVE spaces TO wor-fcoadcli.
           INITIALIZE     wor-fcoadcli2.
           MOVE WNUM   TO ALNUM.
           MOVE ZERO   TO ALNIN.
           MOVE SPACE  TO ALCOD.
           MOVE wclcde TO ALCDE ALLIV ALFAC ALREG.
GPICMT* on charge le commande par de fcoadcli avec le commande par EDI
GPICMT     MOVE w-alcde TO ALCDE
           if wcllia not = zero move wcllia to alliv.
           move wclfacturea to alfac
           move wclreglepar to alreg
           .
       T75.
           MOVE SPACE  TO ALADC.
      *****WRITE ENRADL INVALID KEY GO TO ERRNL.
DD2001*    perform w-fcoadcli.
DD2001     perform w-fjoadcli.
           if file-status not = zero
                    GO TO ERRNL.

GPICMT* controle bloc adresse origine et trouve
           if   (renlib2(7:6) not = spaces and not = alliv)
             or (renlib2(7:6) not = spaces and not = alfac)
             or (renlib2(13:6) not = spaces and not = alreg)
                 move 47 to werr
                 perform err1 thru errf
           end-if
           IF WINVF = 1  MOVE 6 TO WERR  PERFORM ERR1 THRU ERRF.
      *
      ** AC  ELT 4 - LIGNE 1
       T80.
           IF WLEC = 1  MOVE ZERO TO WLEC
                        MOVE gencolig-key TO WRAN1 GO TO T82.
      *****READ LIGCDE INVALID KEY MOVE 1 TO WFIN
           perform r-gencolig.
           if file-status not = zero
                     MOVE 1 TO WFIN    GO TO T200.
           IF RLIGLI = RENGLI AND RLIRCC = RENRCC  and
              rligcd = rengcd GO TO T82.
           MOVE 1 TO WLEC.
           GO TO T200.
       T82.

           string '01' rliart 
               delimited size into fakle1
           perform rnl-fartusac
           if file-status not = zero

      * pour article inexistant on cree une ligne de commande avec article
      * 9999999 00 et prix zero
                                  GO TO t82-f
           end-if

           move fanma1 to fanma.
           move fanar1x to fanarx.
           perform r-fartusap.

           if file-status not = zero                   GO TO t82-f.


DD0394   if w-valid not = spaces
DD0282     if faetat not = cmmpa-etat-valide and
DD0282               not = cmmpa-etat-finvie
DD0282        move faetat to wmmpa-etat-etat
              move ' '       to immpa-etat-trt
              call 'mmpa-etat1' using mmpa-etat adl-art
              string 'Ss-ref. ' ommpa-etat-libelle
                   delimited size into vlib
              move 24 to werr
              go to t82-g
           end-if
DD0394   end-if

           go to t82-z.
       t82-f.
           move 23 to werr.

       t82-g.

      * chargement libelle a saisir ou supprime
           if werr = 23 move "******* A SAISIR" to wlis1
             else       move "******* SUPPRIME" to wlis1.

           if werr = 37 move "***** REF DOUBLE" to wlis1.
           perform err1 thru errf.
           move 9999999 to fanar1x.
           move 00      to fansr1x.
           move 1 to fasufa faqpb.
      * chargement du gencod dans le libelle
           move rliart to wlis2.
           move wlis   to falia.
           move spaces to falis.
           move zero   to fapac faram faram facnu facip facle fava
                          favl fapcb faspc.

           move zero to fapan.

           move 2      to fatvp.
       t82-z.

      *  controle PCB de l'article avec celui de gencolig
       if rlipcb not = zero and not = faqpb
          move 38 to werr
          perform err1 thru errf
       end-if

         .
       T85.
           INITIALIZE  jwor-fcommac22.
           MOVE  ZERO  TO jfcgeo2 jfcnin2 jfcprx jfcest jfckle jfcmar
                          jfcetq.
           MOVE SPACES TO jfcdes jfcsrc jfcrac jfcge1 jfcge2 jfcuat.
           if rendli > zero MOVE RENDLI TO jfcdli2
              else           move zrendli to jfcdli2.
           move zero to jfcremp jfctgc.
           MOVE WFJGEO TO jfcgeo2.
           MOVE WNUM   TO jfcnum2.
           MOVE 4      TO jfcnel2 WFJNEL.
           move wunix2 to jfcunix2.
           add  1      to wunix2.
           MOVE FANAR1 TO jfcnar WFJNARX.
           MOVE FANSR1 TO jfcsre WFJSREX.

      * on initialise le code type de ligne a 2 pour eviter le recalcul de prix
      * par le coefficient de la commande puisqu'il ne veut plus rien dire sauf
      * si le coef est impose dans la fiche du client avec code tarif 9 pour
      * les articles classe 1 uniquement
      *   on met systematiquement le code type de ligne a 1 car 2 est   *GPICMT
      *   utilise pour les articles sur devis                           *GPICMT
      *   le code 1 etatit initialement utilise pour le calucul d'un    *GPICMT
      *   prix net par multiplication avec un coef                      *GPICMT
      *   le code fjtopx permet de connaitre l'origine du prix          *GPICMT
          MOVE 1      TO jfclig.
      * chrgt gde classe ds la ligne
           move fatgc to jfctgc.

           MOVE 5      TO       WFJLIG.
           move faram  to jfcrgt.
           MOVE FALIS  TO jfcsrc.
           MOVE FACNU  TO jfccuf.
           MOVE FACIP  TO jfccip.
           MOVE FACLE  TO jfckle.
           MOVE FAVA   TO jfcva.
           MOVE FAVL   TO jfcvl.
           MOVE FAPCB  TO jfcpcb
           MOVE FASPC  TO jfcspc.
           MOVE FATVP TO WFTVP.
           IF WTAX1X = SPACE MOVE WTVP2 TO WTAX1   GO TO T85B.
           IF WTVP2 = WTAX1                        GO TO T85B.
           IF WTAX2X = SPACE MOVE WTVP2 TO WTAX2   GO TO T85B.
           IF WTVP2 = WTAX2                        GO TO T85B.
           MOVE 25 TO WERR.

      * on signale l'erreur du code taxe et on continue
           perform err1 thru errf.

       T85B.

       T85F.
           if jfcrgt = spaces
                             MOVE FANSE  TO jfcrgt WFJRGT.
           MOVE FALIA  TO jfcdes.
           MOVE RLIQTC TO WQTC.

GPICMT* si les quantites passees dans l'interface sont dans la meme unite
GPICMT* que la commande on ne convertit pas les quantites
DD0394     if w-conv not = 'C'
  -           go to t85l
DD0394     end-if

           IF FAQPB = ZERO OR FAQPB = 1  GO TO T85L.
           move 2 to immca-qtes-cod
           move faqpb to immca-qtes-con
           move wqtc to immca-qtes-qte
           call 'mmca-qtes1' using wmmca-qtes adl-art
           if ommca-qtes-rtn = spaces
              move ommca-qtes-qtr to wqtc
              go to t85l
           end-if
      * si la conversion est impossible on met 1 en qte cdee
           MOVE 1 TO WQTC.
           MOVE 9 TO WERR.
           PERFORM ERR1 THRU ERRF.
       T85L.
      *    MOVE WQTC   TO FJQTC.
           MOVE WQTC   TO jfcqtc.
      *---*
      *----> recherche du prix a appliquer a l'article :
      *    > calcul des remises niveau ARTICLE, REGROUPEMENT, REFERENCEMENT.
      *    > Des qu'il est trouve : go to t15-4 ou t16 suivant le cas.

          move space to td-top-prix.
      * appel fonction recherche de prix
          perform rech-tarif.
          if werr = 7 perform err1 thru errf.
      *
       T86.
      * le tarif est charge par une fonction
      *    MOVE FAPAC TO FJPTH.
      *    if wcltaro = 100
      *       MOVE fapan TO FJPTH
      *    else
      *       MOVE fapac TO FJPTH.
      * zone type de prix (fjtopx)                                      *GPICMT
      * code fjtopx : espace = prix catalogue avec coef                 *GPICMT
      *               1 = prix saisi                                    *GPICMT
      *               2 = prix trouve ds CLIARTSP (articles speciaux)   *GPICMT
      *               3 = remise regroupement                           *GPICMT
      *               4 = prix trouve ds REFERCLI (referencement)       *GPICMT
      *               5 = tarif vrac (tarif 300)                        *GPICMT
      *               6 = remise trouve ds CLIARTSP (art. speciaux)     *GPICMT
      *               7 =  prix article sur devis                       *GPICMT
           move "5"    to jfctopx.
      *----> chargement des top-prix, taux de remise et
      *----> taux de majoration.
          move td-top-prix to jfctopx.
          move zero     to jfctrem.
          move zero     to jfctmaj.
      *----> chargement du numero de ligne.
          add 10 to td-nolig.
          move td-nolig to jfcnlg.
       T90.
           MOVE FAMES  TO jfcmes.
           MOVE FAPRI  TO jfcprx.
           if w-conv not = 'C'
              move rlipcb to jfcqpb
           else
              MOVE FAQPB  TO jfcqpb
           end-if
           MOVE FAPOU  TO jfcpdu.
           MOVE FADIM  TO jfcdim.
           move fadoi  to jfcdou.
           MOVE FATVP  TO jfctvp.
           ADD jfcqtc TO WQUAN.

       T100.
      *****WRITE COMPL ENRCOJ.(el 4 tl1)
           move "A" to jfctref.

      ***** RECHERCHE DU REF. CLIENT DANS LE REFERENCEMENT
      * remplacer par un appel de fonction
           move fjncl  to icgre-arcl-ncl
           move fanma1 to icgre-arcl-magasin
           move fanarx to icgre-arcl-ref
           move fansr1x to icgre-arcl-sref
           call 'cgre-arcl1' using cgre-arcl adl-art
           if ocgre-arcl-rtn = '0'
              move ocgre-arcl-rac to jfcre1
              move ocgre-arcl-mou to jfcre2
              move ocgre-arcl-gma to jfcmar
           else
              if ocgre-arcl-rtn = '2'
                 move 5 to werr
                 perform err1 thru errf
              end-if
          END-IF
           .


      *maj prix de base et % remise par la fonction recherche tarif
      *       cgca-rech1

          if jfcpht = zero move '9' to jfctopx.
GPICMT* controle tarif

           if jfcpht not = rlipht
              move jfcpht to wpht
              move rlipht to wapht
           end-if
           perform w-fcomjoc2.
           IF file-status not = "00"
                       DISPLAY "**FICHIER EL 4 TL 1 JOUR PLEIN**"
                       MOVE 45 TO WERR  GO TO ERR1.
      *         recherche si commentaire article
           perform prix-deb thru prix-fin.
       t119-a1.
      *------*
      *----> Enregistrement des libelles complementaires.
           if wlib-comp = spaces go to t119-b2.
           move 1 to wia.
           move 10 to ztlig.

       t119-a2.
      *------*
           if wia > 18 go to t119-b2.
           if wt-lib-comp (wia) = spaces go to t119-b1.
      *    move space         to wor-fcomjoc3
           INITIALIZE            wor-fcomjoc3
           move jfcnum2        to fjnum3.
           move jfcnin2        to fjnin3.
           move 04            to fjnel3.
           move jfcnar         to fjnar3.
           move jfcsre         to fjsre3.
           move ztlig         to fjlig3.
           move jfcunix2       to fjunix3.
           move wt-lib-comp (wia) to fjtyb.
           if fjlc1 not = space move "G" to fjcl1.
           if fjlc2 not = space move "G" to fjcl2.
           if fjlc3 not = space move "G" to fjcl3.
           move jfcnlg         to fjnlg3.
           move jfcdli2        to fjdli3.
           move jfcgeo2        to fjgeo3.
           move jfcrgt         to fjrgt3.
           perform w-fcomjoc3.
           if file-status = "22" go to t119-b2.
           IF file-status not = "00"
                       DISPLAY "**FICHIER FCOMJOC3  JOUR PLEIN**"
                       MOVE 45 TO WERR  GO TO ERR1.
           add 5 to ztlig.
       t119-b1.
           add 1 to wia.
           go to t119-a2.
       t119-b2.
           go to t150
           .
GPICMT* creation element port
       cre-port.
          INITIALIZE    wor-fcomjoc4
          move zero to fjzx fjqui fjqul fjpuh fjmon fjenc fjnli fjtvp4.
          if rendli > zero MOVE RENDLI TO FJDLI4
            else           move zrendli to fjdli4.
          MOVE WFJGEO TO FJGEO4.
          MOVE WNUM   TO FJNUM4.
          move zero   to fjnin4.
          MOVE 6      TO FJNEL4.
          move wunix4 to fjunix4.
          add  1      to wunix4.
          move 2      to fjtvp4.
          move rlipht to fjmon
          move "+"    to fjsig.
          move "PORT" to fjdop.
          move spaces to fj1lc
                         fj2lc.
          perform w-fcomjoc4.
          if file-status not = zero
                         DISPLAY "**FICHIER EL PORT PLEIN**"
                         MOVE 45 TO WERR  GO TO ERR1.
      *
      *
      ** LECTURE LIGNE SUIVANTE
       T150.

           ADD 1 TO gencolig-key.
           GO TO T80.
      *
      *
      *
      ** FIN DE COMMANDE - ELT 10 SI COMMENTAIRE
       T200.
      ** CREATION DE L'ESCOMPTE AUTOMATIQUE                             *GPICMT
          if td-clesc = 0 go to t201.
      *   MOVE spaces TO wor-fcomjoc4.
           INITIALIZE    wor-fcomjoc4
          move zero to fjzx fjqui fjqul fjpuh fjmon fjenc fjnli fjtvp4.
          if rendli > zero MOVE RENDLI TO FJDLI4
            else           move zrendli to fjdli4.
          MOVE WFJGEO TO FJGEO4.
          MOVE WNUM   TO FJNUM4.
          move zero   to fjnin4.
          MOVE 7      TO FJNEL4.
          move wunix4 to fjunix4.
          add  1      to wunix4.
          move 2      to fjtvp4.
          move td-clesc to fjqui.
          move zero   to fjmon
                         fjcgs
                         fjnur.
          move "-"    to fjsig.
          move "ESCOMPTE AUTOMATIQUE" to fjdop.
          move spaces to fj1lc
                         fj2lc.
          perform w-fcomjoc4.
          if file-status not = zero
                         DISPLAY "**FICHIER EL ESCOMPTE PLEIN**"
                         MOVE 45 TO WERR  GO TO ERR1.
       t201.

           go to t210.                                                  *M0600a

           IF RENLIB1 = SPACES  GO TO T207.
           MOVE RENLIB1 TO WLCOMM.
       T205.
      *    MOVE spaces TO wor-fcomjoc4.
           INITIALIZE     wor-fcomjoc4
      *    MOVE ZERO   TO FJGEO4 FJNIN4 FJCGS FJTVP.
           MOVE ZERO   TO FJGEO4 FJNIN4 FJCGS jfctvp.
           MOVE SPACES TO FJDOP FJ1LC FJ2LC FJSIG FJZ4 FJZ5 FJLIQ.
          if rendli > zero MOVE RENDLI TO FJDLI4
            else           move zrendli to fjdli4.
           MOVE WFJGEO TO FJGEO4.
           MOVE WNUM   TO FJNUM4.
           MOVE 10     TO FJNEL4.
           move wunix4 to fjunix4.
           add  1      to wunix4.
           MOVE WL1    TO FJDOP.
           MOVE WL2    TO FJ1LC.
           MOVE WL3    TO FJ2LC.
      *****WRITE COMPL ENRCOJ.
           perform w-fcomjoc4.
           IF file-status not = "00"
                          DISPLAY "**FICHIER EL 5 A 10 JOUR PLEIN**"
                          MOVE 45 TO WERR  GO TO ERR1.
       T205F.
           EXIT.
       T207.
           IF RENLIB2 = SPACES  GO TO T210.
           MOVE RENLIB2 TO WLCOMM.
           PERFORM T205 THRU T205F.
      *
      ** RECHERCHE COEF. K
       T210.
           move wcltaro1 to wtgcx.
           move spaces  to wzli.

      *M050401
       t211.

      * pas de recherche montant franco si tarif 9 (coef K ds fiche client)
      * ds tous les cas on recherche le coef avec gde classe = 1er caractere
      * du num tarif de la fiche client pour recuperer le montant franco
      * pour recuperer le montant franco on prend le nbre total de boites
      * de la commande
           move wquan to wcumqt.
       t215.
           MOVE 1 TO WCOKCAL.
       t222.
      ** MAJ PHT DANS LIGNES DE COMMANDE
      * calcul du montant ht ligne et total commande fait par une fonction
           MOVE ZERO TO WMONT.
           move 'J'  to icgca-mtht-jou
           move wnum to icgca-mtht-cde
           move zero to icgca-mtht-ind
           move 'M'  to icgca-mtht-trt
           call 'cgca-mtht1' using cgca-mtht adl-art
           if ocgca-mtht-rtn not = '0'
              if ocgca-mtht-rtn = '3'
                 move 29 to werr
                 go to err1
              else
                 move 16 to werr
                perform err1 thru errf
                 move ocgca-mtht-mont to wmont
                 move ocgca-mtht-tqte to wquan
              end-if
           else
              move ocgca-mtht-mont to wmont
              move ocgca-mtht-tqte to wquan
           end-if

      *  controle H.T. cde calcule et celui de gencoent
           if renhtcde not = zero and wmont not = renhtcde
              move 39 to werr
              perform err1 thru errf
           end-if

           .

      *
      ** MAJ ENTETE COMMANDE
       T245.
           move wnum to fjnum.
           move 0    to fjnin.
      *****READ COMJ PRIMARY.
           perform r-fcomjoap.
           if file-status not = zero
                     MOVE 27 TO WERR  GO TO ERR1.
           move wnta to fjtve.
      *GPICMT pour livraison plateforme on force le port a franco
           if fjplateforme = "K"
              move 2 to fjfra
              go to t260
           end-if
           IF FJFRA NOT < 2     GO TO T246.
           IF WMFR = ZERO       GO TO T260.

      * le montant franco pris ds la fiche client est dans la devise
      *       du client donc on ne fait plus de conversion
           IF wmont NOT < WMFR  MOVE 2 TO FJFRA.
           go to t260.
       t246.
           if fjfra = 2 or fjfra = 3 or fjfra = 5 go to t260.
           IF WMFR = ZERO       GO TO T260.
           if wmont not < wmfr go to t260.
           move 5 to fjfra.
       T260.
           MOVE WCOK  TO FJCOK.
           MOVE WMONT TO FJMHT.

      * mettre le code OK a jour juste avant la reecriture
           MOVE WQUAN TO FJTQU.
           MOVE 6 TO FJCOP.

           move wclcde to gsncl.
           perform rnl-guextmst.
           if file-status not = zero go to t270.
           INITIALIZE     wor-ttfacmst2
           move zero   to tsnfa tscpa tsgcp.
           move fjcle  to tscle.
           move gsrdi  to tsrdi.
           move gsco1 to tsco1.
           move gsco2 to tsco2.
           move gsco3 to tsco3.
           move gsco4 to tsco4.
           move gsco5 to tsco5.
           move wcd    to tsdin.
           if renrcd not = zero move renrcd to wtrcd
                                move wtj to tsdinj
                                move wtm to tsdinm.
           move rengcd to tscpa.
DD0420*    if rengcd = rengli move renncl to tsgcp
DD0420     if rengcd = rengli move wclient to tsgcp
                              go to t261.
           if w-ean not = 'I'
              move rengcd to fiean
              perform rnl-filieres
              if file-status not = zero
                 move 19 to werr
                 perform err1 thru errf
              else
                 move fincl to tsgcp
              end-if
           else
              move rengcd to tsgcp
           end-if
           .
       t261.
           perform w-ttfacmst.
           if file-status not = zero move 18 to werr
                          DISPLAY "** ECRITURE TTFACMST IMPOSSIBLE"
                          " - STATUS = " file-status
                          "  COMMANDE :  " fjcle
                                     perform err1 thru errf.
           move tsco5 to fjfdem.
       T270.
           if wcllia not = zero move wcllia to clncl
             else               move fjncl  to clncl.
           perform rnl-fclients.
           if file-status not = zero move "N" to clpubl.
           if clpubl = "O" move 6 to fjcop
             else          move 0 to fjcop.
      *****REWRITE ENRCOJ INVALID KEY MOVE 29 TO WERR  GO TO ERR1.

      ** MAJORATION
       T300.
      * remplacement trt majoration par appel de la fonction mmcd-majo1
      * mise des lignes suivantes en commentaire
      * appel mmcd-majo1 avec code gde classe wtgc (voir si changement multicla)
      * sauf pour dinac
           if mmdt-societe not = 'GPI' and not = 'GERGONNE'
              go to t350
           end-if
           move fjdev to wma-dev.
           move wmont to wma-mht.
           move zero  to wma-maj.
           move wtgc  to wma-tgc.
           call "mmcd-majo1" using wmmcd-majo adl-art.
           if wma-err not = spaces
                     MOVE 8 TO WERR  PERFORM ERR1 THRU ERRF
                                 GO TO T350.
           move wma-maj to wmajo.
           if wma-maj = zero go to t350.
       T330.
      *    MOVE spaces TO wor-fcomjoc4.
           INITIALIZE     wor-fcomjoc4
           MOVE  ZERO  TO FJGEO4 FJNIN4 FJCGS.
           MOVE SPACES TO FJDOP FJ1LC FJSIG FJZ4 FJ2LC FJZ5 FJLIQ.
           MOVE WFJDLI TO FJDLI4.
           MOVE WFJGEO TO FJGEO4.
           MOVE WFJNUM TO FJNUM4.
           MOVE  9     TO FJNEL4.
           move wunix4 to fjunix4.
           add  1      to wunix4.
           MOVE "H.T. INSUFFISANT : MAJORATION" TO FJDOP.
           MOVE WMAJO  TO FJMON.
           MOVE  2     TO FJTVP4.
      *****WRITE COMPL ENRCOJ.
           perform w-fcomjoc4.
           IF file-status not = zero DISPLAY "**FICH. J. 5 a 10 PLEIN**"
                      MOVE 45 TO WERR  GO TO ERR1.
      *
      ** COMMANDE SUIVANTE
       T350.
           ADD 1 TO gencoent-key.
           IF WFIN = 1  MOVE 9999 TO WRAN1
                   ELSE MOVE gencolig-key TO WRAN1.

      * recherche et creation des commentaires
           move "C" to wcrecom.
           perform trtcom.

      * maj code commande OK
           if wok = zero move 1 to fjok
           else          move wok to fjok.
           move alliv to fjlivrea

           perform rw-fcomjoap.
           if file-status not = zero
                    MOVE 29 TO WERR  GO TO ERR1.

      * creation dans recap par assistante
      * recherche de l'assistante commerciale
           move fjrep to icgcd-assi-rep.
           move "A"   to icgcd-assi-trt.
           call "cgcd-assi1" using cgcd-assi adl-art.
           move wcgcd-assi-ass to wassist.
           move fjrcl   to wrcl.
           move wrefcli to wrecli.
           move fjcle   to wcdegpi.
           move fjncl   to wclien.
           move wrdi    to wtclien.
      * si code ok = 1 ==> commande normale sinon en erreur
           if fjok = 1 move "N" to wtcde
             else     move "E" to wtcde.
           move wtrtcom to wtcom.
           move wenrass to wor-seqcom12.
           perform w-seqcom1.
           if file-status not = zero display
             "ECRIT. ANREG. ASSISTANTE INVALIDE, STATUS:  " file-status
                                     move 45 to werr go to err1.

GPICMT* creation dans fichier de correspondance
           initialize wor-cdesrepr
           move fjcle to cr-cle
           move renlib1 to cr-numorix
           move wrefcli to cr-refcde
           perform w-cdesrepr
           if file-status not = zero
              display 'Creation CDESREPR Impossible ' fjcle
                      ' status ' file-status
              move 45 to werr go to err1.
      *---------------------
      * Ecriture de la trace
      *---------------------
           move space to wmmtr-trac
           move "C"   to immtr-trac-type
           string wnum '0' delimited by size into immtr-trac-num
           move "C"   to immtr-trac-action
           move wnom-prog to immtr-trac-prog
           move fjrcl   to wrcl.
           string 'creation par edi: ' fjcle '  ref.client: '
                  wrefcli '  client n: ' fjncl
                      delimited by size into immtr-trac-commentaire
           call 'mmtr-trac1' using mmtr-trac adl-art
           GO TO T20.
      *

       prix-deb.
           move space to wlib-comp.
      *----> On ne passe dans ce traitement que pour les libelles
      *----> complementaires s'il y en a.

       prix-a10.
           move fjncl         to cscli wcli.
       prix-a11.
           move 01            to csnma.
           move jfcnar         to csnar.
           move jfcsre         to csnsr.
           move 00            to csnte.
           perform rnl-cliartsp.
           if file-status not = zero go to prix-a30.
           move cslia to w-lib (1).
           move 2 to wia.

       prix-a20.
           perform nnl-cliartsp.
           if file-status not = zero or
              csnma       not = 01 or
              csnar       not = jfcnar or
              csnsr       not = jfcsre or
              cscli       not = wcli
              go to prix-a30.
           move cslia to w-lib (wia).
           move cslib to w-lib (wia + 1).
           add 2 to wia.

      * blocage du nbre  pouvant etre pris a 53
           if wia > 53 go to prix-a30.

           go to prix-a20.
       prix-a30.
           exit.
       prix-b30.
           if wlib-comp not = spaces go to prix-fin.

      * si filiation a zero on va voir le client 999999
           if fjnof = zero go to prix-a40.
           move fjnof to cscli wcli.
           perform prix-a11 thru prix-a30.

           if wlib-comp not = spaces go to prix-fin.

      * si sous filiation a zero on va voir le client 999999
           if wnofn = zero go to prix-a40.
           move wnofn to cscli wcli.
           perform prix-a11 thru prix-a30.


      * recherche des commentaires sur client 999999 (comm pour tout client)
       prix-a40.
           if wlib-comp not = spaces go to prix-fin.
           move 999999        to cscli wcli.
           perform prix-a11 thru prix-a30.

       prix-fin.  exit.

      * attribution numero de commande
       rechnum section.
       rechnum1.
           move wclcde to icgcd-nume-ncl.
           move w-foa  to icgcd-nume-foa
           call "cgcd-nume1" using cgcd-nume adl-art.
           if ocgcd-nume-rtn = spaces
              move ocgcd-nume-ncd to wnum
              move ocgcd-nume-foa to wfoa
              move ocgcd-nume-su3 to wtba.

      * traitement des commentaires
       trtcom section.
       trtcom0.
      * init code commentaire crees et numero de ligne
           move spaces to wtrtcom.
      * init erreur et rang du 1er commentaire de la commande
           move zero to wrancom.

      * si wleccom = "F" plus de commentaires a lire
           if wleccom = "F" go to ftrtcom.

           move fjcle to fjcle6.
           move zero to fjnli6.
      * secteur
           move mmdt-secteur to fjsect6.
      * fammille
           move "C"          to fjfam6.
      * origine
           move "A"          to fjori6.
      * type client
           move "G"          to fjtcl6.
      * code edition
      *    move "G"          to fjted6.
      * code trt initialise a A pour eviter d'integrer n'importe quoi dans le
      * portefeuille
           move "D"          to fjtrt6.

       trtcom1.
      * cle entete = ==> creation commentaire
           if wcleent = wclecom go to trtcomc.

      * cle entete < ==> pas de commentaire on s'en va
           if wcleent < wclecom go to ftrtcom.

       trtcoma.
      * cle entete > ==> lecture commentaire suivant car encore pas lu pour
      * cette commande
           add 1 to gencocom-key.
           perform n-gencocom.
           if file-status not = zero move "F" to wleccom
                                     go to ftrtcom.
      * memo ds cle commentaire
      *    move rcolc to wclecom.
           move RCOGCD to weancomc
           move RCOGLI to weancom
           move RCORCC to wcdecom
           go to trtcom1.
      * creation commentaire dans commande du jour
       trtcomc.
      * maj 1er rang du commentaire
           if wrancom = zero move gencocom-key to wrancom.

      * on peut avoir 4 enregistrement a creer pour un commentaire
           if fjnli6 > 9975 display "TROP DE COMMENTAIRES, CDE:  "
                            display fjcle
                            go to fin.
           move 1 to wind.
       trtcomc-d.
      * creation ds cde jour ou erreur
           if wcrecom = "E" go to trtcomv.
           if rcocom(wind) = spaces go to trtcomc-s.
           add 5 to fjnli6.
           move rcocom(wind) to fjcom6.
           perform w-fcomjoc6.
           if file-status not = zero
             display "fjnli6 :  " fjnli6 move 45 to werr go to ftrtcom.
      * init code commentaire
           move "C" to wtrtcom.
      * init code OK a 9 pour controle commentaire en validation
           if wok = zero move 9 to wok.
       trtcomc-s.
           if wind < 4 add 1 to wind go to trtcomc-d.
      * commentaire suivant
           go to trtcoma.
       trtcomv.
           move wor-gencocom to wor-errcom.
           perform w-errcom.
           if file-status not = zero move 45 to werr go to ftrtcom.
           go to trtcoma.
       ftrtcom.
           exit.

      * annulation d'une commande
       annul section.
       annu1.
           move fjcle to icgcd-annu-ncdx.
      * code maj "2" ==> suppression sans maj des fichiers autre        *GPICMT
      * que cde jour                                                    *GPICMT
           move "2"          to icgcd-annu-maj.
           call "cgcd-annu1" using cgcd-annu adl-art.
           if ocgcd-annu-rtn = "3"
             display ocgcd-annu-err go to fin.

       erreur section.
       ERRAP.
           IF file-status = "24" DISPLAY "***FICHIER COMMANDE PLEIN***"
                      MOVE 45 TO WERR  GO TO ERR1.
           DISPLAY "***ERREUR CREATION COMMANDE***".
           MOVE 30 TO WERR.
           GO TO ERR1.
       ERRNL.
DD2001*    IF file-status = "24" DISPLAY "***FICHIER FCOADCLI PLEIN***"
DD2001     IF file-status = "24" DISPLAY "***FICHIER FJOADCLI PLEIN***"
                      MOVE 46 TO WERR  GO TO ERR1.
DD2001*    DISPLAY "***ERREUR CREATION FCOADCLI***".
DD2001     DISPLAY "***ERREUR CREATION FJOADCLI***".
           MOVE 31 TO WERR.
           GO TO ERR1.
      *
      **** EDITION DES ERREURS
       ERR1.

      * mise en erreur de la commande par code OK a 0
           move 8    to wok.

           IF WLIG > 60  PERFORM TIT.
GPICMT* si erreur sur etat client on edite le client clncl sinon on edite le client livre
           if werr = 34
              MOVE clncl TO LNCL
           else
              move wclient to lncl
           end-if

           MOVE RENRCC TO LRCC.
           MOVE RENJJ  TO LLJJ.
           MOVE RENMM  TO LLMM.
           MOVE RENAA  TO LLAA.
           MOVE "/"    TO LS1 LS2.

      * edition systematique du numero de commande
           MOVE WNUM   TO LNUM.

           IF WERR = 23 OR WERR = 24 OR WERR = 25 or werr = 37
                        or = 38
                                                  MOVE RLIXAR TO LART
                                                  GO TO ERR2.
           IF WERR > 20 GO TO ERR2.
           IF WERR = 7 OR WERR = 9  MOVE RLIXAR TO LART.
       ERR2.
           if werr = 36 move "MULTICLASSE NON AUTORISE" to llib
                        perform err3
                        go to errf.

           IF WERR = 1  MOVE "DATE LIVR.< DATE JOUR" TO LLIB GO TO ERR3.
           IF WERR = 2  MOVE "DATE LIVR. NON CREEE"  TO LLIB GO TO ERR3.
           IF WERR = 3  MOVE "DATE LIVR. NON OUVREE" TO LLIB GO TO ERR3.
           IF WERR = 4  MOVE "CODE SURV.CLIENT DIFFERENT DE 0" TO LLIB
                        GO TO ERR3.
           IF WERR = 5  MOVE "REFERENCEMENT INCORRECT (M+DOSSIER)"
                        TO LLIB  GO TO ERR3.
           IF WERR = 6  MOVE "NO.CENTRALE INEXISTANT (FAC/REG)" TO LLIB
                        GO TO ERR3.
           IF WERR = 7  MOVE "TARIF NON TROUVE" TO LLIB
                        GO TO ERR3.
           IF WERR = 8  MOVE "PARAMETRE MAJORATION INEXISTANT" TO LLIB
                        GO TO ERR3.
           IF WERR = 9  MOVE "QTE INCORRECTE /QTE BTE  CREE AVEC QTE 1"
                        TO LLIB  GO TO ERR3.
      * recalcul fin de commande en erreur ==> a verifier
          if werr = 16 move '-- COMMANDE A VERIFIER EN MISE A JOUR'
                       TO LLIB  GO TO ERR3.

           if werr = 18 MOVE "--ECRITURE TTFACMST IMPOSSIBLE" to llib
                        go to err3.
           IF WERR = 19 MOVE "--FILIERE COMMANDE PAR INEXISTANTE"
               TO LLIB  GO TO ERR3.

           IF WERR = 20 MOVE "-- EAN LIVRE DIFFERENT EAN CDE --"
               TO LLIB  GO TO ERR3.
           IF WERR = 21 MOVE "--TYPE DE COMMANDE ANORMAL         -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 22 MOVE "--CLIENT INEXISTANT                -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
      * on ne met plus les cde avec article inconnu dans le fichier des erreurs
      * car on cree une ligne article 9999999
           IF WERR = 23 MOVE "--ARTICLE NON TROUVE" to llib go to err3.

      * utilisation code 24 pour article supprime
      * l'article sera cree ds la cde avec code article 999999
           IF WERR = 24
              string "--ARTICLE SUPPRIME--     " vlib
              delimited by size into llib
              go to err3
           END-IF

      * on ne met plus les commandes avec taxe article invalid dans le fichier
      * des erreurs il faudra modifier la ligne
           IF WERR = 25 MOVE "--CODE TAXE/PARA-FISC. DIFFERENT" to llib
                                GO TO ERR3.

           IF WERR = 26 MOVE "--PARAMETRE TARIF NON TROUVE       -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 27 MOVE "--LECTURE COMMANDE INVALIDE        -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 28 MOVE "--MAJ LIGNES COMMANDE INVALIDE     -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 29 MOVE "--MAJ COMMANDE INVALIDE            -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 30 MOVE "--ERREUR CREATION COMMANDE         -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
DD2001*    IF WERR = 31 MOVE "--ERREUR CREATION FCOADCLI         -> CDE
DD2001     IF WERR = 31 MOVE "--ERREUR CREATION FJOADCLI         -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 32 MOVE "--ENTETE/LIGNE NON CORRESPONDANTS  -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 33 MOVE "--FILIERE COMMANDE PAR INEXISTANT  -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.

      * pour info erreur 36 uitilise voir tag ERR1

      * plusieurs ref pour un gencod
           IF WERR = 37
              string fanar1 " " fansr1  " EAN14: " fagean14
                            "   -PLUSIEURS REFERENCES"
                   delimited size into llib
              go to err3.

      * PCB non correspondant
           if werr = 38
              string "--PCB #  -GPI : " faqpb " Recu : " rlipcb
                     " -> CDE EN ERREUR"
              delimited by size into llib
              go to err3
           end-if
      * H.T. total cde different
           if werr = 39
              move wmont to wmontv
              move renhtcde to renhtcdev
              string "TOTAL H.T. CDE " wmontv "   TOTAL H.T. RECU "
              renhtcdev " NON CORRESPONDANT  -> CDE EN ERREUR"
              delimited by size into llib
              go to err3
           end-if

      * Creation donnes specifiques client invalide
           if werr = 40
              string ocmcd-majc-liberr " RTN " ocmcd-majc-rtn
                          delimited size into llib
              go to err3
           end-if

ELGU  *    MOVE 1 TO TTFIN.
           IF WERR = 41 MOVE "**ENTETE ET FIN DES LIGNES         -> CDE
      -    "ET SUITE EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 43 MOVE "**PARAM.COMMANDE INEXISTANT        -> CDE
      -    "ET SUITE EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 44 MOVE "**MAJ PARAM.COMMANDE INVALIDE      -> CDE
      -    "ET SUITE EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 45 MOVE "**FICHIER COMMANDE PLEIN           -> CDE
      -    "ET SUITE EN ERREUR" TO LLIB  GO TO ERR3.
DD2001*    IF WERR = 46 MOVE "**FICHIER FCOADCLI PLEIN           -> CDE
DD2001     IF WERR = 46 MOVE "**FICHIER FJOADCLI PLEIN           -> CDE
      -    "ET SUITE EN ERREUR" TO LLIB.

       ERR3.
           WRITE LIGNE BEFORE 1.
      * creation trace des erreurs
           if werr = 23 or 24 or 25 or 37       move RLIXAR to wrlixar
           else
             if werr = 1 or = 2 or = 3 move rendli to wrlixar
             else                      move spaces to wrlixar
             end-if
           end-if.

      *---------------------
      * Ecriture de la trace
      *---------------------
           move space to wmmtr-trac
           move "C"   to immtr-trac-type
           string wnum '0' delimited by size into immtr-trac-num
           move "E"   to immtr-trac-action
           move wnom-prog to immtr-trac-prog
           move fjrcl   to wrcl.
           move fjrcl   to wrcl.
           string 'commande edi,ref.client: '
                  wrefcli '  client n: ' fjncl '   ' llib '  ' wrlixar
                      delimited by size into immtr-trac-commentaire
      *    call 'mmtr-trac1' using mmtr-trac adl-art
           MOVE SPACES TO LIGNE.
           ADD 1 TO WLIG.
       ERR4.

      * pour les codes erreurs 23/25 article inconnu ou taxe incorrect
      *                        24 article supprime
      * on cree les lignes avec reference 999999 et on ne transfert plus la
      * commande dans le fichier des erreurs
      *le 08/09/00 elgu ajout code 24 pour article supprime idem 23/25
      * + erreur 38/39/40 non blocantes
           IF WERR NOT > 20  or werr = 23 or werr = 25 or werr = 24
                             or werr = 37
                             or werr = 38 or = 39 or = 40 or = 47
                             move 8    to wok
                             MOVE ZERO TO WERR  GO TO ERRF.

      ** CDE TRANSFEREE -> FICHIER DES ERREURS

      * verification si cde cree ==> si oui ==> suppression de la commande
      * pour ne pas avoir la commande a la fois dans le .core et ds las cdes
      * du jour
           if wecree = 1 perform annul.
           MOVE RENGCD TO WGCD.
           MOVE RENGLI TO WGLI.
           MOVE RENRCC TO WRCC.
       ERR5.
           IF WERR NOT = 32  MOVE WRAN1 TO gencolig-key
                             MOVE ZERO TO WLEC
                             GO TO ERR7.
           if wgcd > rligcd  go to err10.
           IF WLC > RLILC    GO TO ERR10.
       ERR7.
           move wor-gencoent to wor-errent
      *****WRITE ENRSEN.
           perform w-errent.
           IF file-status NOT = "00"  GO TO ERRSO.

      * creation des commentaires en erreur
      * si les commentaires ont deja ete lu on se repositionne sur le 1er rang
           if wrancom = zero go to err7-s.
           move wrancom to gencocom-key.
           perform r-gencocom.
           if file-status not = zero go to err8.
      * memo ds cle commentaire
      *    move rcolc to wclecom.
           move RCOGCD to weancomc
           move RCOGLI to weancom
           move RCORCC to wcdecom
           .
       err7-s.
           move "E" to wcrecom.
           perform trtcom.
       err8.

           IF WERR = 32        GO TO ERR14.
           IF WFIN = 1         GO TO ERR14.
           IF WLEC = 1         GO TO ERR10.
       ERR9.
      *****READ LIGCDE INVALID KEY MOVE 1 TO WFIN
           perform r-gencolig.
           if file-status not = zero
                     MOVE 1 TO WFIN
                               GO TO ERR12.
           if wgcd not = rligcd  go to err12.
           IF RLILC NOT = WLC  GO TO ERR12.
           IF WERR = 32        GO TO ERR12.
       ERR10.
           move wor-gencolig to wor-errlig
      *****WRITE ENRSLI.
           perform w-errlig.
           IF file-status NOT = "00"  GO TO ERRSO.
           ADD 1 TO gencolig-key.
           GO TO ERR9.
       ERR12.
           MOVE 1 TO WLEC.
           MOVE gencolig-key TO WRAN1.
           IF WERR NOT = 32    GO TO ERR14.
           IF TTFIN = ZERO     GO TO T25.
           GO TO ERR18.
       ERR14.
           ADD 1 TO gencoent-key.
           IF TTFIN = ZERO     GO TO T20.
      ** CDES SUIVANTES -> FICHIER ERREURS
       ERR16.
      *****READ ENTETE INVALID KEY GO TO FIN.
           perform r-gencoent.
           if file-status not = zero
                     GO TO FIN.

      * init num cde
           move zero to wnum.

       ERR18.
           MOVE ZERO TO WERR.
           IF WFIN = 1         GO TO ERR7.
           MOVE RENGCD TO WGCD.
           MOVE RENGLI TO WGLI.
           MOVE RENRCC TO WRCC.
           if wgcd > rligcd  move 32 to werr go to err10.
           if wgcd < rligcd  move 32 to werr go to err7.
           IF WLC > RLILC  MOVE 32 TO WERR  GO TO ERR10.
           IF WLC < RLILC  MOVE 32 TO WERR.
           GO TO ERR7.
       ERRF.
           EXIT.
       TIT.
           MOVE SPACES TO LIGNE.
           WRITE LIGNE BEFORE PAGE.
           MOVE "ERREURS EN CREATION AUTOMATIQUE DES COMMANDES ALLEGRO"
                         TO LLIB.
           MOVE WJ  TO LLJJ.
           MOVE WM  TO LLMM.
           MOVE WA  TO LLAA.
           MOVE "/" TO LS1 LS2.
           WRITE LIGNE BEFORE 3.
           MOVE SPACES TO LIGNE.
           MOVE "CLIENT    REF.CDE    CDE GPI    DATE LIVR.      ARTICLE
      -    "                    E R R E U R S" TO LIGNE.
           WRITE LIGNE BEFORE 2.
           MOVE SPACES TO LIGNE.
           MOVE 5 TO WLIG.
      *
       ERRSO.
           DISPLAY "***FICHIER ERREURS PLEIN***".
           MOVE "*** FICHIER ERREURS PLEIN ***" TO LLIB.
           PERFORM ERR3.
      *
       FIN section.
      *****    CLOSE
           perform cl-gencoent.
           perform cl-gencolig.
           perform cl-fcommaap.
           perform cl-multidat.
           perform cl-fartusap.
           perform cl-fartusac.
           perform cl-fclients.
DD2001*    perform cl-fcoadcli.
DD2001     perform cl-fjoadcli.
           perform cl-fcomjoap.
           perform cl-fcomjoc1.
           perform cl-fcomjoc2.
           perform cl-fcomjoc4.

           perform cl-fcomjoc6.
           perform cl-fcomjoc5.
           perform cl-errcom.
           perform cl-seqcom1.

           perform cl-paramgpi.
           perform cl-errent.
           perform cl-errlig.

           perform cl-guextmst.
           perform cl-ttfacmst.
           perform cl-filieres.
           perform cl-cdesrepr.
           CLOSE ETAT.
           STOP RUN.

      *=========================================================================
      *                             FONCTIONS LOCALES
      *=========================================================================


      * recherche tarif
       rech-tarif section.
           move "1" to icgta-rech-typqd
           move "3" to icgta-rech-typqf
           move " " to icgta-rech-trech
           move jfcqtc to icgta-rech-q
      * conversion en nbre de blisters
      *GPIWARNING pour l instant on considere le quantitatif tarif dans l unite
      *GPIWARNING de saisie de commande (un/dinac, bte/gpi)
           move 1    to  icgta-rech-m
      * on appelle la recherche tarif avec la date de reference tarif
      *       ici identique a la date de creation de la commande
           move fjdatetarif9 to icgta-rech-datec
           move spaces to icgta-rech-lieu
           move jfcnar  to icgta-rech-narx
           move jfcsre  to icgta-rech-srfx
           move fjncl  to icgta-rech-nclx
      * recherche correspondance devise
           perform rech-devise
           if ommpa-devi-rtn not = "0"
              move 7 to werr
              go to rech-tarif-fin
           else
              move wmmpa-devi-cdev to wcgta-rech-cdev
           end-if

           move zero     to icgta-rech-remsup
           move spaces to icgta-rech-snclx
           move 'C'    to icgta-rech-trt
           move zero to icgta-rech-remsupl
           move zero to icgta-rech-majsupl
DD0420     move ccgta-rech-infocom-h to icgta-rech-infocom
           call 'cgta-rech1' using cgta-rech adl-art
GPICMT* controle prix trouve et interface
           perform ctrl-tarif
ELGU  *    if ocgta-rech-rtn not = '0'
ELGU  *       move 7 to werr
ELGU  *       go to rech-tarif-fin
ELGU  *    display "trace_cged-inte0 : IF no43"
ELGU  *    end-if
           move ocgta-rech-topx to td-top-prix
           move ocgta-rech-pbas to jfcpbas
           move ocgta-rech-poub to jfctrpv
           move ocgta-rech-net to jfcpht jfcpcl
           move ocgta-rech-brut to jfcpth.
GPICMT* on surcharge les zones prix avec ceux de l'interface
           move '1' to ocgta-rech-topx td-top-prix
           move RLIPVT to ocgta-rech-net jfcpht jfcpcl
           move RLIPHT to ocgta-rech-brut jfcpth ocgta-rech-pbas
                          ocgta-rech-pbas
           move RLQTEGRATINCL to  ocgta-rech-poub jfctrpv
           if jfcpth = jfcpht
              move zero to ocgta-rech-poub jfctrpv
           else
              if jfcpht = zero
                 move 100 to ocgta-rech-poub jfctrpv
              else
                 if jfcpht > jfcpth
                    move jfcpht to jfcpth ocgta-rech-pbas
                                   ocgta-rech-pbas
                    move zero to ocgta-rech-poub jfctrpv
                 else
                    subtract jfcpht from jfcpth giving wcalr
                    divide jfcpth into wcalr rounded
                    multiply wcalr by 100 giving ocgta-rech-poub
                                                 jfctrpv
                 end-if
              end-if
           end-if
           .
       rech-tarif-fin.
           exit
           .

GPICMT* controle tarif interface et trouve
       ctrl-tarif section.
           if ocgta-rech-net not = RLIPVT
              move ocgta-rech-net to wpht
              move RLIPVT         to wapht
              if ocgta-rech-net = zero
                 display 'TARIF ZERO |' fjcle
                         '|' RENRCC '|' renbtrans '|' fjncl
                         '|' icgta-rech-narx
                         '|' wpht '|' wapht
              else
                 display 'TARIF DIFFERENT |' fjcle
                         '|' RENRCC '|' renbtrans '|' fjncl
                         '|'  icgta-rech-narx
                         '|' wpht '|' wapht
              end-if
           end-if
           .


      * controle devise
       rech-devise section.
           move "C"                 to immpa-devi-tfc
           move "f"                 to immpa-devi-trt
           move ' '                 to immpa-devi-aff
           move fjdev               to wmmpa-devi-cdev9
           move spaces              to wmmpa-devi-cdev
           call 'mmpa-devi1' using mmpa-devi adl-art
           .


       pro section.

           copy "../copy/pro-gencoent".
           copy "../copy/pro-gencolig".
           copy "../copy/pro-paramgpi".
           copy "../copy/pro-fcomjoap".
           copy "../copy/pro-fcomjoc1".
           copy "../copy/pro-fcomjoc2".
           copy "../copy/pro-fcomjoc4".
DD2001*    copy "../copy/pro-fcoadcli-cdesup".
DD2001     copy "../copy/pro-fjoadcli-cdesup".
           copy "../copy/pro-fclients".
           copy "../copy/pro-fartusap".
           copy "../copy/pro-fartusac".
           copy "../copy/pro-multidat".
           copy "../copy/pro-fcommaap".
           copy "../copy/pro-errent".
           copy "../copy/pro-errlig".

           copy '../copy/pro-guextmst'.
           copy '../copy/pro-ttfacmst-cdesup'.
           copy '../copy/pro-filieres'.

           copy '../copy/pro-fcomjoc3'.
           copy '../copy/pro-cliartsp'.

           copy '../copy/pro-avoircli'.
           copy '../copy/pro-fcomjoc6'.
           copy '../copy/pro-fcomjoc5'.
           copy '../copy/pro-errcom'.
           copy '../copy/pro-seqcom1'.
           copy '../copy/pro-gencocom'.
           copy '../copy/pro-cdesrepr'.
