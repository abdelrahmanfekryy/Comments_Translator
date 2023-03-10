      *DD2001 31/08/16 anes Remplacement fcoadcli par fjoadcli bloc adresse jour
      *DD0351 07/12/15 micn Pas de MAJ du suivi si reference infocom = composant
      *DD0351 10/10/12 pase Changement destinataire de mail
      *DD0712 03/10/12 door gest des commandes SOS => Maj flag reliquat dans OM1
      *DD0516 19/09/11 elgu initialisation code commande piege a partir du clien
      *ELGU09 28/03/11 elgu CHINE
      *DD0351 04/11/10 elgu lire sans bloquer fcommac4 afin d'eviter les patienc
      *DD0459 10/06/10 micn ne pas prendre en cpte la qte cde des fictifs cpsant
      *DD0444 24/03/10 elgu mise a jour MYSQL avec fichier xml
      *DD0351 23/02/10 elgu charger le transporteur impose de la fiche client
      *DD0298 12/11/09 elgu ajout mise a jour nombre de commande preparee dans e
      *DD0351 05/10/09 micn si lancement associe a la commande le solder au mome
      *DD0351 18/09/09 micn affichage libelle erreur en retour de mgca-dest1
      *DD0351 24/04/08 elgu supprimer le traitement pour fcfac = 3
      *DD0350 04/04/08 elgu modif recherche maj suivicde
      *DD0351 06/02/08 elgu envoi des commandes cree dans la jpurnee dans la dat
      *DD0380 06/12/07 elgu gerer destockage special pour ERELS
      *DD0351 15/11/07 elgu ne pas piege les commandes pour slovaquie
      *DD0351 17/09/07 elgu complement message d'erreur
      *DD0351 31/07/07 elgu modification pour stock aricle negatif
      *DD0035 12/01/07 micn appel creation de lancement en arriere plan
      *DD0351 02/11/06 elgu appel maj data ware pour suppression de ligne
      *DD0326 20/09/06 elgu
      *DD0326 12/06/06 elgu
      *DD0316 01/05/06 elgu
      *DD0316 18/04/06 elgu nlle wor-fcommac1.mod c3/c4/c5
      *DD0298 16/02/06 elgu  ne pas traiter une cimmande allotie mere
      *DD9999 16/12/05 micn initialize zones de comm. cmcd-clan
      *DD0180 12/08/04 elgu donnees specifique clients fcomjoc8
      *DD9999 elgu le 02/07/04 blocage fcommac2
      *DD0131 12/11/03 elgu ne pas destocker article 01 sur cde Industrie
      *DD0073 13/05/03 elgu
      *DD0066 17/04/03 elgu
      *DD0031 31/03/03 elgu
      *DD0019 27/02/03 elgu
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CGCD-RELI1.
      *
      * GPICMT SAISIE DES LIVRAISONS : SAISIE DATE DE LIVRAISON
      * GPICMT POUR LIVRAISON DEFINITIVE AVEC RELIQUATS
      * 18/04/2000 creation DOOR
      *
      * GPICMT Cette fonction permet de creer un reliquat pour le Grand-Public
      * GPICMT on fait egalement le destockage.
      * GPICMT FC1 = indique si la ligne est en rupture ou rupture + reliquat
      * GPICMT FC1 = " " livraison qte commandee
      * GPICMT FC1 = "0" rupture
      * GPICMT FC1 = "1" rupture + reliquat
      *
      * GPIWARNING attention pour DINAC on numerote les lignes en reliquat
      * GPIWARNING a voir pour GPI avec trt detail prix et commentaires ligne
      *
      *DDE999  elgu 26/02/03 correction creation depot article si inexistant
      *DE0012 24/02/03 micn on ne cre pas de lancement si reliquat
      *DDE212 05/02/03 micn appel fonction de maj lancement si reliquat
      *DDE046 31/12/02 elgu
      *DDE314 17/12/02 elgu ajout maj code validation possible annulation
      *DDE313 13/12/02 elgu  ajout du niveau de la référence
      *DDE295 19/11/02 elgu
      *DDE275 11/10/02 micn creation d'un fichier contenant le nbre de cde et
      *       de ligne par client
      * 12/09/02 elgu ne pas bloquer fartusap  sauf dans le cas de depot pour
      *          ne pas creer un article par copie alors qu'on le modifie
      *DDE125 nouvelle wor fcommac2 et fcomjoc2
      *DDE054
      *DDE125 ajout maj fichier des ruptures
      *M210302 elgu correction maj total qte livree
      *M090102 correction maj depot livre remplace zones fj par fc
      *DDE089: conversion du montant a mettre dans article en devise de base
      *DDE069: rendre SLIV identique entre GPI et DINAC avec nouveau TARIF
      *        avec remplacement de la td pat liv00.com
      *        numerotation des lignes idem GPI et DINAC
      *M0397a: maj numero de ligne pour les reliquats et commentaires de ligne
      *        pour DINAC uniquement
      *door 03/07/01 Correction suite a modif wor-tracetel
      *door 21/05/01 Correction bug remplacement de variables en working par
      *              des copies
      *DDE069: aj
      *DDE034: door 20/10/00 gestion des familles
      *                      => remise a plat du fichier article
      *DDE064: tenir compte des remises speciales qui ne s'applique pas sur
      *          tous les articles
      *DDE026: ajout trt des commentaires fcomjoc6
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      ***** Variables localess
       01  LOC.
           02 flag-entete       PIC 9.
      *DDE069 l'ecran 28 n'est plus affiche, il n'est renseigne qu'en cas de
      *       creation de fartusap ou consoart (cas depot livre) on estime
      *       que l'erreur ne peut etre qu'une cle double donc sans souci car
      *       l'article peut tres bien avoir ete deja cree

      *
      **** ERREURS DE MAJ ENVOYEES EN FIN
       01  OU28.
           02  OLIGN OCCURS 15.
            03 ONMA               PIC 99.
DDE046      03 ONAR               PIC x(7).
DDE046      03 OSRE               PIC x(2).
            03 OQTL               PIC Z(5)9.99.
            03 OMON               PIC Z(8)9.99.
            03 OSIG               PIC X.
            03 OLIB               PIC X(26).

      *
       01  TOUT.
DD0444     02 sys-var                   PIC X(200).
DD0444     02 syst-rtn                  PIC s9(4)  comp.
DD0444* meme gpimr
         02 wmmdt-mr pic x(15).
DD0131* numero de ligne pour envoi mail
         02 wz    pic 9(2).
DDE069   02  wnom-prog                PIC X(10) value 'cgcd-reli1'.
      * qte cdee et livree pour trace
           02 wqtcap            pic z(5)9v,99.
           02 wqtlap            pic z(5)9v,99.

      * compteur lignes en rupture
           02 wttecpt           pic 999.

      *----> M1099 (D) GPICOL
           02  wedit            pic x.
      *----> M1099 (F) GPICOL
      **** 0=SANS RELIQUAT / 1=AVEC RELIQUAT / 9=SANS MAJ DEVIS
           02  WRELIQ           PIC 9.
           02  I                PIC 99.
           02  WNMA             PIC 99.
           02  WQT              PIC 9(6)V99.
           02  WMONT            PIC 9(9)V99.
           02  WCALC            PIC 9(9)V99.
           02  WREM7 REDEFINES WCALC PIC 9(7)V9(4).
      *    02  WAPART           PIC X(210).
        copy "../copy/wor-fartusap.mod" replacing ==(pref)== by ==w==.
      *    02  WACART           PIC X(215).
        copy "../copy/wor-fartusac.mod" replacing ==(pref)== by ==w==.
           02  WREST            PIC 9(6)V99.
           02  wldop             PIC X(22).
           02  wsupcd           pic 9.
      **** code prepare (3 ou 4 = deja soustrait des reservations)
           02  wprep            pic 9.
           02  wcptr            pic 9.
      * nbre de boites ds la commande en reliquat
           02 wquan             pic 9(8)v99.
      * nbre de boites ds la commande preparee
           02 wquanp            pic 9(8)v99.
      * numero de semaine de livraison
           02 tsem              pic 99.
DDE275* nombre de ligne de cde
           02 wligcde           pic 9(4).
      * nombre de ligne d'avoir
           02 wligavoir         pic 9(4).
      * nombre de ligne de rupture sans reliquat
           02 wligrupt          pic 9(4).
      * nombre de ligne de rupture avec reliquat
           02 wligruptrel       pic 9(4).
      * nombre de ligne de cde EDI
           02 wligedi           pic 9(4).
      * nombre de ligne de cde EDI en rupture sans reliquat
           02 wligedirupt       pic 9(4).
      * nombre de ligne de cde EDI en rupture avec reliquat
           02 wligediruptrel    pic 9(4).
      *
      * numero de cde/indice
           02 wcde.
DD0316       03 wncd            pic 9(7).
             03 wnin            pic 9.
      * date du jour
           02 wdate             pic 9(6).
           02 filler redefines wdate.
             03 waa             pic 99.
             03 wmm             pic 99.
             03 wjj             pic 99.

      * date du jour avec siecle
           02 zwcd.
            03 zss              pic 99.
            03 zaa              pic 99.
            03 zmm              pic 99.
            03 zjj              pic 99.

DDE212     02 wlc1              pic x(18).
DD0351     02 wcpt              pic 9(1).    

DDE069     02  wegnlg           pic 9(4).                               *M0397a
           copy '../copy/wor-fcommaap-cdesup'.                          *GPICMT
           copy '../copy/wor-fcommac1'.                                 *GPICMT
           copy '../copy/wor-fcommac2-cdesup'.                          *GPICMT
           copy '../copy/wor-fcommac3-cdesup'.                          *GPICMT
           copy '../copy/wor-fcommac4-cdesup'.                          *GPICMT
           copy '../copy/wor-fcommac5-cdesup'.                          *GPICMT
DDE045     copy '../copy/wor-fcommac7'.                                 *GPICMT
DD0180     copy '../copy/wor-fcommac8'.                                 *GPICMT
           copy '../copy/wor-fcomjoap'.                                 *GPICMT
           copy '../copy/wor-fcomjoc1'.                                 *GPICMT
           copy '../copy/wor-fcomjoc2'.                                 *GPICMT
           copy '../copy/wor-fcomjoc3'.                                 *GPICMT
           copy '../copy/wor-fcomjoc4'.                                 *GPICMT
           copy '../copy/wor-fcomjoc5'.                                 *GPICMT
DDE045     copy '../copy/wor-fcomjoc7'.                                 *GPICMT
           copy '../copy/wor-fcoadcli-cdesup'.                          *GPICMT
DD2001     copy '../copy/wor-fjoadcli-cdesup'.                          *GPICMT
           copy '../copy/wor-numdevis'.                                 *GPICMT
           copy '../copy/wor-artdevc1'.                                 *GPICMT
           copy '../copy/wor-fartusap'.                                 *GPICMT
           copy '../copy/wor-fartusac'.                                 *GPICMT
           copy '../copy/wor-consoart'.                                 *GPICMT
           copy '../copy/wor-suivicde'.                                 *GPICMT
           copy '../copy/wor-multidat'.                                 *GPICMT
DDE125     copy '../copy/wor-ruptures'.                                 *GPICMT
DDE275     copy '../copy/wor-comptage'.                                 *GPICMT
DD0298     copy '../copy/wor-cdesalle'.                                 *GPICMT
DD0351     copy '../copy/wor-fclients'.                                 *GPICMT
           copy '../copy/cmta-comi.com'.                                *GPICMT
DDE089     copy '../copy/mmca-devi.com'.                                *GPICMT
DDE212     copy '../copy/cmcd-clan.com'.                                *GPICMT
DD0298     copy "../copy/cmpa-tycd.com".                                *GPICMT
DD0351     copy '../copy/cmcd-ware.com'.                                *GPICMT
DD0380     copy '../copy/fgar-spec.com'.                                *GPICMT
DD0351     copy '../copy/cmex-stat.com'.                                *GPICMT
DD0459     copy '../copy/mmut-fict.cst'.                                *GPICMT

      *----> DDE026 (D)
           copy '../copy/wor-fcommac6-cdesup'.                          *GPICMT
           copy '../copy/wor-fcomjoc6'.                                 *GPICMT
DD0351     copy '../copy/wor-cdeslanc'.                                 *GPICMT
      *----> DDE026 (F)

           copy '../../COM/copy/mmau-cdes.com'.                         *GPICMT
           copy '../../COM/copy/mgca-dest.com'.                         *GPICMT
           copy '../../COM/copy/mmdt-lieu.com'.                         *GPICMT
           copy '../../COM/copy/mmti-sema.com'.                         *GPICMT
DDE069     copy '../../COM/copy/cgca-mtht.com'.                         *GPICMT
DDE069     copy '../../COM/copy/mmtr-trac.com'.                         *GPICMT
DDE109     copy '../../COM/copy/mmti-date.com'.                         *GPICMT
DD0073     copy '../../COM/copy/fidv-mjqt.com'.                         *GPICMT
DD0073     copy '../../COM/copy/mmpa-mail.com'.                         *GPICMT
DD0073     copy '../../COM/copy/mmlp-mail.com'.                         *GPICMT
DD0073     copy '../../COM/copy/mmaf-ubla.com'.                         *GPICMT
DD0131     copy '../../COM/copy/mmlx-lect.com'.                         *GPICMT
DD0180     copy '../../COM/copy/cmcd-crea.com'.                         *GPICMT
DD0351     copy '../../COM/copy/micl-lect.com'.                         *GPICMT
DD0351     copy '../../COM/copy/fab30.com'.                             *GPICMT
DD0298     copy '../../COM/copy/cgcd-mere.com'.                         *GPICMT
DD0444     copy '../../COM/copy/cmcd-mjdw.com'.                         *GPICMT
       LINKAGE SECTION.
           copy '../copy/cgcd-reli.com'.                                *GPICMT
           copy "/usr/action/ADL/copy/wor-adl".
004180 PROCEDURE DIVISION using cgcd-reli adl-art.
       DEBDEB.
999999   display "ATTENTION PROGRAMME:  cgcd-reli1 trace par anes".
999999   display "Trace-cgcd-reli1 : DEBDEB mmdt-mr " mmdt-mr.
999999   display "mmdt-envi-batch " mmdt-envi-batch
      * recuperation date du jour                                       *GPICMT
           move 'D' to immti-date-taj
999999   display "trace_cgcd-reli1 : call 'mmti-date1'"
           call 'mmti-date1' using mmti-date adl-art
           move wmmti-date-amj to wdate
           .

           if waa > 50 move 19 to zss
             else      move 20 to zss.
           move waa            to zaa.
           move wmm            to zmm.
           move wjj            to zjj.

           move spaces to wedit.
           MOVE SPACES TO cgcd-reli-liberr.
           move '0' to  ocgcd-reli-rtn

DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcommaap.                                         *GPICMT
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcommac2.                                         *GPICMT
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fartusap.                                         *GPICMT
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fartusac.                                         *GPICMT
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-consoart.                                         *GPICMT
DDE275     move "W" to gfkey.                                           *GPICMT
DDE275     perform op-comptage.                                         *GPICMT
       T5.
999999   display "Trace-cgcd-reli1 : T5".
           MOVE ZERO TO WQUAN wquanp flag-entete.
DDE275     move 0 to wligcde wligavoir wligrupt wligruptrel
DDE275     move 0 to wligedi wligedirupt wligediruptrel
         .
       T20.
999999   display "Trace-cgcd-reli1 : T20".
DD2001*    move cgcd-reli-cde to fcnum wncd alnum pfjnum1
DD2001     move cgcd-reli-cde to fcnum wncd alnum of wor-fcoadcli-cdesup
DD2001                           pfjnum1
                                 fcnum2 fcnum3 fcnum4 fcnum5.
DD2001*    move cgcd-reli-ind to fcnin wnin alnin pfjnin1
DD2001     move cgcd-reli-ind to fcnin wnin alnin of wor-fcoadcli-cdesup
DD2001                           pfjnin1
                                 fcnin2 fcnin3 fcnin4 fcnin5.
      *****READ FCOMMDES INVALID KEY INVA ON LOCK PREEMP.
           perform r-fcommaap.
           if file-status not = zero   go to INVA.
           move fccle to wcde

GPICMT* test du type de commande, si on n'edite pas le BP on ne fait
GPICMT* pas non plus de destockage
DD0298     move fcfeo to wcmpa-tycd-typ                                 *GPICMT
  "        move ccmpa-tycd-gestion-tycd to icmpa-tycd-gestion           *GPICMT
999999   display "trace_cgcd-reli1 : call 'cmpa-tycd1'"
  "        call 'cmpa-tycd1' using cmpa-tycd adl-art                    *GPICMT
  "        if ocmpa-tycd-edbp = cmmdt-envi-rtn-faux                     *GPICMT
  "           string 'Cde ' fccle ' Non livrable '                      *GPICMT
  "                delimited size into cgcd-reli-liberr
  "           move cmmdt-envi-rtn-err to ocgcd-reli-rtn
999999   display "trace_cgcd-reli1 : IF no1"
  "           go to t130
DD0298     end-if

      *GPICMT  Appel de l'automate des commandes avec evenement PIK0
      *GPICMT  pour destockage
           move FCLIV to wmmau-cdes-etliv.
GPICMT     move 'PIK0' to immau-cdes-ev
999999   display "trace_cgcd-reli1 : call 'mmau-cdes1'"
999999   display "trace_cgcd-reli1 : mmdt-mr " mmdt-mr  
999999   display "trace_cgcd-reli1 : mmdt-sect " mmdt-sect  
           call 'mmau-cdes1' using wmmau-cdes adl-art.
           IF ommau-cdes-rtn not = 0
999999   display "trace_cgcd-reli1 : IF no1-a"
              go to err-auto
           .

GPICMT* raz total qte livree avec celle de la commande pour palier au cas ou
GPICMT* une cde a ete traitee et soldee par erreur et scannee apres avoir ete
GPICMT* deverrouillee car le trt des lignes ayant deja ete fait le cumul de qtes
GPICMT* livrees ne se faisait pas et le scannage detectait une incoherence
GPICMT     move fctql to wquanp                                         *M210302

      * appel recherche des remises fin de facture a appliquer sur chaque
      * valeur livree d'article
           move wcde to wcmta-comi-ncd.
999999   display "trace_cgcd-reli1 : call cmta-comi3"
           call "cmta-comi3" using wcmta-comi adl-art.
           if wcmta-comi-err not = spaces go to errremise.

      * initialisation compteur des lignes en rupture
           move zero to wttecpt.

++++++*------------------------------------------------------------------
++++++*  Si on n'a pas de reliquat, on ne cree pas les fichiers d'entete
999999   display "cgcd-reli-rel " cgcd-reli-rel
           IF cgcd-reli-rel not = 'R' go to T35.

++++++**-----------------------------------------------------------------
++++++** CREATION DES ENTETES DE LA COMMANDE RELIQUAT
      *
       t22.
999999   display "Trace-cgcd-reli1 : t22".
           move cgcd-reli-cde to fcnum.
           move cgcd-reli-ind to fcnin.
           perform r-fcommaap.
           if file-status not = zero go to inva.
      * Recuperation du no de la semaine
DD0326     MOVE wor-fcommaap2 to wor-fcomjoap.
           move cgcd-reli-indr to fjnin.
222222     move fcpre to fjpre.
           MOVE ZERO TO FJPOI FJVOL FJMHT FJNBE FJNBC FJNCO FJPBR FJTQL
                        FJTRS.
           MOVE SPACES TO FJPTT FJTAR FJOD1 FJOD2.
           MOVE ZERO TO FJORE FJCAL FJEBL FJEDE FJJNL FJPRE FJLIV FJAFA
                        FJFAC.
           MOVE ZERO TO FJDI1.
      ***  Novembre 97 (On a le No de regroupement qui reste)
           move zero to FJnumr FJninr.
           MOVE 1 TO FJOK.
GPICMT* elgu le 14/10/02 suite a demande mige conserver le code port initial
GPICMT*    MOVE 2 TO FJFRA.

GPICMT* lecture client livre pour charger le transporteur impose
DD0351     move fclivrea to clncl
  "        perform rnl-fclients
  "        if file-status = zero
  "           move cltrh to fjtrs
999999   display "trace_cgcd-reli1 : IF no2"
DD0351     end-if

      *****WRITE FCOMJOUR INVALID KEY ERRWRC.
DDE109     move "W" to gfkey.
DDE109     perform op-fcomjoap.
           perform w-fcomjoap.
           if file-status not = zero   go to ERRWRC.
DDE109     perform cl-fcomjoap

      *DDE069 si mode non batch on affichera le message cree en reliquat
           if mmdt-envi-batch = zero
              move 'RELIQUAT CREE EN COMMNANDE DU JOUR'
                  to cgcd-reli-liberr
999999   display "trace_cgcd-reli1 : IF no3"
           end-if

      *    creation element trace creation reliquat
DDE069*    creation trace par fonction
           move space to wmmtr-trac
           move 'C' to immtr-trac-action
           string fjcle delimited by size into immtr-trac-num
           string 'CREATION DU RELIQUAT DE LA CDE: ' wcde
                  delimited by size into immtr-trac-commentaire
           perform cre-trace

333333** 4 ADRESSES (CDE/LIV/FACT/REGLT)
      *****READ FCOADCLI INVALID KEY T30 NO LOCK.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcoadcli.                                         *GPICMT
DD2001     perform op-fjoadcli.                                         *GPICMT
           perform rnl-fcoadcli.
           if file-status not = zero
999999   display "trace_cgcd-reli1 : IF no3-a"
               go to T30.
DD2001*    MOVE cgcd-reli-indr TO ALNIN.
  |        move wor-fcoadcli-cdesup to wor-fjoadcli-cdesup
  |        move cgcd-reli-indr TO ALNIN of wor-fjoadcli-cdesup
  |   *    perform w-fcoadcli.
DD2001     perform w-fjoadcli.
           if file-status not = zero
DD2001*       string 'Ecriture FCOADCLI invalide ' alcle
  |           string 'Ecriture FJOADCLI invalide ' 
DD2001                         alcle of wor-fjoadcli-cdesup
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
DD2001*       perform cl-fcoadcli
DD2001        perform cl-fjoadcli
              go to t130
999999   display "trace_cgcd-reli1 : IF no4"
           end-if
          .
      *
444444*** ELEMENT 3
       t30.
999999   display "Trace-cgcd-reli1 : t30".
DD2001*    perform cl-fcoadcli
DD2001     perform cl-fjoadcli
DDE069     perform rnl-fcommac1.
           if file-status not = zero  go to t32.
DD0316     move wor-fcommac1 to wor-fcomjoc1.
           move cgcd-reli-indr to fjnin1.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcomjoc1.                                         *GPICMT
           perform w-fcomjoc1.
           if file-status not = zero
              string 'Ecriture FCOMJOC1 invalide ' fjcle1
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcomjoc1
              go to t130
           else
              perform cl-fcomjoc1
999999   display "trace_cgcd-reli1 : IF no5"
           end-if
          .
      *
bbbbbb**  ELEMENT 99 nbre d'echeance
       t32.
999999   display "Trace-cgcd-reli1 : t32".
           move 99 to fcnel5.
DDE069     perform rnl-fcommac5.

      * si invalide ==> trt des commentaires                            *DDE026
      *    if file-status not = zero  go to t35.                        *DDE026
           if file-status not = zero  go to t33.                        *DDE026

           move wor-fcommac52 to wor-fcomjoc5.
           move cgcd-reli-indr to fjnin5.
           move zero  to fjnli5.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcomjoc5.                                         *GPICMT
           perform w-fcomjoc5.
           if file-status not = zero
              string 'Ecriture FCOMJOC5 invalide ' fjcle5
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcomjoc5
              go to t130
           else
              perform cl-fcomjoc5
999999   display "trace_cgcd-reli1 : IF no6"
           end-if
          .

      *----> DDE026 (D)
      * trt des commentaires
       t33.
999999   display "Trace-cgcd-reli1 : t33".
           move cgcd-reli-cde to fcnum6.
           move cgcd-reli-ind to fcnin6.
           move zero to fcnli6.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcomjoc6.                                         *GPICMT
           perform snl-fcommac6.
           if file-status not = zero go to t33-f.
       t33-a.
999999   display "Trace-cgcd-reli1 : t33-a".
           perform nnl-fcommac6.
           if file-status not = zero go to t33-f.
           if fcnum6 not = cgcd-reli-cde or
              fcnin6 not = cgcd-reli-ind go to t33-f.
           move wor-fcommac62 to wor-fcomjoc6.
           move cgcd-reli-indr to fjnin6.
           perform w-fcomjoc6.
           if file-status not = zero
              string 'Ecriture FCOMJOC6 invalide ' fjcle6
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcomjoc6
              go to t130
999999   display "trace_cgcd-reli1 : IF no7"
           end-if
          .
           go to t33-a.
      *----> DDE026 (F)
DDE109 t33-f.
999999   display "Trace-cgcd-reli1 : t33-f".
DDE109     perform cl-fcomjoc6
           .

GPICMT* creation donnees specifiques client
DD0180   move fccle to pf8-cle
         perform rnl-fcommac8
         if file-status = zero
            move wor-fcommac8 to cwor-fcomjoc82
            move fccle to cf8-cle
            move cgcd-reli-indr to cf8-nin
            move ccmcd-crea-trt-jour to icmcd-crea-trt
            move ccmcd-crea-type-c8 to icmcd-crea-type
999999   display "trace_cgcd-reli1 : call 'cmcd-crea1'"
            call 'cmcd-crea1' using cmcd-crea adl-art
            if ocmcd-crea-rtn not = cmmdt-envi-rtn-ok
               string "Cde " fccle " "  ocmcd-crea-liberr  " RTN "
                  ocmcd-crea-rtn
                   delimited size into cgcd-reli-liberr
               move '3' to ocgcd-reli-rtn
               go to t130
999999   display "trace_cgcd-reli1 : IF no8"
            end-if
999999   display "trace_cgcd-reli1 : IF no9"
         end-if
         perform cl-fcommac8
         .


555555*** ELEMENTS 4
       t35.
999999   display "Trace-cgcd-reli1 : t35".
DDE069     move zero to wegnlg.                                          *M0397a
           move  4   to fcnel2.
           move zero to fcunix2.
           perform snl-fcommac2.
           if file-status not = zero  go to t80.
       t40.
999999   display "Trace-cgcd-reli1 : t40".
           move zero to wreliq.
DDE069*    perform nnl-fcommac2.
DD9999     perform n-fcommac2.
           if file-status not = zero  go to t80.
           if fcnum2 not = cgcd-reli-cde or
              fcnin2 not = cgcd-reli-ind  go to t80.
           if fcge1 = "1"             go to t40.
DDE275     if fcfoa < 5
              add 1 to wligcde
              if fcqsa = "AUTO" or fcqsa = "EDI "
                 add 1 to wligedi
999999   display "trace_cgcd-reli1 : IF no10"
              end-if
           else
              add 1 to wligavoir
999999   display "trace_cgcd-reli1 : IF no11"
           end-if
DD0459* on ne traite pas les lignes de composant mais on met a jour la qte livre
      *    if fcfictif = cmmut-fict-fictif-cpst
      *       go to t40
      *    end-if
           move fccle2 to fccle3.
MICN       move fcnel2 to fcnel3
           move fcart  to fcart3.
           move zero   to fclig3.
       T42.
999999   display "Trace-cgcd-reli1 : T42".
      *    FC1 : ' '=normal '0'= rupture '1'=rupture + reliquat

      * si trt rupture creation de la trace
           if fc1 = spaces go to t43.
DDE069*    creation trace par fonction
           move space to wmmtr-trac
           move 'M' to immtr-trac-action
           if fc1 = "1" move "R" to immtr-trac-rup
             else       move "r" to immtr-trac-rup
DDE099* elgu le 060103 end-if manquait ==> dans la trace en cas de reliquat
      *                on perdait le num de cde ainsi que la qte cdee
999999   display "trace_cgcd-reli1 : IF no12"
           end-if
           string wcde delimited by size into immtr-trac-num

           move wttecpt to immtr-trac-cpt
           move fcqtc to wqtcap.
           move fcqtl to wqtlap.
999999   display "cgcd-reli1 fcqtl " fcqtl
           string 'RUPTURE ARTICLE : ' fcnarx '" "' fcsrex
                  '  QTE CDEE: ' wqtcap
                  '  QTE LIVREE:  ' wqtlap
                  delimited by size into immtr-trac-commentaire
           perform cre-trace

GPICMT*DDE125 maj fichier des ruptures
           perform rupture

      * maj compteur lignes en rupture
           add 1 to wttecpt.

       t43.
999999   display "Trace-cgcd-reli1 : t43".
DDE275* comptage des lignes en ruptures sans reliquat
           if fc1 = "0" and fcfoa < 5
              add 1 to wligrupt
              if fcqsa = "AUTO" or fcqsa = "EDI "
                 add 1 to wligedirupt
999999   display "trace_cgcd-reli1 : IF no13"
              end-if
999999   display "trace_cgcd-reli1 : IF no14"
           end-if

      *    Si rupture complete sans reliquat
      *    On met a jour le suivi de cde sans toucher aux E/S article
           IF FCQTL = ZERO AND FC1 = "0"  perform f200 thru fin
              go to t55
           .
      * Si on n'est pas en reliquat
           IF FC1 NOT = '1'  GO TO T57.
------*----------------------------------------
------* A partir d'ici, on est EN RELIQUAT
DDE275* comptage des lignes en rupture avec un reliquat
           if fcfoa < 5
              add 1 to wligruptrel
              if fcqsa = "AUTO" or fcqsa = "EDI "
                 add 1 to wligediruptrel
999999   display "trace_cgcd-reli1 : IF no15"
              end-if
999999   display "trace_cgcd-reli1 : IF no16"
           end-if
      * Cas ou qtl > qtc : interdit pour le secteur GPU ... a voir GPIWARNING
      * on ne traite plus fc2
           IF FCQTL = ZERO  GO TO T70.
      *** DIFFERENCE  QTE CDEE-LIVREE EN RELIQUAT
           MOVE 1 TO WRELIQ  PERFORM F10 THRU FIN.
           IF FCQTL not < FCQTC move fcqtl to fcqtc
                                add fcqtl to wquanp  go to t55.
DDE125     MOVE wor-fcommac22 to jwor-fcommac22.
DDE125     MOVE cgcd-reli-indr TO jfcnin2.
DDE125     SUBTRACT jfcqtl FROM jfcqtc.
GPICMT* on ne cumul pas les qtes cde des fictifs composants
DD0459     if jfcfictif not = cmmut-fict-fictif-cpst   
DDE125        ADD jfcqtc TO WQUAN 
999999   display "trace_cgcd-reli1 : IF no17"
           end-if
DDE125     ADD jfcqtl TO wquanp.
666666 T45.
999999   display "Trace-cgcd-reli1 : T45".
DDE125     MOVE ZERO TO jfcqtl jfcnlg.
DD0712* Conservation des no de ligne si OpenBook => pour coherence avec OM1
DD0712     if fcopenbook = 1 
             move fcnlg to jfcnlg
           else

DDE069*GPIWARNING numerotation des lignes pour DINAC
              add 10 to wegnlg
DDE125        move wegnlg to jfcnlg
999999   display "trace_cgcd-reli1 : IF no18"
           end-if
DDE125     MOVE SPACES TO jfc1 jfc2 jfcge1 jfcge2 jfccres.

      * si tarif catalogue (fjtopx = "") on met 1 (pric net) ds fctopx pour
      * eviter un recalcul du prix en maj de commande
DDE125     if jfctopx = space move "1" to jfctopx.

      *%   WRITE FCOMJOUR COMPL.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcomjoc2.                                         *GPICMT
           perform w-fcomjoc2.
           if file-status not = zero
DDE125        string 'Ecriture FCOMJOC2 invalide ' jfccle2
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcomjoc2
              go to t130
           else
              perform cl-fcomjoc2
999999   display "trace_cgcd-reli1 : IF no19"
           end-if
          .
           MOVE FCQTL TO FCQTC.
      *%   REWRITE FCOMMDES.
GPICMT* changement GPIMR pour ne pas mettre a jour la dataware avec le gf
DD0444     move mmdt-mr to wmmdt-mr
DD0044     move 'DATAW' to mmdt-mr
999999   display "cgcd-reli1 fcqtl avec ecriture " fcqtl
           perform rw-fcommac2.
           if file-status not = zero
DD0444        move wmmdt-mr to mmdt-mr
              go to errmaj.
DD0444     move wmmdt-mr to mmdt-mr
      *%   ENDTR.
      *    commit.
GPICMT* maj livraison de la ligne dans la dataware
DD0444     perform liv-ligne
           move zero to wsupcd.
777777 t47.
999999   display "Trace-cgcd-reli1 : t47".
DDE045* ajout fichier detail prix
           move spaces to pf7-cle.
           move cgcd-reli-cde to pf7-num.
           move cgcd-reli-ind to pf7-nin.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcomjoc7.                                         *GPICMT
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcommac7.                                         *GPICMT
           perform snl-fcommac7.
           if file-status not = zero  go to t47-fin.
       t47-b.
999999   display "Trace-cgcd-reli1 : t47-b".
           perform n-fcommac7.
           if file-status not = zero  go to t47-fin.
           if pf7-num not = cgcd-reli-cde or pf7-nin not = cgcd-reli-ind
              go to t47-fin.
           if fcnlg not = pf7-nlg                       go to t47-b.
           MOVE wor-fcommac7 to wor-fcomjoc7.
           move wegnlg to f7-nlg.
           MOVE cgcd-reli-indr TO f7-nin.
           perform w-fcomjoc7.
           if file-status not = zero
              string 'Ecriture FCOMJOC7 invalide ' f7-cle
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcomjoc7
              go to t130
999999   display "trace_cgcd-reli1 : IF no20"
           end-if
          .
           if wsupcd not = 1            go to t47-b.
           perform d-fcommac7.
           GO TO T47-b.
       t47-fin.
999999   display "Trace-cgcd-reli1 : t47-fin".
DDE109     perform cl-fcomjoc7
DDE109     perform cl-fcommac7
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcommac3.                                         *GPICMT
           perform snl-fcommac3.
           if file-status not = zero  go to t50-fin.
       t50.
999999   display "Trace-cgcd-reli1 : t50".
           perform n-fcommac3.
           if file-status not = zero  go to t50-fin.
           if fcnum3 not = cgcd-reli-cde or
              fcnin3 not = cgcd-reli-ind  go to t50-fin.
           if fcart3 not = fcart                      go to t50-fin.

DDE069*GPIWARNING numerotation des lignes pour DINAC ==> ctrl meme numero de lig
      *GPIWARNING pour les commentaires articles
              if fcnlg3 not = fcnlg
                 go to t50
999999   display "trace_cgcd-reli1 : IF no21"
              end-if

           MOVE wor-fcommac32 to wor-fcomjoc3.
           MOVE cgcd-reli-indr TO FJNIN3.
           MOVE ZERO TO FJNLG3.

DDE069*GPIWARNING numerotation des lignes pour DINAC
              move wegnlg to fjnlg3

DDE212* on n'ecrit pas le commentaire de creation du num de lancement
      * car on va en creer un autre
DD0012* micn 24/02/2003 on ne cree pas de lancement si reliquat donc on garde
      * tous les commentaires

DDE109        move "W" to gfkey                                         *GPICMT
DDE109        perform op-fcomjoc3                                       *GPICMT
      *%      WRITE FCOMJOUR COMPL
              perform w-fcomjoc3

      * SI RELIQUAT COMPLET = ON SUPPRIME LA LIGNE DE COMMANDE GPIWARNING
      * si creation invalide et cle double on sort des commentaires     *M0400
      * sans message d'erreur (commentaire une fois par reference)      *M0400
              if file-status = 22
                 go to t50-fin
999999   display "trace_cgcd-reli1 : IF no22"
              end-if

              if file-status not = zero
                 string 'Ecriture FCOMJOC3 invalide ' fjcle3
                        ' st: (' file-status ')'
                       delimited by size into cgcd-reli-liberr
                 move '3' to ocgcd-reli-rtn
                 perform cl-fcomjoc3
                 go to t130
              else
                 perform cl-fcomjoc3
999999   display "trace_cgcd-reli1 : IF no23"
              end-if
          .
           if wsupcd not = 1            go to t50.
           perform d-fcommac3.
           GO TO T50.
DDE109 t50-fin.
999999   display "Trace-cgcd-reli1 : t50-fin".
           perform cl-fcommac3
           go to t40
           .
       t55.
999999   display "Trace-cgcd-reli1 : t55".
GPICMT* changement GPIMR pour ne pas mettre a jour la dataware avec le gf
DD0444     move mmdt-mr to wmmdt-mr
DD0044     move 'DATAW' to mmdt-mr
999999   display "cgcd-reli1 fcqtl avec ecriture (2) " fcqtl
           perform rw-fcommac2.
           if file-status not = zero
DD0444        move wmmdt-mr to mmdt-mr
              go to errmaj.
DD0444     move wmmdt-mr to mmdt-mr
GPICMT* maj livraison de la ligne dans la dataware
DD0444     perform liv-ligne
           go to t40.
------* SANS RELIQUAT
       t57.
999999   display "Trace-cgcd-reli1 : t57".
           if fcqtl = zero  move fcqtc to fcqtl.
GPICMT* si on est une ligne de composant de fictif, on ne destock pas
           if fcfictif not = cmmut-fict-fictif-cpst
              perform f10 thru fin 
              add fcqtl to wquanp 
999999   display "trace_cgcd-reli1 : IF no24"
           end-if
           go to t55.
      *** ELTS 4 - TOUT EN RELIQUAT
       T70.
999999   display "Trace-cgcd-reli1 : T70".
      *    IF FCNEL = 4 AND FCLIG < 9  GO TO T135.
DDE125     MOVE wor-fcommac22 to jwor-fcommac22.
DDE125     MOVE cgcd-reli-indr TO jfcnin2.
GPICMT* on ne cumul pas les qtes cde des fictifs composants
DD0459     if jfcfictif not = cmmut-fict-fictif-cpst   
DDE125        ADD jfcqtc TO WQUAN 
999999   display "trace_cgcd-reli1 : IF no25"
           end-if
DDE125     MOVE ZERO TO jfcnlg.

DDE069*GPIWARNING numerotation des lignes pour DINAC
              add 10 to wegnlg
DDE125        move wegnlg to jfcnlg
DD0712* Conservation des no de ligne si OpenBook => pour coherence avec OM1
DD0712     if fcopenbook = 1 
             move fcnlg to jfcnlg
           else

DDE125     MOVE SPACES TO jfc1 jfc2 jfccres.
      *%   DELETE FCOMMDES.
           perform d-fcommac2.

GPICMT* appel suppression dans dataware
DD0351     move fcnoc2 to icmcd-ware-cdex                               *GPICMT
  -        move FCDCD to icmcd-ware-datecre                             *GPICMT
  -        move fcnlg  to icmcd-ware-ligne                              *GPICMT
  -        if fctref = "A"                                              *GPICMT
  -           move 01 to icmcd-ware-niveau                              *GPICMT
  -        else                                                         *GPICMT
  -           move 00 to icmcd-ware-niveau                              *GPICMT
999999   display "trace_cgcd-reli1 : IF no26"
  -        end-if                                                       *GPICMT
  -        move fcnar to icmcd-ware-ref                                 *GPICMT
  -        move fcsre to icmcd-ware-sref                                *GPICMT
999999   display "trace_cgcd-reli1 : call cmcd-ware1"
DD0351     call "cmcd-ware1" using cmcd-ware adl-art                    *GPICMT


      * si tarif catalogue (fjtopx = "") on met 1 (pric net) ds fctopx pour
      * eviter un recalcul du prix en maj de commande
DDE125     if jfctopx = space move "1" to jfctopx.

      *%   WRITE FCOMJOUR COMPL.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcomjoc2.                                         *GPICMT
           perform w-fcomjoc2.
           if file-status not = zero
DDE125        string 'Ecriture FCOMJOC2 invalide ' jfccle2
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcomjoc2
              go to t130
           else
              perform cl-fcomjoc2
999999   display "trace_cgcd-reli1 : IF no27"
           end-if
          .
      *    MOVE WQTC TO FCQTC.                  ?????????????
           MOVE 9 TO WRELIQ.
           PERFORM F200 THRU FIN.
           move 1 to wsupcd.
           go to t47.
       t80.
999999   display "Trace-cgcd-reli1 : t80".
      **  ELEMENTS 5 A 10
           move zero to fcnel4 fcunix4.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcommac4.                                         *GPICMT
           perform snl-fcommac4.
           if file-status not = zero  go to t110.
       t85.
999999   display "Trace-cgcd-reli1 : t85".
           perform nnl-fcommac4.
           if file-status not = zero  go to t110.
           if fcnum4 not = cgcd-reli-cde or
              fcnin4 not = cgcd-reli-ind  go to t110.
      * pas de reliquat ==> validation des lignes montant qte cdee dans qte liv
           if cgcd-reli-rel not = "R" go to t100.

      * reliquat ==> meme trt pour les elements autres que 7 (% remise) et
      *                                                   10 (libelle) on ne
      *              fait que la validation des lignes
      * element remise ==> creation de l'element dans le reliquat puis
      *                    validation de la ligne
           if fcnel4 = 10 or fcnel4 = 7 go to t87.
      * trt ligne montant ou ligne marquage par exemple
      * trt validation normal de la ligne
           if fcde1 = " " go to t100.

      * trt ligne rupture complete
           if fcqul = zero and fcde1 = "0" go to t85.
      * trt ligne rupture partielle sans reliquat
           if fcde1 = "0" go to t100.

      * trt rupture avec reliquat
           go to t90.
       t87.
999999   display "Trace-cgcd-reli1 : t87".
           if fcnel4 not = 10   go to t90.
      * element 10 libelle ==> si autre que les 2 libelles suivant on les crees
      * en reliquat puis trt validation sinon on ne fait que la validation
           move fcdop to wldop.
           if fcdop = "AUTRES COMMANDES EXPEDIEES" go to t100.
           if wldop  = "EXPEDIEE AVEC COMMANDE"    go to t100.

aaaaaa t90.
999999   display "Trace-cgcd-reli1 : t90".
           move wor-fcommac42 to wor-fcomjoc4.
           move cgcd-reli-indr to fjnin4.
           move zero  to fjnli.
           move spaces to fjde1 fjde2.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-fcomjoc4.                                         *GPICMT
           perform w-fcomjoc4.
           if file-status not = zero
              string 'Ecriture FCOMJOC4 invalide ' fjcle4
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcomjoc4
              go to t130
           else
              perform cl-fcomjoc4
999999   display "trace_cgcd-reli1 : IF no28"
           end-if
          .
           if fcnel4 = 7 or fcnel4 = 10 go to t100.
           if fcqul not = zero go t85.
      * ligne complete en reliquat
           perform d-fcommac4.

GPICMT* appel suppression dans dataware
DD0351     move spaces to tcmcd-ware-ecran1
  -        move fcnoc4 to icmcd-ware-cdex                               *GPICMT
  -        move FCDCD to icmcd-ware-datecre                             *GPICMT
DD0153     string fcnel4(2:1) fcunix4
                   delimited size into icmcd-ware-ligne
999999   display "trace_cgcd-reli1 : call cmcd-ware1"
DD0351     call "cmcd-ware1" using cmcd-ware adl-art                    *GPICMT

           go to t85.

      * validation ligne normal et partiellement livree
       t100.
999999   display "Trace-cgcd-reli1 : t100".
           if fcqul not = zero  go to t85.
           move fcqui to fcqul.
           perform rw-fcommac4.
           if file-status not = zero
              string 'Reecriture FCOMMAC4 invalide ' fccle4
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-fcommac4
              go to t130
999999   display "trace_cgcd-reli1 : IF no29"
           end-if
          .
           go to t85.

       t110.
999999   display "Trace-cgcd-reli1 : t110".
DDE109     perform cl-fcommac4
      * fin trt lignes de la commande
      * si creation reliquat ==> maj nbre de boites reliquat ds entete reliquat
           if cgcd-reli-rel not = "R" go to t125.

           MOVE cgcd-reli-cder TO FJNUM.
           MOVE cgcd-reli-indr TO FJNIN.
      *****READ FCOMJOUR INVALID KEY INVA1 ON LOCK PREEMP1.
DDE109     move "W" to gfkey.
DDE109     perform op-fcomjoap.
           perform r-fcomjoap.
           if file-status not = zero    go to INVA1.
           MOVE WQUAN TO FJTQU.
      *%   REWRITE FCOMJOUR.
           if fcopenbook > 0
      * on bloc le reliquat en modif tant que la commande mere n'a pas ete factu
             move 9 to fjopenbook
999999   display "trace_cgcd-reli1 : IF no30"
           end-if
           perform rw-fcomjoap.
           if file-status not = zero  go to inva1.
DDE109     perform cl-fcomjoap
           .

      * maj nbre de boites livrees, piege pour industrie et code cde preparee
      * ds entete de commande livree
       T125.
999999   display "Trace-cgcd-reli1 : T125".
           MOVE cgcd-reli-cde TO FCNUM.
           MOVE cgcd-reli-ind TO FCNIN.
      *****READ FCOMMDES INVALID KEY INVA2 ON LOCK PREEMP2.
           perform r-fcommaap.
           if file-status not = zero     go to INVA2.
           move 1 to fcliv.
      *    move wmmau-cdes-etliv to fcliv.
      *
      **** si fcfac = 3 on met a jour les codes pour que la commande ne passe
      **** pas en facturation et soit epuree cas des commandes industrie a
      **** valeur zero
      *
      **** lignes suivantes inactives ne concernent que l'industrie
     *DDE069 on reactive les lignes suivantes pour l'industrie
DD0351* GPIWARNING on annule les lignes suivantes, toutes les commandes doivent 
DD0351*    IF mmdt-secteur not = 'GPU'
  -   *       if fcfac = 3
  -   *          move 2 to fcfac
  -   *          move 2 to fcliv
  -   *          move 1 to fcafa
  -   *          move 1 to fcdi1
  -   *       end-if
DD0351*    END-IF.

      * piege des commandes industrie (probleme de facturat. separee GPIWARNING)
DD0516* pieger les commandes se fait maintenant a la creation des commandes
DD0516*    IF FCTVE > 199 AND FCTVE < 300  MOVE 2 TO FCICP.
DD0516*    IF FCTVE > 399 AND FCTVE < 500  MOVE 2 TO FCICP.
DD0516*    IF FCTVE > 699 AND FCTVE < 900  MOVE 2 TO FCICP.
DD0351* on ne piege pas les commandes pour la slovaquie
  -        if mmdt-societe = 'SLOVAQ' or = "CHINE"
  -           move zero to FCICP
999999   display "trace_cgcd-reli1 : IF no31"
DD0351     end-if

      * maj total boites cdees et livrees
           SUBTRACT WQUAN FROM FCTQU.
           MOVE wquanp TO FCTQL.

      * maj code reliquat
           if cgcd-reli-rel = "R" 
             move "R" to fcod1
      * Si SOS, on incremente l'etat du flag si reliquat
             if fcopenbook not = 0
DD0712         move 2 to fcopenbook
999999   display "trace_cgcd-reli1 : IF no32"
             end-if
999999   display "trace_cgcd-reli1 : IF no33"
           end-if
           perform rw-fcommaap.
           if file-status not = zero  go to inva2.

GPICMT* si commande cree aujourd'hui on appelle la mise a jour dans la dataware
GPICMT* car probleme si regroupement de beaucoup de commandes et que certaines c
GPICMT* il faut recommancer plusieurs fois le transport
GPICMT* a mettre en place quand on pourra creer un repertoire en partage entre l
GPICMT* sinon il faudrait ajouter l'acces a la dataware pour chaque utilisateur 
GPICMT* il faudra alors modifier cmex-stat1 et cmcd-ware2 pour donner le nouveau
DD0351     if mmdt-societe = 'GPI'
DD0525                  or = 'GERGONNE'
  -           if fcjou = wjj and fcmoi = wmm and fcann = waa
  -              perform maj-dataware
999999   display "trace_cgcd-reli1 : IF no34"
  -           end-if
999999   display "trace_cgcd-reli1 : IF no35"
DD0351     end-if

GPICMT* mise a jour nombre de commande fille preparee pour commande allotie
DD0298     if fcfeo = ccmpa-tycd-typ-allotie-fille(1:1)                 *GPICMT
  -           perform maj-mere
999999   display "trace_cgcd-reli1 : IF no36"
DD0298     end-if

      *    creation element trace fin de preparation
DDE069*    creation trace par fonction
           move space to wmmtr-trac
           move 'F' to immtr-trac-action
           move wttecpt to immtr-trac-cpt
           move cgcd-reli-rel to immtr-trac-rup
           string wcde delimited by size into immtr-trac-num
           string "FIN DU DESTOCKAGE"
                  delimited by size into immtr-trac-commentaire
           perform cre-trace

           move "O" to wedit.
DDE275*  mise a jour fichier comptage des cdes et des lignes
           move fcncl to cp-cli
           move waa   to cp-datpaa
           move wmm   to cp-datpmm
           move wjj   to cp-datpjj
           perform r-comptage
           if file-status not = zero
              initialize wor-comptage
              move fcncl to cp-cli
              move waa   to cp-datpaa
              move wmm   to cp-datpmm
              move wjj   to cp-datpjj
              if fcfoa < 5
                 move wligcde to cp-ligcde
                 move 1 to cp-nbcde
                 move wligrupt to cp-ligrupt
                 move wligruptrel to cp-ligruptrel
                 if fcqsa = "AUTO" or fcqsa = "EDI "
                    move 1 to cp-nbedi
                    move wligedi to cp-ligedi
                    move wligedirupt to cp-ligedirupt
                    move wligediruptrel to cp-ligediruptrel
999999   display "trace_cgcd-reli1 : IF no37"
                 end-if
              else
                 move wligavoir to cp-ligavoir
                 move 1 to cp-nbavoir
999999   display "trace_cgcd-reli1 : IF no38"
              end-if
              perform w-comptage
           else
              if fcfoa < 5
                 add wligcde to cp-ligcde
                 add 1 to cp-nbcde
                 add wligrupt to cp-ligrupt
                 add wligruptrel to cp-ligruptrel
                 if fcqsa = "AUTO" or fcqsa = "EDI "
                    add 1 to cp-nbedi
                    add wligedi to cp-ligedi
                    add wligedirupt to cp-ligedirupt
                    add wligediruptrel to cp-ligediruptrel
999999   display "trace_cgcd-reli1 : IF no39"
                 end-if
              else
                 add wligavoir to cp-ligavoir
                 add 1 to cp-nbavoir
999999   display "trace_cgcd-reli1 : IF no40"
              end-if
              perform rw-comptage
999999   display "trace_cgcd-reli1 : IF no41"
           end-if

GPICMT* on regarde si on a des lancements associes a la commande si oui on les s

DD0351     move 1 to imicl-lect-raz
DD0351     move cmicl-lect-trt-c to imicl-lect-trt
DD0351     move FCCLE to  imicl-lect-cdex
           move spaces to imicl-lect-reference
DD0351     move cmicl-lect-action-e to imicl-lect-action
DD0351     move cmmdt-envi-rtn-ok to omicl-lect-rtn
999999   display "trace_cgcd-reli1 : call 'micl-lect1'"
DD0351     call 'micl-lect1' using micl-lect adl-art
           if omicl-lect-rtn not = cmmdt-envi-rtn-ok
              go to solde-fin
999999   display "trace_cgcd-reli1 : IF no42"
           end-if

GPICMT* si le lancement regroupe plusieurs commandes, on ne fait rien           
           move lcl-numlanc to cl-numlanc
           move spaces to cl-reference
           move zero to cl-unix
           perform snl-cdeslanc
           if file-status not = zero 
              go to solde-fin
999999   display "trace_cgcd-reli1 : IF no43"
           end-if
           .
       lect-cdeslanc.
999999   display "Trace-cgcd-reli1 : lect-cdeslanc".
           perform nnl-cdeslanc
           if file-status = zero and cl-numlanc = lcl-numlanc 
              if fccle not = cl-cdeind
                 go to solde-fin
999999   display "trace_cgcd-reli1 : IF no44"
              end-if
              go to lect-cdeslanc
999999   display "trace_cgcd-reli1 : IF no45"
           end-if

GPICMT* sinon on solde le lancement (appel en arriere plan)
           move "M" to ifab30-e1action
           move 3 to ifab30-direct
           move 00                to ifab30-e1niv
           move spaces            to ifab30-e1ref
           move spaces            to ifab30-e1sref
           move zero              to ifab30-e1serie
           move zero              to ifab30-e8jjf
           move zero              to ifab30-e8mmf
           move zero              to ifab30-e8aaf
           move zero              to ifab30-e1client
           move "S"               to ifab30-e9action
           move lcl-numlanc       to ifab30-e9fab
999999   display "trace_cgcd-reli1 : call 'fabzz'"
GPICMT     call 'fabzz' using wfab30 adl-art
           .
       solde-fin.
999999   display "Trace-cgcd-reli1 : solde-fin".



DDE212* mise a jour lancement pour cde en reliquat
           if cgcd-reli-rel = "R"
DD9999        initialize wcmcd-clan
              move ccmcd-clan-reliq to icmcd-clan-reliq
      * cde initiale pour solder ou annuler le lanc.
              move cgcd-reli-cde to icmcd-clan-relcde
              move cgcd-reli-ind to icmcd-clan-relind
      * cde reliquat pour creation lancement
              move cgcd-reli-cder to icmcd-clan-cde
              move cgcd-reli-indr to icmcd-clan-ind
              move ccmcd-clan-trt-c to icmcd-clan-trt
              move ccmcd-clan-lect-j to icmcd-clan-lect
              move ccmcd-clan-act-c to icmcd-clan-action
999999   display "trace_cgcd-reli1 : call 'cmcd-clanz'"
DD0035        call 'cmcd-clanz' using cmcd-clan adl-art
999999   display "trace_cgcd-reli1 : IF no46"
           end-if
FFFFFF*
         .
      * SORTIE DU PROGRAMME
       T130.
999999   display "Trace-cgcd-reli1 : T130".
           commit.
      *%   ENDMR.
      *DDE069 appel fermeture des fichiers
           perform ferm

           exit program.
      *
      *
      *
PPPPPP******************************************************************
      *   MAJ ENTREES-SORTIES FARTUSIN/CONSOART - MAJ QTES SUIVICDE
PPPPPP******************************************************************
      *
       F10.
999999   display "Trace-cgcd-reli1 : F10".
           IF FCFOA = 9  GO TO FIN.
DD0131* GPICMT controle lieu expedition IND ==> pas destockage
           move fclli to immdt-lieu-corlieu
           move "l"   to immdt-lieu-trt
999999   display "trace_cgcd-reli1 : call mmdt-lieu1"
           call "mmdt-lieu1" using wmmdt-lieu adl-art
           perform rech-type-lieu

           MOVE FCDLR  TO FANMA fanma1.
           MOVE FCNARX TO FANARX fanar1x.
      *****READ FARTUSIN INVALID KEY F208.
           perform rnl-fartusap.
           if file-status not = zero   go to F208.
           MOVE wor-fartusap TO wwor-fartusap2

      * calcul reel de la qte a destocker suite au pb des boites incompletes
      * saises en modifiant la qte par boite sur la ligne de commande
           move fanma to imgca-dest-nma.
           move fanarx to imgca-dest-nar.
           move fcqtl to imgca-dest-qte.
           move fcqpb to imgca-dest-qpb.
999999   display "trace_cgcd-reli1 : call mgca-dest1"
           call "mgca-dest1" using wmgca-dest adl-art.
           if omgca-dest-rtn not = spaces
              display "LIGNE: " fcnlg
              display "NON DESTOCKEE"
DD0351        display omgca-dest-err
              move omgca-dest-err to cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              go to t130.
      * la qte a destocker se trouve alors dans omgca-dest-qtr
           .
       F15.
999999   display "Trace-cgcd-reli1 : F15".
      *****READ FARTUSIN COMPL AT END F208.
           move fcsre to fansr1x.
           perform r-fartusac.
           if file-status not = zero  go to F208.
           MOVE wor-fartusac TO wwor-fartusac2.
      *
      **** CALCUL DU MONTANT
       F20.
999999   display "Trace-cgcd-reli1 : F20".
           MOVE ZERO TO WMONT.
           IF FCFOA = 1 OR FCFOA = 6  GO TO F60.

      *DDE069 remplacement calcul montant ligne par une fonction
           move fcqpb to tcgca-mtht-qpb
           move fcqtl to tcgca-mtht-qte
           move fcmes  to tcgca-mtht-mes
           move fcprx  to tcgca-mtht-prx
           move fcpht  to tcgca-mtht-pht
999999   display "trace_cgcd-reli1 : call 'cgca-mtht2'"
           call 'cgca-mtht2' using cgca-mtht adl-art
           move tcgca-mtht-htl to wmont.

      *    IF TREM = ZERO  GO TO F60.
      *
      *----> DDE064 (D)
      * si remises globales detectees par cmta-comi3 ==> appel cmta-comi5 pour
      * calculer le montant reel a passer en stat (remise deduite)
           if wcmta-comi-remx = spaces go to f60.
           move fcnarx to wcmta-comi-narx.
           move fcsrex to wcmta-comi-nsrx.
           move wmont to wcmta-comi-resu.
999999   display "trace_cgcd-reli1 : call cmta-comi5"
           call "cmta-comi5" using wcmta-comi adl-art.
           if wcmta-comi-rtn = "0"
              move wcmta-comi-resu to wmont.
      *----> DDE064 (F)

      *
      **** MAJ SORTIES ARTICLES (MAG.LIVREUR)
       F60.
999999   display "Trace-cgcd-reli1 : F60".

GPICMT* initialisation parametre pour destockage automatique
DD0380     initialize wfgar-spec
      *
DDE089**** appel fonction conversion des devises pour mettre le montant dans
      *    la devise de base
           move fcdev to weu-adev
           move 99    to weu-ndev
           move wmont  to weu-pht
999999   display "trace_cgcd-reli1 : call 'mmca-devi1'"
           call 'mmca-devi1' using wmmca-devi adl-art
           if weu-err = spaces
              move weu-pht to wmont
999999   display "trace_cgcd-reli1 : IF no47"
           end-if.

           IF FCFOA > 4  GO TO F70.

DD0131     if ommlx-lect-rtn = cmmdt-envi-rtn-ok and
              xli-secteur = 'I'
              if fcfoa not = 1
                 add wmont to favvh
DD0380                        ifgar-spec-mtligne
999999   display "trace_cgcd-reli1 : IF no48"
              end-if
              go to f80
999999   display "trace_cgcd-reli1 : IF no49"
           end-if

      *    IF FCFOA NOT = 2  ADD FCQTL TO FASOC FASOP.
           IF FCFOA NOT = 2  ADD omgca-dest-qtr TO FASOC FASOP
DD0380                                             ifgar-spec-sortie
999999   display "trace_cgcd-reli1 : IF no50"
           END-IF
           IF FCFOA NOT = 1  ADD WMONT TO FAVVH
DD0380                                    ifgar-spec-mtligne
999999   display "trace_cgcd-reli1 : IF no51"
           END-IF
           GO TO F80.
       F70.
999999   display "Trace-cgcd-reli1 : F70".

DD0131     if ommlx-lect-rtn = cmmdt-envi-rtn-ok and
              xli-secteur = 'I'
              if fcfoa not = 6
                 subtract wmont from favvh
DD0380                               ifgar-spec-mtligne
999999   display "trace_cgcd-reli1 : IF no52"
              end-if
              go to f80
999999   display "trace_cgcd-reli1 : IF no53"
           end-if

      *    IF FCFOA NOT = 7  SUBTRACT FCQTL FROM FASOC FASOP.
DDE295* on ne remet pas en stock les articles pour un avoir normal ou un*GPICMT
DDE295* avoir comptable seulement pour un avoir conditionnel            *GPICMT
DDE295*    IF FCFOA NOT = 7  SUBTRACT omgca-dest-qtr FROM FASOC FASOP.
DDE295     IF FCFOA NOT = 7  and not = 5                                *GPICMT
              SUBTRACT omgca-dest-qtr FROM FASOC FASOP                  *GPICMT
DD0380                                     ifgar-spec-sortie            *GPICMT
999999   display "trace_cgcd-reli1 : IF no54"
           END-IF
           IF FCFOA NOT = 6  SUBTRACT WMONT  FROM FAVVH
DD0380                                            ifgar-spec-mtligne    *GPICMT
999999   display "trace_cgcd-reli1 : IF no55"
           END-IF
           .
       F80.
999999   display "Trace-cgcd-reli1 : F80".
      *%   REWRITE FARTUSIN.
      *M1096 ------------------------------------------------------------------
DD0351     if mmdt-societe not = 'GPI'
DD0525                 and not = 'GERGONNE'
  -           go to f81
999999   display "trace_cgcd-reli1 : IF no56"
DD0351     end-if
           if fasufa not = 2 go to f81.
           if fasoc < zero move zero to fasoc.
           if fasop < zero move zero to fasop.
           if fasoc > faenc move fasoc to faenc.
           if fasop > faenp move fasop to faenp.
       f81.
999999   display "Trace-cgcd-reli1 : f81".
      *M1096 ------------------------------------------------------------------
DDE314* l'article est mouvemente donc annulation non autorise
           move 1 to favannul
DD0380               ifgar-spec-codannul

GPICMT* si erels on ne met pas a jour l'article, on appellera une fonction speci
GPICMT* apres gestion des consos
DD0380     if mmdt-societe = 'ERELS'
  -           go to f100
999999   display "trace_cgcd-reli1 : IF no57"
DD0380     end-if
           perform rw-fartusac.
           if file-status not = zero  go to errma.
      *
      **** MAJ CONSOMMATIONS (MAG.LIVREUR)
       F100.
999999   display "Trace-cgcd-reli1 : F100".
DD0019* GPIWARNING pour DINAC si depot livre 31 ne pas mettre a jour les consos
      *            du magasin livreur (cas transfert de stock nlle usine)
           if mmdt-societe = 'DINAC'
              if fcdle = 31
                 go to f140
999999   display "trace_cgcd-reli1 : IF no58"
              end-if
999999   display "trace_cgcd-reli1 : IF no59"
           end-if

DD0131     if ommlx-lect-rtn = cmmdt-envi-rtn-ok and
              xli-secteur = 'I'
              go to f140
999999   display "trace_cgcd-reli1 : IF no60"
           end-if


           MOVE FCDLR  TO CACNA.
           MOVE FCNARX TO CAPROX.
           MOVE FCSREX TO CAREFX.
      *****READ CONSOART INVALID KEY F320.
           perform r-consoart.
           if file-status not = zero    go to F320.
       F110.
999999   display "Trace-cgcd-reli1 : F110".
      *    IF FCFOA = 0 OR FCFOA = 1 ADD FCQTL TO CASMC CASCU
           IF FCFOA = 0 OR FCFOA = 1 ADD omgca-dest-qtr TO CASMC CASCU
DD0380                                           ifgar-spec-sortie-conso
                                     GO TO F120.
      *    IF FCFOA = 5 OR FCFOA = 6  SUBTRACT FCQTL FROM CASMC CASCU
DDE295* on ne soustrait pas des conso les articles pour un avoir normal *GPICMT
DDE295* ou un avoir comptable seulement pour un avoir conditionnel      *GPICMT
DDE295*    IF FCFOA = 5 OR FCFOA = 6  SUBTRACT omgca-dest-qtr
DDE295     IF FCFOA = 6  SUBTRACT omgca-dest-qtr FROM CASMC CASCU
DD0380                                           ifgar-spec-sortie-conso
                       GO TO F120.
           GO TO F140.
       F120.
999999   display "Trace-cgcd-reli1 : F120".
      *%   REWRITE CONSOART.
      *M1096 ------------------------------------------------------------------
DD0351     if mmdt-societe not = 'GPI'
DD0525                 and not = 'GERGONNE'
  -           go to f121
999999   display "trace_cgcd-reli1 : IF no61"
DD0351     end-if
           if fasufa not = 2 go to f121.
           if cascu < zero move zero to cascu.
           if casmc < zero move zero to casmc.
           if cascu > caecu move cascu to caecu.
           if casmc > caepe move casmc to caepe.
       f121.
999999   display "Trace-cgcd-reli1 : f121".
      *M1096 ------------------------------------------------------------------

GPICMT* si erels on ne met pas a jour consoart, on appellera une fonction specia
DD0380     if mmdt-societe = 'ERELS'
  -           go to f140
999999   display "trace_cgcd-reli1 : IF no62"
DD0380     end-if

           perform rw-consoart.
           if file-status not = zero  go to errmco.
      *
      **** MAJ ENTREES ARTICLES/CONSOMMATIONS (DEPOT LIVRE)
       F140.
999999   display "Trace-cgcd-reli1 : F140".

GPICMT* si erels on appelle une fonction speciale de maj
DD0380     if mmdt-societe = 'ERELS'
  -           perform maj-specif
999999   display "trace_cgcd-reli1 : IF no63"
DD0380     end-if
           IF FCDLE = ZERO  GO TO F200.                                 *M090102
      *----> M1097 (D)
           MOVE FCDLE  TO FANMA.                                        *M090102
           MOVE FCNARX TO FANARX.
           perform r-fartusap.
           if file-status = zero   go to F141.
           MOVE wwor-fartusap2 TO wor-fartusap
           MOVE FCDLE   TO FANMA                                        *M090102
           perform w-fartusap.
           if file-status not = zero go to ERRWAR.
       f141.
999999   display "Trace-cgcd-reli1 : f141".
      *----> M1097 (F)
           MOVE FCDLE  TO FANMA1 CACNA.                                 *M090102
           MOVE FCNARX TO fanar1x CAPROX.
           MOVE FCSREX TO CAREFX.
       F150.
999999   display "Trace-cgcd-reli1 : F150".
      *****READ FARTUSIN COMPL AT END F280.
           move fcsre to fansr1x.
           perform r-fartusac.
DDE999*    if file-status not = zero  go to F280.
DDE999     if file-status not = zero
              perform f260
              perform w-fartusac
              if file-status not = zero
                 go to ERRWAR
              else
                 go to f141
999999   display "trace_cgcd-reli1 : IF no64"
              end-if
999999   display "trace_cgcd-reli1 : IF no65"
           end-if
           .

       F155.
999999   display "Trace-cgcd-reli1 : F155".
      *****READ CONSOART INVALID KEY F330.
           perform r-consoart.
           if file-status not = zero
               go to F330.
       F160.
999999   display "Trace-cgcd-reli1 : F160".
      *    IF FCFOA < 5  ADD FCQTL TO FAENC FAENP CAECU CAEPE
      *            ELSE SUBTRACT FCQTL FROM FAENC FAENP CAECU CAEPE.
           IF FCFOA < 5  ADD omgca-dest-qtr TO FAENC FAENP CAECU CAEPE
                   ELSE SUBTRACT omgca-dest-qtr
                                       FROM FAENC FAENP CAECU CAEPE.
      *M1096 ------------------------------------------------------------------
DD0351     if mmdt-societe not = 'GPI'
DD0525                 and not = 'GERGONNE'
  -           go to f160f
999999   display "trace_cgcd-reli1 : IF no66"
DD0351     end-if
           if fasufa not = 2 go to f160f.
           if fasoc < zero move zero to fasoc.
           if fasop < zero move zero to fasop.
           if fasoc > faenc move fasoc to faenc.
           if fasop > faenp move fasop to faenp.
           if cascu < zero move zero to cascu.
           if casmc < zero move zero to casmc.
           if cascu > caecu move cascu to caecu.
           if casmc > caepe move casmc to caepe.
       f160f.
999999   display "Trace-cgcd-reli1 : f160f".
           exit.
      *M1096 ------------------------------------------------------------------
       F170.
999999   display "Trace-cgcd-reli1 : F170".
      *%   REWRITE CONSOART.
           perform rw-consoart.
           if file-status not = zero  go to errmco.
       F180.
999999   display "Trace-cgcd-reli1 : F180".
      *%   REWRITE FARTUSIN.
DDE314* l'article est mouvemente donc annulation non autorise
           move 1 to favannul

           perform rw-fartusac.
           if file-status not = zero  go to errma.
      *
PPPPPP**** MAJ SUIVICDE
       F200.
999999   display "Trace-cgcd-reli1 : F200".
           IF FCFOA > 1  GO TO F208.

DD0351* Si ligne de composant infocom => pas MAJ suivicde
           if fcfictif = '2'
              go to F208
999999   display "trace_cgcd-reli1 : IF no67"
           end-if

      * appel fconction  qui recherche si maj du suivi des commandes a faire
      * par rapport au lieu d'expedition
           move fcdlr to immdt-lieu-lieu.
DD0131*    move spaces  to immdt-lieu-trt
DD0350     move 'S' to immdt-lieu-trt
999999   display "trace_cgcd-reli1 : call mmdt-lieu1"
           call "mmdt-lieu1" using wmmdt-lieu adl-art.
           if ommdt-lieu-suicde = 0 go to f208.

      * maj suivi a faire
           MOVE FCDLR TO SUNMA.
           MOVE FCNARX TO SUNARX.
           MOVE FCSREX TO SUNSRX.
      *****READ SUIVICDE INVALID KEY F208.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-suivicde.                                         *GPICMT
           perform r-suivicde.
           if file-status not = zero
DDE109        perform cl-suivicde
              go to F208.

      * appel focntion recherche du numero de la semaine de livraison
           move fcaa to immti-sema-aa.
           move fcmm to immti-sema-mm.
           move fcjj to immti-sema-jj.
999999   display "trace_cgcd-reli1 : call mmti-sema1"
           call "mmti-sema1" using wmmti-sema adl-art.
           if ommti-sema-err not = spaces move zero to tsem
             else                         move ommti-sema-sem to tsem.

           IF TSEM = ZERO  GO TO F206.
      *
           MOVE FCQTC TO WQT.
           IF WQT NOT > SUSEM (TSEM)  GO TO F204.
           IF SUCG1 NOT = "*"         GO TO F203.
           SUBTRACT SUSEM (TSEM) FROM WQT GIVING WREST.
           IF WREST > SUSEM (1)        GO TO F203F.
           SUBTRACT WREST FROM SUSEM (1).
           MOVE ZERO TO SUSEM (TSEM).
           SUBTRACT WQT FROM SUTOT.
           IF SUSEM (1) = ZERO  MOVE SPACE TO SUCG1.
           GO TO F205.
       F203.
999999   display "Trace-cgcd-reli1 : F203".
           MOVE SUSEM (TSEM) TO WQT.
           IF SUCG2 = "3"  MOVE "X" TO SUCG2  GO TO F204.
           IF SUCG2 = "6"  MOVE "Y" TO SUCG2  GO TO F204.
           IF SUCG2 = "9"  MOVE "Z" TO SUCG2  GO TO F204.
           if sucg2 = space MOVE "C" TO SUCG2
             else           move "W" to sucg2.
           GO TO F204.
       F203F.
999999   display "Trace-cgcd-reli1 : F203F".
           MOVE SUSEM (TSEM) TO WQT.
           MOVE "B" TO SUCG2.
       F204.
999999   display "Trace-cgcd-reli1 : F204".
           SUBTRACT WQT FROM SUSEM (TSEM) SUTOT.
       F205.
999999   display "Trace-cgcd-reli1 : F205".
           IF SUTOT NOT > ZERO  MOVE zero      TO SUZON
                             MOVE SPACE TO SUCG1 SUCG2.
           GO TO F207.
      *
       F206.
999999   display "Trace-cgcd-reli1 : F206".
           IF FCQTC > SUSEM (1) GO TO F206F.
           SUBTRACT FCQTC FROM SUSEM (1)  SUTOT.
           IF SUSEM (1) = ZERO  MOVE SPACE TO SUCG1.
           GO TO F207.
       F206F.
999999   display "Trace-cgcd-reli1 : F206F".
           SUBTRACT SUSEM (1) FROM SUTOT.
           MOVE ZERO TO SUSEM (1).
           MOVE "A"  TO SUCG2.
      *
       F207.
999999   display "Trace-cgcd-reli1 : F207".
      *%   REWRITE SUIVICDE.
           perform rw-suivicde.
           if file-status not = zero
              string 'Reecriture SUIVICDE invalide ' sucle
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-suivicde
              go to t130
           else
              perform cl-suivicde
999999   display "trace_cgcd-reli1 : IF no68"
           end-if
          .
       F208.
999999   display "Trace-cgcd-reli1 : F208".
           MOVE 1 TO FCGE1.
      *
      **** MAJ RESERVATIONS DEVIS (SAUF SI WRELIQ=9): - QTE CDEE
      ****  + QTE RELIQ. SI WRELIQ=1
      **** sauf si code prepare > 2 (deja maj dans PREP)
       F208D.
999999   display "Trace-cgcd-reli1 : F208D".
DD0031* tester si article ligne est un devis
           if fctref not = 'D'
              go to fin
999999   display "trace_cgcd-reli1 : IF no69"
           end-if

DD0073* ajout maj stock devis picking
           move spaces to ifidv-mjqt-s ifidv-mjqt-f ifidv-mjqt-t
                          ifidv-mjqt-r ifidv-mjqt-c
           move fcfoa        to ifidv-mjqt-foa
           move cfidv-mjqt-s to ifidv-mjqt-s
           move cfidv-mjqt-t to ifidv-mjqt-t
           move spaces       to ifidv-mjqt-code-r
           move fcnar       to ifidv-mjqt-ref
           move fcsre       to ifidv-mjqt-sref
           move fcqtl       to ifidv-mjqt-q-ajout
           move zero         to ifidv-mjqt-q-suppr
           move fcnum2      to ifidv-mjqt-cde
           move fcnin2      to ifidv-mjqt-ind
999999   display "trace_cgcd-reli1 : call 'fidv-mjqt1'"
           call 'fidv-mjqt1' using fidv-mjqt adl-art
           if ofidv-mjqt-rtn not = cmmdt-envi-rtn-ok
              move cmmlp-mail-type-ss to immlp-mail-type(1)
              string "ECHEC "
                    delimited by size into immlp-mail-data(1)
              move cmmlp-mail-type-s to immlp-mail-type(2)
              move fcqtl to wqtlap
              string "Mise a jour Qte Livree " wqtlap
                 "pour le devis " fcnar " " fcsre
                    delimited by size into immlp-mail-data(2)
               move cmmlp-mail-type-s to immlp-mail-type(3)
               string "Commande : " fccle
                    delimited by size into immlp-mail-data(3)
               move cmmlp-mail-trt-notif to immlp-mail-trt

               move spaces to immlp-mail-objet
               string "ECHEC MAJ DESTOCKAGE DEVIS "
                 fcnar " " fcsre
                   delimited by size into immlp-mail-objet
               move spaces to immlp-mail-destg
               move spaces to immlp-mail-groupe
DD0351*        move "door elgu" to immlp-mail-dest
DD0351         move "anes elgu"   to immlp-mail-dest
999999   display "trace_cgcd-reli1 : call 'mmlp-mail1'"
               call 'mmlp-mail1' using mmlp-mail adl-art
999999   display "trace_cgcd-reli1 : IF no70"
           end-if

           if fccres = "1" and wreliq not = 1 go to fin.
      *    if wprep > 2   and wreliq not = 1 go to fin.
           IF WRELIQ = 9  GO TO FIN.
           IF FCFOA > 1    GO TO FIN.
           MOVE FCNARX TO NDNAR ADNAR1.
           MOVE FCSREX TO NDSRF ADSRF1.
      *****READ NUMDEVIS INVALID KEY FIN NO LOCK.
           perform rnl-numdevis.
           if file-status not = zero
               go to FIN.
           MOVE NDNCL TO ADNCL1.
      *****READ ARTDEVIS INVALID KEY FIN.
       F208F.
999999   display "Trace-cgcd-reli1 : F208F".
      *****READ ARTDEVIS COMPL AT END FIN.
           move  2   to adtye1.
           move zero to adunix1.
DDE109     move "W" to gfkey.                                           *GPICMT
DDE109     perform op-artdevc1.                                         *GPICMT
           perform r-artdevc1.
           if file-status not = zero
DDE109        perform cl-artdevc1
              go to FIN.
           if fccres = "1" go to f208g.
           IF FCQTC NOT > ADREC  SUBTRACT FCQTC FROM ADREC
                           ELSE  MOVE ZERO TO ADREC.
           move "1" to fccres.
       f208g.
999999   display "Trace-cgcd-reli1 : f208g".
           IF WRELIQ NOT = 1  GO TO F208J.
           IF FCQTL > FCQTC   GO TO F208J.
           SUBTRACT FCQTL FROM FCQTC GIVING WQT.
           ADD WQT TO ADREC.
       F208J.
999999   display "Trace-cgcd-reli1 : F208J".
      *%   REWRITE ARTDEVIS.
           perform rw-artdevc1.
           if file-status not = zero
              string 'Reecriture ARTDEVC1 invalide ' adcle1
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr
              move '3' to ocgcd-reli-rtn
              perform cl-artdevc1
              go to t130
           else
              perform cl-artdevc1
999999   display "trace_cgcd-reli1 : IF no71"
           end-if
          .
      *
       FIN.
999999   display "Trace-cgcd-reli1 : FIN".
           EXIT.
      *
      *
      **** RECHERCHE ARTICLE DANS MAGASIN 01 POUR CREATION DEPOT LIVRE
       F210.
999999   display "Trace-cgcd-reli1 : F210".
           IF FCDLR = 1  GO TO F230.
           MOVE  1   TO FANMA fanma1.
           MOVE FCNARX TO FANARX fanar1x.
      *****READ FARTUSIN INVALID KEY F200.
           perform r-fartusap.
           if file-status not = zero  go to F200.
           MOVE wor-fartusap TO wwor-fartusap2
           .
       F220.
999999   display "Trace-cgcd-reli1 : F220".
      *****READ FARTUSIN COMPL AT END F200.
           move fcsre to fansr1x.
           perform r-fartusac.
           if file-status not = zero   go to F200.
      *    IF FANSRX NOT = FCSRE  GO TO F220.
           MOVE wor-fartusac TO wwor-fartusac2.
      *
      **** CREATION EP/EC DU DEPOT ET CONSOMMATIONS
       F230.
999999   display "Trace-cgcd-reli1 : F230".
           MOVE wwor-fartusap2 TO wor-fartusap
DDE999     MOVE FCDLE   TO FANMA WNMA
      *****WRITE FARTUSIN INVALID KEY ERRWAR.
           perform w-fartusap
           if file-status not = zero go to ERRWAR.
       F250.
999999   display "Trace-cgcd-reli1 : F250".
DDE197     MOVE spaces    TO wor-consoart.
DDE999     MOVE FCDLE  TO CACNA.
           MOVE FCNARX TO CAPROX.
           MOVE FCSREX TO CAREFX.
           MOVE ZERO TO CAECU CAEPE CASCU CASMC CAM24 CAM23 CAM22 CAM21
                  CAM20 CAM19 CAM18 CAM17 CAM16 CAM15 CAM14 CAM13 CAM12
                  CAM11 CAM10 CAM09 CAM08 CAM07 CAM06 CAM05 CAM04 CAM03
                  CAM02 CAM01 cazpr.
      *    MOVE ZWCD TO CADAT.
           accept cadat from date.
       F260.
999999   display "Trace-cgcd-reli1 : F260".
           MOVE wwor-fartusac2 TO wor-fartusac.
DDE999     MOVE FCDLE TO FANMA1.
           MOVE ZERO TO FAENC FAENP FASOC FASOP FAREC FAREP FACLA FAVVH.
           MOVE zero      TO FADAT fagei fadei.
           MOVE 1 TO FALOT.
DDE999 f261.
999999   display "Trace-cgcd-reli1 : f261".
      *M1096 ------------------------------------------------------------------
      *    PERFORM F160.
           PERFORM F160 thru f160f.
      *M1096 ------------------------------------------------------------------
       F270.
999999   display "Trace-cgcd-reli1 : F270".
      *%   WRITE FARTUSIN COMPL.
           perform w-fartusac.
           if file-status not = zero  GO TO ERRWAR.
           GO TO F300.
      *** CREATION EC SEUL
       F280.
999999   display "Trace-cgcd-reli1 : F280".
           go to f260.
      **** CONSOMMATIONS
       F300.
999999   display "Trace-cgcd-reli1 : F300".
      *****WRITE CONSOART INVALID KEY ERRWCO.
           perform w-consoart.
           if file-status not = zero
               go to ERRWCO.
           GO TO F200.
      *
      *
      **** CREATION CONSO (MAG.LIVREUR)
       F320.
999999   display "Trace-cgcd-reli1 : F320".
           PERFORM F250.
           MOVE FCDLR TO CACNA WNMA.
           IF FCFOA = 0 OR FCFOA = 1  ADD omgca-dest-qtr TO CASMC CASCU.
DDE295* on ne soustrait pas des conso les articles pour un avoir normal *GPICMT
DDE295* ou un avoir comptable seulement pour un avoir conditionnel      *GPICMT
DDE295     IF FCFOA = 6  SUBTRACT omgca-dest-qtr
                                                     FROM CASMC CASCU.
      *M1096 ------------------------------------------------------------------
           if casmc < zero move zero to casmc.
           if cascu < zero move zero to cascu.
      *M1096 ------------------------------------------------------------------
      *****WRITE CONSOART INVALID KEY ERRWCO.
           perform w-consoart.
           if file-status not = zero
               go to ERRWCO.
           GO TO F140.
      *
      *
      **** CREATION CONSO (DEPOT LIVRE)
       F330.
999999   display "Trace-cgcd-reli1 : F330".
           PERFORM F250.
           PERFORM F160 thru f160f.
DDE999     MOVE FCDLE TO WNMA.
      *****WRITE CONSOART INVALID KEY ERRWCO.
           perform w-consoart.
           if file-status not = zero
               go to ERRWCO.
           GO TO F180.
      *
       ERRWAR.
999999   display "Trace-cgcd-reli1 : ERRWAR".
           ADD 1 TO I.
           IF I > 15 MOVE "ERREURS SUIVANTES NON VISUALISEES" TO
                                                      cgcd-reli-liberr
                     move '3' to ocgcd-reli-rtn
                     GO TO F200.
           IF file-status = "24" MOVE "FICHIER ART. PLEIN" TO OLIB (I)
                               GO TO ERRF.
           IF file-status = "22" MOVE "DEJA CREE DANS FARTUSIN"
                               TO OLIB (I)  GO TO ERRF.
           MOVE "CREATION INVALIDE FARTUSIN" TO OLIB (I).
           GO TO ERRF.
       ERRWCO.
999999   display "Trace-cgcd-reli1 : ERRWCO".
           ADD 1 TO I.
           IF I > 15 MOVE "ERREURS SUIVANTES NON VISUALISEES" TO
                                                      cgcd-reli-liberr
                     move '3' to ocgcd-reli-rtn
                     GO TO F200.
           IF file-status = "24" MOVE "FICHIER CONSO PLEIN" TO OLIB (I)
                               GO TO ERRF.
           IF file-status = "22" MOVE "DEJA CREE DANS CONSOART"
                               TO OLIB (I)  GO TO ERRF.
           MOVE "CREATION INVALIDE CONSOART" TO OLIB (I).
       ERRF.
999999   display "Trace-cgcd-reli1 : ERRF".
           MOVE WNMA  TO ONMA (I).
           MOVE FCNARX TO ONAR (I).
           MOVE FCSREX TO OSRE (I).
           MOVE FCQTL TO OQTL (I).
           MOVE WMONT  TO OMON (I).
           IF FCFOA > 4  MOVE "-" TO OSIG (I).
           GO TO F200.
      *
      *
       ERRWR.
999999   display "Trace-cgcd-reli1 : ERRWR".
           MOVE "FICHIER FCOMJOUR PLEIN" TO cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           GO TO T130.
       errremise.
999999   display "Trace-cgcd-reli1 : errremise".
           move "TABLE REMISE TROP PETITE" to cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           go to t130.
       errtr.
999999   display "Trace-cgcd-reli1 : errtr".
           move "TRANSFERT PARTIELLEMENT EFFECTUE" to cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           go to t130.
       errmaj.
999999   display "Trace-cgcd-reli1 : errmaj".
MICN       string 'Reecriture FCOMMAC2 invalide ' fccle2 fcnel2
                     ' st: (' file-status ')'
                    delimited by size into cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           go to t130.
       errma.
999999   display "Trace-cgcd-reli1 : errma".
           move "ERREUR MAJ ARTICLE" to cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           go to t130.
       errmco.
999999   display "Trace-cgcd-reli1 : errmco".
           move "ERREUR MAJ CONSO"   to cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           go to t130.
       ERRWRC.
999999   display "Trace-cgcd-reli1 : ERRWRC".
           IF file-status = "24" MOVE "FICHIER FCOMJOUR PLEIN" TO
                                     cgcd-reli-liberr
           else
DD0351        IF file-status = "22" MOVE "COMMANDE RELIQUAT DEJA CREEE"
                                  TO cgcd-reli-liberr
              else
                 MOVE "CREATION COMMANDE INVALIDE" TO cgcd-reli-liberr
999999   display "trace_cgcd-reli1 : IF no72"
              end-if
999999   display "trace_cgcd-reli1 : IF no73"
           END-IF
           move '3' to ocgcd-reli-rtn
           perform cl-fcomjoap
           GO TO T130.
       INVA.
999999   display "Trace-cgcd-reli1 : INVA".
           MOVE "COMMANDE INEXISTANTE" TO cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           GO TO T130.
       invarel.
999999   display "Trace-cgcd-reli1 : invarel".
           move "CREATION RELIQUAT IMPOSSIBLE"  to cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           go to t130.
       INVA1.
999999   display "Trace-cgcd-reli1 : INVA1".
DDE109     perform cl-fcomjoap
           MOVE "MAJ COMMANDE CREEE IMPOSSIBLE" TO cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           GO TO T130.
       INVA2.
999999   display "Trace-cgcd-reli1 : INVA2".
           MOVE "MAJ COMMANDE IMPOSSIBLE" TO cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn
           GO TO T130.
       PRE.
999999   display "Trace-cgcd-reli1 : PRE".
           MOVE "COMMANDE PROVISOIREMENT INDISPONIBLE" TO
                                                   cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn.
      * Refus de l'automate : l'etat de la commande ne permet pas de la traiter
       ERR-AUTO.
999999   display "Trace-cgcd-reli1 : ERR-AUTO".
           move ommau-cdes-lib to cgcd-reli-liberr.
           move '3' to ocgcd-reli-rtn

      *DDE069 appel fermeture des fichiers
           perform ferm

           exit program.

      *===================================================================
      *                       FONCTIONS     LOCALES
      *===================================================================

      *---------------------
      * Ecriture de la trace
      *---------------------
       cre-trace section.
999999   display "Trace-cgcd-reli1 : cre-trace section".
           move "C"   to immtr-trac-type
           move wnom-prog to immtr-trac-prog
999999   display "trace_cgcd-reli1 : call 'mmtr-trac1'"
           call 'mmtr-trac1' using mmtr-trac adl-art
           .

      *DDE125 maj des ruptures
       rupture section.
999999   display "Trace-cgcd-reli1 : rupture section".
           move 'W' to gfkey
           perform op-ruptures
           .
       rup-cle.
999999   display "Trace-cgcd-reli1 : rup-cle".
DD0326     move cgcd-reli-cde to RU-CDEX-cdesup(1:7).
           move cgcd-reli-ind to ru-ind
DDE313* ajout du niveau de la référence                                 *GPICMT
GPICMT* si reference catalogue on prend le depot qui livre
GPICMT* sinon on force a zero pour les devis
GPICMT     if fctref = 'A'
GPICMT        move fcdlr to ru-niv
GPICMT     else
GPICMT        move zero to ru-niv
999999   display "trace_cgcd-reli1 : IF no74"
           end-if
           move fcnar to ru-ref
           move fcsre to ru-sref
           .
       rup-lec.
999999   display "Trace-cgcd-reli1 : rup-lec".
           perform r-ruptures
           if file-status not = zero
              initialize wor-ruptures
              perform rup-cle
              move cgcd-reli-indr to ru-indr
              move fcqtc to ru-qtec
              move fcqtl to ru-qtel
              move fcqpb to ru-qpb
              move fcmes to ru-umes
              move fcprx to ru-upri
              move fcpht to ru-pht
              move fcncl to ru-client
              move wjj to ru-datpjj
              move wmm to ru-datpmm
              move waa to ru-datpaa
              perform w-ruptures
           else
              add fcqtc to ru-qtec
              add fcqtl to ru-qtel
              perform rw-ruptures
999999   display "trace_cgcd-reli1 : IF no75"
           end-if
           perform cl-ruptures
           .
      *DDE069 fermeture des fichiers
       ferm section.
999999   display "Trace-cgcd-reli1 : ferm section".
           perform cl-fcommaap
           perform cl-fcommac2
           perform cl-fartusac
           perform cl-fartusap
           perform cl-consoart
DDE275     perform cl-comptage
           .

GPICMT* recherche type du lieu (industrie ou GP)
DD0131 rech-type-lieu section.
999999   display "Trace-cgcd-reli1 : rech-type-lieu section".
           move lili-lieu         to immlx-lect-lieu
           move spaces            to immlx-lect-secteur
           move cmmlx-lect-trt-l  to immlx-lect-trt
           move 1                 to immlx-lect-raz
999999   display "trace_cgcd-reli1 : call mmlx-lect1"
           call "mmlx-lect1" using mmlx-lect adl-art
           if ommlx-lect-rtn not = cmmdt-envi-rtn-ok
      *GPIMAIL
              move 1 to wz
              move cmmlp-mail-type-oo to immlp-mail-type(wz)
              move "Recherhe type de lieu" to immlp-mail-data(wz)
              add 1 to wz
              move cmmlp-mail-type-o to immlp-mail-type(wz)
              string "Lieu " lili-lieu " Inconnu, Commande " fccle
                    delimited by size into immlp-mail-data(wz)
              add 1 to wz
              move cmmlp-mail-type-ss to immlp-mail-type(wz)
              string "ECHEC "
                    delimited by size into immlp-mail-data(wz)
              add 1 to wz
              move cmmlp-mail-type-s to immlp-mail-type(wz)
              move fcqtl to wqtlap
              string "Mise a jour Qte Livree " wqtlap
                 "pour article " fcnar " " fcsre
                    delimited by size into immlp-mail-data(wz)
              add 1 to wz
              move cmmlp-mail-type-aa to immlp-mail-type(wz)
              string "Stock Article a verifier"
                    delimited by size into immlp-mail-data(wz)
              move spaces to immlp-mail-objet
              string "ECHEC DESTOCKAGE ARTICLE"
                   delimited by size into immlp-mail-objet
              move cmmlp-mail-trt-notif to immlp-mail-trt
              move spaces to immlp-mail-destg
              move spaces to immlp-mail-groupe
DD0351*       move "door elgu" to immlp-mail-dest
DD0351        move "anes elgu"    to immlp-mail-dest
999999   display "trace_cgcd-reli1 : call 'mmlp-mail1'"
              call 'mmlp-mail1' using mmlp-mail adl-art
999999   display "trace_cgcd-reli1 : IF no76"
           end-if
           .

GPICMT* maj specifique de l'article
DD0380 maj-specif section.
999999   display "Trace-cgcd-reli1 : maj-specif section".
GPICMT* maj stock PK (+ puis -)
           string cgcd-reli-cde cgcd-reli-ind
              delimited size into ifgar-spec-numcdecli
           move fakle1 to ifgar-spec-reference
           move cfgar-spec-action-mpick to wfgar-spec-action
999999   display "trace_cgcd-reli1 : call 'fgar-spec1'"
           call 'fgar-spec1' using fgar-spec adl-art
GPICMT* maj stock PK autre societe
           string cgcd-reli-cde cgcd-reli-ind
              delimited size into ifgar-spec-numcdecli
           move fakle1 to ifgar-spec-reference
           move cfgar-spec-action-mpick-soc to wfgar-spec-action
999999   display "trace_cgcd-reli1 : call 'fgar-spec1'"
           call 'fgar-spec1' using fgar-spec adl-art
           .

GPICMT* mise de la commande dans la dataware
DD0351 maj-dataware section.
999999   display "Trace-cgcd-reli1 : maj-dataware section".
           move fccle to icmex-stat-cdex
           move "J" to wcmex-stat-e1periode
           move 99  to wcmex-stat-e1nbm
           move zero to wcmex-stat-e1datdeb
           move 999999 to wcmex-stat-e1datfin
           move ccmex-stat-e1action-cdex to wcmex-stat-e1action
           move 3 to icmex-stat-direct
999999   display "trace_cgcd-reli1 : call 'cmex-stat1'"
           call 'cmex-stat1' using cmex-stat adl-art
           if ocmex-stat-rtn not = cmmdt-envi-rtn-ok
              move cmmlp-mail-type-oo to immlp-mail-type(1)
              string "Extraction commande " fccle " pour Dataware"
                    delimited by size into immlp-mail-data(1)
              move cmmlp-mail-type-o to immlp-mail-type(2)
              string "Extraction ko, code retour " ocmex-stat-rtn
                    delimited by size into immlp-mail-data(2)
              move cmmlp-mail-type-o to immlp-mail-type(3)
              string 'Erreur ' ocmex-stat-liberr
                    delimited by size into immlp-mail-data(3)
               move cmmlp-mail-trt-notif to immlp-mail-trt

               move spaces to immlp-mail-objet
               string "ECHEC INTEGRATION COMMANDE DANS DATAWARE"
                   delimited by size into immlp-mail-objet
               move spaces to immlp-mail-destg
               move spaces to immlp-mail-groupe
               move wnom-prog to immlp-mail-pgm
DD0351*        move "elgu micn" to immlp-mail-dest
DD0351         move "anes micn"   to immlp-mail-dest
999999   display "trace_cgcd-reli1 : call 'mmlp-mail1'"
               call 'mmlp-mail1' using mmlp-mail adl-art
999999   display "trace_cgcd-reli1 : IF no77"
           end-if
           .

GPICMT* mise a jour nbre de commande preparees dans le mere
DD0298  maj-mere section.
GPICMT* recherche commande allotie mere si traitement d'une fille
           move fccle to icgcd-mere-numcdex-fille
           move ccgcd-mere-e1trt-mere to wcgcd-mere-e1trt
999999   display "trace_cgcd-reli1 : call cgcd-mere1"
           call "cgcd-mere1" using cgcd-mere adl-art
           if ocgcd-mere-rtn not = cmmdt-envi-rtn-ok
              move cmmlp-mail-type-ss to immlp-mail-type(1)
              string "ECHEC "
                    delimited by size into immlp-mail-data(1)
              move cmmlp-mail-type-s to immlp-mail-type(2)
              string 'Cde ' fccle
                   ' Allotie mere Inexistante'
                    delimited by size into immlp-mail-data(2)
               move cmmlp-mail-trt-notif to immlp-mail-trt

               move spaces to immlp-mail-objet
               string "ECHEC LECTURE ALLOTIE MERE "
                 fcnar " " fcsre
                   delimited by size into immlp-mail-objet
               move spaces to immlp-mail-destg
               move spaces to immlp-mail-groupe
DD0351*        move "door elgu micn" to immlp-mail-dest
DD0351         move "anes micn"   to immlp-mail-dest
999999   display "trace_cgcd-reli1 : call 'mmlp-mail1'"
               call 'mmlp-mail1' using mmlp-mail adl-art
              go to maj-mere-f
999999   display "trace_cgcd-reli1 : IF no78"
           end-if
           move 'W' to gfkey
           perform op-cdesalle
           move mwor-cdesalle2 to wor-cdesalle2
           perform r-cdesalle
           if file-status not = zero
              move cmmlp-mail-type-ss to immlp-mail-type(1)
              string "ECHEC "
                    delimited by size into immlp-mail-data(1)
              move cmmlp-mail-type-s to immlp-mail-type(2)
              string 'Entete allotie ' cae-numcdex ' non trouvee'
                    delimited by size into immlp-mail-data(2)
               move cmmlp-mail-trt-notif to immlp-mail-trt

               move spaces to immlp-mail-objet
               string "ECHEC LECTURE ENTETE ALLOTIE MERE "
                   delimited by size into immlp-mail-objet
               move spaces to immlp-mail-destg
               move spaces to immlp-mail-groupe
DD0351*        move "door elgu micn" to immlp-mail-dest
DD0351         move "anes micn"   to immlp-mail-dest
999999   display "trace_cgcd-reli1 : call 'mmlp-mail1'"
               call 'mmlp-mail1' using mmlp-mail adl-art
               perform cl-cdesalle
              go to maj-mere-f
           else
              add 1 to cae-nbrfilp
              perform rw-cdesalle
              perform cl-cdesalle
999999   display "trace_cgcd-reli1 : IF no79"
           end-if
           .
        maj-mere-f.
           exit.

GPICMT* livraison de la lign ede commande dans la dataware
DD0444  liv-ligne section.
DD0444* Mise a jour MySQL
           string "cmcdmjdw.sh " ccmcd-mjdw-trt-ligne
                                 "," ccmcd-mjdw-even-destock   
                                 "," fcnoc2 "," fcnlg
                                                     X'00'
                  delimited by size into sys-var
999999   display "trace_cgcd-reli1 : call systcc"
           call "systcc" using sys-var syst-rtn

         .
        pro section.
           copy '/usr/action/ADL/copy/pro-std'.
           copy '../copy/pro-fcommaap-cdesup2'.
           copy '../copy/pro-fcommac1'.
           copy '../copy/pro-fcommac2-cdesup2'.
           copy '../copy/pro-fcommac3-cdesup2'.
           copy '../copy/pro-fcommac4-cdesup2'.
           copy '../copy/pro-fcommac5-cdesup'.
           copy '../copy/pro-fcomjoap2'.
           copy '../copy/pro-fcomjoc12'.
           copy '../copy/pro-fcomjoc22'.
           copy '../copy/pro-fcomjoc32'.
           copy '../copy/pro-fcomjoc42'.
           copy '../copy/pro-fcomjoc52'.
           copy '../copy/pro-fcoadcli-cdesup2'.
DD2001     copy '../copy/pro-fjoadcli-cdesup2'.
           copy '../copy/pro-numdevis'.
           copy '../copy/pro-artdevc12'.
           copy '../copy/pro-fartusap2'.
           copy '../copy/pro-fartusac2'.
           copy '../copy/pro-consoart2'.
           copy '../copy/pro-suivicde2'.
           copy '../copy/pro-multidat'.
           copy '../copy/pro-fcommac6-cdesup'.                          *DDE026
           copy '../copy/pro-fcomjoc62'.                                *DDE026
DDE045     copy '../copy/pro-fcomjoc72'.
DDE045     copy '../copy/pro-fcommac72'.
DDE125     copy '../copy/pro-ruptures2'.
DDE275     copy '../copy/pro-comptage'.
DD0180     copy '../copy/pro-fcommac8'.
DD0351     copy '../copy/pro-cdeslanc3'.
DD0298     copy '../copy/pro-cdesalle'.
DD0351     copy '../copy/pro-fclients'.
