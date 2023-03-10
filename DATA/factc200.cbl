      *DD2063 06/10/20 anes Ajout code contrat pour UGD
      *DD9999 01/04/20 anes on force date fac si BL posterieur pour tous    
      *DD2023 30/03/20 anes Bamappro : ajout type 4 sur segment CD EXONERATION.
      *DD9999 20/12/19 anes changement numero d'accord Leclerc et Brico 2020
      *DD9999 03/01/19 anes changement numero d'accord Leclerc et Brico
      *DD9999 15/01/18 anes numero de ligne pour tapis st maclou TSM
      *DD9999 15/03/17 anes on force la date de facturation si BL posterieur
      *DD0351 17/11/16 anes Correction pour ne plus envoyer remise a zero
      *DD0351 16/02/16 anes changement numero d'accord Leclerc et Brico
      *DD0351 24/02/15 micn changement numero d'accord Leclerc et Brico
      *DD0351 15/01/15 anes LEROY MERLIN St-Louis, Ste-Marie : ajout commentaire
      *DD0351 03/10/14 anes gestion maxeda comme pointP     
      *DD0777 11/02/14 micn ajout code EAN client final dans le cas de commande transitaire
      *DD0774 31/12/13 micn specifique pour client point P et Plateforme du batiment 
      *DD0774 12/12/13 door Renumeratation des clients en dur suite a la fusion
      *DD0771 08/08/13 micn ajout zone libelle d'accord pour groupement LCL
      *DD0221 18/03/11 elgu ne plus charger la reference commnade client dans fcomjoc1
      *DD0351 28/02/11 elgu modif calcul montant ligne avec arrondi sur la 3eme decimale
      *DD0221 13/01/11 anes Allongement reference commande client
      *DD0351 11/01/11 elgu supprimer controle avec @GP tous les clients passent avec @GP
      *DD0351 20/12/10 elgu je passe la reference commande client a 12 caractere dans fcommac1
      *DD0420 01/12/10 elgu MST(tapis st maclou) avec @GP
      *DD0420 01/12/10 elgu BRMet BTK avec @GP
      *DD0351 25/11/10 elgu ne pas envoyer de facture a montant zero
      *DD0465 06/08/10 Plasto ajout pcb sur ligne
      *DD0351 22/04/10 elgu pour export code taxe different de 1 mettre zero dans la base au lieu de 20.6
      *DD0351 19/04/10 elgu ajout AUCHAN
      *DD0351 18/02/10 elgu suite a erreur d'avoir pour LER, segment ligne obligatoire, je passe la meme modif pour tout lemonde,
      *DD0350 01/02/10 elgu faire traitement identique meme si date echeance = date facture
      *DD0420 04/12/09 elgu ajout AKI,BRM,BAO avec @GP avec agrandissemnt zone note de debit et chargement dans zone dfida
      *DD0420 16/11/09 elgu ajout LER avec envoi @GP
      *DD0420 12/11/09 elgu ajout code remise en fin de segment car zone trop courte: dfparg/dfcprc
      *DD0351 07/10/09 elgu ajout BMN comme LER pour contrtole demat
      *DD0351 07/10/09 elgu correction envoi des avoirs ne se fait plus depuis la modif de Bricot depot le 19/06/09
      *DD0423 22/09/09 elgu ajout BDP avec envoi @GP
      *DD0351 19/06/09 elgu pour Bricot depot on n'envoi pas les avoirs (BDP)
      *DD0423 20/05/09 elgu ajout Tpis ST Macloud trt avec @GP
      *DD0423 07/05/09 elgu mettre obligatoirement une ligne article avec gencod 999999999 qte et prix a zero
      *                     pour castorama une facture sans ligne est rejetee
      *DD0423 28/04/09 elgu nouveau code ean societe et demat CASTORAMA
      *DD0423 26/03/09 elgu modif pour traitement avoir castorama ne faisant pas reference a une facture
      *                     bon de sortie et note de debit
      *DD0350 19/03/09 elgu suite a reclassement des niveaux dans mmpa-nivc.com modif pste table utilise
      *DD0351 13/11/08 elgu test code ean pour Bricodepot ERELS
      *DD0221 29/09/08 elgu modif reference client pour PLanit/BriKodepot idem Bricorama
      *DD0351 23/06/08 elgu controler que la facture est passee en journal des ventes
      *DD0351 15/04/08 elgu pour BAO ajout capital et devise dans meme zone
      *DD0350 04/12/07 elgu correction creation trace avec c au lieu de C
      *                     perturbait l'affichage du tracetel
      *DD0351 24/10/07 elgu traiter LAPEYRE comme BRM
      *                     pour BAO mettre 2 dans le code remise ligne
      *DD0351 21/09/07 elgu ajouter une trace
      *DD0301 16/06/07 elgu demarrage ERELS
      *DD0351 19/03/07 elgu suite a d'autres factures non envoyees je fais la modif ci-dessous
      *                     pour tous les clients sauf pour les avoirs sans reference
      *DD0351 08/03/07 elgu laisser le code a traiter pour les factures LER, si on ne veut
      *                     pas qu'elles passent par EDI il faudra mettre le code tsco3 a 9
      *                     par le pgm mst00
      *DD0350 30/11/06 elgu correction numerotation ligne en debut de facture et non en debut
      *                     de commande ==> probleme pour TSM pulsieurs commandes sur 1 facture
      *DD0326 03/07/06 elgu
      *DD0324 16/06/06 micn
      *DD9999 23/06/06 elgu suppreesion modif pour brico depot, ils se debrouillent car
      *DD9999 15/06/06 elgu meme modif pour brico depot que pour bricorama (ref cde client
      *                     trop courte dans fcommaap (BTK idem BRM)
      *DD0324 23/05/06 elgu suite a edition numero facture sur 7 car
      *DD9999 17/05/06 elgu correction recherche enseigne et non groupement le plus haut
      *DD0316 18/04/06 elgu nlle wor-ffacture.mod wor-fcommac1
      *DD9999 21/03/06 elgu changement code EAN BRICODEPOT
      *DD9999 23/08/05 elgu ne pas envoyer d'avoir sans reference facture pour
      *                     CASTORAMA
      *V10221 22/06/05 elgu controle referebce commande client pour BRICORAMA
      *                     suite a probleme pour les commandes saisies par SCOM
      *DD0221 09/05/05 elgu modif prise reference commande client pour
      *                     bricorama (trop petite ds fcommaap)
      *DD0219 29/11/04 elgu
      *DD9999 14/05/04 elgu suite a mail Mr Honnard de Castorama correction
      *                     valeur montant remise ligne non unitaire
      *DD0164 29/04/04 elgu ajouter code regroupement client das E1
      *DD0126 22/10/03 elgu modif chartgement code taxe pour element RG
      *DD9999 28/05/03 elgu
       PROGRAM-ID. FACTC200.
      *
      ******************************************************************
      * GPICMT    Preparation des Factures-Avoirs a envoyer sur micro  *
      *           creation du fichier sequentiel EMISSION              *
      ******************************************************************
      *
      *DDE125: ajout zone negoce pour DOMAXEL + type remise pour bricodepot
      *DDE153: ajout trt regroupement BL sur une facture
      *DDE089: trt des lignes factures a la sous ref et non plus en cumul/ref
      *      : envoi des montants en devise si devise # de la compta
      *DDE011: ajout recherche libelle de la devise et modifs pour version
      *        commune GPI/DINAC
      *M0800 : correction lecture invalide calendrier
      *M0400 : suite a changement tva prendre la tva en cours par defaut
      *M0300 : tester la rupture sur article en tenant compte du prix suite
      *        a probleme LEROY avec facture 2 lignes articles cumulees en une
      *        seule alors que 2 lignes avec prix different
      *M1299 : modif cles secondaires 1/3/4 multidat ajout du siecle
      *M1099a: ajout demande entrer pour continuer apres erreur
      *M1099 : controle rue du facture # espaces
      *M0999 : ajout test file-status apres start MULTIDAT
      *M0599 : creer iun elt tva meme si facture a zero
      *M0399 : ne pas traiter une facture sans reference de commande
      *        renseigner les zones acheteurs reprise de DINAC
      *M0299 : mettre en dur le code EAN pour client 151005 et 465533
      *M0199 : ajout controle pied de facture = celui de la commande
      *M1298 : modif pour ne pas renseigner les zones facture de reference ds le
      *        cas ou il n'y a pas de facture de reference
      *M1098 : modif pour CASTO pb des etiquettes
      *M0998 : trt des cdes sans expemois
      *        ajout lecture transporteur
      *        ne pas traiter l'affranchissement postal si franco
      *M0798a: traiter les factures/avoirs de redressement pour LEROY MERLIN
      *M0798 : modif demandee par LEROY MERLIN, code EAN "3001000001100" pour
      *        frais de facturation
      *      : mettre  cod tva 1 pour element remise globale
      *M0698 : traiter les avoir ne faisant pas reference a une facture pour
      *        LEROY MERLIN ( mettre tout a zero)
      *M0498c: ajout saisie num. expedition si num = zero
      *M0498b: ne pas traiter les avoirs ne faisant pas reference a une facture
      *M0498a: ajout controle date de facture not < date du jour
      *M0498 : modif suite controle LEROY MERLIN
      *
      *valeurs du code : tsco3 -  espace ==> ne pas traiter la facture demate
      *                                1 ==> facture a dematerialiser
      *                                2 ==> facture editee a dematerialiser
      *                                3 ==> facture dematerialisee
      *                                5 ==> montant a zero non traitee
      *                                6 ==> a ne pas dematerialiser,code mis a
      *                                      jour par SLIV
      *                                7 ==> code devise # zero
      *                                8 ==> avoir/fac sans reference avec "S"
      *                                9 ==> facture en erreur (a regarder)
      *M0599 : door modifi an 2000 pour multidat
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. DPS-4.
       OBJECT-COMPUTER. DPS-4.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           copy "/usr/action/ADL/copy/wor-adl".
           copy "../copy/wor-ttfacmst-cdesup".
           copy "../copy/wor-fcommaap-cdesup".
           copy "../copy/wor-fcommac1".
           copy "../copy/wor-fcommac2-cdesup".
           copy "../copy/wor-fcommac4-cdesup".
           copy "../copy/wor-fcoadcli-cdesup".
           copy "../copy/wor-fclients".
           copy "../copy/wor-clisuite".
           copy "../copy/wor-filieres".
           copy "../copy/wor-expemois-cdesup".
           copy "../copy/wor-paramgpi".
           copy "../copy/wor-multidat".
           copy "../copy/wor-emission".
           copy "../copy/wor-ffacture-cdesup".
           copy "../copy/wor-fartusac".
      *----> M0998 (D)
           copy "../copy/wor-trpntran".
      *----> M0998 (F)

DDE011     copy "../copy/mmpa-devi.com".                                *GPICMT
DDE089     copy "../copy/mmcp-devb.com".                                *GPICMT
DD0164     copy "../copy/fgcl-affi.com".                                *GPICMT
DD9999     copy "../copy/mmpa-nivc.com".                                *GPICMT
DD0351     copy '../copy/mmtr-trac.com'.                                *GPICMT
DD0351     copy '../copy/mmaf-ubla.com'.                                *GPICMT
DD0351     copy "../copy/mmca-atoi.com".                                *GPICMT
       01  tout.
DD0351   02  wnom-prog                PIC X(10) value 'factc200'.
DD0465   02  wpcb                     pic 9(6).
DD0351   02  wnbr15x                  PIC X(15).
DD0351   02  wnbr15 redefines wnbr15x  PIC 9(15).
DD0774* Clients specifiques => modif pour renumerotation fusion
DD0774   02  wclient-spe1            PIC X(6).
DD0774   02  wclient-spe2            PIC X(6).
GPICMT* flag ligne article cree
DD0423   02  w-flag-ligne   pic x.

DD0221** LIBELLE REFERENCE COMMANDE DU CLIENT POUR BRICORAMA            *M0600a
           02   WLENTB.                                                 *M0600a
            03  WLAB              PIC X(18).                            *M0600a
            03  WLBB              pic x(12).                            *M0600a
DDE153* numero du BL Interne (avec indice reliquat)
           02 wnbil.
DD0316        03 wnbi1l         pic 9(7).
              03 wnbi2l         pic 9.
DDE153* memo 1numero facture en cours
DD0316     02 w-tsnfa           pic 9(7).
DDE153* memo code mise cde traitee
           02 w-tsco3           pic x.
      * memo test cde suite pour une mmeme facture
           02 w-retour          pic x.

           02 wsnbj             pic x.                                  *M0800
      *----> M0498a (D)
           02 wdate             pic 9(6).
           02 wdatex redefines wdate.
              03 wda            pic 99.
              03 wdm            pic 99.
              03 wdj            pic 99.
           02 wdats.
              03 wanc.
                04 wdss           pic 99.
                04 wdaa           pic 99.
              03 wanc9 redefines wanc pic 9(4).
              03 wdmm           pic 99.
              03 wdjj           pic 99.
           02 wdatfs.
              03 wdfss          pic 99.
              03 wdfaa          pic 99.
              03 wdfmm          pic 99.
              03 wdfjj          pic 99.
      *----> M0498a (F)
           02 wemex.
              03 weme           pic 9(13).
DD0774     02 weme-pointp       pic 9(13).
DD0774     02 wclrdi            pic x(3).
           02 wsiex.
              03 wsie           pic 9(13).
           02 wffp01            pic x(35).
           02 wffp02            pic x(35).
           02 wffp03            pic x(35).
           02 wffp04            pic x(35).
           02 wffp05            pic x(35).
           02 wffp06.
              03 wffp061        pic x(26).
              03 wffp062        pic x(26).
              03 wffp063        pic x(26).
           02 wffp07.
              03 wffp071        pic x(26).
              03 wffp072        pic x(26).
              03 wffp073        pic x(26).
           02 wass01            pic x(35).
           02 wass02            pic x(35).
           02 wass03            pic x(35).
           02 wass04            pic x(35).
           02 wass05            pic x(35).
           02 wass06.
              03 wass061        pic x(26).
              03 wass062        pic x(26).
              03 wass063        pic x(26).
           02 wmoe01            pic x(30).
           02 wmoe02            pic x(30).
           02 wmoe03            pic x(30).
           02 wmoe04            pic x(30).
           02 wmoe05            pic x(30).
           02 wmoe06            pic x(30).
           02 weanrex.
              03 weanre         pic 9(13).
           02 wescpex.
              03 wtesc1         pic x(7).
              03 filler         pic xxx.
              03 wtesc2         pic x(7).
           02 wc022             pic 99.99.
           02 wc042             pic 9(4).99.
           02 wc122             pic 9(9).99.
           02 wesct1            pic x(70).
           02 wesct2            pic x(70).
           02 wesct3            pic x(70).
           02 wpent1            pic x(70).
           02 wpent2            pic x(70).
           02 wpent3            pic x(70).
           02 wdes              pic x(30).
           02 wdes2             pic x(15).
           02 WMES           PIC 99.
           02 WPRX           PIC 9.
           02 WQPB           PIC 9(4).
           02 WPHT           PIC 9(6)V99.
           02 WPHTB          PIC 9(6)V99.
           02 wtrpv          pic 999v99.
           02 wdesd.
              03 wdesa          pic 99.
              03 wdesm          pic 99.
              03 wdesj          pic 99.
           02 wpend.
              03 wpena          pic 99.
              03 wpenm          pic 99.
              03 wpenj          pic 99.
           02 wcetx.
              03 wcet01         pic x(80).
              03 wcet02         pic x(80).
              03 wcet03         pic x(80).
              03 wcet04         pic x(12).
           02 wcepx.
              03 wcep01         pic x(80).
              03 wcep02         pic x(80).
              03 wcep03         pic x(80).
              03 wcep04         pic x(12).
           02 wcnudx.
              03 wcnudt         pic 999.
           02 weanx.
              03 filler         pic xxx.
              03 wcnud          pic 999.
              03 filler         pic x(7).
           02 weandx.
              03 weand          pic 9(13).
           02 wpgcle.
              03 filler         pic x(7).
              03 wpgcnu         pic 999.
           02 wnfacx.
DD0316        03 filler         pic 9 value zero.
DD0316        03 wnfac          pic 9(7).
           02 wnfarx.
DD0316        03 filler         pic 9 value zero.
DD0316        03 wnfar          pic 9(7).
      *----> M0399 (D)
           02 wclcde            pic 9(6).
           02 wadrcde.
              03 weacx.
                04 weac         pic 9(13).
              03 wrsc1          pic x(26).
              03 wrsc2          pic x(26).
              03 wrc1           pic x(26).
              03 wrc2           pic x(26).
              03 wvic           pic x(26).
              03 wcpc           pic x(5).
              03 wicc           pic x(15).
           02 wclfac            pic 9(6).
           02 wadrfac.
              03 weamx.
                04 weam         pic 9(13).
              03 wrsm1          pic x(26).
              03 wrsm2          pic x(26).
              03 wrm1           pic x(26).
              03 wrm2           pic x(26).
              03 wvim           pic x(26).
              03 wcpm           pic x(5).
              03 wicm           pic x(15).
DDE125* siret du facture a
              03 wsiret         pic x(30).

           02 wclliv            pic 9(6).
           02 wealx.
              03 weal           pic 9(13).
           02 wadrliv.
              03 wrsl1          pic x(26).
              03 wrsl2          pic x(26).
              03 wrl1           pic x(26).
              03 wrl2           pic x(26).
              03 wvil           pic x(26).
              03 wcpl           pic x(5).
DD0777     02 wclNCL-FINAL      pic 9(6).
DD0777     02 weanfinal         pic 9(13).
           02 wdcdc.
              03 wdcj           pic 99.
              03 wdcm           pic 99.
              03 wdca           pic 99.
           02 wnbi.
DD0316        03 wnbi1          pic 9(7).
              03 wnbi2          pic 9.
           02 wrepx.
DD0316        03 wrep9          pic 9(7).

DD0423* note de debit
DD0420     02 wnotedebit        pic x(15).

           02 wokx              pic xxx.
           02 wdatfx.
              03 wj             pic 99.
              03 wm             pic 99.
              03 wa             pic 99.
           02 wdatf9 redefines wdatfx pic 9(6).
           02 wnbjfx.
              03 wnbjf          pic 999.
           02 wnbjex.
              03 wnbje          pic 999.
           02 wpds              pic 9(5).9.
           02 WENRTVA1.
             03 WTAUPA1      PIC 99V999.
             03 WTVAX1.
               04 WTVA1      OCCURS 5  PIC  99V999.
           02 WENRTVA2.
             03 WTAUPA2      PIC 99V999.
             03 WTVAX2.
               04 WTVA2      OCCURS 5  PIC 99V999.
           02          WAPCOM.
             03         WCLE.
DD0316         04       WNUM           PIC 9(7).
               04       WNIN           PIC 9.
DD0316       03         WNFA           PIC 9(7).
             03         WFOA           PIC 9.
      *
      **** enreg facture
      *
          copy "../copy/wor-ffacture-cdesup.mod"
                 replacing ==(pref)== by ==w==.
           02 WCODTAX1       PIC 99.
           02 FILLER         REDEFINES WCODTAX1.
             03 WPAR1        PIC 9.
             03 WTV1         PIC 9.
           02 wnlfa          pic 9(6).
GPICMT* memo numero ligne de la commande
DD0465     02 wnlcd          pic 9(6).
           02 WWCLE.
             03 WNEL         PIC 99.
             03 WNLIG        PIC 99.
DDE046       03 WART1        PIC x(7).
           02 WWWCLE.
             03 WWNEL        PIC 99.
             03 WWNLIG       PIC 99.
DDE046       03 WWART1       PIC x(7).
           02   xart.
DDE046       03 xnar         pic x(7).
             03 xsre         pic 99.
           02 WQTELIV        PIC 9(10)V99.
           02 wqtecde        pic 9(10)v99.
           02 wcalc          pic 9(10)v99.
           02 wcalc3         pic 9(10)v999.
           02 wcall          pic 9(10)v99.
           02 weanarx.
              03 weanar      pic 9(13).
           02 wvlfa          pic xx.
           02 wvafa          pic xx.
           02 wc112          pic 9(8).99.
           02 wpri9          pic 9(6)v99.
DDE125*    02 wenrte0        pic x(62).
DDE125*    02 wenrte1        pic x(62).
DDE125*    02 wenrte2        pic x(62).
DDE125    copy "../copy/wor-emission.mod" replacing ==(pref)== by ==a==.
DDE125    copy "../copy/wor-emission.mod" replacing ==(pref)== by ==b==.
DDE125    copy "../copy/wor-emission.mod" replacing ==(pref)== by ==c==.
           02 i              pic 9.
           02 wdfnbe         pic 999.
           02 wrclx.
              03 wrcl1.
                04 wrcj      pic 99.
                04 wrcm      pic 99.
DD0221*       03 wrclr       pic x.
DD0221        03 wrcl2       pic x(15).
DD9999* memo ref cde client de fcommaap avant recherche ds fcommac1
           02 wrcl3          pic x(15).
           02 wht0           pic 9(9)v99.
           02 wx12.
              03 wsig12      pic x.
              03 wx11.
                04 wsig11    pic x.
                04 filler    pic x(10).
           02 wdatecx.
              03 wecj        pic 99.
              03 wecm        pic 99.
              03 weca        pic 99.
           02 wdateex.
              03 wes         pic 99.
              03 wea         pic 99.
              03 wem         pic 99.
              03 wej         pic 99.
           02 wdatee9 redefines wdateex pic 9(8).
           02 wdatefx.
              03 wfs         pic 99.
              03 wfa         pic 99.
              03 wfm         pic 99.
              03 wfj         pic 99.
           02 wdatef9 redefines wdatefx pic 9(8).
           02 wrang          pic 9(5).
           02 wrangx redefines wrang.
              03 wr1         pic 99.
              03 wr2         pic 999.
           02  WGENCOD.
            03 WG3                PIC 9.
            03 WCNU               PIC 9(5).
            03 WCIP               PIC 9(6).
            03 WKLE               PIC 9.
      *----> M0498c (D)
DD0326     02 wexpedit       pic x(8).
      *----> M0498c (F)

      *----> M0798a (D)
           02 wdop.
              03 wdop1       pic x(6).
              03 filler      pic x(24).
           02 wanrx.
             03 wsr          pic 99.
             03 war          pic 99.
           02 wanr9 redefines wanrx pic 9(4).
      *----> M0798a (F)

      *----> M0998 (D)
           02 wdatlx.
              03 wjl            pic 99.
              03 wml            pic 99.
              03 wal            pic 99.
           02 wdatl9 redefines wdatlx pic 9(6).
           02 wexist            pic x.
      *----> M0998 (F)
DD9999* anes 23/03/17
           02 wdfdfa1           pic 9(8). 
       PROCEDURE DIVISION.
      *
       TRAIT SECTION.
      *
       deb.
DD0774* Renumerotation des tiers
DD0774     if mmdt-envi-annee > 13
DD0774        move '104178' to wclient-spe1
DD0774        move '105503' to wclient-spe2
DD0774     else
DD0774        move '151005' to wclient-spe1
DD0774        move '465533' to wclient-spe2
DD0774     end-if
           . 
      *----> M0498a (D)
           accept wdate from date.
           move wda to wdaa.
           move wdm to wdmm.
           move wdj to wdjj.
           if wda > 50 move 19 to wdss
             else      move 20 to wdss.
      *----> M0498a (F)
DD0423*    accept wexpedit.
           move 'W' to gfkey.
           perform op-ttfacmst.
           move 'I' to gfkey.
           perform op-fcommaap.
           move 'I' to gfkey.
           perform op-fcommac1.
           move 'I' to gfkey.
           perform op-fcommac2.
           move 'I' to gfkey.
           perform op-fcommac4.
           move 'I' to gfkey.
           perform op-fcoadcli.
           move 'I' to gfkey.
           perform op-fclients.
           move 'I' to gfkey.
           perform op-clisuite.
           move 'I' to gfkey.
           perform op-filieres.
           move 'I' to gfkey.
           perform op-expemois.
           move 'I' to gfkey.
           perform op-paramgpi.
           move 'I' to gfkey.
           perform op-multidat.
           move 'O' to gfkey.
           perform op-emission.
           move 'I' to gfkey.
           perform op-ffacture.
           move 'I' to gfkey.
           perform op-fartusac.
      *----> M0998 (D)
           move 'I' to gfkey.
           perform op-trpntran.
      *----> M0998 (F)

      *    recup des variables d'environnement
           call 'mmdt-envi1' using adl-art.

DDE089* recup devise de base en compta
           call 'mmcp-devb1' using mmcp-devb adl-art
           if ommcp-devb-rtn not = '0'
              display ommcp-devb-liberr
              go to fin
           .


      *
      *** chargement de tous les parametres fixes de paramgpi en wss
      *
           MOVE "PARAFITVA1" TO PGCLE.
           perform rnl-paramgpi.
           if file-status not = zero
                 DISPLAY "EL.TVA1 ABSENT"
                                 GO TO FIN.
           MOVE PGFZON TO WENRTVA1.
           MOVE "PARAFITVA2" TO PGCLE.
           perform rnl-paramgpi.
           if file-status not = zero
                 DISPLAY "EL.TVA2 ABSENT"
                                 GO TO FIN.
           MOVE PGFZON TO WENRTVA2.
           move "DEMATDFEME" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATDFEME" go to fin.
           move pgbar   to wemex.
           if wemex = spaces or wemex not numeric display
             "PARAMETRE DEMATDFEME INCORRECT"    go to fin.
           move "DEMATFFP01" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATFFP01" go to fin.
           move pgbar   to wffp01.
           if wffp01 = spaces display
             "PARAMETRE DEMATFFP01 INCORRECT" go to fin.
           move "DEMATFFP02" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATFFP02" go to fin.
           move pgbar   to wffp02.

      * pour DINAC on accepte le parametre raison sociale 2 a espace
      * je l'autorise aussi pour GPI (elgu)
      *    if wffp02 = spaces display
      *      "PARAMETRE DEMATFFP02 INCORRECT" go to fin.
           move "DEMATFFP03" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATFFP03" go to fin.
           move pgbar   to wffp03.
           if wffp03 = spaces display
             "PARAMETRE DEMATFFP03 INCORRECT" go to fin.
           move "DEMATFFP04" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATFFP04" go to fin.
           move pgbar   to wffp04.
           if wffp04 = spaces display
             "PARAMETRE DEMATFFP04 INCORRECT" go to fin.
           move "DEMATFFP05" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATFFP05" go to fin.
           move pgbar   to wffp05.
           if wffp05 = spaces display
             "PARAMETRE DEMATFFP05 INCORRECT" go to fin.
           move "DEMATFFP06" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATFFP06" go to fin.
           move pgbar   to wffp06.
           if wffp061 = spaces or wffp062 = spaces or
              wffp063 = spaces display
             "PARAMETRE DEMATFFP06 INCORRECT" go to fin.
           move "DEMATFFP07" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATFFP07" go to fin.
           move pgbar   to wffp07.
           if wffp071 = spaces or wffp072 = spaces or
              wffp073 = spaces display
             "PARAMETRE DEMATFFP07 INCORRECT" go to fin.
           move "DEMATDFSIE" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATDFSIE" go to fin.
           move pgbar   to wsiex.
           if wsiex = spaces or wsiex not numeric display
             "PARAMETRE DEMATDFSIE INCORRECT"    go to fin.
           move "DEMATASS01" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATASS01" go to fin.
           move pgbar   to wass01.
           if wass01 = spaces display
             "PARAMETRE DEMATASS01 INCORRECT" go to fin.
           move "DEMATASS02" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATASS02" go to fin.
           move pgbar   to wass02.
      * pour DINAC on accepte le parametre raison sociale 2 a espace
      * je l'autorise aussi pour GPI (elgu)
      *    if wass02 = spaces display
      *      "PARAMETRE DEMATASS02 INCORRECT" go to fin.
           move "DEMATASS03" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATASS03" go to fin.
           move pgbar   to wass03.
           if wass03 = spaces display
             "PARAMETRE DEMATASS03 INCORRECT" go to fin.
           move "DEMATASS04" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATASS04" go to fin.
           move pgbar   to wass04.
           if wass04 = spaces display
             "PARAMETRE DEMATASS04 INCORRECT" go to fin.
           move "DEMATASS05" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATASS05" go to fin.
           move pgbar   to wass05.
           if wass05 = spaces display
             "PARAMETRE DEMATASS05 INCORRECT" go to fin.
           move "DEMATASS06" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATASS06" go to fin.
           move pgbar   to wass06.
           if wass061 = spaces or wass062 = spaces display
             "PARAMETRE DEMATASS06 INCORRECT" go to fin.
           move "DEMATMOE01" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATMOE01" go to fin.
           move pgbar   to wmoe01.
           if wmoe01 = spaces display
             "PARAMETRE DEMATMOE01 INCORRECT" go to fin.
           move "DEMATMOE02" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATMOE02" go to fin.
           move pgbar   to wmoe02.
           if wmoe02 = spaces display
             "PARAMETRE DEMATMOE02 INCORRECT" go to fin.
           move "DEMATMOE03" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATMOE03" go to fin.
           move pgbar   to wmoe03.
           if wmoe03 = spaces display
             "PARAMETRE DEMATMOE03 INCORRECT" go to fin.
           move "DEMATMOE04" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATMOE04" go to fin.
           move pgbar   to wmoe04.
           if wmoe04 = spaces display
             "PARAMETRE DEMATMOE04 INCORRECT" go to fin.
           move "DEMATMOE05" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATMOE05" go to fin.
           move pgbar   to wmoe05.
           if wmoe05 = spaces display
             "PARAMETRE DEMATMOE05 INCORRECT" go to fin.
           move "DEMATMOE06" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATMOE06" go to fin.
           move pgbar   to wmoe06.
           if wmoe06 = spaces display
             "PARAMETRE DEMATMOE06 INCORRECT" go to fin.
           move "DEMATEANRE" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATEANRE" go to fin.
           move pgbar   to weanrex.
           if weanrex = spaces or weanrex not numeric or
              weanre = zero display
             "PARAMETRE DEMATEANRE INCORRECT" go to fin.
           move "DEMATESCPE" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATESCPE" go to fin.
           move pgbar   to wescpex.
DD0465*    if wtesc1 = spaces or wtesc1 = spaces display
DD0465*      "PARAMETRE DEMATESCPE INCORRECT" go to fin.
           move "DEMATESCT1" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATESCT1" go to fin.
           move pgbar   to wesct1.
           move "DEMATESCT2" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATESCT2" go to fin.
           move pgbar   to wesct2.
           move "DEMATESCT3" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATESCT3" go to fin.
           move pgbar   to wesct3.
           move "DEMATPENT1" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATPENT1" go to fin.
           move pgbar   to wpent1.
           move "DEMATPENT2" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATPENT2" go to fin.
           move pgbar   to wpent2.
           move "DEMATPENT3" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATPENT3" go to fin.
           move pgbar   to wpent3.
           move "DEMATCET01" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCET01" go to fin.
           move pgbar   to wcet01.
           move "DEMATCET02" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCET02" go to fin.
           move pgbar   to wcet02.
           move "DEMATCET03" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCET03" go to fin.
           move pgbar   to wcet03.
           move "DEMATCET04" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCET04" go to fin.
           move pgbar   to wcet04.
           move "DEMATCPE01" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCPE01" go to fin.
           move pgbar   to wcep01.
           move "DEMATCPE02" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCPE02" go to fin.
           move pgbar   to wcep02.
           move "DEMATCPE03" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCPE03" go to fin.
           move pgbar   to wcep03.
           move "DEMATCPE04" to pgcle.
           perform rnl-paramgpi.
           if file-status not = zero display
             "MANQUE DANS PARAMGPI : DEMATCPE04" go to fin.
           move pgbar   to wcep04.
       p00.
DDE153* on lit ttfacmst sur cle numero de facture
      *    move spaces to tscle.
      *    perform snl-ttfacmst.
           move zero to tsnfa.
           perform snlsk1-ttfacmst.
           if file-status not = zero go to fin.
           move spaces to wcnudx.
       p01.
           perform n-ttfacmst.
           if file-status not = zero go to p30.
      *    if tsrdi not = "MST"    go to p01.
           if tsco3 not = "2"      go to p01.

DDE153     move tsnfa to w-tsnfa.
       p1a.
           move tscle   to fccle.
           perform rnl-fcommaap.
           if file-status not = zero display
             "COMMANDE INEXISTANTE :  " tscle go to err.
GPICMT* pour bricodepot on envoie pas les avoirs
DD0351     if (fcfoa = 5 or fcfoa = 6 or fcfoa = 7)
  -           and tsrdi = 'BDP'
  -           move "8" to tsco3
  -           go to p21
DD0351     end-if
DDE011* autorise les commandes avec devise
DDE011*    if fcdev not = zero move "7" to tsco3 go to p21.
      *----> M0798a (D)
DD0221     move spaces to wrclx
           move fcrcl  to wrclx.
      *----> M0798a (F)

      *----> M0399 (F)
DDE153*    move fcnfa to fbnfa.
           move w-tsnfa to fbnfa.
           perform rnl-ffacture.
           if file-status not = zero display
             "FACTURE INEXISTANTE :  " w-tsnfa "  COMMANDE N.: " tscle
             "   NON TRAITEE"        go to err.

GPICMT* ne pas envoyer de facture a montant zero
           if fbnpf = zero and fbnpd = zero
              display 'FACTURE: ' fcnfa ' a zero'
              move spaces to wokx
              display "FAIRE ENTRER POUR CONTINUER"
              move "5" to tsco3
              go to err
           end-if

GPICMT* on controle que la facture est passee en compta (journal des ventes fait)
DD0351     if fbjvt  not = 1
  -           display 'FACTURE: ' fcnfa ' Non passee en compta FACTC025'
  -           move spaces to wokx
  -           display "FAIRE ENTRER POUR CONTINUER"
  -           accept wokx
  -           go to fin
DD0351     end-if

DDE153* si num cde ds ffacture = zero ==> plusieurs Bl
           if fbncd not = zero
               if fbncd not = tscle display
             "PIED DE FACTURE NON CONCORDANT :  " fcnfa
             "  COMMANDE N.:  " tscle
             "   NON TRAITEE"        go to err
               end-if
           end-if

      *DDE011 ajout recuperation sigle de la devise
           perform ctrl-devise
           if ommpa-devi-rtn not = '0'
              display "DEVISE INEXISTANTE, FACTURE:  " fcnfa
             "  COMMANDE N.:  " tscle
             "   NON TRAITEE"
              go to err.

           move fbdfa to wdfaa.
           move fbdfm to wdfmm.
           move fbdfj to wdfjj.
           if fbdfa > 50 move 19 to wdfss
             else        move 20 to wdfss.
           if wdatfs > wdats go to p01.
           move fbdaf to wfbdaf.
           move fbreg to wfbreg.
           move fbfra to wfbfra.
           move fbtax to wfbtax.
           move fbdae to wfbdae.

      * DDE089 si devise facture = devise de base comptable on passe les zones
      *        francs/euro sinon on passe les zones devises
           if fbdev = ommcp-devb-cod9
              move fbc1f to wfbc1f
              move fbht1f to wfbht1f
              move fbtx1f to wfbtx1f
              move fbc2f to wfbc2f
              move fbht2f to wfbht2f
              move fbtx2f to wfbtx2f
              move fbnpf to wfbnpf
              move fbe1f to wfbe1f
              move fbe2f to wfbe2f
              move fbe3f to wfbe3f
           else
              move fbc1d to wfbc1f
              move fbht1d to wfbht1f
              move fbtx1d to wfbtx1f
              move fbc2d to wfbc2f
              move fbht2d to wfbht2f
              move fbtx2d to wfbtx2f
              move fbnpd to wfbnpf
              move fbe1d to wfbe1f
              move fbe2d to wfbe2f
              move fbe3d to wfbe3f
           end-if

      *    recherche du destinataire par la filiere du livre a
           move fcncl to fincl wclliv wclfac.
           move fcncl to wclcde.
DD0777     move zero to wclNCL-FINAL

           move fccle to alcle.
           perform rnl-fcoadcli.
           if file-status not = zero go to p1b.
           if alliv not = zero move alliv to wclliv fincl.
           if alfac not = zero move alfac to wclfac.
           if alcde not = zero move alcde to wclcde.
DD0777     if ALNCL-FINAL not = zero 
              move ALNCL-FINAL to wclNCL-FINAL
           end-if

      *
      **************  recherche du facture a ********************
      * pour savoir si a traiter
      *
           move wclfac to fincl.
           move spaces to wadrfac.
           perform rnlsk1-filieres.
           if file-status = zero
              move fiean to weamx weandx weanx
######* pas de GLN sur facture a, on regarde GLN du commande par
######*    else
######*      if fiean = space or fiean = zero
######*        move wclcde to fincl
######*        perform rnlsk1-filieres
######*        if file-status = zero
######*          move fiean to weamx weandx weanx
######*        end-if
######*      end-if
           end-if
           move wclfac to clncl.
           perform rnl-fclients.
           if file-status not = zero display
             "CLIENT facture INEXISTANT , CLIENT :  " wclfac "  "
             "FACTURE NON TRAITEE, COMMANDE N. :  " fccle go to err.

DD0774* on sauvegarde le groupement
DD0774     move clrdi to wclrdi
DD0774     if wclrdi = "PFB" or "PTP" or "BRE"
DD0774        or "BRO" or "DPN" or "DPS" or "PUM" or "SFI" 
DD0774        or "OUT" or "DSC"
DD0351*anes 03/10/2014 Gestion maxeda comme pointP
DD0351        or "BBE" or "BCO" or "BKD" or "MXD" or "PLA" or "PRA" 
DD0774        move 3011003400107 to weme-pointp
DD0774     end-if

      *DDE125 prise du code EAN si pas trouve ds filiere
           if weamx = spaces
              move clean to weamx weandx weanx
           end-if
      *DDE125 ctrl validite du code ean
           if weamx = spaces or weamx not numeric or weam = zero display
             "FILIERE INCORRECTE , CLIENT :  " wclfac "  "
             "FACTURE NON TRAITEE, COMMANDE N. :  " fccle
             perform disp-fil go to err.

           move clnom to wrsm1.
           move clrss to wrsm2.
      *----> M1099 (D)
           if clrue = spaces display
             "ADRESSE INCORRECTE, CLIENT :  " wclfac "  "
             "FACTURE NON TRAITEE, COMMANDE N. :  " fccle go to err.
      *----> M1099 (F)
           move clrue to wrm1.
           move clvil to wrm2.
           move clbud to wvim.
           move clxcop to wcpm.
DDE125     move clsiret to wsiret
           move wclfac to clsncl.
           perform rnl-clisuite.
      *----> M0399 (D)
      *    if file-status not = zero go to p3.
      *    move clsntv to wicm.
           if file-status = zero
              move clsntv to wicm
           end-if

GPICMT*GPIWARNING pour BRICODEPOT on fait un test particulier car jusqu'a
GPICMT*GPIWARNING aujourd'hui(21/03/2006) leurs factures C)taient envoyees
GPICMT*GPIWARNING sur l'EAN de CASTORAMA 3020400000100, le parametrage est fait
GPICMT*GPIWARNING avec les caracteres 456 du code EAN ici 040, le probleme pour
GPICMT*GPIWARNING BRICODEPOT est que leur codes ean commence par 302040017
GPICMT*GPIWARNING comme castorama donc meme parametre 040
GPICMT*GPIWARNING pour depanner je vais tester le code 302040017
GPICMT*GPIWARNING plus le groupement pour verifier que c'est bien bricodepot
DD9999*  if weanx(1:9) = '302040017'
DD0351*             or = '302040019'
  "   *    if ofgcl-affi-grouph not = "BDP"
  "   *       display "Commande N.: " fccle  "  NON TRAITEE"
  "   *             " Code ean non bricodepot,groupe " ofgcl-affi-grouph
      *       go to err
  "   *     else
      *        move 3016016500103 to weandx
  "   *     end-if
  "   *  else
         move "DEMATDE" to wpgcle
         move wcnud     to wpgcnu
         move wpgcle    to pgcle
         perform rnl-paramgpi
         if file-status not = zero display
           "PARAMETRE INEXISTANT :  " wpgcle "  COMMANDE N. :  "
             fccle "   NON TRAITEE"
             go to err
         end-if
         move pgbar to weandx

GPICMT*DD0164 ajout code regroupement client suite a demande CASTORAMA de
GPICMT*       pouvoir leur donner le code magasin livre dans facture a
           perform rech-groupement
DD0221* GPIWARNING tant que l'on a pas agrandi la zone ds l'entete de commande
DD0221* GPIWARNING on prend la ref commande de fcommac1 pour BRICORAMA
GPICMT* GPIWARNING renseignee ==> facture en erreur
DD0221*    if ofgcl-affi-grouph = "BRM"  or = "BDP" or = "BTK"          *GPICMT
DD0221*    if ofgcl-affi-grouph = "BRM"  or = "BTK"                     *GPICMT
DD0221*                      or = "PLA"  or = "BKD" or = "LAP"          *GPICMT
DD0465*                      or = "C01" or = "SYU"                      *GPICMT
      *          perform rech-refcde
      *    end-if

           if wrcl2 = spaces display
             "COMMANDE SANS REFERENCE COMMANDE CLIENT:  " tscle
             "   NON TRAITEE"        go to err.

       p1b.
      *
GPICMT* recherche EAN du livre
           move wclliv to fincl
           perform rnlsk1-filieres.
           if file-status = zero go to p1bb.
      *----> M0299 (D)
      *GPIWARNING forcer le code ean client pour GPI
           if mmdt-societe = 'GPI'
DD0774        if fincl = wclient-spe1 move 3020400124600 to fiean
                             go to p1bb
              end-if
DD0774        if fincl = wclient-spe2 move 3025940007300 to fiean
                             go to p1bb
              end-if
           END-IF.
      *----> M0299 (F)
           display "FILIERE INEXISTANTE, CLIENT :  " wclliv "   "
             "COMMANDE N. :  " fccle.
           perform disp-fil.
       p1ba.
           move spaces to wealx
           display "ENTRER SON CODE EAN : <9999999999999>,"
                   "<S>= FAC SUIVANTE".
           accept wealx.
           if wealx = "s" or wealx = "S" go to err.
           if wealx = spaces or wealx not numeric or weal = zero
                         go to p1ba.
           go to p1bc.
       p1bb.
           move fiean to wealx.
           if wealx = spaces or wealx not numeric or weal = zero display
             "FILIERE INCORRECTE , CLIENT :  " wclliv "   "
             "FACTURE NON TRAITEE, COMMANDE N. :  " fccle
             perform disp-fil go to err.
       p1bc.
           if wcnudx = spaces go to p1c.
           if wcnud not = wcnudt go to p01.
       p1c.

           move wcnud to wcnudt.
           move fccle to wcle.
           move fcnfa to wnfa.
           move fcfoa to wfoa.
           move fcnumr to cemnum.
           move fcninr to cemind.

      *----> M0998 (D)
           move spaces to wexist.
      *----> M0998 (F)

      *----> M0498c (D)
           if fcfoa = 5 or fcfoa = 6 or fcfoa = 7 go to p1t.
           if fcnumr not = zero go to p1s.

DD0219* Favotex ne fait pas la saisie du transport
           if mmdt-societe = 'FAVOTEX'
              go to p1t
           end-if

           display "COMMANDE N.:  " wcle "  NUM. EXPEDITION = ZERO".
       p1f.
           move spaces to wexpedit.
DD0326     display "ENTRER LE NUMERO DE L'EXPEDITION: <99999999>,"
      *----> M0998 (D)
      *            "<S>= FACTURE SUIVANTE".
      *            "<L>=LETTRE, <S>= FACTURE SUIVANTE".
                   "<N>=SANS EXPEDITION, <S>= FACTURE SUIVANTE".
      *----> M0998 (F)
           accept wexpedit.
           if wexpedit = "S" or wexpedit = "s" go to err.
           if wexpedit = "N" or wexpedit = "n" go to p1t.
      *----> M0998 (D)
      *
      *************** accept date de livraison ******************
      *    if wexpedit not = "L" and wexpedit not = "l" go to p1h.
      *p1g.
      *    move spaces to wdatlx.
      *    display "ENTRER LA DATE DE LIVRAISON: JJMMAA".
      *    accept wdatlx.
      *    if wdatlx = spaces or wdatlx not numeric or
      *       wdatl9 = zero  go to p1g.
      *    if wal > 50 move 19 to dfsf
      *      else      move 20 to dfsf.
      *    move wal to cemdaa.
      *    move wml to cemdmm.
      *    move wjl to cemdjj.
      *    go to p1t.
      *p1h.
      *----> M0998 (F)
           if wexpedit = spaces or wexpedit not numeric go to p1f.
           move wexpedit to cemcle.
       p1s.
      *----> M0498c (F)
           perform rnl-expemois.
           if file-status not = zero display
             "expemois INEXISTANT :  " fcnumr fcninr "  COMMANDE N.: "
             wcle                    go to p1f.
      *----> M0998 (D)
      *    if file-status = zero go to p1t.
           move cemtre to tnntr.
           perform rnl-trpntran.
           if file-status not = zero move spaces to tnnom.
           move "O" to wexist.
      *----> M0998 (F)
      *----> M0498b (D)
       p1t.
      *----> M0798a (D)
      * pour les avoirs de redressement ou facture de redressement on demande
      * tjrs l'avoir ou la facture de reference
DD0221*    if wrclr = "R" go to p1u. TEST OBSOLETE
      *----> M0798a (F)
           IF fcfoa not = 5 and fcfoa not = 6 and fcfoa not = 7
                                          go to p2.

      *
      ***************** seulement pour les avoirs
      *
      ******** demande n.facture de reference
       p1u.
DD0423     move spaces to wrepx wnotedebit.
      *----> M0798a (D)
      *    display "NUMERO AVOIR :  " wnfa.
           if fcfoa = 5 or fcfoa = 6 or fcfoa = 7
                  display "NUMERO AVOIR :  " wnfa "  CDE N.:  " fccle
                          " Groupement " ofgcl-affi-grouph
             else display "NUMERO FACTURE :  " wnfa "  CDE N.:  " fccle
                          " Groupement " ofgcl-affi-grouph
           end-if
      *----> M0798a (F)
DD0316     display "ENTRER LE NUMERO DE FACT/AV. DE REFERENCE : 9999999"
                   " SANS REFERENCE : <NON>, <S>=FACTURE"
                   " SUIVANTE, <D>=NOTE DEBIT".
           accept wrepx.
GPICMT* pour LER on laisse la commande a traiter
DD0351*    if wrepx = "s" or wrepx = "S" move "8" to tsco3
DD0351     if wrepx = "s" or wrepx = "S"
  -           if ofgcl-affi-grouph not = "LER"
DD0423                         and not = "CAS"
DD0423                         and not = "MST"
DD0423                         and not = "BDP"
DD0351                         and not = "BMN"
DD0351                         and not = "AKI"
DD0351                         and not = "BRM"
DD0351                         and not = "BTK"
DD0351                         and not = "BAO"
DD0351                         and not = "AUC"
                 move "8" to tsco3
DD0351        end-if
              go to p21.
           if wrepx = "NON" or wrepx = "non" move zero to wrep9
DD0423                                                    wnotedebit
GPICMT* on teste si avoir CASTORAMA ==> on passe a la facture suivante, car les
GPICMT* avoirs ne faisant pas reference a une facture sont rejetes par CASTO
GPICMT*       if ofgcl-affi-grouph = "CAS"
      *          display "Avoir CASTORAMA " wnfa
      *                  " Sans reference facture non traite"
      *          move spaces to wrepx
      *          display "Entrez pour continuer"
      *          accept wrepx
      *          move "8" to tsco3
      *          go to p21
      *       else
                 go to p2
      *       end-if
           end-if

GPICMT* traitement sans facture de reference mais avec note de debit
DD0423     if wrepx = 'D' or = 'd'
  -           perform saisie-notedebit
  -           if wnotedebit = "s" or = "S"
  -              if ofgcl-affi-grouph not = "LER"
  -                            and not = "CAS"
  -                            and not = "MST"
  -                            and not = "BDP"
DD0351                         and not = "BMN"
DD0351                         and not = "AKI"
DD0351                         and not = "BRM"
DD0351                         and not = "BTK"
DD0351                         and not = "BAO"
DD0351                         and not = "AUC"
                    move "8" to tsco3
  -              end-if
  -              go to p21
  -           else
  -              go to p2
  -           end-if
DD0423     end-if

           if wrepx = spaces or wrepx not numeric go to p1u.
           move spaces to wokx.
           display "FACTURE DE REFERENCE : " wrepx "  TAPER OUI POUR CON
      -    "FIRMER".
           accept wokx.
           if wokx not = "OUI" and wokx not = "oui" go to p1u.
      *----> M0498b (F)

      *
      ***** CREATION FICHIER EMISSION
      *
       p2.
      **************  recherche du livre a ********************
           move spaces to wadrliv.
           move wclliv to clncl.
           perform rnl-fclients.
           if file-status not = zero display
             "CLIENT livre INEXISTANT , CLIENT :  " wclliv "  "
             "FACTURE NON TRAITEE, COMMANDE N. :  " fccle go to err.
           move clnom to wrsl1.
           move clrss to wrsl2.
           move clrue to wrl1.
           move clvil to wrl2.
           move clbud to wvil.
           move clxcop to wcpl.

DD0777**************  recherche du client final si renseigne ********************
           if wclNCL-FINAL = spaces or = zero
              go to p2cdepar
           end-if
           move wclNCL-FINAL to fincl 
           perform rnlsk1-filieres 
           if file-status = zero
              move fiean to weacx weanfinal
              if weacx = spaces or weacx not numeric or weac = zero 
                 display "FILIERE INCORRECTE , CLIENT :  " wclcde "  "
                 "FACTURE NON TRAITEE, COMMANDE N. :  " fccle
                 perform disp-fil go to err 
              end-if
              go to p2suite
           end-if

           display "FILIERE INEXISTANTE, CLIENT :  " wclNCL-FINAL 
             "  " "COMMANDE N. :  " fccle 
         .
       p2clf.
           move spaces to weacx 
           display "ENTRER SON CODE EAN : <9999999999999>,"
                   "<S>= FAC SUIVANTE" 
           accept weacx 
           if weacx = "s" or weacx = "S" 
              go to err 
           end-if
           if weacx = spaces or weacx not numeric or weac = zero
              go to p2clf 
           end-if
         .
       p2suite.
           move wclNCL-FINAL to clncl 
           perform rnl-fclients 
           if file-status not = zero 
              display "CLIENT Final INEXISTANT , CLIENT :  " 
              wclNCL-FINAL "  "
              "FACTURE NON TRAITEE, COMMANDE N. :  " fccle 
              go to err 
           end-if
      *
      **************  recherche du commande par ********************
      *
         .
       p2cdepar.
           move wclcde to fincl.
           move spaces to wadrcde.
           perform rnlsk1-filieres.
           if file-status = zero go to p2j.
           display "FILIERE INEXISTANTE, CLIENT :  " wclcde  "  "
             "COMMANDE N. :  " fccle.
       p2i.
           move spaces to weacx.
           display "ENTRER SON CODE EAN : <9999999999999>,"
                   "<S>= FAC SUIVANTE".
           accept weacx.
           if weacx = "s" or weacx = "S" go to err.
           if weacx = spaces or weacx not numeric or weac = zero
                         go to p2i.
           go to p2k.
       p2j.
           move fiean to weacx.
           if weacx = spaces or weacx not numeric or weac = zero display
             "FILIERE INCORRECTE , CLIENT :  " wclcde "  "
             "FACTURE NON TRAITEE, COMMANDE N. :  " fccle
             perform disp-fil go to err.
       p2k.
           move wclcde to clncl.
           perform rnl-fclients.
           if file-status not = zero display
             "CLIENT commande INEXISTANT , CLIENT :  " wclcde "  "
             "FACTURE NON TRAITEE, COMMANDE N. :  " fccle go to err.
           move clnom to wrsc1.
           move clrss to wrsc2.
           move clrue to wrc1.
           move clvil to wrc2.
           move clbud to wvic.
           move clxcop to wcpc.
           move wclcde to clsncl.
           perform rnl-clisuite.
           if file-status not = zero go to p3.
           move clsntv to wicc.
      *----> M0399 (F)
      *
      ***** creation E1
      *
       p3.
GPICMT* initialisation flag creation ligne article
DD0423     move spaces to w-flag-ligne

           move spaces to wor-emission2.
           move "E1" to dfte.
           IF FCFOA = 5 OR FCFOA = 6 OR FCFOA = 7 MOVE "AVO" TO dftdoc
             else                                 MOVE "FAC" TO dftdoc.
           move "OR"  to dfcof.
           move weme  to dfeme.
           move weand to dfdes.
           move fcnfa to wnfac.
           move wnfacx to dfndf.
           if fbdfa > 50 move 19 to dfsf1
             else        move 20 to dfsf1.
           move fbdfa to dfaf1.
           move fbdfm to dfmf1.
           move fbdfj to dfjf1.
DD9999* anes 23/03/17 stockage date facturation pour ctrl sur segment D3
DD9999     move dfdfa1 to wdfdfa1
DD0465     if mmdt-societe not = 'PLASTO'
  -           if wfoa = 5 or wfoa = 6 or wfoa = 7
  -              go to p3b
  -           end-if
DD0465     end-if
           if cemdaa > 50 move 19 to dfsl
             else         move 20 to dfsl.
           move cemdaa to dfal.
           move cemdmm to dfml.
           move cemdjj to dfjl.
GPICMT* pour certains client(Casino) ils veulent la date d'expedition
GPICMT* si a zero on met la date de facture
DD0465     if CEMDAJ = zero or space
  -           move DFDFA1 to DFDLI
DD0465     end-if
           .
       p3b.
DD9999* anes 29/11/2018 on met une date de livraison meme pour les avoirs
  |   * en particulie pour Casino. Pas d'incidence pour les autres clients
  |        if dftdoc = "AVO" 
  |        and ( DFDLI = zero or DFDLI = space )
  |          move DFDFA1 to DFDLI
DD9999     end-if
GPICMT*DDE125 ajout NEGOCE si client cde = client facture
GPICMT*DDE125 ajout MANDAT si client cde # client facture
GPICMT     if tscpa = weam
GPICMT        move 'NEGOCE' to dfnegoce
GPICMT     else
GPICMT        move 'MANDAT' to dfnegoce
           end-if

           move ofgcl-affi-grouph   to dfrdi

           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION E1 INVALIDE, STATUS :  " file-status
                                     go to fin.
GPICMT* raz numero de ligne facture en debut de facture
DD0350     move zero to wnlfa.
DD0465     move zero to wnlcd.
      *
      ***** creation CE
      *
       p4.
**************     lecture    des elements 01 : libelles  ************
           move fccle  to pfjcle1.
           perform rnl-fcommac1.
           if file-status not = zero
DDE153*         GO TO p5.
DDE153          GO TO p4-s.

           IF pfjli1 = SPACE AND pfjli2 = SPACE
DDE153*              AND pfjli3 = SPACE AND pfjli4 = SPACE GO TO p5.
DDE153               AND pfjli3 = SPACE AND pfjli4 = SPACE GO TO p4-s.
           move spaces  to wor-emission2.
           move "CEPUR" to wor-emission2.
           move pfjli1 to dfco1.
           move pfjli2 to dfco2.
           move pfjli3 to dfco3.
           move pfjli4 to dfco4.
GPICMT* reclasser les commentaires si precedent vide
DD0221     perform tri-comment
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CE INVALIDE, STATUS :  " file-status
                                     go to fin.
DD0351* anes 15/01/2015 ajout des commentaires pour Leroy Merlin sites de
  |   * ste marie et st louis --> CESUREXONERATION, ARTICLE 262 I DU CGI
  |   *GPIWARNING voir s'il est possible de ne pas hard-coder les codes clients
  |   *GPIWARNING EAN en testant d'autres infos (ex: si LER et DOMTOM...?)
  |        if weacx = "3025940029500" or "3025940029200" 
  |          move spaces  to wor-emission2
  |          move "CESUR" to wor-emission2
  |          move "EXONERATION, ARTICLE 262 I DU CGI" to dfco1
  |          perform w-emission
  |          if file-status not = zero display
  |            "ECRITURE EMISSION CE 2 INVALIDE, STATUS :  " file-status
  |                                    go to fin
  |          end-if
DD0351     end-if.

DDE153 p4-s.
DDE153* lecture cde suivante si regroupement a voir GPIWARNING
           if fbncd not = zero go to p5.

      *    perform lec-cde
      *    IF w-retour = 9
      *       go to fin
      *    else
      *       if w-retour = 1
      *          go to p4
      *       end-if
      *    END-IF
           .

      *
      ***** creation E2
      *
       p5.
           move spaces  to wor-emission2.
           move "E2"    to dfte.
           move weam    to dfeam.
           move wclfac  to dfinm.
           move wrsm1   to dfrsm1.
           move wrsm2   to dfrsm2.
           move wrm1    to dfrm1.
           move wrm2    to dfrm2.
           move wvim    to dfvim.
           move wcpm    to dfcpm.
           move "FR"    to dfpam.
           move wicm    to dficm.
DDE125     move wsiret  to dfsiretfac
      *----> M0399 (D)
           move weac    to dfeaa.
           move wclcde  to dfina.
           move wrsc1   to dfrsa1.
           move wrsc2   to dfrsa2.
           move wrc1    to dfra1.
           move wrc2    to dfra2.
           move wvic    to dfvia.
           move wcpc    to dfcpa.
           move "FR"    to dfpaa.
           move wicc    to dfica.
      *----> M0399 (F)
DD0774* si groupement Poibnt P et Plateforme du batiment on force le GLN de GPI
           if wclrdi = "PFB" or "PTP" or "BRE"
DD0774        or "BRO" or "DPN" or "DPS" or "PUM" or "SFI" 
DD0774        or "OUT" or "DSC"
DD0351*anes 03/10/2014 Gestion maxeda comme pointP
DD0351        or "BBE" or "BCO" or "BKD" or "MXD" or "PLA" or "PRA" 
              move weme-pointp to dfeaf
           else
              move weme    to dfeaf 
           end-if
           move wffp01  to dfrsf1.
           move wffp02  to dfrsf2.
           move wffp03  to dfrf1.
           move wffp04  to dfrf2.
           move wffp05  to dfvif.
           move wffp061 to dfcpf.
           move wffp062 to dfpaf.
           move wffp063 to dficf.
           move wffp071 to dfctf.
           move wffp072 to dftef.
           move wffp073 to dffxf.
           if wsie = weme go to p5a.
           move wsie    to dfeas.
           move wass01   to dfrss1.
           move wass02  to dfrss2.
           move wass03  to dfrs1.
           move wass04  to dfrs2.
           move wass05  to dfvis.
           move wass061 to dfcps.
           move wass062 to dfpas.
           move wass063 to dfics.
       p5a.
           move wmoe01  to dffju.
           move wmoe02  to dfcap.
           move wmoe03  to dfmca.

GPICMT* sur demande bao on passe le capital et la devise dans le meme champ
DD0351     move spaces to wmmaf-ubla
DD0351     string wmoe02 ' ' wmoe03
  -           delimited size into wmmaf-ubla
  -        call 'mmaf-ubla1' using mmaf-ubla adl-art
DD0351     move wmmaf-ubla to DFCAPITALDEV

           move wmoe04  to dfrdc.
           move wmoe05  to dfsir.
           move wmoe06  to dfape1.
           move weal    to dfeal.
           move wclliv  to dfinl.
           move wrsl1   to dfrsl1.
           move wrsl2   to dfrsl2.
           move wrl1    to dfrl1.
           move wrl2    to dfrl2.
           move wvil    to dfvil.
           move wcpl    to dfcpl.
           move "FR"    to dfpal.
           move weanre  to dfear.
DD0777     move weanfinal to DFEANCLFIN
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION E2 INVALIDE, STATUS :  " file-status
                                     go to fin.
      *
      ***** creation E3
      *
       p6.
           move spaces to wor-emission2.
           move "E3"   to dfte.
      *----> M0798a (D)
DD0221*    if wrclr = "R" go to p6c0. TEST OBSOLETE
      *----> M0798a (F)
           move "CDE"  to dftci.
           move tscpa  to dfeci.
           move fcncl  to dfici.
DD0221*    move fcrcl  to wrclx.
           move wrcl2  to dfnci.
           move fcdcd to wdcdc.
           if fcnin = zero go to p6a.
           move zero to fcnin.
           perform rnl-fcommaap.
           if file-status not = zero go to p6a.
           move fcdcd to wdcdc.
       p6a.
           if tsdin = zero go to p6b.
           if tsdina > 50 move 19 to dfsi
             else         move 20 to dfsi.
           move tsdina to dfai.
           move tsdinm to dfmi.
           move tsdinj to dfji.
           go to p6c.
       p6b.
           if wdca > 50 move 19 to dfsi
             else       move 20 to dfsi.
           move wdca to dfai.
           move wdcm to dfmi.
           move wdcj to dfji.
       p6c.
           move "AVI" to dftbi.
           move weme  to dfebi.
           move wcle  to wnbi.
           move zero  to wnbi2.
           move wnbi  to dfnbi.
DDE125* chargement delai initial de la commande
      *    if wdca > 50 move 19 to dfsb
      *      else       move 20 to dfsb.
      *    move wdca to dfab.
      *    move wdcm to dfmb.
      *    move wdcj to dfjb.
           move 20    to dfsb
           move fcaa  to dfab
           move fcmm  to dfmb
           move fcjj  to dfjb

      *----> M0798a (D)
      *    IF WFOA not = 5 and WFOA not = 6 and WFOA not = 7 go to p6i.
           IF WFOA not = 5 and WFOA not = 6 and WFOA not = 7
DD0221*                    and wrclr not = "R" 
              go to p6i.
       p6c0.
      *----> M0798a (F)
      *
      ***************** seulement pour les avoirs ou fac/av de redressement
      *
      *----> M1298 (D)
DD0423     if wrep9 = zero and wnotedebit = zero go to p6i.
      *----> M1298 (F)

           move "FAC"  to dftfi.
      *----> M0498 (D)
      *    move weam   to dfefi.
           move weme   to dfefi.
      *----> M0498 (F)
           move wclfac to dfifi.
GPICMT* on teste si on a une saisie une note de debit ou une facture
DD0423     if wrepx = 'D' or = 'd'
  -   *       move wnotedebit to wnfarx
  -   *       move wnfarx to dfnfi
DD0420        move wnotedebit to dfida
  -           go to p6f
  -        else
  -           move wrep9  to wnfar
DD0420        move wnfarx to dfnfi wnotedebit
DD0423     end-if
      *
      ************** recherche date facture initiale *******************
      *----> M1298 (D)
      *----> M0698(D)
      *    if wrep9 = zero move zero to dfsf dfaf dfmf dfjf
      *                    go to p6i.
      *----> M0698(F)
      *----> M1298 (F)
           move wrep9 to fbnfa.
           perform rnl-ffacture.
           if file-status = zero go to p6h.
      *
      *************** accept date de facture initiale ******************
       p6f.
           move spaces to wdatfx.
DD0420     display "FACTURE/NOTE DEBIT DE REFERENCE :  " wnotedebit.
           display "ENTRER DATE DE LA FACTURE/NOTE DEBIT DE REFERENCE :"
                   " JJMMAA".
           accept wdatfx.
           if wdatfx = spaces or wdatfx not numeric or
              wdatf9 = zero  go to p6f.
           if wa > 50 move 19 to dfsf
             else     move 20 to dfsf.
           move wa to dfaf.
           move wm to dfmf.
           move wj to dfjf.
           go to p6i.
       p6h.
           if fbdfa > 50 move 19 to dfsf
             else        move 20 to dfsf.
           move fbdfa to dfaf.
           move fbdfm to dfmf.
           move fbdfj to dfjf.
       p6i.
      *----> M0798a (D)
DD0221* ELGU je ne fait plus le test ci-dessous fait pour LER mais plus actif
DD0221     go to p6i0
DD0221*    if wrclr not = "R" go to p6i0. TEST OBSOLETE
           move "RED"  to dftda.
           move weam   to dfeda.
DD0420*    move wclfac to dfida.
           move wrcl2  to dfnda.
           if wrcm > wdmm subtract 1 from wanc9 giving wanr9
             else         move            wanc9 to     wanr9.
           move wsr    to dfsd.
           move war    to dfad.
           move wrcm   to dfmd.
           move wrcj   to dfjd.
       p6i0.
      *----> M0798a (F)
DDE011*    move "FRF" to dfcmf.
DDE011     move wmmpa-devi-cdev to dfcmf.
           move 1     to dftdc.
           move "5"   to dfdrp.
           move "3"   to dfcrd.
           move "D"   to dftdp.
           if wfbde1 not = zero go to p6iz.
           move wfbdfa to cadata wfa.
           move wfbdfm to cadatm wfm.
           move wfbdfj to cadatj wfj.
      *----> M0599 (D) GPI2000 door
           if CADATA > 50 move 19 to CADATS
             else                  move 20 to CADATS.
      *----> M0599 (F) GPI2000 door
           if wfa > 50 move 19 to wfs
             else      move 20 to wfs.
           perform rnl-multidat.
           if file-status not = zero go to p6if.
      *----> M1299 (D)
      *    move carang to wrang.
      *    if wr2 > 365 add 1 to wr1
      *                 subtract 365 from wr2.
      *    move wrang to carang.
      *----> M1299 (F)
           perform snlsk3-multidat.
      *----> M0999 (D)
           if file-status not = zero go to p6if.
      *----> M0999 (F)
       p6ia.
           perform rnl-multidat.
           if file-status not = zero go to p6if.
           if cavajo not = 1 go to p6ia.
           move cadata to wfbd1a.
           move cadatm to wfbd1m.
           move cadatj to wfbd1j.
           go to p6iz.
      *
      ********* demande saisie date echeance
       p6if.
           move spaces to wdatecx.
           display "FACTURE :  " wnfa "  date facture:  " wfbdaf.
           display "ENTRER LA DATE D ECHEANCE :  (JJMMAA)".
           accept wdatecx.
           if wdatecx = spaces or wdatecx not numeric go to p6if.
           if wecm < 1 or wecm > 12 go to p6if.
           if wecj < 1 or wecm > 31 go to p6if.
           move wecj to wej wfbd1j.
           move wecm to wem wfbd1m.
           move weca to wea wfbd1a.
           if wea > 50 move 19 to wes
             else      move 20 to wes.
           move spaces to wokx.
           display "DATE ECHEANCE RETENUE :  " wdatecx "  ENTRER OUI PO
      -    "UR CONFIRMER".
           accept wokx.
           if wokx not = "OUI" and wokx not = "oui" go to p6if.
           if wdatef9 > wdatee9 go to p6if.
       p6iz.
      *
      ************* calcul nbre de jours entre date facture et echeance

      * init zone memo saisie nbre de jour entre date facture et echeance
           move spaces to wsnbj.                                        *M0800

DD0351* suite a un probleme calcul echeance les factures qui ont la meme date de facture et d'echeance sont en ereur
DD0351* je fais donc le meme trt dans ts les cas
DD0351*    if wfbdaf = wfbde1 go to p6o.
           move wfbdfa to cadata.
           move wfbdfm to cadatm.
           move wfbdfj to cadatj.
      *----> M0599 (D) GPI2000 door
           if CADATA > 50 move 19 to CADATS
             else                  move 20 to CADATS.
      *----> M0599 (F) GPI2000 door
           perform rnl-multidat.
           if file-status = zero move carang to wnbjf go to p6l.
       p6j.
      * saisie nbre de jour entre date echeance et date facture
      * init a "1" zone memo saisie nbre de jour entre date facture et echeance
           display "DATE FACTURE INEXISTANTE AU CALENDRIER".            *M0800
           move "1" to wsnbj.                                           *M0800

           move zero to wnbjf.
           move spaces to wnbjex.
           display "FACTURE NUMERO  :  " wnfa.
           display "DATE DE FACTURE :  " wfbdfj wfbdfm wfbdfa.
           display "DATE ECHEANCE   :  " wfbd1j wfbd1m wfbd1a.
           display "ENTRER LE NOMBRE DE JOUR ENTRE DATE DE FACTURE ET DA
      -    "TE ECHEANCE :  999".
           accept wnbjex.
           if wnbjex = spaces or wnbjex not numeric go to p6j.
       p6l.
           if wfbd1a > 50 move 19 to dfsec
             else         move 20 to dfsec.
           move wfbd1a to cadata dfaec.
           move wfbd1m to cadatm dfmec.
           move wfbd1j to cadatj dfjec.
      *----> M0599 (D) GPI2000 door
           if CADATA > 50 move 19 to CADATS
             else                  move 20 to CADATS.
      *----> M0599 (F) GPI2000 door
           perform rnl-multidat.
           if file-status = zero go to p6lf.
       p6lb.

      *----> M0800 (D)
      * saisie nbre de jour entre date echeance et date facture
      * init a "2" zone memo saisie nbre de jour entre date facture et echeance
           display "DATE ECHEANCE INEXISTANTE AU CALENDRIER".
           move "2" to wsnbj.
           move zero to wnbjf.
           move spaces to wnbjex.
           display "FACTURE NUMERO  :  " wnfa.
           display "DATE DE FACTURE :  " wfbdfj wfbdfm wfbdfa.
           display "DATE ECHEANCE   :  " wfbd1j wfbd1m wfbd1a.
           display "ENTRER LE NOMBRE DE JOUR ENTRE DATE DE FACTURE ET DA
      -    "TE ECHEANCE :  999".
           accept wnbjex.
           if wnbjex = spaces or wnbjex not numeric go to p6lb.
      *----> M0800 (F)

           move wfbd1a to wdesa wpena.
           move wfbd1m to wdesm wpenm.
           if wfbd1j > 1 subtract 1 from wfbd1j giving wdesj go to p6ld.
           move 30 to wdesj.
           if wfbd1m > 1 subtract 1 from wfbd1m giving wdesm go to p6ld.
           move 12 to wdesm.
           if wfbd1a not > 50 move 99 to wdesa
             else          subtract 1 from wfbd1a giving wdesa.
       p6ld.
           if wfbd1j < 31 add 1 to wfbd1j giving wpenj go to p6lf.
           move 01 to wpenj.
           if wfbd1m < 12 add 1 to wfbd1m giving wpenm go to p6lf.
           move 01 to wpenm.
           add 1 to wfbd1a giving wpena.
       p6lf.
      *----> M0800 (D)
      * si zone memo nbre de jour saisi a "2" on charge le nbre de jour ds
      * wnbjf et on va mettre a jour les zones de l'enregistrement puisque
      * les zones escompte et penalite ont deja ete calculees
           if wsnbj = "2" go to p6o.
      * si zone memo nbre de jour saisi a "3" ==>les zones escompte et penalites
      * sont renseignees et le nbre de jour a calculer
           if wsnbj = "3" go to p6n.
      *----> M0800 (F)

           move carang to wnbje.
       p6l1.
      * calcul date d'escompte
           perform prnl-multidat.

      * si lecture invalide ==> on va mettre mettre a jour les zones penalites
      *    if file-status not = zero go to p6ld.
           if file-status not = zero move "3" to wsnbj go to p6ld.      *M0800

           if cavajo not = 1 go to p6l1.
           move cadata to wdesa.
           move cadatm to wdesm.
           move cadatj to wdesj.
       p6l5.
           move wfbd1a to cadata.
           move wfbd1m to cadatm.
           move wfbd1j to cadatj.
      *----> M0599 (D) GPI2000 door
           if CADATA > 50 move 19 to CADATS
             else                  move 20 to CADATS.
      *----> M0599 (F) GPI2000 door
           perform rnl-multidat.

      * si lecture invalide ==> on va mettre mettre a jour les zones penalites
      *    if file-status not = zero go to p6ld.
           if file-status not = zero move "3" to wsnbj go to p6ld.      *M0800
       p6l6.
           perform nnl-multidat.

      * si lecture invalide ==> on va mettre mettre a jour les zones penalites
      *    if file-status not = zero go to p6ld.
           if file-status not = zero move "3" to wsnbj go to p6ld.      *M0800
           if cavajo not = 1 go to p6l6.
           move cadata to wpena.
           move cadatm to wpenm.
           move cadatj to wpenj.

      * si nbre de jour saisi ==> mise a jour zones enregistrement
      *    go to p6n.                                                   *M0800
           if wsnbj = "1" go to p6o.                                    *M0800

       p6n.
           if wfbdfa not = wfbd1a go to p6n1.
           if wnbjf < wnbje subtract wnbjf from wnbje giving wnbjf.
           go to p6o.
       p6n1.
           subtract wnbjf from 365 giving wnbjf.
           add wnbje to wnbjf.
       p6o.

      * si nbre de jour saisi "1" ou "2" on met wnbje ds wnbjf
           if wsnbj = "1" or = "2" move wnbje to wnbjf.                 *M0800

           move wnbjf to dfnbp.
           move 5 to dfdre.
           move 3 to dfcre.
           move "D" to dftde.
           if wnbjf > zero subtract 1 from wnbjf giving wdfnbe
                           move wdfnbe to dfnbe.
           if wdesa > 50 move 19 to dfses
             else        move 20 to dfses.
           move wdesa to dfaes.
           move wdesm to dfmes.
           move wdesj to dfjes.
           move wtesc1 to dftesc.
           move wcle  to fccle4.
           move  7    to fcnel4.
           move zero  to fcunix4.
           perform snl-fcommac4.
           if file-status not = zero  go to p6r.
           perform nnl-fcommac4.
           if file-status not = zero  go to p6r.
           if fcnoc4 not = wcle       go to p6r.
           if fcnel4 not = 7 go to p6r.
           move zero to dftesc.
       p6r.
           move wesct1 to dftxe1.
           move wesct2 to dftxe2.
           move wesct3 to dftxe3.
      ***************************** penalite
           move "5"    to dfdpe.
           move "3"    to dfcpe.
           move "D"    to dftpe.
           add 1 to wnbjf giving wdfnbe
           move wdfnbe to dfnpe.
           if wpena > 50 move 19 to dfspe
             else        move 20 to dfspe.
           move wpena  to dfape.
           move wpenm  to dfmpe.
           move wpenj  to dfjpe.
           move wtesc2 to dftpen.
           move wpent1 to dftxp1.
           move wpent2 to dftxp2.
           move wpent3 to dftxp3.
      ***************************** transport principal
           if wfoa not = 5 and wfoa not = 6 and wfoa not = 7
      *----> M0998 (D)
      *      ne transferer les zones "ce" suivantes que si expemois trouve
                           and wexist = "O"
                move tnnom  to dfntl
      *----> M0998 (F)
                move cemrex to dfnrt
                move cemtre to dfctl
                move cemnco to dfnco
                move cempre to wpds
                move wpds   to dfpds.
           move "9"    to dfcmt.
      *    if wexist not = "O" move fcntr  to dfntl.
           move 6 to dfcdl.
           if fcfra = 1 move 3 to dfcdl.
           if fcfra = 3 move 4 to dfcdl.
           move "CT"    to dfemb.
      *    move wpds to dfpds.

GPICMT*DDE125 pour domaxel on met le net a payer ds E3 (considere ici comme
GPICMT*       montant de l'echeance) GPIWARNING attention si multiecheance
           move wfbnpf to wc122.
           move wc122  to dfmech1.

      * anes 2/12/2014 reactivation des 3 lignes ci-dessous pour test avec LCL
      *GPICMT GPIWARNING: changer aussi ce numero dans cglp-fact1 et cglp-factr
DD0771     if clrdi = "LCL" and mmdt-societe = "GPI"
DD0771*       move "2015-02808-42 " to dfaccord
DD0351*       move "2016-02808-42 " to dfaccord
DD9999*       move "2019-02808-42-N " to dfaccord
DD9999        move "2020-02808-42-N " to dfaccord
DD0771     end-if
DD2063*    if clrdi = "UGD" 
  |   *      evaluate mmdt-societe 
  |   *       when "GPI"
  |   *        move "738_GPI" to dfaccord
  |   *       when "DINAC"
  |   *        move "713_DINAC" to dfaccord
  |   *      end-evaluate
DD2063*    end-if

DD9999* GPIWARNING correction temporaire de date pour centrale UGD 
DD9999* anes 23/03/17 pour UGD et PAI (temporaire !!??) : 
  |   * si la date de BL est posterieure a la date de facturation,
  |   * on met la date de facturation dans la date de BL
DD9999* anes 01/04/20 controle valable pour tout le monde 
  |   *    if clrdi = "UGD" or "PAI" 
  |   *    or clrdi = "ITA"
  |          if dfdbi > wdfdfa1
  |            move wdfdfa1 to dfdbi
  |          end-if
DD9999*    end-if

           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION E3 INVALIDE, STATUS :  " file-status
                                     go to fin.
      *
      ***** creation RG
      *
       p7.
**************  lecture    des elements 05 A 09                *******
           move zeroes to fccle4 wht0.
           move wcle   to fcnoc4.
           move 05     to fcnel4.
           move zeroes to fcunix4.
           perform snl-fcommac4.
           if file-status not = zero
DDE153*         GO TO p8.
DDE153          GO TO p7-f.
       p7a.
           perform nnl-fcommac4.
           if file-status not = zero
DDE153*         GO TO p8.
DDE153*    if fcnoc4 not = wcle go to p8.
DDE153*    IF FCNEL4 > 9 GO TO p8.
DDE153          GO TO p7-f.
DDE153     if fcnoc4 not = wcle go to p7-f.
DDE153     IF FCNEL4 > 9 GO TO p7-f.
DDE153*    if wfbfra = 2 and fcnel4 = 6 go to p7a.
DDE153     if fcfra = 2 and fcnel4 = 6 go to p7a.
      *----> M0998 (D)
DDE153*    if wfbfra = 2 and fcnel4 = 8 go to p7a.
DDE153     if fcfra = 2 and fcnel4 = 8 go to p7a.
      *----> M0998 (F)
           move spaces to wor-emission2.
           move "RG"   to dfte.
DDE125* GPICMT pour bricodepot les codes type ligne montant correspondent a
GPICMT* si ligne montant ou frais : 1=horsfacture,6=frais regle par acheteur
GPICMT* si ligne remise             1=hors facture,2=deduit de la facture
GPICMT*                             5=frais a regler par le vendeur
           if fcsig = "-" move "A"  to dfiarg
                          move "QD" to dfparg dfparg-long
                          move "2"  to dfrarg
                          move 3001000001025 to dfnarg
             else         move "C" to dfiarg
DDE125                    move "6"  to dfrarg
                          move 3001000001001 to dfnarg
                          move "FC" to dfparg dfparg-long.
      *----> M1098 (D)
           if fcsig not = "-" and fcnel4 not = 6 and fcnel4 not = 8
                          move "FI" to dfparg dfparg-long.
      *----> M1098 (F)
           if fcdop = "REMISE D'OUVERTURE" move "DI" to dfparg
                                                        dfparg-long.
           if fcnel4 = 5 or fcnel4 = 9 move "3001000009991" to dfnarg.

      *----> M0798 (D)
           if fcdop = "H.T. INSUFFISANT : MAJORATION"
                                       move "3001000001100" to dfnarg.
      *----> M0798 (F)

           move "01"   to dfcarg.
           move "0" to dfttarg.
           move "00.00" to dftxarg.
           MOVE FCDOP  TO dflarg.
           if fcnel4 = 7 move fcqul to wc022
                         move wc022 to dftarg
                         move fcmon to wc122
                         move wc122 to dfmarg
                         move fcpuh  to wc122
                         move wc122  to dfbarg
DD0126* ajout taxe
                         move fctvp4 to wcodtax1
                         perform chgt-taxe
                         go to p7c.
           if fcnel4 not = 5 move fcmon to wc122
                             move wc122 to dfmarg
                             go to p7b.
           if fcmon = zero MULTIPLY FCQUL BY FCPUH GIVING wc122
             else          move fcmon  to wc122.
           move wc122  to dfmarg.
       p7b.
           if fcnel4 not = 5 and fcnel4 not = 6 and fcnel4 not = 9
                       go to p7c.
           move "1" to dfttarg.
           move fctvp4 to wcodtax1.
           IF WTV1 = ZERO move "0" to dfttarg
                          move "00.00" to dftxarg
                          GO TO p7c.
           IF WTV1 = 9 MOVE 5    TO I
             ELSE           MOVE WTV1  TO I.

      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
           IF WFBTAX = 1 or = 5
                         MULTIPLY fcmon BY WTVA1 (I) GIVING WCALC
                         move wtva1 (i) to wc022
                         move wc022 to dftxarg
             ELSE        MULTIPLY fcmon BY WTVA2 (I) GIVING WCALC
                         move wtva2 (i) to wc022
                         move wc022 to dftxarg.
           DIVIDE 100 INTO WCALC  ROUNDED.
           move wcalc to wc122.
           move wc122 to dfmtarg.
       p7c.
           if fcnel4 = 8 add fcmon to wht0.

      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
           if wfbtax not = 1 and wfbtax not = 2 and
                     not = 5 and        not = 6 move "0" to dfttarg
                                                move "00.00" to dftxarg.
GPICMT* spec pour LER
DD0420     if tsrdi = 'LER'
  -           perform spec-ler
DD0420     end-if

GPICMT* pour BRICORAMA on efface le taux de remise, ils ne veulent que le montant
DD0420     if tsrdi = 'BRM' or = 'BTK'
  -           move spaces to DFTARG
DD0420     end-if

GPICMT* pour @gp si code remise DI mettre TD seul @gp utilise la zone dfparg-long
           if dfparg-long = "DI"
              move "TD" to dfparg-long
           end-if
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION GR INVALIDE, STATUS :  " file-status
                                     go to fin.
           go to p7a.

DDE153* lecture cde suivante si regroupement
       p7-f.
           IF fbncd = zero
              perform lec-cde
              if w-retour = 9
                 go to fin
              else
                 if w-retour = 1
                    go to p7
                 end-if
              end-if
           END-IF
           .

      *
      ***** creation LA
      *
       p8.
**************  lecture    des elements 04 : lignes articles   *******
           move wcle to fcnoc2.
           move zero to fclig.
           move spaces to fcart wwwcle.
DD0350*    move zero to wnlfa.
           perform snlsk3-fcommac2.
           if file-status not = zero
DDE153*         GO TO p10.
DDE153          GO TO p9-f.
       p8a.
           perform nnl-fcommac2.
DDE089*    if file-status not = zero move all "9" to wwwcle
DDE089     if file-status not = zero
DDE153*         GO TO p10.
DDE153          GO TO p9-f.
DDE153*    if fcnoc2 not = wcle go to p10.
DDE153     if fcnoc2 not = wcle go to p9-f.
DDE089     if fcqtl = zero go to p8a.
       p8b.
DDE089*    move fcart to xart.
DDE089*    if wwwcle = all "9" go to p9.
       p8c.
      *
      **** DEBUT ARTICLE ****
      *
           MOVE ZERO   TO WQTELIV wqtecde.
           MOVE FCNEL2 TO WNEL.
           MOVE FCLIG  TO WNLIG.
           MOVE FCNAR  TO WART1.
           MOVE FCDES  TO WDES.
           move fcsrc  to wdes2.
           MOVE FCMES  TO WMES.
           MOVE FCPRX  TO WPRX.
           MOVE FCQPB  TO WQPB.
           MOVE FCPHT  TO WPHT.
           MOVE fcpbas TO WPHTB.
           move fcvl   to wvlfa.
           move fcva   to wvafa.
           move fctrpv to wtrpv.
           move spaces to fakle1.
           move 01     to fanma1.
           move fcnar  to fanar1.
           move fcsre  to fansr1.
           perform rnl-fartusac.
           if file-status not = zero move "0000000000000" to wgencod
             else                    move fapay  to WG3
                                     move facnu  to WCNU
                                     move facip  to WCIP
                                     move facle  to WKLE.
           move wgencod to weanarx.
           IF FCQTL = ZERO GO TO p8e.
      *
      ** MVTS ARTICLE **
      *
       p8d.

      * GPIQPB ne pas tester la qte par boite pour dinac
DD0301*    if wqpb > 1 and mmdt-societe = 'GPI'
DD0301     if wqpb > 1 and (mmdt-societe = 'GPI' or = 'ERELS')
                       multiply fcqtl by wqpb giving wcall
                       multiply fcqtc by wqpb giving wcalc
             else      move fcqtl to wcall
                       move fcqtc to wcalc.
           IF WMES > 3 AND WMES < 7 GO TO p8d1.
           GO TO p8d2.
       p8d1.
           IF WMES = 06 MULTIPLY 1000 BY wcall
                        multiply 1000 by wcalc
                   ELSE MULTIPLY  100 BY wcall.
                        MULTIPLY  100 BY wcalc.
       p8d2.
           ADD wcall TO WQTELIV.
           ADD wcalc TO WQTECDE.
       p8e.
DDE089*    perform nnl-fcommac2.
DDE089*    if file-status not = zero
DDE089*         MOVE ALL "9" TO WWWCLE
DDE089*                               GO TO p8f.
DDE089*    MOVE FCNEL2 TO WWNEL.
DDE089*    MOVE FCLIG  TO WWNLIG.
DDE089*    MOVE FCNAR  TO WWART1.
DDE089*    if fcnoc2 not = wcle move all "9" to wwwcle
DDE089*                          go to p8f.
      * tester en plus de la reference article que les conditions de prix
      * sont les memes
      *    IF WWCLE = WWWCLE GO TO p8d.                                 *M0300
DDE089*    IF WWCLE = WWWCLE AND FCPHT = WPHT and fcpbas = WPHTB        *M0300
DDE089*                      and fctrpv = wtrpv                         *M0300
DDE089*                          go to p8d.                             *M0300
      *
       p8f.
      **** FIN ARTICLE ****
      *
DDE089*    IF WQTELIV = 0 GO TO p9f.
           add 1 to wnlfa.
           move spaces to wor-emission2.
           move "LA"   to dfte.
           move wnlfa  to dfnlfa.
DD0465     if mmdt-societe = 'PLASTO'
  -           move fcnlg  to wnlcd
  -           move wnlcd  to dfnlcd
DD0465     end-if
DD9999* anes 15/01/18 chargement du numero de ligne pour MST Tapis St Maclou
  |        if ofgcl-affi-grouph = "MST"
  |           move fcnlg  to wnlcd
  |           move wnlcd  to dfnlcd
DD9999     end-if
           move weanar to dfeafa.
           move wart1  to dfcifa.
           move "SA"   to dftifa.
           move wvlfa  to dfvlfa.
           move wvafa  to dfvafa.
           move wqteliv to wc122.
           move wc122 to dfqffa dfqlfa dfqefa.
           move "PCE"  to dfcufa.
           move wqtecde to wc122.
           move wc122  to dfqcfa.
           if wpht = zero move wqteliv to wc122
GPICMT*GPIWARNING pour l'instant si prix a zero la qte totale est gratuite
GPICMT*    on charge alors la qte livree dans la qte gratuite et on remet
GPICMT*    a zero la qte facturee
GPICMT                    move zero to dfqffa
                          move wc122 to dfqgfa.
           if wqteliv > wqtecde subtract wqteliv from wqtecde
              giving wcalc      move wcalc to wc122
                                move wc122 to dfdqfa.
           if wqteliv < wqtecde move "OS" to dfcrfa.
           if wprx = 2 divide wpht by 100 giving wc112 wpri9 go to p8i.
           if wprx = 3 divide wpht by 1000 giving wc112 wpri9 go to p8i.
           move wpht to wc112 wpri9.
       p8i.
           move wc112 to dfpnfa.
           if wprx = 2 divide wphtb by 100 giving wc112 go to p8j.
           if wprx = 3 divide wphtb by 1000 giving wc112 go to p8j.
           move wphtb to wc112.
       p8j.
           move wc112 to dfpbfa.
           move "1"  to dfctfa.
DD0351*    multiply wqteliv by wpri9 giving wc122 wcalc.
DD0351     multiply wqteliv by wpri9 giving wcalc3
DD0351     multiply wcalc3 by 1 giving wcalc rounded
DD0351     move wcalc to wc122
           move wc122 to dfhtfa.
           move wdes to dfl1fa.
           move wdes2 to dfl2fa.

      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
           if wfbtax not = 1 and wfbtax not = 2 and
                     not = 5 and        not = 6 move "0" to dfctfa
                                                move "00.00" to dftxfa
                                                go to p8k.
           move fctvp to wcodtax1.
           IF WTV1 = ZERO move "0" to dfctfa
                          move "00.00" to dftxfa
                          GO TO p8k.
           IF WTV1 = 9 MOVE 5    TO I
             ELSE           MOVE WTV1  TO I.

      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
           IF WFBTAX = 1 or = 5
                         MULTIPLY WTVA1 (I)  by WCALC
                         move wtva1 (i) to wc022
                         move wc022 to dftxfa
             ELSE        MULTIPLY WTVA2 (I)  by WCALC
                         move wtva2 (i) to wc022
                         move wc022 to dftxfa.
      *    DIVIDE 100 INTO WCALC  ROUNDED.
      *    move wcalc to wc122.
      *    move wc122 to dftvfa.
       p8k.
DDE153* ajout zones cde client/date cde/num BL/date Bl/ean cde/ean livre
      * Reference commande client
           move wrcl2 to          dfrefcl
      * Date de commande client
           move 20    to          dfdatcdes
           move fcann to          dfdatcdea
           move fcmoi to          dfdatcdem
           move fcjou to          dfdatcdej
      * Reference commande du bl livre
           move fcnum to          wnbi1l
           move fcnin to          wnbi2l
           move wnbil to          dfnbl
      * Date de livraison du BL
           move 20    to          dfdatcdesl
           move cemdaa         to dfdatcdeal
           move cemdmm         to dfdatcdeml
           move cemdjj         to dfdatcdejl
      * EAN du commande par
           move weac  to          dfeancde
      * EAN du livre a
           move weal  to          dfeanliv
DD0465* ajout pcb sur demande carrefour
DD0465     move fcqpb to wpcb
DD0465     move wpcb  to dfpcb

           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION LA INVALIDE, STATUS :  " file-status
                                     go to fin.
DD0423     move 1 to w-flag-ligne
           .
      *
      ***** creation RM
       p9.

DD0465     if mmdt-societe = 'PLASTO'
  -           if wphtb not > wpht
  -              go to p9f
  -           end-if
DD0465     end-if
           move spaces to wor-emission2.
           move "RM"   to dfte.
           move "A"    to dfrcrc.
           move "REMISE TARIF" to dflrrc.
           move "1"    to dftrrc.
      *----> M0498 (D)
           move "1"    to dfmprc.
      *----> M0498 (F)
GPICMT* pour BAO on met 2 dans le code (2=remise deduite 1=????)
DD0351     if ofgcl-affi-grouph = 'BAO'
  -           move "2" to dfmprc
DD0351     end-if
           move "01"   to dfbcrc.
DD0351* anes 17/11/2016 : on ne cree pas la ligne si valeur a zero
  |        if wtrpv = zero 
  |          go to p9f
DD0351     end-if
           move wtrpv  to wc042 wc122.
           move wc042  to dfpcrc.
           move wc122  to dfeurc.
           subtract wphtb from wpht giving wcalc.
DD9999*    multiply wcalc by wqteliv giving wc122.
           move wcalc to wc122
           move wc122 to dfmtrc.
           move "QD"  to dfcprc dfcprc-long.
GPICMT* code specifique pour LER
DD0420     if tsrdi = 'LER'
              perform specl-ler
           end-if
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION RM INVALIDE, STATUS :  " file-status
                                     go to fin.
       p9f.
DDE089*    IF WWWCLE NOT = ALL "9" GO TO p8b.
           go to p8a.

DDE153 p9-f.
DDE153* lecture cde suivante si regroupement
           IF fbncd = zero
              perform lec-cde
              if w-retour = 9
                 go to fin
              else
                 if w-retour = 1
                    go to p8
                 end-if
              end-if
           END-IF

GPICMT* si aucune ligne article cree et envoi par @GP on cree une ligne article avec gencod 3001000009991 et qte et prix a zero
  -        if w-flag-ligne = space
  -           perform cre-ligne-vide
DD0423     end-if

           .
      *
      ***** creation PS
      *
       p10.
**************  lecture    des elements 05 A 09                *******
           move zeroes to fccle4.
           move wcle   to fcnoc4.
           move 05     to fcnel4.
           move zeroes to fcunix4.
           perform snl-fcommac4.
           if file-status not = zero
DDE153*         GO TO p11.
DDE153          GO TO p10-f.
       p10a.
           perform nnl-fcommac4.
           if file-status not = zero
DDE153*         GO TO p11.
DDE153*    if fcnoc4 not = wcle go to p11.
DDE153*    IF FCNEL4 > 9 GO TO p11.
DDE153          GO TO p10-f.
DDE153     if fcnoc4 not = wcle go to p10-f.
DDE153     IF FCNEL4 > 9 GO TO p10-f.
DDE153*    if wfbfra = 2 and fcnel4 = 6 go to p10a.
DDE153     if fcfra = 2 and fcnel4 = 6 go to p10a.
      *----> M0998 (D)
DDE153*    if wfbfra = 2 and fcnel4 = 8 go to p10a.
DDE153     if fcfra = 2 and fcnel4 = 8 go to p10a.
      *----> M0998 (F)
           move spaces to wor-emission2.
           move "PS"   to dfte.
           if fcsig = "-" move 3001000001025 to dfeaps
             else         move 3001000001001 to dfeaps.
           if fcnel4 = 5 or fcnel4 = 9 move "3001000009991" to dfeaps.
      *----> M0798 (D)
           if fcdop = "H.T. INSUFFISANT : MAJORATION"
                                       move "3001000001100" to dfeaps.
      *----> M0798 (F)

      *----> M0798a (D)
           move fcdop to wdop.
           if wdop1 = "REMISE" move 3001000001025 to dfeaps.
      *----> M0798a (F)

           move "1"   to dfqups.
      *----> M0798 (D)
      *    if fcnel4 = 7 or fcnel4 = 8 move "0" to dfctps
           if fcnel4 = 8 move "0" to dfctps
      *----> M0798 (F)
             else        move "1" to dfctps.
           MOVE FCDOP  TO dflips.
      *----> M0498 (D)
           if fcnel4 = 6 move "PORT" to dflips.
      *----> M0498 (F)
           if fcnel4 = 7 move fcmon to wc122
                         move wc122 to wx12 dfhtps
                         move wx11  to dfpups
                         go to p10b.
           if fcnel4 not = 5 move fcmon to wc122
                             move wc122 to wx12 dfhtps
                             move wx11 to dfpups
                             go to p10b.
           if fcmon = zero MULTIPLY FCQUL BY FCPUH GIVING wc122
             else          move fcmon  to wc122.
           move wc122  to wx12 dfhtps.
           move wx11   to      dfpups.
       p10b.
      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
           if wfbtax not = 1 and wfbtax not = 2 and
                     not = 5 and        not = 6 move "0" to dfctps.
      *----> M0498 (D)
      *      pour l'instant on peut mettre la remise en negatif quelque
      *      soit le client car seul LEROY MERLIN est traite ces enreg. si
      *      d'autre voir INFLUE qui doit passer une modif
      *    if wclreg not = 465600 go to p7c.
           if fcsig = "-" move dfhtps to wx12
                          move "-"    to wsig12
                          move wx12   to dfhtps
                          move dfpups to wx11
                          move "-"    to wsig11
                          move wx11   to dfpups.
       p10c.
      *----> M0498 (F)
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION PS INVALIDE, STATUS :  " file-status
                                     go to fin.
           go to p10a.

DDE153* lecture cde suivante si regroupement
       p10-f.
           IF fbncd = zero
              perform lec-cde
              if w-retour = 9
                 go to fin
              else
                 if w-retour = 1
                    go to p10
                 end-if
              end-if
           END-IF
           .

      *
      ***** creation TE
       p11.
DDE125*    move spaces to wenrte1 wenrte2 wenrte0.
DDE125     move spaces to awor-emission2 bwor-emission2
DDE125                    cwor-emission2.
      *----> M0599 (D)
      *    if wht0 = zero go to p11a.
           if wfbht1f = zero and wfbht2f = zero go to p11a0.
           if wht0 = zero go to p11a.
       p11a0.
      *----> M0599 (F)
           move spaces to wor-emission2.
           move "TE"   to dfte.
           move "00.00" to dfttte
           move "0"      to dfctte.
      *----> M0599 (D)
           if wht0 = zero and wfbht1f = zero and wfbht2f = zero
      * prendre la tva en cours code 1                                  *M0400
      *      move "20.60" to dfttte                                     *M0400
GPICMT* je prends le code taxe 1 du pieds de facture
DD0351*      if WFBTAX = 1 move wtva1 (2) to wc022                      *M0400
  -   *        else        move wtva2 (2) to wc022                      *M0400
  -   *      end-if                                                     *M0400
DD0351       move FBTT1 to wc022
             move wc022 to dfttte                                       *M0400
             move "1"     to dfctte                                     *M0400
           end-if.                                                      *M0400
      *----> M0599 (F)
           move wht0     to wc122.
           move wc122    to dfhtte.
           move "000000000.00" to dftxte.
           move "T.V.A." to dflite.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION TE INVALIDE, STATUS :  " file-status
                                     go to fin.
DDE125*    move wor-emission2 to wenrte0.
DDE125     move wor-emission2 to awor-emission2.
       p11a.
           if wfbht1f = zero go to p11c.
           move spaces to wor-emission2.
           move "TE"   to dfte.
           move wfbc1f to wcodtax1.
           IF WTV1 = ZERO GO TO p11c.
           IF WTV1 = 9 MOVE 5    TO I
             ELSE           MOVE WTV1  TO I.

      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
GPICMT* on met le 1er taux de tva du pieds de facture
DD0351*    IF WFBTAX = 1 or = 5
  -   *                  move wtva1 (i) to wc022
  -   *                  move wc022 to dfttte
  -   *      ELSE        move wtva2 (i) to wc022
  -   *                  move wc022 to dfttte.
  -        move FBTT1 to wc022
DD0351     move wc022 to dfttte
           move "1"      to dfctte.
           move wfbht1f   to wc122.
           move wc122    to dfhtte.
           move wfbtx1f   to wc122.
           move wc122    to dftxte.
           move "T.V.A." to dflite.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION TE INVALIDE, STATUS :  " file-status
                                     go to fin.
DDE125*    move wor-emission2 to wenrte1.
DDE125     move wor-emission2 to bwor-emission2.
       p11c.
           if wfbht2f = zero go to p12.
           move wfbc2f to wcodtax1.
           IF WTV1 = ZERO GO TO p12.
           IF WTV1 = 9 MOVE 5    TO I
             ELSE           MOVE WTV1  TO I.

      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
GPICMT* on met le 2 eme taux de tva non utilise actuellement
DD0351*    IF WFBTAX = 1 or = 5
  -   *                  move wtva1 (i) to wc022
  -   *                  move wc022 to dfttte
  -   *      ELSE        move wtva2 (i) to wc022
  -   *                  move wc022 to dfttte.
  -        move FBTT2 to wc022
DD0351     move wc022 to dfttte
           move "2"      to dfctte.
           move wfbht2f   to wc122.
           move wc122    to dfhtte.
           move wfbtx2f   to wc122.
           move wc122    to dftxte.
           move "T.V.A." to dflite.
DDE125*    move wor-emission2 to wenrte2.
DDE125     move wor-emission2 to cwor-emission2.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION TE INVALIDE, STATUS :  " file-status
                                     go to fin.
      *
      ***** creation CR
       p12.
           if wfbde1 = zero go to p12a.
           move spaces to wor-emission2.
           move "CR"   to dfte.
           if wfbd1a > 50 move 19 to dfsecr
             else         move 20 to dfsecr.
           move wfbd1a to dfaecr.
           move wfbd1m to dfmecr.
           move wfbd1j to dfjecr.
           move wfbreg to dfmrcr.
           move "7"   to dftdcr.
           if wfbdfa > 50 move 19 to dfsbcr
             else         move 20 to dfsbcr.
           move wfbdfa to dfabcr.
           move wfbdfm to dfmbcr.
           move wfbdfj to dfjbcr.
           move wfbe1f to wc122.
           move wc122 to dfmtcr.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CR INVALIDE, STATUS :  " file-status
                                     go to fin.
       p12a.
           if wfbde2 = zero go to p12b.
           move spaces to wor-emission2.
           move "CR"   to dfte.
           if wfbd2a > 50 move 19 to dfsecr
             else         move 20 to dfsecr.
           move wfbd2a to dfaecr.
           move wfbd2m to dfmecr.
           move wfbd2j to dfjecr.
           move wfbreg to dfmrcr.
           move "7"   to dftdcr.
           if wfbdfa > 50 move 19 to dfsbcr
             else         move 20 to dfsbcr.
           move wfbdfa to dfabcr.
           move wfbdfm to dfmbcr.
           move wfbdfj to dfjbcr.
           move wfbe2f to wc122.
           move wc122 to dfmtcr.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CR INVALIDE, STATUS :  " file-status
                                     go to fin.
       p12b.
           if wfbde3 = zero go to p13.
           move spaces to wor-emission2.
           move "CR"   to dfte.
           if wfbd3a > 50 move 19 to dfsecr
             else         move 20 to dfsecr.
           move wfbd3a to dfaecr.
           move wfbd3m to dfmecr.
           move wfbd3j to dfjecr.
           move wfbreg to dfmrcr.
           move "7"   to dftdcr.
           if wfbdfa > 50 move 19 to dfsbcr
             else         move 20 to dfsbcr.
           move wfbdfa to dfabcr.
           move wfbdfm to dfmbcr.
           move wfbdfj to dfjbcr.
           move wfbe3f to wc122.
           move wc122 to dfmtcr.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CR INVALIDE, STATUS :  " file-status
                                     go to fin.
      *
      ***** creation PF
       p13.
           move spaces to wor-emission2.
           move "PF"   to dfte.
      *----> M0498 (D)
      *    add wfbht1f wfbht2f giving wc122.
           add wfbht1f wfbht2f wht0 giving wc122.
      *----> M0498 (F)
           move wc122  to dfthpf.
           add wfbtx1f wfbtx2f giving wc122.
           move wc122  to dftxpf.
           move wfbnpf to wc122.
           move wc122  to dfttpf.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION PF INVALIDE, STATUS :  " file-status
                                     go to fin.
      *
      ***** creation CD
       p14.
           move spaces to wor-emission2.
           move "CD"   to dfte.
           move "1"    to dficcd dftccd.
           move wcetx  to dflicd.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CD INVALIDE, STATUS :  " file-status
                                     go to fin.
           move spaces to wor-emission2.
           move "CD"   to dfte.
           move "1"    to dficcd.
           move "2"    to  dftccd.
           move wcepx  to dflicd.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CD INVALIDE, STATUS :  " file-status
                                     go to fin.
           
DD2023* commentaire indemnite forfaitaire
           move spaces to wor-emission2.
           move "CD"   to dfte.
           move "1"    to dficcd.
           move "3"    to  dftccd.
           string "POUR TOUT RETARD DE PAIEMENT,"
                  " PENALITE FORFAITAIRE DE 40 EUROS"  
                  delimited by size into dflicd.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CD INVALIDE, STATUS :  " file-status
                                     go to fin.
           
DD2023   if weacx = "3760221650015"
           move spaces to wor-emission2 
           move "CD"   to dfte 
           move "1"    to dficcd 
           move "4"    to  dftccd 
      *    move "EXONERATION DE TAXE"  to dflicd
           move "EXONERATION DE TVA, ART.262, TEXTE 1 DU C.G.I"
                to dflicd
           perform w-emission
           if file-status not = zero display
             "ECRITURE EMISSION CD INVALIDE, STATUS :  " file-status
                                     go to fin
           end-if
         end-if
         .
      *
      ***** creation TV
       p15.
DDE125*    if wenrte0 = spaces go to p15a.
DDE125*    move wenrte0 to wor-emission2.
DDE125     if awor-emission2 = spaces go to p15a.
DDE125     move awor-emission2 to wor-emission2.
           move "TV"   to dfte.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION TV INVALIDE, STATUS :  " file-status
                                     go to fin.
       p15a.
DDE125*    if wenrte1 = spaces go to p15f.
DDE125*    move wenrte1 to wor-emission2.
DDE125     if bwor-emission2 = spaces go to p15f.
DDE125     move bwor-emission2 to wor-emission2.
           move "TV"   to dfte.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION TV INVALIDE, STATUS :  " file-status
                                     go to fin.
       p15f.
DDE125*    if wenrte2 = spaces go to p20.
DDE125*    move wenrte2 to wor-emission2.
DDE125     if cwor-emission2 = spaces go to p20.
DDE125     move cwor-emission2 to wor-emission2.
           perform w-emission.
           if file-status not = zero display
             "ECRITURE EMISSION CD INVALIDE, STATUS :  " file-status
                                     go to fin.
      *
       p20.
           move "3"     to tsco3.
       p21.
DDE153     move tsco3 to w-tsco3
           .
       p21-s.
           perform rw-ttfacmst.
           if file-status not = zero  display "REW. TTFAC INV :" tscle
                                      go to fin.
DD0351     perform cre-trace
           .
       p22.
DDE153     if fbncd = zero
              perform reec-cde
              if w-retour = '9'
                 go to fin
              end-if
           END-IF

           go to p01.
       p30.
           if wcnudx = spaces go to fin
             else             go to p00.
       err.
GPICMT* pour LER on conserve le code a traiter, le non traitement se fera en manuel
GPICMT* par MST00
DD0351*    if tsrdi not = "LER"
      *       move "9" to tsco3
DD0351*    end-if
           perform rw-ttfacmst.
           if file-status not = zero display
             "REECRITURE ttfacmst INVALIDE : STATUS :  " file-status
             "  CLE :  " tscle.
DD0351     perform cre-trace
           .
      *----> M1099a (D)
DDE153 err-a.
           move spaces to wokx.
           display "FAIRE ENTRER POUR CONTINUER w-retour " w-retour.
           accept wokx.
DDE153     if fbncd = zero
              perform reec-cde
              if w-retour = '9'
                 go to fin
              end-if
           END-IF
           .
DDE153 err-f.
           exit.

      *----> M1099a (F)
           go to p01.
       FIN section.
DDE153     if w-retour = '9'
              perform err-a
           end-if

           perform cl-ttfacmst.
           perform cl-ffacture.
           perform cl-fcommaap.
           perform cl-fcommac1.
           perform cl-fcommac2.
           perform cl-fcommac4.
           perform cl-fcoadcli.
           perform cl-fclients.
           perform cl-clisuite.
           perform cl-filieres.
           perform cl-expemois.
           perform cl-paramgpi.
           perform cl-multidat.
           perform cl-emission.
           STOP RUN.

      *=========================================================================
      *                         FONCTIONS LOCALES
      *=========================================================================
      *M030702 ajout info filiere
       disp-fil section.
GPICMT     display 'Verifier son existence dans EANC, si inexistante '
GPICMT     display 'mettre son code ean dans sa fiche client'
           .

      * controle devise
       ctrl-devise section.
           move "C"                 to immpa-devi-tfc
           move "f"                 to immpa-devi-trt
           move fbdev               to wmmpa-devi-cdev9
           call 'mmpa-devi1' using mmpa-devi adl-art
           .

DDE153* lecture cde suivante meme facture avec repositionnement
      * sur 1ere si demandee
       lec-cde section.
       lec-cde-a.
      * si retour espace on demande a lire la 1ere cde de la facture
DDE153* lecture cde suivante
           move zero to w-retour.
           perform nnl-ttfacmst
           if file-status not = zero or tsnfa not = fbnfa
              go to lec-cde-s
           end-if
      * recherche donnees entete de commande
           perform rech-entete
           move 1 to w-retour
           go to lec-cde-f
           .
       lec-cde-s.
           move fbnfa to tsnfa
           perform snlsk1-ttfacmst
           if file-status not = zero
              go to lec-cde-err
           end-if
           .
       lec-cde-ss.
           perform n-ttfacmst
           if file-status not = zero or tsnfa not = fbnfa
              go to lec-cde-err
           end-if
      * recherche donnees entete de commande
           perform rech-entete
           go to lec-cde-f
           .
       lec-cde-err.
           display 'Relecture 1ere cde facture ' fbnfa ' impossible'
           move 9 to w-retour
           move spaces to wokx.
           display "FAIRE ENTRER POUR CONTINUER".
           accept wokx.
           .
       lec-cde-f.
           exit.

DDE153* maj ttfacmst suivants pour la meme facture
       reec-cde section.
       reec-cde-a.
           perform lec-cde
           IF w-retour = 9
              go to reec-cde-f
           else
              if w-retour = 1
                 move w-tsco3 to tsco3
                 perform rw-ttfacmst
                 if file-status not = zero
                    display 'Reecriture ttfacmst IMPOSSIBLE ' tscle
                    move 9 to w-retour
                 end-if
DD0351           perform cre-trace
              go to reec-cde-a
              end-if
           END-IF
           .
       reec-cde-f.
           exit.

DDE153* recherche donnees entete de commande
       rech-entete section.
           move tscle to fccle
           perform rnl-fcommaap
           if file-status not = zero
              display 'Commande inexistante ' tscle ' facture ' tsnfa
              perform err
              go to fin
           end-if
DD0221     move spaces to wrclx
           move fcrcl  to wrclx.

GPICMT*DD0164 ajout code regroupement client suite a demande CASTORAMA de
GPICMT*       pouvoir leur donner le code magasin livre dans facture a
           perform rech-groupement
DD0221* GPIWARNING tant que l'on a pas agrandi la zone ds l'entete de commande
DD0221* GPIWARNING on prend la ref commande de fcommac1 pour BRICORAMA
GPICMT* GPIWARNING renseignee ==> facture en erreur
DD0221*    if ofgcl-affi-grouph = "BRM" or = "BDP" or = "BTK"           *GPICMT
DD0221*    if ofgcl-affi-grouph = "BRM" or = "BTK"                      *GPICMT
DD0221*                      or = "PLA" or = "BKD" or = "LAP"           *GPICMT
DD0465*                      or = "C01" or = "SYU"                      *GPICMT
      *          perform rech-refcde
      *    end-if

           move fccle to wcle
           if wrcl2 = spaces
              display "CDE SANS REFERENCE :  " tscle ' facture ' tsnfa
              perform err
              go to fin
           end-if
           move fcncl to wclcde wclliv
           move fccle to alcle
           perform rnl-fcoadcli
           if file-status not = zero
              go to rech-entete-c
           end-if
           if alcde not = zero move alcde to wclcde.
           if alliv not = zero move alliv to wclliv.
      * recherche ean cdepar
       rech-entete-c.
           move wclcde to fincl
           perform rnlsk1-filieres.
           if file-status = zero
              move fiean to weacx
              go to rech-entete-c-f
           end-if
      *GPIWARNING forcer le code ean client pour GPI
           if mmdt-societe = 'GPI'
DD0774        if fincl = wclient-spe1 move 3020400124600 to weacx
                             go to rech-entete-c-f
              end-if
DD0774        if fincl = wclient-spe2 move 3025940007300 to weacx
                             go to rech-entete-c-f
              end-if
           END-IF.
           display "FILIERE INEXISTANTE, CLIENT COMMANDE:  " wclcde "  "
             "COMMANDE N. :  " fccle.
       rech-entete-c-a.
           move spaces to weacx.
           display "ENTRER SON CODE EAN : <9999999999999>,"
           accept weacx.
       rech-entete-c-f.
           if weacx = spaces or weacx not numeric or weac = zero
                         go to rech-entete-c-a.
       rech-entete-l.
           move wclliv to fincl
           perform rnlsk1-filieres.
           if file-status = zero
              move fiean to wealx
              go to rech-entete-l-f
           end-if
      *GPIWARNING forcer le code ean client pour GPI
           if mmdt-societe = 'GPI'
DD0774        if fincl = wclient-spe1 move 3020400124600 to wealx
                             go to rech-entete-l-f
              end-if
DD0774        if fincl = wclient-spe2 move 3025940007300 to wealx
                             go to rech-entete-l-f
              end-if
           END-IF.
           display "FILIERE INEXISTANTE, CLIENT LIVREA:  " wclliv "  "
             "COMMANDE N. :  " fccle.
       rech-entete-l-a.
           move spaces to wealx.
           display "ENTRER SON CODE EAN : <9999999999999>,"
           accept wealx.
       rech-entete-l-f.
           if wealx = spaces or wealx not numeric or weal = zero
                         go to rech-entete-l-a.
      * recherche date de livraison
       rech-entete-expe.
           move fcnumr to cemnum.
           move fcninr to cemind.
           perform rnl-expemois
           if file-status not = zero
              move zero to CEMDAJ 
           end-if
           .

DD0126* chargement taxe
       chgt-taxe section.
           move "1" to dfttarg
           IF WTV1 = ZERO
              move "0" to dfttarg
              move "00.00" to dftxarg
           ELSE
              if WTV1 = 9
                 MOVE 5    TO I
              else
                 MOVE WTV1 TO I
              end-if

      *GPIWARNING pour DINAC les codes taxes 5 et 6 sont utilises
      *****************ajout trt code taxe 5 idem code taxe 1
      *****************                    6       "        2
              if WFBTAX = 1 or = 5
                 move wtva1 (i) to wc022
                 move wc022 to dftxarg
              else
                 move wtva2 (i) to wc022
                 move wc022 to dftxarg
              end-if
           END-IF
           .

GPICMT* reference commande BRICORAMA
       rech-refcde section.
              move fccle  to pfjcle1
              perform rnl-fcommac1
              if file-status not = zero
                 go to rech-refcde-fin
              end-if
              move pfjli1 to wlentb
              if wlab = "COMMANDE CLIENT:  "
                 move wlbb to wrcl2
                 go to rech-refcde-fin
              end-if
              move pfjli2 to wlentb
              if wlab = "COMMANDE CLIENT:  "
                 move wlbb to wrcl2
                 go to rech-refcde-fin
              end-if
              move pfjli3 to wlentb
              if wlab = "COMMANDE CLIENT:  "
                 move wlbb to wrcl2
                 go to rech-refcde-fin
              end-if
              move pfjli4 to wlentb
              if wlab = "COMMANDE CLIENT:  "
                 move wlbb to wrcl2
                 go to rech-refcde-fin
              end-if
              .
       rech-refcde-fin.
              .
GPICMT* recherche centrale de + haut niveau si filiation # zero
       rech-groupement section.
           move wclliv to ifgcl-affi-client
           move 'R'    to ifgcl-affi-trt
DD9999     move cst-mmpa-nivc-tabp3(1:1) to ifgcl-affi-niveau
           call 'fgcl-affi1' using fgcl-affi adl-art
           if ofgcl-affi-rtn not = '0'
             display
               "Code regroupement client Inexistant :  " wclliv "  "
               "FACTURE NON TRAITEE, COMMANDE N. :  " fccle
             go to err
           end-if
           .

DD0221 tri-comment section.
           if dfco1 = spaces
              move dfco2 to dfco1
              move dfco3 to dfco2
              move dfco4 to dfco3
           end-if
           if dfco2 = spaces
              move dfco3 to dfco2
              move dfco4 to dfco3
           end-if
           if dfco3 = spaces
              move dfco4 to dfco3
              move spaces to dfco4
           end-if
           .

DD0351* trace creation dans fichier des factures a envoyer par EDI
       cre-trace section.
         move spaces to immtr-trac-commentaire
         if tsco3 = 3
            string 'Commande ' tscle ' extraite pour envoi facture '
                    fbnfa ' par EDI'
            delimited size into immtr-trac-commentaire
         else
            string 'Commande ' tscle ' traitement envoi facture '
                    fbnfa ' par EDI en erreur'
            delimited size into immtr-trac-commentaire
         end-if
         move cmmtr-trac-type-cdecli to immtr-trac-type
         move tscle                 to immtr-trac-num
         move "B" to immtr-trac-cop
         move wnom-prog to immtr-trac-prog
DD0350   move "c"   to immtr-trac-action
         call 'mmtr-trac1' using mmtr-trac adl-art
         move space to wmmtr-trac
         .

DD0423* saisie note de debit
DD0316 saisie-notedebit section.
         move spaces to wnotedebit
         display "ENTRER LE NUMERO DE NOTE DEBIT : 15 car maxi"
                 ",S=FAC SUIVANTE".
         accept wnotedebit.
         if wnotedebit = 'S' or = 's'
            go to saisie-notedebit-f
         else
      * recherche si zone numerique
            move 15 to immca-atoi-taille
            move wnotedebit to immca-atoi-nombre
            move "E" to immca-atoi-trt
            perform cal-num
            IF ommca-atoi-rtn not = cmmdt-envi-rtn-ok
                go to saisie-notedebit
            END-IF
         end-if
         .
       saisie-notedebit-f.
         exit.

GPICMT* creation ligne article vide
DD0423  cre-ligne-vide section.
           move spaces to wor-emission2
           initialize wor-emission2
           move "LA"   to dfte.
      * numero de ligne
           add 1 to wnlfa
           move wnlfa to dfnlfa
DD0465     if mmdt-societe = 'PLASTO'
  -           move wnlcd  to dfnlcd
DD0465     end-if
DD9999* anes 15/01/18 chargement du numero de ligne pour MST Tapis St Maclou
  |        if ofgcl-affi-grouph = "MST"
  |           move wnlcd  to dfnlcd
DD9999     end-if
      * ean article
           move 3001000009991 to dfeafa
      * designation
           move 'LIGNE FICTIVE' to dfl1fa
           move "SA"   to dftifa
      * unite
           move "PCE"  to dfcufa
      * quantite facturee
           move 1      to DFQFFA
      * code et taux de tva
           if WFBTX1F = zero and FBTX2F = zero
              move "0" to dfctfa
              move "00.00" to dftxfa
           else
              if WFBTAX = 1
                 move wtva1 (2) to wc022
              else
                 move wtva2 (2) to wc022
              end-if
              move "1" to dfctfa
              move wc022 to dftxfa
           end-if
           perform p8k
           .

GPICMT* specificite code remise pour LER
DD0420 spec-ler section.
           if dfparg-long = 'QD' or 'DI'
              move 'RAA' to dfparg-long
           end-if
           .
DD0420 specl-ler section.
           if dfcprc-long = 'QD'
              move 'RAA' to dfcprc-long
           end-if
           .

       cal-num section.
         call 'mmca-atoi1' using mmca-atoi adl-art
         IF ommca-atoi-rtn not = cmmdt-envi-rtn-ok
            move spaces to  wnbr15x
         ELSE
            move ommca-atoi-nombre to wnbr15x 
         END-IF
           .
       pro section.
           copy "../copy/pro-ttfacmst-cdesup".
           copy "../copy/pro-ffacture-cdesup".
           copy "../copy/pro-fcommaap-cdesup".
           copy "../copy/pro-fcommac1".
           copy "../copy/pro-fcommac2-cdesup".
           copy "../copy/pro-fcommac4-cdesup".
           copy "../copy/pro-fcoadcli-cdesup".
           copy "../copy/pro-fclients".
           copy "../copy/pro-clisuite".
           copy "../copy/pro-filieres".
           copy "../copy/pro-expemois-cdesup".
           copy "../copy/pro-paramgpi".
           copy "../copy/pro-multidat".
           copy "../copy/pro-emission".
           copy "../copy/pro-fartusac".
      *----> M0998 (D)
           copy "../copy/pro-trpntran".
      *----> M0998 (F)
