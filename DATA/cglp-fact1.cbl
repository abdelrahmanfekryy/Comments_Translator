      *DD9999 20/12/19 anes Changement accord leclerc 2020 
      *DD9999 03/01/19 anes Changement du numero accord Leclerc et BBJ
      *DD0820 25/09/18 anes modif. pour traitement en mode batch
      *DD9999 27/03/18 door Msg plus precis en cas d'erreur
      *DD9999 16/09/14 anes Ajout de la mise a jour des compteurs FACTURE000 de
      *                parbatch apres chaque ecriture reussie de la facture afin
      *                d'eviter la casse dans la sequence de numerotation
      *DD0351 18/04/14 anes verrouillage sur la date du jour
      *DD0351 19/02/14 anes traitement de l'edition reliquat (filigrane)
      *DD0351 24/01/14 micn correction edition ARC pour commande regroupee 
      *DD0351 21/01/14 anes Lect fcommaap pour verif proforma en edit ARC dinac
      *DD0712 25/09/13 micn ne pas envoyer a l'imprimeur si cde openbook
      *DD0351 13/09/13 anes Correction trt taxe 5 pour la Corse
      *DD0351 03/07/13 micn test societe pour numero d'accord
      *DD0712 21/05/13 door Inclusion de l'envoi des messade SOS SPLIT et SHIP
      *DD0351 02/04/13 micn Dinac - edition facture laser
      *DD0351 18/03/13 micn enlever la phrase de l'escompte pour toutes les societes
      *DD0351 10/10/12 pase Changement destinataire de mail
      *DD0351 12/04/12 micn  ajout refenece contrat pour BBJ et LCL
      *DD0557 30/12/11 micn force la date de facture a date de cde si refacturation dinac
      *DD0448 15/12/11 elgu si adresse livraison prise dans l'entete ne pas prendre le commande par
      *                     pour le controle code taxe, adresse livree correcte dans cmcd-gest1
      *DD0516 02/10/11 elgu supprimer l'edition preimprime pour GPI
      *DD0516 19/09/11 elgu pour GPI edition preimprime et laser pour test imprimeur
      *DD0351 29/08/11 elgu edition code douanier hors france sur test client livrea sauf transitaire
      *DD0448 04/07/11 elgu cntrole code taxe
      *ELGU17 05/05/11 elgu pour les proforma forcer le nombre d'exemplaire a 1
      *                     ajour edition nomenclature douaniere pour export hors cee
      *DD0351 02/05/11 ELGU POUR cofaq mettre la ',' dans le ht et le montant tva
      *DD0448 14/04/11 elgu traitement code taxe par fonction avec edition texte
      *DD0351 26/04/11 elgu pour depanner l'utilisation du code CFR si franco et espace, j'edite la 
      *                     zone port a espace et l'assistanre mettra CFR en commentaire
      *ELGU09 28/03/11 elgu CHINE 
      *DD0221 18/03/11 elgu ne plus charger la reference commnade client dans fcomjoc1
      *DD0351 01/03/11 micn traduction
      *DD0474 17/02/11 elgu pour OYOBRICO ajouter une plage pour les avoirs
      *DD0350 10/02/11 elgu memoriser le code demat de la commande pour controle flag imprimeur
      *DD0351 31/01/11 elgu ajout comentaire pour LCL et BBJ
      *DD0221 13/01/11 anes allongement reference commande client
      *DD0351 07/01/11 elgu ne plus editer le rib client
      *DD0351 20/12/10 elgu je passe la reference commande client a 12 caractere dans fcommac1
      *DD0444 02/12/10 elgu maj data ware evenement facturation
      *DD0474 29/11/10 micn OYOBRICO en laser
      *DD0351 02/11/10 elgu modif flag imprimmeur
      *DD0465 22/06/10 elgu PLASTO en laser + code reedition + maj code envoi imprimeur
      *DD0444 10/05/10 micn maj dataware en xml
      *DD0351 06/04/10 elgu nouvelles coordonnees bancaire pour tous les clients 
      *DD0448 08/02/10 elgu prendre le numero intracom du client payeur
      *DD0351 07/01/10 elgu prendre les conditions de reglement du pieds de facture en cas de reedition
      *DD0298 05/01/10 elgu en reedition de facture allotie ne pas traiter les filles
      *DD0444 24/12/09 elgu appel maj commande en dataware apres w-ffacture, car fait dans la gf avant la craetion de ffacture donc perte des infos pieds
      *DD0438 10/09/09 micn modif zone edition des conditions de reglement pour 120 jrrs        
      *DD0298 27/10/09 elgu modif appel gestion des codes type de commandes
      *DD0351 27/10/09 elgu  pour slovaquie autrise taxe 1 et pays 1
      *DD0438 27/09/09 micn nbre de jours des conditions de reglement a 3 caract.
      *                     + lecture paramcpt idem factc025 pour slovaquie
      *DD0438 20/08/09 micn modif appel calcul echeance
      *DD0351 24/06/09 elgu deblocage fcommac4 et ffacturea et fcommaap
      *DD0351 15/04/09 elgu appel programm specifique pour DINAC GPISPEDINAC cglp-factD
      *DD0424 25/03/09 micn supression du code releve
      *DD0425 10/03/09 elgu ajout commentaire entete pour type de commande location gerance erels
      *DD0422 28/01/09 elgu pour controle paramcpt prendre le code geo du livrea
      *DD0351 22/12/08 micn Modif condition de reglement
      *DD0221 29/09/08 elgu modif reference client pour PLanit/BriKodepot/lapeyre idem Bricorama
      *DD0350 18/09/08 elgu correction calcul remise speciale, raz non fait en regroupement
      *                     ne plus faire le traitement de remise SP
      *DD0351 22/07/08 micn ajout texte pour conditions de ventes
      *DD0358 18/03/08 elgu ajout d'une 4eme banque
      *DD0394 19/02/08 elgu ajout du type de commande
      *DD0350 16/01/08 elgu modification taux de change pour Slovaquie
      *DD0389 11/01/08 elgu ajoute 'Client facture' au-dessus de l'adresse de livraison
      *                     pour COFAQ
      *DD0380 08/01/08 elgu pour erels et favotex editer infos banque tout le temps
      *DD0387 04/01/08 elgu initialisation choix pour appel mmpa-regl1
      *                     traitement calcul echeance par fonction
      *DD0350 02/01/08 elgu ne pas creer de trace de facturation si edition arc
      *DD0351 12/07/07 micn suppression des open input et changement niveau
      *                     suite a edition des arc en pdf (blocage SLIV)
      *DD0301 16/06/07 elgu demarrage ERELS
      *DD0362 02/04/07 micn  edition arc chiffre + correction proforma
      *DD0358 27/12/06 elgu ajout infos banques (IBAN/BIC)
      *DD0350 26/10/06 micn ne pas mettre le com. escompte pour favotex
      *DD9999 09/10/06 elgu ajout commentaire escompte pour toutes les factures
      *DD0337 10/08/06 elgu edition laser pour slovaquie
      *DD0314 28/08/06 elgu suppression commentaire entete suite a fusion leau
      *DD9999 31/07/06 elgu corection edition contre partie en FRF pour BL regroupes
      *DD0326 21/06/06 elgu
      *DD9999 23/06/06 elgu sip modif pour bricot depot, ils se debrouillent car tropd
      *                de fournisseur en cause
      *DD0316 23/05/06 elgu edition numero facture et commande sur 7 et 8 car
      *DD9999 15/06/06 elgu meme modif pour brico depot que pour bricorama (ref cde client
      *                     trop courte dans fcommaap(BTK idem BRM)
      *DD0316 01/05/06 elgu
      *DD0316 29/04/06 door alongement no de commande
      *DD0316 18/04/06 elgu nlle wor-ffacture.mod wor-fcommac1
      *DD0314 30/03/06 elgu ajout commentaire entete suite a fusuion leau
      *DD0298 06/02/06 elgu tester parametrage type de commande
      *DD9999 25/01/06 elgu ne pas ouvrir paramgpi et mettre en niveau 3
      *                     si facture proforma demande a la saisie de
      *                     commande le fichier s'ouvrait en input
      *                     et plantage sur la saisie de la commande suivante
      *DD9999 25/11/05 elgu correction creation pied fe facture si facture
      *                     proforma regroupee
      *DD9999 18/10/05 elgu controle existence paramcpt pour ventilation compta
      *DD0275 26/09/05 elgu impression d'un texte fin de page pour escompte
      *DD9999 07/09/05 elgu perte du pieds de facture en reedition
      *DD9999 27/07/05 elgu correction edition ref commande client pour BL
      *                     regroupes
      *DD9999 22/06/05 elgu prendre le code reglement du regle par
      *DDE175 02/03/05 elgu ne plus creer intracom
      *DDE153 10/01/05 elgu correction reedition facture avec BL regroupes
      *DD0219 29/12/04 elgu
      *DD0062 29/12/04 micn changement code action trace
      *V30002 15/12/04 elgu
      *V20002 10/12/04 elgu
      *DD0221 02/12/04 elgu suite a refus BRICORAMA des factures depuis le 1711
      *                     modif rapide pour editer la ref commande commentaire
      *DD9999 25/10/04 elgu sup DD0122 suite a nouvel imprime
      *V10002 27/09/04 elgu
      *DD9999 12/08/04 elgu controle coherence des codes avec affichage erreur
      *                     et ctrl numero intracommunautaire
      *DD0002 04/08/04 elgu activer le bloc adresse fiche client
      *DD0188 28/06/04 elgu trt adresse export
      *                     du ode nature de filiation
      *DD0122 13/10/03 elgu edition nlle denomination et numero TVA
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cglp-fact1.
      *
      ***************************************************
      * GPICMT EDITION   DES  FACTURES GD PUBLIC  GENCOD*
      ***************************************************
      *
      *  nouveau format : 12 car au pouce en largeur
      *                   6 lignes au pouce    (0797)
      *-------------------------------------------------------------------------
      *DDE153 ajout regroupement des cdes sur une meme facture
      *DDE046: ref en x
      *        maj taux commission representant depuis fiche client
      *DDE130: agrandissement reference commande client avec modif mep
      *DDE069: numerotation lignes des commentaires idem DINAC
      *        edition des lignes dans l'ordre de la commande
      *DDE057: nlle edition avec gencod et une seule colonne qtee
      *DDE089: edition contre valeur en FRF pour facture EURO suivant code
      *        clcvfrf du client facture
      *        edition des commentaires apres la l'article au lieu d'avant
      *        ajout controle validite de la devise
      *DDE011: ne plus controler la devise avec le code facture dematerialisee
      *DDE043: trt el 5 (fcommac4) traiter le signe meme si montant non
      *        renseigne
      *DDE033: mettre un "*" sur les lignes ou s'applique la remise MB
      *        ajout tracetel
      *DDE027: lecture fcommaap sur cle 3 afac/fac/cde pour ne lire que les
      *        commandes a facturer
      *DDE022: changer libelle REMISE MB par REMISE SP pour pouvoir l'utiliser
      *        pour d'autres clients que Mr BRICOLAGE
      *M1199 : edition libelles articles suivant code langue du client facture
      *        annulation modif M0499
      *M0499 : aj. texte nouveau tel et fax pour commande VEYZIAT
      *M0299 : agrandiss. zones wcale et wzca, passe de 6 a 9 (pb lire et peset)
      *M1298 : ajout test du code decalage de l'echeance si date > 24
      *M1198 : Modifs pour passage a l' EURO
      *      : modif pour ne pas editer les factures dematerialisees
      *M1098 : trt remise MB speciale pour ne pas appliquer la remise sur tous
      *        les articles
      *M0798 : ne pas editer les colonnes prix brut et remise pour les clients
      *        facture ayant "N" dans code rfa de la fiche client
      *M1297 : mise a jour montant remise et base remise pour les elmts 7 pour
      *        la dematerialisation de la facture
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
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ETAT ASSIGN TO wlabel-etat
                       organization line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  ETAT
                LINAGE IS 72
                DATA RECORD LIGNE.
       01  LIGNE             PIC X(99).
       01  L1.
           02 filler         pic x.
           02 LNOMLIV.
             03 FILLER       PIC X(17).
             03 LSUITE       PIC X(5).
             03 FILLER       PIC X(5).
             03 filler       pic x(7).
           02 LCOND          PIC X(12).
           02 filler         pic x(3).
           02 LNOMPAY        PIC X(26).
           02 FILLER         PIC X(23).
       01  L2.
           02 filler         pic x.
DD0188* agrandissement zone pour adresse export
DD0188     02 lbureaul.
            03 LCODP1         PIC 9(5)            BLANK ZERO.
            03 FILLER         PIC X.
            03 LXBD1          PIC X(29).
DD0316     02 lnfact         PIC 9(7).
DDE130     02 filler redefines lnfact.
DDE130        03 filler      pic x(5).                                  *DD0188
              03 LNBEX       PIC Z9.
           02 FILLER         PIC X(7).
DD0188* agrandissement zone pour adresse export
DD0188     02 lbureau.
            03 LCODP2        PIC 9(5)            BLANK ZERO.
            03 FILLER        PIC X.
            03 LXBD2         PIC X(29).
           02 FILLER         PIC X(14).
       01  L3.
      * DDE089 edition cip puis reference interne (+sous ref)
      *    02 LNREF          PIC 9(7).
      *    02 filler         pic x.
      *    02 LVREF          PIC X(8).
      *    02 filler         pic x.
      *DDE057 edition complete du gencod ean14/pays/cnuf/cip/cle
           02 lgencodx.
              03 filler      pic x.
              03 lean14      pic x.
              03 lge1        pic x.
              03 lpays       pic x.
              03 lcnuf       pic x(5).
              03 lge2        pic x.
              03 lcip        pic x(6).
              03 lge3        pic x.
              03 lcle13      pic x.
           02 lsep1          pic x.
           02 lnrefx.
             03 lnref          pic x(7).
             03 lsep2          pic x.
             03 lsref          pic xx.
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
             03 LUV          PIC xx.
DDE057*      03 LNBTES       PIC ZZZ9.
DDE057*      03 FILLER REDEFINES LNBTES.
DDE057*        04 FILLER     PIC XXX.
DDE057*        04 LNBTESX    PIC X.
DDE057*    02 filler         pic x.
DDE057*    02 LQTEFA         PIC Z(8)9V99B       BLANK ZERO.
DDE057*      03 FILLER REDEFINES LQTEFA.
DDE057*      03 LQTEUN       PIC ZZZ9            BLANK ZERO.
DDE057*      03 LQTEFAU      PIC Z(4)9V99B       BLANK ZERO.
DDE057     02 LQTEFA         PIC Z(6)9           BLANK ZERO.
DDE057     02 FILLER REDEFINES LQTEFA.
DDE057         04 FILLER     PIC X(5).
DDE057         04 LQTEFAD    PIC XX.
           02 LPUB           PIC Z(4)9V99        BLANK ZERO.

      * edition "*" pour ligne avec remise MB                           *DDE033
DDE057     02 lpremas        pic x.                                     *DDE033
           02 lprem          pic zz9v99           blank zero.

           02 LPU            PIC Z(4)9vB99        BLANK ZERO.
DDE057*    02 FILLER REDEFINES LPU.
DDE057*      03 LUCM         PIC X.
DDE057*      03 FILLER       PIC X(6).
           02 LMONT          PIC Z(5)9vB99        BLANK ZERO.
           02 LSIG           PIC X.

       01  L4.
           02 FILLER         PIC X.
           02 LNBCOL         PIC Z9B             BLANK ZERO.
           02 filler         pic x.
           02 LPDSTO         PIC ZZZ9V,9         BLANK ZERO.
DDE057     02 filler         pic x.
DDE057     02 LNETMA         PIC Z(7)9V99B       BLANK ZERO.
DDE057*    02 filler         pic xx.
           02 LTHT           PIC Z(7)9V99        BLANK ZERO.
DD0351* pour COFAQ faire apparaitre la ','
DD0351     02 LTHT-cofaq redefines LTHT    PIC Z(6)9V,99 BLANK ZERO.
           02 filler         pic x.
           02 LPORTT         PIC ZZZ9V99B        BLANK ZERO.
           02 LTAU1          PIC Z9V99B          BLANK ZERO.
           02 filler         pic xxx.
           02 LMTTVA1        PIC Z(5)9V99B       BLANK ZERO.
DD0351* pour COFAQ faire apparaitre la ','
DD0351     02 LMTTVA1-cofaq  redefines LMTTVA1
DD0351            PIC Z(4)9V,99B       BLANK ZERO.
           02 LTAU2          PIC Z9V99B          BLANK ZERO.
           02 filler         pic xx.
           02 LMTTVA2        PIC Z(5)9V99        BLANK ZERO.
           02 filler         pic x.
           02 LAFFRAN        PIC ZZZ9V99         BLANK ZERO.
           02 filler         pic xx.
           02 lffr           pic x(4).
           02 LNETPAY        PIC Z(8)9V,99      BLANK ZERO.
           02 LSIGN          PIC X.
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
DD0387      03 lcondx        pic x(20).
DD0351*      04 LRJO          PIC 99B.
DD0351*      04 LRJL          PIC X(6).
DD0351*      04 LRLE          PIC 99.
DD0351*    02 filler         pic x(10).
DD0351     02 filler         pic x(1).
           02 LJEE           PIC 99.
           02 FILLER         PIC X.
           02 LMEE           PIC 99.
           02 FILLER         PIC X.
           02 LAEE           PIC 99.
           02 FILLER         PIC X(20).
           02 lffrp          pic x(4).
           02 LPAPI.
             03 LNETPAYP.
               04 LNETPAY9   PIC Z(8)9V,99.
               04 LPASIG     PIC X.
             03 FILLER REDEFINES LNETPAYP.
               04 FILLER     PIC X(6).
               04 LNCLIP     PIC 9(6)B.
             03 FILLER REDEFINES LNETPAYP.
               04 FILLER     PIC X(4).
               04 LJP        PIC 99.
               04 LSL1P      PIC X.
               04 LMP        PIC 99.
               04 LSL2P      PIC X.
               04 LAP        PIC 99.
               04 FILLER     PIC X.
             03 FILLER REDEFINES LNETPAYP.
DD0316         04 FILLER     PIC X(5).
DD0316         04 LNFACTP    PIC 9(7)B.
             03 FILLER REDEFINES LNETPAYP.
DD0316         04 FILLER     PIC X(3).
DD0316         04 LNCDE      PIC 9(7)B.
               04 LNIN       PIC 9B.
       01  L6.
DDE057*    02 FILLER         PIC X(17).
DDE057     02 FILLER         PIC X(29).
           02 LIBCA13.
             03 LIBCA12      PIC X(30).
DDE057       03 filler       pic xx.
             03 LTAUX        PIC Z9V,99B.
             03 LPOURC       PIC X.
DDE057*      03 FILLER       PIC X(35).
DDE057       03 FILLER       PIC X(19).
DDE057*      03 LMT          PIC Z(6)9V99       BLANK ZERO.
DDE057       03 LMT          PIC Z(7)9vB99      BLANK ZERO.
             03 FILLER       PIC X.
       01  L7.
           02 FILLER         PIC X(17).
           02 LFIN.
             03 LIBF         PIC X(15).
             03 LJF          PIC 99B.
             03 LSL5         PIC XX.
             03 LMF          PIC 99B.
             03 LSL6         PIC XX.
             03 LAF          PIC 99.
           02 FILLER         PIC X(37).
           02 LDEV           PIC X(18).
       01  L8.
           02 FILLER         PIC X(17).
           02 LPAY           PIC XX.
           02 FILLER         PIC XX.
           02 LCLE           PIC XX.
           02 FILLER         PIC XX.
           02 LSIR           PIC X(11).
           02 FILLER         PIC X(5).
           02 LEXP.
             03 LNV          PIC X(6).
             03 LTXT         PIC X(36).
           02 filler         pic x(16).
      *
       WORKING-STORAGE SECTION.
DDE089     copy "../copy/wor-fartusac".                                 *GPICMT
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
DD9999     copy "../copy/wor-paramcpt".                                 *GPICMT
      *----> M1098 (D)
           copy "../copy/wor-fartusap".                                 *GPICMT
      *----> M1098 (F)

      *----> M1199 (D)
           copy "../copy/wor-languear".                                 *GPICMT
      *----> M1199 (F)

      *----> DDE033 (D)
           copy '../copy/cmta-comi.com'.                                *GPICMT
      *----> DDE033 (F)
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
DD0219     copy "../copy/mmca-qtes.com".                                *GPICMT
DD0219     copy "../copy/cgca-mtht.com".                                *GPICMT
DD0298     copy '../copy/cmpa-tycd.com'.                                *GPICMT
DD0298     copy "../copy/cgcd-fill.com".
DD0337     copy "../copy/mmlp-hfac.com".
DD0358     copy "../copy/mmpa-pays.com".
DD0358     copy "../copy/mmpa-soci.com".
DD0387     copy "../copy/mmca-eche.com".
DD0351     copy '../copy/mmlp-mail.com'.                                *GPICMT
DD0351     copy '../copy/mmaf-vrep.com'.                                *GPICMT
DD0444*    copy '../copy/cmex-cdes.com'.                                *GPICMT
DD0444     copy '../copy/cmcd-lect.com'.                                *GPICMT
DD0444     copy '../copy/cmcd-mjdw.com'.                                *GPICMT
DD0448     copy '../copy/mmpa-vtax.com'.                                *GPICMT
DD0316 77  WNFANA            PIC 9(7).
DD0316 77  WNFACA            PIC 9(7).
DD0351 77  WNAVOI            PIC 9(7).
       77  WTOTFA            PIC S9(7)V99.
       77  WTOTAV            PIC S9(7)V99.
       77  WCPTR             PIC s999.
GPICMT* compteur ligne de banque a editer
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
DD0820 77  wflag-trt-cde     pic x. 
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
DD0221* memo code groupement du client
           02 wrdi    pic x(3).

DD0221** LIBELLE REFERENCE COMMANDE DU CLIENT POUR BRICORAMA            *M0600a
           02   WLENTB.                                                 *M0600a
            03  WLAB              PIC X(18).                            *M0600a
            03  WLBB              pic x(12).                            *M0600a

DD0316     02 wlnfac  pic 9(7).
DDE057     02 zqtefav pic 9(6)v99.
           02 filler redefines zqtefav.
              03 filler pic x(6).
              03 zqtefad pic xx.
      * nom pgm pour vali1
           02  wnom-prog                PIC X(10) value 'cglpfact1'.
ELGU17     02  wgeo-livrea              pic x.

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
DD0351* flag d'edition laser pour reedition
           02 w-flag-edit          pic 9.

      *----> DDE033 (D)
      * memo date du jour
           02 wdatej              pic 9(6).
           02 filler redefines wdatej.
             03 waj               pic 99.
             03 wmj               pic 99.
             03 wjj               pic 99.
      *----> DDE033 (F)

      *----> M1199 (D)
           02 wlngfac             pic 99.
      *----> M1199 (F)

      *----> M1298 (D)
           02 wcec           pic x.
      *----> M1298 (F)

GPICMT*memo commision representant fiche client
DDE046     02 w-fbtrh        pic 99v99.

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

      *----> M0798 (D)
           02 wclcrf         pic x.
      *----> M0798 (F)

      * DDE089 code edition contre valeur en francs
           02 wclcvfrf       pic x.
      * numero de commande a la demande
           02 wcdex.
DD0316        03 wcde        pic 9(7).
DDE153        03 wind        pic 9.


      * trt des erreur avec reponse par accept
           02 wtrt           pic x.

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
             03 filler       pic xx.
DDE130*      03 WNCDE        PIC X(9).
DDE130       03 WNCDE        PIC X(17).
             03 FILLER       PIC X.
             03 FILLER       PIC XX.
             03 WCOTA        PIC 9B.
             03 WTARI        PIC 999B.
             03 filler       pic xx.
             03 WPORT        PIC X(8).
             03 FILLER       PIC X.
             03 filler       pic x(4).
             03 WEXPE        PIC X(14).
             03 FILLER       PIC X(4).
DD0316*      03 filler       pic x.
DD0316       03 WLNUM        PIC 9(7)B.
DD0316       03 WLNIN        PIC 9.
             03 filler       pic x.
             03 WSECT        PIC 999BB.
             03 filler       pic xx.
             03 WNCL         PIC 9(6)B.
DDE130*      03 filler       pic x.
DDE130*      03 WDATEF       PIC X(6).
DDE130       03 WDATEF.
DDE130          04 WDATJ       PIC 99.
DDE130          04 wsl1         pic x.
DDE130          04 wdatm       pic 99.
DDE130          04 wsl2         pic x.
DDE130          04 wdata       pic 99.
DDE130*      03 FILLER       PIC X.
DDE130*      03 filler       pic xx.
DDE130*      03 WLNFAC       PIC 9(5).
DDE130*      03 FILLER       PIC X.
           02 WNCLP          PIC 9(6).
           02 WGEOP          PIC 9.
           02 WGEOL          PIC 9.
M0297      02 wegtvp         PIC 99.
  |        02 FILLER         REDEFINES wegtvp.
  |          03 filler       PIC 9.
M0297        03 wegtv1       PIC 9.
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
DDE046       03 wadnar       pic x(7).
DDE046       03 wadsrf       pic xx.
DD0424*    02 WCRE           PIC 9.
           02 WDES           PIC X(30).
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
DD0351* memo code openbook pour ne pas envoyer a l'imprimeur
           02 wfcopenbook      pic 9.

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
           02 WCON           REDEFINES WCONX  PIC 9(5).
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
      *----> M1198 (F)
      *****02 WSTAT          PIC XX.
           02 WBTED          PIC X(17).
           02 WNEL9          PIC 99.
           02 WCPT7          PIC 9.
           02 WREMX.
             03 WREM7        OCCURS 5.
               04 WLIB7      PIC X(30).
               04 WQUI       PIC S9(6)V99.
      *----> M1297 (D)
               04 wbas7      pic 9(6)v99.
               04 wmon7      pic 9(6)v99.
DD0316         04 wcle7      pic x(13).
      *----> M1297 (F)
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
       01       wver         pic xxx.
       01  wdopx.
           02 wdop1             pic x(16).
           02 wdop2x.
              03 wdop2          pic x(7).
              03 filler         pic x.
           02 wdop3             pic 99.
DDE046 01  wnart                pic x(7).
       01  wnsrf                pic xx.
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

      *----> M1098 (D)
       01  wfcdop.
           02 wmb               pic x(9).
           02 filler            pic x(21).
       01  whtmbx.
           02 wcpt7a            pic 9.
           02 whtmb             pic s9(9)v99.
           02 whtmb9 occurs 2   pic s9(9)v99.
      *----> M1098 (F)

      *       memo enreg pied de facture pour controle identique qd reedition
DD0316   copy "../copy/wor-ffacture.mod" replacing ==(pref)== by ==ww==.
      *
      *DDE037 ligne article en working pour mettre decimale ds qte facturee
       01  wl31.
           02 filler         pic x(61).
DDE057     02 lqtefav        pic z(3)9v,99 blank zero.
           02 filler         pic x(31).
       LINKAGE SECTION.
           copy '../copy/cglp-fact.com'.                                *GPICMT
           copy "/usr/action/ADL/copy/wor-adl".
      *
       PROCEDURE DIVISION using cglp-fact adl-art.
       DEB SECTION.

GPICMT*GPISPEDINAC appel pgm specifique d'edition de facture
DD0351*    if mmdt-societe = 'DINAC'                                    *GPICMT
  -   *       call 'cglp-factD' using cglp-fact adl-art                 *GPICMT
  -   *       go to fini
DD0351*    end-if

DD0358* recherche infos societe
  -        move mmdt-societe to immpa-soci-societe
DD0358     call 'mmpa-soci1' using mmpa-soci adl-art

DD0820     initialize wflag-trt-cde

      *DDE089
      * reedition terminee ==> ferm = 'F' ==> fermeture des fichiers
           move spaces to immaf-vali-tit.
           if icglp-fact-ferm = 'F' go to fin.
       D10.

      *DDE089
           move spaces to wtrt
           move '0' to ocglp-fact-rtn
           move spaces to ocglp-fact-liberr.
      * recuperation date du jour                                       *GPICMT
           move 'D' to immti-date-taj
           call 'mmti-date1' using mmti-date adl-art
           if ommti-date-rtn not = '0'
              move ommti-date-rtn    to ocglp-fact-rtn
              move ommti-date-liberr to ocglp-fact-liberr
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to fin
           end-if
           move wmmti-date-amj to wdatej

      *----> M0797 (D)
      *    move "chmod 766 fcomma*" to cde-data.
      *    call 'systcc' using zon-cde sysrtn.
      *    if sysrtn not = 0 display "ERREUR/Chmod 766 FCOMMA..".
      *----> M0797 (F)
      *----> M1297 (D)
      *    move 'I' to gfkey.
           move 'W' to gfkey.
      *----> M1297 (F)
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
           IF icglp-fact-ferm = spaces
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
      *
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
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
DD0820* anes 25/09/18 si traitement batch, on force toutes les commandes
  |   * et on part en traitement 
  |      if icglp-fact-direct = 2
  |        move zero to wcdex
  |        move wmmti-date-jma to wdate
  |        go to trt-deb
DD0820   end-if

DDE089* on met "#" ds code trt de vali1 pour attendre une reponse au    *GPICMT
      * clavier en cas d'erreur par vali1                               *GPICMT
           move '#' to wtrt.

           move spaces to wcdex
        if mmdt-langue = "FR"
           DISPLAY "EDITION DES FACTURES,T=toutes ,99999999=N.cde,"
                   " F=FIN" 
        else
           if mmdt-langue = "ES"
              DISPLAY "EDICION DE LAS FACTURAS,T=todas ,99999999=Pedido"
                   ", F=FIN" 
           else
              DISPLAY "PUBLISHING OF INVOICES,T=all ,99999999=N.order,"
                   " F=Exit" 
           end-if
        end-if

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
DD0351* Chargement de la date du jour pour affichage
DD0351     move wmmti-date-jma to wdate
           if mmdt-langue = "FR"
DD0351*       DISPLAY "ENTRER DATE DE FACTURE: <JJMMAA>,F=FIN" 
DD0351        DISPLAY "DATE DE FACTURE: " wj "/" wm "/" wa " ,F=FIN" 
           else
              if mmdt-langue = "ES"
DD0351*          DISPLAY "FECHA DE LA FACTURA ? : <DDMMAA>,F=FIN" 
  |              DISPLAY "FECHA DE LA FACTURA ? : " 
DD0351                    wj "/" wm "/" wa " ,F=FIN"
              else
DD0351*          DISPLAY "DATE OF INVOICE ? : <DDMMAA>,F=FIN" 
  |              DISPLAY "DATE OF INVOICE ? : " 
DD0351                    wj "/" wm "/" wa " ,F=FIN"
              end-if
           end-if
           ACCEPT WDATE.
           if wdate = 'f' or 'F' go to fin.
DD0351* Rechargement de la date du jour car elle imposee
  |        move wmmti-date-jma to wdate 
DD0351     go to trt-console-s2 

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
        if mmdt-langue = "FR"
           display '************************************************'
           display 'DATE FACTURE SAISIE HORS PERIODE !!!!!'
           display '************************************************'
           display "Date = " wj "-" wm "-" wa " - <hor> si ok"
        else
           if mmdt-langue ="ES"
              display '************************************************'
              display 'FECHA DE FACTURA ES FUERA DEL PERÍODO !!!!!'
              display '************************************************'
              display "Date = " wj "-" wm "-" wa " - <hor> si ok"
           else
              display '************************************************'
              display 'DATE OF INVOICE IS OUTSIDE PERIOD   !!!!!'
              display '************************************************'
              display "Date = " wj "-" wm "-" wa " - <hor> si ok"
           end-if
        end-if
           accept wver
           if wver not = "hor" and wver not = "HOR"
              go to trt-console
           else
              go to trt-console-s2
           end-if
           .

       trt-console-s1.
           if mmdt-langue = "FR"
              display "Date = " wj "-" wm "-" wa " - <oui> si ok"
           else
              if mmdt-langue = "ES"
                 display "Fecha = " wj "-" wm "-" wa " - <oui> si ok"
              else
                 display "Date = " wj "-" wm "-" wa " - <oui> if ok"
              end-if
           end-if
           accept wver
           if wver not = "oui" and wver not = "OUI"
              go to trt-console
           end-if

           .

       trt-console-s2.
           if wcdex = zero
              if mmdt-langue = "FR"
                 display 'Facturation de toutes les commandes a ' 
                         'facturer'
              else
                 if mmdt-langue = "ES"
                    display 'Facturacion de todas los pedidos a ' 
                            'facturar'
                 else
                    display 'Invoicing of all orders to be invoiced'
                 end-if
              end-if
           else
              if mmdt-langue = "FR"
                 display 'Facturation de la commande: ' wcdex
              else
                 if mmdt-langue = "ES"
                    display 'Facturacion del pedido: ' wcdex
                 else
                    display 'Invoicing of order: ' wcdex
                 end-if
              end-if
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
              move ommdt-parb-liberr to ocglp-fact-liberr
              move cmmdt-envi-rtn-err to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit    
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
      *       perform erreur
              go to fin
           end-if

DDE089* recup devise de base en compta
           call 'mmcp-devb1' using mmcp-devb adl-art
           if ommcp-devb-rtn not = '0'
              move ommcp-devb-liberr to immaf-vali-tit
              move ommcp-devb-rtn    to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
              move ommpa-devi-rtn    to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to fin
           else
      * controle des taux
              if ommpa-devi-tcd = zero or ommpa-devi-teu = zero
                 move '2' to ocglp-fact-rtn
                if mmdt-langue = "FR"
                 move "PARAMETRE DEVISE EURO (50) INCORRECT"
                          to immaf-vali-tit
                else
                 move "PARAMETER CONVERSES EURO (50) WRONG"
                          to immaf-vali-tit
                end-if
                 move '2' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
                if mmdt-langue = "FR"
                 move "EL.FACTURE000 ABSENT" to immaf-vali-tit
                else
                 move "EL.FACTURE000 WRONG" to immaf-vali-tit
                end-if
                 move '3'                    to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
                 go to fin
              end-if
              MOVE PHANOC (1) TO WNFANA
              MOVE PHANOC (2) TO WNFACA
              MOVE PHANOC (3) TO WNAVOI
           END-IF.

           MOVE "PARAFITVA1" TO PGCLE.
           perform rnl-paramgpi.
           if file-status not = zero
             if mmdt-langue = "FR"
              move "EL.TVA1 ABSENT" to immaf-vali-tit
             else
              move "EL.TVA1 WRONG" to immaf-vali-tit
             end-if
              move '3'              to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to fin.
           MOVE PGFZON TO WENRTVA1.
           MOVE "PARAFITVA2" TO PGCLE.
           perform rnl-paramgpi.
           if file-status not = zero
             if mmdt-langue = "FR"
              move "EL.TVA2 ABSENT" to immaf-vali-tit
             else
              move "EL.TVA2 WRONG" to immaf-vali-tit
             end-if
              move '3'              to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
DD0122*    compute wnbl = 2 - linage-counter of etat
DD0122*    write ligne before wnbl
DD0122*    move '     GPI GROUPE GERGONNE' to ligne
DD0122*    write ligne before 0.
           compute wnbl = 10 - linage-counter of etat.
           write ligne before wnbl.
DD0122*    move spaces to ligne
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
DDE057     MOVE all '9' TO lgencodx.
DDE089*    MOVE 999999 TO LUV LNBTES LMONT.
           MOVE 999999 TO LUV lqtefa LMONT.
           compute wnbl = 60 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L60.
           MOVE SPACE TO LIGNE.
           MOVE 999999 TO LNETMA LTAU1 LMTTVA1 LNETPAY.
           WRITE LIGNE BEFORE 3.
           MOVE SPACE TO LIGNE.
           MOVE 999999 TO LJEE LMEE LAEE LNCLIP.
      *    WRITE LIGNE BEFORE 1.
DD0122*    perform identifiant
           WRITE LIGNE BEFORE page.
           MOVE SPACES TO LIGNE.
       D40.
           PERFORM D31.
      *
      **** TRAITEMENT FACTURATION ****
      *
       T10.
           MOVE 0 TO WTOTAV WTOTFA.
DD0316*    MOVE ZERO TO FCNUM FCNIN.
DD0316     MOVE ZERO TO fccle-cdesup

      *DDE089 lecture directe de la commande qd reedition avec controle
      *       deja facturee
999999   display "cglpfact cde " wcdex
           if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
DDE153* on lit les commandes des sur cle facture/numero cde
DDE153*       move wcdex to fccle
DDE153*       perform r-fcommaap
DDE153        move spaces to fccle-cdesup
DD0316        move wcde to fcnfa-cdesup
DDE153        perform snlsk4-fcommaap
DDE153        if file-status = zero
DD0351           perform nnl-fcommaap
DDE153        end-if
DD0316        if file-status not = zero or fcnfa-cdesup not = wcde
                if mmdt-langue = "FR"
DD0316           string 'FACTURE NON TROUVEE: ' fcnfa-cdesup
                     delimited size into immaf-vali-tit
                else
DD0316           string 'INVOIC NOT FOUND: ' fcnfa-cdesup
                     delimited size into immaf-vali-tit
                end-if
                 move '3'               to ocglp-fact-rtn
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
                 go to fin
              end-if
DD0316        move fcnfa-cdesup to wlnfac
DDE153        move zero to wcdex
999999   display "cglpfact wcdex 1 " wcdex
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
           if icglp-fact-arc = "X"
DD0351        move 0 to fcafa fcfac
  |   * 21/01/14 si commande proforma Dinac, chargt de cle differente pour start
  |   * car dans CDI63, si proforma et societe <> GPI -> fcafa est force a 1
  |           if mmdt-societe not = 'GPI'
  |             move wcdex to fccle-cdesup
  |             perform rnl-fcommaap
  |             if fcfoa = 9
  |               move 1 to fcafa
  |             else
  |               move 0 to fcafa
  |             end-if
  |           end-if
DD0351        move 0 to fcfac
           else
              move 1 to fcafa
              move 0 to fcfac
           end-if
999999   display "cglpfact wcdex 2 " wcdex
DD0316     move wcdex to fccle-cdesup.
           if icglp-fact-e1regroup not = '1'
999999   display "cglpfact snlsk3-fcommaap"
999999   display "cglpfact cle " fcafa " " fcfac " " fccle-cdesup
              perform snlsk3-fcommaap
           ELSE
              move 1 to fcafa
              move 0 to fcfac fcdev fcgeo fcfoa fcdi2
              move spaces to fcclefac fccle-cdesup
              perform snlsk5-fcommaap
999999   display "cglpfact snlsk5-fcommaap"
           END-IF

           if file-status not = zero
             if mmdt-langue = "FR"
              string 'RIEN A FACTURER, status: (' file-status ' )'
                     delimited size into immaf-vali-tit
             else
              string 'NOTHING TO INVOICE, status: (' file-status ' )'
                     delimited size into immaf-vali-tit
             end-if
              move '3'            to ocglp-fact-rtn
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to fin.
      *----> DDE027 (F)

GPICMT* lecture non blocante si reeditiion
DD0351     if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
              perform nnl-fcommaap
999999   display "cglpfact nnl-fcommaap"
           else
              perform n-fcommaap
999999   display "cglpfact n-fcommaap"
           end-if
           if file-status not = zero
             if mmdt-langue = "FR"
              string 'RIEN A FACTURER, status: (' file-status ' )'
                     delimited size into immaf-vali-tit
             else
              string 'NOTHING TO INVOICE, status: (' file-status ' )'
                     delimited size into immaf-vali-tit
             end-if
              move '3'            to ocglp-fact-rtn
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to fin.
      * controle facture a la demande
           if wcdex not = spaces and not = zero
DD0316        if fccle-cdesup not = wcdex
999999   display "cglpfact fccle-cdesup " fccle-cdesup
                if mmdt-langue = "FR"
                 string 'Commande inexistante ou facturee : 'wcdex
                    delimited size into immaf-vali-tit
                else
                 string 'Order not found or invoiced : 'wcdex
                    delimited size into immaf-vali-tit
                end-if
                 move '2' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
                 go to fin
              end-if

GPICMT* appel gestion du type de commande
DD0298        initialize wcmpa-tycd
  "           move fcfeo to wcmpa-tycd-typ                              *GPICMT
  "           move ccmpa-tycd-gestion-tycd to icmpa-tycd-gestion        *GPICMT
  "           call 'cmpa-tycd1' using cmpa-tycd adl-art                 *GPICMT
  "           if ocmpa-tycd-edfact = cmmdt-envi-rtn-faux                *GPICMT
                if mmdt-langue = "FR"
  "              string 'Commande non facturable : 'wcdex               *GPICMT
  "                 delimited size into immaf-vali-tit
                else
  "              string 'Order not allowed : 'wcdex                     *GPICMT
  "                 delimited size into immaf-vali-tit
                end-if
  "              move '2' to ocglp-fact-rtn                             *GPICMT
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
  "              perform erreur                                         *GPICMT
  "              go to fin                                              *GPICMT
DD0298     end-if

           END-IF.
      *
      **** DEBUT FACTURE ****
      *
       T20.

      *-------------------------------------                            *GPICMT
      * si fcafa > 1 ==> fini                                           *GPICMT
      * si fcfac > 0 ==> fini                                           *GPICMT
           if fcafa > 1 or fcfac > 0 go to fin1.                        *DDE027

DDE153* flag debut trt d'une facture
           perform ini-fac.

GPICMT* appel gestion du type de commande
DD0298     initialize wcmpa-tycd
  "        move fcfeo to wcmpa-tycd-typ                                 *GPICMT
  "        move ccmpa-tycd-gestion-tycd to icmpa-tycd-gestion           *GPICMT
  "        call 'cmpa-tycd1' using cmpa-tycd adl-art                    *GPICMT
  "        if ocmpa-tycd-edfact = cmmdt-envi-rtn-faux                   *GPICMT
  "           go to t420
DD0298     end-if

DD0351* GPIWARNING a faire seulement si on est pas en ARC
           IF icglp-fact-arc = spaces
DDE153* test si factures a regrouper demande
              IF icglp-fact-e1regroup = '1'
                 if fcregrfa not = '1'
                    go to t420
                 end-if
              ELSE
                 if fcregrfa = '1'
                    go to t420
                 end-if
              END-IF
           END-IF

      * controle facture a dematerialiser si demander sauf si facture a *GPICMT
      *                                               la demande        *GPICMT
           if wcdex = zero                                              *GPICMT
              if fcfdem = "1"                                           *GPICMT
                 if icglp-fact-e1dem not = 'D'                          *GPICMT
                 go to t420
                 end-if
              else
                 if icglp-fact-e1dem = 'D'
                    go to t420
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
           IF FCAFA NOT = 1 GO TO T420.
           IF FCLIV NOT = 2 GO TO T420.
           IF FCFAC NOT = ZERO GO TO T420.
           IF FCICP NOT = ZERO GO TO T420.

       t20a.

GPICMT* appel gestion du type de commande
DD0298     initialize wcmpa-tycd
  "        move fcfeo to wcmpa-tycd-typ                                 *GPICMT
  "        move ccmpa-tycd-gestion-tycd to icmpa-tycd-gestion           *GPICMT
  "        call 'cmpa-tycd1' using cmpa-tycd adl-art                    *GPICMT
  "        if ocmpa-tycd-edfact = cmmdt-envi-rtn-faux                   *GPICMT
  "           go to t420                                                *GPICMT
DD0298     end-if

      *
      * controle Taux de la devise de la commande
      * appel fonction lecture devise
           move 'f' to immpa-devi-trt
           move fcdev  to wmmpa-devi-cdev9
           move 'C' to immpa-devi-tfc
DDE089     move ' ' to immpa-devi-aff
           call 'mmpa-devi1' using mmpa-devi adl-art
           if ommpa-devi-rtn not = '0'
             if mmdt-langue = "FR"
DD0316        string 'Cde: ' fccle-cdesup ' ' ommpa-devi-liberr
                   delimited size into immaf-vali-tit
             else
DD0316        string 'Order: ' fccle-cdesup ' ' ommpa-devi-liberr
                   delimited size into immaf-vali-tit
             end-if
              move ommpa-devi-rtn    to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to t420
           else
      ***************** si taux en euro = zero on ne traite pas la commande
              if ommpa-devi-teu = zero
                if mmdt-langue = "FR"
                 string 'Cde: ' fccle-cdesup ' ' "TAUX DEVISE ???? :  "
                   delimited size into immaf-vali-tit
                else
                 string 'Order: ' fccle-cdesup " RATE CONVERSES ???? : "
                   delimited size into immaf-vali-tit
                end-if
                 move '2' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
                 go to t420
              end-if
           END-IF.
           move ommpa-devi-teu  to wpgbteu.
           move ommpa-devi-ceu  to wpgbceu.
      *    move ommpa-devi-ldev to wlde.
           move spaces          to wlde.


      *----> M1298 (D)
           move spaces to wcec.
           move fcncl to clncl.
           perform rnl-fclients.
           if file-status not = zero
             if mmdt-langue = "FR"
              string 'Cde: ' fccle-cdesup ' Client: ' fcncl
                     ' Cinexistant'
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
             else
              string 'Order: ' fccle-cdesup ' Customer ordered: ' fcncl
                     ' not found'
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
             end-if
              move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to t420
           end-if

DD0221     move clrdi to wrdi

GPICMT* memo taux com representant depuis fiche client
GPICMT* permet d'imposer un taux de commission pour un client et l'ensemble
GPICMT* de ses commandes sans trt a la ligne
DDE046     move clcor to w-fbtrh

DD0002*GPICMT on prend le code calcul echeance sur le client payeur
DD0002*    if clnaf = zero or clnaf = 3 or clnuf = zero
DD0002*                              move clcec to wcec
DD0002*                              go to  t20f.
DD0002*    move clnuf to clncl.
DD0002*    perform rnl-fclients.
DD0002*    if file-status not = zero
DD0002*       string 'Cde: ' fccle ' Client: ' fcncl ' inexistant'
DD0002*              ' (' file-status ')'
DD0002*              delimited size into immaf-vali-tit
DD0002*       move '3' to ocglp-fact-rtn
DD0002*       perform erreur
DD0002*       go to t420
DD0002*    end-if

DD0002*    move clcec to wcec.
DD0002*t20f.
      *----> M1298 (F)

      * recherche des remises pieds de facture pour test si remise SP   *GPICMT
DD0316     move fccle-cdesup to wcmta-comi-ncd.                         *DDE033
           call "cmta-comi3" using wcmta-comi adl-art.                  *DDE033

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
      *
      ** MISE EN WSS DE L'ENTETE **
      *
           MOVE SPACE TO WAD WORIDUP WFACAVO WPROCON.
           MOVE ZERO TO WCODP5 WCODP7.
      ***  TT DES NOUVEAUX BLOCS ADRESSES  (JUIN 90)                 ***
DD0002*    MOVE FCNUM TO ALNUM.
DD0002*    MOVE FCNIN TO ALNIN.
DDE153     move fcfacturea to walfac.
      *M1096 -----------------------------------------------------------------
           move zero to walliv.
      *M1096 -----------------------------------------------------------------
      *M0197 -----------------------------------------------------------------
DDE153*    move zero to walfac.
      *M1096 -----------------------------------------------------------------

      *----> M0798 (D)
           move spaces to wclcrf.
      *----> M0798 (F)

DDE089     move spaces to wclcvfrf.
      *----> M1199 (D)
           move zero to wlngfac.
      *----> M1199 (F)

DD0002* appel fct bloc adresse commande
DD0002*    perform r-fcoadcli.
DD0002*    if file-status not = zero
DD0002*          GO TO T25.
      *M1096 -----------------------------------------------------------------
DD0002*    move alliv to walliv.
      *M1096 -----------------------------------------------------------------
      *M0197 -----------------------------------------------------------------
DDE153*    move alfac to walfac.
      *M0197 -----------------------------------------------------------------
DD0002*    IF ALADC = "C"        GO TO T25.
      ***  TT DU LIVRE A                                             ***
DD0002*    MOVE ALLIV TO CLNCL.
DD0316     move fccle-cdesup to icmcd-gest-numcde
ELGU17     move '0' to wgeo-livrea
           call "cmcd-gest1" using cmcd-gest adl-art
           if ocmcd-gest-rtn = cmmdt-envi-rtn-ok
V20002        MOVE ocmcd-gest-livrea-nom TO WAD6
V20002        MOVE ocmcd-gest-livrea-bureau TO wad7
V30002        move ocmcd-gest-livrea to walliv
V30002        MOVE ocmcd-gest-facturea-nom TO WAD1
V30002        MOVE ocmcd-gest-facturea-raison TO WAD2
V30002        MOVE ocmcd-gest-facturea-rue TO WAD3
V30002        MOVE ocmcd-gest-facturea-ville TO WAD4
V30002        MOVE ocmcd-gest-facturea-bureau TO WAD5
GPICMT* recherche code geographique du livre a
ELGU17        perform rech-geo
GPICMT* controle validite code taxe avec code pays
DD0448        if mmdt-societe not = 'SLOVAQ' and not = 'CHINE'
  -              perform ctrl-taxe
  -              if ommpa-vtax-rtn not = cmmdt-envi-rtn-ok
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
  -                 perform erreur
  -                 go to t420
  -              end-if
DD0448        end-if
           else
             if mmdt-langue = "FR"
              string 'Cde: ' fccle-cdesup ' Client livre Inexistant'
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
             else
              string 'Order: ' fccle-cdesup
                     ' Customer delivered not found'
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
             end-if
              move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to t420
           end-if
           .

      ***  TT DU FACTURE A                                           ***
DDE153     IF walfac = CLNCL GO TO T21.
DDE153     MOVE walfac TO CLNCL.
           perform r-fclients.
           if file-status not = zero
             if mmdt-langue = "FR"
DDE153        string 'Cde: ' fccle-cdesup ' Client: ' walfac
                     ' Finexistant' ' (' file-status ')'
                     delimited size into immaf-vali-tit
             else
DDE153        string 'Order: ' fccle-cdesup ' Customer invoiced: '
                     walfac ' not found' ' (' file-status ')'
                     delimited size into immaf-vali-tit
             end-if
              move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to t420
           end-if
           .
       T21.
      *----> M1199 (D)
      *
      *    memo code langue client
           move cllng to wlngfac.
      *----> M1199 (F)

      *----> M0798 (D)
           move clcrf to wclcrf.
      *----> M0798 (F)

DDE089     move clcvfrf to wclcvfrf.
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
DDE153*    IF ALREG = CLNCL GO TO T22.
DDE153*    MOVE ALREG TO CLNCL.
DDE153     IF fcreglepar = CLNCL GO TO T22.
DDE153     MOVE fcreglepar TO CLNCL.
DDE153     perform rnl-fclients.
           if file-status not = zero
             if mmdt-langue = "FR"
DD0316        string 'Cde: ' fccle-cdesup ' Client: ' fcreglepar
                     ' Rinexistant' ' (' file-status ')'
                     delimited size into immaf-vali-tit
              else
DD0316        string 'Order: ' fccle-cdesup ' Customer payment: '
                      fcreglepar ' not found' ' (' file-status ')'
                     delimited size into immaf-vali-tit
              end-if
              move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to t420
           end-if
           .

       T22.
           MOVE CLNCL TO WNCLP.
           MOVE CLGEO TO WGEOP.
DD0424*    MOVE CLCRE TO WCRE.
           MOVE CLCON TO WCON.
DD0002     move clcec to wcec.
DD9999     move clcrt to w-regl
GPICMT* en reedition on prend le mode de reglement et les conditions dans ffacture
DD0351     if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
  -           move w-fbreg to w-regl
  -           move w-fbcjl to WCON
DD0351     end-if
            

           move clcsu to w-suc
           move clgen to w-gen

           MOVE CLDOM TO WDOM.
           MOVE CLDOA TO WDOA.
       T40.
      *DDE089 controle code reglement                                   *GPICMT
           move 'C' to immpa-regl-cof
DDE153     move w-regl to wmmpa-regl-regl
DD0387     move spaces to immpa-regl-choix
           call 'mmpa-regl1' using mmpa-regl adl-art
           if ommpa-regl-rtn not = '0'
              move ommpa-regl-rtn    to ocglp-fact-rtn
             if mmdt-langue = "FR"
DD0316        string 'Cde: ' fccle-cdesup ' ' w-regl ' '
                          ommpa-regl-liberr
                   delimited size into immaf-vali-tit
             else
DD0316        string 'Order: ' fccle-cdesup ' ' w-regl ' '
                          ommpa-regl-liberr
                   delimited size into immaf-vali-tit
             end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to t420
           end-if

      * DDE089 controle conditions de reglement
           move 'C' to immpa-regl-choix
           move wcon to wmmpa-regl-con9
           call 'mmpa-regl1' using mmpa-regl adl-art
           if ommpa-regl-rtn not = '0'
             if mmdt-langue = "FR"
DD0316        string 'Cde: ' fccle-cdesup ' Cond. de reglement ??? '
                             wcon
                             delimited size into immaf-vali-tit
             else
DD0316        string 'Order: ' fccle-cdesup ' regulations KO ??? '
                             wcon
                             delimited size into immaf-vali-tit
             end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
DD0221* GPIWARNING tant que l'on a pas agrandi la zone ds l'entete de commande
DD0221* GPIWARNING on edite la ref commande de fcommac1 pour BRICORAMA
DD0221*       if wrdi = "BRM" or = "BDP" or = "BTK"                     *GPICMT
DD0221*       if wrdi = "BRM" or = "BTK"                                *GPICMT
DD0221*            or = "PLA" or = "BKD" or = "LAP"                     *GPICMT
DD0465*            or = "C01" or = "SYU"                                *GPICMT
DD0221*          perform rech-refcde                                    *GPICMT
DD0221*       end-if
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
DD0351        if mmdt-societe = 'GPI' and fcfra = 2 
DD0351           and ocmcd-gest-livrea-pays not = 'FR'
DD0351           move spaces to WPORT
DD0351        end-if
           END-IF
DDE130*    MOVE WDATE TO WDATEF.
DDE130     MOVE WJ TO WDATJ.
DDE130     MOVE WM TO WDATM.
DDE130     MOVE WA TO WDATA.
DDE130     move '/' to wsl1 wsl2.

DDE089     IF icglp-fact-e1red not = 'R'
DD0362        and icglp-fact-arc = spaces
              if WTEST1 = 1 MOVE WNFACA TO WLNFAC
                            ADD 1 TO WNFACA
              else
GPICMT* pour oyobrico on prend la plage 3 pour les avoirs
                 if mmdt-societe = 'OYOBRICO'
                    if fcfoa = 5 or = 6 or = 7
                       move WNAVOI to WLNFAC
                       add  1 to WNAVOI
                    else
                      MOVE WNFANA TO WLNFAC
                      ADD 1 TO WNFANA
                    end-if
                 else
                      MOVE WNFANA TO WLNFAC
                      ADD 1 TO WNFANA
                 end-if
              end-if
           END-IF.
      *
      ** INITIALISATION PIED DE FACTURE **
      *
       T61.
DDE153     move 1 to w-creat
DD0316     MOVE spaces TO wor-ffacture.
DD0316     MOVE WLNFAC TO fbcle-cdesup
DD0316* pour pouvoir reediter une ancienne facture leau je charge
DD0316* la facture demandee
DD0316     if icglp-fact-e1red = 'R'
DD0316        move fcnfa-cdesup to fbcle-cdesup
DD0316     end-if
GPICMT* si refacturation automatique dinac, on charge la date de commande
DD0557     if fcrcl(6:9) = "REFACAUTO"
              move fcjou to wj
              move fcmoi to wm
              move fcann to wa
           end-if
           MOVE WDATE9 TO FBDAF.
           MOVE FCFOA  TO FBCFA.
DD0326*    MOVE FCCLE  TO FBNCD.
DD0326     MOVE FCCLE-cdesup  TO FBNCD-cdesup.
           MOVE WGEOL  TO FBPML.
           MOVE FCNCL  TO FBNCL.
           MOVE WGEOP  TO FBPMP.
           MOVE WNCLP  TO FBNCP.
DDE153*    MOVE FCREP  TO FBNRH.
DDE153     MOVE w-repr TO FBNRH.
DD0394     MOVE w-type TO FBTYP.
DDE046     move w-fbtrh to fbtrh.
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
           MOVE 90     TO WCPTR.
           PERFORM TITRE THRU FTITRE.
      *----> M1098 (D)
           move zero to whtmb whtmb9(1) whtmb9(2).
      *----> M1098 (F)
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
DD0448     IF WGEOL NOT = 1 GO TO edit-ltaxe.
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
      *M0197 -----------------------------------------------------------------
      *          GO TO T64.
      *    MOVE CLSNTV TO WNIC.
DD0448*          go to t63a.
DD0448*    if clsntv not = spaces move clsntv to wnic go to t64.
DD0448*t63a.
DD0448*    move fcncl to clsncl.
DD0448*    if walfac not = zero move walfac to clsncl.
      * on doit editer le code intracom du reglepar si so pays = celui du livre
      * sinon on edite celui du livrea
DD0448     if ocmcd-gest-reglepar-pays = ocmcd-gest-livrea-pays
              MOVE fcreglepar  TO CLSNCL
           else
              move fclivrea to CLSNCL
           end-if
           perform r-clisuite.
           if file-status not = zero go to t64.
           move clsntv to wnic.
      *M0197 -----------------------------------------------------------------
       T64.
GPICMT* message si numero intracommunautaire vide
DD9999     if wnic = spaces
             if mmdt-langue = "FR"
DD0316        string "Numero Intracom Vide,  Commande " fccle-cdesup
                     " traitee mais a reediter"
                    delimited size into immaf-vali-tit
              else
DD0316        string "Number TVA space,  Order " fccle-cdesup
                     "  a edit"
                    delimited size into immaf-vali-tit
              end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
DD0448     evaluate fbtax
  -          when 3
  -             string "     "
  -                  "MARCHANDISE DEBITEE EN EXONERATION DE TAXES "
  -                  "DESTINEE A L'EXPORTATION" 
  -                       delimited size into ligne
  -             WRITE LIGNE BEFORE 2
  -             MOVE SPACES TO LIGNE
  -             ADD  2      TO WCPTR
  -          when 4
  -           string "     "
  -                  "EXONERATION DE TVA EN FRANCE - ARTICLE 262 TER.1 "
  -                "DU CGI " delimited size into ligne 
  -           WRITE LIGNE BEFORE 2
  -           MOVE SPACES TO LIGNE
  -           ADD  2      TO WCPTR
  -          when 5
  -           string "     "
  -                  "TAUX DE TVA SPECIFIQUE A LA CORSE "
  -                          delimited size into ligne 
  -           WRITE LIGNE BEFORE 2
  -           MOVE SPACES TO LIGNE
  -           ADD  2      TO WCPTR
  -          when 6
  -           string "     "
  -                  "EXONERATION DE TVA EN FRANCE - ARTICLE 196 DE LA "
  -                  "DIRECTIVE 2006/112/CE" delimited size into ligne
  -           WRITE LIGNE BEFORE 2
  -           MOVE SPACES TO LIGNE
  -           ADD  2      TO WCPTR
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
  -                       delimited size into ligne
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
  -          when 9
  -             string "     "
  -                    "EXONERATION DE TVA SUIVANT ARTICLE 272-2"
  -                    " ET 283-4 DU CGI"
  -                       delimited size into ligne
  -             WRITE LIGNE BEFORE 2
  -             MOVE SPACES TO LIGNE
  -             ADD  2      TO WCPTR
DD0448     end-evaluate
           .
      ***  FIN 2 LIGNES EN TETE DE FACTURE      ***
      **** TRAITEMENT LIGNES DETAIL ****
      *
       LECECC1.
DDE153     if w-regroup = '1'
              perform entete-cde
           end-if

GPICMT* edition commentaire entete location gerance
DD0425     if fcfeo = ccmpa-tycd-typ-locger(1:1)
  -           perform edit-com-locger
DD0425     end-if

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
      *07/96----------------------------------------------------------->
           move zero to fclig.
      *07/96----------------------------------------------------------->
           move spaces to fcart.
DDE069* on edite plus les lignes ds l'ordre des articles mais de la commande
DDE069* suite a la demande de BRICORAMA et a cause des dicplay
DDE069*    perform snlsk3-fcommac2.
DDE069     move "04"   to fcnel.
DDE069     move zero   to fcunix2.
DD0316     move fccle-cdesup  to fcnoc2-cdesup
DDE069     perform snl-fcommac2.
           if file-status not = zero
                GO TO lecc4.
       lec2.
           perform n-fcommac2.
           if file-status not = zero GO TO lecc4.
DD0316     if fcnoc2-cdesup not = fccle-cdesup go to lecc4.

           if icglp-fact-arc = spaces
      * on n'edite pas les ruptures (qtl = 0)
              if fcqtl = zero
                 go to lec2
              end-if
              move fcqtl to wqteliv
           else
              move fcqtc to wqteliv
           end-if
         .
      *
       T92.
      *----> M1098 (D)
           move 01    to fanma fanma1.
           move fcnar to fanar fanar1.
           move fcsre to fansr1.
           perform rnl-fartusap.
           if file-status not = zero move spaces to facom.
      *----> M1098 (F)
           perform rnl-fartusac.
           if file-status not = zero move zero to facip.
           IF FCLIG NOT = 1 GO TO T100.
           PERFORM TEST2b THRU FTEST2b.
           IF WTEST2 = 3 GO TO DISP.
      *
       t92b.
           MOVE FCPDU  TO WPDU.
           MOVE FCDES  TO WDES.
      *----> M1199 (D)
      *
      ****** ed libelle langue etrangere
           if wlngfac = zero go to t92c.
           move wlngfac to lglng.
           move fcnar to lgnar.
           perform rnl-languear.
           if file-status = zero move lglia to wdes.
      *----> M1199 (F)
       T92C.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE ZERO TO WTEST6.
      *DDE089 avant on editait la reference article du client
      *       edition gencode +ref+sous ref
DDE057     move fagean14 to lean14
DDE057     move fapay to lpays
DDe057     move facnu to lcnuf
DDE089     move facip to lcip.
DDE057     move facle to lcle13
DDE089     MOVE fcnar TO LNREF.
DDE089     move fcsre to lsref.
           MOVE WDES TO LDESAR.
DDE057*    MOVE fcmes TO LUV.
DDE057* GPIWARNING mise en dur de l'unite de vente car le code edite
      *            coorespond a un libelle preimprime sur la facture
      *1=l'unite,5=le cent,6=le mille
           move ' 1' to luv
           IF fcprx = 2
              move ' 5' to luv
           else
              if fcprx = '3'
                 move ' 6' to luv
              end-if
           END-IF

           IF WDEC NOT = ZERO AND fcqpb > 1 MOVE 1 TO WTEST3 WTEST6.
      ***MODIF DE LA FACTURATION 4/87 ***( GO TO T92D. NON REPRIS ICI)
           IF fcmes > 3 AND fcmes < 7 GO TO T92D.
           GO TO T92E.
       T92D.
           IF fcmes = 06 MULTIPLY 1000 BY WQTELIV
                   ELSE MULTIPLY  100 BY WQTELIV.
       T92E.
DDE057*    IF WTEST6 = 1 MOVE "*" TO LNBTESX GO TO T92EA.
DDE057*    IF fcqpb > 1   MOVE WENT TO LNBTES.
       T92EA.
DD0219* conversion des boites en pieces si besoin
DDE057*    IF fcqpb   > 1 MULTIPLY fcqpb BY WQTELIV.
DD0219     move wqteliv to lqtefa zqtefav
           move 1 to immca-qtes-cod
           move fcqpb to immca-qtes-con
           move wqteliv to immca-qtes-qte
           call 'mmca-qtes1' using wmmca-qtes adl-art
           if ommca-qtes-rtn = spaces
              move ommca-qtes-qtr to wqteliv
           end-if

           MOVE WQTELIV  TO WQTEFA.
DDE057*    IF WQTEFA > 99999 MOVE WQTEFA TO LQTEFA
DDE057*      ELSE            MOVE WQTEFA TO LQTEFAU.
DD0219     if mmdt-societe = 'GPI'
DD0301          or = "ERELS"
              MOVE WQTEFA TO LQTEFA zqtefav
           end-if
DDE057*    IF LQTEFAD = "00" MOVE SPACE TO LQTEFAD.
           MOVE fcpht TO LPU.

      *----> M0798 (D)
           if wclcrf not = "N" move fcpbas to lpub
                               move fctrpv to lprem.
      *----> M0798 (F)

      * calcul montant ligne ==> tcgca-mtht-htl
           if icglp-fact-arc not = spaces
              move fcqtc to tcgca-mtht-qte
           else
DD0219        move fcqtl to tcgca-mtht-qte
           end-if
DD0219     move fcmes to tcgca-mtht-mes.
DD0219     move fcprx to tcgca-mtht-prx.
DD0219     move fcpht to tcgca-mtht-pht.
DD0219     move fcqpb to tcgca-mtht-qpb.
DD0219     call 'cgca-mtht2' using cgca-mtht adl-art
           .
       T95.
DD0219*    ADD wta-resu TO WMT4 (WTEST2).
DD0219     add tcgca-mtht-htl to WMT4 (WTEST2)
      *----> M1098 (D)
DD0219*    if facom not = "N" add wta-resu to whtmb9(wtest2).
DD0219     if facom not = "N" add tcgca-mtht-htl to whtmb9(wtest2).
      *----> M1098 (F)

      * edition "*" pour ligne si remise SP trouvee                     *GPICMT
DD0350*    if wcmta-comi-spe = "1" and facom not = "N"                  *DDE033
DD0350*                       move "*"   to lpremas.                    *DDE033
DDE057*                       move "*"   to lsig.                       *DDE033

DD0219*    MOVE wta-resu TO LMONT.
DD0219     MOVE tcgca-mtht-htl TO LMONT.
DDE057     IF ZQTEFAD not = "00" MOVE l3 to wl31
DDE057                           MOVE WQTEFA TO LQTEFAV
DDE057                           move wl31 to ligne.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
GPICMT* edition nomenclature douaniere pour export hors CEE
999999     if mmdt-logn = "DINA"
999999  display "trace cglp-fact1 fcfoa " fcfoa "'"
999999  display "trace cglp-fact1 fcnpo " fcnpo "'"
999999  display "trace cglp-fact1 wgeo-livrea " wgeo-livrea "'"
           end-if
ELGU17     if fcfoa < 5 and FCNPO not = '999999999' and not = spaces
  -           if wgeo-livrea not = '0' and not = '1'
  -              string 'Douane : ' FCNPO delimited size into LLIB
999999     if mmdt-logn = "DINA"
999999  display "trace cglpfact1 ecriture douane "
           end-if
  -              write ligne before 1
  -              add 1 to wcptr
  -              move spaces to ligne
  -           end-if
ELGU17     end-if
           .
      *
       lecc3.
      ********  lecture    des elements 04 : libelles complem. *******
DD0316     move zeroes to fccle3-cdesup
DD0316     move fccle-cdesup  to fccle3-cdesup
           move 04     to fcnel3.
           move fcart  to fcart3.
           move 10     to fclig3.
           perform snl-fcommac3.
           if file-status not = zero
                GO TO t95a.
       lec3.
           perform nnl-fcommac3.
           if file-status not = zero
                GO TO t95a.
DD0316     if fcnum3 not = fcnum or fcnin3 not = fcnin
DD0316      or fccle3-newcde not = fccle-newcde
DD0316      or fccle3-newsoc not = fccle-newsoc go to t95a.
           if fcart3 not = fcart go to t95a.
DDE069     if fcnlg3 not = fcnlg go to lec3.
           perform t110 thru t110e.
           go to lec3.
      ***  TT INTRACOMMUNAUTAIRE POUR LES ELT 4     ***
       T95A.
      *    IF WGEOL NOT = 1 GO TO T97.
           if fcfoa     = 9 go to t95g.
           IF WGEOL NOT = 1 GO TO T95G.
       T95B.
           MOVE SPACES TO ENRICO.
           MOVE FBNCL  TO ICCLF.
           MOVE fbnfa TO ICNFA.
           MOVE FBCFA  TO ICCFA.
           MOVE FBTAX  TO ICCTA.
           MOVE FBDEV  TO ICCDE.
           MOVE "L"    TO ICCIL.
           MOVE WNIC   TO ICNIC.
           MOVE WDATE9 TO ICDFA.
           MOVE WAD1   TO ICRCF.
DD0316*    MOVE WNUM   TO ICNOC.
DD0316*    MOVE WNIN   TO ICNIN.
DD0316     MOVE wcle   TO ICNCD-cdesup.
           MOVE WPORT  TO ICPOR.
       T95C. EXIT.
       T95D.
DDE046     MOVE fcnar  TO icxar.
           move fcsre  to icsrf.
           MOVE fcqpb   TO ICQPB.
           MOVE WQTEFA TO ICQTL.
           MOVE fcpht   TO ICPU.
DD0219*    MOVE wta-resu TO ICMON.
DD0219     MOVE tcgca-mtht-htl TO ICMON.
           MOVE fcmes   TO ICUM.
           MOVE fcprx   TO ICUP.
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

DDE089     if icglp-fact-e1red = 'R' go to t95g.

DDE175*    perform w-intracom.
DDE175*    IF file-status NOT = "00"
DDE175*       string 'Cde: ' fccle ' ANOMALIE / INTRACOM, ST: '
DDE175*                               file-status
DDE175*                        delimited size into immaf-vali-tit
DDE175*       move '3' to ocglp-fact-rtn
DDE175*       perform erreur
DDE175*       perform subnum
DDE175*       go to fin1.
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
             if mmdt-langue = "FR"
DD0316        string 'Cde: ' fccle-cdesup ' type ligne #1/2, TL: ' fclig
                   delimited size into immaf-vali-tit
             else
DD0316        string 'Order: ' fccle-cdesup ' line #1/2, TL: ' fclig
                   delimited size into immaf-vali-tit
             end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to lec2.
      *
      *  RECHERCHE TAUX COMMISSION DANS ART./DEVIS
           IF WITC NOT = 9  GO TO T102F.
           MOVE FBNCL TO ADNCL1 wadncl.
           MOVE FCNAR TO ADNAR1 wadnar.
           MOVE FCSRE TO ADSRF1 wadsrf.
           move 02    to adtye1.
           move 00    to adunix1.
           perform r-artdevc1.
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
           perform r-artdevc1.
           if file-status not = zero
                     GO TO T102F.
           GO TO T102.
      *
       T102F.
           PERFORM TEST2b THRU FTEST2b.
           IF WTEST2 = 3 GO TO DISP.
           go to t92b.
      *
      * TRT LIGNE > 9 *
      *
       T110.
           IF FCCL1 NOT = "G" AND FCCL1 NOT = "F" GO TO T110b.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FCLC1 TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T110b.
           IF FCCL2 NOT = "G" AND FCCL2 NOT = "F" GO TO T110c.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FCLC2 TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       T110c.
           IF FCCL3 NOT = "G" AND FCCL3 NOT = "F" GO TO t110e.
DD0122*    IF WCPTR > 34 write ligne before page
DD0122     perform saut34
           MOVE FCLC3 TO LLIB.
           WRITE LIGNE BEFORE 1.
           ADD 1 TO WCPTR.
           MOVE SPACE TO LIGNE.
       t110e. exit.
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
999999   display "facr cde " fcnoc4-cdesup " (" fccle-cdesup ")"
DD0316         if fcnoc4-cdesup not = fccle-cdesup
DD0351            perform rw-fcommac4 go to flec4.
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
           MOVE FCQUL TO LQTEFA zqtefav.
DDE057*    IF LQTEFAD = "00" MOVE SPACE TO LQTEFAD.
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
      *----> M1098 (D)
DD0219*    add wta-resu to whtmb9(wtest2).
DD0219     add tcgca-mtht-htl to whtmb9(wtest2).
      *----> M1098 (F)
           MOVE WMONT TO LMONT.
DDE057     IF ZQTEFAD not = "00" MOVE l3 to wl31
DDE057                           MOVE fcqul TO LQTEFAV
DDE057                           move wl31 to ligne.
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
           IF FCSIG = "-" SUBTRACT FCMON FROM WMT6 (WTEST2)
             ELSE         ADD      FCMON TO   WMT6 (WTEST2).
           IF WGEOL = 1 GO TO T150A.
           GO TO lec4.
      *
      ** TRT EL.7 **
      *
       T140.
           IF FCNEL4 NOT = 7 GO TO T150.
      *----> M1098 (D)
      *    ADD 1 TO WCPT7.
           move wcpt7 to wcpt7a.
           move fcdop to wfcdop.
      *    if wmb = "REMISE MB"move 1 to wcpt7 go to t142.              *DDE022
DD0350*    if wmb = "REMISE SP"move 1 to wcpt7 go to t142.              *DDE022
           if wcpt7 = zero add 2 to wcpt7
             else          add 1 to wcpt7.
       t142.
      *----> M1098 (F)
           MOVE FCDOP TO WLIB7 (WCPT7).
DD0362     if icglp-fact-arc = spaces
              MOVE FCQUL TO WQUI (WCPT7)
           else
              move fcqui to WQUI (WCPT7)
           end-if
      *----> M1297 (D)
DD0316     move fccle4-cdesup to wcle7(wcpt7).
      *----> M1297 (F)
           IF FCSIG = "-" MULTIPLY -1 BY WQUI (WCPT7).
      *----> M1098 (D)
           if wcpt7a > wcpt7 move wcpt7a to wcpt7.
      *----> M1098 (F)
           IF WGEOL = 1  GO TO T150A.
           GO TO lec4.
      *
      ** TRT EL.8 **
      *
       T150.
           IF FCNEL4 NOT = 8 GO TO T160.
DDE153     if fcfra = 2 go to lec4.
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
           MOVE FCQUL TO LQTEFA zqtefav.
DDE057*    IF LQTEFAD = "00" MOVE SPACE TO LQTEFAD.
           MOVE FCPUH TO LPU.
           MOVE FCMON TO LMONT.
           IF FCSIG = "-" MOVE "-" TO LSIG
                          SUBTRACT FCMON FROM WMT9 (WTEST2)
             ELSE         ADD      FCMON TO   WMT9 (WTEST2).
DDE057     IF ZQTEFAD not = "00" MOVE l3 to wl31
DDE057                           MOVE fcqul TO LQTEFAV
DDE057                           move wl31 to ligne.
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
           if wnart = spaces and wnsrf = spaces go to lecc5.
           move wnart to ndxar.
           move wnsrf to ndsrfx.
           perform rnl-numdevis.
           if file-status not = zero
              if mmdt-langue = "FR"
                 display
                 "ERREUR : DEVIS INEXISTANT, ARTICLE: " wnart "  " wnsrf
              else
                 display
                 "ERROR : QUOTE NOT FOUND: " wnart "  " wnsrf
              end-if

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
DD0316     if fccle5-cdesup not = fccle-cdesup go to t200.
      *
       T180.
           IF FCNEL5 NOT = 99
              MOVE FCNEL5 TO WDNEL
DD0316*       MOVE FCNUM TO WDNUM
DD0316        MOVE fccle-cdesup(1:7) TO WDNUM
              MOVE FCNIN TO WDNIN
             if mmdt-langue = "FR"
DD0316        string 'Cde: ' fccle-cdesup ' Type elt 5 inconnu: ' wdnel
                     delimited size into immaf-vali-tit
             else
DD0316        string 'Order: ' fccle-cdesup
                     ' recording not found: ' wdnel
                     delimited size into immaf-vali-tit
              end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
      *----> M1098 (D)
           if i > 1 go to t2059.
           move wlib7(i) to wfcdop.
      *    if wmb not = "REMISE MB" go to t205.                         *DDE022
GPICMT* suppression edition remise SP
DD0351*    if wmb not = "REMISE SP" go to t205.                         *DDE022
DD0351     go to t205
DD0122*    IF WCPTR > 31 write ligne before page
DD0122     perform saut31
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 1.

      *    MOVE "BASE HORS TAXES AVANT REMISE" TO LIBCA13.              *DDE033
           MOVE "BASE HORS TAXES AVANT REMISE (LIGNES AVEC *)"          *DDE033
                                               TO LIBCA13.              *DDE033
           add whtmb9(1) whtmb9(2) giving whtmb.
      *
      *********** calcul remise MB sur base whtmb
      *
DDE057*    MOVE WHTMB TO LMONT.
DDE057     MOVE WHTMB TO lmt.
           IF WHTMB < 0 MOVE "-" TO LSIG.
           if whtmb < 0 multiply whtmb by -1 giving wbas7(i)
             else      move whtmb          to     wbas7(i).
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MULTIPLY WQUI (I) BY WHTMB GIVING WREM.
           DIVIDE 100 INTO WREM ROUNDED.
           ADD WREM TO FBREF.
           ADD WREM TO WHT1.
DDE153*    if fbfra not = 2 subtract wmt6(1) from fbht1f.
DDE153     if fcfra not = 2 subtract wmt6(1) from w-fbht1f.
           MULTIPLY WQUI (I) BY whtmb9(1) GIVING WREM1.
           DIVIDE 100 INTO WREM1 ROUNDED.
           ADD WREM1 TO w-FBHT1F.
           SUBTRACT w-FBHT1F FROM WHT1 GIVING w-FBHT2F.
DDE153*    IF FBFRA NOT = 2 ADD WMT6 (1) TO FBHT1F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (1) TO w-FBHT1F.
DDE153*    IF FBFRA NOT = 2 ADD WMT6 (2) TO FBHT2F.
DDE153     IF FCFRA NOT = 2 ADD WMT6 (2) TO w-FBHT2F.
      ***
           MOVE WLIB7 (I) TO LLIB.
DDE057*    MOVE WQUI (I) TO LQTEFA zqtefav.
DDE057     MOVE WQUI (I) TO lprem.
           MOVE WREM TO LMONT.
           move wrem to wmon7(i).
           IF WREM < 0 MOVE "-" TO LSIG.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           ADD 3 TO WCPTR.
      *    MULTIPLY WQUI (I) BY FBNMF GIVING WREM2.
      *    DIVIDE 100 INTO WREM2 ROUNDED.
           ADD WREM TO w-FBNMF.
           go to t205b.
       t2059.
      *----> M1098 (F)
DD0122*    IF WCPTR > 31 write ligne before page
DD0122     perform saut31
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 1.
           MOVE "BASE HORS TAXES AVANT REMISE" TO LIBCA13.
DDE057*    MOVE WHT1 TO LMONT.
DDE057     MOVE WHT1 TO lmt.
           IF WHT1 < 0 MOVE "-" TO LSIG.
      *----> M1297 (D)
           if wht1 < 0 multiply wht1 by -1 giving wbas7(i)
             else      move wht1           to     wbas7(i).
      *----> M1297 (F)
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
DDE057*    MOVE WQUI (I) TO LQTEFA zqtefav.
DDE057     MOVE WQUI (I) TO lprem.
           MOVE WREM TO LMONT.
      *----> M1297 (D)
           move wrem to wmon7(i).
      *----> M1297 (F)
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
DD0316     move zeroes to fccle4-cdesup
DD0316     move fccle-cdesup  to fcnoc4-cdesup
           move 10     to fcnel4.
           move zeroes to fcunix4.
           perform snl-fcommac4.
           if file-status not = zero
                GO TO t213.
       t211.
           perform n-fcommac4.
           if file-status not = zero
                GO TO t213.
DD0316     if fcnoc4-cdesup not = fccle-cdesup
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
DD0362* pas de mise a jour code facture si edition arc
           if icglp-fact-arc = spaces
DD0316        MOVE fbcle-cdesup TO FCNFA-cdesup
              MOVE 1 TO FCFAC
           end-if

DDE089     if icglp-fact-e1red not = 'R'
              perform rw-fcommaap
              if file-status not = zero
                if mmdt-langue = "FR"
DD0316           string 'Reecriture FCOMMAAP impossible: ' fccle-cdesup
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                else
DD0316           string 'Updating FCOMMAAP impossible: ' fccle-cdesup
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                end-if
                 move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
                 go to fin1
              end-if
GPICMT* si facturation commande allotie mere ==> maj des filles
DD0298        if fcfeo = ccmpa-tycd-typ-allotie(1:1)                    *GPICMT
  "              perform maj-filles                                     *GPICMT
  "              if ocglp-fact-rtn not = cmmdt-envi-rtn-ok
  "                 go to fin1
  "              end-if
DD0298        end-if
           END-IF.

      *---------------------
      * Ecriture de la trace
      *---------------------
           if icglp-fact-e1red not = 'R'
DD0350        and icglp-fact-arc = spaces
              move space to wmmtr-trac
              move "C"   to immtr-trac-type
DD0316        string fccle-cdesup delimited by size into immtr-trac-num
DD0062        move "M"   to immtr-trac-action
              move wnom-prog to immtr-trac-prog
DD0316        string 'Facturation de la commande: ' fccle-cdesup
DD0316               '  sous le n.: ' fbcle-cdesup
                      delimited by size into immtr-trac-commentaire
              call 'mmtr-trac1' using mmtr-trac adl-art
DD0712* -- Si openbook, on lance les messgaes de livraison et reliquat
DD0712* -- => lance en batch car risque de desynchro boucle fcommaap
              if fcopenbook not = 0
                 move space to sys-var var-data
                 string 'GPIPROC' x'00' delimited by size into var-name
                 move space to var-data
                 call 'genvcc' using var-name var-data

                 string var-data delimited by space 
                        "/cgoscdmg " fccle-cdesup X'00' 
                        delimited by size 
                        into sys-var
                call "systcc" using sys-var syst-rtn
              end-if
           end-if.

         .
      *
      * TEST DUPLICATA *
      *
       T410A.
GPICMT* si edition laser on ne fait pas de duplicata
ELGU17     if mmdt-societe = "SLOVAQ"
  -           or mmdt-societe = "ERELS"
  -           or mmdt-societe = "PLASTO"
  -           or mmdt-societe = "OYOBRICO"
  -           or mmdt-societe = "CHINE"
DD0516        or mmdt-societe = "GPI"
  -           or icglp-fact-arc not = spaces
  -           or icglp-fact-pdf = "O"
  -           go to t419
ELGU17     end-if
           SUBTRACT 1 FROM WWNBF.
      *----> M1297 (D)
      *    IF WWNBF < 1 GO TO T420.
           IF WWNBF < 1 GO TO T419.
      *----> M1297 (F)
DDE153* GPIWARNING pas de duplicata pour cde regroupee
           if w-regroup = '1'
             if mmdt-langue = "FR"
DD0316        string 'Duplicata non édité pour facture ' fbcle-cdesup
                   delimited size into immaf-vali-tit
             else
DD0316        string 'Duplication not edited for invoic ' fbcle-cdesup
                   delimited size into immaf-vali-tit
             end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to t419
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
      *----> M1297 (D)
       T419.
      *
      *********** reecriture base et montant remise pour les elmnts 7
      *
           if wcpt7 = zero go to t420.
           move 1 to i.
       t419a.
           if i > wcpt7 go to t420.
DD0316     move wcle7(i) to fccle4-cdesup.
GPICMT* lecture non blocante si reedition
DD0351     if icglp-fact-e1red not = 'R'
              perform r-fcommac4
           else
              perform rnl-fcommac4
           end-if
           if file-status not = zero go to t419b.
           move wbas7(i) to fcpuh.
           move wmon7(i) to fcmon.

           IF icglp-fact-e1red not = 'R'
              perform rw-fcommac4
              if file-status not = zero
                if mmdt-langue = "FR"
                 string 'Reecriture FCOMMAC4 impossible: ' wcle7(i)
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                else
                 string 'Updating FCOMMAC4 impossible: ' wcle7(i)
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                end-if
                 move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
       t420.
DD0820   initialize wflag-trt-cde
      *DDE089 si facture a la demande ==> fini
DDE153*    if wcdex not = zero go to fin1.
DDE153     if wcdex not = zero go to t420-f.

GPICMT* mecture non blcante si reedition
DD0351     if icglp-fact-e1red = 'R'
              and icglp-fact-arc = spaces
              perform nnl-fcommaap
           else
              perform n-fcommaap
           end-if 
           if file-status not = zero
DD9999*         GO TO FIN1.
DD9999          GO TO t420-f.

DDE153     if icglp-fact-e1red = 'R'
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

DDE153     if icglp-fact-e1red = 'R'
              go to fin1
           end-if
           if wcdex not = zero go to fin1.

           GO TO T20.
       FIN1.
DDE089     if icglp-fact-e1red = 'R'
              go to fini
           end-if
DD0362     if icglp-fact-arc not = spaces
              go to fin
           end-if

           MOVE "FACTURE000" TO PHCLE.
           perform r-parbatch.
           if file-status not = zero
             if mmdt-langue = "FR"
              string 'Relecture PARBATCH impossible: ' PHCLE
                     ' (' file-status ')'
                  delimited size into immaf-vali-tit
             else
              string 'Rereading PARBATCH impossible: ' PHCLE
                     ' (' file-status ')'
                  delimited size into immaf-vali-tit
             end-if
              move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
              go to fin
           end-if
           MOVE WNFANA TO PHANOC (1).
           MOVE WNFACA TO PHANOC (2).
           MOVE WNAVOI TO PHANOC (3).
           perform rw-parbatch.
           if file-status not = zero
             if mmdt-langue = "FR"
              string 'Reecriture PARBATCH impossible: ' PHCLE
                    ' (' file-status ')'
                     delimited size into immaf-vali-tit
             else
              string 'Updating PARBATCH impossible: ' PHCLE
                    ' (' file-status ')'
                     delimited size into immaf-vali-tit
             end-if
              move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
           end-if

           GO TO FIN.
       TEST2.
M0297      if fbtax = 5              move fctvp4 to wegtvp
  |                                  move 1      to wegtv1
M0297                                move wegtvp to fctvp4.
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
M0297      if fbtax = 5              move fctvp  to wegtvp
  |                                  move 1      to wegtv1
M0297                                move wegtvp to fctvp.
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
DD0122*    perform identifiant
           WRITE LIGNE BEFORE page.
           MOVE SPACE TO LIGNE.
          if mmdt-langue = "FR"
           string "FACTURE: " WNFA9 " NON TRAITEE, NBRE DE TAXE > 2"
                  delimited size into immaf-vali-tit
          else
           string "INVOIC: " WNFA9 " NOT TREATED, NUMBER OF TAX > 2"
                  delimited size into immaf-vali-tit
          end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
           perform erreur
           GO TO T420.
      *
      **** EDITION DE L'ENTETE ****
      *
       TITRE.
           MOVE SPACE TO LIGNE.
DD0122*    compute wnbl = 2 - linage-counter of etat
DD0122*    write ligne before wnbl
DD0122*    move '     GPI GROUPE GERGONNE' to ligne
DD0122*    write ligne before 0.
           compute wnbl = 10 - linage-counter of etat.
           write ligne before wnbl.
DD0122*    move spaces to ligne
      *    WRITE LIGNE BEFORE L10.
           IF WCPTR NOT = 90 MOVE "SUITE" TO LSUITE.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE WAD1 TO LNOMPAY.

      *-----------------------------------------                        *GPICMT
      *DDE089 ajout libelle pour facture demateria lise                 *GPICMT
DDE153     if wfcfdem = '1'                                              *GPICMT
              move "*   DEMAT  *" to lcond                              *GPICMT
           else
              move WORIDUP TO LCOND
           end-if
      *-----------------------------------------                        *GPICMT

DDE130     WRITE LIGNE BEFORE 1.
DDE130     MOVE SPACE TO LIGNE.

           MOVE WAD2 TO LNOMPAY.
DDE130     MOVE WFACAVO TO LCOND.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DDE130*    MOVE WFACAVO TO LCOND.
DD0316     MOVE fbcle-cdesup TO LNFACT.
           MOVE WAD3 TO LNOMPAY.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE WPROCON TO LCOND.
           MOVE WAD4 TO LNOMPAY.
GPICMT* pour COFAQ ajout une ligne GPIWARNING a revoir si on regroupe
GPICMT*GPIWARNING plusieurs bl sur une facture
DD0389     if wrdi = 'COF'
  -           move 'Client facture' to lnomliv
DD0389     end-if
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE WNBF TO LNBEX.
DD0188*    MOVE WCODP5 TO LCODP2.
DD0188*    MOVE WBD5   TO LXBD2.
DD0188     move wad5 to lbureau
      *    WRITE LIGNE BEFORE 1.
      *    MOVE SPACE TO LIGNE.
DDE153*    MOVE WAD6 TO LNOMLIV.
DDE153     if w-regroup not = '1'
              MOVE WAD6 TO LNOMLIV
           end-if
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
DDE153     if w-regroup not = '1'
DD0188*       MOVE WCODP7 TO LCODP1
DD0188*       MOVE WBD7 TO LXBD1
DD0188        move wad7 to lbureaul
           end-if
           compute wnbl = 19 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L19.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE FROM WLREF BEFORE 3.
           MOVE SPACE TO LIGNE.
           MOVE ZERO TO WCPTR
           .
      *----> M0797 (D)
      *    IF WDEV = 00 GO TO FTITRE.
      * on n'edite plus le libelle devise en entete
      *    IF wlde = spaces GO TO FTITRE.
      *----> M0797 (F)
      *    MOVE WLDE TO LDEV.
      * en attendant les nouveaux imprimes on edite 'cip' et 'ref.GPI' dans
      * les colonnes
DDE057*    move '  CIP' to lcip
DDE057*    move '  Ref. GPI' to lnrefx
DDE057*    WRITE LIGNE BEFORE 1.
DDE057*    MOVE SPACES TO LIGNE.
DDE057*    ADD 2 TO WCPTR.
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
DD0301        or mmdt-societe = "ERELS"
DD0465        or mmdt-societe = "PLASTO"
DD0516        or mmdt-societe = "GPI"
DD0474        or mmdt-societe = "OYOBRICO"
ELGU09        or mmdt-societe = "CHINE"
DD0351        or mmdt-societe = "DINAC"
DD0362        or icglp-fact-arc not = spaces
DD0362        or icglp-fact-pdf = "O"
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
DD0350   if  icglp-fact-direct not = 3
DD0820   and icglp-fact-direct not = 2
           perform cl-fclients
           perform cl-ffacture
           perform cl-paramgpi
           perform cl-fcommaap
           perform cl-fcommac1
           perform cl-fcommac2
           perform cl-fcommac3
           perform cl-fcommac4
           perform cl-fcommac5
           perform cl-fartusap
           perform cl-languear
           perform cl-intracom
           perform cl-artdevc1
           perform cl-clisuite
DD0002*    perform cl-fcoadcli
DD0350   end-if
         .
DDE089 fini.
           exit program.

      *=========================================================================
      *                              FONCTIONS LOCALES
      *=========================================================================


      *DDE089 affichage fenetre d'erreur
       ERREUR section.
999999   display "traitement erreur"
DD0820* si traitement batch...
  |      if icglp-fact-direct = 2
  |        move space to immlp-mail-type (4) immlp-mail-data(4)
  |        move space to immlp-mail-type (5) immlp-mail-data(5)
  |   * ..si commande en cours de traitement, on la bloque avant de sortir
  |        if wflag-trt-cde = "1"
  |          move 0 to fcafa
  |          perform rw-fcommaap
  |          move cmmlp-mail-type-o to immlp-mail-type(4)
  |                                    immlp-mail-type(5)
  |          string "BLOCAGE FACTURATION DE LA COMMANDE " fccle-cdesup
  |            delimited by size into immlp-mail-data (4)
  |          move "ACTION MANUELLE REQUISE" 
  |                                  to immlp-mail-data (5)
  |
  |   * Ecriture d'une trace
  |          move space to wmmtr-trac
  |          move "C"   to immtr-trac-type
  |          string fccle-cdesup delimited by size into immtr-trac-num
  |          move "M"   to immtr-trac-action
  |          move wnom-prog to immtr-trac-prog
  |          string "Ano. trt batch facturation de la cde " 
  |                 " -> blocage facturation ; action manuelle requise"
  |                  delimited by size into immtr-trac-commentaire
999999*  display "appel de la trace " immtr-trac-commentaire
999999*  display " cde " immtr-trac-num " " immtr-trac-action 
999999*  display " pgm " immtr-trac-prog
  |          call 'mmtr-trac1' using mmtr-trac adl-art
  |
  |        end-if
  |   
  |        perform env-mail
  |        exit section 
DD0820   end-if

DD0351   perform env-mail
         move wnom-prog to immaf-vali-pgm
         move "V=Validation" to immaf-vali-act
         move "V" to wmmaf-vali-trt
         move "B" to immaf-vali-pos
         move wtrt to wmmaf-vali-trt.
         call 'mmaf-vali1' using mmaf-vali adl-art.
         .
      * decrementation numero de facture
       subnum section.
           IF WPROCON = "CONDITIONNEL"
              SUBTRACT 1 FROM WNFACA
           ELSE
              if mmdt-societe = 'OYOBRICO'
                 if fbcfa = 5 or = 6 or = 7
                    subtract 1 from WNAVOI
                 else
                    subtract 1 from WNFANA
                 end-if
              else
                 subtract 1 from WNFANA
              end-if
           END-IF
           .

DDE153* ecriture entete commande si regroupee
       entete-cde section.
DD0122     perform saut34
           write ligne before 1
DD0316     string '    Notre Reference : ' fccle-cdesup
                  '  Representant : '
            fcrep '  Livre A : ' walliv '  ' wad6 ' '
             delimited size into ligne
           write ligne before 1
           add 2 to wcptr
           move spaces to ligne
DD0221*    string '    Votre Reference : ' wref(2:8)
DD0221     string '    Votre Reference : ' wref
                  '                                     '
                                           wcodp7 ' ' wbd7
             delimited size into ligne
           write ligne before 1
           add 1 to wcptr
           move spaces to ligne
           .
DDE153* edition et creation pied de facture
       pied section.

M0297 *    IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T220.
  |        IF FBTAX NOT = 1 AND FBTAX NOT = 2 and fbtax not = 5
M0297                                         GO TO T220.
           IF WPAR1 = ZERO AND  WPAR2 = ZERO GO TO T215.
           MOVE ZERO TO WHT1.
           IF WPAR1 = 1 MOVE FBHT1F TO FBBF1F
                        ADD FBBF1F TO WHT1.
           IF WPAR2 = 1 MOVE FBHT2F TO FBBF2F
                        ADD FBBF2F TO WHT1.
M0297 *    IF FBTAX = 1 MULTIPLY WTAUPA1 BY WHT1 
  |   *      ELSE       MULTIPLY WTAUPA2 BY WHT1.
  |   *    DIVIDE 100 INTO WHT1 ROUNDED.
  |   *    IF FBTAX = 1 MULTIPLY WTAUPA1 BY FBBF1F GIVING WCAL
  |   *      ELSE       MULTIPLY WTAUPA2 BY FBBF1F GIVING WCAL.
  |        IF FBTAX = 1 or fbtax = 5 MULTIPLY WTAUPA1 BY WHT1
  |          else                    MULTIPLY WTAUPA2 BY WHT1.
  |        DIVIDE 100 INTO WHT1 ROUNDED.
  |        IF FBTAX = 1 or fbtax = 5
  |                     MULTIPLY WTAUPA1 BY FBBF1F GIVING WCAL
M0297        else       MULTIPLY WTAUPA2 BY FBBF1F GIVING WCAL.
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
M0297 *    IF FBTAX = 1 MULTIPLY FBHT1F BY WTVA1 (I) GIVING WCAL
  |   *      ELSE       MULTIPLY FBHT1F BY WTVA2 (I) GIVING WCAL.
  |        IF FBTAX = 1 or fbtax = 5
  |                     MULTIPLY FBHT1F BY WTVA1 (I) GIVING WCAL
M0297        else       MULTIPLY FBHT1F BY WTVA2 (I) GIVING WCAL.
           DIVIDE 100 INTO WCAL   ROUNDED.
           MOVE WCAL TO FBTX1F.
       T215A.
           IF WTV2 = ZERO GO TO T220.
           IF WTV2 = 9 MOVE 5    TO I
             ELSE           MOVE WTV2  TO I.
M0297 *    IF FBTAX = 1 MULTIPLY FBHT2F BY WTVA1 (I) GIVING WCAL
  |   *      ELSE       MULTIPLY FBHT2F BY WTVA2 (I) GIVING WCAL.
  |        IF FBTAX = 1 or fbtax = 5
  |                     MULTIPLY FBHT2F BY WTVA1 (I) GIVING WCAL
M0297        else       MULTIPLY FBHT2F BY WTVA2 (I) GIVING WCAL.
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
DD0438     call 'mmca-eche1' using mmca-eche  mmaf-vrep adl-art
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
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
M0297 *    IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T330B.
  |        IF FBTAX NOT = 1 AND FBTAX NOT = 2 
  |          and fbtax not = 5 
  |          GO TO T330B
M0297      end-if.
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
M0297 *    IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T340B.
  |        IF FBTAX NOT = 1 AND FBTAX NOT = 2 
  |          and fbtax not = 5 
  |          GO TO T340B
M0297      end-if.
           IF WPAR1 = ZERO AND WPAR2 = ZERO GO TO T340A.
           MOVE SPACE TO LIGNE.
           WRITE LIGNE BEFORE 2.
           MOVE "BASE HORS-TAXES AVANT TAXE PARAFISCALE" TO LIBCA13.
           ADD FBBF1F FBBF2F GIVING LMT.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE "TAXE   PARAFISCALE   DE" TO LIBCA12.
M0297 *    IF FBTAX = 1 MOVE WTAUPA1 TO LTAUX
M0297      IF FBTAX = 1 or fbtax = 5 MOVE WTAUPA1 TO LTAUX
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
M0297 *    IF FBTAX = 1 MOVE WTVA1 (WTV1) TO LTAUX
M0297      IF FBTAX = 1 or fbtax = 5 MOVE WTVA1 (WTV1) TO LTAUX
             ELSE       MOVE WTVA2 ( WTV1) TO LTAUX.
           MOVE "%" TO LPOURC.
           MOVE FBHT1F TO LMT.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
           MOVE "BASE HORS-TAXES AVANT TVA" TO LIBCA12.
M0297 *    IF FBTAX = 1 MOVE WTVA1 (WTV2) TO LTAUX
M0297      IF FBTAX = 1 or fbtax = 5 MOVE WTVA1 (WTV2) TO LTAUX
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
GPICMT* ajout commentaire pour LCL
           if wrdi = 'LCL'
              if mmdt-societe = "GPI"
                 perform comment-lcl
              end-if
           end-if
GPICMT* ajout commentaire pour BBJ
           if wrdi = 'BBJ'
              perform comment-bbj
           end-if

GPICMT* ajout commentaire fin de facture
           perform finleau
      **** AJOUT DU TEXTE FIN DE FACTURE : SEPTEMBRE 1993     ****
           compute wnbl = 60 - linage-counter of etat.
           write ligne before wnbl.
      *    WRITE LIGNE BEFORE L60.
           MOVE FBNCO TO LNBCOL.
           MOVE FBPBR TO LPDSTO.
           MOVE FBNMF TO LNETMA.
DD0351* pour COFAQ faire apparaitre la ','
ELGU  *    if wrdi = 'COF'
ELGU  *       ADD FBHT1F FBHT2F GIVING LTHT-cofaq
ELGU  *    else
              ADD FBHT1F FBHT2F GIVING LTHT
ELGU  *    end-if
           IF FBFRA NOT = 2 MOVE FBPOF TO LPORTT.
M0297 *    IF FBTAX NOT = 1 AND FBTAX NOT = 2 GO TO T350B.
  |        IF FBTAX NOT = 1 AND FBTAX NOT = 2 
M0297        and fbtax not = 5 GO TO T350B.
           IF WTV1 = ZERO GO TO T350A.
M0297 *    IF FBTAX = 1 MOVE WTVA1 (WTV1) TO LTAU1 FBTT1
M0297      IF FBTAX = 1 or fbtax = 5 MOVE WTVA1 (WTV1) TO LTAU1 FBTT1
             ELSE       MOVE WTVA2 (WTV1) TO LTAU1 FBTT1.
DD0351* pour COFAQ faire apparaitre la ','
ELGU  *    if wrdi = 'COF'
ELGU  *       MOVE FBTX1F TO LMTTVA1-cofaq
ELGU  *    else
              MOVE FBTX1F TO LMTTVA1
ELGU  *    end-if
           .
       T350A.
           IF WTV2 = ZERO GO TO T350B.
M0297 *    IF FBTAX = 1 MOVE WTVA1 (WTV2) TO LTAU2 FBTT2
M0297      IF FBTAX = 1 or fbtax = 5 MOVE WTVA1 (WTV2) TO LTAU2 FBTT2
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
  -        move spaces to lcondx
DD0387     if FBLE = zero
  -           string fbjou ' Jrs Net'
  -               delimited size into lcondx
  -        else
              if FBLE = 30
DD0351           string fbjou " J. FIN MOIS"
  -               delimited size into lcondx
              else
DD0438           string fbjou " J.FIN MOIS LE " FBLE
  -               delimited size into lcondx
              end-if
DD0351*       MOVE FBJOU      TO LRJO
DD0351*       MOVE FBLE       TO LRLE
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
      * DDE089 on edite la valeur en franc suivant code wclcvfrf
      * avant nlle fiche client le code tester etair '0' maintenant 'N'
           if wclcvfrf = "N" or = '0'   go to t370-a.

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
           MOVE SPACE TO LIGNE.
      *----> M1198 (D)
      *    WRITE LIGNE BEFORE 3.
DD9999*    if fcdev = 00 or fcdev = 50 or fcdev = 62
DD9999     if w-dev = 00 or w-dev = 50 or w-dev = 62
      * DDE089 la valeur en franc a ete edite suivant code wclcvfrf
DD9999*       if fcdev = 62
DD9999        if w-dev = 62
                 if wclcvfrf = "N" or = '0'
                    write ligne before 3
                 else
                    write ligne before 2
                 end-if
              else
                 write ligne before 2
              end-if
           else
               write ligne before 3
           END-IF.
      *----> M1198 (F)
           MOVE "*  BOITES DETAILLEES" TO LIBCA12.
           WRITE LIGNE BEFORE 1.
           MOVE SPACE TO LIGNE.
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
GPICMT*
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
                if mmdt-langue = "FR"
                 string '* ATTENTION !!! LA FACTURE : ' fbcle-cdesup
                        ' EST DIFFERENTE DE SON PIED'
                        delimited size into immaf-vali-tit
                else
                 string '* WARNING !!! THE INVOIC : ' fbcle-cdesup
                        ' IS DIFFERENT OF FEET'
                        delimited size into immaf-vali-tit
                end-if
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
                 move zero to wcptr
                 move spaces to woridup
                 perform titre thru ftitre
                 move all '*' to ligne
                 write ligne before 1
DD0316           string '*     ATTENTION !!! LA FACTURE : ' fbcle-cdesup
                        ' EST DIFFERENTE DE SON PIEDS DE FFACTURE'
                        delimited size into ligne
                 write ligne before 1
                 string '*      VOUS DEVEZ DECHIRER CETTE FACTURE'
                        delimited size into ligne
DD0122           perform identifiant
                 write ligne before page
DDE153           go to t419
DDE153           go to pied-f
              end-if
           END-IF

           IF icglp-fact-e1red not = 'R'
DD0362        and icglp-fact-arc = spaces
ELGU17        and wtest1 = zero
GPICMT* pour la slovaquie on recalcul le taux de chnage montant devise/montant SK
DD0351        if mmdt-societe = 'SLOVAQ'
  -              divide fbnpd by fbnpf giving fbtch
DD0351        end-if
GPICMT* pour PLasto on met la facture a envoyer par l'imprimeur si:
GPICMT*       facture normal avec destockage 
GPICMT*       TTC different de zero
GPICMT*       facture non dematerialisee
GPICMT*       cde non openbook                  
DD0465        if (mmdt-societe = 'PLASTO'
DD0516           or mmdt-societe = 'GPI'
DD0351           or mmdt-societe = 'DINAC')
  -              if FBCFA = 0
  -                 and FBNPF not = zero
  -                 and wfcfdem not = '1'
DD0516              and witc not = 1
DD0351              and wfcopenbook = 0
  -                 move 1 to FBIMPRIM
                 else
                    if (witc = 1 or FBCFA not = 0) and FBNPF not = zero
                       move 2 to FBIMPRIM
                    end-if
                 end-if
DD0465        end-if
              perform w-ffacture
DD9999* anes 16/09/14 MaJ des compteurs FACTURE000 de parbatch
              if file-status = zero
                perform maj-parbatch
              end-if
              if file-status not = zero
                if mmdt-langue = "FR"
DD0316           string 'Cde: ' fccle-cdesup
DD0316                  ' Ecriture facture impos.: ' fbcle-cdesup
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                else
DD0316           string 'Order: ' fccle-cdesup
DD0316                  ' Writing invoic impossible.: ' fbcle-cdesup
                     ' (' file-status ')'
                     delimited size into immaf-vali-tit
                end-if
                 move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
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
GPICMT* GPIWARNING pour les factures avec code geo 0 on peut par exemple pour les tansporteur (marchandises abimees)
GPICMT*    faire une facture exoneree et utiliser le code 4 ==> prevoir gestion de ce cas
GPICMT*    gpi met un code taxe 3 au lieu de 4 et ca passe mais on ne devrait pas utiliser ce code 3 qui est fait
GPICMT*    pour de la suspension de taxe avec un montant ht affacte
DD0438     if mmdt-societe = 'SLOVAQ'
DD0438       if CPPAYS = 0 and CPTAXE = 4
  -           move 3 to CPTAXE
  -          end-if
  -          if CPPAYS = 1 and CPTAXE = 1
  -           move 0 to cppays
  -          end-if
DD0438     end-if
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
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
              end-if
           END-IF
           .
       pied-f.
GPICMT* si edition laser
DD0337     if mmdt-societe = "SLOVAQ"
DD0301        or mmdt-societe = "ERELS"
DD0465        or mmdt-societe = "PLASTO"
DD0474        or mmdt-societe = "OYOBRICO"
ELGU09        or mmdt-societe = "CHINE"
DD0516        or mmdt-societe = "GPI"
DD0351        or mmdt-societe = "DINAC"
DD0362        or icglp-fact-arc not = spaces
DD0362        or icglp-fact-pdf = "O"

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
              move spaces to immlp-hfac-reedit
              if icglp-fact-e1red = 'R'
                 move 'X' to immlp-hfac-reedit
              end-if
DD0362        move wor-ffacture to fwor-ffacture2
              move 3 to immlp-hfac-direct
DD0351        move icglp-fact-reliq to immlp-hfac-reliq
              call "mmlp-hfac1" using mmlp-hfac adl-art
              if ommlp-hfac-rtn not = cmmdt-envi-rtn-ok
                 move ommlp-hfac-liberr to immaf-vali-tit
                 move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
                 perform erreur
                 go to fin1
              end-if
           end-if
           .
       pied-f-edit.
           exit.

DDE153* initialisation zone facture
       ini-fac section.
DD0820     move "1" to wflag-trt-cde
DD0351     move 0 to w-flag-edit
           move spaces to w-regroup w-creat
DD9999     move zero to wtest1

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
           move fcrep      to w-repr
DD0394     move fcfeo      to w-type
           move fcfdem     to wfcfdem
           move fcopenbook to wfcopenbook
           .

DDE153* initialisation des zones par cde a traiter
       ini-cde section.
DD0350     move zero to whtmb whtmb9(1) whtmb9(2) wcpt7a
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
           move
             ' ______________________________________________________'
             to ligne
           compute wnbl = 70 - linage-counter of etat
           write ligne before wnbl
           move
             ' 379 622 160 RCS BOURG/SIRET 379 622 160 00016/APE 252H'
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
DD0122*       perform identifiant
              write ligne before page
              PERFORM TITRE THRU FTITRE
           END-IF
           .

DD0122* GPICMT saut de page > 31 lignes
       saut31 section.
           IF WCPTR > 31
DD0122*       perform identifiant
              write ligne before page
              PERFORM TITRE THRU FTITRE
           END-IF
           .

DD0314* GPICMT saut de page > 29 lignes
       saut29 section.
DD0337*    IF WCPTR > 29
DD0337*    IF WCPTR > 35
DD0358     IF WCPTR > ( 35 - wcptr-tot)
DD0122*       perform identifiant
              write ligne before page
              PERFORM TITRE THRU FTITRE
           END-IF
           .

GPICMT* erreur coherence des codes
DD9999 erreur-codes section.
          if mmdt-langue = "FR"
DD0316     string "CODES  Commande " fccle-cdesup
                  " incoherents a verifier"
                    delimited size into immaf-vali-tit
          else
DD0316     string "CODES  Order " fccle-cdesup
                  " wrong to prove"
                    delimited size into immaf-vali-tit
          end-if
           move '2' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
           perform erreur
           .

DD0221* recherche reference commande dans fichier commentaire
       rech-refcde section.
DD0316     move fccle-cdesup  to pfjcle1
           perform r-fcommac1.
           if file-status = zero
      * reference commande BRICORAMA
              move pfjli2 to wlentb
              if wlab = "COMMANDE CLIENT:  "
                 move wlbb to wncde
              end-if
           end-if
           .

GPICMT* maj facturation commandes alloties filles
DD0298 maj-filles section.
DD0316     move fccle-cdesup to icgcd-fill-numcdex
DD0316*    move fcnum to icgcd-fill-numcde
DD0316*    move fcnin to icgcd-fill-numind
           move ccgcd-fill-e1trt-fac to wcgcd-fill-e1trt
           move fcfac to icgcd-fill-fac
DD0316     move fcnfa-cdesup to icgcd-fill-nfac
           call "cgcd-fill1" using cgcd-fill adl-art
           if ocgcd-fill-rtn not = cmmdt-envi-rtn-ok
              move ocgcd-fill-liberr to immaf-vali-tit
              move '3' to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
              perform erreur
           end-if
           .

GPICMT* finleau sert a rajouter un commentaire en fin de facture
DD0314 finleau section.
DD0314*    if fbtve(1:1) = 9
GPICMT* recherche si infos banque a editer
DD0358        perform cpt-banque
GPICMT* pour favotex on edite pas le commentaire d'escompte
DD0301*       if mmdt-societe not = "GPI"
DD0301        if (mmdt-societe not = "GPI" and not = "ERELS")
  -              and wcptr-banque = zero
  -              go to finleau-fin
DD0358        end-if
DD0358        move wcptr-banque to wcptr-tot
GPICMT* ajout lignes pour escompte et conditions de ventes
  -           if mmdt-societe = "GPI"
DD0301            or = "ERELS"
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
GPICMT* on n'edite plus la phrase sur l'escompte pour toutes les societes
      *       if mmdt-societe not = "GPI"
DD0301*            and not = "ERELS"
                 go to finleau-fin
      *       end-if
DD0351* Ajout infos ventes
              string "Toutes nos ventes sont effectuees selon nos "
                "conditions generales de ventes figurant au verso "
                   delimited by size into ligne
              write ligne before 1
              move spaces to ligne
              write ligne before 1
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
DD0337*       STRING '             SERVICE COMMERCIAL ET ADV:'
DD0337*           '(Mme MONNET) TEL 04.74.12.05.10 - FAX 04.74.12.05.12'
DD0337*          DELIMITED SIZE INTO LIGNE
DD0337*       WRITE LIGNE BEFORE 1
DD0337*       MOVE SPACES TO LIGNE
DD0337*       STRING '                          EMAIL'
DD0337*              ' v.monnet@gpi.fr'
DD0337*D0337*    DELIMITED SIZE INTO LIGNE
DD0337*       WRITE LIGNE BEFORE 2
DD0337*       MOVE SPACES TO LIGNE
DD0337*       STRING '             SERVICE FACTURATION (Martine BREVET)'
DD0337*              ' TEL 04.74.12.05.53 - FAX 04.74.12.05.12'
DD0337*          DELIMITED SIZE INTO LIGNE
DD0337*       WRITE LIGNE BEFORE 1
DD0337*       MOVE SPACES TO LIGNE
DD0337*       string '                          EMAIL m.brevet@gpi.fr'
DD0337*          DELIMITED SIZE INTO LIGNE
DD0337*       WRITE LIGNE BEFORE 2
DD0337*       MOVE SPACES TO LIGNE
      *    end-if
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
                                   ' BIC: ' ommpa-soci-bic1
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq2 not = spaces
            string ommpa-soci-dom2 ' IBAN: ' ommpa-soci-iban2
                                   ' BIC: ' ommpa-soci-bic2
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq3 not = spaces
            string ommpa-soci-dom3 ' IBAN: ' ommpa-soci-iban3
                                   ' BIC: ' ommpa-soci-bic3
            delimited size into ligne
            write ligne before 1
            move spaces to ligne
            add 1 to wcptr
         end-if
         if ommpa-soci-banq4 not = spaces
            string ommpa-soci-dom4 ' IBAN: ' ommpa-soci-iban4
                                   ' BIC: ' ommpa-soci-bic4
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
                                     ' BIC: ' ommpa-soci-bic-be
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
                                     ' BIC: ' ommpa-soci-bic-de
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
GPICMT* pour favotex et erels on edite toujours les infos banque
DD0380*  if ommpa-pays-cgeo = zero
DD0380*  if (ommpa-pays-cgeo = zero and mmdt-societe not = "ERELS"      *GPICMT
DD0380*                             and mmdt-societe not = "FAVOTEX")   *GPICMT
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
            add 1 to wcptr-banque
         end-if
         if ommpa-soci-banq2 not = spaces
            add 1 to wcptr-banque
         end-if
         if ommpa-soci-banq3 not = spaces
            add 1 to wcptr-banque
         end-if
         if ommpa-soci-banq4 not = spaces
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
DD0351*    move "anes micn"  to immlp-mail-dest
DD0820     move "vamo nobo cafo geca door anes"  to immlp-mail-dest
           move wnom-prog to immlp-mail-pgm
           call 'mmlp-mail1' using mmlp-mail adl-art
            .

GPICMT* edition commentaire location gerance
DD0425 edit-com-locger section.
           move spaces to ligne
           string "             **** "
                  "STE GPI LOCATAIRE GERANTE DE STE ERELS          "
                  "                 ****"
                   delimited by size into ligne
           write ligne before 1
           move spaces to ligne
           string "             **** "
                  "MERCI D'ETABLIR VOTRE REGLEMENT A GPI LOCATAIRE "
                  " GERANT DE ERELS ****"
                   delimited by size into ligne
           write ligne before 2
           move spaces to ligne
           add 3 to wcptr
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


GPICMT* commentaire LCL
       comment-lcl section.
         add 2 to wcptr
         perform saut34
         move spaces to ligne
      *GPICMT GPIWARNING: changer aussi ce numero dans cglp-factr et factc200  
DD9999*  string 'N.Accord : 2013-02808-42'
DD9999*         ' du 01/03/13 au 31/12/13'
DD9999*  string 'N.Accord : 2019-02808-42-N'
DD9999*         ' du 01/01/13 au 31/12/19'
DD9999   string 'N.Accord : 2020-02808-42-N'
DD9999          ' du 01/01/20 au 31/12/20'
               delimited size into ligne
         write ligne before 1
         move spaces to ligne
         string "La totalite des conditions commerciales de l'operation"
                " de vente sont deduites sur facture"
               delimited size into ligne
         write ligne before 1
         move spaces to ligne
         .

GPICMT* commentaire BBJ
       comment-bbj section.
         add 2 to wcptr
         perform saut34
DD9999   move spaces to ligne
DD9999*  string 'N.ACCORD:2013-02808-46 du 01/03/13 au 31/12/13'
DD9999*  string 'N.ACCORD:2019-02808-46-N du 01/01/19 au 31/12/19'
DD9999   string 'N.ACCORD:2020-02808-46-N du 01/01/20 au 31/12/20'
               delimited size into ligne
         write ligne before 1
         move spaces to ligne
         string "La totalite des conditions commerciales de l'operation"
                " de vente sont deduites sur facture"
               delimited size into ligne
         write ligne before 1
         move spaces to ligne
         .

GPICMT* recherche code geo du livrea et facturea
ELGU17 rech-geo section.
         move zero to wgeo-livrea
         move "L"      to immpa-pays-fic
         if ocmcd-gest-livrea-niveau not = 'T'
            move ocmcd-gest-livrea-pays to wmmpa-pays-pays
         else
            move ocmcd-gest-cdepar-pays to wmmpa-pays-pays
         end-if
999999  if mmdt-logn = "DINA"
999999  display "trace cglp-fact1 niv " ocmcd-gest-livrea-niveau "'"
999999  display "trace cglp-fact1 pays " wmmpa-pays-pays "'"            
        end-if
         call 'mmpa-pays1' using mmpa-pays adl-art
         move ommpa-pays-cgeo to wgeo-livrea
999999  if mmdt-logn = "DINA"
999999  display "trace cglp-fact1 wgeo-livrea " wgeo-livrea "'"
        end-if
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
      *         string fccle-cdesup ' Code taxe/Pays invalide'
                string fccle-cdesup ' ' ommpa-vtax-liberr
                       delimited size into immaf-vali-tit
             else
                string fccle-cdesup 'Tax/Country Code not correct' 
                       delimited size into immaf-vali-tit
             end-if
           end-if
           .

DD9999* anes 16/09/2014 MaJ des compteurs FACTURE000 de parbatch
       maj-parbatch section. 
         MOVE "FACTURE000" TO PHCLE
         perform r-parbatch
         if file-status not = zero
           string 'Relecture 2 PARBATCH impossible: ' PHCLE
                   ' (' file-status ')'
              delimited size into immaf-vali-tit
            move "3"           to ocglp-fact-rtn
            go to fin
         end-if

         MOVE WNFANA TO PHANOC (1)
         MOVE WNFACA TO PHANOC (2)
         MOVE WNAVOI TO PHANOC (3)
         perform rw-parbatch
         if file-status not = zero
            string 'Reecriture 2 PARBATCH impossible: ' PHCLE
                  ' (' file-status ')'
              delimited size into immaf-vali-tit
            move "3"           to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
            perform erreur
            go to fin
         end-if

         perform r-parbatch
         if file-status not = zero
           string 'Relecture 3 PARBATCH impossible: ' PHCLE
                   ' (' file-status ')'
              delimited size into immaf-vali-tit
            move "3"           to ocglp-fact-rtn
999999   display "cglpfact liberr " immaf-vali-tit   
999999   display "cglpfact ERR " ocglp-fact-liberr " " immaf-vali-tit
            perform erreur
            go to fin
         end-if
         MOVE PHANOC (1) TO WNFANA
         MOVE PHANOC (2) TO WNFACA
         MOVE PHANOC (3) TO WNAVOI
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
      *----> M1098 (D)
           copy "../copy/pro-fartusap".
      *----> M1098 (F)

      *----> M1199 (D)
           copy "../copy/pro-languear".
DDE089     copy "../copy/pro-fartusac".
DD9999     copy "../copy/pro-paramcpt".

