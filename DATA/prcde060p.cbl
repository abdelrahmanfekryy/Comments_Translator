      *DD0814 12/01/18 anes Gestion du rayon => on va le chercher dans le refercli referencement client
      *DD0812 24/08/17 anes Traitement LEROY MERLIN
      *DD9999 15/06/17 anes Blocage systematique des commandes TSM
      *DD2001 24/08/16 anes Remplacement fcoadcli par fjoadcli bloc adresse jour
      *DD0351 23/06/16 anes Ajout du libelle correspondant au type erreur    
      *DD0351 25/05/16 anes L'erreur 60 n'est plus une erreur --> "A VALIDER"
      *DD0351 18/05/16 anes Correction d'appel gestion d'erreur pour werr = 26
      *                     amelioration des messages d'information mgcd-vean1
      *DD0351 22/12/15 micn pas d'arret de l'integration si fichier d'erreur plein
      *DD0351 02/12/15 micn ne pas accepter les qts decimales pour GPI/PLASTO
      *DD9999 20/10/15 door Conversion du log pour web
      *DD0800 11/09/14 anes MAJ numero de cde si cde intersite dans foucmagi
      *DD0351 24/02/14 micn modif libelle de la trace                    
      *DD0777 12/02/14 anes chargt du client final dans foadcli
      *DD0775 12/02/14 anes ajout gestion du code surveillance par cgcl-csur1
      *DD0774 17/12/13 door Renumerotation client en dur suite a fusion
      *DD0516 19/09/11 elgu initialisation code commande piege a partir du client facture
      *DD0448 08/07/11 elgu controle validite code taxe
      *DD0459 30/05/11 micn on bloque toutes les cdes EDI pour detecter les infocoms par SCOM
      *                elgu ajout 3eme deimale pour controle prix tarif et client
      *DD0221 18/03/11 elgu ne plus charger la reference commnade client dans fcomjoc1
      *DD0351 16/03/11 elgu recuperer le libeller erreur de cgca-mtht1
      *DD0221 13/01/11 anes allongement reference commande client
      *DD0351 12/01/11 elgu edition numero de commande deja cree dans SCOM/SLIV
      *DD0351 20/12/10 elgu je passe la reference commande client a 12 caractere dans fcommac1
      *DD0351 27/09/10 micn on met le code maj annul a 1 pour annul cde ds dataware (cela va mettre a jour avoircli mais pas genant)
      *DD0465 20/08/10 elgu pour scalandes et plasto impose le livre a plateforme
      *                     on prend le numero de ligne de la commande edi
      *DD0459 10/06/10 micn gestion des fictifs
      *DD0466 04/06/10 gich Appel choix EAN14 automatique
      *DD0351 11/05/10 elgu creer l'entete allotie meme si erreur sur filiere des filles 
      *                     afin de pouvoir supprimer la mere pour relancer l'integration
      *DD0351 06/04/10 elgu pour dinac bloquer toutes les commandes castorama
      *DD0298 06/01/10 elgu bloquer toutes les commandes alloties/mere et filles), doivent etre validees
      *DD0298 09/11/09 elgu modif gestion ttfacmst pour alloti at suppression 1er saut de page
      *DD0351 12/10/10 elgu controle commande avec meme ref commande client/client et delai
      *                     et correction traitement commande sans ligne, parfois ne traitait pas la commande suivante meme si ok
      *DD0351 03/09/09 elgu refc cde client plus de 8 car : prendre les 8 derniers
      *DD0425 23/04/09 elgu location gerance seulement pour GPI
      *DD0424 18/03/09 micn suppression de la zone depot livre de la fiche client
      *DD0425 10/03/09 elgu trt location gerance erels
      *DD0400 29/12/08 micn Saisie de commande pour client = etat valide
      *DD0420 25/11/08 elgu agrandissement ref commande client et traitement avec code ean client au lieu du code interne
      *DD0412 29/10/08 elgu controle pour dinac qte multiple de pcb pour article deconditionnable
      *DD0351 29/01/08 elgu controle code reglement
      *DD0351 18/12/07 elgu sur demande mabr et tarif non renseigne (nlle gamme)
      *                     si prix a zero mettre 1 dans topx pour pouvoir
      *                     faire un recalcul du prix
      *DD0351 13/04/07 elgu ne pas creer la commande si client bloque (clcsu = 9)
      *DD0359 13/02/07 elgu traitement ref commade client > 8 car
      *DD0279 18/12/06 elgu remplacer apcoj par wor-fcomjoap
      *                     ajout code pays et nouveau bureau distributeur
      *DD9999 07/09/06 elgu
      *DD0326 12/06/06 elgu
      *DD9999 07/06/06 elgu correction demande MBR (test sur clniveau du client cde au lieu
      *                     du client livre
      *DD9999 19/05/06 elgu suite demande MBR, gerer le livrea passe en EDI comme un magasin
      *                     ==> centrale passe la commande pour livraison magasin
      *DD0316 29/04/06 door alongement no de cde + controle PCB client
      *DD0316 22/04/06 elgu  nlle wor-seqcomc1
      *DD0314 31/03/06 elgu ajout commentaire LEAU
      *DD0298 22/11/05 elgu traitement commande allotie
      *DD0282 27/06/05 anes Remplacement des "valide" par "etat"
      *DD9999 22/06/05 elgu prendre le code reglement chez le reglepar et
      *V10162 18/01/05 elgu ramener la 1ere refe valide si plusieurs ref
      *                     pour le meme gencod
      *DD0219 29/11/04 elgu
      *DD9999 23/11/04 elgu
      *DD0177 25/08/04 elgu ajout livrea
      *DD0076 20/08/04 elgu ajout libelle contre marque ds fcomjoc1
      *DD0180 12/08/04 elgu
      *DD0002 06/08/04 elgu active bloc adresse fiche client
      *DD0162 20/04/04 elgu
      *DD9999 09/03/04 elgu correction utilisation file-status apres appel
      *                     de mmtr-trac1 qui change le file-status avant appel
      *                     cas multi ref pour un  meme gencod
      *DD9999 31/07/03 elgu correction initialyze fcomjoap + on lit l'article
      *       donnee par la fonction de recherche des doublons sinon on prend
      *       le 1er cip trouve cas gencod 544040
      *DD9999 06/06/03 elgu trt code tarif 9
      *DD9999 06/05/03 elgu initialisation enregistrement
      *DD0066 17/04/03 elgu
      *DDE069 14/02/03 elgu gestion validite article
      *DDE339 30/01/03 elgu
       PROGRAM-ID. PRCDE060.
      *
      * GPICMT *** CREATION DES COMMANDES PASSEES PAR ALLEGRO A PARTIR
      * GPICMT ***  DES 2 FICH.sequent. TRIES PAR CLIENT/CDE (ENTETE-LIGNES)
      *
      *DDE191 ajout marche par rapport au secteur client
      *DDE125 nouvelle wor fcommac2 et fcomjoc2
      *DDE171 Verif PCB ligne de cde par rapport a celui recu => erreur listing
      *       verif total HT fin de cde et celui recu => erreur listing
      *       Ajout controle de validite article et sous reference
      *DDE153/DDE154
      *DDE155 mettre l'adresse du livre dans l'entete de commande
      *DDE089  modif appel devise avec controle de validite
      *DDE069
      *M0497a: mettre "AUTO" dans la zone qui saisit
      *        mettre prix de base client dans fjpcl
      *M0299b: ajout creation automatique de l'escompte
      *DDE045: modification recherche tarif
      *DDE084: calcul delai de livraiso par une fonction en tenant compte des
      *        jours ouvres
      *M0299 : si date de livraison = zero ==> mettre date cde + 1 (jour ouvre)
      *DDE103 remplacement zone clcav par clsecteur et clcma par clfamstat
      *M05040j: correction trt client avec coef K
      *M020301: correction calcul en euro
      *DDE086: prendre ancien prix pour client tarif 100 et prix actuel
      *        pour les autres
      *DDE079: suite creer la ligne article avec la 1er trouvee et l'editer
      *DDE079: controle plusieurs references pour meme gencod
      *DDE049
      *DDE046
      *DDE045
      *DDE053 : bloquer la saisie d'un article supprime (faedi=9)
      *         utilisation du code erreur 24 utilise auparavant pour le control
      *         gde classe
      *DDE032: passer de 5 a 18 enreg commentaires posibles pour un article
      *        prendre les commentaires sur le client 999999 si aucun trouve
      *DDE026: integration des commentaires dans fcomjoc6 + tracetel
      *DDE025: ajout trt multiclasse
      *M0600a: ajout reference cde trouve ds les commentaires (gere ds EDI)
      *        seulement pour BRICORAMA (pb ref cde sur 10 et nous sur 8)
      *        ne plus traiter les commentaires car traitement special avec
      *        nouveaux fichiers
      *M0600 : ajout type de commande BRICORAMA "220"
      *M0400 : modif trt montant majoration en devise
      *DDE011: ajout controle devise tarif et devise client
      *M1299 : correction modif an 2000 cas > 2000
      *M0999 : prendre le code edition du prix sur BL ds la fiche du client
      *M0699 : modif chgt nom de zone fjnlg = fjlng (confusion code langue)
      *M0599 : door modif an 2000
      *M1298 : modif passage euro
      *M1198 : maj a jour code edition de facture "dematerialisee"
      *M0998 : maj recheche REFERCLI si lecture avec le livre trouve et gestion
      *        du prix non = 1 ou date > date cde ==> lecture centrale
      *M0898 : ajout recherche libelles articles sur filiation de la filiation
      *        du client
      *M0798 : modif recherche tarif, ajout fichiers REGROUPD CLIARTSP
      *M0598 : tenir compte du client livre (ex: LEROY NICE ET AUBAGNE pour
      *      : lieu de livraison a AIX
      *M0498 : maj codes de traitement ds ttfacmst
      *M1297 : creation TTFACMST pour dematerialisation de la facture
      *M0797  : pour code tarif 9 mettre code grande classe+99 ds n.tarif
      *M0697b : maj prix de base et % remise dans ligne article
      *         ajout type reference saisie (A=ART CAT, D=DEVIS ,I= NON REFEREN)
      *M0697a: modif traitement du code port
      *M0497 : si n.dossier de la fiche du client cde est # zero le mettre
      *        dans le regle par
      *M1296: prise du client livre a dans fclients si # zero
      * ajout nomenclature produit dans fcomjoc2
      * maj code taxe a 4 pour client export
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
                LINAGE IS 64  LINES.
       01  LIGNE                  PIC X(133).
       01  L1.
           02 LNCL                PIC 9(6) blank zero.
           02 FILLER              PIC X(3).
           02 SEP1                PIC X  value '|'.
           02 LRCC                PIC X(15).
           02 FILLER              PIC X(3).
           02 SEP2                PIC X  value '|'.
DD0326     02 LNUM                PIC 9(7) blank zero.
           02 cdeind              PIC X(1) value '0'.
           02 FILLER              PIC X(4).
           02 SEP3                PIC X  value '|'.
           02 LLJJ                PIC 99.
           02 LS1                 PIC X.
           02 LLMM                PIC 99.
           02 LS2                 PIC X.
           02 LLAA                PIC 99.
           02 FILLER              PIC X(3).
           02 SEP4                PIC X  value '|'.
           02 LART                PIC X(13).
           02 FILLER              PIC XX.
           02 SEP5                PIC X  value '|'.
           02 LLIB                PIC X(60).
           02 SEP6                PIC X  value '|'.
      *
       WORKING-STORAGE SECTION.
           copy "/usr/action/ADL/copy/wor-adl".
DD0814     copy '../copy/fgrc-lect.com'.                                *GPICMT
DD0298     copy "../copy/wor-gencoale".
DD0298     copy "../copy/wor-gencoall".
DD0298     copy "../copy/wor-gencoalc".
DD0298     copy "../copy/wor-cdesalle".
DD0298     copy "../copy/wor-cdesalll".
           copy "../copy/wor-gencoent".
           copy "../copy/wor-gencolig".
           copy "../copy/wor-errlig".
           copy "../copy/wor-errent".
           copy "../copy/wor-fcommaap-cdesup".
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
DD0800     copy '../copy/wor-foucmagi'.
DD0814     copy '../copy/wor-refercli'.

      *----> M1297 (D)
           copy '../copy/wor-guextmst'.
           copy '../copy/wor-ttfacmst-cdesup'.
           copy '../copy/wor-filieres'.
      *----> M1297 (F)

      *----> M0798 (D)
           copy '../copy/wor-cliartsp'.
           copy '../copy/wor-fcomjoc3'.
      *----> M0798 (F)

      *----> M0400 (D)
      * ajout copy parametres appel mmcd-majo1                          *GPICMT
           copy '../copy/mmcd-majo.com'.                                *GPICMT
      *----> M0400 (F)


      *----> DDE026 (D)
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

      *----> DDE026 (F)
DD0466     copy '../copy/mgcd-vean.com'.                                *GPICMT
DDE079     copy '../copy/mgca-arti.com'.                                *GPICMT
DDE045     copy '../copy/cgca-mtht.com'.                                *GPICMT
DDE045     copy '../copy/mmpa-devi.com'.                                *GPICMT
DDE045     copy '../copy/cgta-rech.com'.                                *GPICMT
DDE045     copy '../copy/cgre-arcl.com'.                                *GPICMT
DDE084     copy '../copy/mmca-date.com'.                                *GPICMT
DDE069     copy "../copy/mmti-date.com".                                *GPICMT
DDE069     copy "../copy/mmtr-trac.com".                                *GPICMT
DDE069     copy '../copy/mgre-mtfr.com'.                                *GPICMT
DDE069     copy '../copy/mmca-qtes.com'.                                *GPICMT
DDE171     copy '../copy/mmpa-etat.com'.                                *GPICMT
DDE191     copy '../copy/mmpa-sect.com'.                                *GPICMT
DDE339     copy '../copy/mmdt-lieu.com'.                                *GPICMT
DD0180     copy '../copy/cmcd-majc.com'.                                *GPICMT
DD0180     copy '../copy/cmcd-gest.com'.                                *GPICMT
DD0076     copy '../copy/fcomjoc8.com'.                                 *GPICMT
DD0298     copy '../copy/mmaf-vali.com'.                                *GPICMT
DD0298     copy '../copy/cmpa-tycd.com'.                                *GPICMT
DD0351     copy "../copy/mmpa-regl.com".                                *GPICMT
DD0412     copy '../copy/cmca-qpcb.com'.                                *GPICMT
DD0351     copy '../copy/cmcd-lect.com'.                                *GPICMT
DD0448     copy "../copy/mmpa-vtax.com".                                *GPICMT
DD0775     copy "../copy/cgcl-csur.com".                                *GPICMT
DD0800     copy '../copy/mmlp-mail.com'.                                *GPICMT
DD0800     copy '../copy/mmpa-mail.com'.                                *GPICMT

       01  wlabel-etat pic x(64) value space.
       01  var-name pic x(64).
       01  var-data pic x(64).
       01  TOUT.
DD0351* memo ref client entiere
           02 wi pic 99.
           02 wz pic 99.
           02 wrenccx.
              03 wrencc occurs 15 pic x.
DD0351* memo ref client construite
           02 wrenccx-new.
              03 wrencc-new occurs 8 pic x.
DD0420* memo numero client lu pour affectation de la commande
           02 wclient  pic 9(6).
DD0412* qte saisie avant controle deconditionnement
           02 wqte-ori pic z(8)9v,99.
DD0412* qte saisie apres controle deconditionnement
           02 wqte-arr pic z(8)9v,99.
           02 wpcb pic 9(6)v99.
           02 wpcbx redefines wpcb.
              03 wpcb-ent pic 9(6).
              03 wpcb-dec pic 9(2).
DD0298     02 wentree.
DD0298       03 wtrt pic x.
DD0177* memo livraison sur plateforme
           02 wplateforme pic x.
DD0298* memo transporteur habiruel
           02 wtrh                pic 9(2).
DDE155* memo adresse livraison pour maj entete
           02   wnom              pic x(26).
           02   wrso              pic x(26).
           02   wrue              pic x(26).
           02   wbpo              pic x(26).
           02   wccp              pic 9(5).
DD0279     02   wbdi              pic x(35).
DD0279     02   wpays             pic x(2).

      *----> DDE026 (D)
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
DD0425* memo type de client
           02 wtype            pic x.
      * reference commande client
           02 wrcl.
              03 filler        pic x(5).
DD0221*       03 wrefcli       pic x(8).
DD0221        03 wrefcli       pic x(15).
DD0359     02 wrefcdecli.
DD0420        03 wref8         pic x(8).
DD0420        03 wrefsuite     pic x(7).
      * enreg recap commandes par assistante commerciale
           02 wenrass.
      * nom assistante
             03 wassist        pic x(4).
             03 filler         pic x value "|".
      * reference commande du client
             03 wrecli         pic x(8).
             03 filler         pic x value "|".
      * numero de commande gpi
DD0326       03 wcdegpi        pic x(8).
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
DDE069       03 weanentc       pic 9(13).
             03 weanent        pic 9(13).
DD0420*      03 wcdeent        pic x(8).
DD0420       03 wcdeent        pic x(15).

      * cle  commentaire de cde pour test commentaires
           02 wclecom.
DDE069       03 weancomc       pic 9(13).
             03 weancom        pic 9(13).
DD0420*      03 wcdecom        pic x(8).
DD0420       03 wcdecom        pic x(15).
      *----> DDE026 (F)

      *----> DDE025 (D)
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
      *DDE046
                 04 wart3xx.
                   05 wart3     pic 9(7).
                 04 wqte3       pic 9(8)v99.
      * memo nbre de poste maxi de la table
           02 wind3             pic 999 value 500.
      * indice d'utilisation de la table.
           02 windt             pic 999.
      *----> DDE025 (F)

      *----> M1298 (D)
           02 wtba              pic x.
      ***************** pour l'arrondi sur la 3eme decimale
           02 t4                pic 9(5) value 00050.
           02 ta4 redefines t4.
             03 ar              pic 9v9(4).
           02 zeuro             pic 9(5)v9(6).
           02 wcale             pic 9(6)v9(4).
           02 wzca              pic 9(6)v99.
      *----> M1298 (F)

      *----> M1297 (D)
           02 wtrcd.
              03 wtj pic 99.
              03 wtm pic 99.
      *----> M1297 (F)
      ** CLIENT LIVRE - FRANCO - FILIATION - GDE CLASSE
           02   WLIV              PIC 9(6).
           02   WFRA              PIC 9.
           02   WMFR              PIC 9(6).
           02   WNAF              PIC 9.
           02   WNOF              PIC 9(6).

      *----> M0898 (D)
           02   wnofn             pic 9(6).
      *----> M0898 (F)

      *----> M0497 (D)
           02   wclpa             pic 9(6).
      *----> M0497 (F)
      *DDE049
           02   wtgcx.
             03 WTGC              PIC 9.
           02   WCTA              PIC 9.
      *
      *
DD0326     02   WNUM              PIC 9(7).
           02   wfoa              pic 9.
           02   WCD               PIC 9(6).
           02   WRCD REDEFINES WCD.
            03  WA                PIC 99.
            03  WM                PIC 99.
            03  WJ                PIC 99.
           02   WFIN              PIC 9.
DD0351*    02   WLIG              PIC 99.
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
DD0298** RANG EN COURS - ENTETE ET LIGNE pour commande allotie fille
  "        02   gencoale-key comp PIC 9(8).
  "        02   gencoalc-key comp PIC 9(8).
DD0298     02   gencoall-key comp PIC 9(8).
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
            03  WLAB              PIC X(18).                            *M0600a
            03  WLBB              pic x(12).                            *M0600a

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
      *----> M0797 (D)
           02   wntax redefines wnta.
            03  wnta1             pic 9.
            03  wnta2             pic 99.
      *----> M0797 (F)
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
      *----> M0598 (D)
           02   WGCD              PIC 9(13).
      *----> M0598 (F)
      ** QTE - MONT COMMANDE
           02   WQUAN             PIC 9(8)V99.
           02   WMONT             PIC 9(8)V99.
           02   WMONTV            PIC Z(7)9V,99.
           02   WRESU             PIC 9(8)V99.
           02   renhtcdev         PIC Z(7)9V,99.
      ** DEBUT AC
           02   WACCOJ.
DD0326      03  WFJNUM            PIC 9(7).
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
      *DDE046
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
      *DDE046
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
           02    wcllia  pic 9(6).
DD0002* client facture
           02    wclfacturea  pic 9(6).
DD0002* client regle
           02    wclreglepar  pic 9(6).

      *----> M0598 (D)
           02    wclcde  pic 9(6).
      *----> M0598 (F)
DD9999* numero client commande par du bloc adresse (fcoadcli
           02    w-alcde pic 9(6).

      *----> M0697a (D)
           02 wcalf             pic s9(8)v99.
           02 ztdv              pic 99v9(6).
      *----> M0697a (F)

      *----> M0798 (D)
           02 ztlig             pic 99.
           02 wcli              pic 9(6).
           02 wia               pic 999.
           02 wlib-comp.
              03 w-libelles.
                 04 w-lib occurs 54 pic x(30).                          *DDE032
           02 wt-lib-comp redefines wlib-comp occurs 18 pic x(90).      *DDE032
      *----> M0798 (F)
      *----> M0998 (D)
           02 wclref            pic x.
      *----> M0998 (F)
      *----> M0299 (D)
           02 zrendli        pic 9(6).
DDE084     02 filler redefines zrendli.
             03 zaa          pic 99.
             03 zmm          pic 99.
             03 zjj          pic 99.
      *----> M0299 (F)
DDE069     02 td-nolig          pic 9(4).
      *----> Escompte du client.
DDE069     02 td-clesc           pic 99v99.
DDE069     02  wnom-prog                PIC X(10) value 'prcde060'.
DDE069     02 td-top-prix        pic x.

DD0800     02 wlibelle-erreur    pic x(72).
DD0351* anes 18/05/16 amelioration des messages retour de mgcd-vean1
  |        02 wliberr.
  |          03 wliberr-nb-ref            pic 9.
  |          03 wliberr-wt-ref.
  |            04 wliberr-wp-reference occurs 7.
  |              05 wliberr-wp-ref        pic x(7).
  |              05 wliberr-wp-sref       pic x(2).
  |          03 wliberr-reference-retenue.
  |            04 wliberr-niv-retenue     pic x(2).
  |            04 wliberr-ref-retenue     pic x(7).
  |            04 wliberr-sref-retenue    pic x(2).
DD0351       03 wliberr-critere           pic 99.
DD0814     02 wrayon                      pic x(6).

DD0465* memo prix pour calcul difference
           02 wprxnet            pic 9(6)v,99.
           02 wprxcli            pic 9(6)v,99.
           02 wabsolu            pic 9(6)v999.
DD0298* copie gencoent pour creation gencoale
        copy "../copy/wor-gencoent.mod" replacing ==(pref)== by ==w==.
DD0298* copie gencolig pour creation gencoall
        copy "../copy/wor-gencolig.mod" replacing ==(pref)== by ==w-==.
DD0298* copie gencocom pour creation gencoalc
        copy "../copy/wor-gencocom.mod" replacing ==(pref)== by ==w-==.
DD0298* copie fcoadcli pour maj asdresse commande allotie fille
DD2001* copy "../copy/wor-fcoadcli-cdesup.mod"
DD2001  copy "../copy/wor-fjoadcli-cdesup.mod"
                       replacing ==(pref)== by ==w==.
       PROCEDURE DIVISION.
       PREM SECTION.
       T10.
DD0298     accept wentree
      * recuperation date du jour                                       *GPICMT
           move 'D' to immti-date-taj
           call 'mmti-date1' using mmti-date adl-art
           move wmmti-date-amj to wcd.

           move 'I' to gfkey.
           perform op-fcommaap.
           move 'I' to gfkey.
           perform op-multidat.
           move 'I' to gfkey.
           perform op-fartusap.
           move 'I' to gfkey.
           perform op-fclients.
           move 'W' to gfkey.
           perform op-gencoent.
DD0298     move "O" to gfkey
  "        perform op-gencoale
  "        move "O" to gfkey
  "        perform op-gencoall
  "        move "O" to gfkey
  "        perform op-gencoalc
  "        move "W" to gfkey
  "        perform op-cdesalle
  "        move "W" to gfkey
DD0298     perform op-cdesalll

      *----> M1297 (D)
           move 'I' to gfkey.
           perform op-guextmst.
           move 'W' to gfkey.
           perform op-ttfacmst.
           move 'I' to gfkey.
           perform op-filieres.
      *----> M1297 (F)

      *----> M0798 (D)
           move 'I' to gfkey.
           perform op-cliartsp.
           move 'W' to gfkey.
           perform op-fcomjoc3.
      *----> M0798 (F)
       deb.
           move 'W' to gfkey.
           perform op-gencolig.
       debdeb.
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
           move 'W' to gfkey.                                           *DDE026
           perform op-fcomjoc5.                                         *DDE026

      * fichier commentaire cde du jour
           move 'W' to gfkey.                                           *DDE026
           perform op-fcomjoc6.                                         *DDE026
      * fichier commentaire issu cde EDI
           move 'I' to gfkey.                                           *DDE026
           perform op-gencocom.                                         *DDE026
      * fichier commentaires des commandes en erreur
           move 'E' to gfkey.                                           *DDE026
           perform op-errcom.                                           *DDE026
      * fichier recap des commandes par assistante
           move 'O' to gfkey.                                           *DDE026
           perform op-seqcom1.                                          *DDE026

           move 'W' to gfkey.
           perform op-paramgpi.
           move 'E' to gfkey.
           perform op-errlig.
           move 'E' to gfkey.
           perform op-errent.
           string 'ADLPID' x'00' delimited by size
                  into var-name.
           move space to var-data.
           call 'genvcc' using var-name var-data.
           string 'prcde060.'
                  var-data delimited by ' '
                                     into wlabel-etat.

      *----> M0799 (D)
      *    recup des variables d'environnement
           call 'mmdt-envi1' using adl-art.
      *----> M0799 (D)

      *----> M1298 (D)
           move "DEVISE00" to pgbrac.
           move 50         to pgbcod.
           perform rnl-paramgpi.
           if file-status not = zero
            display "GPIWARNING MANQUE EL. DEVISE 50" go to fin.
           if pgbteu = zero display
                    "GPIWARNING EL. DEVISE 50 INCORRECT" go to fin.
           move pgbteu to zeuro.
      *----> M1298 (F)

      * init memo lecture des commentaires
           move spaces to wleccom.                                      *DDE026

           OPEN OUTPUT ETAT.
           MOVE ZERO TO WFIN WLEC TTFIN.
DD0351*    MOVE  90  TO WLIG.
           MOVE   1  TO gencoent-key gencolig-key WRAN1.

      * init rang commentaires
           move 0    to gencocom-key.                                   *DDE026
      * init cle entete et cle commentaire et fin fichier commentaire
           move spaces to wcleent wclecom wleccom                       *DDE026
DD9999     MOVE '|' TO SEP1 SEP2 SEP3 SEP4 SEP5 SEP6 
           .

       T20.
           MOVE SPACES TO WTAX1X WTAX2X.
      *DDE049
           MOVE  ZERO  TO WTGCX WREF WQUAN.

      * init code commande OK code creation entete et rang 1er commentaire
           move 0 to wok.                                               *DDE026
           move 0 to wecree wrancom.                                    *DDE026

           move zeroes to wcok wmfr wmgc.

DDE069     move zero  to td-clesc td-nolig.
DDE069     move spaces to wlib-comp.
DDE069     move 1     to wcok.
DD0400* raz client pour edition bon numero de client en anomalie
DD0400     move spaces to clcle

           perform r-gencoent.
           if file-status not = zero
                     GO TO FIN.

      * memo ean cde, livre et reference commande client
           move RENGCD to weanentc.                                     *DDE026
           move rengli to weanent.                                      *DDE026
           move renrcc to wcdeent.                                      *DDE026
DD0177     move spaces to wplateforme
DD0298     move zero to wtrh
      * init num cde
           move zero to wnum.                                           *DDE026

      * RENTYP : type de commande EDI ALLEGRO                           *GPICMT
      *                       23=LEROY (message gencod)                 *GPICMT
      *                      105=CASTORAMA (message eancom)             *GPICMT
      *                      220=BRICORAMA (message eancom              *GPICMT
DD0298*                      YB1=ALLOTIE AUCHAN                         *GPICMT
GPICMT     IF RENTYP NOT = "023" and rentyp not = "105"                 *M0600
GPICMT                          and RENTYP NOT = "YB1"                  *DD0298
GPICMT                          and rentyp not = "220" MOVE 21 TO WERR	*M0600
                              GO TO ERR1.
      *----> M0598 (D)
GPICMT* recherche code interne client
DD0420*    move renncl to wclcde w-alcde.
  -        move rengli to fiean
  -        perform rnl-filieres
  -        IF file-status not = zero
  -           MOVE 49 TO WERR
  -           go to ERR1
  -        END-IF
DD0420     move fincl to wclcde w-alcde wclient
           .
      *----> M0598 (F)
       T25.
           IF WFIN = 1         MOVE 41 TO WERR  GO TO ERR1.
           IF WLEC = 0         GO TO T30.
      *----> M0598 (D)
           IF RENGLI = RLIGLI AND RENRCC = RLIRCC  and
              rengcd = rligcd GO TO T30.
      *----> M0598 (F)
           MOVE 32 TO WERR.
           GO TO ERR1.
       T30.
           move zero to wcllia.

DD0420*    MOVE RENNCL TO CLNCL.
DD0420     MOVE wclient TO CLNCL.

      *----> M0598 (D)
DDE155* lecture client livre pour recuperer adresse de livraison
           perform rnl-fclients.
           if file-status not = zero
                    MOVE 22 TO WERR  GO TO ERR1.

GPICMT* si client bloque on ne cree pas la commande
DD0775*
DD0351*    if clcsu = 9
  -   *       MOVE 22 TO WERR
  -   *       GO TO ERR1
DD0351*    end-if
DD0775*

GPICMT* si client non valide on cree la commande a valider
DD0400     if cletat not = cmmpa-etat-valide
  -           MOVE 34 TO WERR
  -           perform err1 thru errf
DD0400     end-if

DDE191* recherche marche du client a partir de son secteur
           move clsecteur to wmmpa-sect-sect
           move ' ' to immpa-sect-trt
           call 'mmpa-sect1' using mmpa-sect adl-art

DDE339* recherche lieu edition a partir du code magasin expediteur client
           move "L" to immdt-lieu-trt
           move clmli to immdt-lieu-lieu
           call "mmdt-lieu1" using wmmdt-lieu adl-art
           if lili-corlieu = spaces
              if mmdt-societe = "GPI"
                 move "3" to lili-corlieu
              else
                 move "1" to lili-corlieu
              end-if
           end-if


           move clnom to wnom.
           move clrss to wrso.
           move clrue to wrue.
           move clvil to wbpo.
GPICMT* pour la france on charge le code postal et on l'enleve du bureau
DD0279     move clpays to wpays
  -        if clpays = "FR"
  -           move clcop to wccp
  -           move clbud to wbdi
  -        else
              if CLADREXPOR not = spaces
  -              move CLADREXPOR to wbdi
  -              move zero to wccp
  -           else
  -              move clcop to wccp
  -              move clbud to wbdi
  -           end-if
DD0279     end-if

      * memo transporteur habituel
DD0298     move cltrh to wtrh

DD0420*    move renncl to wclcde w-alcde.
DD0420     move wclient to wclcde w-alcde.
           if rengcd = rengli go to t31.
           move rengcd to fiean.
           perform rnl-filieres.
           if file-status not = zero move 33 to werr
                                     go to err1.
DD0180* GPICMT renncl correspond au client interne de l'ean livre
DD0180* GPICMT dans le cas d'une livraison sur plateforme il faut relire
DD0180* GPICMT mettre l'adresse du commande par dans l'entete
DD0420*    move renncl to clncl
GPICMT* pour PLASTO et SCALANDES on frce le livrea a la pateforme en attendant que le message edi soit conforme
DD0774     if mmdt-societe = 'PLASTO' and clnuf = 191315 
              AND mmdt-envi-annee < 14
  -           move 206756 to wclient
DD0465     end-if
DD0774     if mmdt-societe = 'PLASTO' and clnuf = 126590
              AND mmdt-envi-annee > 13
DD0774        move 130630 to wclient
DD0774     end-if
DD0420     move wclient to clncl
           perform rnl-fclients
           if file-status not = zero
              MOVE 22 TO WERR
              GO TO ERR1
           else

DD0775*
GPICMT* si client bloque on ne cree pas la commande
DD0351*       if clcsu = 9
  -   *          MOVE 22 TO WERR
  -   *          GO TO ERR1
DD0351*       end-if
DD0775*

GPICMT* si client non valide on cree la commande a valider
DD0400     if cletat not = cmmpa-etat-valide
              MOVE 34 TO WERR
              perform err1 thru errf
DD0400     end-if

GPICMT* si livrea est different du commande par et que le niveau du livrea est Plateforme
GPICMT* transporteur ou cross dock(T/K) ou depot(D) le livrea est considere comme une adresse
GPICMT* de livraison et la commande sera imputee au commande par, dans les autres cas la
GPICMT* commande sera imputee au livrea (commande par la centrale ==> livre au magasin)
GPICMT        if clniveau = "K" or = "T" or = "D"
GPICMT           move fincl to wclcde clncl
              end-if
              if clniveau = "K"
                 move fincl to clncl
                 perform rnl-fclients
                 if file-status not = zero
                    MOVE 22 TO WERR
                    GO TO ERR1
                 else
DD0775*
GPICMT* si client bloque on ne cree pas la commande
DD0351*             if clcsu = 9
  -   *                MOVE 22 TO WERR
  -   *                GO TO ERR1
DD0351*             end-if
DD0775*
GPICMT* si client non valide on cree la commande a valider
DD0400             if cletat not = cmmpa-etat-valide
  -                   MOVE 34 TO WERR
  -                   perform err1 thru errf
DD0400             end-if

                    move clnom to wnom
                    move clrss to wrso
                    move clrue to wrue
                    move clvil to wbpo
DD0279              move clpays to wpays
  -                 if clpays = "FR"
  -                    move clcop to wccp
  -                    move clbud to wbdi
  -                 else
                       if CLADREXPOR not = spaces
  -                       move CLADREXPOR to wbdi
  -                       move zero to wccp
  -                    else
  -                       move clcop to wccp
  -                       move clbud to wbdi
  -                    end-if
DD0279              end-if

                    move "K" to wplateforme
                 end-if
              end-if
           end-if
           move fincl to w-alcde
DD0420*    move renncl to wcllia.
DD0420     move wclient to wcllia.
       t31.
      *----> M0598 (F)

           perform r-fclients.
           if file-status not = zero
                     MOVE 22 TO WERR  GO TO ERR1.
DD0775*
GPICMT* si client bloque on ne cree pas la commande
DD0351*    if clcsu = 9
  -   *       MOVE 22 TO WERR
  -   *       GO TO ERR1
DD0351*    end-if
DD0775*

DD0775*
GPICMT* Lecture du code surveillance du facture a
GPICMT* --> si bloque --> pas de creation de commande
  |        initialize wcgcl-csur
  |        move clfaa                 to icgcl-csur-client
  |        move ccgcl-csur-trt-client to icgcl-csur-trt
  |        call "cgcl-csur1" using cgcl-csur adl-art
  |        if ocgcl-csur-rtn = cmmdt-envi-rtn-ERR
  |          move 22 to werr
  |          go to err1
  |        end-if
DD0775*

GPICMT* si client non valide on cree la commande a valider
DD0400     if cletat not = cmmpa-etat-valide
              MOVE 34 TO WERR
              perform err1 thru errf
DD0400     end-if


      *----> DDE025 (D)
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
      *----> DDE025 (F)

           if cltrif = 9 move clrek to wcok.
      *chargement de l'escompte
DDE069     move clesc to td-clesc.

DD0076*GPICMT memo code greoupement pour gestion zones specifiques
DD0076     move clrdi to wrdi.                                          *DDE02
DD0425* memo type de client pour trt location gerance erels
DD0425     move cltype to wtype

      *----> M0598 (D)
           if wcllia = zero move cllia to wcllia.
      *----> M0598 (F)

DD0002* memo client facture et regle du client de la commande
            move clfaa to wclfacturea
            move clrep to wclreglepar

      *
      ** ATTRIBUTION NO.COMMANDE

           perform rechnum.
           if ocgcd-nume-rtn not = spaces display ocgcd-nume-err
                                          display "GPIWARNING"
                                          move 43 to werr go to err1.

GPICMT*DD0800 recherche si cde magique intersite
GPICMT*si groupement = SIT c'est une cde magique, on met a jour le num. cde client ds foucmagi
           if wrdi = "SIT"
              move "W" to gfkey
              perform op-foucmagi
              move wcdeent to fcm-cle
              perform r-foucmagi
              if file-status = zero
                 move wnum to fcm-num
                 move zero to fcm-ind
                 perform rw-foucmagi
                 if file-status not = zero
                    string "erreur rw-foucmagi FS " file-status
                      " cde d'achat : " wcdeent
                      delimited by size into wlibelle-erreur
                    perform env-mail
                 end-if
              else
                 string "Commande d'achat " wcdeent
                   " non trouve dans foucmagi FS "
                   file-status
                   delimited by size into  wlibelle-erreur
                    perform env-mail
              end-if
              perform cl-foucmagi
           end-if


      *----> DDE026 (F)

      ** VERIF. DATE LIVRAISON - CODE SURV. CLIENT
      *----> M0299 (D)
          move rendli to zrendli.
          if rendli > zero move rendli to cadate go to t51.

     *DDE084 calcul delai par fonction
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

      *----> M0599 (D) GPI2000 door
           if CADATA > 50 move 19 to CADATS
             else         move 20 to CADATS.
      *----> M1299 (D)
           IF CADATA > 50 AND WA < 51
      *----> M1299 (F)
                           MOVE 1 TO WERR  PERFORM ERR1 THRU ERRF
                            GO TO T52.
      *----> M1299 (D)
           IF CADATA < 51 AND WA > 50 GO TO suit2000.
      *----> M1299 (F)
           IF CADATE < WCD MOVE  1 TO WERR  PERFORM ERR1 THRU ERRF
                            GO TO T52.
       suit2000.
      *----> M0599 (F) GPI2000 door
           perform r-multidat.
           if file-status not = zero
                     MOVE  2 TO WERR  PERFORM ERR1 THRU ERRF
                                 GO TO T52.
           IF CAVAJO NOT = 1     MOVE  3 TO WERR  PERFORM ERR1 THRU ERRF
                                 GO TO T52.
       T52.
           IF CLCSU NOT = ZERO   MOVE  4 TO WERR PERFORM ERR1 THRU ERRF.

      * init tables qte classe 3                                        *DDE025
           move spaces to wtab3x.                                       *DDE025

      *
      ** CREATION AP COMMANDE
DD9999* elgu le 06/05/03 initialisation fcomjoap car fcnfa non initialise
      *                  a zero ==> manque element lecture sur cle 4
DD0279     INITIALIZE wor-fcomjoap WACCOJ

           MOVE ZERO TO FJGEO FJNIN FJFOA FJCTA FJFRA FJNBF FJNAF FJCCP
                        fjnfa
                        fjnumr fjninr.
           MOVE ALL "0" TO    FJGCO.
           MOVE SPACES  TO    FJDTR FJRCL FJPTT FJTAR FJACS FJNTR FJNOM
                  FJRSO FJRUE FJBPO FJBDI FJQUA FJCOP FJCQU FJCCO.
      *----> M0497a (D)
      *elgu le 22/10/02 on met EDI ds qui saisit permettra de reconnaitre
      *     les commandes passant par edi sans risque d'avoir un login AUTO
      *   move "AUTO" to fjqsa.
          move "EDI " to fjqsa.
      *----> M0497a (F)

GPICMT*DDE191 on initialise le marche a GDPU si non parametre
GPICMT     if ommpa-sect-marche not = spaces
GPICMT        move ommpa-sect-marche to fjmarche
GPICMT     else
GPICMT        move 'GDPU' to fjmarche
           end-if

           MOVE ZERO TO WQUAN WMONT.


      *----> M0299 (D)
          if rendli > zero MOVE RENDLI TO FJDLI WFJDLI
            else           move zrendli to fjdli wfjdli.
      *----> M0299 (F)
           MOVE CLGEO  TO FJGEO WFJGEO.
           MOVE WNUM   TO FJNUM WFJNUM.
      *----> M1298 (D)
           move wtba   to fjtba.
      *----> M1298 (F)
           move wfoa   to fjfoa.
           MOVE ZERO   TO FJNIN WFJNIN wunix2 wunix4.
      *----> M0598 (D)
           MOVE wclcde TO FJNCL.
      *----> M0598 (F)
      *on chagre la date du jour ds la date de reference tarif          *GPICMT
DDE153     move wcd    to fjdatetarif9
           MOVE WA     TO FJANN.
           MOVE WM     TO FJMOI.
           MOVE WJ     TO FJJOU.
           MOVE CLNRE  TO FJREP.
           MOVE "D"    TO FJDTR.
DD0221*    MOVE RENRCL TO FJRCL wrcl.
DD0221     MOVE RENRCL TO wrcl.
DD0221     MOVE RENRCL(1:4)  TO FJRCL(1:4).
DD0221     MOVE RENRCL(6:15) TO FJRCL(5:15).
GPICMT* on reconstruit la refernece commade client avec les 8 derniers caracteres
DD0221*    perform cal-refcli
DD0221*    move wrcl to fjrcl
DD0420*    move renrcl (6:8) to wrefcdecli
DD0420     move renrcl (6:15) to wrefcdecli
           MOVE CLTRIF TO FJCTA WCTA.

      * chargt tarif fiche client                                       *DDE025
           move wcltaro to fjtaro.                                      *DDE025

           IF CLCFR NOT = ZERO  MOVE CLCFR TO FJFRA WFRA
                          ELSE  MOVE 1     TO FJFRA WFRA.
GPICMT* recherche montant franco du client                              *DDE069
           move zero to wmfr
GPICMT     move clncl to imgre-mtfr-ncl
GPICMT     move wcltaro1 to imgre-mtfr-sufa
GPICMT     call 'mgre-mtfr1' using mgre-mtfr adl-art
           IF omgre-mtfr-rtn = '0'
              move omgre-mtfr-mfr to wmfr
           END-IF
           .

           MOVE 1      TO FJDLR.
DD0424*    MOVE cldli  TO FJDLE.
DD0424     MOVE zero   TO FJDLE.
           MOVE CLCSU  TO FJCSC.
           MOVE 1      TO FJIEF.
           MOVE CLTRH  TO FJTHA.
      *anciennement zone sernam plus uitilisee
DDE103     MOVE spaces TO FJACS.
           MOVE RENGCD TO WGENC.
           MOVE WNUD   TO FJNUD.
           MOVE WCID   TO FJCID.
           IF RENRCC NUMERIC  MOVE RENRD TO FJRD.
           MOVE RENGLI TO WGENC.
           MOVE WNUD   TO FJZO1.
           MOVE WCID   TO FJZO2.
           MOVE CLNAF  TO FJNAF WNAF.
           MOVE CLNUF  TO FJNOF WNOF.

      *----> M0898 (D)
           move zero to wnofn.
      *----> M0898 (F)

      *----> M0497 (D)
           move clnum  to wclpa.
      *----> M0497 (F)

      *----> M0299b (D)
          move td-clesc to fjesc.
      *----> M0299b (F)

DDE155* maj adresse client livre de l'entete depuis client livre
           MOVE wnom   TO FJNOM.
           MOVE wrso   TO FJRSO.
           MOVE wrue   TO FJRUE.
           MOVE wbpo   TO FJBPO.
           MOVE wccp   TO FJCCP.
           MOVE wbdi   TO FJBDI.
DD0279     move wpays  to fjpays

DD0448*    MOVE 1      TO FJDI2.
DD0448*    if clgeo not = zero move 4 to fjdi2.
DD0448     move cltaxe to FJDI2
           MOVE CLCQU  TO FJCQU.
           MOVE CLCCO  TO FJCCO.
           MOVE ZERO TO WREF.
       T54D.
GPICMT* lecture de la filiation du client afin de memoriser sa propre filiation
GPICMT* pour la recherche des libelles complementaires articles
DD9999* si filiation differente de zero
        if clnuf not = zero
           MOVE CLNUF TO CLNCL
           MOVE ZERO TO WINVF
           perform r-fclients
           if file-status not = zero
              MOVE 1 TO WINVF
              GO TO T54F
           end-if
        end-if

      *----> M0898 (D)
           move clnuf to wnofn.
      *----> M0898 (F)
DD9999* lecture client regle pour prendre condition de reglement
           MOVE wclreglepar TO CLNCL.
           MOVE ZERO TO WINVF.
           perform r-fclients.
           if file-status not = zero
                     MOVE 1 TO WINVF  GO TO T54F.
           MOVE CLCRT TO FJREG.

DD0351* controle code reglement                                         *GPICMT
  -        move 'C' to immpa-regl-cof
  -        move fjreg  to wmmpa-regl-regl
  -        move spaces to immpa-regl-choix
  -        call 'mmpa-regl1' using mmpa-regl adl-art
  -        if ommpa-regl-rtn not = '0'
  -           move 47 to werr
  -           perform err1 thru errf
DD0351     end-if

DD0002* lecture client facture pour prendre les donnees de facturation
DD9999     MOVE wclfacturea TO CLNCL.
           MOVE ZERO TO WINVF.
           perform r-fclients.
           if file-status not = zero
                     MOVE 1 TO WINVF  GO TO T54F.

DD0775*
GPICMT* --> si bloque --> Referencement incorrect
  |        initialize wcgcl-csur
  |        move clcsu                  to wcgcl-csur-surveil
  |        move ccgcl-csur-trt-surveil to icgcl-csur-trt
  |        call "cgcl-csur1" using cgcl-csur adl-art
  |        if ocgcl-csur-rtn = cmmdt-envi-rtn-ERR
  |          MOVE 1 TO WINVF
  |          GO TO T54F
  |        end-if
DD0775*
           .

       T54F.
           EXIT.
       T55.
DD0176* on prend le code RDI du commande par
DDE153     move clregrfa to fjregrfa
           MOVE CLNBF TO FJNBF.
           MOVE CLCDD TO FJDEV PGBCOD.
      * chargement code piege client
DD0516     move clpiege to FJICP
      * chargement code impression manuelle facture client
DD0516     move climprim to FJITC
      *----> M0697a (D)
           move 1 to ztdv.
      *----> M0697a (F)
           IF CLCDD = 0  GO TO T55F.
           MOVE "DEVISE00" TO  PGBRAC.
           perform rnl-paramgpi.
           if file-status not = zero
                     GO TO T55F.
      *----> M0697a (D)
           move pgbtcd to ztdv.
      *----> M0697a (F)
           IF PGBLNG = ZERO      GO TO T55F.
      *----> M0699 (D)
           IF PGBLNG = PGBCOD MOVE PGBLNG TO FJLNG  GO TO T55F.
      *----> M0699 (F)
           MOVE PGBLNG TO PGBCOD.
           perform rnl-paramgpi.
           if file-status not = zero
                     GO TO T55F.
           IF PGBLNG = PGBCOD MOVE PGBLNG TO FJLNG.
       T55F.
           EXIT.
       T60.
DDE339* ajout maj codes lieux prod/expe/edition
           move lili-corlieu to fjlpr fjlli fjled
DD0177     move wplateforme to fjplateforme
GPICMT* chargement type de commande
DD0298     move ccmpa-tycd-typ-normale(1:1) to fjfeo
DD0425     if wtype = 'E'
  -             and mmdt-societe = 'GPI'
  -           move ccmpa-tycd-typ-locger(1:1) to fjfeo
DD0425     end-if
  "        if rentyp = "YB1"
  "           if wtrt = "A"
  "              move ccmpa-tycd-typ-allotie-fille(1:1) to fjfeo
  "           else
  "              move ccmpa-tycd-typ-allotie(1:1) to fjfeo
  "           end-if
DD0298     end-if

           move wnta to fjtve

GPICMT* controle existence d'une commande avec meme ref commande client/meme client et meme delai
GPICMT* afin de vpir les commandes passees en double
DD0351     perform ctrl-doublon-j
  -        if ocmcd-lect-rtn = cmmdt-envi-rtn-ok
  -           move 50 to werr
  -           go to err1
  -        end-if
DD0351     perform ctrl-doublon-p
  -        if ocmcd-lect-rtn = cmmdt-envi-rtn-ok
  -           move 51 to werr
  -           go to err1
DD0351     end-if

DD0812* anes 24/08/2017 Determination des flux LER
  |        if wrdi = "LER"
  |          if RENRFF-AFO = "S"
  |            move "LS" to fjfluxclient
  |          end-if
  |          if renrff-ct1 not = space
  |            move "CT" to fjfluxclient
  |          end-if
  |          if renrff-pd1 not = space
  |            move "PD" to fjfluxclient
  |          end-if
DD0812     end-if
 
DD0814* DOOR 10/01/2018 ajout du rayon
DD0814     move space to wrayon
           perform w-fcomjoap.
           if file-status not = zero
                    GO TO ERRAP.

GPICMT* controle code taxe
DD0448     perform ctrl-taxe

GPICMT* traitement entete commande allotie
DD0298     if rentyp = "YB1" and wtrt not = "A"                         *GPICMT
  "           perform entete-allotie                                    *GPICMT
  "           if werr not = zero
  "              go to err1
  "           end-if
DD0298     end-if

DD0180* GPICMT creation donnees specifique commande client
GPICMT* suppression des donnes specifiques clients utilisees seulement pour TSM
GPICMT* traitement plus loin par rapport au code groupement clrdi
GPICMT* ces donnees avaient ete utiliser pour recuperer une ref commande client de plus de 8 car
GPICMT* finalement on a mis la reference entiere dand fcomjoc1 afin de pouvoir le mettre
GPICMT* en saisie de commande manuelle le fichier fcomjoc8 n'etant pas saisi
DD0420     if renlib1 not = spaces
  -           move fjcle to icmcd-majc-e1numcde
  -           move "C"   to icmcd-majc-e1action
  -           move ccmcd-gest-trt-jour to icmcd-majc-e1trt
  -           move ccmcd-majc-e1type-c8 to icmcd-majc-e1type
  -           move renlib1 to icmcd-majc-e2texte
  -           move 3 to icmcd-majc-direct
  -           call 'cmcd-majc1' using cmcd-majc adl-art
  -           if ocmcd-majc-rtn not = cmmdt-envi-rtn-ok
  -              move 40 to werr
  -              perform err1 thru errf
  -           end-if
DD0420     end-if


      *----> DDE026 (D)
      * maj code entete de commande cree
           move 1 to wecree.
      *----> DDE026 (F)

      *
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
GPICMT* pour AUCHAN on remplace l'EAN du commande par en commentaire
GPICMT* par le rayon et notre numero fournissuer chez AUCHAN
GPICMT     if wrdi = "AUC"
GPICMT        MOVE "RY053 FR71418" to FJLI1
GPICMT     end-if

      * memo reference commande BRICORAMA                               *M0600a
GPICMT* si commande par influe les ref commande client de plus de 8 car sont dans renlib1
GPICMT* si commande par @gp la ref commande client a bien 15 car
DD0420*    if wrefsuite not = spaces
  -   *       move "COMMANDE CLIENT:  " to wlab
  -   *       move wrefcdecli           to wlbb
  -   *       move wlentb to fjli2
DD0420*    else
      *       if renlib1 not = spaces
      *          move "COMMANDE CLIENT:  " to wlab                      *M0600a
      *          move renlib1              to wlbb                      *M0600a
DD0359*          move renlib1 to wrefcdecli
      *          move wlentb to fjli2                                   *M0600a
      *       end-if
      *    end-if

DD0076* GPICMT chargement contremarque
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

GPICMT* pour LEAU mettre un commentaire gamme leau
DD0314     if fjtve(1:1) = 9
               move all "*" to fjli2 fjli4
               move "********  GAMME LEAU  ********" to fjli3
           end-if

           perform w-fcomjoc1.
           IF file-status not = "00"
                          display "GPIWARNING"
                          DISPLAY "** FICHIER en-tete JOUR PLEIN **"
                          " - STATUS = " file-status
                          MOVE 45 TO WERR  GO TO ERR1.
      *
      ** CREATION FCOADCLI (4 NOS CLIENTS) - VERIF. NOS EXISTANTS
       T70.
DD9999     INITIALIZE     wor-fcoadcli2.
           MOVE WNUM   TO ALNUM.
           MOVE ZERO   TO ALNIN.
           MOVE SPACE  TO ALCOD.
      *----> M0598 (D)
           MOVE wclcde TO ALCDE ALLIV ALFAC ALREG.
      *----> M0598 (F)
GPICMT* on charge le commande par de fcoadcli avec le commande par EDI
GPICMT     MOVE w-alcde TO ALCDE
           if wcllia not = zero move wcllia to alliv.
DD0002     move wclfacturea to alfac
DD0002     move wclreglepar to alreg
           .
       T75.
           MOVE SPACE  TO ALADC.
      *----> M0497 (D)
      *----> M0497 (F)
DD0777* Traduction du code EAN du client final en numero de client
  |        if reneanclfin not = space and not = zero
  |          move reneanclfin to fiean
  |          perform rnl-filieres 
  |          if file-status not = zero
  |            move 56 to werr
  |            perform err1 thru errf
  |            move zero to alncl-final
  |          else
  |            move fincl to alncl-final clncl
  |            perform rnl-fclients
  |            if file-status not = zero
  |              move 57 to werr
  |              perform err1 thru errf
  |              move zero to alncl-final
  |            end-if
  |          end-if
DD0777     end-if
DD2001*    perform w-fcoadcli.
DD2001     perform w-fjoadcli.
           if file-status not = zero
                    GO TO ERRNL.
           IF WINVF = 1  MOVE 6 TO WERR  PERFORM ERR1 THRU ERRF.
      *
      ** AC  ELT 4 - LIGNE 1
       T80.
           IF WLEC = 1  MOVE ZERO TO WLEC
                        MOVE gencolig-key TO WRAN1 GO TO T82.
           perform r-gencolig.
           if file-status not = zero
                     MOVE 1 TO WFIN    GO TO T200.
      *----> M0598 (D)
           IF RLIGLI = RENGLI AND RLIRCC = RENRCC  and
              rligcd = rengcd GO TO T82.
      *----> M0598 (F)
           MOVE 1 TO WLEC.
           GO TO T200.
       T82.
GPICMT* si commande allotie mere creation des lignes articles ==> gencoall
DD0298     if rentyp = "YB1" and wtrt not = "A"                         *GPICMT
  "           perform ligne-allotie                                     *GPICMT
  "           go to t150
DD0298     end-if

      * pour article inexistant on cree une ligne de commande avec article
      * 9999999 00 et prix zero
           IF RLIXAR NOT NUMERIC go to t82-f.                           *DDE026
           MOVE RLIART TO WART.
           MOVE WCIP   TO WCLEN.
           move 01     to fanma1.
           move wcip   to facip.
           perform rsk1-fartusac.
           if file-status not = zero GO TO t82-f.
DD0466     go to  T82-1.

DD9999* appel recherche plusieurs reference avec meme gencod complet ean14#
           move rliart to wmgca-arti-gencod
           move 'B'    to imgca-arti-trt
DD0162     move cmmpa-etat-valide to imgca-arti-etat
GPICMT     call 'mgca-arti1' using mgca-arti adl-art
GPICMT     if omgca-arti-rtn = cmmdt-envi-rtn-ok
GPICMT        if omgca-arti-rtn = cmmdt-envi-rtn-faux
                 move omgca-arti-ref to fanar1
                 move omgca-arti-ssref to fansr1
GPICMT           move 37 to werr
GPICMT           perform err1 thru errf
GPICMT        else
GPICMT           go to t82-f
GPICMT        end-if
           end-if
           .
DD0466****************************DEBIT MODIF 1*******************************
DD0466* mise en place de mgcd-vean1 qui avec un gencod en entree, un client
DD0466* et une quantite, permet le retour d'une reference interne 
DD0466* ce programme supporte deux parametres, mis par default a zero (NON)
DD0466* controle pcb(--> verifie que la quantite est un multiple du pcb
DD0466* controle referencement ne tient pas compte des produits non referencee
DD0466* chez le cleint concerne
DD0351*anes 18/05/16 amelioration des messages retour de mgcd-vean1
DD0466*-----------------------------------------------------------------------
DD0466 T82-1.
DD0466     move rliart            to imgcd-vean-gencod
DD0466     move RLIQTC            to imgcd-vean-quantite 
DD0466     move FJNCL             to imgcd-vean-client
DD0466     move RLIPCB            to imgcd-vean-pcb-client
DD0466     move RLIPHT            to imgcd-vean-prix-client
DD0466     call 'mgcd-vean1' using mgcd-vean adl-art
DD0466     move omgcd-vean-ref  to fanar1
DD0466     move omgcd-vean-sref to fansr1
DD0466     IF omgcd-vean-rtn not = cmmdt-envi-rtn-OK
DD0466*     move 37 to werr DD0351
DD0351*anes 18/5/16 si erreur, edition du libelle d'erreur recu de mgcd-vean1
DD0351      move 59 to werr
  |         move omgcd-vean-liberr to llib
DD0351      move rlixar  to lart
DD0466      perform err1 thru errf
DD0466      go to T82-2
DD0466     END-IF
DD0351*anes 18/05/16 amelioration des messages retour de mgcd-vean1
  |   * Arrive ici il n'y a pas d'erreur.    
  |   * Si liberr not = space -> edition des infos transmises
  |        if omgcd-vean-liberr not = space
  |          move omgcd-vean-liberr to wliberr
  |          move 60      to werr
  |          move rlixar  to lart
  |   * Edition des references internes trouvees
  |          move space   to llib
  |          string wliberr-nb-ref " ref internes trouvees "
  |            wliberr-wp-ref(1) " " wliberr-wp-sref(1) " - "
  |            wliberr-wp-ref(2) " " wliberr-wp-sref(2) " - "
  |            wliberr-wp-ref(3) " " wliberr-wp-sref(3)
  |            delimited by size into llib
  |          perform err1 thru errf
  |          move space   to lart llib
  |   * deuxieme ligne des references trouvees...s'il y a lieu
  |          if wliberr-wp-ref (4) not = space
  |            move 60      to werr
  |            string "--"
  |              wliberr-wp-ref(4) " " wliberr-wp-sref(4) " - "
  |              wliberr-wp-ref(5) " " wliberr-wp-sref(5) " - "
  |              wliberr-wp-ref(6) " " wliberr-wp-sref(6) " - "
  |              wliberr-wp-ref(7) " " wliberr-wp-sref(7)
  |              delimited by size into llib
  |            perform err1 thru errf
  |          end-if
  |   * Edition reference retenue et critere de selection de ladite
  |          move spaces to llib
  |          if wliberr-critere numeric
  |          and wliberr-critere > 0
  |          and wliberr-critere not > cmgcd-vean-nbr-critere
  |            move 60      to werr
  |            string "--Retenue "
  |                   wliberr-ref-retenue " " wliberr-sref-retenue " "
  |                  tmgcd-vean-pos-libelle (wliberr-critere)
  |                  delimited by size into llib
  |          end-if
  |          perform err1 thru errf
  |          move spaces to llib
DD0351     end-if
DD0466     .
DD0466 T82-2.
DD0466****************************FIN MODIF 1*********************************
GPICMT* dans le cas de multi ref pour le meme gencod la fonction mgca-arti1
GPICMT* ramene la 1ere refe valide trouvee
DD0466*    move omgca-arti-ref to fanar1
DD0466*    move omgca-arti-ssref to fansr1
           perform rnl-fartusac
           if file-status not = zero

      * pour article inexistant on cree une ligne de commande avec article
      * 9999999 00 et prix zero
                                  GO TO t82-f                           *DDE026
           end-if

           move fanma1 to fanma.
      *DDE046
           move fanar1x to fanarx.
           perform r-fartusap.

           if file-status not = zero                   GO TO t82-f.     *DDE026
GPICMT* n'autoriser qu'un article valide ou en fin de vie
GPICMT* si supprime creation avec code 9999999 et meme trt que si non trouve
GPICMT* DDE171 controle validite article
GPICMT* DDE171 controle validite sous reference
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

           go to t82-z.
       t82-f.
           move 23 to werr.

      *DDE053
       t82-g.

      * chargement libelle a saisir ou supprime
           if werr = 23 move "******* A SAISIR" to wlis1                *DDE053
             else       move "******* SUPPRIME" to wlis1.               *DDE053

DDE079     if werr = 37 move "***** REF DOUBLE" to wlis1.
           perform err1 thru errf.
      *DDE046
           move 9999999 to fanar1x.
           move 00      to fansr1x.
      *DDE049
           move 1 to fasufa faqpb.
      * chargement du gencod dans le libelle
           move rliart to wlis2.
           move wlis   to falia.
           move spaces to falis.
           move zero   to fapac faram faram facnu facip facle fava
                          favl fapcb faspc.

DDE086     move zero to fapan.

           move 2      to fatvp.
       t82-z.
      *----> DDE026 (F)

DDE171*  controle PCB de l'article avec celui de gencolig
          if rlipcb not = zero and not = faqpb
             move 38 to werr
GPICMT* controle que PCB client est un multiple de celui de la ref (AUCHAN)
             divide rlipcb by faqpb giving wpcb
             perform err1 thru errf
          end-if

      *----> DDE025 (D)
      * suppression du trt classe 3 la recherche du prix se fera a la fin
      * de la commande comme les autres
      *----> DDE025 (F)
         .
       T85.
DD0814* DOOR 10/01/2018 Recherche du rayon
DD0814     move 'E'                 to ifgrc-lect-testlg
DD0814     move space               to ifgrc-lect-testlg
DD0814     move FJNCL               to ifgrc-lect-e1client
DD0814     move 1                   to ifgrc-lect-raz
DD0814     move '01'                to ifgrc-lect-e1nmag
DD0814     move 'I'                 to ifgrc-lect-trt
DD0814     move FANAR1              to ifgrc-lect-e1ref
DD0814     move FANSR1              to ifgrc-lect-e1sref
DD0814     call 'fgrc-lect1' using fgrc-lect adl-art
DD0814     IF ofgrc-lect-rtn = cfgrc-lect-rtn-ok
DD0814       move trencl to rencl
DD0814       move "01"  to remag
DD0814       move fanar1 to renar
DD0814       move fansr1 to rensr
DD0814       perform rnl-refercli
DD0814       if file-status = zero
DD0814         if rearr not = space
DD0814           move rearr to wrayon
DD0814         end-if
DD0814       end-if
DD0814     END-IF

DD9999     INITIALIZE  jwor-fcommac22.                                  *DD9999
DDE125     MOVE  ZERO  TO jfcgeo2 jfcnin2 jfcprx jfcest jfckle jfcmar
DDE125                    jfcetq.
DDE125     MOVE SPACES TO jfcdes jfcsrc jfcrac jfcge1 jfcge2 jfcuat.
      *----> M0299 (D)
DDE125     if rendli > zero MOVE RENDLI TO jfcdli2
DDE125        else           move zrendli to jfcdli2.
      *----> M0299 (F)
DDE069     move zero to jfcremp jfctgc.                                   DDE125
DDE125     MOVE WFJGEO TO jfcgeo2.
DDE125     MOVE WNUM   TO jfcnum2.
DDE125     MOVE 4      TO jfcnel2 WFJNEL.
DDE125     move wunix2 to jfcunix2.
           add  1      to wunix2.
      *DDE046
DDE125     MOVE FANAR1 TO jfcnar WFJNARX.
DDE125     MOVE FANSR1 TO jfcsre WFJSREX.

      *----> DDE025 (D)
      * on initialise le code type de ligne a 2 pour eviter le recalcul de prix
      * par le coefficient de la commande puisqu'il ne veut plus rien dire sauf
      * si le coef est impose dans la fiche du client avec code tarif 9 pour
      * les articles classe 1 uniquement
      *DDE049
DDE045*   on met systematiquement le code type de ligne a 1 car 2 est   *GPICMT
DDE045*   utilise pour les articles sur devis                           *GPICMT
DDE045*   le code 1 etatit initialement utilise pour le calucul d'un    *GPICMT
DDE045*   prix net par multiplication avec un coef                      *GPICMT
DDE045*   le code fjtopx permet de connaitre l'origine du prix          *GPICMT
DDE045    MOVE 1      TO jfclig.                                        DDE125
      * chrgt gde classe ds la ligne
DDE125     move fatgc to jfctgc.
      *----> DDE025 (F)

           MOVE 5      TO       WFJLIG.
DDE125     move faram  to jfcrgt.
DDE125     MOVE FALIS  TO jfcsrc.
DDE125     MOVE FACNU  TO jfccuf.
DDE125     MOVE FACIP  TO jfccip.
DDE125     MOVE FACLE  TO jfckle.
DDE125     MOVE FAVA   TO jfcva.
DDE125     MOVE FAVL   TO jfcvl.
DDE125     MOVE FAPCB  TO jfcpcb.
DDE125     MOVE FASPC  TO jfcspc.
           MOVE FATVP TO WFTVP.
           IF WTAX1X = SPACE MOVE WTVP2 TO WTAX1   GO TO T85B.
           IF WTVP2 = WTAX1                        GO TO T85B.
           IF WTAX2X = SPACE MOVE WTVP2 TO WTAX2   GO TO T85B.
           IF WTVP2 = WTAX2                        GO TO T85B.
           MOVE 25 TO WERR.

      * on signale l'erreur du code taxe et on continue
           perform err1 thru errf.                                      *DDE026

       T85B.

      *----> DDE025 (D)
      * suppression trt gde classes differentes
      *    IF WTGC = ZERO  MOVE FATGC TO WTGC      GO TO T85F.
      *    IF FATGC NOT = WTGC    MOVE 24 TO WERR  GO TO ERR1.
      *----> DDE025 (F)

       T85F.
DDE125     if jfcrgt = spaces
DDE125                       MOVE FANSE  TO jfcrgt WFJRGT.
DDE125     MOVE FALIA  TO jfcdes.
           MOVE RLIQTC TO WQTC.
           IF FAQPB = ZERO OR FAQPB = 1  GO TO T85L.

GPICMT* controle qte saisie en pcb pour deconditionnement dinac
DD0412   if mmdt-societe = 'DINAC'
  -        move wqtc         to icmca-qpcb-qte
  -        move faqpb        to icmca-qpcb-qpcb
  -        move 01           to icmca-qpcb-niv
  -        move fanar1       to icmca-qpcb-ref
  -        move fansr1       to icmca-qpcb-sref
  -        move fjncl        to icmca-qpcb-client
  -        call 'cmca-qpcb1' using cmca-qpcb adl-art
GPICMT* si retour warning, on considere que le controle a ete realise et on recupere la nouvelle quantite
  -        if ocmca-qpcb-rtn = cmmdt-envi-rtn-war
  -           if wqtc not = ocmca-qpcb-qte
  -              move wqtc to wqte-ori
  -              move ocmca-qpcb-qte to wqte-arr
  -              move 48 to werr
  -              perform err1 thru errf
  -           end-if
  -           move ocmca-qpcb-qte to wqtc
  -           go to T85L
DD0412     end-if
DD0351* pour GPI/PLASTO ne n'accepte pas le deconditionnement
  |        if mmdt-societe = 'PLASTO'
  |        or mmdt-societe = 'GPI'
  |          move spaces  to immca-qtes-reference
  |          move 4       to immca-qtes-cod
  |          move faqpb   to immca-qtes-con 
  |          move wqtc    to immca-qtes-qte
  |          move cmmca-qtes-arrondi-oui to immca-qtes-arrondi
  |          call 'mmca-qtes1' using wmmca-qtes adl-art
  |          if ommca-qtes-rtn = cmmdt-envi-rtn-war
  |             move wqtc to wqte-ori
  |             move ommca-qtes-qtr to wqte-arr
  |          end-if
  |        end-if           
  |        move wqte-arr to wqtc 
  |        if mmdt-societe = "GPI"
  |          move spaces  to immca-qtes-reference  
  |          move 1       to immca-qtes-cod
  |          move faqpb   to immca-qtes-con
  |          move wqtc    to immca-qtes-qte
  |          call 'mmca-qtes1' using wmmca-qtes adl-art
  |          move ommca-qtes-qtr to wqtc wqte-arr
  |        end-if
  |        if wqte-ori not = wqte-arr
  |          move 48 to werr
  |          perform err1 thru errf
DD0351     end-if
DD0412   end-if

GPICMT*DDE069 appel fonction de conversion des qtes en boites ou blisters
           move 2 to immca-qtes-cod
           move faqpb to immca-qtes-con
           move wqtc to immca-qtes-qte
           call 'mmca-qtes1' using wmmca-qtes adl-art
           if ommca-qtes-rtn = spaces
              move ommca-qtes-qtr to wqtc
              go to t85l
           end-if
GPICMT* si la conversion est impossible on met 1 en qte cdee
           MOVE 1 TO WQTC.
           MOVE 9 TO WERR.
           PERFORM ERR1 THRU ERRF.
       T85L.
DDE125     MOVE WQTC   TO jfcqtc.
      *---*
      *----> AE001 (D) : recherche du prix a appliquer a l'article :
      *    > calcul des remises niveau ARTICLE, REGROUPEMENT, REFERENCEMENT.
      *    > Des qu'il est trouve : go to t15-4 ou t16 suivant le cas.

          move space to td-top-prix.
      * appel fonction recherche de prix
          perform rech-tarif.
          if werr = 7 perform err1 thru errf.
GPICMT* pour plasto controle prix a l ligne
DD0465  if mmdt-societe = 'PLASTO'
  -        perform ctrl-prix
DD0465  end-if
        .
       T86.
DDE045* le tarif est charge par une fonction
      * zone type de prix (fjtopx)                                      *GPICMT
      * code fjtopx : espace = prix catalogue avec coef                 *GPICMT
      *               1 = prix saisi                                    *GPICMT
      *               2 = prix trouve ds CLIARTSP (articles speciaux)   *GPICMT
      *               3 = remise regroupement                           *GPICMT
      *               4 = prix trouve ds REFERCLI (referencement)       *GPICMT
      *               5 = tarif vrac (tarif 300)                        *GPICMT
      *               6 = remise trouve ds CLIARTSP (art. speciaux)     *GPICMT
      *               7 =  prix article sur devis                       *GPICMT
DDE125     move "5"    to jfctopx.                                       *DDE011
      *----> chargement des top-prix, taux de remise et
      *----> taux de majoration.
DDE125    move td-top-prix to jfctopx.
DDE125    move zero     to jfctrem.
DDE125    move zero     to jfctmaj.
      *----> chargement du numero de ligne.
          add 10 to td-nolig.
DDE125    move td-nolig to jfcnlg.
DD0465* pour Brico depot on prend le numero de ligne transmis par edi
  -        if wrdi = 'BDP'
  -           move RLINOL to jfcnlg
DD0465     end-if
           .
       T90.
DDE125     MOVE FAMES  TO jfcmes.
DDE125     MOVE FAPRI  TO jfcprx.
DDE125     MOVE FAQPB  TO jfcqpb.
DDE125     MOVE FAPOU  TO jfcpdu.
DDE125     MOVE FADIM  TO jfcdim.
DDE125     move fadoi  to jfcdou.
DDE125     MOVE FATVP  TO jfctvp.
DDE125     ADD jfcqtc TO WQUAN.

       T100.
      *----> M0697b (D)
DDE125     move "A" to jfctref.
      *----> M0697b (F)

      ***** RECHERCHE DU REF. CLIENT DANS LE REFERENCEMENT
DDE045* remplacer par un appel de fonction
           move fjncl  to icgre-arcl-ncl
           move fanma1 to icgre-arcl-magasin
           move fanarx to icgre-arcl-ref
           move fansr1x to icgre-arcl-sref
           call 'cgre-arcl1' using cgre-arcl adl-art
           if ocgre-arcl-rtn = '0'
DDE125        move ocgre-arcl-rac to jfcre1
DDE125        move ocgre-arcl-mou to jfcre2
DDE125        move ocgre-arcl-gma to jfcmar
           else
              if ocgre-arcl-rtn = '2'
                 move 5 to werr
                 perform err1 thru errf
              end-if
          END-IF
           .


      *DDE045 maj prix de base et % remise par la fonction recherche tarif
      *       cgca-rech1

DDE045*   if jfcpht = zero move '9' to jfctopx.                           DDE125
DD0351    if jfcpht = zero move spaces to jfctopx.                        DDE125
           perform w-fcomjoc2.
           IF file-status not = "00"
                       display "GPIWARNING"
                       DISPLAY "**FICHIER EL 4 TL 1 JOUR PLEIN**"
                       MOVE 45 TO WERR  GO TO ERR1.
      *----> M0798 (D)

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
           if wia > 18 go to t119-b2.                                   *DDE032
           if wt-lib-comp (wia) = spaces go to t119-b1.
DD9999     INITIALIZE            wor-fcomjoc3
DDE125     move jfcnum2        to fjnum3.
DDE125     move jfcnin2        to fjnin3.
           move 04            to fjnel3.
DDE125     move jfcnar         to fjnar3.
DDE125     move jfcsre         to fjsre3.
           move ztlig         to fjlig3.
DDE069     move jfcunix2       to fjunix3.                              DDE125
           move wt-lib-comp (wia) to fjtyb.
           if fjlc1 not = space move "G" to fjcl1.
           if fjlc2 not = space move "G" to fjcl2.
           if fjlc3 not = space move "G" to fjcl3.
DDE069     move jfcnlg         to fjnlg3.                               DDE125
DDE125     move jfcdli2        to fjdli3.
DDE125     move jfcgeo2        to fjgeo3.
DDE125     move jfcrgt         to fjrgt3.
           perform w-fcomjoc3.
           if file-status = "22" go to t119-b2.
           IF file-status not = "00"
                       display "GPIWARNING"
                       DISPLAY "**FICHIER FCOMJOC3  JOUR PLEIN**"
                       MOVE 45 TO WERR  GO TO ERR1.
           add 5 to ztlig.
       t119-b1.
           add 1 to wia.
           go to t119-a2.
       t119-b2.
      *----> M0798 (F)
      *
      *
      ** LECTURE LIGNE SUIVANTE
       T150.

      *----> DDE026 (D)
      * suite au trt multiclasse on ne controle plus si article classe 3
      *----> DDE026 (F)
DD0298     move wor-gencolig to w-wor-gencolig2
           ADD 1 TO gencolig-key.
           GO TO T80.
      *
      *
      *
      ** FIN DE COMMANDE - ELT 10 SI COMMENTAIRE
       T200.
      *----> M0299b (D)
DDE069** CREATION DE L'ESCOMPTE AUTOMATIQUE                             *GPICMT
          if td-clesc = 0 go to t201.
DD9999     INITIALIZE    wor-fcomjoc4
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
      *----> M0299b (F)

           go to t210.                                                  *M0600a

           IF RENLIB1 = SPACES  GO TO T207.
           MOVE RENLIB1 TO WLCOMM.
       T205.
DD9999     INITIALIZE     wor-fcomjoc4
DDE125     MOVE ZERO   TO FJGEO4 FJNIN4 FJCGS jfctvp.
           MOVE SPACES TO FJDOP FJ1LC FJ2LC FJSIG FJZ4 FJZ5 FJLIQ.
      *----> M0299 (D)
          if rendli > zero MOVE RENDLI TO FJDLI4
            else           move zrendli to fjdli4.
      *----> M0299 (F)
           MOVE WFJGEO TO FJGEO4.
           MOVE WNUM   TO FJNUM4.
           MOVE 10     TO FJNEL4.
           move wunix4 to fjunix4.
           add  1      to wunix4.
           MOVE WL1    TO FJDOP.
           MOVE WL2    TO FJ1LC.
           MOVE WL3    TO FJ2LC.
           perform w-fcomjoc4.
           IF file-status not = "00"
                          display "GPIWARNING"
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
      *DDE049
           move wcltaro1 to wtgcx.                                      *DDE025
           move spaces  to wzli.                                        *DDE025

      *M050401
       t211.

      * pas de recherche montant franco si tarif 9 (coef K ds fiche client)
      * ds tous les cas on recherche le coef avec gde classe = 1er caractere
      * du num tarif de la fiche client pour recuperer le montant franco
      * pour recuperer le montant franco on prend le nbre total de boites
      * de la commande
           move wquan to wcumqt.
       t215.
           MOVE 1 TO WCOKCAL.                                           *DDE025
           MOVE "COEF" TO PGCS1.
           MOVE  ZERO  TO PGCS2.
      *DDE049
           MOVE  WTGCX TO PGCS3X.
           MOVE  WCTA  TO PGCS4.

      *----> DDE025 (D)
      * si gde classe 3 et code categorie "B" 1 ds code tarif (tarif 300)
      *           "        autre actegorie    7      "        (tarif 301)
           if wtgc = 3
              if wzli = "B" move "1" to pgcs4
              else          move "7" to pgcs4
              end-if
           end-if.
      *----> DDE025 (F)

           perform rnl-paramgpi.
GPICMT*elgu le 06/06/03 cas code tarif 9 utilise pour articles speciaux client
GPICMT* les articles sont saisie en tarif liste et non en condition particuliere
GPICMT* le sous type client est K (ss condition particuliere)
GPICMT* pour resoudre ce probleme et utiliser un sous type C/M etc.. il faudrait
GPICMT* saisir des conditions particulieres ex: client 742501 KOTECA

DD9999     if file-status not = zero and wcta not = 9                   *GPICMT
DD0351* anes 18/05/2016 correction de la gestion de l'erreur
  |   *              MOVE 26 TO WERR  GO TO ERR1.
DD0351               MOVE 26 TO WERR  perform ERR1 thru errf.



GPICMT* on lit les coef simplement pour recuperer le numero de tarif article
GPICMT* pour le trt des commissions representant le trt des coef est supprime
           MOVE PGCNTA TO WNTAL.                                        *DDE025
           if wcta = 9 move wtgc to wnta1l                              *DDE025
                       move 99   to wnta2l.                             *DDE025


GPICMT*DDE069 le franco est traite par la fiche client uniquement et non plus
GPICMT*       par les coefficients et nbre de boites
       t221.
       t221f.
           exit.
       t222.
      ** MAJ PHT DANS LIGNES DE COMMANDE
DDE045* calcul du montant ht ligne et total commande fait par une fonction
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

DDE171*  controle H.T. cde calcule et celui de gencoent
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
           perform r-fcomjoap.
           if file-status not = zero
                     MOVE 27 TO WERR  GO TO ERR1.
           move wnta to fjtve.
DD0177*GPICMT pour livraison plateforme on force le port a franco
DD0177     if fjplateforme = "K"                                        *GPICMT
DD0298*GPICMT ou livraison allotie
DD0298        or fjfeo = ccmpa-tycd-typ-allotie(1:1)                    *GPICMT
DD0298        or fjfeo = ccmpa-tycd-typ-allotie-fille(1:1)              *GPICMT
              move 2 to fjfra
              go to t260
           end-if
      *----> M0697a (D)
           IF FJFRA NOT < 2     GO TO T246.
      *----> M0697a (F)
           IF WMFR = ZERO       GO TO T260.
      *----> M0697a (D)

GPICMT*DDE069 le montant franco pris ds la fiche client est dans la devise
GPICMT*       du client donc on ne fait plus de conversion
DDE069     IF wmont NOT < WMFR  MOVE 2 TO FJFRA.
           go to t260.
       t246.
           if fjfra = 2 or fjfra = 3 or fjfra = 5 go to t260.
           IF WMFR = ZERO       GO TO T260.
DDE069     if wmont not < wmfr go to t260.
           move 5 to fjfra.
      *----> M0697a (F)
       T260.
           MOVE WCOK  TO FJCOK.
           MOVE WMONT TO FJMHT.

      * mettre le code OK a jour juste avant la reecriture
           MOVE WQUAN TO FJTQU.
           MOVE 6 TO FJCOP.

      *----> M1297 (D)
      *----> M0598 (D)
           move wclcde to gsncl.
      *----> M0598 (F)
           perform rnl-guextmst.
           if file-status not = zero go to t270.
DD9999     INITIALIZE     wor-ttfacmst2
           move zero   to tsnfa tscpa tsgcp.
           move fjcle  to tscle.
           move gsrdi  to tsrdi.
      *----> M0498 (D)
           move gsco1 to tsco1.
           move gsco2 to tsco2.
           move gsco3 to tsco3.
           move gsco4 to tsco4.
           move gsco5 to tsco5.
      *----> M0498 (F)

GPICMT* appel gestion des types de commandes pour controle trt des codes
GPICMT* pour emissions factures/bl/aviexp par EDI
DD0298     initialize wcmpa-tycd                                        *GPICMT
  "        move fjfeo to wcmpa-tycd-typ                                 *GPICMT
  "        move ccmpa-tycd-gestion-tycd to icmpa-tycd-gestion           *GPICMT
DD0298     call 'cmpa-tycd1' using cmpa-tycd adl-art                    *GPICMT

GPICMT* test pour envoi AVIEXP suivi par rapport au type de commande
DD0298     if tsco1 = 1 and ocmpa-tycd-edbp = cmmdt-envi-rtn-faux       *GPICMT
  "           move zero to tsco1                                        *GPICMT
DD0298     end-if

GPICMT* test pour envoi BL(castorama) par rapport au type de commande
DD0298     if tsco2 = 1 and ocmpa-tycd-edbp = cmmdt-envi-rtn-faux       *GPICMT
  "           move zero to tsco2                                        *GPICMT
DD0298     end-if

GPICMT* test pour envoi facture par rapport au type de commande
DD0298     if tsco3 = 1 and ocmpa-tycd-edfact = cmmdt-envi-rtn-faux     *GPICMT
  "           move zero to tsco3 tsco5                                  *GPICMT
DD0298     end-if

           move wcd    to tsdin.
           if renrcd not = zero move renrcd to wtrcd
                                move wtj to tsdinj
                                move wtm to tsdinm.

GPICMT* pour une commande allotie fille on charge le commande par de la mere
DD0298     if fjfeo = ccmpa-tycd-typ-allotie-fille(1:1)
  "           move w-rleanmag to tscpa
  "        else
              move rengcd to tscpa
DD0298     end-if
DD0420*    if rengcd = rengli move renncl to tsgcp
DD0420     if rengcd = rengli move wclient to tsgcp
                              go to t261.
           move rengcd to fiean.
           perform rnl-filieres.
           if file-status not = zero move 19 to werr
                                     perform err1 thru errf
             else move fincl to tsgcp.
       t261.
           perform w-ttfacmst.
           if file-status not = zero move 18 to werr
                          display "GPIWARNING"
                          DISPLAY "** ECRITURE TTFACMST IMPOSSIBLE"
                          " - STATUS = " file-status
                          "  COMMANDE :  " fjcle
                                     perform err1 thru errf.
      *----> M1297 (F)

      *----> M1198 (D)
           move tsco5 to fjfdem.
      *----> M1198 (F)
       T270.
      *----> M0999 (D)
           if wcllia not = zero move wcllia to clncl
             else               move fjncl  to clncl.
           perform rnl-fclients.
           if file-status not = zero move "N" to clpubl.
           if clpubl = "O" move 6 to fjcop
             else          move 0 to fjcop.
      *----> M0999 (F)

      *----> DDE026 (D)
      * la maj de l'entete se fera apres le trt des commentaires
      *----> DDE026 (D)
      *
      ** MAJORATION
       T300.
      *----> M0400 (D)
      * remplacement trt majoration par appel de la fonction mmcd-majo1
      * mise des lignes suivantes en commentaire
      * appel mmcd-majo1 avec code gde classe wtgc (voir si changement multicla)
      * sauf pour dinac
DD0219     if mmdt-societe not = 'GPI'
              go to t350
           end-if

GPICMT* appel gestion type commande pour savoir si traitement majoration
DD0298     initialize wcmpa-tycd
  "        move fjfeo to wcmpa-tycd-typ
  "        move fjok  to wcmpa-tycd-ok
  "        move ccmpa-tycd-gestion-tycd to icmpa-tycd-gestion
  "        call "cmpa-tycd1" using cmpa-tycd adl-art
  "        if ocmpa-tycd-majo = cmmdt-envi-rtn-faux
  "           go to t350
DD0298     end-if

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
      *----> M0400 (F)
       T330.
DD9999     INITIALIZE     wor-fcomjoc4
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
           perform w-fcomjoc4.
           IF file-status not = zero DISPLAY "**FICH. J. 5 a 10 PLEIN**"
                      display "GPIWARNING"
                      MOVE 45 TO WERR  GO TO ERR1.
      *
      ** COMMANDE SUIVANTE
       T350.
           ADD 1 TO gencoent-key.
           IF WFIN = 1  MOVE 9999 TO WRAN1
                   ELSE MOVE gencolig-key TO WRAN1.

      *----> DDE026 (D)
      * recherche et creation des commentaires
           move "C" to wcrecom.
           perform trtcom.

      * maj code commande OK
           if wok = zero move 1 to fjok
           else          move wok to fjok.

GPICMT* pour dinac, blocage systematique des commandes CASTORAMA, a cause du regrouepement des commandes pour le franco et les modifs de prix pour deconditionnement
DD0350     if mmdt-societe = 'DINAC' and fjok  = 1 and wrdi = 'CAS'
  -           move 8 to fjok
DD0350     end-if

GPICMT* on bloque l'entete de la commande allotie mere afin qu'elle ne puisse
GPICMT* pas s'integrer dans le portefeuille de commande sans ligne, les lignes
GPICMT* seront generees a partir des commandes alloties fille validees, lors
GPICMT* de le chaine par le programme cgcd-cral*
DD0298     if fjfeo = ccmpa-tycd-typ-allotie-fille(1:1)                 *GPICMT
  "           if fjok = 1
  "              move 2 to fjok
  "           end-if
DD0298     end-if
DD0298     if fjfeo = ccmpa-tycd-typ-allotie(1:1)                       *GPICMT
  -           move 7 to fjok
DD0298     end-if
DD0177     move alliv to fjlivrea
DD9999     move wtrh to fjtrs

GPICMT* reecriture entete commande allotie avec le nombre total de lignes a livrer
DD0298     if fjfeo = ccmpa-tycd-typ-allotie(1:1)
  "           move wtrh to fjtrs
  "           move fjnum to fjnumr
  "           perform rw-cdesalle
  "           if file-status not = zero
  "              MOVE 29 TO WERR
  "              GO TO ERR1
  "           end-if
DD0298     end-if

GPICMT* ecriture ligne commande allotie fille
DD0298     if fjfeo = ccmpa-tycd-typ-allotie-fille(1:1)
  "   * on charge te transporteur habituel
  "           move wtrh to fjtrs
GPICMT* recherche commande allotie mere
  "           perform rech-allotie
  "           move cae-numcde to fjnumr
GPICMT* mise a jour bloc adresse commande fille avec bloc commande mere
  "           move fjcle to alcle
DD2001*       perform r-fcoadcli
DD2001        perform r-fjoadcli
  "           if file-status not = zero
  "              MOVE 29 TO WERR
  "              GO TO ERR1
  "           end-if
  "           move walfac to alfac
  "           move walreg to alreg
DD2001*       perform rw-fcoadcli
DD2001        perform rw-fjoadcli
  "           if file-status not = zero
  "              MOVE 29 TO WERR
  "              GO TO ERR1
  "           end-if
GPICMT* maj commande allotie mere dans ttfacmst
  "           move fjcle to tscle
  "           perform r-ttfacmst
  "           if file-status = zero
  "              move cae-numcde to tsnum-mere
  "              move cae-numind to tsind-mere
GPICMT* maj du commande par de la mere
  "              move fincl to tsgcp
  "              move w-rleanmag to tscpa
  "              perform rw-ttfacmst
  "              if file-status not = zero
  "                 MOVE 29 TO WERR
  "                 GO TO ERR1
  "              end-if
  "           end-if
  "           initialize wor-cdesalll
  "           move cae-numcde to cal-numcde
  "           move fjnum      to cal-num
  "           move fjnin      to cal-numi
  "   * reference commande client magasin
DD0221*       move fjrcl   to wrcl
DD0221        move fjrcl(1:4)  to wrcl(1:4)
DD0221        move fjrcl(5:15) to wrcl(6:15)
DD0359*       move wrefcli    to cal-refcdecli
DD0359        move wrefcdecli to cal-refcdecli
  "   * Numero de client
  "           move fjncl      to cal-numcli
  "   * groupement client
  "           move wrdi       to cal-grpcli
  "           perform w-cdesalll
  "           if file-status not = zero
  "              MOVE 29 TO WERR
  "              GO TO ERR1
  "           end-if
DD0298     end-if

DD0459*Pour GPI - on bloque les commandes EDI meme si elles sont correct pour detecter 
DD0459* les infocoms par SCOM
DD0459* a terme faire la detections des infocoms ici pour celles qui sont corrects
           if mmdt-societe = "GPI" and fjok = 1
              move 0 to fjok
           end-if

DD9999* anes 14/06/2017
  |   * GPIWARNING 14/06/17 blocage systematique commandes Tapis Saint-Maclou
  |        if wrdi = "MST" and fjok = 1
  |           move 8 to fjok
DD9999     end-if

DD0814* DOOR 10/01/2018 ajout du rayon
DD0814     move wrayon to fjrayon
           perform rw-fcomjoap.
           if file-status not = zero
                    MOVE 29 TO WERR  GO TO ERR1.

DD0351* anes 23/06/16 ajout du libelle correspondant au code ok
  |      move fjnum to lnum
  |      move '0' to cdeind
  |      evaluate fjok 
  |       when 0 string "Commande " fjcle " --> ERR 0 " 
  |                     delimited by size into llib
  |       when 1 string "Commande " fjcle  " --> OK"
  |                     delimited by size into llib
  |       when 2 string "Commande " fjcle  " --> OK FILLE"
  |                     delimited by size into llib
  |       when 8 string "Commande " fjcle  " --> ERR A VAL" 
  |                     delimited by size into llib
  |       when 9 string "Commande " fjcle  " --> A VALIDER "
  |                     delimited by size into llib
  |       when other string "Commande " fjcle  " --> ERREUR"
  |                  delimited by size into llib
  |      end-evaluate
  |      write ligne before 1
DD0351   initialize lnum llib 
 
      * creation dans recap par assistante
      * recherche de l'assistante commerciale
           move fjrep to icgcd-assi-rep.
           move "A"   to icgcd-assi-trt.
           call "cgcd-assi1" using cgcd-assi adl-art.
           move wcgcd-assi-ass to wassist.
DD0221*    move fjrcl   to wrcl
DD0221     move fjrcl(1:4)  to wrcl(1:4)
DD0221     move fjrcl(5:15) to wrcl(6:15)
           move wrefcli to wrecli.
           move fjcle   to wcdegpi.
           move fjncl   to wclien.
           move wrdi    to wtclien.
      * si code ok = 1 ==> commande normale sinon en erreur
           if fjok = 1 move "N" to wtcde
             else     move "E" to wtcde.
           move wtrtcom to wtcom.
DD0316     move wenrass to wor-seqcom12.
           perform w-seqcom1.
           if file-status not = zero display
             "ECRIT. ANREG. ASSISTANTE INVALIDE, STATUS:  " file-status
                                     display "GPIWARNING"
                                     move 45 to werr go to err1.

      *---------------------
      * Ecriture de la trace
      *---------------------
           move space to wmmtr-trac
           move "C"   to immtr-trac-type
           string wnum '0' delimited by size into immtr-trac-num
           move "C"   to immtr-trac-action
           move wnom-prog to immtr-trac-prog
DD0221*    move fjrcl   to wrcl
DD0221     move fjrcl(1:4)  to wrcl(1:4)
DD0221     move fjrcl(5:15) to wrcl(6:15)
           string 'creation par edi: ' fjcle '  ref.client: '
                  wrefcli '  client n: ' fjncl
                      delimited by size into immtr-trac-commentaire
           call 'mmtr-trac1' using mmtr-trac adl-art
      *----> DDE026 (F)
           GO TO T20.
      *

      *----> M0798 (D)
       prix-deb.
           move space to wlib-comp.
      *----> On ne passe dans ce traitement que pour les libelles
      *----> complementaires s'il y en a.

       prix-a10.
           move fjncl         to cscli wcli.
       prix-a11.
           move 01            to csnma.
DDE125     move jfcnar         to csnar.
DDE125     move jfcsre         to csnsr.
           move 00            to csnte.
           perform rnl-cliartsp.
           if file-status not = zero go to prix-a30.
           move cslia to w-lib (1).
           move 2 to wia.

       prix-a20.
           perform nnl-cliartsp.
           if file-status not = zero or
              csnma       not = 01 or
DDE125        csnar       not = jfcnar or
DDE125        csnsr       not = jfcsre or
              cscli       not = wcli
              go to prix-a30.
           move cslia to w-lib (wia).
           move cslib to w-lib (wia + 1).
           add 2 to wia.

      * blocage du nbre  pouvant etre pris a 53
           if wia > 53 go to prix-a30.                                  *DDE032

           go to prix-a20.
       prix-a30.
           exit.
       prix-b30.
           if wlib-comp not = spaces go to prix-fin.

      * si filiation a zero on va voir le client 999999
           if fjnof = zero go to prix-a40.                              *DDE032
           move fjnof to cscli wcli.
           perform prix-a11 thru prix-a30.

      *----> M0898 (D)
           if wlib-comp not = spaces go to prix-fin.

      * si sous filiation a zero on va voir le client 999999
           if wnofn = zero go to prix-a40.                              *DDE032
           move wnofn to cscli wcli.
           perform prix-a11 thru prix-a30.
      *----> M0898 (D)


      *----> DDE032 (D)
      * recherche des commentaires sur client 999999 (comm pour tout client)
       prix-a40.
           if wlib-comp not = spaces go to prix-fin.
           move 999999        to cscli wcli.
           perform prix-a11 thru prix-a30.
      *----> DDE032 (F)

       prix-fin.  exit.
      *----> M0798 (F)

      *----> DDE026 (D)
      * attribution numero de commande
       rechnum section.
       rechnum1.
           move wclcde to icgcd-nume-ncl.
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
DDE069     move RCOGCD to weancomc
DDE069     move RCOGLI to weancom
DDE069     move RCORCC to wcdecom
           go to trtcom1.
      * creation commentaire dans commande du jour
       trtcomc.
      * maj 1er rang du commentaire
           if wrancom = zero move gencocom-key to wrancom.

      * on peut avoir 4 enregistrement a creer pour un commentaire
           if fjnli6 > 9975 display "TROP DE COMMENTAIRES, CDE:  "
                            display "GPIWARNING"
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
             display "GPIWARNING"
             display "fjnli6 :  " fjnli6 move 45 to werr go to ftrtcom.
      * init code commentaire
           move "C" to wtrtcom.
      * init code OK a 9 pour controle commentaire en validation
DD0465*    if wok = zero move 9 to wok.
DD0465     if wok = zero
  -           move 54 to werr
  -           perform perform err1 thru errf
  -           move 9 to wok
DD0465     end-if
           .

       trtcomc-s.
           if wind < 4 add 1 to wind go to trtcomc-d.
      * commentaire suivant
           go to trtcoma.
       trtcomv.
DD0298     move wor-gencocom to wor-errcom.
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
           move "1"          to icgcd-annu-maj.
           call "cgcd-annu1" using cgcd-annu adl-art.
           if ocgcd-annu-rtn = "3"
             display "GPIWARNING"
             display ocgcd-annu-err go to fin.
      *----> DDE026 (F)

       erreur section.
       ERRAP.
           display "GPIWARNING"
           IF file-status = "24" DISPLAY "***FICHIER COMMANDE PLEIN***"
                      MOVE 45 TO WERR  GO TO ERR1.
           DISPLAY "***ERREUR CREATION COMMANDE***".
           MOVE 30 TO WERR.
           GO TO ERR1.
       ERRNL.
           display "GPIWARNING"
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
GPICMT* on ne bloque pas la commande si le pcb client est un multiple de PCB
GPICMT* pour auchan la plupart sont des multiples de PCB
           if werr = 38
              if wpcb-dec not = zero
                 move 8    to wok
              end-if
           else
DD0351* Cas particulier de l'erreur 60 pour laquelle on doit afficher
  |   * "A VALIDER" (ok=9) au lieu de "ERR A VALIDER" (ok=8).
  |   * ATTENTION ok=8 est prioritaire ! Donc on ne peut dire ok=9 que si ok n'a
  |   * pas encore ete charge. Dans tous les autres cas : ok=8
  |          if werr = 60  
  |            if wok = zero
  |              move 9 to wok 
  |            end-if 
DD0351       else
               move 8    to wok
DD0351       end-if
           end-if

DD0351*    IF WLIG > 60  PERFORM TIT.
DD0420*    MOVE RENNCL TO LNCL.
GPICMT* si erreur sur etat client on edite le client clncl sinon on edite le client livre
DD0420     if werr = 34
              MOVE clncl TO LNCL
           else
DD0465*       move wclient to lncl
DD0465        move w-alcde to lncl
           end-if
DD0465* si ean non trouve on ne met pas de numero client
DD0465     if werr = 49
  -           move zero to LNCL
DD0465     end-if
           MOVE RENRCC TO LRCC.
           MOVE RENJJ  TO LLJJ.
           MOVE RENMM  TO LLMM.
           MOVE RENAA  TO LLAA.
           MOVE "/"    TO LS1 LS2.

      * edition systematique du numero de commande
           MOVE WNUM   TO LNUM.                                         *DDE026
           move '0' to cdeind

DD0351*anes 18/5/16 edition des messages retour de mgcd-vean1
  |   * erreur sur retour mgcd-vean1
  |         if werr = 59 or werr = 60
  |           go to err3
DD0351      end-if.

           IF WERR = 23 OR WERR = 24 OR WERR = 25 or werr = 37          *DDE079
DDE171                  or = 38
DD0412                  or = 48
DD0465                  or = 53
                                                  MOVE RLIXAR TO LART
                                                  GO TO ERR2.
           IF WERR > 20 GO TO ERR2.
           IF WERR = 7 OR WERR = 9  MOVE RLIXAR TO LART.
       ERR2.
      *----> DDE025 (D)
           if werr = 36 move "MULTICLASSE NON AUTORISE" to llib
                        perform err3
                        go to errf.
      *----> DDE025 (F)

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
DDE045* recalcul fin de commande en erreur ==> a verifier
DDE045*   if werr = 16 move '-- COMMANDE A VERIFIER EN MISE A JOUR'
DD0351    if werr = 16 move ocgca-mtht-liberr
DDE045                 TO LLIB  GO TO ERR3.

      *----> M1297 (D)
           if werr = 18 MOVE "--ECRITURE TTFACMST IMPOSSIBLE" to llib
                        go to err3.
           IF WERR = 19 MOVE "--FILIERE COMMANDE PAR INEXISTANTE"
               TO LLIB  GO TO ERR3.
      *----> M1297 (F)
DD0777     if werr = 56
  |          string "Filiere (EAN) du client final "
  |                reneanclfin " inconnue"
  |          delimited by size into llib
  |          go to err3
  |        end-if
  |        if werr = 57
  |          string "Numero client du client final "
  |                fincl " inconnu"
  |          delimited by size into llib
  |          go to err3
DD0777     end-if

      *----> M0598 (D)
           IF WERR = 20 MOVE "-- EAN LIVRE DIFFERENT EAN CDE --"
               TO LLIB  GO TO ERR3.
      *----> M0598 (F)
           IF WERR = 21 MOVE "--TYPE DE COMMANDE ANORMAL         -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
           IF WERR = 22 MOVE "--CLIENT INEXISTANT OU BLOQUE      -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.
      * on ne met plus les cde avec article inconnu dans le fichier des ERReurs
      * car on cree une ligne article 9999999
           IF WERR = 23 MOVE "--ARTICLE NON TROUVE" to llib go to err3. *DDE026

      * utilisation code 24 pour article supprime
      * l'article sera cree ds la cde avec code article 999999
           IF WERR = 24                                                 *DDE171
              string "--ARTICLE SUPPRIME--     " vlib                   *DDE171
              delimited by size into llib                               *DDE171
              go to err3
           END-IF

      * on ne met plus les commandes avec taxe article invalid dans le fichier
      * des erreurs il faudra modifier la ligne
           IF WERR = 25 MOVE "--CODE TAXE/PARA-FISC. DIFFERENT" to llib *DDE026
                                GO TO ERR3.                             *DDE026

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
      *----> M0598 (D)
DD0420*    IF WERR = 33 MOVE "--FILIERE COMMANDE PAR INEXISTANT  -> CDE
DD0420*-    "EN ERREUR" TO LLIB  GO TO ERR3.
DD0420     IF WERR = 33 string "--FILIERE COMMANDE PAR " fiean " INEXIST
DD0420-    "ANT  -> CDE EN RREUR"
DD0420          delimited size into LLIB  GO TO ERR3.
      *----> M0598 (F)
DD0400     IF WERR = 34 MOVE "--CODE ETAT CLIENT NON VALIDE      -> CDE
      -    "EN ERREUR" TO LLIB  GO TO ERR3.

      * pour info erreur 36 uitilise voir tag ERR1

      * DDE079 plusieurs ref pour un gencod
           IF WERR = 37
              string fanar1 " " fansr1  " EAN14: " fagean14
                            "   -PLUSIEURS REFERENCES"
                   delimited size into llib
              go to err3.

DDE171* PCB non correspondant
           if werr = 38
              string "--PCB #  -GPI : " faqpb " Recu : " rlipcb
                     " -> CDE EN ERREUR"
              delimited by size into llib
              go to err3
           end-if

DD0412* Quantite arrondi au pcb inferieur ou superieur
           if werr = 48
              string "Quantite arrondie : " wqte-ori " ==> " wqte-arr
                     " -> CDE EN ERREUR"
              delimited by size into llib
              go to err3
           end-if

DDE171* H.T. total cde different
           if werr = 39
              move wmont to wmontv
              move renhtcde to renhtcdev
              string "TOTAL H.T. CDE " wmontv "   TOTAL H.T. RECU "
              renhtcdev " NON CORRESPONDANT  -> CDE EN ERREUR"
              delimited by size into llib
              go to err3
           end-if

DD0180* Creation donnes specifiques client invalide
           if werr = 40
              string ocmcd-majc-liberr " RTN " ocmcd-majc-rtn
                          delimited size into llib
              go to err3
           end-if

DD0351* Code reglement incorrect
  -        if werr = 47
  -           string ommpa-regl-liberr " RTN " ommpa-regl-rtn
  -                       delimited size into llib
  -           go to err3
DD0351     end-if

DD0420* code ean non trouve
  -        if werr = 49
  -           string 'Code ean client ' rengli ' non trouve'
  -                       delimited size into llib
  -           go to err3
DD0420     end-if

DD0420* commande deja cree
  -        if werr = 50
  -           string 'Commande ' LFCCLE-CDESUP ' deja cree dans SCOM'
  -                       delimited size into llib
  -           go to err3
  -        end-if
  -        if werr = 51
  -           string 'Commande ' LFCCLE-CDESUP ' deja cree dans SLIV'
  -                       delimited size into llib
  -           go to err3
DD0351     end-if
GPICMT* erreur filieres sur commande fille
DD0351     if werr = 52
  -           string 'Filiere Fille Inexistante ' fiean
  -                       delimited size into llib
  -           go to err3
DD0351     end-if

GPICMT* prix client different de plus de 1 centime
DD0465     if werr = 53
  -           string 'Prix DIFF Client ' wprxcli
  -                  ' societe ' wprxnet
  -                 delimited size into llib
  -           go to err3
DD0465     end-if

GPICMT* controle commentaire
DD0465     if werr = 54
  -           move 'COMMENTAIRES' TO llib
  -           GO TO ERR3
DD0465     END-IF

GPICMT* erreur code taxe
DD0448     if werr = 55
  -           move 'Code taxe/pays invalide'  to llib
  -           go to err3
DD0448     end-if

           MOVE 1 TO TTFIN.
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
           if werr = 23 or = 24 or = 25 or = 37 or = 53
              move RLIXAR to wrlixar
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
DD0221*    move fjrcl   to wrcl
DD0221     move fjrcl(1:4)  to wrcl(1:4)
DD0221     move fjrcl(5:15) to wrcl(6:15)
           string 'EDI,ref.clt: ' wrefcli
                  '   ' wrlixar '  ' llib 
                      delimited by size into immtr-trac-commentaire
           call 'mmtr-trac1' using mmtr-trac adl-art
      *----> DDE026 (F)
           MOVE SPACES TO LIGNE.
DD9999     MOVE '|' TO SEP1 SEP2 SEP3 SEP4 SEP5 SEP6 
DD0351*    ADD 1 TO WLIG.
         .
       ERR4.

      *----> DDE026 (D)
      * pour les codes erreurs 23/25 article inconnu ou taxe incorrect
      *                        24 article supprime
      * on cree les lignes avec reference 999999 et on ne transfert plus la
      * commande dans le fichier des erreurs
      *le 08/09/00 elgu ajout code 24 pour article supprime idem 23/25
DDE171* + erreur 38/39/40/48 non blocantes
           if werr = 38
DD0351     or werr = 59 or werr = 60
              MOVE ZERO TO WERR
              GO TO ERRF
           end-if
           IF WERR NOT > 20  or werr = 23 or werr = 25 or werr = 24     *DDE053
                             or werr = 37                               *DDE079
DDE171*                      or werr = 38 or = 39 or = 40
DDE171                       or werr = 39 or = 40
DD0351                       or werr = 47
DD0412                       or werr = 48
DD0400                       or werr = 34
DD0351                       or werr = 52
DD0465                       or werr = 53
DD0465                       or werr = 54
DD0448                       or werr = 55
                             move 8    to wok
                             MOVE ZERO TO WERR  GO TO ERRF.
      *----> DDE026 (F)

      ** CDE TRANSFEREE -> FICHIER DES ERREURS

      * verification si cde cree ==> si oui ==> suppression de la commande
      * pour ne pas avoir la commande a la fois dans le .core et ds las cdes
      * du jour
           if wecree = 1 perform annul.                                 *DDE026
      *----> M0598 (D)
           MOVE RENGCD TO WGCD.
      *----> M0598 (F)
           MOVE RENGLI TO WGLI.
           MOVE RENRCC TO WRCC.
       ERR5.
           IF WERR NOT = 32  MOVE WRAN1 TO gencolig-key
                             MOVE ZERO TO WLEC
                             GO TO ERR7.
      *----> M0598 (D)
           if wgcd > rligcd  go to err10.
DD0351     if wgcd < rligcd  go to err7.
      *----> M0598 (F)
           IF WLC > RLILC    GO TO ERR10.
       ERR7.
           move wor-gencoent to wor-errent                              *DDE171
           perform w-errent.
DD0351* MICN - je ne sais pas a quoi sert ce fichier d'erreur
DD0351* mais ne pas bloquer l'integration si il se trouve plein
      *    IF file-status NOT = "00"  GO TO ERRSO.

      *----> DDE026 (D)
      * creation des commentaires en erreur
      * si les commentaires ont deja ete lu on se repositionne sur le 1er rang
           if wrancom = zero go to err7-s.
           move wrancom to gencocom-key.
           perform r-gencocom.
           if file-status not = zero go to err8.
      * memo ds cle commentaire
DDE069     move RCOGCD to weancomc
DDE069     move RCOGLI to weancom
DDE069     move RCORCC to wcdecom
           .
       err7-s.
           move "E" to wcrecom.
           perform trtcom.
       err8.
      *----> DDE026 (F)

           IF WERR = 32        GO TO ERR14.
           IF WFIN = 1         GO TO ERR14.
           IF WLEC = 1         GO TO ERR10.
       ERR9.
           perform r-gencolig.
           if file-status not = zero
                     MOVE 1 TO WFIN
                               GO TO ERR12.
      *----> M0598 (D)
           if wgcd not = rligcd  go to err12.
      *----> M0598 (F)
           IF RLILC NOT = WLC  GO TO ERR12.
           IF WERR = 32        GO TO ERR12.
       ERR10.
           move wor-gencolig to wor-errlig                              *DDE171
           perform w-errlig.
DD0351* MICN - je ne sais pas a quoi sert ce fichier d'erreur
DD0351* mais ne pas bloquer l'integration si il se trouve plein
      *    IF file-status NOT = "00"  GO TO ERRSO.
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
           perform r-gencoent.
           if file-status not = zero
                     GO TO FIN.

      * init num cde
           move zero to wnum.                                           *DDE026

       ERR18.
           MOVE ZERO TO WERR.
           IF WFIN = 1         GO TO ERR7.
      *----> M0598 (D)
           MOVE RENGCD TO WGCD.
      *----> M0598 (F)
           MOVE RENGLI TO WGLI.
           MOVE RENRCC TO WRCC.
      *----> M0598 (D)
           if wgcd > rligcd  move 32 to werr go to err10.
           if wgcd < rligcd  move 32 to werr go to err7.
      *----> M0598 (F)
           IF WLC > RLILC  MOVE 32 TO WERR  GO TO ERR10.
           IF WLC < RLILC  MOVE 32 TO WERR.
           GO TO ERR7.
       ERRF.
           EXIT.
       TIT.
DD0351*    MOVE SPACES TO LIGNE.
DD0351*    MOVE '|' TO SEP1 SEP2 SEP3 SEP4 SEP5 SEP6 
GPICMT* ne pas faire le 1er saut de page
DD9999*  if wlig not = 90
DD9999*    WRITE LIGNE BEFORE PAGE
DD9999*  end-if
DD0351*    MOVE "ERREURS EN CREATION AUTOMATIQUE DES COMMANDES ALLEGRO"
DD0351*                  TO LLIB.
DD0351*    MOVE WJ  TO LLJJ.
DD0351*    MOVE WM  TO LLMM.
DD0351*    MOVE WA  TO LLAA.
DD0351*    MOVE "/" TO LS1 LS2.
DD9999*    WRITE LIGNE BEFORE 3.
DD9999*    MOVE SPACES TO LIGNE.
DD9999*    MOVE "CLIENT    REF.CDE           CDE GPI    DATE LIVR.      
DD9999*    "ARTICLE                    E R R E U R S" TO LIGNE.
DD9999*    WRITE LIGNE BEFORE 2.
DD0351*    MOVE SPACES TO LIGNE.
DD0351*    MOVE '|' TO SEP1 SEP2 SEP3 SEP4 SEP5 SEP6 
DD0351*    MOVE 5 TO WLIG.
      *
        .
       ERRSO.
           display "GPIWARNING"
           DISPLAY "***FICHIER ERREURS PLEIN***".
           MOVE "*** FICHIER ERREURS PLEIN ***" TO LLIB.
           PERFORM ERR3.
      *
       FIN section.
DD0298     perform cl-gencoale.
DD0298     perform cl-gencoall.
DD0298     perform cl-gencoalc.
DD0298     perform cl-cdesalle.
DD0298     perform cl-cdesalll.
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

           perform cl-fcomjoc6.                                         *DDE026
           perform cl-fcomjoc5.                                         *DDE026
           perform cl-errcom.                                           *DDE026
           perform cl-seqcom1.                                          *DDE026

           perform cl-paramgpi.
           perform cl-errent.
           perform cl-errlig.

      *----> M1197 (D)
           perform cl-guextmst.
           perform cl-ttfacmst.
           perform cl-filieres.
      *----> M1197 (F)
           CLOSE ETAT.
           STOP RUN.

      *=========================================================================
      *                             FONCTIONS LOCALES
      *=========================================================================

      *DD0298 affichage fenetre d'erreur
       aff-erreur section.
           move wnom-prog to immaf-vali-pgm
           move "V=Validation" to immaf-vali-act
           move "V" to wmmaf-vali-trt
           move "B" to immaf-vali-pos
GPICMT* affichage message erreur avec demande de reponse
GPICMT     move "#" to wmmaf-vali-trt.
GPICMT     call 'mmaf-vali1' using mmaf-vali adl-art.


      * recherche tarif
       rech-tarif section.
           move "1" to icgta-rech-typqd
           move "3" to icgta-rech-typqf
           move " " to icgta-rech-trech
DDE125     move jfcqtc to icgta-rech-q
DDE069* conversion en nbre de blisters
GPICMT*GPIWARNING pour l instant on considere le quantitatif tarif dans l unite
GPICMT*GPIWARNING de saisie de commande (un/dinac, bte/gpi)
           move 1    to  icgta-rech-m
      *DDE153 on appelle la recherche tarif avec la date de reference tarif
      *       ici identique a la date de creation de la commande
           move fjdatetarif9 to icgta-rech-datec
           move spaces to icgta-rech-lieu
DDE125     move jfcnar  to icgta-rech-narx
DDE125     move jfcsre  to icgta-rech-srfx
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
           call 'cgta-rech1' using cgta-rech adl-art
           if ocgta-rech-rtn not = '0'
              move 7 to werr
              go to rech-tarif-fin
           end-if
           move ocgta-rech-topx to td-top-prix
DDE125     move ocgta-rech-pbas to jfcpbas
DDE125     move ocgta-rech-poub to jfctrpv
DDE125     move ocgta-rech-net to jfcpht jfcpcl
DDE125     move ocgta-rech-brut to jfcpth.
       rech-tarif-fin.
           exit
           .

      * controle devise
       rech-devise section.
           move "C"                 to immpa-devi-tfc
           move "f"                 to immpa-devi-trt
DDE089     move ' '                 to immpa-devi-aff
           move fjdev               to wmmpa-devi-cdev9
           move spaces              to wmmpa-devi-cdev
           call 'mmpa-devi1' using mmpa-devi adl-art
           .

      *-----------------------------------------
GPICMT* creation entete commande allotie
DD0298 entete-allotie section.
      *-----------------------------------------
           initialize wor-cdesalle
           move fjnum   to cae-numcde
DD0221*    move fjrcl   to wrcl
DD0221     move fjrcl(1:4)  to wrcl(1:4)
DD0221     move fjrcl(5:15) to wrcl(6:15)
DD0359*    move wrefcli to cae-refcdecli
DD0359     move wrefcdecli to cae-refcdecli
           move fjncl   to cae-numcli
           move wrdi    to cae-grpcli
           perform w-cdesalle
           if file-status not = zero
              string 'Cde Allotie ' fjnum ' non cree,(' file-status ')'
                    delimited size into immaf-vali-tit
              perform aff-erreur
              move 45 to werr
           end-if
           perform r-cdesalle
           move spaces to w-rlrefmag
           .
      *-----------------------------------------
GPICMT* creation ligne commande allotie fille
DD0298 ligne-allotie section.
      *-----------------------------------------
GPICMT* ligne allotie fille si cle differente de la precedente
GPICMT* il faut creer une entete de commande magasin
GPICMT     if rlrefmag not = w-rlrefmag
              move wor-gencoent to wor-gencoale
              move rlrefcdemag to ap-renrcc
              move rleanmag    to ap-rengcd fiean
              perform rnl-filieres
              if file-status not = zero
                 move 52 to werr
                 perform err1 thru errf
              end-if
              add 1 to gencoale-key
              perform w-gencoale
              if file-status not = zero
                 string 'Cde Allotie ' fjnum ' non cree,('
                     file-status ')'
                    delimited size into immaf-vali-tit
                 perform aff-erreur
                 move 45 to werr
              end-if
DD0298        add 1 to cae-nbrfil
           end-if
GPICMT* on copie la ligne article dans gencoalll en invertissant les donnees
GPICMT* ean client et ref commande du magasin et de la plateforme
           move wor-gencolig to wor-gencoall
GPICMT* donnees magasin
GPICMT     move rlrefcdemag  to al-rlircc
GPICMT     move rleanmag to al-rligcd
GPICMT* donnees allotie mere
GPICMT     move rlircc to al-rlrefcdemag
GPICMT     move rligcd to al-rleanmag
           add 1 to gencoall-key
           perform w-gencoall
           if file-status not = zero
              string 'Cde Allotie ' fjnum ' non cree,('
                    file-status ')'
                    delimited size into immaf-vali-tit
              perform aff-erreur
              move 45 to werr
           end-if
           add 1 to cae-nbrlig
           .

      *-----------------------------------------
GPICMT* recheche commande allotie mere
       rech-allotie section.
      *-----------------------------------------
           move w-rleanmag to fiean.
           perform rnl-filieres.
           if file-status not = zero
              string 'Code EAN ' rleanmag ' Inexistant '
                  delimited size into immaf-vali-tit
              perform aff-erreur
              move 45 to werr
           else
              move fincl to  cae-numcli
              move w-rlrefcdemag to cae-refcdecli
              perform rnlsk1-cdesalle
              if file-status not = zero
                 string 'Entete Allotie ' rlrefcdemag ' client ' fincl
                        ' inexistante'
                     delimited size into immaf-vali-tit
                 perform aff-erreur
                 move 45 to werr
              end-if
           end-if
GPICMT* lecture bloc adresse de la commande allotie mere pour mise a jour
GPICMT* client facture et regle de la commande fille
           move zero to alcle
           move cae-numcde to alnum
DD2001*    perform rnl-fcoadcli
DD2001     perform rnl-fjoadcli
           if file-status not = zero
              move spaces to wwor-fcoadcli2
           else
              move wor-fcoadcli2 to wwor-fcoadcli2
           end-if
           .

GPICMT* constuction reference commande client sur 8 car. si 9eme car non vide
       cal-refcli section.
           move renrcc to wrenccx
           if wrencc(9) = spaces
              move renrcc to wrenccx-new
              go to cal-refcli-f
           end-if
           move all zero to wrenccx-new
           move 15 to wi
           move 8 to wz
           .
       cal-refcli-1.
           if wj = 0 or wz = 0
              go to cal-refcli-f
           end-if
           if wrencc(wi) not = spaces
              move wrencc(wi) to wrencc-new(wz)
              subtract 1 from wz
           end-if
           subtract 1 from wi
           go to cal-refcli-1
           .
       cal-refcli-f.
           move wrenccx-new to wrefcli
           exit.

GPUCMT* controle existence commande client avec meme ref dans commande jour
GPICMT ctrl-doublon-j section.
           move ccmcd-lect-e1type-j to wcmcd-lect-e1type
           move 1 to icmcd-lect-raz
           move ccmcd-lect-trt-f to icmcd-lect-trt
DD0221*    move fjrcl(6:8) to wcmcd-lect-e1refccli
DD0221     move fjrcl(5:15) to wcmcd-lect-e1refccli
           .
       ctrl-doublon-j-a.
           call 'cmcd-lect1' using cmcd-lect adl-art
           if ocmcd-lect-rtn not = cmmdt-envi-rtn-ok
              go to  ctrl-doublon-j-f
           end-if
           if lFCDLI = fjdli and lfcncl = fjncl
              move cmmdt-envi-rtn-ok to ocmcd-lect-rtn
              go to ctrl-doublon-j-f
           else
              go to ctrl-doublon-j-a
           end-if
           .
       ctrl-doublon-j-f.
           exit.


GPUCMT* controle existence commande client avec meme ref dans commandes protefeuille
GPICMT ctrl-doublon-p section.
           move ccmcd-lect-e1type-e to wcmcd-lect-e1type
           move 1 to icmcd-lect-raz
           move ccmcd-lect-trt-f to icmcd-lect-trt
DD0221*    move fjrcl(6:8) to wcmcd-lect-e1refccli
DD0221     move fjrcl(5:15) to wcmcd-lect-e1refccli
           .
       ctrl-doublon-p-a.
           call 'cmcd-lect1' using cmcd-lect adl-art
           if ocmcd-lect-rtn not = cmmdt-envi-rtn-ok
              go to  ctrl-doublon-p-f
           end-if
           if lFCDLI = fjdli and lfcncl = fjncl
              move cmmdt-envi-rtn-ok to ocmcd-lect-rtn
              go to ctrl-doublon-p-f
           else
              go to ctrl-doublon-p-a
           end-if
           .
       ctrl-doublon-p-f.
           exit.

GPICMT* controle prix ligne
DD0465 ctrl-prix section.
           if ocgta-rech-net not = RLIPHT 
              subtract RLIPHT from ocgta-rech-net giving wabsolu
              if wabsolu > 0.01
                 move RLIPHT TO WPRXCLI
                 MOVE ocgta-rech-net TO WPRXNET
                 MOVE 53 TO WERR
                 perform err1 thru errf
              end-if
            end-if
            .
DD0448 ctrl-taxe section.
           move fjgeo to wmmpa-vtax-cgeo
           move fjdi2 to wmmpa-vtax-vtax
           call 'mmpa-vtax1' using mmpa-vtax adl-art
           if ommpa-vtax-rtn not = cmmdt-envi-rtn-ok   
             move 55 to werr
             perform err1 thru errf
           end-if

GPICMT*-----------------------------------------------
GPICMT*     Envoi de mail
GPICMT*-----------------------------------------------
         .
DD0800 env-mail section.
         initialize immlp-mail-ligx

         move cmmlp-mail-type-oo to immlp-mail-type(1)
         move "PRCDE060 - Erreur su rcommandes magiques "
             to immlp-mail-data(1)

         move cmmlp-mail-type-o      to immlp-mail-type(2)
DD0732   move wlibelle-erreur to immlp-mail-data(2)
         move spaces to wlibelle-erreur immlp-mail-destg
                        immlp-mail-groupe

         move cmmlp-mail-trt-notif     to immlp-mail-trt
         initialize immlp-mail-objet
         string "Anomalie(s) PRCDE060 "
                delimited by size into immlp-mail-objet

GPICMT*---- Appel fonct. recherche des destinataires de mail
         move spaces to immpa-mail-type     immpa-mail-sect
                        immpa-mail-lieusect immpa-mail-lieu
         call 'mmpa-mail1' using mmpa-mail adl-art
         move ommpa-mail-dest to immlp-mail-dest

         move wnom-prog       to immlp-mail-pgm
         call 'mmlp-mail1' using mmlp-mail adl-art
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
           copy "../copy/pro-fcommaap-cdesup".
           copy "../copy/pro-errent".
           copy "../copy/pro-errlig".

      *----> M1197 (D)
           copy '../copy/pro-guextmst'.
           copy '../copy/pro-ttfacmst-cdesup'.
           copy '../copy/pro-filieres'.
      *----> M1197 (F)

      *----> M0798 (D)
           copy '../copy/pro-fcomjoc3'.
           copy '../copy/pro-cliartsp'.
      *----> M0798 (F)

           copy '../copy/pro-avoircli'.                                 *DDE026
           copy '../copy/pro-fcomjoc6'.                                 *DDE026
           copy '../copy/pro-fcomjoc5'.                                 *DDE026
           copy '../copy/pro-errcom'.                                   *DDE026
           copy '../copy/pro-seqcom1'.                                  *DDE026
           copy '../copy/pro-gencocom'.                                 *DDE026
DD0298     copy "../copy/pro-gencoale".
DD0298     copy "../copy/pro-gencoall".
DD0298     copy "../copy/pro-gencoalc".
DD0298     copy "../copy/pro-cdesalle".
DD0298     copy "../copy/pro-cdesalll".
DD0800     copy "../copy/pro-foucmagi".
DD0814     copy "../copy/pro-refercli".
