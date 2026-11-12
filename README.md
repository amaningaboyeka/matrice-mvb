# Matrice de risque MVB — appui à la surveillance ORACLE (Bunia, Butembo)

Classement automatisé des 70 zones de santé de l'Ituri et du Nord-Kivu selon le risque de
maladie à virus Bundibugyo, avec dynamique, projections à 4 semaines et alerte précoce autour
des sites ORACLE de Bunia et de Butembo.

**Auteur** : Dr Gaylord Amani Ngaboyeka, MD, MPH, PhD — co-investigateur et responsable de la
qualité des données, projet ORACLE BDBV ; École Régionale de Santé Publique, Université
Catholique de Bukavu (ERSP/UCB). Contact : amani.ngaboyeka@ucbukavu.ac.cd

La page est **recalculée chaque jour à 7 h (heure de Bukavu)** et publiée à une adresse publique
du type `https://<votre-identifiant>.github.io/matrice-mvb/`. Elle contient aussi un PDF et les
tableaux en CSV.

---

## Mise en ligne (une seule fois, environ 15 minutes)

1. **Créer un compte GitHub** gratuit sur <https://github.com/signup> (adresse ERSP conseillée).
2. **Créer le dépôt** : bouton **New repository**, nom `matrice-mvb`, visibilité **Public**,
   puis **Create repository**.
3. **Déposer les fichiers** : sur la page du dépôt, **Add file → Upload files**, glisser
   tout le contenu du dossier `matrice-mvb` (y compris le dossier `.github`), puis
   **Commit changes**.
   *Astuce : si le navigateur n'affiche pas le dossier `.github` (fichier caché), créez-le à la
   main : **Add file → Create new file**, nom `.github/workflows/mise_a_jour.yml`, et collez le
   contenu du fichier.*
4. **Activer la page publique** : **Settings → Pages → Build and deployment → Source :
   GitHub Actions**.
5. **(Facultatif) Brancher ACLED** : **Settings → Secrets and variables → Actions → New
   repository secret**, créer `ACLED_EMAIL` (adresse de votre compte myACLED) et
   `ACLED_PASSWORD` (mot de passe). Ils restent secrets et ne sont jamais publiés.
   Compte gratuit : <https://acleddata.com/user/register>.
6. **Première mise à jour** : onglet **Actions → Mise à jour de la matrice → Run workflow**.
   Au bout de 5 à 10 minutes, l'adresse publique apparaît dans **Settings → Pages**.

Ensuite, tout est automatique.

## Ce que l'équipe met à jour (directement sur GitHub, sans programmer)

| Fichier | Contenu | Qui |
|---|---|---|
| `donnees_terrain/securite_terrain.csv` | Accès par zone de santé : `Accessible`, `Partiellement accessible`, `Inaccessible` ou `Non renseigné`, avec date, source et commentaire | Équipes de terrain ORACLE |
| `donnees_terrain/zones_oracle.csv` | Zones d'intervention ORACLE, site et statut (`Intervention en cours`, `En préparation`) | Coordination |

Pour modifier : ouvrir le fichier sur GitHub, cliquer sur le crayon, changer la ligne, puis
**Commit changes**. La page est recalculée automatiquement dans les minutes qui suivent.

## Contenu du dépôt

| Dossier | Rôle |
|---|---|
| `R/01_telecharger.R` | Télécharge les données publiques INSP/INRB-UMIE et, si configuré, les incidents ACLED |
| `R/02_analyse.R` | Calcule les indicateurs, la matrice, les deux listes opérationnelles, le Rt et les projections |
| `outils/construire_page.py` | Produit la page web, le PDF et les CSV dans `docs/` |
| `modele/page.html` | Mise en page du tableau de bord |
| `.github/workflows/mise_a_jour.yml` | Automatisation quotidienne et publication |

## Méthode (résumé)

- **Probabilité** : cas des 21 derniers jours, incidence, tendance et pression d'importation
  (cas récents des zones voisines pondérés par le temps de route).
- **Impact** : densité de population, accès aux formations sanitaires, privation
  socio-économique, létalité récente et insécurité (ACLED + avis du terrain).
- **Zones à surveiller** : zones sans cas depuis 21 jours, alerte si la pression
  d'importation est dans le quintile supérieur. Validation juin–octobre 2026 : AUC 0,76.
- **Zones actives à renforcer** : classées par cas récents (Spearman 0,90 avec les cas des
  21 jours suivants).
- **Projections à 4 semaines** : quasi-Poisson avec marche aléatoire sur le taux de
  croissance ; couverture rétrospective de 80 % pour l'intervalle à 80 %.

## Sources et licences

- Rapports de situation de l'INSP, transcrits par l'INRB-UMIE :
  <https://github.com/INRB-UMIE/Ebola_DRC_2026>
- WorldPop, GRID3, CCVI, OSRM (même dépôt).
- ACLED (Armed Conflict Location & Event Data), selon les conditions d'utilisation ACLED.

Citer : Amani Ngaboyeka G. *Matrice de risque MVB — appui à la surveillance ORACLE, Ituri et
Nord-Kivu.* ERSP/UCB, 2026.
