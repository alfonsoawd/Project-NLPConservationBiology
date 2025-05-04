# 📊 Data used in the project

------------------------------------------------------------------------

## 🇫🇷 Version Française

### Source principale

Les données utilisées dans ce projet proviennent de l’article de référence de Van Houtan et al. (2020), qui a collecté **4 313 résumés scientifiques** liés aux réintroductions d’espèces publiés entre 1987 et 2016.

📥 Base de données originale disponible ici :\
<https://github.com/MBayOtolith/nlp_conservation_lit_review_2018>

Le fichier de base `1- data.csv` contient tous les résumés, ainsi que certaines métadonnées fournies par les auteurs.

### Autres données

-   📍 **Shapefile mondial** (limites administratives) utilisé pour la visualisation géographique des pays mentionnés dans les résumés :\
    <https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/>

-   🗺 `country_mapping.csv` : Table de correspondance manuelle entre noms géographiques ambigus et pays normalisés, utilisée dans le pipeline de géocodage.

-   📁 Dossiers `2- data_with_info` et `3- data_with_score` : fichiers générés automatiquement à chaque étape du pipeline (informations extraites et scores de sentiment).

### Structure du dossier

```         
├── 1- data.csv <- Données brutes (résumés scientifiques)
├── 2- data_with_info <- Résumés avec taxonomie, UICN, pays extraits
├── 3- data_with_score <- Résumés avec scores de sentiment
├── country_mapping.csv <- Table de correspondance pays
└── World Shapefile/ <- Limites administratives des pays (format .shp)
```

------------------------------------------------------------------------

## 🇬🇧 English Version

### Main Data Source

The dataset used in this project was provided by Van Houtan et al. (2020), who compiled **4,313 scientific abstracts** related to species reintroductions published between 1987 and 2016.

📥 Original dataset available here:\
<https://github.com/MBayOtolith/nlp_conservation_lit_review_2018>

The base file `1- data.csv` contains all abstracts, along with basic metadata provided by the authors.

### Additional Files

-   📍 **World shapefile** (administrative boundaries) used for mapping the countries mentioned in the abstracts:\
    <https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/information/>

-   🗺 `country_mapping.csv`: Manual mapping table to standardize ambiguous location names during geographic extraction.

-   📁 Folders `2- data_with_info` and `3- data_with_score`: intermediate and final datasets generated during the pipeline (with extracted information and sentiment scores).

### Folder Structure

```         
├── 1- data.csv <- Raw abstracts and metadata
├── 2- data_with_info <- Abstracts with taxonomy, IUCN, and countries
├── 3- data_with_score <- Abstracts with sentiment scores
├── country_mapping.csv <- Location name harmonization table
└── World Shapefile/ <- Country boundaries shapefiles (.shp)
```
