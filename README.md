# 🐾 *Sentiment and Structure in Reintroduction Science: A Meta-Analysis Using NLP Tools*

------------------------------------------------------------------------

## 🇫🇷 Version Française

### 🎯 Objectif du Projet

Ce projet applique des outils de traitement du langage naturel (NLP) à plus de 4 000 résumés scientifiques portant sur les réintroductions d’espèces entre 1987 et 2016. Il vise à extraire des informations structurées (taxonomie, pays, statut UICN) et à estimer le ton émotionnel des textes via une analyse de sentiment.

### 🔑 Points Clés

-   **Données :** Corpus de résumés extrait de l’étude de Van Houtan et al. (2020)
-   **Extraction :** Pipeline pour récupérer taxons, catégories UICN et pays mentionnés
-   **Sentiment :** Score composite basé sur cinq modèles d’analyse de sentiment
-   **Résultats :** Biais géographiques et taxonomiques, surreprésentation des pays à hauts revenus et des espèces peu menacées

### 📁 Structure du Projet

```         
├── README.md                             <- Documentation principale du projet
│
├── data                                            
│   ├── 1- data.csv                       <- Données brutes
│   ├── 2- data_with_info                 <- Données enrichies (pays, taxons, UICN)
│   ├── 3- data_with_score                <- Données finales avec scores de sentiment
│   ├── country_mapping.csv               <- Table de correspondance pays
│   └── World Shapefile                   <- Shapefile utilisés pour les cartes
│ 
├── src                                             
│   ├── 01- Information Extraction.R      <- Script d'extraction pays, taxons, catégorie UICN
│   ├── 02- Sentiment Analysis.R          <- Script de calcul des scores de sentiment
│   ├── 03- Analysis & Figures.R          <- Script de création des graphiques d’analyse
│
├── reports                                         
│   ├── Final Report.pdf                  <- Rapport final au format PDF
│   ├── Figures                           <- Figures insérées dans le rapport
│
```

------------------------------------------------------------------------

## 🇬🇧 English Version

### 🎯 Project Objective

This project applies NLP tools to over 4,000 abstracts from the reintroduction biology literature (1987–2016). It extracts structured data (species, threat status, geography) and computes ensemble sentiment scores to analyse how scientific attention and emotional tone vary across taxa, threat levels, and regions.

### 🔑 Key Highlights

-   **Dataset:** Abstracts compiled by Van Houtan et al. (2020)
-   **Pipeline:** Custom extraction of taxonomic, geographic, and IUCN metadata
-   **Sentiment:** Ensemble score from five sentiment models
-   **Findings:** Spatial and taxonomic biases, with dominant focus on high-income countries and less-threatened species

### 📁 Project Structure

```         
├── README.md                             <- Main documentation of the project
│
├── data                                            
│   ├── 1- data.csv                       <- Raw data
│   ├── 2- data_with_info                 <- Enriched data with metadata
│   ├── 3- data_with_score                <- Final dataset with sentiment scores
│   ├── country_mapping.csv               <- Country matching table
│   └── World Shapefile                   <- Shapefiles required for mapping
│ 
├── src                                             
│   ├── 01- Information Extraction.R      <- Script for extracting taxonomy, IUCN, and countries
│   ├── 02- Sentiment Analysis.R          <- Script for computing sentiment scores
│   ├── 03- Analysis & Figures.R          <- Script for generating all figures
│
├── reports                                         
│   ├── Final Report.pdf                  <- Final written report
│   ├── Figures                           <- All figures contained in the report
│
```

------------------------------------------------------------------------

### 📬 Contact

**Alfonso Awadalla Carreño**\
[alfonso.awadalla-carreno\@polytechnique.edu](mailto:alfonso.awadalla-carreno@polytechnique.edu)
