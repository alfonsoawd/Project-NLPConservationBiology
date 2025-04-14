# 🔍 Replication of Sentiment Analysis in Conservation Studies

---

## Version Française

### Contexte & Objectif

Ce projet consiste en une réplication du papier *Sentiment Analysis of Conservation Studies Captures Successes of Species Reintroductions* en utilisant mon propre code et mes analyses. L’objectif est de comparer les résultats obtenus et d’identifier des pistes d’amélioration.  
Ce travail est réalisé dans le cadre du cours **Machine Learning for NLP**, sous la supervision de **Christopher Kermorvant**, et par **Alfonso Awadalla** (alfonso.awadalla-carreno@polytechnique.edu).

### Points Clés

- **Objectif :** Répliquer l’analyse de sentiment présentée dans l’article afin de comparer les résultats et proposer des améliorations.
- **Méthode :** Utilisation de modèles NLP et d’analyse de sentiment personnalisés sur un corpus de textes scientifiques de conservation.
- **Comparaison :** Evaluation des résultats par rapport à ceux du papier de référence et identification des axes d’optimisation.

### Structure du Projet
```
├── README.md                                      <- Documentation principale du projet
├── LICENSE                                        <- Licence du projet
├── app                                            <- Scripts pour exécuter l’outil de cartographie interactive
├── data                                           <- Toutes les données
│   ├── 1- Raw Data                                <- Données brutes
│   ├── 2- Formatted Data                          <- Données formatées
│   ├── 3- Final Data                              <- Données finales
│   ├── linking tables                             <- Tables de liaison
│   └── shapefiles                                 <- Shapefiles nécessaires à la cartographie
├── src                                            <- Code source pour le traitement et l’analyse des données.
│   ├── 00_explore_jocas_missing_values.ipynb      <- Exploration des valeurs manquantes de JOCAS
│   ├── 01_match_communes_with_shapefile.ipynb     <- Correspondance des communes avec le shapefile
│   ├── 02_clean_rome_fap_mapping.ipynb            <- Nettoyage de la table de correspondance ROME-FAP
│   ├── 03_process_stmt_demand.ipynb               <- Traitement des données STMT (demande)
│   ├── 04_process_jocas_supply.ipynb              <- Traitement des données JOCAS (offre)
│   └── 05_compute_labour_tightness_ratio.ipynb    <- Calcul du ratio de tension sur le marché du travail
├── docs                                           <- Références et documents utilisés lors du projet
├── reports                                        <- Note méthodologique + diaporama de présentation
```

### Exécution du Projet

1. Placer les données dans le dossier `data`.
2. Exécuter les notebooks dans le dossier `src` dans l'ordre suivant :  
   - `data_preprocessing.ipynb`  
   - `sentiment_analysis.ipynb`  
   - `result_comparison.ipynb`
3. Consulter le dossier `docs` pour le rapport complet et la présentation des résultats.

### Contact

- **Alfonso Awadalla** : [alfonso.awadalla-carreno@polytechnique.edu](mailto:alfonso.awadalla-carreno@polytechnique.edu)

---

## English Version

### Context & Objective

This project replicates the paper *Sentiment Analysis of Conservation Studies Captures Successes of Species Reintroductions* using my own code and analysis. The goal is to compare the results with the original study and explore possible improvements.  
This work is carried out as part of the **Machine Learning for NLP** course, supervised by **Christopher Kermorvant**, and is developed solely by **Alfonso Awadalla** (alfonso.awadalla-carreno@polytechnique.edu).

### Key Points

- **Objective:** Replicate the sentiment analysis presented in the paper to compare outcomes and propose enhancements.
- **Method:** Use custom NLP models and sentiment analysis on a conservation science abstracts corpus.
- **Comparison:** Evaluate and benchmark the results against those in the reference paper, identifying areas for improvement.

### Project Structure
```
├── README.md                                       <- Main documentation of the project
├── LICENSE                                         <- Project license
├── app                                             <- Scripts to run the interactive mapping tool
├── data                                            <- All the data
│   ├── 1- Raw Data                                 <- Raw data
│   ├── 2- Formatted Data                           <- Formatted data
│   ├── 3- Final Data                               <- Final data
│   ├── linking tables                              <- Linking tables
│   └── shapefiles                                  <- Shapefiles required for mapping
├── src                                             <- Source code for data processing and analysis.
│   ├── 00_explore_jocas_missing_values.ipynb       <- Exploration of missing values in JOCAS
│   ├── 01_match_communes_with_shapefile.ipynb      <- Matching communes with shapefile
│   ├── 02_clean_rome_fap_mapping.ipynb             <- Cleaning the ROME-FAP mapping table
│   ├── 03_process_stmt_demand.ipynb                <- Processing STMT data (demand)
│   ├── 04_process_jocas_supply.ipynb               <- Processing JOCAS data (supply)
│   └── 05_compute_labour_tightness_ratio.ipynb     <- Calculating the labour market tightness ratio
├── docs                                            <- References and documents used during the project
├── reports                                         <- Methodological note + presentation slides
```

### Running the Project

1. Place the necessary data in the `data` folder.
2. Run the notebooks in the `src` folder in the following order:  
   - `data_preprocessing.ipynb`  
   - `sentiment_analysis.ipynb`  
   - `result_comparison.ipynb`
3. Refer to the `reports` folder for the full report and presentation of the results.

### Contact

- **Alfonso Awadalla** : [alfonso.awadalla-carreno@polytechnique.edu](mailto:alfonso.awadalla-carreno@polytechnique.edu)
