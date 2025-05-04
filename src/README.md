# 📜 Scripts of the project — Sentiment Analysis & Information Extraction

------------------------------------------------------------------------

## 🇫🇷 Version Française

### Description

Ce dossier contient les trois scripts R développés pour ce projet, chacun correspondant à une étape clé du pipeline d’analyse :

1.  **Extraction d'information** — Identification des espèces, des catégories de menace (UICN) et des localisations géographiques à partir des résumés.
2.  **Analyse de sentiment** — Application de cinq modèles lexicaux et syntaxiques pour évaluer le ton émotionnel de chaque résumé.
3.  **Analyse & Figures** — Génération de statistiques descriptives et visualisations présentées dans le rapport final.

Les scripts sont conçus pour être exécutés séquentiellement.

### Structure

```         
├── 01- Information Extraction.R <- Extraction des entités taxonomiques, UICN, et géographiques
├── 02- Sentiment Analysis.R <- Préparation des textes et calcul des scores de sentiment
├── 03- Analysis & Figures.R <- Analyse des résultats et production des figures finales
```

### Pré-requis

-   Les bibliothèques R nécessaires sont installées automatiquement si elles ne sont pas présentes.
-   Il est recommandé d’utiliser **R 4.0+**.
-   Une connexion internet est requise à la première exécution.
-   ⚠️ Le script `01- Information Extraction.R` nécessite la configuration de **trois clés API** :
    -   **NCBI** : utilisée via le package `taxize` pour récupérer les hiérarchies taxonomiques → [NCBI API](https://support.nlm.nih.gov/kbArticle/?pn=KA-05317)
    -   **IUCN Red List** : utilisée via le package `rredlist` pour accéder aux statuts de conservation → [IUCN API](https://api.iucnredlist.org/)
    -   **Zhipu AI** : utilisée pour compléter automatiquement les pays depuis des localisations ambigües ou régionales → [Zhipu API](https://open.bigmodel.cn/dev/api#%E8%AF%AD%E4%B9%89%E7%90%86%E8%A7%A3%E7%BB%93%E6%9E%84%E5%88%86%E6%9E%90)

------------------------------------------------------------------------

## 🇬🇧 English Version

### Description

This folder contains the three R scripts developed for this project, each corresponding to a key stage in the analysis pipeline:

1.  **Information Extraction** — Identifies taxonomic entities, IUCN categories, and geographic locations from scientific abstracts.
2.  **Sentiment Analysis** — Applies five lexicon- and syntax-based sentiment models to assess the emotional tone of each abstract.
3.  **Analysis & Figures** — Generates descriptive statistics and all final figures used in the report.

Scripts are intended to be run sequentially.

### Structure

```         
├── 01- Information Extraction.R <- Extracts taxonomy, IUCN status, and countries from abstracts
├── 02- Sentiment Analysis.R <- Preprocesses text and computes sentiment scores
├── 03- Analysis & Figures.R <- Performs final analysis and generates visual outputs
```

### Requirements

-   Required R packages are installed automatically if not already present.
-   **R version 4.0 or above** is recommended.
-   An internet connection is required during the first run.
-   ⚠️ Script `01- Information Extraction.R` requires setting up **three API keys**:
    -   **NCBI** — used via the `taxize` package to retrieve taxonomic hierarchies → [NCBI API](https://support.nlm.nih.gov/kbArticle/?pn=KA-05317)
    -   **IUCN Red List** — used via the `rredlist` package to access species conservation statuses → [IUCN API](https://api.iucnredlist.org/)
    -   **Zhipu AI** — used to infer country names from vague or regional mentions → [Zhipu API](https://open.bigmodel.cn/dev/api#%E8%AF%AD%E4%B9%89%E7%90%86%E8%A7%A3%E7%BB%93%E6%9E%84%E5%88%86%E6%9E%90)
