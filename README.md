
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psy4032

<!-- badges: start -->
<!-- badges: end -->

Ce package contient plusieurs jeux de données utilisés spécifiquement
pour le cours d’Analyse quantitative en psychologie (PSY4032).

## Installation

Pour installer la version de développement du package:

``` r
#install.packages("remotes")    # S'il n'est pas déjà intallé
remotes::install_github("agkamel/psy4032")
```

## Example

Une fois le package chargé, les jeux de données sont immédiatement
accessibles.

``` r
library(psy4032)
var_couleur
#>  [1] vert  vert  vert  bleu  vert  rouge bleu  vert  rouge rouge vert  bleu 
#> [13] vert  bleu  bleu  bleu  rouge rouge rouge bleu 
#> Levels: bleu rouge vert
```

``` r
var_temps
#>  [1] 29.40912 29.49457 32.51542 27.52559 23.29525 31.86605 29.09402 32.12871
#>  [9] 35.97881 20.52134 16.54915 22.66291 23.20038 28.73086 25.84351 33.25092
#> [17] 25.16738 19.83755 21.83782 27.89073
```
