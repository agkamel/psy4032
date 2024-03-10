
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psy4032

<!-- badges: start -->
<!-- badges: end -->

Ce package contient plusieurs jeu de données utilisées pour le cours
d’Analyse quantitative en psychologie (PSY4032).

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
couleur
#>  [1] rouge vert  bleu  vert  bleu  bleu  vert  vert  rouge bleu  rouge bleu 
#> [13] rouge bleu  vert  bleu  rouge rouge vert  vert 
#> Levels: bleu rouge vert
```

``` r
vitesse
#>  [1] 21.02816 25.98629 30.00852 31.44413 29.52877 27.46834 27.99643 17.10196
#>  [9] 30.00310 35.94004 18.95338 22.06344 30.28060 23.41708 24.72729 26.64876
#> [17] 28.31548 29.39164 26.01437 36.37201
```
