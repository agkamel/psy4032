## code to prepare `DATASET` dataset goes here

set.seed(41)

vitesse <- stats::rnorm(20, mean = 25, sd = 5)
score01 <- agk::create_discrete()
score02 <- agk::create_discrete()
anxiete <- agk::create_ordinal(points = 20)
depression <- agk::create_ordinal(points = 40)
couleur <- agk::create_nominal(levels = c("bleu", "rouge", "vert"))

usethis::use_data(vitesse, overwrite = TRUE)
usethis::use_data(score01, overwrite = TRUE)
usethis::use_data(score02, overwrite = TRUE)
usethis::use_data(anxiete, overwrite = TRUE)
usethis::use_data(depression, overwrite = TRUE)
usethis::use_data(couleur, overwrite = TRUE)
