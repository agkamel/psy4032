## code to prepare `DATASET` dataset goes here

set.seed(41)

age <- agk::create_discrete(mean = 35, sd = 15)
sexe <- agk::create_nominal(points = 2, levels = c("h", "f"))
presence <- agk::create_dichotomic(levels = c("present", "absent"))
vitesse <- stats::rnorm(20, mean = 25, sd = 5)
anxiete <- agk::create_ordinal(points = 20)
depression <- agk::create_ordinal(points = 40)
couleur <- agk::create_nominal(levels = c("bleu", "rouge", "vert"))
score01 <- agk::create_discrete()
score02 <- agk::create_discrete(mean = 70, sd = 7)

usethis::use_data(age, overwrite = TRUE)
usethis::use_data(sexe, overwrite = TRUE)
usethis::use_data(presence, overwrite = TRUE)
usethis::use_data(vitesse, overwrite = TRUE)
usethis::use_data(anxiete, overwrite = TRUE)
usethis::use_data(depression, overwrite = TRUE)
usethis::use_data(couleur, overwrite = TRUE)
usethis::use_data(score01, overwrite = TRUE)
usethis::use_data(score02, overwrite = TRUE)

tableau <- tibble::tibble(
  id = 1:20,
  age = age,
  sexe = sexe,
  presence = presence,
  vitesse = vitesse,
  anxiete = anxiete,
  depression = depression,
  couleur = couleur,
  score01 = score01,
  score02 = score02
)

usethis::use_data(tableau, overwrite = TRUE)

data_context <- dplyr::tribble(
  ~var, ~type, ~points, ~mean, ~sd, ~levels,
  "depression", "ordinal", 20, 10, 5, NA,
  "anxiete", "ordinal", 25, 12, 5, NA,
  "estimesoi", "ordinal", 31, 20, 6, NA,
  "satisfaction", "ordinal", 50, 30, 8, NA,
  "verbalstatement", "dichotomic", 2, NA, NA, c("defusion", "acceptance"),

  "verbalabuse", "discrete", NA, 5, 10, NA,
  "gambling", "discrete", NA, 25, 40, NA,
  "intelligence", "continuous", NA, 100, 15, NA,
  "temps_ecran", "continuous", NA, 240, 90, NA,
  "socialisation", "ordinal", 5, 4, 1.5, c("très peu", "peu", "moyen", "élevé", "très élevé"),

  "education", "ordinal", 21, 12, 2, NA,
  "agressitive", "discrete", NA, 10, 7, NA,
  "sexe", "nominal", 2, 1.5, 0.5, c("homme", "femme"),
  "age", "discrete", NA, 35, 15, NA)

#data_context

#data_context |> tidyr::unnest(levels)


# [1] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=24"
# [2] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=356"
# [3] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=601"
# [4] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=165"
# [5] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=622"
# [6] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=532"
# [7] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=410"
# [8] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=297"
# [9] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=692"
# [10] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=283"






