## code to prepare `DATASET` dataset goes here

set.seed(41)

age <- agk::create_discrete(mean = 35, sd = 15)
sexe <- agk::create_nominal(points = 2, levels = c("h", "f"))
presence <- agk::create_dichotomous(levels = c("present", "absent"))
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

tableau500 <- agk::create_dataset(n = 500, v = 10)

tableau500 <- tableau500 |> dplyr::mutate(
  id = 1:nrow(tableau500),
  age = agk::rescale(v01, mean = 35, sd = 15) |> agk::as_discrete(),
  sexe = agk::as_nominal(v02, points = 2, levels = c("h", "f")),
  presence = agk::as_dichotomous(v03, points = 2, levels = c("present", "absent")),
  vitesse = agk::rescale(v04, mean = 25, sd = 5) |> agk::as_continuous(),
  anxiete = agk::as_ordinal(v05, points = 20),
  depression = agk::as_ordinal(v06, points = 40),
  couleur = agk::as_nominal(v07, levels = c("bleu", "rouge", "vert")),
  score01 = agk::rescale(v08, mean = 50, sd = 10) |> agk::as_discrete(),
  score02 = agk::rescale(v09, mean = 70, sd = 7) |> agk::as_discrete()
) |> dplyr::select(!c("v01", "v02", "v03", "v04", "v05", "v06", "v07", "v08", "v09", "v10"))

usethis::use_data(tableau500, overwrite = TRUE)


tableau10000 <- agk::create_dataset(n = 10000, v = 10)

tableau10000 <- tableau10000 |> dplyr::mutate(
  id = 1:nrow(tableau10000),
  age = agk::rescale(v01, mean = 35, sd = 15) |> agk::as_discrete(),
  sexe = agk::as_nominal(v02, points = 2, levels = c("h", "f")),
  presence = agk::as_dichotomous(v03, points = 2, levels = c("present", "absent")),
  vitesse = agk::rescale(v04, mean = 25, sd = 5) |> agk::as_continuous(),
  anxiete = agk::as_ordinal(v05, points = 20),
  depression = agk::as_ordinal(v06, points = 40),
  couleur = agk::as_nominal(v07, levels = c("bleu", "rouge", "vert")),
  score01 = agk::rescale(v08, mean = 50, sd = 10) |> agk::as_discrete(),
  score02 = agk::rescale(v09, mean = 70, sd = 7) |> agk::as_discrete()
) |> dplyr::select(!c("v01", "v02", "v03", "v04", "v05", "v06", "v07", "v08", "v09", "v10"))

usethis::use_data(tableau10000, overwrite = TRUE)







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

set.seed(41)

ndataset <- 3
possible_observation <- 200:1000
possible_variable <- 6:10
possible_functions <- c(cont = agk::as_continuous, disc = agk::as_discrete, ord = agk::as_ordinal, nom = agk::as_nominal, dich = agk::as_dichotomous)

for (i in seq(1,ndataset)) {
  n <- sample(possible_observation, 1)
  v <- sample(possible_variable, 1)
  funs <- sample(possible_functions, size = v, replace = TRUE)
  funs_names <- names(funs)

  #print(funs)
  #print(funs_names)
  #print(n)
  #print(v)

  tb <- agk::create_dataset(n = n,
                            v = v)


  for (i in 1:v) {

    if (names(funs[i]) == "cont") {
      tb[[i]] <-
        agk::rescale(tb[[i]],
                     mean = sample(25:50, 1),
                     sample(seq(10, 15, by = 0.1), 1)) |>
        agk::as_continuous()
      names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])

    } else if (names(funs[i]) == "disc") {
        tb[[i]] <-
          agk::rescale(tb[[i]],
                       mean = sample(25:50, 1),
                       sample(seq(10, 15, by = 0.1), 1)) |>
          agk::as_discrete()
        names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])

    } else if (names(funs[i]) == "ord") {
      tb[[i]] <-
        agk::as_ordinal(tb[[i]], points = sample(7:20, 1))
      names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])

    } else if (names(funs[i]) == "nom") {
      tb[[i]] <- agk::as_nominal(tb[[i]], points = sample(3:5, 1))
      names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])

    } else if (names(funs[i]) == "dich") {
      tb[[i]] <- agk::as_dichotomous(tb[[i]])
      names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])
    }


  }

  print(tb)

}



tb <- agk::create_dataset()



purrr::map(tb, funs) #|> dplyr::bind_cols()

library(agk)

