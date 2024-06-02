## code to prepare `DATASET` dataset goes here

set.seed(41)

var_age <- agk::create_discrete(mean = 35, sd = 15)
var_sexe <- agk::create_nominal(points = 2, levels = c("h", "f"))
var_presence <- agk::create_dichotomous(levels = c("present", "absent"))
var_temps <- stats::rnorm(20, mean = 25, sd = 5)
var_distance <- stats::rnorm(20, mean = 40, sd = 10)
var_anxiete <- agk::create_ordinal(points = 20)
var_depression <- agk::create_ordinal(points = 40)
var_couleur <- agk::create_nominal(levels = c("bleu", "rouge", "vert"))
var_score01 <- agk::create_discrete()
var_score02 <- agk::create_discrete(mean = 70, sd = 7)



usethis::use_data(var_age, overwrite = TRUE)
usethis::use_data(var_sexe, overwrite = TRUE)
usethis::use_data(var_presence, overwrite = TRUE)
usethis::use_data(var_temps, overwrite = TRUE)
usethis::use_data(var_distance, overwrite = TRUE)
usethis::use_data(var_anxiete, overwrite = TRUE)
usethis::use_data(var_depression, overwrite = TRUE)
usethis::use_data(var_couleur, overwrite = TRUE)
usethis::use_data(var_score01, overwrite = TRUE)
usethis::use_data(var_score02, overwrite = TRUE)

tab20 <- tibble::tibble(
  id = 1:20,
  age = var_age,
  sexe = var_sexe,
  presence = var_presence,
  temps = var_temps,
  distance = var_distance,
  anxiete = var_anxiete,
  depression = var_depression,
  couleur = var_couleur,
  score01 = var_score01,
  score02 = var_score02
)

usethis::use_data(tab20, overwrite = TRUE)

tab500 <- agk::create_dataset(n = 500, v = 10)

tab500 <- tab500 |> dplyr::mutate(
  id = 1:nrow(tab500),
  age = agk::rescale(v01, mean = 35, sd = 15) |> agk::as_discrete(),
  sexe = agk::as_nominal(v02, points = 2, levels = c("h", "f")),
  presence = agk::as_dichotomous(v03, points = 2, levels = c("present", "absent")),
  temps = agk::rescale(v04, mean = 25, sd = 5) |> agk::as_continuous(),
  distance = agk::rescale(v05, mean = 75, sd = 16) |> agk::as_continuous(),
  anxiete = agk::as_ordinal(v06, points = 20),
  depression = agk::as_ordinal(v07, points = 40),
  couleur = agk::as_nominal(v08, levels = c("bleu", "rouge", "vert")),
  score01 = agk::rescale(v09, mean = 50, sd = 10) |> agk::as_discrete(),
  score02 = agk::rescale(v10, mean = 70, sd = 7) |> agk::as_discrete()
) |> dplyr::select(!c("v01", "v02", "v03", "v04", "v05", "v06", "v07", "v08", "v09", "v10"))

usethis::use_data(tab500, overwrite = TRUE)


tab10000 <- agk::create_dataset(n = 10000, v = 10)

tab10000 <- tab10000 |> dplyr::mutate(
  id = 1:nrow(tab10000),
  age = agk::rescale(v01, mean = 35, sd = 15) |> agk::as_discrete(),
  sexe = agk::as_nominal(v02, points = 2, levels = c("h", "f")),
  presence = agk::as_dichotomous(v03, points = 2, levels = c("present", "absent")),
  temps = agk::rescale(v04, mean = 25, sd = 5) |> agk::as_continuous(),
  distance = agk::rescale(v05, mean = 75, sd = 16) |> agk::as_continuous(),
  anxiete = agk::as_ordinal(v05, points = 20),
  depression = agk::as_ordinal(v06, points = 40),
  couleur = agk::as_nominal(v07, levels = c("bleu", "rouge", "vert")),
  score01 = agk::rescale(v08, mean = 50, sd = 10) |> agk::as_discrete(),
  score02 = agk::rescale(v09, mean = 70, sd = 7) |> agk::as_discrete()
) |> dplyr::select(!c("v01", "v02", "v03", "v04", "v05", "v06", "v07", "v08", "v09", "v10"))

usethis::use_data(tab10000, overwrite = TRUE)


# Pour les distributions de données

var_anxiete500 <- agk::create_discrete(n = 500, mean = 32, sd = 20.7)

var_temps500 <- agk::create_continuous(n = 500, mean = 56, sd = 14.2)

usethis::use_data(var_anxiete500, overwrite = TRUE)
usethis::use_data(var_temps500, overwrite = TRUE)

#
#
#
#
#
# data_context <- dplyr::tribble(
#   ~var, ~type, ~points, ~mean, ~sd, ~levels,
#   "depression", "ordinal", 20, 10, 5, NA,
#   "anxiete", "ordinal", 25, 12, 5, NA,
#   "estimesoi", "ordinal", 31, 20, 6, NA,
#   "satisfaction", "ordinal", 50, 30, 8, NA,
#   "verbalstatement", "dichotomic", 2, NA, NA, c("defusion", "acceptance"),
#
#   "verbalabuse", "discrete", NA, 5, 10, NA,
#   "gambling", "discrete", NA, 25, 40, NA,
#   "intelligence", "continuous", NA, 100, 15, NA,
#   "temps_ecran", "continuous", NA, 240, 90, NA,
#   "socialisation", "ordinal", 5, 4, 1.5, c("très peu", "peu", "moyen", "élevé", "très élevé"),
#
#   "education", "ordinal", 21, 12, 2, NA,
#   "agressitive", "discrete", NA, 10, 7, NA,
#   "sexe", "nominal", 2, 1.5, 0.5, c("homme", "femme"),
#   "age", "discrete", NA, 35, 15, NA)
#
# #data_context
#
# #data_context |> tidyr::unnest(levels)
#
#
# # [1] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=24"
# # [2] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=356"
# # [3] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=601"
# # [4] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=165"
# # [5] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=622"
# # [6] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=532"
# # [7] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=410"
# # [8] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=297"
# # [9] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=692"
# # [10] "https://psycnet-apa-org.proxy.bibliotheques.uqam.ca/thesaurus/item?page=283"
#
# set.seed(41)
#
# ndataset <- 3
# possible_observation <- 200:1000
# possible_variable <- 6:10
# possible_functions <- c(cont = agk::as_continuous, disc = agk::as_discrete, ord = agk::as_ordinal, nom = agk::as_nominal, dich = agk::as_dichotomous)
#
# for (i in seq(1,ndataset)) {
#   n <- sample(possible_observation, 1)
#   v <- sample(possible_variable, 1)
#   funs <- sample(possible_functions, size = v, replace = TRUE)
#   funs_names <- names(funs)
#
#   #print(funs)
#   #print(funs_names)
#   #print(n)
#   #print(v)
#
#   tb <- agk::create_dataset(n = n,
#                             v = v)
#
#
#   for (i in 1:v) {
#
#     if (names(funs[i]) == "cont") {
#       tb[[i]] <-
#         agk::rescale(tb[[i]],
#                      mean = sample(25:50, 1),
#                      sample(seq(10, 15, by = 0.1), 1)) |>
#         agk::as_continuous()
#       names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])
#
#     } else if (names(funs[i]) == "disc") {
#         tb[[i]] <-
#           agk::rescale(tb[[i]],
#                        mean = sample(25:50, 1),
#                        sample(seq(10, 15, by = 0.1), 1)) |>
#           agk::as_discrete()
#         names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])
#
#     } else if (names(funs[i]) == "ord") {
#       tb[[i]] <-
#         agk::as_ordinal(tb[[i]], points = sample(7:20, 1))
#       names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])
#
#     } else if (names(funs[i]) == "nom") {
#       tb[[i]] <- agk::as_nominal(tb[[i]], points = sample(3:5, 1))
#       names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])
#
#     } else if (names(funs[i]) == "dich") {
#       tb[[i]] <- agk::as_dichotomous(tb[[i]])
#       names(tb)[i] <- paste0(names(tb)[i], "_", funs_names[i])
#     }
#
#
#   }
#
#   print(tb)
#
# }
#
#
#
# tb <- agk::create_dataset()
#
#
#
# purrr::map(tb, funs) #|> dplyr::bind_cols()

library(agk)
library(dplyr)
set.seed(41)

# Pour Test-t

# Une variable dichotomique
# Une variable continue
data01 <- agk::gen_variable(n = 50) |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2"),
                                                          labels = c("groupe1", "groupe2")
                                                          ))


data02 <- agk::gen_variable(n = 49) |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2"),
                                                          labels = c("groupe1", "groupe2")
                                              ))

data03 <- agk::gen_variable(n = 62) |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2"),
                                                          labels = c("groupe1", "groupe2")
                                              ))

data04 <- agk::gen_variable(n = 43) |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2"),
                                                          labels = c("groupe1", "groupe2")
                                              ))

data05 <- agk::gen_variable(n = 63) |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2"),
                                                          labels = c("groupe1", "groupe2")
                                              ))

data06 <- agk::gen_variable(n = 66)

data07 <- agk::gen_variable(n = 89)

data08 <- agk::gen_variable(n = 52)

data09 <- agk::gen_variable(n = 87)


# ANOVA
# Une variable catégorielles à 3 groupes
# Deux variables continues
data10 <- agk::gen_variable(n = 66) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data11 <- agk::gen_variable(n = 50) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data12 <- agk::gen_variable(n = 49) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data13 <- agk::gen_variable(n = 62) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data14 <- agk::gen_variable(n = 43) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data15 <- agk::gen_variable(n = 63) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data16 <- agk::gen_variable(n = 66) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data17 <- agk::gen_variable(n = 89) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data18 <- agk::gen_variable(n = 52) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))

data19 <- agk::gen_variable(n = 87) |> mutate(v1 = as.double(cut(v1, breaks = 3)),
                                              v1 = factor(as.character(v1),
                                                          levels = c("1", "2", "3"),
                                                          labels = c("groupe1", "groupe2", "groupe3")
                                              ))


## Corrélation régression
# Trois variables continues
data20 <- agk::gen_variable(n = 66)

data21 <- agk::gen_variable(n = 50)

data22 <- agk::gen_variable(n = 49)

data23 <- agk::gen_variable(n = 62)

data24 <- agk::gen_variable(n = 43)

data25 <- agk::gen_variable(n = 63)

data26 <- agk::gen_variable(n = 66)

data27 <- agk::gen_variable(n = 89)

data28 <- agk::gen_variable(n = 52)

data29 <- agk::gen_variable(n = 87)



## Chi-carré
# Trois variables dichotomiques
data30 <- agk::gen_variable(n = 66)  |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                               v1 = factor(as.character(v1),
                                                           levels = c("1", "2"),
                                                           labels = c("groupe1", "groupe2")
                                               ),
                                               v2 = as.double(cut(v2, breaks = 2)),
                                               v2 = factor(as.character(v2),
                                                           levels = c("a", "b"),
                                                           labels = c("groupe_a", "groupe_b")),
                                               v3 = as.double(cut(v3, breaks = 3)),
                                               v3 = factor(as.character(v3),
                                                           levels = c("a", "b"),
                                                           labels = c("groupe_a", "groupe_b")))
data31 <- agk::gen_variable(n = 66)  |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                               v1 = factor(as.character(v1),
                                                           levels = c("1", "2"),
                                                           labels = c("groupe1", "groupe2")
                                               ),
                                               v2 = as.double(cut(v2, breaks = 2)),
                                               v2 = factor(as.character(v2),
                                                           levels = c("a", "b"),
                                                           labels = c("groupe_a", "groupe_b")),
                                               v3 = as.double(cut(v3, breaks = 3)),
                                               v3 = factor(as.character(v3),
                                                           levels = c("a", "b"),
                                                           labels = c("groupe_a", "groupe_b")))

data32 <- agk::gen_variable(n = 66)  |> mutate(v1 = as.double(cut(v1, breaks = 2)),
                                               v1 = factor(as.character(v1),
                                                           levels = c("1", "2"),
                                                           labels = c("groupe1", "groupe2")
                                               ),
                                               v2 = as.double(cut(v2, breaks = 2)),
                                               v2 = factor(as.character(v2),
                                                           levels = c("a", "b"),
                                                           labels = c("groupe_a", "groupe_b")),
                                               v3 = as.double(cut(v3, breaks = 3)),
                                               v3 = factor(as.character(v3),
                                                           levels = c("a", "b"),
                                                           labels = c("groupe_a", "groupe_b")))



usethis::use_data(data01, overwrite = TRUE)
usethis::use_data(data02, overwrite = TRUE)
usethis::use_data(data03, overwrite = TRUE)
usethis::use_data(data04, overwrite = TRUE)
usethis::use_data(data05, overwrite = TRUE)
usethis::use_data(data06, overwrite = TRUE)
usethis::use_data(data07, overwrite = TRUE)
usethis::use_data(data08, overwrite = TRUE)
usethis::use_data(data09, overwrite = TRUE)
usethis::use_data(data10, overwrite = TRUE)
usethis::use_data(data11, overwrite = TRUE)
usethis::use_data(data12, overwrite = TRUE)
usethis::use_data(data13, overwrite = TRUE)
usethis::use_data(data14, overwrite = TRUE)
usethis::use_data(data15, overwrite = TRUE)
usethis::use_data(data16, overwrite = TRUE)
usethis::use_data(data17, overwrite = TRUE)
usethis::use_data(data18, overwrite = TRUE)
usethis::use_data(data19, overwrite = TRUE)
usethis::use_data(data20, overwrite = TRUE)
usethis::use_data(data21, overwrite = TRUE)
usethis::use_data(data22, overwrite = TRUE)
usethis::use_data(data23, overwrite = TRUE)
usethis::use_data(data24, overwrite = TRUE)
usethis::use_data(data25, overwrite = TRUE)
usethis::use_data(data26, overwrite = TRUE)
usethis::use_data(data27, overwrite = TRUE)
usethis::use_data(data28, overwrite = TRUE)
usethis::use_data(data29, overwrite = TRUE)
usethis::use_data(data30, overwrite = TRUE)


