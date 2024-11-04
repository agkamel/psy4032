compare_two_dist <- function(mean1 = 100,
                             sd1 = 15,
                             mean2 = 107,
                             sd2 = 15,
                             n = 50,
                             alpha = 0.05,
                             fill = "alpha",
                             alternative = "two.sided"
                             ) {

  z_scores <- zone <- is_beta <- dist1 <- density <- dist2 <- NULL

  alternative <- alternative     # Choix: "two.sided", "greater", "lower"

  fill <- fill                   # Choix: "alpha" ou "beta"
  if (fill == "alpha") {

    opacity_dist1 <- 0.5
    opacity_dist2 <- 0

  } else if (fill == "beta") {

    opacity_dist1 <- 0
    opacity_dist2 <- 0.5

  }

  mu_1 <- mean1
  sd_1 <- sd1

  mu_2 <- mean2
  sd_2 <- sd2

  alpha <- alpha

  valeur_z <-  (mu_2 - mu_1) / sd_1

  if (alternative == "two.sided") {

    r_alpha <- alpha / 2

    # Valeur critique ####
    valeur_critique_inf <- stats::qnorm(r_alpha)
    valeur_critique_sup <- stats::qnorm(r_alpha, lower.tail = FALSE)

    # Valeur critique non standardisé ####
    lim_inf_dist1 <- stats::qnorm(r_alpha, mean = mu_1, sd = sd_1)
    lim_sup_dist1 <- stats::qnorm(r_alpha, mean = mu_1, sd = sd_1, lower.tail = FALSE)

    # Intervalle de confiance ####
    lim_inf_dist2 <- stats::qnorm(r_alpha, mean = mu_2, sd = sd_2)
    lim_sup_dist2 <- stats::qnorm(r_alpha, mean = mu_2, sd = sd_2, lower.tail = FALSE)

    # Erreur de type II et puissante ####
    beta <- stats::pnorm(lim_sup_dist1, mean = mu_2, sd = sd_2) - stats::pnorm(lim_inf_dist1, mean = mu_2, sd = sd_2)
    puissance <- 1 - beta

    # Tableau des distribution ####
    two_dist_table <- dplyr::tibble(
      z_scores = seq(-3, 3, by = 0.005),
      density = stats::dnorm(z_scores),
      dist1 = z_scores * sd_1 + mu_1,
      dist2 = z_scores * sd_2 + mu_2,
      zone = dplyr::case_when(
        z_scores < valeur_critique_inf ~ "zone_rejet_inf",
        z_scores > valeur_critique_sup ~ "zone_rejet_sup",
        z_scores >= valeur_critique_inf | z_scores <= valeur_critique_sup ~ "zone_acceptation"
      ),
      is_beta = dplyr::case_when(
        dist2 < lim_inf_dist1 ~ "puissance_inf",
        dist2 > lim_sup_dist1 ~ "puissance_sup",
        dist2 >= lim_inf_dist1 | dist2 <= lim_sup_dist1 ~ "beta"
      )) |>
      dplyr::mutate(zone = forcats::fct(zone,
                                        levels = c("zone_rejet_inf", "zone_acceptation", "zone_rejet_sup")),
                    is_beta = forcats::fct(is_beta, levels = c("puissance_inf", "beta", "puissance_sup")))

  } else if (alternative == "greater") {

    r_alpha <- alpha

    # Valeur critique ####
    valeur_critique_inf <- NULL
    valeur_critique_sup <- stats::qnorm(r_alpha, lower.tail = FALSE)

    # Valeur critique non standardisé ####
    lim_inf_dist1 <- NULL
    lim_sup_dist1 <- stats::qnorm(r_alpha, mean = mu_1, sd = sd_1, lower.tail = FALSE)


    # Intervalle de confiance ####
    lim_inf_dist2 <- stats::qnorm(r_alpha, mean = mu_2, sd = sd_2)
    lim_sup_dist2 <- NULL

    # Erreur de type II et puissante ####
    beta <- stats::pnorm(lim_sup_dist1, mean = mu_2, sd = sd_2)
    puissance <- 1 - beta

    # Tableau des distribution ####
    two_dist_table <- dplyr::tibble(
      z_scores = seq(-3, 3, by = 0.005),
      density = stats::dnorm(z_scores),
      dist1 = z_scores * sd_1 + mu_1,
      dist2 = z_scores * sd_2 + mu_2,
      zone = dplyr::case_when(
        z_scores > valeur_critique_sup ~ "zone_rejet_sup",
        z_scores <= valeur_critique_sup ~ "zone_acceptation"
      ),
      is_beta = dplyr::case_when(
        dist2 > lim_sup_dist1 ~ "puissance_sup",
        dist2 <= lim_sup_dist1 ~ "beta"
      )) |>
      dplyr::mutate(zone = forcats::fct(zone,
                                        levels = c("zone_rejet_inf", "zone_acceptation", "zone_rejet_sup")),
                    is_beta = forcats::fct(is_beta, levels = c("puissance_inf", "beta", "puissance_sup")))


  } else if (alternative == "less") {

    r_alpha <- alpha

    # Valeur critique ####
    valeur_critique_inf <- stats::qnorm(r_alpha)
    valeur_critique_sup <- NULL

    # Valeur critique non standardisé ####
    lim_inf_dist1 <- stats::qnorm(r_alpha, mean = mu_1, sd = sd_1)
    lim_sup_dist1 <- NULL

    # Intervalle de confiance ####
    lim_inf_dist2 <- NULL
    lim_sup_dist2 <- stats::qnorm(r_alpha, mean = mu_2, sd = sd_2, lower.tail = FALSE)

    # Erreur de type II et puissante ####
    beta <- 1 - stats::pnorm(lim_inf_dist1, mean = mu_2, sd = sd_2)
    puissance <- 1 - beta

    # Tableau des distribution ####
    two_dist_table <- dplyr::tibble(
      z_scores = seq(-3, 3, by = 0.005),
      density = stats::dnorm(z_scores),
      dist1 = z_scores * sd_1 + mu_1,
      dist2 = z_scores * sd_2 + mu_2,
      zone = dplyr::case_when(
        z_scores < valeur_critique_inf ~ "zone_rejet_inf",
        z_scores >= valeur_critique_inf ~ "zone_acceptation"
      ),
      is_beta = dplyr::case_when(
        dist2 < lim_inf_dist1 ~ "puissance_inf",
        dist2 >= lim_inf_dist1 ~ "beta"
      )) |>
      dplyr::mutate(zone = forcats::fct(zone,
                                        levels = c("zone_rejet_inf", "zone_acceptation", "zone_rejet_sup")),
                    is_beta = forcats::fct(is_beta, levels = c("puissance_inf", "beta", "puissance_sup")))


  }



  # Données à exporter ####
  visualise_area_data <- list(mu_1 = mu_1,
                              sd_1 = sd_1,
                              mu_2 = mu_2,
                              sd_2 = sd_2,
                              alpha = alpha,
                              r_alpha = r_alpha,
                              valeur_critique_inf = valeur_critique_inf,
                              valeur_critique_sup = valeur_critique_sup,
                              lim_inf_dist1 = lim_inf_dist1,
                              lim_sup_dist1 = lim_sup_dist1,
                              lim_inf_dist2 = lim_inf_dist2,
                              lim_sup_dist2 = lim_sup_dist2,
                              beta = beta,
                              puissance = puissance#,
                              #two_dist_table
                              )

  # Paramétrage du graphique ####
  dist1_linewidth <- 1.2
  dist2_linewidth <- 1.2
  dist2_linetype <- 5
  #dist1_color <- "red3"
  dist2_color <- "blue"
  mu_linetype <- 3

  # Graphique ####
  graph <- ggplot2::ggplot(two_dist_table) +

    # Zone de coloration ####

  # Distribution 1
  ggplot2::geom_area(ggplot2::aes(x = dist1, y = density, fill = zone), alpha = opacity_dist1) +

    # Distribution 2
    ggplot2::geom_area(
      stat = "identity",
      ggplot2::aes(x = dist2, y = density, fill = is_beta),
      alpha = opacity_dist2,
      orientation = "x"
    ) +

    # Lignes de la distribution 1 ####

  # Ligne distribution 1
  ggplot2::geom_line(ggplot2::aes(x = dist1, y = density), linewidth = dist1_linewidth) +
    #ggplot2::scale_fill_manual(values = c("red3", "lightblue", "red", "turquoise3", "purple")) +

    # Ligne moyenne
    ggplot2::geom_segment(
      x = mu_1,
      y = 0,
      yend = stats::dnorm(0),
      linewidth = dist1_linewidth,
      linetype = mu_linetype
    )

  if (alternative == "less" | alternative == "two.sided") {
    # Ligne de limite - zone rejet bas
    graph <- graph +
      ggplot2::geom_segment(
        x = stats::qnorm(
          r_alpha,
          lower.tail = TRUE,
          mean = mu_1,
          sd = sd_1
        ),
        y = 0,
        yend = stats::dnorm(valeur_critique_inf),
        linewidth = dist1_linewidth
      )
  }

  if (alternative == "greater" | alternative == "two.sided") {
    graph <- graph +
      # Ligne de limite - zone rejet haut
      ggplot2::geom_segment(
        x = stats::qnorm(1 - r_alpha, mean = mu_1, sd = sd_1),
        y = 0,
        yend = stats::dnorm(valeur_critique_sup),
        linewidth = dist1_linewidth
      )
  }





  # Lignes de la distribution 2 ####
  graph <- graph +

    # Ligne de la distribution 2
    ggplot2::geom_line(
      ggplot2::aes(x = dist2, y = density),
      linetype = dist2_linetype,
      linewidth = dist2_linewidth,
      color = dist2_color
    ) +

    # Ligne de la moyenne
    ggplot2::geom_segment(
      x = mu_2,
      y = 0,
      yend = stats::dnorm(0),
      linewidth = dist2_linewidth,
      linetype = mu_linetype,
      color = dist2_color
    )


  if ((alternative == "less" | alternative == "two.sided") & (fill == "beta")) {
    graph <- graph +

      # Ligne de limite zone de puissance - basse
      ggplot2::geom_segment(
        x = lim_inf_dist1,
        y = 0,
        yend = stats::dnorm((lim_inf_dist1 - mu_2) / sd_2),
        linewidth = dist2_linewidth,
        linetype = 5,
        color = dist2_color
      )
  }

  if ((alternative == "greater" | alternative == "two.sided") & (fill == "beta")) {

    # Ligne de limite zone de puissance - haut
    graph <- graph +
      ggplot2::geom_segment(
        x = lim_sup_dist1,
        y = 0,
        yend = stats::dnorm((lim_sup_dist1 - mu_2) / sd_2),
        linewidth = dist2_linewidth,
        linetype = 5,
        color = dist2_color
      )
  }


  if (alternative == "greater" | alternative == "two.sided") {
    # Ligne de limite - Intervalle de confiance
    graph <- graph + ggplot2::geom_segment(
      x = lim_inf_dist2,
      y = 0,
      yend = stats::dnorm(valeur_critique_sup),
      linewidth = dist2_linewidth,
      linetype = dist2_linetype,
      color = dist2_color
    )

  }


  if (alternative == "less" | alternative == "two.sided") {
    # Ligne de limite - Intervalle de confiance
    graph <- graph + ggplot2::geom_segment(
      x = lim_sup_dist2,
      y = 0,
      yend = stats::dnorm(valeur_critique_inf),
      linewidth = dist2_linewidth,
      linetype = dist2_linetype,
      color = dist2_color
    )


  }

  graph <- graph +

    # Annotation et titres ####
  # Moyennes
  ggplot2::annotate(
    geom = "label",
    x = mu_1,
    y = 0.42,
    label = paste0("mu[0] == ", mu_1),
    parse = TRUE
  ) +
    ggplot2::annotate(
      geom = "label",
      x = mu_2,
      y = 0.35,
      label = paste0("mu[1] == ", mu_2),
      parse = TRUE,
      color = "blue"
    )

    # Valeurs critiques

  if (alternative == "less" | alternative == "two.sided") {
  graph <- graph +


    ggplot2::annotate(
      geom = "label",
      x = lim_inf_dist1,
      y = stats::dnorm(valeur_critique_inf),
      label = paste0("Valeur critique\n", round(valeur_critique_inf, 2))
    )

  }

  if (alternative == "greater" | alternative == "two.sided") {
    graph <- graph +
    ggplot2::annotate(
      geom = "label",
      x = lim_sup_dist1,
      y = stats::dnorm(valeur_critique_sup),
      label = paste0("Valeur critique\n", round(valeur_critique_sup, 2))
    )
  }

  # Intervalle de confiance

  if (alternative == "greater" | alternative == "two.sided") {
    graph <- graph +

    ggplot2::annotate(
      geom = "label",
      x = lim_inf_dist2,
      y = stats::dnorm(valeur_critique_sup) - 0.03,
      label = paste0("IC inf.\n", round(lim_inf_dist2, 2)),
      alpha = 0.70,
      color = "blue"
    )

  }

  if (alternative == "less" | alternative == "two.sided") {

    graph <- graph +

    ggplot2::annotate(
      geom = "label",
      x = lim_sup_dist2,
      y = stats::dnorm(valeur_critique_inf) - 0.03,
      label = paste0("IC sup.\n", round(lim_sup_dist2, 2)),
      alpha = 0.70,
      color = "blue"
    )

  }


  # Valeur z
  graph <- graph +

    ggplot2::annotate(
      geom = "label",
      x = mu_2,
      y = 0.1,
      label = paste0("z = ", round(valeur_z,2))
    )

  graph <- graph +

    ggplot2::xlab("Scores") +
    ggplot2::ylab("Densité de probabilité") +
    ggplot2::labs(
      title = "Visualisation des différentes zones.",
      subtitle = paste0(
        "\nNiveau de confiance = ",
        1 - alpha,
        "   ",
        "Alpha = ",
        alpha,
        "   ",
        "\nBeta = ",
        round(beta, 4),
        "   ",
        "Puissance = ",
        round(puissance, 4),
        "\nn = ",
        n,
        "   ",
        "Erreur type = ",
        round(sd1, 4),
        "   ",
        "Intervalle de confiance = ",
        paste0("[",
               if (is.null(lim_inf_dist2) == FALSE) {round(lim_inf_dist2, 2)} else {"-Inf"},
               ", ",
        if (is.null(lim_sup_dist2) == FALSE) {round(lim_sup_dist2, 2)} else {"Inf"},
               "]")
      ))


  print(graph)

  invisible(visualise_area_data)
}


#compare_two_dist()
# compare_two_dist(mean1 = 700, mean2 = 702.5, sd1 = 1.5, sd2 = 1.5, alpha = 0.0456, fill = "beta")
# compare_two_dist(mean1 = 100, mean2 = 110, sd1 = 15, sd2 = 15, alpha = 0.05, fill = "beta")

#' Visualisation des zones de alpha et beta sous la courbe normale
#'
#' @param mu_0 La moyenne de la population
#' @param sigma_0 L'écart type de la population
#' @param mu_1 La moyenne de l'échantillon
#' @param sigma_1 L'écart type de l'échantillon, si différente de sigma_0
#' @param n La taille d'échantillon
#' @param alpha L'erreur de type I, valeur entre 0 et 1
#' @param fill Type de coloration sous la courbe, soit "alpha" (défaut) ou "beta"
#' @param alternative Direction du test: "two.sided" pour bilatéral (défaut), "greater" pour unilatéral supérieur, "less" pour unilatéral inférieur
#'
#' @return Un graphique.
#' @export
#'
#' @examples
#' visualize_area()
visualize_area <- function(mu_0 = 100, sigma_0 = 15,
                           mu_1 = 107, sigma_1 = NULL,
                           n = 50, alpha = 0.05,
                           fill = "alpha",
                           alternative = "two.sided") {

  if (is.null(sigma_1) == TRUE) {
    sigma_1 <- sigma_0
    erreur_type_0 <- sigma_0 / sqrt(n)
    erreur_type_1 <- erreur_type_0
  } else {
    erreur_type_0 <- sigma_0 / sqrt(n)
    erreur_type_1 <- sigma_1 / sqrt(n)
  }

  compare_two_dist(mean1 = mu_0,
                   sd1 = erreur_type_0,
                   mean2 = mu_1,
                   sd2 = erreur_type_1,
                   n = n,
                   alpha = alpha,
                   fill = fill,
                   alternative = alternative
                   )

}

