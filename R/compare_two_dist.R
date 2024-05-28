compare_two_dist <- function(mean1 = 100,
                             sd1 = 15,
                             mean2 = 107,
                             sd2 = 15,
                             n = 50,
                             alpha = 0.05,
                             fill = "alpha") {

  z_scores <- zone <- is_beta <- dist1 <- density <- dist2 <- NULL

  fill <- fill # "alpha" ou "beta"
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

  lim_inf <- stats::qnorm(alpha / 2)
  lim_sup <- stats::qnorm(1 - (alpha / 2))

  lim_inf_dist1 <- stats::qnorm(alpha / 2, mean = mu_1, sd = sd_1)
  lim_sup_dist1 <- stats::qnorm(1 - (alpha / 2), mean = mu_1, sd = sd_1)

  lim_inf_dist2 <- stats::qnorm(alpha / 2, mean = mu_2, sd = sd_2)
  lim_sup_dist2 <- stats::qnorm(1 - (alpha / 2), mean = mu_2, sd = sd_2)





  beta <- stats::pnorm(lim_sup_dist1, mean = mu_2, sd = sd_2) - stats::pnorm(lim_inf_dist1, mean = mu_2, sd = sd_2)
  puissance <- 1 - beta



  two_dist_table <- dplyr::tibble(
    z_scores = seq(-3, 3, by = 0.005),
    density = stats::dnorm(z_scores),
    dist1 = z_scores * sd_1 + mu_1,
    dist2 = z_scores * sd_2 + mu_2,
    zone = dplyr::case_when(
      z_scores < lim_inf ~ "zone_rejet_inf",
      z_scores > lim_sup ~ "zone_rejet_sup",
      z_scores >= lim_inf | z_scores <= lim_sup ~ "zone_acceptation"
    ),
    is_beta = dplyr::case_when(
      dist2 < lim_inf_dist1 ~ "puissance_inf",
      dist2 > lim_sup_dist1 ~ "puissance_sup",
      dist2 >= lim_inf_dist1 | dist2 <= lim_sup_dist1 ~ "beta"
    )) |>
    dplyr::mutate(zone = forcats::fct(zone,
                                      levels = c("zone_rejet_inf", "zone_acceptation", "zone_rejet_sup")),
                  is_beta = forcats::fct(is_beta, levels = c("puissance_inf", "beta", "puissance_sup")))


  visualise_area_data <- list(mu_1 = mu_1, sd_1 = sd_1, mu_2 = mu_2, sd_2 = sd_2,
       alpha = alpha, lim_inf = lim_inf, lim_sup = lim_sup,
       lim_inf_dist1 = lim_inf_dist1, lim_sup_dist1 = lim_sup_dist1,
       lim_inf_dist2 = lim_inf_dist2, lim_sup_dist2 = lim_sup_dist2,
       beta = beta, puissance = puissance#,
       #two_dist_table
  )







  dist1_linewidth <- 1.2
  dist2_linewidth <- 1.2

  dist2_linetype <- 4
  mu_linetype <- 3


  graph <- ggplot2::ggplot(two_dist_table) +


    # Zone de coloration
      # Distribution 1
    ggplot2::geom_area(ggplot2::aes(x = dist1,
                                    y = density,
                                    fill = zone),
                       alpha = opacity_dist1) +
      # Distribution 2
    ggplot2::geom_area(stat = "identity",
                       ggplot2::aes(x = dist2,
                                    y = density,
                                    fill = is_beta),
                       alpha = opacity_dist2,
                       orientation = "x") +

    # Distribution 1
    ggplot2::geom_line(ggplot2::aes(x = dist1, y = density), linewidth = dist1_linewidth) +
    #ggplot2::scale_fill_manual(values = c("red3", "lightblue", "red", "turquoise3", "purple")) +

    # Mu
    ggplot2::geom_segment(x = mu_1, y = 0, yend = stats::dnorm(0), linewidth = dist1_linewidth, linetype = mu_linetype) +


    # Limite zone rejet bas
    ggplot2::geom_segment(x = stats::qnorm(alpha / 2, lower.tail = TRUE, mean = mu_1, sd = sd_1),
                 y = 0,
                 yend = stats::dnorm(lim_sup), linewidth = dist1_linewidth) +

    # Limite zone rejet haut
    ggplot2::geom_segment(x = stats::qnorm(1 - (alpha / 2), mean = mu_1, sd = sd_1),
                 y = 0,
                 yend = stats::dnorm(lim_sup), linewidth = dist1_linewidth) +



    # Distribution 2

    ggplot2::geom_line(ggplot2::aes(x = dist2, y = density), linetype = dist2_linetype, linewidth = dist2_linewidth) +

    # Mu
    ggplot2::geom_segment(x = mu_2, y = 0, yend = stats::dnorm(0), linewidth = dist2_linewidth, linetype = mu_linetype) +


    # Limite zone bas
    ggplot2::geom_segment(x = lim_inf_dist1,
                          y = 0,
                          yend = stats::dnorm((lim_inf_dist1 - mu_2) / sd_2), linewidth = dist2_linewidth, linetype = dist2_linetype) +

    # Limite zone haut
    ggplot2::geom_segment(x = lim_sup_dist1,
                          y = 0,
                          yend = stats::dnorm((lim_sup_dist1 - mu_2) / sd_2), linewidth = dist2_linewidth, linetype = dist2_linetype) +





    # Annotation et titres
    ggplot2::annotate(geom = "label", x = mu_1, y = 0.42, label = paste0("mu_0 = ", mu_1)) +
    ggplot2::annotate(geom = "label", x = mu_2, y = 0.35, label = paste0("mu_1 = ", mu_2)) +


    ggplot2::annotate(geom = "label", x = lim_inf_dist1, y = stats::dnorm(lim_inf), label = paste0(round(lim_inf_dist1, 2))) +
    ggplot2::annotate(geom = "label", x = lim_sup_dist1, y = stats::dnorm(lim_sup), label = paste0(round(lim_sup_dist1, 2))) +

    ggplot2::xlab("Scores") +
    ggplot2::ylab("Densité de probabilité") +
    ggplot2::labs(title = "Visualisation des différentes zones.",
         subtitle = paste0("\nNiveau de confiance = ", 1 - alpha, "   ", "Alpha = ", alpha, "   ",
                           "\nBeta = ", round(beta, 4), "   ", "Puissance = ", round(puissance, 4),
                          "\nn = ", n,  "   ", "Erreur type = ", round(sd1, 4), "   ", "Intervalle de confiance = ", paste0("[", round(lim_inf_dist1, 2), ", ", round(lim_sup_dist1, 2), "]")))

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
#'
#' @return Un graphique.
#' @export
#'
#' @examples
#' visualize_area()
visualize_area <- function(mu_0 = 100, sigma_0 = 15, mu_1 = 107, sigma_1 = NULL, n = 50, alpha = 0.05, fill = "alpha") {

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
                   alpha = alpha,
                   fill = fill)

}

