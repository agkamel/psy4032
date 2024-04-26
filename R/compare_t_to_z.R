#' Fonction simple pour comparer la distribution t et la distribution normale
#'
#' Cette fonction permet de visualiser la différence entre les courbes de t et
#' de z en changeant le nombre de degré de liberté de t. Plus dl est grand, et plus
#' elle se rapproche d'une distribution z.
#'
#' @param df Nombre de degre de liberte.
#'
#' @return Un graphique
#' @export
#'
#' @examples
#' compare_t_to_z(df = 2)
compare_t_to_z <- function(df = 1) {

  density <- NULL

  stopifnot("df must me a numeric value" = is.numeric(df))

  t_scores <- seq(-4, 4, 0.005)

  student <- dplyr::tibble(
    t_scores = t_scores,
    stu = stats::dt(t_scores, df = df),
    norm = stats::dnorm(t_scores)
  )

  student <- student |>
    tidyr::pivot_longer(cols = c("stu", "norm"), names_to = "df", values_to = "density") |>
    dplyr::mutate(df = factor(df, levels = c("stu", "norm")))

  ggplot2::ggplot(student) +
    ggplot2::geom_line(ggplot2::aes(x = t_scores, y = density, color = df), linewidth = 1.5) +
    ggplot2::labs(title = paste0("Comparaison de la distribution t avec (dl de ", df, ") et de la distribution normale")) +
    ggplot2::xlab("Scores t") +
    ggplot2::ylab("Densité de probabilité") +
    ggplot2::scale_x_continuous(limits = c(-5, 5))
}
