#' Echantillonnage de statistique d'echantillons
#'
#' @param n La taille de chaque echantillon
#' @param k Le nombre d'echantillons
#' @param mean La moyenne de la population
#' @param sd L'ecart type de la population
#' @param min Valeur minimale
#' @param max Valeur maximale
#'
#' @return Un vecteur de statistiques echantillonnales de longueur k.
#' @export
#'
#' @examples
#' sample_mean_from_rnorm(n = 10, k = 100)
#' sample_mean_from_rnorm(n = 10, k = 100, mean = 50, sd = 10)
#'
#' sample_sd_from_rnorm(n = 10, k = 100)
#' sample_sd_from_rnorm(n = 10, k = 100, mean = 50, sd = 10)
#'
#' @rdname sample_mean_from_rnorm
#' @export
sample_mean_from_rnorm <- function(n, k = 1, mean = 0, sd = 1) {
  output <- vector(mode = "numeric", length = k)
  for (i in 1:k) {
    single_sample <- stats::rnorm(n, mean = mean, sd = sd)
    output[i] <- mean(single_sample)
  }

  message(paste0("Nombre d'echantillons: ", k))
  message(paste0("Taille de chaque echantillon: ", n))

  output
}

#' @rdname sample_mean_from_rnorm
#' @export
sample_sd_from_rnorm <- function(n, k = 1, mean = 0, sd = 1) {
  output <- vector(mode = "numeric", length = k)
  for (i in 1:k) {
    single_sample <- stats::rnorm(n, mean = mean, sd = sd)
    output[i] <- sd(single_sample)
  }

  message(paste0("Nombre d'echantillons: ", k))
  message(paste0("Taille de chaque echantillon: ", n))

  output
}

#' @rdname sample_mean_from_rnorm
#' @export
sample_mean_from_runif <- function(n, k = 1, min = 0, max = 1) {
  output <- vector(mode = "numeric", length = k)
  for (i in 1:k) {
    single_sample <- stats::runif(n, min = min, max = max)
    output[i] <- mean(single_sample)
  }

  message(paste0("Nombre d'echantillons: ", k))
  message(paste0("Taille de chaque echantillon: ", n))

  output
}
