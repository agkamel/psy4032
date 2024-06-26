% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_mean_from_rnorm.R
\name{sample_mean_from_rnorm}
\alias{sample_mean_from_rnorm}
\alias{sample_sd_from_rnorm}
\alias{sample_mean_from_runif}
\alias{sample_mean_from_rexp}
\title{Echantillonnage de statistique d'echantillons}
\usage{
sample_mean_from_rnorm(n, k = 1, mean = 0, sd = 1)

sample_sd_from_rnorm(n, k = 1, mean = 0, sd = 1)

sample_mean_from_runif(n, k = 1, min = 0, max = 1)

sample_mean_from_rexp(n, k = 1, rate = 1)
}
\arguments{
\item{n}{La taille de chaque echantillon}

\item{k}{Le nombre d'echantillons}

\item{mean}{La moyenne de la population}

\item{sd}{L'ecart type de la population}

\item{min}{Valeur minimale}

\item{max}{Valeur maximale}

\item{rate}{Taux}
}
\value{
Un vecteur de statistiques echantillonnales de longueur k.
}
\description{
Echantillonnage de statistique d'echantillons
}
\examples{
sample_mean_from_rnorm(n = 10, k = 100)
sample_mean_from_rnorm(n = 10, k = 100, mean = 50, sd = 10)

sample_sd_from_rnorm(n = 10, k = 100)
sample_sd_from_rnorm(n = 10, k = 100, mean = 50, sd = 10)

}
