% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuel_correction.R
\name{fuel_correction}
\alias{fuel_correction}
\title{Correction factor equation}
\usage{
fuel_correction(
  pollutant,
  euro_stage,
  improved_fuel = c(den = 835, s = 40, pah = 5, cn = 53, t95 = 320)
)
}
\arguments{
\item{pollutant}{character. Pollutant classified in "CO", "VOC", "NOx" or
"PM10".}

\item{euro_stage}{character. EURO period of vehicle, classified in "PRE", "I",
"II", "III", "IV", "V" and "VI".}

\item{improved_fuel}{numeric. Numeric vector for characteristics of an
improved fuel, ordered by: den (Density at 15 degrees C), s (Sulphur
content in ppm), pah (Polycyclic aromatics content in \\%), cn (Cetane
number), t95 (Back end distillation in degrees C). Default input uses
c(den = 835, s = 40, pah = 5, cn = 53, t95 = 320).}
}
\value{
numeric. A fuel correction factor.
}
\description{
Relations between emissions and fuel properties for diesel heavy-duty
vehicles based on EMEP/EEA. Function based on values from the \href{https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook}{EMEP/EEA air pollutant emission inventory guidebook 2019}.
Estimates are given by the ratio between correction factor of improved fuel
by the correction factor of base fuel.
}
\keyword{internal}
