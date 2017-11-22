# A function to quickly calculate the equilibrium radiation emissions of a planet

# The Stefan Boltzmann constant
sigma = 5.7 * 10 ** -8

calc_ert <- function(A, d, f, S = 340) {
  # The function takes A (albido), d (distance from star in AU),
  # f (absorptivity), and S (solar radiation input)
  k = (S / sigma)
  ref = ((1 - A) / (d ** 2))
  absp = (1 - .5 * f)
  (k * ref / absp) ** .25
}

calc_ert(.1, 1, .9, 340)

# statistics for Venus, Earth, Mars, Jupiter
albidos = c(.77, .3, .15, .58)
distances = c(.72, 1.0, 1.52, 5.20)

library(purrr)
map2_dbl(.x = refs, .y = dists, .f = calc_ert)
