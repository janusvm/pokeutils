#' Calculate Stat Value
#'
#' Calculates the raw stat of a Pokémon, i.e. the numerical value which would be
#' displayed in the game's summary menu.
#'
#' @param base base stat for the species (1-255)
#' @param level level of the Pokémon (1-100)
#' @param iv Individual Value in the given stat (0-31)
#' @param ev Effort Values in the given stat (0-252)
#' @param nature which effect does the Pokémon's nature have on the given stat?
#' @param is_hp is the stat being calculated HP or not?
#'
#' @return stat value given as an integer
#'
#' @export
stat_value <- function(base, level = 50, iv = 31, ev = 252, nature = c("positive", "neutral", "negative"), is_hp = FALSE) {
  nature <- switch(match.arg(nature),
    positive = 1.1,
    neutral  = 1.0,
    negative = 0.9
  )

  if (is_hp) {
    ((2 * base + iv + ev %/% 4) * level) %/% 100 + level + 10
  } else {
    floor((((2 * base + iv + ev %/% 4) * level) %/% 100 + 5) * nature)
  }
}
