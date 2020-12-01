#' Calculate Damage
#'
#' Calculates the damage inflicted by an attack of given base power, with given
#' stats, levels, and modifiers.
#'
#' @param power base power of the move being used
#' @param atk offensive stat of the attacking Pokémon; for physical moves this is the Attack stat, and for special moves this is the Special Attack stat (with some exceptions)
#' @param def defensive stat of the defensive Pokémon; either Defense or Special Defense, similar to atk
#' @param level level of the attacking Pokémon (1-100)
#' @param modifiers a vector of modifiers affecting the damage calculation
#'
#' @return damage value given as an integer
#'
#' @export
damage <- function(power, atk, def, level = 50, modifiers = c()) {
  floor(prod(((floor((2 * level) / 5 + 2) * power * atk) %/% def) %/% 50 + 2, modifiers))
}
