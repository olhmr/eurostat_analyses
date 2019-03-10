# Scripts for visualising data

# Make this an interactive shiny app instead?
plot_bars_over_time <- function(crim_pris_ctz) {
  
  # Dependencies
  require(data.table)
  require(ggplot2)
  require(gganimate) # Also requires gifski to be installed (if using gganimate's default renderer)
  
  ggplot(data = crim_pris_ctz[citizenship != "total" & unit == "per hundred thousand inhabitants"], 
         aes(x = region, y = value, fill = citizenship)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
    labs(title = "Year: {closest_state}", x = "Region", y = "Prisoners per hundred thousand inhabitants") +
    transition_states(year, state_length = 4, transition_length = 0) + 
    ease_aes("linear")
  
}