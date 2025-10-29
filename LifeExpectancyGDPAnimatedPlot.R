#### Practicing creating an animated plot

library(gapminder)
library(ggplot2)
library(gganimate)

### Plotting GDP per capita against life expectancy, with dots sized by population
### and color-coded by continent
setwd("")
plot2 <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() + 
  scale_x_log10() +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') + 
  transition_time(year) +
  ease_aes('linear')

plot(plot2)

### Export plot to local folder
setwd("C:/Users/18159/Documents/Plots")
animate(plot2, duration = 30, fps = 20, width = 600, height = 600, renderer = gifski_renderer())
anim_save(plot2)

View(gapminder)

plot3 <- plot2 + shadow_wake(wake_length = 0.1, alpha = FALSE)
animate(plot3, duration = 30, fps = 20, width = 600, height = 400, renderer = gifski_renderer())
anim_save(plot3)






