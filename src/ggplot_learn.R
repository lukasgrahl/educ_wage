library(tidyverse)

df <- ggplot2::mpg

# view(df)
summray(df)
table(df$class) # value counts
sum(is.na(df$displ)) # count isna

# SCATTER PLOT
ggplot(data = df) + geom_point(mapping = aes(x = displ,
                                             y = hwy, 
                                             colour = class))

# jitter adds some noise to data so that points are more visible when having the same x
ggplot(data = df) + 
  geom_point(mapping = aes(x = displ,
                           y = hwy, 
                           colour = class),
             position = "jitter")

# FACETS GRID -> education effect on salary
ggplot(data = df) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

ggplot(data = df) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ .)

ggplot(data = df) +
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(~ drv)

# MULTIPLE PLOTS ADDING
# Plots can be added up, whereas each plot is layered on top of the previous, 
# this it can have different data on each axis

ggplot(data = df) + 
  geom_point(mapping = aes(x = displ, y = hwy, colour = drv),
             show.legend =FALSE) +
  geom_smooth(mapping = aes(x = displ, y = hwy, colour = drv),
              show.legend = FALSE)


# GLOBAL PLOT
# To set a global standard include mapping into ggplot argument
# This can be locally changed if the data argument is passed to one subplot

ggplot(data = df, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(colour = class)) + 
  geom_smooth(data = filter(df, class == "subcompact"),
              se = FALSE)

# # STAT PLOTS

df <- ggplot2::diamonds

# BAR PLOT

# the standard for geom_bar is count, its needs no specification
ggplot(data = df) + 
  geom_bar(mapping = aes(x = cut, y = ..count..))

# the agg func can be changed in y 
ggplot(data = df) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1)) + 
  coord_flip()

ggplot(data = df)+
  stat_summary(
    mapping = aes(
      x = cut,
      y = depth
    ),
    fun.min = min,
    fun.max = max,
    fun = mean
  )

# POSITIONAL ARGUMENTS

ggplot(data = df) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = df, 
       mapping = aes(x = cut, fill = clarity)
       ) +
  geom_bar(alpha = 1/2, position = "identity")

ggplot(data = df, 
       mapping = aes(x = cut, fill = clarity)
) +
  geom_bar(alpha = 1/2, position = "dodge")










