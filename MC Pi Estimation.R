library(tidyverse)

# set the number of points to generate
runs = 10000

set.seed(982)

# create tibble of randomly sampled values from the first 
#     quadrant of the unit circle
points = tibble(x = runif(runs, min = 0, max = 0.5),
                y = runif(runs, min = 0, max = 0.5))

# create the in.circle binary variable and the running
#     pi estimate for plotting
points %>%
  mutate(in.circle = ifelse(x^2 + y^2 <= 0.5^2, 1, 0),
         id = row_number(),
         ones = 1,
         running.mc.pi = (cumsum(in.circle)/cumsum(ones)*4)) -> points

# calculate the Monte Carlo estimate of pi
points %>% 
  summarise(mc.pi = (sum(in.circle)/runs)*4) %>% 
  pull() -> mc.pi

# plot the samples
ggplot(points, aes(x, y, col = factor(in.circle))) +
  geom_point(size = 0.3) +
  ggtitle(paste0("Pi estimate: ",mc.pi)) +
  theme_minimal() +
  theme(legend.position = 'none')

# plot the running estimate
ggplot(points, aes(id, running.mc.pi)) +
  geom_point(size = 0.3, color = 'blue') +
  geom_hline(yintercept = 3.1415629) +
  xlab('Number of points') + ylab('Pi Estimate') +
  ggtitle(paste0("Pi estimate: ",mc.pi)) +
  geom_text(aes(x = nrow(points) + nrow(points)*0.05, y = mc.pi + 0.05, label = 'Pi')) +
  theme_minimal()
