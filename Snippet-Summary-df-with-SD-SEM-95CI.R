######### Calulcate summary statistics to fill dataframe 'df_summary' ########
# This is tidyverse approach

require(magrittr)
require(dplyr)

df_summary <- df_tidy %>%
  group_by(Time) %>%
  summarise(mean = mean(Ratio, na.rm = TRUE),
            sd = sd(Ratio, na.rm = TRUE),
            n = n()) %>%
  mutate(sem = sd / sqrt(n - 1),
         CI_lower = mean + qt((1-Conf_level)/2, n - 1) * sem,
         CI_upper = mean - qt((1-Conf_level)/2, n - 1) * sem)