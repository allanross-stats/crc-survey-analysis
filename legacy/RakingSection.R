# proportional iterative fitting 

# ---- Simple raking / weighting explanation ----
# Goal:
# Adjust survey weights so the sample better matches known population totals.
# We are NOT changing people's answers.
# We are changing how much each respondent "counts."

# Example:
# Suppose our sample has too many females and not enough males.
sample_gender <- tibble(
  gender = c("Male", "Female"),
  sample_prop = c(0.35, 0.65),
  population_prop = c(0.50, 0.50)
)

sample_gender <- sample_gender |>
  mutate(
    adjustment_factor = population_prop / sample_prop
  )

sample_gender


# Interpretation:
# If adjustment_factor > 1, that group was underrepresented and gets weighted up.
# If adjustment_factor < 1, that group was overrepresented and gets weighted down.


# In your actual dataset, the raking has already been done.
# The variable `wt` is the survey weight.

# Unweighted table:
df_eda |>
  count(cc1) |>
  mutate(prop_unweighted = n / sum(n))

# Weighted table:
df_eda |>
  group_by(cc1) |>
  summarise(weighted_n = sum(wt, na.rm = TRUE)) |>
  mutate(prop_weighted = weighted_n / sum(weighted_n))


# Weighted plot:
ggplot(df_eda, aes(x = cc1, weight = wt)) +
  geom_bar() +
  labs(
    title = "Weighted Support for Building a CRC",
    x = "Support response",
    y = "Weighted count"
  ) +
  theme_minimal()

#

