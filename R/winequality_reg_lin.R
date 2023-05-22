# May 22, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Using Regression Linear for each feature in the data

# Setup
data.features.independent = dim(data)[2] - 1

set.seed(seed)
lm.error = rep(0,data.features.independent)
names(lm.error) = colnames(data)[1:data.features.independent]

# Applying K-Fold Cross Validation
for (i in 1:data.features.independent) {
  set.seed(seed)
  fit = glm(as.double(data$quality) ~ data[,i], data = data)
  lm.error[i] = cv.glm(data, fit, K = 10)$delta[1]
}

# Data Frame
sort(lm.error)

lm.dataframe = tibble(
  feature = colnames(data)[1:data.features.independent],
  error   = lm.error
)

# Plot

ggplot(lm.dataframe, aes(
    x = fct_rev(fct_reorder(feature, error)),
    y = error
  )) +
  geom_col(fill = "darkgreen", width = 0.7) +
  geom_text(aes(label = round(error,3)), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Linear Regression",
    subtitle = "Using MSE",
    x = "",
    y = "MSE (Mean Square Error)"
  ) +
  theme_classic()

