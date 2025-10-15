# R.version.string
# 
# install.packages("https://cran.r-project.org/src/contrib/Archive/Boom/Boom_0.9.15.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/BoomSpikeSlab/BoomSpikeSlab_1.2.6.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/bsts/bsts_0.9.10.tar.gz", repos = NULL, type = "source")
# install.packages("https://cran.r-project.org/src/contrib/Archive/CausalImpact/CausalImpact_1.4.0.tar.gz", repos = NULL, type = "source")
# 
# remotes::install_github("cran/CausalImpact")
# install.packages("xts", dependencies = TRUE)



library(CausalImpact)
library(zoo)
library(ggplot2)
library(scales)
set.seed(42)

# --- Load and prepare data ---
df <- read.csv("/Users/mustakahmad/Library/CloudStorage/OneDrive-purdue.edu/FACAI LAB/Project3_tariff/tariff_war/raw_data/forest_loss.csv")
df$date <- as.Date(paste0(df$year, "-01-01"))
zdata <- zoo(df[, c(43,8,9,10,11,12,13,14,18,20,21,22,23)], order.by = df$date)
zdata <- na.omit(zdata)
colnames(zdata)

# --- Run CausalImpact ---
pre.period <- as.Date(c("2002-01-01", "2017-01-01"))
post.period <- as.Date(c("2018-01-01", "2023-01-01"))
impact <- CausalImpact(zdata, pre.period = pre.period, post.period = post.period,  model.args = list(niter = 2500, standardize.data = TRUE, prior.level.sd = 0.18))
print(impact)
summary(impact,"report")

# Convert impact plot to ggplot object
p <- plot(impact) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  scale_x_date(
    breaks = seq(min(index(impact$series)), max(index(impact$series)), by = "1 year"),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p)


# --- Extract coefficients safely ---
coef_summary <- summary(impact$model$bsts.model)$coefficients
colnames(coef_summary)

# Build dataframe
df1 <- data.frame(
  Variable = rownames(coef_summary),
  InclusionProb = as.numeric(coef_summary[, "inc.prob"])
)

# --- Clean and order data ---
df1 <- subset(df1, Variable != "(Intercept)")
df1 <- df1[order(df1$InclusionProb), ]
df1$Variable <- factor(df1$Variable, levels = df1$Variable)

# --- Nature-style plot ---
p <- ggplot(df1, aes(x = InclusionProb, y = Variable)) +
  geom_col(color = "black", fill = "darkgrey", width = 0.5) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = NULL , y = NULL) +
  theme_classic(base_size = 12, base_family = "Helvetica") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(size = 10.5, color = "black", margin = margin(r = 6)),
    axis.text.x = element_text(size = 10, color = "black", margin = margin(t = 6)),
    # axis.title.x = element_text(size = 11, face = "bold", margin = margin(t = 8)),
    plot.margin = margin(5, 12, 5, 5)
  )

print(p)

