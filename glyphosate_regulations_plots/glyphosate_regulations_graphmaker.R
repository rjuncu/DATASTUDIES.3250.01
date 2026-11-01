install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("scales")
library(dplyr)
library(readr)
library(ggplot2)
library(scales)

list.files()
legal_data <- read_tsv("./settlment_data.txt")

# Create a proper date variable for plotting
legal_data <- legal_data %>%
  mutate(
    Date = as.Date(sprintf("%d-%02d-01", Year, Month)),
#    Amount_Billions = Amount_USD / 1e9
  )
head(input_data)

# ----------------------------------------
# 2. Plot 1: Individual legal events over time
# ----------------------------------------

plot_timeline <- ggplot(
  legal_data,
  aes(x = Date, y = Amount_USD, color = Type)
) +
  geom_point(size = 3.5, alpha = 0.85) +
  scale_y_continuous(
    labels = label_dollar(scale=1)
  ) +
  scale_color_manual(
    values = c(
      "Verdict" = "steelblue",
      "Settlement" = "darkorange",
      "Final judgment" = "darkgreen"
    )
  ) +
  labs(
    title = "Roundup/Glyphosate Legal Awards Over Time",
    subtitle = "Reported verdicts, settlements, and final judgments",
    x = "Date",
    y = "Amount (USD)",
    color = "Legal event type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_timeline)

# ----------------------------------------
# 3. Plot 2: Total amount by year
# ----------------------------------------

yearly_totals <- legal_data %>%
  group_by(Year) %>%
  summarise(
    Total_USD = sum(Amount_USD, na.rm = TRUE),
    Number_of_Events = n(),
    .groups = "drop"
  )

plot_yearly <- ggplot(
  yearly_totals,
  aes(x = factor(Year), y = Total_USD)
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = label_dollar(
      scale = 1e-9, suffix = "B", accuracy = 0.01
    )(Total_USD)),
    vjust = -0.4,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = label_dollar(scale = 1e-9, suffix = "B"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Reported Legal Amounts by Year",
    subtitle = "Sum of the listed events in each year",
    x = "Year",
    y = "Total reported amount (USD)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

print(plot_yearly)

# ----------------------------------------
# 4. Plot 3: Top 10 largest events
# ----------------------------------------

top_10_cases <- legal_data %>%
  slice_max(
    order_by = Amount_USD,
    n = 10,
    with_ties = FALSE
  ) %>%
  arrange(Amount_USD) %>%
  mutate(
    Case = factor(Case, levels = Case)
  )

plot_top_10 <- ggplot(
  top_10_cases,
  aes(x = Amount_USD, y = Case, fill = Type)
) +
  geom_col() +
  scale_x_continuous(
    labels = label_dollar(scale = 1e-9, suffix = "B"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_fill_manual(
    values = c(
      "Verdict" = "steelblue",
      "Settlement" = "darkorange",
      "Final judgment" = "darkgreen"
    )
  ) +
  labs(
    title = "10 Largest Reported Legal Amounts",
    x = "Amount (USD)",
    y = NULL,
    fill = "Legal event type"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(plot_top_10)

# ----------------------------------------
# 5. Optional: Save the plots
# ----------------------------------------

ggsave("legal_timeline.png", plot_timeline,
       width = 11, height = 6, dpi = 300)

ggsave("legal_amounts_by_year.png", plot_yearly,
       width = 9, height = 6, dpi = 300)

ggsave("top_10_legal_amounts.png", plot_top_10,
       width = 11, height = 7, dpi = 300)

