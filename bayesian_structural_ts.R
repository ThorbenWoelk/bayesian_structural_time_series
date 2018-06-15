library(tidyverse)
library(scales)
library(CausalImpact)
library(readr)
library(extrafont) 
loadfonts()

# SET CONSTANTS
DATE.OF.INTERVENTION <- as.Date("2018-06-01")

# define a plotting style 
style <- list(theme_bw(base_size = 15),
              theme(axis.title.x=element_blank(),
                    strip.text = element_text(size=10, family = "DINPro-Regular", colour = "black"),
                    axis.title.y=element_text(size=10, vjust = 3, family = "DINPro-Regular"), # uncomment to specify a different font type for title axis y
                    legend.text = element_text(size=10, family = "DINPro-Regular"), # uncomment to specify a different font type for text in legend
                    legend.title = element_blank(),
                    axis.text.x = element_text(size=8, vjust = 3, family = "DINPro-Regular"), # uncomment to specify a different font type for text on x axis
                    plot.title = element_text(hjust = 0.4),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    axis.ticks=element_blank(),
                    legend.position="top"
              ))

# import data
data.file <- "data.tsv"
data <- read_tsv(data.file)

# define dates column
dated <- filter(data, campaign == "response_ts")$dated

# define response time series
ts.response <- filter(data, campaign == "response_ts")$clicks
# define predictor time series
ts.pred <- filter(data, campaign == "predictor_ts")$clicks

# join all time series and date column together
data <- zoo(cbind(ts.response, ts.pred), dated)
# View(data)

# plot this
matplot(data, type = "l")

# specify training period and counterfactual prediction (post-intervention period)

pre.period <- as.Date(c(as.character(min(dated)), as.character(DATE.OF.INTERVENTION)))
post.period <- as.Date(c(as.character(DATE.OF.INTERVENTION + 1), as.character(max(dated))))

# perform inference
impact <- CausalImpact(data, pre.period, post.period)

# plot this
plot(impact, c("original", "pointwise", "cumulative")) +
  style + 
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%m")

# in summary table
summary(impact)

