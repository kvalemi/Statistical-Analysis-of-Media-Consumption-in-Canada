## Install libraries ##
# install.packages('rjson')
# install.packages('dplyr')
# install.packages("ggplot2")


## Load libraries ##
library(ggplot2)
library(rjson)
library(dplyr)


## DATA Analysis ##
##################################

# preliminary data loading + preparation (CHANGE THIS TO YOUR DIRECTORY)
setwd('/Users/Kaveh/Documents/School Files/STAT 403/Project/Project Delivarables/Data/')
video_df = read.csv('processed_video_data.csv', header = TRUE)
video_df = video_df[,-1] # remove index

# set correct data type
video_df$categoryName    = as.factor(video_df$categoryName)
video_df$trending_month  = as.factor(video_df$trending_month)
video_df$trending_year   = as.factor(video_df$trending_year)
video_df$like_view_ratio = as.numeric(video_df$like_view_ratio)


# Figure 2
LTV_boxplot <- ggplot(data = video_df, aes(x = categoryName, y = like_view_ratio)) + 
  ggtitle("Like-To-View Ratio Per Trending Video Categories") +
  xlab("Trending Video Categories") + 
  ylab("Like-To-View (LTV) Ratio") + 
  theme(axis.text.x = element_text(size = 13, angle = 90)) +
  geom_boxplot(alpha=0.2)

LTV_boxplot

# Figure 3
LTV_Year_boxplot <- ggplot(data = video_df, 
                           aes(x = categoryName, y = like_view_ratio, fill = trending_year)) + 
  ggtitle("Like-To-View Ratio Per Trending Video Categories and Year") +
  xlab("Trending Video Categories") + 
  ylab("Like-To-View (LTV) Ratio") + 
  geom_boxplot(alpha=0.2) + 
  theme(axis.text.x = element_text(size = 13, angle = 90))

LTV_Year_boxplot

# 95% confidence interval of  LTV ratio for 2020 and 2021 and upload year
summary_stats <- plyr::ddply(video_df, "trending_year", function(x) {
  
  oneci <- t.test(x$like_view_ratio)$conf.int
  names(oneci) <- c("lcl","ucl")
  return(oneci)
  
})
summary_stats$mean = rowMeans(summary_stats[ , c(2,3)], na.rm=TRUE)

# Figure 4
ltv_ci_plot <- ggplot(data = summary_stats, aes(x = trending_year, y = mean)) +
  geom_point() +
  geom_line(group = 1)+
  xlab("Trending Year") +
  ylab("Like-To-View (LTV) Ratio") +
  ggtitle("95% Confidence Interval of Like-To-View Ratio Per Year")+
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width=.02,color="black") + 
  theme(axis.text.x = element_text(size = 13, angle = 90))

ltv_ci_plot

# ANOVA
LTV_cat_anova_test = aov(like_view_ratio ~ categoryName + trending_year, data = video_df)
summary(LTV_cat_anova_test)


# Tukey Kramer Test
posthoc <- TukeyHSD(x=LTV_cat_anova_test, conf.level=0.95)
(posthoc)
TK_data = as.data.frame(posthoc[1:1])
# write.csv(TK_data, 'TK_data.csv')


# Check the ANOVA assumptions through plots
par(mfrow=c(1,2))
plot(LTV_cat_anova_test, 1)
plot(LTV_cat_anova_test, 2)
