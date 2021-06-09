library(sentimentr)

Norm_func <- function(var, a, b){
       var_new = (b - a)*((var - min(var))/(max(var) - min(var))) + a
      return(var_new)
}

Modes <- function(x) {
      ux <- unique(x)
       tab <- tabulate(match(x, ux))
       ux[tab == max(tab)]
}

df <- data.table::fread("C:/Users/namja/Desktop/covid_tweet/covid_text_sentiment.csv", sep = "|",data.table = FALSE)

df$sentiment_r      <- sentiment(df[,"cleaned_text"])$sentiment

df$sentiment_r_norm <- Norm_func(var = df$sentiment_r, a = -1, b = 1)

summary(df$sentiment_r)

summary(df$sentiment_r_norm)

hist(df$sentiment_r_norm, breaks = 200)

hist(df$compound, breaks = 200)

plot(df$compound, df$sentiment_r_norm)

cor(df$compound, df$sentiment_r_norm, method= "spearman")

df$mean_sentiment = round((df$compound + df$sentiment_r_norm)/2, 4)

hist(df$mean_sentiment, breaks = 100)

Modes(df$mean_sentiment)

summary(df$mean_sentiment)
 
df$binary_class <- ifelse(df$mean_sentiment > median(df$mean_sentiment), 1, 2)
df$three_class  <- ifelse(df$mean_sentiment > median(df$mean_sentiment), 1, 2)
df$three_class[which(df$mean_sentiment == Modes(df$mean_sentiment))] <- 0

data.table::fwrite(df, "C:/Users/namja/Desktop/covid_tweet/covid_text_sentiment_mean1_r.csv", sep = "|", row.names = FALSE)
