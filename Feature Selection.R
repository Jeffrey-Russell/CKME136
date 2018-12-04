# libarary(car)


VIF <- vif(lm(log(TotalGross) ~ OpeningTheaters + log(ViewCount) + log(LikeCount) +
               log(DislikeCount) + log(LikeDislikeRatio) + log(CommentCount) + 
               log(positiveWordsBL) + log(negativeWordsBL) +
               netSentimentBL + scoreAbsoluteAFINN + log(budget) + 
               log(OpeningWeekendGross) + genre + mpaaRating, data = data))

# Remove log(DislikeCount), log(LikeDislikeRatio), log(negativeWordsBL), log(CommentCount)
VIF <- vif(lm(log(TotalGross) ~ OpeningTheaters + log(ViewCount) + log(LikeCount) +
                log(positiveWordsBL) + netSentimentBL + scoreAbsoluteAFINN + log(budget) + 
                log(OpeningWeekendGross) + genre + mpaaRating, data = data))

#Descriptive statistics table of features
stargazer(VIF, type = "html", digits = 5, out = "descriptive2.doc")
