# A Comparative Analysis of Hillary Clinton and Donald Trump's Twitter Usage

## Introduction

Words are often believed to be indicators of values that are important to politicians. A large body of research argue that political leanings correlate with various factors. On average, **left-and-right leaning individuals express differently  through their use of language in different political orientations (speeches, debates, and social media content)**. 

During a presidential election, such language differences in orientations may influence voters' perceptions of a given candidate. In recent years, many scholars have begun through computerized methods to analyze political candidates' discourse to determine whether the candidates' linguistic forms may be differentially linked to particular aspects of their personalities, political parties, or their stances on important topics such as healthcare, education, immigration, abortion, or economy. 

A recent study analyzed Trump and Clinton's Twitter accounts in the six months before the election and found that *Trump benefited by using moral-emotional languages but Clinton did not: moral emotions like "hate" were more likely to unite supporters, but a non-moral emotion concept like "justice" failed to do so.* (Twitter's Passion Politics: https://nyti.ms/2uWiSDY) The research team speculated that the differences could be a result of political parties. 

The purpose of this study is to contribute to the growing body of quantitative research on the discourse and personalities of politicians. **In order to discover how Republican and Democrat candidates use Twitter during the campaign, I conduct a content analysis of Hillary Clinton and Donald Trump's tweets from 2016 election season. I compare the two candidates' tweets from the 2016 presidential election to find any systematic differences between the two and model the outcomes using a logistics regression model.**

## Related Work and Hypothesis

### Literature

I discuss three aspects of literature:

1. Value-centered theoretical frameworks: this section summarizes different models and approaches for measuring psychological and language use differences between liberals and conservatives.

2. Twitter analyses and computational approaches: this section introduces how various applications of Twitter analyses are incorporated in social research, and how computerized methods gain widespread use

3. Linguistic Inquiry and Word Count (LIWC): this section summarizes the most recent studies that use computational linguistic analytical tools to analyze content from speeches, debates, and social networking platforms 

### Hypotheses

Based on the literature, I hypothesize that: 

*H1: Democrat candidate Hillary Clinton emphasizes the perception of uniqueness, and Republican candidate Donald Trump emphasizes group memberships.*

*H2: Democrat candidate's language contains more positive sentiments and emotions and more swear words, while Republican candidate uses more negation words and more negative sentiments and emotions.*

*H3: Republican candidate Donald Trump are more likely to emphasizes death, achievement, and religion than his Democratic counterpart.*

I plan to test these hypotheses using a content analysis of tweets from Hillary Clinton and Donald Trump during the 2016 Elections. For outcomes where there is no sufficient evidence in the literature or features that have not been comprehensively discussed before, the analysis is treated as exploratory.


## Data

This study uses **a public Twitter dataset that consists of tweets of the official Twitter accounts of the Republican and Democratic candidates: Hilary Clinton (@HillaryClinton) and Donald Trump (@realDonaldTrump)**. 

The original dataset contains 11,770 tweets from 2014-01-01 to 2016-10-14 from the candidates' official Twitter accounts with no retweets. For the purpose of the study, I choose a sub-dataset that contains a total of **7,901 tweets** from **2015-03-01 to 2016-10-14**, which is roughly from the launch to the end of 2016 presidential run (On March 18, 2015, Donald Trump formed an exploratory committee in preparation for a possible run for president on the Republican Party ticket; on April 12, 2015, former Secretary of State Hillary Clinton formally announced her candidacy for the presidential nomination of the Democratic Party). 

Tweets were obtained through javascript scraping of the browser twitter timeline rather than a Tweepy python API or the twitter timeline API. Because the dataset contains no retweets, it will provide us with more accurate insights about the language use difference between the two candidates, rather than the original author of the tweets. 

Although there have been data scientists arguing that Trump's campaign team and himself are using the same account to spread tweets, there are no formal evidence proving that the tweets are from different people. (http://varianceexplained.org/r/trump-tweets/) Therefore, I will treat all tweets from the two official accounts as political orientation of the two candidates and include them in the analysis as they represent the activity, opinions, and ideas of the two.

Each tweet contains its id, time, text, link, and author (Hillary Clinton/realDonaldTrump). The dataset can be downloaded from the Kaggle website. (https://www.kaggle.com/speckledpingu/RawTwitterFeeds)

## Research design and Method

The analysis consists of three parts:

1. Investigate the most differentiating terms for Republican and Democrat candidate, Donald Trump and Hillary Clinton (data exploration)

2. Investigate sentiments and emotions manifested in tweets for each candidate (data exploration)

3. Investigate the most differentiating features of tweets for the two candidates (logistics regression: significant features)

The first two parts of the analysis utilizes **statistical and text mining tools in R**. The third part involves finding predictors of political orientation using categories from **LIWC**. Then I use related predictors to run a logistic regression and find out the most significant features. 

### Data cleaning and preprocessing

normalization/stemming, stop words removal, and TF/IDF

### Data exploration: 

**tf-idf bigrams, words related to sentiments/emotions, change of sentiments over time**
(The Syuzhet package supports the use of different Sentiment Analysis lexicons. The **NRC method**, for example, evaluates a body of text and **returns positive values for eight different emotions (anger, anticipation, disgust, fear, joy, sadness, surprise, and trust)**, and **two sentiments (negative and positive)**. You can compute these scores with the get_nrc_sentiment() function.)

### Logistic regression
#### Annotate every tweet with LIWC software 
(**The Linguistic Inquiry and Word Count (LIWC) software** provides a set of dictionaries that group words by category (i.e. i-related words, religion-related words, family-related words, achievement-related words). LIWC calculates the percentages of words of specified categories appearing in the submitted text. LIWC has been validated and successfully used by social science researchers in the past. )

#### Features to test the hypotheses:

Based on the hypotheses, I focus on 12 dimensions in 3 major categories in order to profile political sentiment: **1st person singular pronouns, 1st person plural pronouns, positive emotions, negative emotions, negation words, sadness, anxiety, anger, swear words, death, achievement, and religion**. 

#### Logistic regression: significance of different features, compute odds ratio

### Some interesting insights

In order to discover how Republican and Democratic candidates use Twitter during the 2016 campaign, I conduct a content analysis of Hillary Clinton and Donald Trump's tweets from the 2016 election season. I compare the two candidates' tweets to find any systematic differences between the two and model the outcomes using a logistic regression model. In this study, I also conduct extensive experiments on the dataset and explored different approaches for text mining and incorporated them into the analysis. 

![Image of frequency](https://github.com/yanningcui/thesis/blob/master/thesis_draft_files/figure-markdown_github/frequency-1.png)

While specific hypotheses follow a thorough review of the relevant literature, the study finds that **Donald Trump is more likely to attack his opponents by sticking negative labels on them. He also emphasizes individualism and narcissism, and uses uncivil words more frequently; Hillary Clinton's attention is more spread out, including different political issues and policies. She tends to emphasize group membership and pays more attention to female-related topics.** 

![Image of tf-idf bigram](https://github.com/yanningcui/thesis/blob/master/thesis_draft_files/figure-markdown_github/unnamed-chunk-4-1.png)

YOU CAN FIND MORE RESULTS HERE: (https://github.com/yanningcui/thesis/blob/master/thesis_draft.md)

This finding suggests that **both Donald Trump and Hillary Clinton utilized rhetorical techniques to resonate with their supporters during the campaign, and adopted communication strategies to present a likable self-image. While Hillary Clinton put more emphasis on feminine issues and talked about different political topics, Donald Trump repeatedly launched attacks on his opponents on Twitter, emphasized his personal charm, and used his "bluntness" as a rhetorical technique. Voters might receive his outrageous speech as pragmatic and refreshing.**

This finding continues to build on the literature of computational linguistic differences between political parties on social media. It also provides another exploratory look at the newest form of campaigning: Twitter, and interesting insight into a relevant contemporary social problem.

I also want to propose several directions to develop my work beyond this thesis. Regarding the sample size, I will address more candidates from the two parties throughout the campaign season, to have a more representative sample. If there still exists such differences in the language usage, we might conclude that the differences are caused by different parties and ideologies. I will also introduce more data sources, including debates, speeches, conferences, and media coverage, to conduct a more thorough content analysis of the language uses. As for the methodologies, I propose to experiment with machine learning approaches for sentiment analysis of tweets, rather than a simple word count. I will train sentiment classifiers for Twitter datasets with both supervised and unsupervised models, and predict positive/negative sentiment classification. 
