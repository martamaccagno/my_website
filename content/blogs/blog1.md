---
categories:
- ""
- ""
date: "2020-10-05"
description: "Instagram and Snapchat usage by sex"
draft: false
image: INSTA_SNAP.jpg
keywords: ""
slug: blog1
title: Instagram and Snapchat usage
---

```{r, read_gss_data, cache=TRUE}

gss <- read_csv(here::here("data", "smallgss2016.csv"), 
                na = c("", "Don't know",
                       "No answer", "Not applicable"))
```


## Instagram and Snapchat, by sex

Can we estimate the *population* proportion of Snapchat or Instagram users in 2016?

1. We create a  new variable, `snap_insta` that is *Yes* if the respondent reported using any of Snapchat (`snapchat`) or Instagram (`instagrm`), and *No* if not. If the recorded value was NA for both of these questions, the value in your new variable should also be NA.

```{r, clean_data}
#addition of new column that shows who uses either or both platforms, none, or didnt proivde data (Yes, No, NA)
gss <- gss %>% 
  mutate(snap_insta= ifelse(snapchat=="Yes" | instagrm =="Yes", "Yes", ifelse(snapchat== "NA" & instagrm== "NA", "NA", "No")))

gss
```

2. We calculate the proportion of Yes’s for `snap_insta` among those who answered the question, i.e. excluding NAs.

```{r, prop_yes}
# this variable counts all people who sumbitted an answer 
sum_users_answer <- sum(gss$snap_insta!= "NA")

#this calculates the proportion of people using platforms (Yes/(Yes+No))
prop_yes_gss <-  gss %>% 
  summarise(prop_yes = (sum(ifelse(snap_insta=="Yes", 1, 0))/sum_users_answer))
                        
prop_yes_gss
```

3. Using the CI formula for proportions, we construct 95% CIs for men and women who used either Snapchat or Instagram

```{r, snap_insta}

#women who use either Snapchat or Instagram

#sum for women who use either Snapchat or Instagram
sum_women_snap_insta <- sum(gss$snap_insta != "NA" & gss$sex=="Female") 

#proportion of women using platforms (Yes/(Yes+No))
prop_yes_gss_women <-  gss %>% 
  filter (sex=="Female") %>%
  summarise(prop_yes = (sum(ifelse(snap_insta=="Yes", 1, 0))/sum_women_snap_insta))
                        
#standard erros and the average for women 
stan_error_women <- sqrt(prop_yes_gss_women*(1-prop_yes_gss_women)/ (sum_women_snap_insta))
mean_snap_insta_women <- prop_yes_gss_women 

#CI of 95% for women who use either Snapchat or Instagram
critical_t_value <- qt(0.975,  sum_women_snap_insta -1)
lower_CI_women <- mean_snap_insta_women - (critical_t_value*stan_error_women)
upper_CI_women <- mean_snap_insta_women + (critical_t_value*stan_error_women)


#men who use either Snapchat or Instagram

#sum for men who use either Snapchat or Instagram
sum_men_snap_insta <- sum(gss$snap_insta != "NA" & gss$sex=="Male") 

#proportion of men using platforms (yes/(yes+no))
prop_yes_gss_men <-  gss %>% 
  filter (sex=="Male") %>%
  summarise(prop_yes = (sum(ifelse(snap_insta=="Yes", 1, 0))/sum_men_snap_insta))
                        
#standard erros and the average for men 
stan_error_men <- sqrt(prop_yes_gss_men*(1-prop_yes_gss_men)/ (sum_men_snap_insta))
mean_snap_insta_men <- prop_yes_gss_men

#CI of 95% for men who use either Snapchat or Instagram
critical_t_value <- qt(0.975,  sum_men_snap_insta -1)
lower_CI_men <- mean_snap_insta_men - (critical_t_value*stan_error_men)
upper_CI_men <- mean_snap_insta_men + (critical_t_value*stan_error_men)

#print CI of 95% for women and men
paste("Proportion of women using either Snapchat or Instagram:", prop_yes_gss_women)
paste("CI 95% women:", "[", lower_CI_women, "-", upper_CI_women, "]")
paste("Proportion of men using either Snapchat or Instagram:", prop_yes_gss_men)
paste("CI 95% men:", "[", lower_CI_men, "-", upper_CI_men, "]")
```
