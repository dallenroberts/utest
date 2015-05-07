################################################################################
## Author: Allen Roberts
## Date Created: March 2015
## Description: Read in raw UTEST data and format for analysis
################################################################################

################################################################################
## SET UP
################################################################################
library(plyr)
library(reshape2)
library(lubridate)

## Set version
version <- "UTESTData_DATA_2015-03-25_1545"

## Load data
data <- read.csv(paste0("data/raw/", version, ".csv"), stringsAsFactors = FALSE)

## Drop test data
data <- data[!data$tester %in% c("Allen", ""), ]

################################################################################
## FORMATTING
################################################################################
## Unique ID
if(!length(unique(data$survey_id)) == nrow(data)) stop("survey_id is not unique")

## Drop redcap_survey_identifer
data <- data[, !names(data) %in% "redcap_survey_identifier"] 

## Entry date
data$entry_date <- as.Date(data$combined_timestamp)

## Site location
data$site_location[data$site_location %in% c("sasg", "ASG", "SASG85")] <- "SASG"

## Age
data$age[data$age > 100] <- NA

## Zip code
data$zip_code[data$zip_code < 10000] <- NA

## Testers
data$tester[data$tester == "Ben Lang"] <- "Benjamin Lang"
data$tester[data$tester == "Christina "] <- "Christina Tolley"
data$tester[data$tester == "Jesse"] <- "Jesse Maupin"
data$tester[data$tester %in% c("Kellen Dunden", "Kelly Dundan")] <- "Kelly Dundon"
data$tester[data$tester %in% c("Kirsten Hansen Day", "Kristen Hansen Day")] <- "Kirsten Hansen-Day"
data$tester[data$tester == "Lindsay"] <- "Lindsay Capron"
data$tester[data$tester == "Liza Hutchinson"] <- "Eliza Hutchinson"
data$tester[data$tester == "Derek"] <- "Derek Blechinger"
data$tester[data$tester %in% c("Peter ", "Peter Meztger")] <- "Peter Metzger"
data$tester[data$tester %in% c("Treavor", "Treavor Robison")] <- "Treavor Robinson"
data$tester[data$tester == "Anna"] <- "Anna Greer"
data$tester[data$tester == "Braiden"] <- "Braiden Eilers"
data$tester[data$tester == "Brandon"] <- "Brandon Maust"
data$tester[data$tester == "Elly"] <- "Elly Osborn"
data$tester[data$tester == "Ryan"] <- "Ryan Smith"
data$tester[data$tester == "Shane"] <- "Shane Collins"
data$tester[data$tester == "Sophie"] <- "Sophie Miller"
data$tester[data$tester == "Tory Olson"] <- "Tory Olsen"
data$tester[grepl("Loc", data$tester)] <- "Loc Duong"

## Observers
data$observer <- gsub(",", "&", data$observer)
data$observer <- gsub(" and ", " & ", data$observer)
data$observer[data$observer == "Sheyda Moshia& Bea Lang"] <- "Sheyda Moshia & Ben Lang"

observers <- strsplit(data$observer, " & ")
data$observer_1 <- sapply(observers, function(x) x[1])
data$observer_2 <- sapply(observers, function(x) x[2])

data$observer_1[data$observer_1 %in% c("AJ", "AT Warr")] <- "AJ Warr"
data$observer_1[data$observer_1 %in% c("Alex", "Alex Greer Phillips")] <- "Alex Greer-Phillips"
data$observer_1[data$observer_1 %in% c("Ana Chartiev")] <- "Ana Chartier"
data$observer_1[data$observer_1 %in% c("Anna")] <- "Anna Greer"
data$observer_1[data$observer_1 %in% c("Ariel Desure")] <- "Ariell Desure"
data$observer_1[data$observer_1 %in% c("Becca Briggs")] <- "Rebecca Briggs"
data$observer_1[data$observer_1 %in% c("Ben Lang")] <- "Benjamin Lang"
data$observer_1[data$observer_1 %in% c("Braiden")] <- "Braiden Eilers"
data$observer_1[data$observer_1 %in% c("Brandon")] <- "Brandon Maust"
data$observer_1[data$observer_1 %in% c("Christina")] <- "Christina Tolley"
data$observer_1[data$observer_1 %in% c("Daron", "Dovon")] <- "Daron Vandeleur"
data$observer_1[data$observer_1 %in% c("Eliza", "Liza")] <- "Eliza Hutchinson"
data$observer_1[data$observer_1 %in% c("Elly Osburn")] <- "Elly Osborn"
data$observer_1[data$observer_1 %in% c("Emily")] <- "Emily M."
data$observer_1[data$observer_1 %in% c("Eric")] <- "Eric Bautista"
data$observer_1[data$observer_1 %in% c("Ilya")] <- "Ilya Golovaty"
data$observer_1[data$observer_1 %in% c("Jesse", "Jesse Manpin")] <- "Jesse Maupin"
data$observer_1[data$observer_1 %in% c("Josh Frnaland")] <- "Josh Frankland"
data$observer_1[data$observer_1 %in% c("Kelly", "Kelly Dundan", "Kelly Dandon")] <- "Kelly Dundon"
data$observer_1[data$observer_1 %in% c("Kirsten Hansen Day")] <- "Kirsten Hansen-Day"
data$observer_1[data$observer_1 %in% c("Kseniya")] <- "Kseniya Deryckx"
data$observer_1[data$observer_1 %in% c("Lauren")] <- "Lauren Campbell"
data$observer_1[data$observer_1 %in% c("Mark")] <- "Mark McGrath"
data$observer_1[data$observer_1 %in% c("Mimi")] <- "Mimi Kennelly"
data$observer_1[data$observer_1 %in% c("Shane")] <- "Shane Collins"
data$observer_1[data$observer_1 %in% c("Sheyda", "Sheyda Moshia")] <- "Sheyda Moshiri"
data$observer_1[data$observer_1 %in% c("Sophie")] <- "Sophie Miller"
data$observer_1[data$observer_1 %in% c("Tori", "Tory Olson")] <- "Tory Olsen"
data$observer_1[data$observer_1 %in% c("Treavor")] <- "Treavor Robinson"

data$observer_2[data$observer_2 == "Ben Lang"] <- "Benjamin Lang"

## How found UTEST
data <- rename(data, c("found_other" = "found_othertext"))
data <- rename(data, c("how_found___1" = "found_current_sasg_client", "how_found___2" = "found_sasg_website", "how_found___3" = "found_google_search", "how_found___4" = "found_referred_friend", "how_found___5" = "found_other"))

## Gender
data <- rename(data, c("gender_other" = "gender_othertext"))
data <- rename(data, c("gender___1" = "gender_male", "gender___2" = "gender_female", "gender___3" = "gender_mtf", "gender___4" = "gender_ftm", "gender___5" = "gender_genderqueer", "gender___6" = "gender_other"))

## Race
names(data)[which(names(data) == "race_other")] <- "race_othertext"
data <- rename(data, c("race___1" = "race_afr_amer", "race___2" = "race_asian", "race___3" = "race_latino", "race___4" = "race_native_amer", "race___5" = "race_pac_isla", "race___6" = "race_white", "race___7" = "race_foreign_born", "race___8" = "race_other"))

## Last HIV Test - move this to month, year
data$last_hiv_test_date[data$last_hiv_test_date == "2011 - 2013"] <- "2012"
data$last_hiv_test_date <- gsub("[.]", "/", data$last_hiv_test_date)
data$last_hiv_test_date[data$last_hiv_test_date == "12/15/14"] <- "12/2014"
data$last_hiv_test_date[data$last_hiv_test_date == "<5 yr"] <- "2010"
last_hiv_test_date <- strsplit(data$last_hiv_test_date, "/")

data$last_hiv_test_date_mo <- as.numeric(sapply(last_hiv_test_date, function(x) {
	if(length(x) < 2) {
		return(NA)
	} else {
		return(x[1])
	}
}))

data$last_hiv_test_date_yr<- as.numeric(sapply(last_hiv_test_date, function(x) {
		if(length(x) == 0) {
			return(NA)
		} else {
			return(x[length(x)])
		}
}))
data$last_hiv_test_date_mo[data$last_hiv_test_date_mo == 0] <- NA
data$last_hiv_test_date_yr[data$last_hiv_test_date_yr > 20 & data$last_hiv_test_date_yr < 100 & !is.na(data$last_hiv_test_date_yr)] <- data$last_hiv_test_date_yr[data$last_hiv_test_date_yr > 20 & data$last_hiv_test_date_yr < 100 & !is.na(data$last_hiv_test_date_yr)]  + 1900
data$last_hiv_test_date_yr[data$last_hiv_test_date_yr < 20 & !is.na(data$last_hiv_test_date_yr)] <- data$last_hiv_test_date_yr[data$last_hiv_test_date_yr < 20 & !is.na(data$last_hiv_test_date_yr)]  + 2000

## Last HIV test result
data$last_hiv_test_result[data$last_hiv_test_result == 1] <- "Negative"
data$last_hiv_test_result[data$last_hiv_test_result == 2] <- "Positive"
data$last_hiv_test_result[data$last_hiv_test_result == 3] <- "Never"

## Risk category
data <- rename(data, c("risk_category___1" = "risk_cat_sex_men", "risk_category___2" = "risk_cat_sex_women", "risk_category___3" = "risk_cat_sex_trans", "risk_category___4" = "risk_cat_sex_hiv", "risk_category___5" = "risk_cat_sex_idu", "risk_category___6" = "risk_cat_sex_drugs_money", "risk_category___7" = "risk_cat_inject_drugs"))

## Sexual history
apply(data[, 41:56], 2, max, na.rm = TRUE)

## Risk Behavior: IDU
data <- rename(data, c("sex_idu_behavior___1" = "sex_idu_events_past_2mo", "sex_idu_behavior___2" = "sex_idu_events_past_year", "sex_drug_behavior___1" = "sex_drugs_money_events_past_2mo", "sex_drug_behavior___2" = "sex_drugs_money_events_past_year", "idu_behavior___1" = "inject_drugs_events_past_2mo", "idu_behavior___2" = "inject_drugs_events_past_year"))

## Substance Use
data <- rename(data, c("substance_use_other" = "substance_use_othertext"))
data <- rename(data, c("substance_use_gen___1" = "substance_use_current", "substance_use_gen___2" = "substance_use_recovery", "substance_use_specific___1" = "substance_use_alcohol", "substance_use_specific___2" = "substance_use_cocaine", "substance_use_specific___3" = "substance_use_ghb", "substance_use_specific___4" = "substance_use_heroin", "substance_use_specific___5" = "substance_use_k", "substance_use_specific___6" = "substance_use_meth", "substance_use_specific___7" = "substance_use_poppers", "substance_use_specific___8" = "substance_use_tobacco", "substance_use_specific___9" = "substance_use_x", "substance_use_specific___10" = "substance_use_marijuana", "substance_use_specific___11" = "substance_use_other"))

## Condom use
data$condom_use[data$condom_use == 1] <- "Always"
data$condom_use[data$condom_use == 2] <- "Usually"
data$condom_use[data$condom_use == 3] <- "Sometimes"
data$condom_use[data$condom_use == 4] <- "Never"

## Last Unprotected Intercourse
data <- rename(data, c("last_unprotect_sex___1" = "last_unprotect_anal", "last_unprotect_sex___2" = "last_unprotect_vaginal"))

## Anal Intercourse Action
data <- rename(data, c("anal_sex_action___1" = "anal_sex_insert", "anal_sex_action___2" = "anal_sex_recep", "anal_sex_action___3" = "anal_sex_recep_ejac"))

## Vaginal Intercourse Action
data <- rename(data, c("vag_sex_action___1" = "vag_sex_insert", "vag_sex_action___2" = "vag_sex_recep", "vag_sex_action___3" = "vag_sex_recep_ejac"))

## Test result
data$test_result_today[data$test_result_today == 1] <- "Non-Reactive"
data$test_result_today[data$test_result_today == 2] <- "Reactive"
data$test_result_today[data$test_result_today == 3] <- "Invalid"

## Expiration date
data$expiration_date <- gsub("-", "/", data$expiration_date)
data$expiration_date[data$expiration_date == "2014/11/07"] <- "11/2014"
data$expiration_date[data$expiration_date == "08/12/15"] <- "08/2015"
data$expiration_date[data$expiration_date == "10/22/15"] <- "10/2015"
data$expiration_date[data$expiration_date == "10/22/2015"] <- "10/2015"
data$expiration_date[data$expiration_date == "08/19/2015"] <- "08/2015"
expiration_dates <- strsplit(data$expiration_date, "/")
data$expiration_date_mo <- as.numeric(sapply(expiration_dates, function(x) x[1]))
data$expiration_date_mo[data$expiration_date_mo == 15] <- 5
data$expiration_date_yr <- as.numeric(sapply(expiration_dates, function(x) x[2]))
data$expiration_date_yr[data$expiration_date_yr > 10 & data$expiration_date_yr <= 15 & !is.na(data$expiration_date_yr)] <- data$expiration_date_yr[data$expiration_date_yr > 10 & data$expiration_date_yr <= 15 & !is.na(data$expiration_date_yr)] + 2000
data$expiration_date_yr[data$expiration_date_yr %in% c(2, 19)] <- 2015
data$expiration_date_yr[data$expiration_date_yr == 4] <- 2014

## Referrals
data <- rename(data, c("referral_other" = "referral_othertext"))
data <- rename(data, c("referral___1" = "referral_medical", "referral___2" = "referral_mental", "referral___3" = "referral_sti", "referral___4" = "referral_chemical", "referral___5" = "referral_needle_exch", "referral___6" = "referral_hep_a", "referral___7" = "referral_hep_b", "referral___8" = "referral_hpv_vac", "referral___9" = "referral_other"))

## Order columns