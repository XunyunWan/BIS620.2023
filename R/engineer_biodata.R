#' Engineer Biodata Features
#'
#' This function enhances a biodata data frame by adding new categorical features 
#' based on existing variables such as age, BMI, treatment, KRAS status, sex, and race. 
#' It creates age groups, BMI categories, and various combinations of treatment 
#' with KRAS status, age group, sex, and race.
#'
#' @param biodata A data frame containing patient biodata. 
#' The data frame must have the columns: AGE, BMI, ATRT (treatment), KRAS, SEX, and RACE.
#'
#' @return The function returns the modified biodata data frame with added columns:
#' `age_group` categorizing patients into 'mid_age' or 'old_age' based on their AGE;
#' `bmi_group` categorizing patients based on their BMI into 'underweight', 'normal_weight', or 'overweight';
#' `arm` combining treatment and KRAS status;
#' `arm_age` combining treatment and age group;
#' `arm_sex` combining treatment and sex;
#' `arm_bmi` combining treatment and BMI category;
#' `arm_race` combining treatment and race.
#'
#' @examples
#' # Assuming biodata is a data frame with the required columns
#' data(NCT00364013, package = "bis620.2023")
#' dl <- NCT00364013
#' engineered_data <- engineer_biodata(get_biodata(dl))
#'
#' @export
engineer_biodata <- function(biodata) {
  biodata$age_group <- ifelse(biodata$AGE < 56, "mid_age",
                              ifelse(biodata$AGE >= 56, "old_age", NA))
  biodata$bmi_group <- ifelse(biodata$BMI < 18, "underweight",
                              ifelse(biodata$BMI >= 18 & biodata$BMI < 25, "normal_weight", 
                                     ifelse(biodata$BMI >= 25, "overweight", NA)))
  biodata$arm <- paste(biodata$ATRT, biodata$KRAS, sep = ", ")
  biodata$arm_age <- paste(biodata$ATRT, biodata$age_group, sep = ", ")
  biodata$arm_sex <- paste(biodata$ATRT, biodata$SEX, sep = ", ")
  biodata$arm_bmi <- paste(biodata$ATRT, biodata$bmi_group, sep = ", ")
  biodata$arm_race <- paste(biodata$ATRT, biodata$RACE, sep = ", ")
  return(biodata)
}
