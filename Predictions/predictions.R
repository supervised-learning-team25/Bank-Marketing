test<-read.csv('data/raw/bank_marketing_test.csv')[-1]

test_bin <- test

test_bin$cons.price.idx <- ifelse(test_bin$cons.price.idx < 93, 1, 0)
names(test_bin)[names(test_bin) == "cons.price.idx"] <- "low_cpi"

test_bin$cons.conf.idx <- ifelse(test_bin$cons.conf.idx > median(train$cons.conf.idx), 1, 0)
names(test_bin)[names(test_bin) == "cons.conf.idx"] <- "high_cci"

test_bin$euribor3m <- ifelse(test_bin$euribor3m < mean(train$euribor3m), 1, 0)
names(test_bin)[names(test_bin) == "euribor3m"] <- "low_euribor"

test_bin$emp.var.rate <- ifelse(test_bin$emp.var.rate < 0, 1, 0)
names(test_bin)[names(test_bin) == "emp.var.rate"] <- "negative_emp"

test_bin$university<-ifelse(test_bin$education=='university.degree', 1, 0)
test_bin$p_course<-ifelse(test_bin$education=='professional.course', 1, 0)
test_bin <- subset(test_bin, select = -education)

test_bin$job_student <- ifelse(test_bin$job == "student", 1, 0)
test_bin$job_retired <- ifelse(test_bin$job == "retired", 1, 0)
test_bin$job_admin <- ifelse(test_bin$job == "admin.", 1, 0)
test_bin <- subset(test_bin, select = -job)

test_bin$month_sep <- ifelse(test_bin$month == "sep", 1, 0)
test_bin$month_oct <- ifelse(test_bin$month == "oct", 1, 0)
test_bin$month_dec <- ifelse(test_bin$month == "dec", 1, 0)
test_bin$month_mar <- ifelse(test_bin$month == "mar", 1, 0)

test_bin$p_failure <- ifelse(test_bin$poutcome == "failure", 1, 0)
test_bin$p_success <- ifelse(test_bin$poutcome == "success", 1, 0)
test_bin <- subset(test_bin, select = -poutcome)

test_bin$contact <- ifelse(test_bin$contact == "cellular", 1, 0)
names(test_bin)[names(test_bin) == "contact"] <- "cellular"

test_bin$marital <- ifelse(test_bin$marital == "single", 1, 0)
names(test_bin)[names(test_bin) == "marital"] <- "single"

test_bin$campaign <- ifelse(test_bin$campaign > 5, 0, 1)
names(test_bin)[names(test_bin) == "campaign"] <- "low_call"


drop_var<-c('pdays', "default", 'housing' ,"loan", "day_of_week", 
            "month", "nr.employed", 'job', "duration", "emp_cat")

test_df <- test_bin[, !(names(test_bin) %in% drop_var)]

probs_rf <- predict(rf_model, newdata = test_df, type = "prob")[, "Yes"]
pred <- ifelse(probs_rf >= 0.03, "Yes", "No")

round(table(pred)/length(pred), 2) #86%/14%

sub <- data.frame(prediction = pred)
write.csv(sub, "Predictions/predictions.csv", row.names = FALSE)
