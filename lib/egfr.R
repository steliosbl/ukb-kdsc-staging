# CKD-EPI 2021 eGFR Estimation Functions
# Based on: Inker et al. NEJM 2021
#
# Both functions expect creatinine in mg/dL.
# If creatinine is in µmol/L, convert by dividing by 88.4.

egfr_creat_2021 <- function(creat, age, sex) {
    u <- 142
    a1 <- if_else(sex == "Female", -0.241, -0.302)
    a2 <- -1.200
    c <- 0.9938
    d <- if_else(sex == "Female", 1.012, 1)
    k <- if_else(sex == "Female", 0.7, 0.9)

    u * pmin(creat / k, 1)^a1 * pmax(creat / k, 1)^a2 * (c^age) * d
}

egfr_creat_cys_2021 <- function(creat, cyst, age, sex) {
    u <- 135
    a1 <- if_else(sex == "Female", -0.219, -0.144)
    a2 <- -0.544
    b1 <- -0.323
    b2 <- -0.778
    c <- 0.9961
    d <- if_else(sex == "Female", 0.963, 1)
    k <- if_else(sex == "Female", 0.7, 0.9)

    u * (
        pmin(creat / k, 1)^a1 * pmax(creat / k, 1)^a2 *
            pmin(cyst / 0.8, 1)^b1 * pmax(cyst / 0.8, 1)^b2 *
            (c^age) * d
    )
}
