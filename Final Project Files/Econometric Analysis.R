# First regression: log of ACRES on log of HPI without TWFE
regress_1 <- feols(ln_HPI_base_2000 ~ ln_ACRES, clean_econo)
summary(regress_1)

# Second regression: log of ACRES on log of HPI with county and year TWFE
regress_2 <- feols(ln_HPI_base_2000 ~ ln_ACRES + FIPS + YEAR, clean_econo)
summary(regress_2)

# Third regression: log of ACRES on log of HPI with polynomials but without TWFE
regress_3 <- feols(ln_HPI_base_2000 ~ ln_ACRES + I(ln_ACRES^2) + I(ln_ACRES^3), clean_econo)
summary(regress_3)
print(regress_3$coeftable)

# Fourth regression:
regress_4 <- feols(ln_HPI_base_2000 ~ ln_ACRES + I(ln_ACRES^2) + I(ln_ACRES^3) + I(ln_ACRES^4) + FIPS + YEAR, clean_econo)
summary(regress_4)

# Create table of coefficients
regression_table <- esttable(regress_1, regress_2, regress_3, regress_4,
                             drop = c("FIPS", "YEAR"),
                             headers = c("Base", "With TWFE", "With Polynomials", "With Polynomials and TWFE")
)

# View and save
print(regression_table)

# Screenshot