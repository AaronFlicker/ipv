library(tidyverse)
library(lubridate)
library(odbc)
library(DBI)
library(readxl)
con <- dbConnect(odbc::odbc(), "ClarityProd")

ipv <- dbGetQuery(con, "
  SELECT bp.pat_mrn_id
        ,CAST(bp.birth_date AS DATE) AS birth_date
        ,CAST(fp.reporting_date AS DATE) AS ReportMonth
        ,fp.numerator
        ,fp.denominator
        ,CAST(fp.num_component_02 AS DATE) AS IPV1_Date
        ,fp.num_component_11 AS WWC2Mo
        ,fp.den_component_01 AS Division
        ,fp.den_component_02 AS Site
        ,a.addr_hx_line1
		    ,a.addr_hx_line2
		    ,a.city_hx
		    ,a.state
		    ,a.zip_hx
		    ,g.geoid
		    ,t.arealabel
		    ,CASE WHEN t.arealabel in ('N. Fairmount', 'S. Fairmount') 
		        THEN 'North and South Fairmount'
		      WHEN t.arealabel in ('Lower Price Hill-Queensgate', 'E. Price Hill')
		        THEN 'Lower/East Price Hill'
		      ELSE t.arealabel END AS Neighborhood
  FROM datamart_outcomes.dbo.measure_fact_metadata fmd
    INNER JOIN datamart_outcomes.dbo.measure_fact fp
      ON fmd.measure_number = fp.measure_number
    INNER JOIN hpceclarity.bmi.patient bp
      ON fp.pat_id = bp.pat_id
    INNER JOIN hpceclarity.dbo.chmc_adt_addr_hx a
			on bp.pat_id = a.pat_id
		INNER JOIN temptable.dbo.full_list_geocode g
			ON (a.addr_hx_line1 = g.add_line_1 
			    OR (a.addr_hx_line1 IS NULL AND g.add_line_1 IS NULL))
				AND (a.addr_hx_line2 = g.add_line_2 
				  OR (a.addr_hx_line2 IS NULL AND g.add_line_2 IS NULL))
				AND (a.city_hx = g.city OR (a.city_hx IS NULL AND g.city IS NULL))
				AND (a.state = g.state OR (a.state IS NULL and g.state IS NULL))
				AND (a.zip_hx = g.zip OR (a.zip_hx IS NULL AND g.zip IS NULL))
		INNER JOIN temptable.dbo.tract_to_neighborhood t
			ON g.geoid = t.censustract11
  WHERE fmd.measure_number = 9411
    AND fp.num_component_01 >= a.eff_start_date
		AND (fp.num_component_01 < a.eff_end_date OR a.eff_end_date IS NULL)
		AND t.arealabel in ('Roll Hill', 
							          'Lower Price Hill-Queensgate', 
							          'E. Price Hill', 
							          'Winton Hills', 
							          'S. Cumminsville-Millvale', 
							          'N. Fairmount', 
							          'S. Fairmount', 
							          'Carthage',
							          'West End',
							          'OTR-Pendleton',
							          'Avondale',
							          'Walnut Hills'
							          )
                  ") |>
  mutate(
    Date76DaysOld = birth_date+76,
    WWC2 = !is.na(WWC2Mo)
    )

totals <- filter(ipv, ReportMonth >= "2021-01-01") |>
  group_by(ReportMonth) |>
  summarise(
    Total = n(),
    Vaccinated = sum(numerator),
    WWC2 = sum(WWC2)
    ) |>
  pivot_longer(
    cols = c(Vaccinated, WWC2),
    names_to = "Measure"
  ) |>
  mutate(Pct = value/Total) |>
  group_by(Measure) |>
  mutate(
    Center = mean(Pct),
    Sigma = sqrt((Center*(1-Center))/Total),
    UCL = Center + (3 * Sigma),
    LCL = Center - (3 * Sigma),
    xlabel = paste0(ReportMonth, " (n = ", Total, ")"),
    Goal = ifelse(Measure == "Vaccinated", .8, NA),
    Measure = ifelse(Measure == "WWC2", "Well Child Visit (2 mo.)", Measure)
    )

monthlist <- seq.Date(max(totals$ReportMonth), min(totals$ReportMonth), by = "-1 month")
monthlist <- sort(monthlist)
nlist <- sort(unique(totals$xlabel))
#nlist <- nlist[which(1:length(nlist)%%2 == length(nlist)%%2)]

ggplot(totals, aes(x = ReportMonth, y = Pct)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 2, color = "blue") +
  facet_grid(Measure~.) +
  scale_y_continuous(
    limits = c(.3, 1), 
    breaks = seq(.4, 1, .1), 
    labels = seq(40, 100, 10)
    ) +
  labs(
    x = NULL, 
    y = "%", 
    title = "% of 76-day-old Gen Peds Patients Who Have Received an IPV Vaccine
    \n in 10 Most Structurally Deprived Neighborhoods"
    ) +
  scale_x_date(breaks = monthlist, labels = nlist) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90),
    plot.title = element_text(hjust = .5)
    ) +
  geom_line(aes(x = ReportMonth, y = Center), color = "red") +
  geom_line(aes(x = ReportMonth, y = UCL), color = "red", linetype = "dashed") +
  geom_line(aes(x = ReportMonth, y = LCL), color = "red", linetype = "dashed") +
  geom_line(
    aes(x = ReportMonth, y = Goal), 
    color = "green",
    linetype = "twodash"
    )
ggsave("IPV_chart.pdf")











totals |>
  ggplot(aes(x = Month, y = Pct)) +
  geom_point(size = 2, color = "blue") +
  geom_line(linewidth = 1, color = "blue") +
  facet_grid(variable~.) +
  scale_y_continuous(
    limits = c(0, 1), 
    breaks = seq(0, 1, .2), 
    labels = seq(0, 100, 20)
  ) +
  labs(
    x = NULL, 
    y = "% of patients", 
    title = "% of patients with food insecurity who have it resolved within four months"
  ) +
  scale_x_date(
    breaks = seq.Date(as.Date("2022-01-01"), max(totals$Month), "months"),
    labels = xlabel
  ) +
  theme(
    plot.title.position = "plot", 
    plot.title = element_text(hjust = .5),
    axis.text.x = element_text(angle = 90)
  ) +
  geom_line(aes(x = Month, y = UC), color = "red", linetype = "dashed") +
  geom_line(aes(x = Month, y = LC), color = "red", linetype = "dashed") +
  geom_line(aes(x = Month, y = Pbar), color = "green", linewidth = 1)