library(readxl)
library(highcharter)
library(data.table)
library(lubridate)

.file = "C:\\Users\\Aubur\\github\\auburngrads\\LMI\\data\\12_2021 CBM+ Monthly Metrics Data.xlsx"
.sheets = readxl::excel_sheets(.file)

this_sheet = 7

.df   = readxl::read_excel(.file, sheet = .sheets[this_sheet])

.last_name <- .df[[1]][nrow(.df)]

cnames = colnames(.df)

for(i in 2:length(cnames)){

    .df[[cnames[i]]] <- tidyr::replace_na(.df[[cnames[i]]],0)

}

.df_longer = .df %>% tidyr::pivot_longer(cols = -1)

.df_longer %>% setnames(c("NIIN", "Date", "Value"))

.df_longer$Date <- as.Date(as.numeric(.df_longer$Date),
                               origin = '1899-12-30')

ggplot(subset(.df_longer, NIIN != .last_name),
       aes(x = Date, y = Value, color = NIIN)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2020-02-01"),lwd = 1)+
  facet_wrap(facets = "NIIN") +
  theme(legend.position = "none") +
  ylab(.sheets[this_sheet])

highchart() %>%
  hc_add_series(data = subset(.micaps_longer,NIIN != "CBM+ Monthly MICAPS"),
                type = "line",
                mapping = hcaes(x = Date, y = MICAPS, group = NIIN),
                showInLegend = FALSE) %>%
  hc_xAxis(type = 'date',
           crosshair = T,
           title = list(text = "Date")) %>%
  hc_yAxis(type = "linear",
           crosshair = !T,
           title = list(text = "MICAPS"))

ggplot(subset(.micaps_longer,NIIN != "CBM+ Monthly MICAPS"),
        aes(x = Date, y = MICAPS, color = NIIN)) +
  geom_line() +
facet_wrap(facets = "NIIN")


.nins = unique(.data$NIIN)

.data$month <- ceiling_date(.data$OldestorderDate, 'month') %m-% days(1)

setDT(.data, key = c("NIIN", "month"))





.chk = .data[,OpenMICAPBackorders, by = c("NIIN","month")]

.chk[!is.na(month) & NIIN == "011355681",]
.data[NIIN == "011355681",]


