library(httr)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(lubridate)

#1st Step: Accessing the API of covid.19.go.id
set_config(config(ssl_verifypeer = 0L))
resp_kaltim <- GET("https://data.covid19.go.id/public/api/prov_detail_KALIMANTAN_TIMUR.json")

#Evaluate the status using
status_code(resp)
#or
resp$status_code
identical(resp$status_code, status_code(resp))

#extracting response
cov_kaltim_raw <- content(resp_kaltim, as = "parsed", simplifyVector = TRUE)

#extracting few data
names(cov_kaltim_raw)
cov_kaltim_raw$kasus_total
cov_kaltim_raw$meninggal_persen
cov_kaltim_raw$sembuh_persen

cov_kaltim <- cov_kaltim_raw$list_perkembangan
str(cov_kaltim)
head(cov_kaltim)

#data wrangling
new_cov_kaltim <-
  cov_kaltim %>%
  select(-contains("DIRAWAT_OR_ISOLASI")) %>%
  select(-starts_with("AKUMULASI")) %>%
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>%
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(new_cov_kaltim)

#visualize confirmed case trend
ggplot(new_cov_kaltim, aes(x = tanggal, y = kasus_baru)) +
	geom_col(fill = "salmon") +
	labs(
	x = NULL,
	y = "Jumlah Kasus",
	title = "Kasus Harian Positif COVID-19 di Kalimantan Timur",
	subtitle = "Terjadi lonjakan kasus di awal tahun 2022 akibat varian baru Omicron",
	caption = "Sumber data : covid.19.go.id") +
	theme_ipsum(
		base_size = 13,
		plot_title_size = 21,
		grid = "Y",
		ticks = TRUE) +
theme(plot.title.position = "plot")

#visualize recovered case trend
ggplot(new_cov_kaltim, aes(x = tanggal, y = sembuh)) +
	geom_col(fill = "olivedrab2") +
	labs(
	x = NULL,
	y = "Jumlah Kasus",
	title = "Kasus Harian Sembuh dari COVID-19 di Kalimantan Timur",
	caption = "Sumber data: covid.19.go.id") +
	theme_ipsum(
		base_size = 13,
		plot_title_size = 21,
		grid = "Y",
		ticks = TRUE)

#visualize death trend
ggplot(new_cov_kaltim, aes(x = tanggal, y = meninggal)) +
	geom_col(fill = "darkslategray4") +
	labs(
	x = NULL,
	y = "Jumlah Kasus",
	title = "Kasus Harian Meninggal Akibat COVID-19 di Kalimantan Timur",
	caption = "Sumber data: covid.19.co.id") +
	theme_ipsum(
		base_size = 13,
		plot_title_size = 21,
		grid = "Y",
		ticks = TRUE) +
theme(plot.title.position = "plot")

#organize to weekly report
cov_kaltim_pekanan <- new_cov_kaltim %>%
	count(tahun = year(tanggal),
		pekan_ke = week(tanggal),
		wt = kasus_baru,
		name = "jumlah")
glimpse(cov_kaltim_pekanan)

cov_kaltim_pekanan <- 
	cov_kaltim_pekanan %>%
	mutate(
	jumlah_pekanlalu = dplyr::lag(jumlah, 1),
	jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
		lebih_baik = jumlah < jumlah_pekanlalu)
glimpse(cov_kaltim_pekanan)

#visualize weekly trend
ggplot(cov_kaltim_pekanan[cov_kaltim_pekanan$tahun==2022,],
	aes(x = pekan_ke, y = jumlah, fill = lebih_baik)) +
	geom_col(show.legend = FALSE) +

scale_x_continuous(breaks = 1:9, expand = c(0,0)) +

scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
	labs(
	x = NULL,
	y = "Jumlah Kasus",
	title = "Kasus Pekanan Positif COVID-19 di Kalimantan Timur",
	subtitle = "Kolom hijau menunjukkan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
	caption = "Sumber data : covid.19.go.id" ) +
		theme_ipsum(
		base_size = 13,
		plot_title_size = 21,
		grid = "Y",
		ticks = TRUE) +
theme(plot.title.position = "plot")

#organize to accumulation cases
cov_kaltim_akumulasi <- new_cov_kaltim %>%
	transmute(
	tanggal,
	akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
	akumulasi_sembuh = cumsum(sembuh),
	akumulasi_meninggal = cumsum(meninggal))

tail(cov_kaltim_akumulasi)

#visualization of accumulation cases
ggplot(data = cov_kaltim_akumulasi,
	aes(x = tanggal, y = akumulasi_aktif)) +
	geom_line()
dim(cov_kaltim_akumulasi)

#pivot
cov_kaltim_akumulasi_pivot <- cov_kaltim_akumulasi %>%
gather(
	key = "kategori",
	value = "jumlah",
	-tanggal) %>%
mutate(
	kategori = sub(pattern = "akumulasi_",
	replacement = "", kategori))
dim(cov_kaltim_akumulasi_pivot)
glimpse(cov_kaltim_akumulasi_pivot)

#Another way to arrange the formula for pivot
cov_kaltim_akumulasi_pivot <-
	cov_kaltim_akumulasi %>%
	pivot_longer(
		cols = -tanggal,
		names_to = "kategori",
		names_prefix = "akumulasi_",
		values_to = "jumlah")
dim(cov_kaltim_akumulasi_pivot)
glimpse(cov_kaltim_akumulasi_pivot)

#Dynamics of COVID-19 Case in East Borneo
ggplot(cov_kaltim_akumulasi_pivot,
	aes(tanggal, jumlah, colour = (kategori))) +
	geom_line(size = 0.9) +
	scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
	scale_colour_manual(
		values = c("aktif" = "salmon", "meninggal" = "darkslategray4", "sembuh" = "olivedrab2"),
		labels = c("Aktif", "Meninggal", "Sembuh")) +
			labs(
			x = NULL,
			y = "Jumlah Kasus Akumulasi",
			colour = NULL,
			title = "Dinamika Kasus COVID-19 di Kalimantan Timur",
			caption = "Sumber data: covid.19.go.id") +
				theme_ipsum(
				base_size = 13,
				plot_title_size = 21,
				grid = "Y",
				ticks = TRUE) +
theme(plot.title.position = "plot")

#Dynamics of COVID-19 Case in East Borneo with Legend
ggplot(cov_kaltim_akumulasi_pivot,
	aes(tanggal, jumlah, colour = (kategori))) +
	geom_line(size = 0.9) +
	scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
	scale_colour_manual(
		values = c("aktif" = "salmon", "meninggal" = "darkslategray4", "sembuh" = "olivedrab2"),
		labels = c("Aktif", "Meninggal", "Sembuh")) +
			labs(
			x = NULL,
			y = "Jumlah Kasus Akumulasi",
			colour = NULL,
			title = "Dinamika Kasus COVID-19 di Kalimantan Timur",
			caption = "Sumber data: covid.19.go.id") +
				theme_ipsum(
				base_size = 13,
				plot_title_size = 21,
				grid = "Y",
				ticks = TRUE) +
theme(plot.title = element_text(hjust = 0.5),
	legend.position = "top")

