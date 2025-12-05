# Практическая работа 5
artem23mnd@yandex.ru

# Исследование информации о состоянии беспроводных сетей

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки.
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI.
3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Операционная система: Windows 10
2.  Среда разработки: RStudio
3.  Версия интерпретатора R: 4.5.1

## Ход работы

1.  Импортируем данные wifi –
    https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv и
    https://github.com/AutomationNetTech/Vendor-OUI-MongoDB-Collection/blob/main/vendor_oui_id_is_oui_prefix.json;
    подготовим данные для дальнейшего анализа.

2.  Проведем анализ датасета с точками доступа: 2.1 Определить
    небезопасные точки доступа (без шифрования – OPN) 2.2 Определить
    производителя для каждого обнаруженного устройства 2.3 Выявить
    устройства, использующие последнюю версию протокола шифрования WPA3,
    и названия точек доступа, реализованных на этих устройствах 2.4
    Отсортировать точки доступа по интервалу времени, в течение которого
    они находились на связи, по убыванию 2.5 Обнаружить топ-10 самых
    быстрых точек доступа. 2.6 Отсортировать точки доступа по частоте
    отправки запросов (beacons) в единицу времени по их убыванию.

3.  Проведем анализ датасета с клиентами: 3.1 Определить производителя
    для каждого обнаруженного устройства 3.2 Обнаружить устройства,
    которые НЕ рандомизируют свой MAC адрес 3.3 Кластеризовать запросы
    от устройств к точкам доступа по их именам Определить время
    появления устройства в зоне радиовидимости и время выхода его из нее
    3.4 Оценить стабильность уровня сигнала внури кластера во времени.
    Выявить наиболее стабильный кластер.

### Шаг 1

### Загрузим необходимые библиотеки.

``` r
library(stringr)
library(tidyr)
library(knitr)
library(readr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
url <- "https://storage.yandexcloud.net/dataset.ctfsec/P2_wifi_data.csv"
```

``` r
download.file(url, "wifi_data.csv", mode = "wb")
```

``` r
wifi_ap <- read_csv("wifi_data.csv", n_max = 167)
```

    Rows: 167 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (6): BSSID, Privacy, Cipher, Authentication, LAN IP, ESSID
    dbl  (6): channel, Speed, Power, # beacons, # IV, ID-length
    lgl  (1): Key
    dttm (2): First time seen, Last time seen

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
clients <- read_csv("wifi_data.csv", skip = 169)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 12081 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (3): Station MAC, BSSID, Probed ESSIDs
    dbl  (2): Power, # packets
    dttm (2): First time seen, Last time seen

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
glimpse(clients)
```

    Rows: 12,081
    Columns: 7
    $ `Station MAC`     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:…
    $ `First time seen` <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-2…
    $ `Last time seen`  <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-2…
    $ Power             <dbl> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -4…
    $ `# packets`       <dbl> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77,…
    $ BSSID             <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D…
    $ `Probed ESSIDs`   <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C32…

``` r
names(wifi_ap) <- trimws(names(wifi_ap))

wifi_ap_clean <-  wifi_ap %>%
  rename(
    bssid       = BSSID,
    first_seen  = `First time seen`,
    last_seen   = `Last time seen`,
    channel     = channel,
    speed       = Speed,
    privacy     = Privacy,
    cipher      = Cipher,
    auth        = Authentication,
    power       = Power,
    beacons     = `# beacons`,
    iv_count    = `# IV`,
    lan_ip      = `LAN IP`,
    id_length   = `ID-length`,
    essid       = ESSID,
    key         = Key
  ) %>%

mutate(across(where(is.character), ~trimws(.))) %>%
  mutate(
    first_seen = as.POSIXct(first_seen, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    last_seen  = as.POSIXct(last_seen,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    channel    = as.numeric(channel),
    speed      = as.numeric(speed),
    power      = as.numeric(power),
    beacons    = as.numeric(beacons),
    iv_count   = as.numeric(iv_count),
    id_length  = as.numeric(id_length)
  ) %>%
  tibble::as_tibble()
```

``` r
names(clients) <- trimws(names(clients))

wifi_clients_clean <- clients %>%
  rename(
    station_mac   = `Station MAC`,
    first_seen    = `First time seen`,
    last_seen     = `Last time seen`,
    power         = Power,
    packets       = `# packets`,
    bssid         = BSSID,
    probed_essids = `Probed ESSIDs`
  ) %>%
  mutate(across(where(is.character), ~ trimws(.))) %>%
  mutate(
    first_seen = as.POSIXct(first_seen, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    last_seen  = as.POSIXct(last_seen,  format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    power      = as.numeric(power),
    packets    = as.numeric(packets),
    station_mac = toupper(station_mac),
    bssid = case_when(
      is.na(bssid) ~ NA_character_,
      grepl("(?i)<?not associated>?", bssid) ~ NA_character_,
      TRUE ~ toupper(bssid)
    )
  ) %>%
  tibble::as_tibble()
```

``` r
glimpse(wifi_ap_clean)
```

    Rows: 167
    Columns: 15
    $ bssid      <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9:04:1…
    $ first_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 09:13…
    $ last_seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 11:53…
    $ channel    <dbl> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, 6, 6,…
    $ speed      <dbl> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180, 65, …
    $ privacy    <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2", "WPA…
    $ cipher     <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "CCMP",…
    $ auth       <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "PSK",…
    $ power      <dbl> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42, -62,…
    $ beacons    <dbl> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 1390, 1…
    $ iv_count   <dbl> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, 907, …
    $ lan_ip     <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.  0. …
    $ id_length  <dbl> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 12, 0,…
    $ essid      <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "MIREA_…
    $ key        <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…

``` r
glimpse(wifi_clients_clean)
```

    Rows: 12,081
    Columns: 7
    $ station_mac   <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E:1…
    $ first_seen    <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 09…
    $ last_seen     <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 11…
    $ power         <dbl> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45, -…
    $ packets       <dbl> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7, …
    $ bssid         <chr> "BE:F1:71:D5:17:8B", NA, "BE:F1:71:D6:10:D7", "BE:F1:71:…
    $ probed_essids <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U13…

### Шаг 2. Анализ

#### 1. Определить небезопасные точки доступа (без шифрования – OPN)

``` r
wifi_ap_clean %>% filter(privacy == "OPN")
```

    # A tibble: 42 × 15
       bssid    first_seen          last_seen           channel speed privacy cipher
       <chr>    <dttm>              <dttm>                <dbl> <dbl> <chr>   <chr> 
     1 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
     2 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:12       6   130 OPN     <NA>  
     3 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:11       6   130 OPN     <NA>  
     4 E8:28:C… 2023-07-28 09:13:06 2023-07-28 11:55:10       6    -1 OPN     <NA>  
     5 00:25:0… 2023-07-28 09:13:06 2023-07-28 11:56:21      44    -1 OPN     <NA>  
     6 E8:28:C… 2023-07-28 09:13:09 2023-07-28 11:56:05      11   130 OPN     <NA>  
     7 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:27:06       6   130 OPN     <NA>  
     8 E8:28:C… 2023-07-28 09:13:13 2023-07-28 10:39:43       6   130 OPN     <NA>  
     9 E8:28:C… 2023-07-28 09:13:17 2023-07-28 11:52:32       1   130 OPN     <NA>  
    10 E8:28:C… 2023-07-28 09:13:50 2023-07-28 11:43:39      11   130 OPN     <NA>  
    # ℹ 32 more rows
    # ℹ 8 more variables: auth <chr>, power <dbl>, beacons <dbl>, iv_count <dbl>,
    #   lan_ip <chr>, id_length <dbl>, essid <chr>, key <lgl>

### 2. Определить производителя для каждого обнаруженного устройства

``` r
oui_data <- read_csv("oui.csv", show_col_types = FALSE)
```

``` r
glimpse(oui_data)
```

    Rows: 38,463
    Columns: 4
    $ Registry               <chr> "MA-L", "MA-L", "MA-L", "MA-L", "MA-L", "MA-L",…
    $ Assignment             <chr> "286FB9", "08EA44", "F4EAB5", "B87CF2", "E0A129…
    $ `Organization Name`    <chr> "Nokia Shanghai Bell Co., Ltd.", "Extreme Netwo…
    $ `Organization Address` <chr> "No.388 Ning Qiao Road,Jin Qiao Pudong Shanghai…

``` r
find_manufacturer <- function(mac) {
  if (is.na(mac)) return(NA)
  mac_clean <- gsub("[: -]", "", mac)
  oui_code <- substr(mac_clean, 1, 6)
  result <- oui_data %>% 
    filter(grepl(paste0("^", oui_code), Assignment))
  
  if (nrow(result) > 0) {
    return(result$`Organization Name`[1])
  } else {
    return("Неизвестный производитель")
  }
}
wifi_ap_clean <- wifi_ap_clean %>%
  mutate(
    производитель = sapply(bssid, find_manufacturer)
  )
wifi_ap_clean %>%
  select(bssid, essid, производитель) %>%
  head(10)
```

    # A tibble: 10 × 3
       bssid             essid                    производитель            
       <chr>             <chr>                    <chr>                    
     1 BE:F1:71:D5:17:8B C322U13 3965             Неизвестный производитель
     2 6E:C7:EC:16:DA:1A Cnet                     Неизвестный производитель
     3 9A:75:A8:B9:04:1E KC                       Неизвестный производитель
     4 4A:EC:1E:DB:BF:95 POCO X5 Pro 5G           Неизвестный производитель
     5 D2:6D:52:61:51:5D <NA>                     Неизвестный производитель
     6 E8:28:C1:DC:B2:52 MIREA_HOTSPOT            Eltex Enterprise Ltd.    
     7 BE:F1:71:D6:10:D7 C322U21 0566             Неизвестный производитель
     8 0A:C5:E1:DB:17:7B AndroidAP177B            Неизвестный производитель
     9 38:1A:52:0D:84:D7 EBFCD57F-EE81fI_DL_1AO2T Seiko Epson Corporation  
    10 BE:F1:71:D5:0E:53 C322U06 9080             Неизвестный производитель

#### 3. Выявить устройства, использующие последнюю версию протокола шифрования WPA3, и названия точек доступа, реализованных на этих устройствах?

``` r
wpa3_devices <- wifi_ap_clean %>%
  filter(
    str_detect(toupper(privacy), "WPA3") | 
    str_detect(toupper(auth), "WPA3")
  )
wpa3_devices
```

    # A tibble: 8 × 16
      bssid     first_seen          last_seen           channel speed privacy cipher
      <chr>     <dttm>              <dttm>                <dbl> <dbl> <chr>   <chr> 
    1 26:20:53… 2023-07-28 09:15:45 2023-07-28 09:33:10      44   866 WPA3 W… CCMP  
    2 A2:FE:FF… 2023-07-28 09:41:52 2023-07-28 09:41:52       6   130 WPA3 W… CCMP  
    3 96:FF:FC… 2023-07-28 09:52:54 2023-07-28 10:25:02      44   866 WPA3 W… CCMP  
    4 CE:48:E7… 2023-07-28 09:59:20 2023-07-28 10:04:15      44   866 WPA3 W… CCMP  
    5 8E:1F:94… 2023-07-28 10:08:32 2023-07-28 10:15:27      44   866 WPA3 W… CCMP  
    6 BE:FD:EF… 2023-07-28 10:15:24 2023-07-28 10:15:28       6   130 WPA3 W… CCMP  
    7 3A:DA:00… 2023-07-28 10:27:01 2023-07-28 10:27:10       6   130 WPA3 W… CCMP  
    8 76:C5:A0… 2023-07-28 11:16:36 2023-07-28 11:16:38       6   130 WPA3 W… CCMP  
    # ℹ 9 more variables: auth <chr>, power <dbl>, beacons <dbl>, iv_count <dbl>,
    #   lan_ip <chr>, id_length <dbl>, essid <chr>, key <lgl>, производитель <chr>

#### 4. Отсортировать точки доступа по интервалу времени, в течение которого они находились на связи, по убыванию

``` r
wifi_ap_clean <- wifi_ap_clean %>%
  mutate(
    connection_time_sec = as.numeric(difftime(last_seen, first_seen, units = "secs")),
    connection_time_min = connection_time_sec / 60,
    connection_time_hours = connection_time_sec / 3600
  )

sorted_ap <- wifi_ap_clean %>%
  arrange(desc(connection_time_sec))

sorted_ap %>%
  select(bssid, essid, first_seen, last_seen, 
         connection_time_hours, производитель) %>%
  head(20)
```

    # A tibble: 20 × 6
       bssid     essid first_seen          last_seen           connection_time_hours
       <chr>     <chr> <dttm>              <dttm>                              <dbl>
     1 00:25:00… <NA>  2023-07-28 09:13:06 2023-07-28 11:56:21                  2.72
     2 E8:28:C1… MIRE… 2023-07-28 09:13:09 2023-07-28 11:56:05                  2.72
     3 E8:28:C1… MIRE… 2023-07-28 09:13:03 2023-07-28 11:55:38                  2.71
     4 08:3A:2F… <NA>  2023-07-28 09:13:27 2023-07-28 11:55:53                  2.71
     5 6E:C7:EC… Cnet  2023-07-28 09:13:03 2023-07-28 11:55:12                  2.70
     6 E8:28:C1… MIRE… 2023-07-28 09:13:06 2023-07-28 11:55:12                  2.70
     7 E8:28:C1… <NA>  2023-07-28 09:13:06 2023-07-28 11:55:11                  2.70
     8 48:5B:39… <NA>  2023-07-28 09:13:06 2023-07-28 11:55:11                  2.70
     9 E8:28:C1… <NA>  2023-07-28 09:13:06 2023-07-28 11:55:10                  2.70
    10 8E:55:4A… Vlad… 2023-07-28 09:13:06 2023-07-28 11:55:09                  2.70
    11 00:26:99… GIVC  2023-07-28 09:13:20 2023-07-28 11:55:10                  2.70
    12 00:26:99… GIVC  2023-07-28 09:13:06 2023-07-28 11:54:53                  2.70
    13 1E:93:E3… Gala… 2023-07-28 09:13:04 2023-07-28 11:53:37                  2.68
    14 9A:75:A8… KC    2023-07-28 09:13:03 2023-07-28 11:53:31                  2.67
    15 0C:80:63… <NA>  2023-07-28 09:13:08 2023-07-28 11:53:36                  2.67
    16 00:23:EB… GIVC  2023-07-28 09:13:40 2023-07-28 11:53:35                  2.67
    17 9E:A3:A9… <NA>  2023-07-28 09:13:15 2023-07-28 11:52:30                  2.65
    18 E8:28:C1… MIRE… 2023-07-28 09:13:17 2023-07-28 11:52:32                  2.65
    19 1C:7E:E5… <NA>  2023-07-28 09:13:04 2023-07-28 11:51:48                  2.65
    20 00:26:99… IKB   2023-07-28 09:13:06 2023-07-28 11:51:18                  2.64
    # ℹ 1 more variable: производитель <chr>

#### 5. Обнаружить топ-10 самых быстрых точек доступа

``` r
wifi_ap_clean %>%
  arrange(desc(speed)) %>%
  select(essid, bssid, speed, производитель) %>%
  head(10)
```

    # A tibble: 10 × 4
       essid              bssid             speed производитель            
       <chr>              <chr>             <dbl> <chr>                    
     1 <NA>               26:20:53:0C:98:E8   866 Неизвестный производитель
     2 <NA>               96:FF:FC:91:EF:64   866 Неизвестный производитель
     3 iPhone (Анастасия) CE:48:E7:86:4E:33   866 Неизвестный производитель
     4 iPhone (Анастасия) 8E:1F:94:96:DA:FD   866 Неизвестный производитель
     5 KC                 9A:75:A8:B9:04:1E   360 Неизвестный производитель
     6 POCO X5 Pro 5G     4A:EC:1E:DB:BF:95   360 Неизвестный производитель
     7 OnePlus 6T         56:C5:2B:9F:84:90   360 Неизвестный производитель
     8 MIREA_GUESTS       E8:28:C1:DC:B2:41   360 Eltex Enterprise Ltd.    
     9 MIREA_HOTSPOT      E8:28:C1:DC:B2:40   360 Eltex Enterprise Ltd.    
    10 <NA>               E8:28:C1:DC:B2:42   360 Eltex Enterprise Ltd.    

### 6. Отсортировать точки доступа по частоте отправки запросов (beacons) в единицу времени по их убыванию.

``` r
wifi_ap_clean
```

    # A tibble: 167 × 19
       bssid    first_seen          last_seen           channel speed privacy cipher
       <chr>    <dttm>              <dttm>                <dbl> <dbl> <chr>   <chr> 
     1 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:50       1   195 WPA2    CCMP  
     2 6E:C7:E… 2023-07-28 09:13:03 2023-07-28 11:55:12       1   130 WPA2    CCMP  
     3 9A:75:A… 2023-07-28 09:13:03 2023-07-28 11:53:31       1   360 WPA2    CCMP  
     4 4A:EC:1… 2023-07-28 09:13:03 2023-07-28 11:04:01       7   360 WPA2    CCMP  
     5 D2:6D:5… 2023-07-28 09:13:03 2023-07-28 10:30:19       6   130 WPA2    CCMP  
     6 E8:28:C… 2023-07-28 09:13:03 2023-07-28 11:55:38       6   130 OPN     <NA>  
     7 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 11:50:44      11   195 WPA2    CCMP  
     8 0A:C5:E… 2023-07-28 09:13:03 2023-07-28 11:36:31      11   130 WPA2    CCMP  
     9 38:1A:5… 2023-07-28 09:13:03 2023-07-28 10:25:02      11   130 WPA2    CCMP  
    10 BE:F1:7… 2023-07-28 09:13:03 2023-07-28 10:29:21       1   195 WPA2    CCMP  
    # ℹ 157 more rows
    # ℹ 12 more variables: auth <chr>, power <dbl>, beacons <dbl>, iv_count <dbl>,
    #   lan_ip <chr>, id_length <dbl>, essid <chr>, key <lgl>, производитель <chr>,
    #   connection_time_sec <dbl>, connection_time_min <dbl>,
    #   connection_time_hours <dbl>

``` r
sorted_data <- wifi_ap_clean[order(-wifi_ap_clean$beacons), ]
top_10 <- sorted_data[1:10, ]
result <- top_10[, c("essid", "beacons")]

result
```

    # A tibble: 10 × 2
       essid                    beacons
       <chr>                      <dbl>
     1 C322U21 0566                1647
     2 Galaxy A71                  1390
     3 AndroidAP177B               1251
     4 C322U13 3965                 846
     5 Cnet                         750
     6 Redmi Note 8 Pro             738
     7 EBFCD57F-EE81fI_DL_1AO2T     704
     8 KC                           694
     9 <NA>                         647
    10 C322U06 9080                 617

### Данные клиентов

``` r
wifi_clients_clean$company <- vapply(wifi_clients_clean$station_mac, find_manufacturer, character(1))

head(wifi_clients_clean[, c("station_mac", "company", "bssid")], 20)
```

    # A tibble: 20 × 3
       station_mac       company                              bssid            
       <chr>             <chr>                                <chr>            
     1 CA:66:3B:8F:56:DD Неизвестный производитель            BE:F1:71:D5:17:8B
     2 96:35:2D:3D:85:E6 Неизвестный производитель            <NA>             
     3 5C:3A:45:9E:1A:7B CHONGQING FUGUI ELECTRONICS CO.,LTD. BE:F1:71:D6:10:D7
     4 C0:E4:34:D8:E7:E5 AzureWave Technology Inc.            BE:F1:71:D5:17:8B
     5 5E:8E:A6:5E:34:81 Неизвестный производитель            <NA>             
     6 10:51:07:CB:33:E7 Intel Corporate                      <NA>             
     7 68:54:5A:40:35:9E Intel Corporate                      1E:93:E3:1B:3C:F4
     8 74:4C:A1:70:CE:F7 Liteon Technology Corporation        E8:28:C1:DC:FF:F2
     9 8A:A3:5A:33:76:57 Неизвестный производитель            00:25:00:FF:94:73
    10 CA:54:C4:8B:B5:3A Неизвестный производитель            00:26:99:F2:7A:E2
    11 BC:F1:71:D4:DB:04 Intel Corporate                      <NA>             
    12 4A:C9:28:46:EE:3F Неизвестный производитель            0C:80:63:A9:6E:EE
    13 A6:EC:3C:AB:BA:10 Неизвестный производитель            <NA>             
    14 4C:44:5B:14:76:E3 Intel Corporate                      E8:28:C1:DD:04:52
    15 9E:01:46:3E:EF:4E Неизвестный производитель            <NA>             
    16 A0:E7:0B:AE:D5:44 Intel Corporate                      0A:C5:E1:DB:17:7B
    17 00:95:69:E7:7F:35 LSD Science and Technology Co.,Ltd.  <NA>             
    18 00:95:69:E7:7C:ED LSD Science and Technology Co.,Ltd.  <NA>             
    19 14:13:33:59:9F:AB AzureWave Technology Inc.            <NA>             
    20 10:51:07:FE:77:BB Intel Corporate                      <NA>             

#### 2. Обнаружить устройства, которые НЕ рандомизируют свой MAC адрес

``` r
is_randomized_mac <- function(mac) {
  if (is.na(mac)) return(NA)
  mac_clean <- gsub("[: -]", "", mac)
  first_byte <- substr(mac_clean, 1, 2)
  first_byte_num <- strtoi(first_byte, 16)
  second_bit <- bitwAnd(first_byte_num, 0x02)  
  return(second_bit != 0)
}

wifi_clients_clean <- wifi_clients_clean %>%
  mutate(
    is_randomized = sapply(station_mac, is_randomized_mac)
  )

non_random_devices <- wifi_clients_clean %>%
  filter(!is_randomized) %>%
  distinct(station_mac, .keep_all = TRUE)
non_random_devices %>%
  select(station_mac, company, is_randomized) %>%
  head(20)
```

    # A tibble: 20 × 3
       station_mac       company                              is_randomized
       <chr>             <chr>                                <lgl>        
     1 5C:3A:45:9E:1A:7B CHONGQING FUGUI ELECTRONICS CO.,LTD. FALSE        
     2 C0:E4:34:D8:E7:E5 AzureWave Technology Inc.            FALSE        
     3 10:51:07:CB:33:E7 Intel Corporate                      FALSE        
     4 68:54:5A:40:35:9E Intel Corporate                      FALSE        
     5 74:4C:A1:70:CE:F7 Liteon Technology Corporation        FALSE        
     6 BC:F1:71:D4:DB:04 Intel Corporate                      FALSE        
     7 4C:44:5B:14:76:E3 Intel Corporate                      FALSE        
     8 A0:E7:0B:AE:D5:44 Intel Corporate                      FALSE        
     9 00:95:69:E7:7F:35 LSD Science and Technology Co.,Ltd.  FALSE        
    10 00:95:69:E7:7C:ED LSD Science and Technology Co.,Ltd.  FALSE        
    11 14:13:33:59:9F:AB AzureWave Technology Inc.            FALSE        
    12 10:51:07:FE:77:BB Intel Corporate                      FALSE        
    13 10:51:07:CB:33:BF Intel Corporate                      FALSE        
    14 BC:F1:71:D5:17:8B Intel Corporate                      FALSE        
    15 48:68:4A:93:DF:B4 Intel Corporate                      FALSE        
    16 28:7F:CF:23:25:53 Intel Corporate                      FALSE        
    17 00:95:69:E7:7D:21 LSD Science and Technology Co.,Ltd.  FALSE        
    18 BC:F1:71:D5:0E:53 Intel Corporate                      FALSE        
    19 8C:55:4A:DE:F2:38 Intel Corporate                      FALSE        
    20 BC:F1:71:D6:10:D7 Intel Corporate                      FALSE        

#### Кластеризовать запросы от устройств к точкам доступа по их именам. Определить время появления устройства в зоне радиовидимости и время выхода его из нее

``` r
ap_clusters <- wifi_ap_clean %>%
  group_by(essid) %>%
  summarise(
    first_time_seen = min(first_seen, na.rm = TRUE),
    last_time_seen = max(last_seen, na.rm = TRUE),
    duration_hours = round(as.numeric(
      difftime(max(last_seen), min(first_seen), units = "hours")
    ), 1),
    device_count = n_distinct(bssid),
    avg_power = mean(power, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(first_time_seen)

ap_clusters %>%
  head(20)
```

    # A tibble: 20 × 6
       essid     first_time_seen     last_time_seen      duration_hours device_count
       <chr>     <dttm>              <dttm>                       <dbl>        <int>
     1 AndroidA… 2023-07-28 09:13:03 2023-07-28 11:36:31            2.4            1
     2 C322U06 … 2023-07-28 09:13:03 2023-07-28 10:29:21            1.3            1
     3 C322U13 … 2023-07-28 09:13:03 2023-07-28 11:50:50            2.6            1
     4 C322U21 … 2023-07-28 09:13:03 2023-07-28 11:50:44            2.6            1
     5 Cnet      2023-07-28 09:13:03 2023-07-28 11:55:12            2.7            1
     6 EBFCD57F… 2023-07-28 09:13:03 2023-07-28 10:25:02            1.2            1
     7 KC        2023-07-28 09:13:03 2023-07-28 11:53:31            2.7            1
     8 MIREA_HO… 2023-07-28 09:13:03 2023-07-28 11:56:05            2.7            8
     9 POCO X5 … 2023-07-28 09:13:03 2023-07-28 11:04:01            1.8            1
    10 <NA>      2023-07-28 09:13:03 2023-07-28 11:56:21            2.7           71
    11 Galaxy A… 2023-07-28 09:13:04 2023-07-28 11:53:37            2.7            1
    12 EBFCD593… 2023-07-28 09:13:05 2023-07-28 10:21:11            1.1            1
    13 EBFCD597… 2023-07-28 09:13:06 2023-07-28 11:37:27            2.4            1
    14 GIVC      2023-07-28 09:13:06 2023-07-28 11:55:10            2.7            5
    15 IKB       2023-07-28 09:13:06 2023-07-28 11:53:35            2.7            5
    16 MIREA_GU… 2023-07-28 09:13:06 2023-07-28 11:55:12            2.7            9
    17 Vladimir  2023-07-28 09:13:06 2023-07-28 11:55:09            2.7            1
    18 Mura's G… 2023-07-28 09:14:12 2023-07-28 09:14:12            0              1
    19 Long Huo… 2023-07-28 09:17:49 2023-07-28 10:34:01            1.3            1
    20 OnePlus … 2023-07-28 09:17:49 2023-07-28 10:27:22            1.2            1
    # ℹ 1 more variable: avg_power <dbl>

#### Оценить стабильность уровня сигнала внури кластера во времени. Выявить наиболее стабильный кластер.

``` r
most_stable <- wifi_ap_clean %>%
  group_by(essid) %>%
  summarise(
    mean_power = mean(power, na.rm = TRUE),
    sd_power = sd(power, na.rm = TRUE),
    n_measurements = n(),
    power_range = max(power, na.rm = TRUE) - min(power, na.rm = TRUE),
    total_hours = as.numeric(
      difftime(max(last_seen), min(first_seen), units = "hours")
    ),
    .groups = "drop"
  ) %>%
  filter(n_measurements > 1, !is.na(sd_power)) %>%
  arrange(sd_power) %>%
  slice(1)

most_stable
```

    # A tibble: 1 × 6
      essid              mean_power sd_power n_measurements power_range total_hours
      <chr>                   <dbl>    <dbl>          <int>       <dbl>       <dbl>
    1 iPhone (Анастасия)        -66     1.41              2           2       0.269

### Шаг 3.

Оформить отчет

## Вывод

В ходе практической работы я развил навыки работы с языком
программирования R, а также научился исследовать информацию о состоянии
беспроводных сетей
