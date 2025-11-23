# Основы обработки данных с помощью R и Dplyr
artem23mnd@yandex.ru

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Операционная система Windows
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.1

## Задание

Используя программный пакет `dplyr`, освоить анализ DNS логов с помощью
языка программирования R.

## Ход работы

1.  Импортируйте данные DNS –
    https://storage.yandexcloud.net/dataset.ctfsec/dns.zip. Данные были
    собраны с помощью сетевого анализатора zeek
2.  Добавьте пропущенные данные о структуре данных (назначении столбцов)
3.  Преобразуйте данные в столбцах в нужный формат,просмотрите общую
    структуру данных с помощью функции glimpse()
4.  Сколько участников информационного обмена всети Доброй Организации?
5.  Какое соотношение участников обмена внутрисети и участников
    обращений к внешним ресурсам?
6.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность.
7.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений
8.  Определите базовые статистические характеристики (функция summary()
    ) интервала времени между последовательными обращениями к топ-10
    доменам.
9.  Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?
10. Определите местоположение (страну, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние
    сервисы,например http://ip-api.com (API-эндпоинт –
    http://ip-api.com/json).

### Шаг 1 - Подготовка данных

Установим и подключим необходимые библиотеки

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.1     ✔ readr     2.1.6
    ✔ ggplot2   4.0.1     ✔ stringr   1.6.0
    ✔ lubridate 1.9.4     ✔ tibble    3.3.0
    ✔ purrr     1.2.0     ✔ tidyr     1.3.1
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readr)
```

``` r
library(httr)
```

``` r
library(jsonlite)
```


    Attaching package: 'jsonlite'

    The following object is masked from 'package:purrr':

        flatten

``` r
library(stringr)
```

``` r
library(knitr)
```

Импортируем данные DNS

``` r
download.file("https://storage.yandexcloud.net/dataset.ctfsec/dns.zip", "dns.zip")
```

``` r
unzip("dns.zip")
```

``` r
dns_data <- read.table(
  "dns.log",
  header = FALSE,
  sep = "\t",
  comment.char = "#",
  fill = TRUE
)
```

    Warning in scan(file = file, what = what, sep = sep, quote = quote, dec = dec,
    : EOF within quoted string

Добавьте пропущенные данные о структуре данных (назначении столбцов)

``` r
column_names <- c(
  "timestamp", "uid", "source_ip", "source_port", "destination_ip", 
  "destination_port", "protocol", "transaction_id", "query", "qclass", 
  "qclass_name", "qtype", "qtype_name", "rcode", "rcode_name", 
  "AA", "TC", "RD", "RA", "Z", "answers", "TTLS", "rejected"
)

colnames(dns_data) <- column_names
dns_data <- as_tibble(dns_data)
print(dns_data, n = 10)
```

    # A tibble: 320,840 × 23
         timestamp uid         source_ip source_port destination_ip destination_port
             <dbl> <chr>       <chr>           <int> <chr>                     <int>
     1 1331901006. CWGtK431H9… 192.168.…       45658 192.168.27.203              137
     2 1331901015. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     3 1331901016. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     4 1331901017. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     5 1331901006. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     6 1331901007. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     7 1331901007. C36a282Jlj… 192.168.…         137 192.168.202.2…              137
     8 1331901006. ClEZCt3GLk… 192.168.…         137 192.168.202.2…              137
     9 1331901007. ClEZCt3GLk… 192.168.…         137 192.168.202.2…              137
    10 1331901008. ClEZCt3GLk… 192.168.…         137 192.168.202.2…              137
    # ℹ 320,830 more rows
    # ℹ 17 more variables: protocol <chr>, transaction_id <int>, query <chr>,
    #   qclass <chr>, qclass_name <chr>, qtype <chr>, qtype_name <chr>,
    #   rcode <chr>, rcode_name <chr>, AA <lgl>, TC <lgl>, RD <lgl>, RA <lgl>,
    #   Z <int>, answers <chr>, TTLS <chr>, rejected <lgl>

Преобразуйте данные в столбцах в нужный формат

``` r
dns_data_clean <- dns_data %>%
  mutate(
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    across(c(source_port, destination_port, transaction_id, qclass, qtype, rcode), as.numeric)
  ) %>% 
  filter(!is.na(source_ip) & !is.na(destination_ip))
```

    Warning: There were 3 warnings in `mutate()`.
    The first warning was:
    ℹ In argument: `across(...)`.
    Caused by warning:
    ! NAs introduced by coercion
    ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.

``` r
glimpse(dns_data_clean)
```

    Rows: 320,840
    Columns: 23
    $ timestamp        <dttm> 2012-03-16 16:30:05, 2012-03-16 16:30:15, 2012-03-16…
    $ uid              <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jl…
    $ source_ip        <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76"…
    $ source_port      <dbl> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ destination_ip   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255…
    $ destination_port <dbl> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ protocol         <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp…
    $ transaction_id   <dbl> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 6218…
    $ query            <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    $ qclass_name      <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET…
    $ qtype            <dbl> 33, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 1…
    $ qtype_name       <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB"…
    $ rcode            <dbl> 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ rcode_name       <chr> "NOERROR", "-", "-", "-", "-", "-", "-", "-", "-", "-…
    $ AA               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
    $ TC               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
    $ RD               <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE…
    $ RA               <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…
    $ Z                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1,…
    $ answers          <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ TTLS             <chr> "-", "-", "-", "-", "-", "-", "-", "-", "-", "-", "-"…
    $ rejected         <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALS…

### Шаг 2 - Анализ данных

Сколько участников информационного обмена в сети Доброй Организации?

``` r
length(unique(c(dns_data_clean$source_ip, dns_data_clean$destination_ip)))
```

    [1] 1288

Какое соотношение участников обмена внутри сети и участников обращений к
внешним ресурсам?

``` r
un_ip <- unique(c(dns_data_clean$source_ip, dns_data_clean$destination_ip))
int_ip <- un_ip[grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", un_ip)]
ext_ip <- un_ip[!grepl("^(10\\.|192\\.168\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)", un_ip)]
length(int_ip)/length(ext_ip)
```

    [1] 15.72727

Найдите топ-10 участников сети, проявляющих наибольшую сетевую
активность.

``` r
dns_data_clean |> count(source_ip, sort = TRUE) |> head(10)
```

    # A tibble: 10 × 2
       source_ip           n
       <chr>           <int>
     1 10.10.117.210   75943
     2 192.168.202.93  15725
     3 10.10.117.209   14222
     4 192.168.202.97  14216
     5 192.168.202.110 13372
     6 192.168.202.103 12818
     7 192.168.203.63  11962
     8 192.168.202.76  10950
     9 192.168.229.252  9530
    10 192.168.202.83   9165

Найдите топ-10 доменов, к которым обращаются пользователи сети и
соответственное количество обращений

``` r
dns_data_clean |> count(query, sort = TRUE) |> head(10)
```

    # A tibble: 10 × 2
       query                                                                       n
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39187
     2 "tools.google.com"                                                      13722
     3 "time.apple.com"                                                        10691
     4 "safebrowsing.clients.google.com"                                       10673
     5 "www.apple.com"                                                          9631
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x…  6582
     7 "44.206.168.192.in-addr.arpa"                                            5832
     8 "imap.gmail.com"                                                         5533
     9 "stats.norton.com"                                                       5531
    10 "WPAD"                                                                   4489

Опеределите базовые статистические характеристики (функция summary())
интервала времени между последовательными обращениями к топ-10 доменам.

``` r
top_10_dom <- dns_data_clean |> count(query, sort = TRUE) |> head(10) |> pull(query)
dns_data_clean |> filter(query %in% top_10_dom) |>
     arrange(timestamp) |> group_by(query) |>
     mutate(time = lead(timestamp) - timestamp) |>
     filter(!is.na(time))|>
     summarise(
         min = min(time),
         Q1 = quantile(time, 0.25),
         median = median(time),
         Q3 = quantile(time, 0.75),
         max = max(time),
         mean = mean(time)
     )
```

    # A tibble: 10 × 7
       query                                    min   Q1    median Q3    max   mean 
       <chr>                                    <drt> <drt> <drtn> <drt> <drt> <drt>
     1 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\… 0 se… 0.06… 0.370… 1.50… 5272… 13.3…
     2 "44.206.168.192.in-addr.arpa"            0 se… 2.08… 2.090… 4.00… 4967… 14.9…
     3 "WPAD"                                   0 se… 0.75… 0.750… 3.31… 5004… 19.6…
     4 "imap.gmail.com"                         0 se… 0.00… 0.000… 3.00… 5235… 15.3…
     5 "safebrowsing.clients.google.com"        0 se… 0.06… 1.000… 2.01… 4995…  8.2…
     6 "stats.norton.com"                       0 se… 0.00… 0.000… 2.17…  979…  4.0…
     7 "teredo.ipv6.microsoft.com"              0 se… 0.00… 0.000… 0.50… 5038…  2.2…
     8 "time.apple.com"                         0 se… 0.00… 1.030… 3.03… 5092…  8.1…
     9 "tools.google.com"                       0 se… 0.00… 0.000… 0.95… 5036…  6.3…
    10 "www.apple.com"                          0 se… 0.00… 1.000… 3.01… 5096…  9.1…

Часто вредоносное программное обеспечение использует dns канал в
качестве канала управления, периодически отправляя запросы на
подконтрольный злоумышленникам DNS сервер. По периодическим запросам на
один и тот же домен можно выявить скрытый DNS канал. Есть ли такие IP
адреса в исследуемом датасете?

``` r
dns_data_clean |>
  arrange(source_ip, query, timestamp) |>
  group_by(source_ip, query) |>
  mutate(time = as.numeric(lead(timestamp) - timestamp)) |>
  filter(!is.na(time)) |>
  summarise(requests = n() + 1,
    avg_interval = mean(time)) |>
  filter(requests >= 10, avg_interval <= 30) |>
  group_by(source_ip) |>
  summarise(
    pd_domains = n(),
    total_requests = sum(requests),
  ) |>
  arrange(desc(pd_domains))
```

    `summarise()` has grouped output by 'source_ip'. You can override using the
    `.groups` argument.

    # A tibble: 85 × 3
       source_ip       pd_domains total_requests
       <chr>                <int>          <dbl>
     1 192.168.202.97         284           9104
     2 192.168.202.71          51           1203
     3 192.168.202.84          49           3027
     4 10.10.117.210           47          69031
     5 192.168.202.79          42            895
     6 192.168.202.100         37            578
     7 192.168.202.103         28           6876
     8 192.168.202.110         23           2378
     9 192.168.202.106         21           2051
    10 192.168.202.115         17            897
    # ℹ 75 more rows

Определите местоположение (страну, город) и организацию-провайдера для
топ-10 доменов. Для этого можно использовать сторонние сервисы, например
http://ip-api.com (API-эндпоинт – http://ip-api.com/json).

``` r


library(httr)
library(jsonlite)


top_10_domains <- dns_data_clean %>%
  count(query, sort = TRUE) %>%
  head(10)


get_domain_geo_info <- function(domain) {
  # Пропускаем некорректные домены - возвращаем NA
  if (grepl("[*\\\\\\x00]", domain) || domain == "-" || nchar(domain) < 3) {
    return(tibble(
      domain = domain,
      country = NA_character_,
      city = NA_character_,
      isp = NA_character_,
      org = NA_character_,
      as = NA_character_
    ))
  }
  
  tryCatch({
 
    url <- paste0("http://ip-api.com/json/", domain, "?fields=status,message,country,city,isp,org,as,query")
    response <- GET(url, timeout(5))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      if (data$status == "success") {
        return(tibble(
          domain = domain,
          country = data$country,
          city = data$city,
          isp = data$isp,
          org = data$org,
          as = data$as
        ))
      } else {
        return(tibble(
          domain = domain,
          country = NA_character_,
          city = NA_character_,
          isp = NA_character_,
          org = NA_character_,
          as = NA_character_
        ))
      }
    } else {
      return(tibble(
        domain = domain,
        country = NA_character_,
        city = NA_character_,
        isp = NA_character_,
        org = NA_character_,
        as = NA_character_
      ))
    }
  }, error = function(e) {
    return(tibble(
      domain = domain,
      country = NA_character_,
      city = NA_character_,
      isp = NA_character_,
      org = NA_character_,
      as = NA_character_
    ))
  })
}


cat("Получение геоданных для топ-10 доменов...\n")
```

``` r
geo_results <- map_dfr(top_10_domains$query, function(domain) {
  cat("Обработка:", domain, "\n")
  result <- get_domain_geo_info(domain)
  # Пауза между запросами (1 секунда)
  Sys.sleep(1)
  return(result)
})
```

    Обработка: teredo.ipv6.microsoft.com 
    Обработка: tools.google.com 
    Обработка: time.apple.com 
    Обработка: safebrowsing.clients.google.com 
    Обработка: www.apple.com 
    Обработка: *\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 
    Обработка: 44.206.168.192.in-addr.arpa 
    Обработка: imap.gmail.com 
    Обработка: stats.norton.com 
    Обработка: WPAD 

``` r

geo_results
```

    # A tibble: 10 × 6
       domain                                        country city  isp   org   as   
       <chr>                                         <chr>   <chr> <chr> <chr> <chr>
     1 "teredo.ipv6.microsoft.com"                   <NA>    <NA>  <NA>  <NA>  <NA> 
     2 "tools.google.com"                            United… Moun… Goog… Goog… AS15…
     3 "time.apple.com"                              United… New … Appl… Appl… AS71…
     4 "safebrowsing.clients.google.com"             United… Moun… Goog… Goog… AS15…
     5 "www.apple.com"                               United… Ashb… Akam… Akam… AS16…
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\… <NA>    <NA>  <NA>  <NA>  <NA> 
     7 "44.206.168.192.in-addr.arpa"                 <NA>    <NA>  <NA>  <NA>  <NA> 
     8 "imap.gmail.com"                              United… Moun… Goog… Goog… AS15…
     9 "stats.norton.com"                            United… Boyd… Micr… Micr… AS80…
    10 "WPAD"                                        <NA>    <NA>  <NA>  <NA>  <NA> 

### Шаг 3

Отчёт написан и оформлен
