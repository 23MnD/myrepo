# Практическая работа 002
artem23mnd@yandex.ru

# Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета `dplyr` – функции
    `select(), filter(), mutate(), arrange(), group_by()`

## Исходные данные

1.  Программное обеспечение Windows 10
2.  Rstudio Desktop
3.  Интерпретатор языка R 4.5.2

## Задание

Используя программный пакет dplyr, освоить базовые операции в языке
программирования R.

## Ход работы

1.  Проанализировать встроенный в пакет dplyr набор данных starwars с
    помощью языка R и ответить на вопросы:
    -   Сколько строк в датафрейме?
    -   Сколько столбцов в датафрейме?
    -   Как просмотреть примерный вид датафрейма?
    -   Сколько уникальных рас персонажей (species) представлено в
        данных?
    -   Найти самого высокого персонажа.
    -   Найти всех персонажей ниже 170
    -   Подсчитать ИМТ (индекс массы тела) для всех персонажей.
    -   Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
        отношению массы (mass) к росту (height) персонажей
    -   Найти средний возраст персонажей каждой расы вселенной Звездных
        войн
    -   Найти самый распространенный цвет глаз персонажей вселенной
        Звездных войн.
    -   Подсчитать среднюю длину имени в каждой расе вселенной Звездных
        войн.
2.  Оформить отчет в соответствии с шаблоном

### Шаг 1

## Подключаем библиотеку dplyr

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

#### Сколько строк в датафрейме?

``` r
starwars %>% nrow()
```

    [1] 87

#### Сколько столбцов в датафрейме?

``` r
starwars %>% ncol()
```

    [1] 14

#### Как просмотреть примерный вид датафрейма?

``` r
starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

#### Сколько уникальных рас персонажей (species) представлено в данных?

``` r
starwars %>% select(., species) %>% filter(!is.na(species)) %>% unique(.) %>% count(.) |> knitr::kable(format = "markdown")
```

<table>
<thead>
<tr>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: right;">37</td>
</tr>
</tbody>
</table>

#### Найти самого высокого персонажа.

``` r
starwars %>% arrange(., desc(height)) %>% select(name) %>% slice(1)  |> knitr::kable(format = "markdown")
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Yarael Poof</td>
</tr>
</tbody>
</table>

#### Найти всех персонажей ниже 170

``` r
starwars %>% filter(height < 170) %>% select(., name, height)|> knitr::kable(format = "markdown")
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">height</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">C-3PO</td>
<td style="text-align: right;">167</td>
</tr>
<tr>
<td style="text-align: left;">R2-D2</td>
<td style="text-align: right;">96</td>
</tr>
<tr>
<td style="text-align: left;">Leia Organa</td>
<td style="text-align: right;">150</td>
</tr>
<tr>
<td style="text-align: left;">Beru Whitesun Lars</td>
<td style="text-align: right;">165</td>
</tr>
<tr>
<td style="text-align: left;">R5-D4</td>
<td style="text-align: right;">97</td>
</tr>
<tr>
<td style="text-align: left;">Yoda</td>
<td style="text-align: right;">66</td>
</tr>
<tr>
<td style="text-align: left;">Mon Mothma</td>
<td style="text-align: right;">150</td>
</tr>
<tr>
<td style="text-align: left;">Wicket Systri Warrick</td>
<td style="text-align: right;">88</td>
</tr>
<tr>
<td style="text-align: left;">Nien Nunb</td>
<td style="text-align: right;">160</td>
</tr>
<tr>
<td style="text-align: left;">Watto</td>
<td style="text-align: right;">137</td>
</tr>
<tr>
<td style="text-align: left;">Sebulba</td>
<td style="text-align: right;">112</td>
</tr>
<tr>
<td style="text-align: left;">Shmi Skywalker</td>
<td style="text-align: right;">163</td>
</tr>
<tr>
<td style="text-align: left;">Ratts Tyerel</td>
<td style="text-align: right;">79</td>
</tr>
<tr>
<td style="text-align: left;">Dud Bolt</td>
<td style="text-align: right;">94</td>
</tr>
<tr>
<td style="text-align: left;">Gasgano</td>
<td style="text-align: right;">122</td>
</tr>
<tr>
<td style="text-align: left;">Ben Quadinaros</td>
<td style="text-align: right;">163</td>
</tr>
<tr>
<td style="text-align: left;">Cordé</td>
<td style="text-align: right;">157</td>
</tr>
<tr>
<td style="text-align: left;">Barriss Offee</td>
<td style="text-align: right;">166</td>
</tr>
<tr>
<td style="text-align: left;">Dormé</td>
<td style="text-align: right;">165</td>
</tr>
<tr>
<td style="text-align: left;">Zam Wesell</td>
<td style="text-align: right;">168</td>
</tr>
<tr>
<td style="text-align: left;">Jocasta Nu</td>
<td style="text-align: right;">167</td>
</tr>
<tr>
<td style="text-align: left;">R4-P17</td>
<td style="text-align: right;">96</td>
</tr>
</tbody>
</table>

### Подсчитать ИМТ для всех персонажей

``` r
starwars %>% mutate(., imt = mass / (height^2)) %>% select(., name, imt) |> knitr::kable(format = "markdown")
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">imt</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Luke Skywalker</td>
<td style="text-align: right;">0.0026028</td>
</tr>
<tr>
<td style="text-align: left;">C-3PO</td>
<td style="text-align: right;">0.0026892</td>
</tr>
<tr>
<td style="text-align: left;">R2-D2</td>
<td style="text-align: right;">0.0034722</td>
</tr>
<tr>
<td style="text-align: left;">Darth Vader</td>
<td style="text-align: right;">0.0033330</td>
</tr>
<tr>
<td style="text-align: left;">Leia Organa</td>
<td style="text-align: right;">0.0021778</td>
</tr>
<tr>
<td style="text-align: left;">Owen Lars</td>
<td style="text-align: right;">0.0037874</td>
</tr>
<tr>
<td style="text-align: left;">Beru Whitesun Lars</td>
<td style="text-align: right;">0.0027548</td>
</tr>
<tr>
<td style="text-align: left;">R5-D4</td>
<td style="text-align: right;">0.0034010</td>
</tr>
<tr>
<td style="text-align: left;">Biggs Darklighter</td>
<td style="text-align: right;">0.0025083</td>
</tr>
<tr>
<td style="text-align: left;">Obi-Wan Kenobi</td>
<td style="text-align: right;">0.0023246</td>
</tr>
<tr>
<td style="text-align: left;">Anakin Skywalker</td>
<td style="text-align: right;">0.0023766</td>
</tr>
<tr>
<td style="text-align: left;">Wilhuff Tarkin</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Chewbacca</td>
<td style="text-align: right;">0.0021545</td>
</tr>
<tr>
<td style="text-align: left;">Han Solo</td>
<td style="text-align: right;">0.0024691</td>
</tr>
<tr>
<td style="text-align: left;">Greedo</td>
<td style="text-align: right;">0.0024725</td>
</tr>
<tr>
<td style="text-align: left;">Jabba Desilijic Tiure</td>
<td style="text-align: right;">0.0443429</td>
</tr>
<tr>
<td style="text-align: left;">Wedge Antilles</td>
<td style="text-align: right;">0.0026644</td>
</tr>
<tr>
<td style="text-align: left;">Jek Tono Porkins</td>
<td style="text-align: right;">0.0033951</td>
</tr>
<tr>
<td style="text-align: left;">Yoda</td>
<td style="text-align: right;">0.0039027</td>
</tr>
<tr>
<td style="text-align: left;">Palpatine</td>
<td style="text-align: right;">0.0025952</td>
</tr>
<tr>
<td style="text-align: left;">Boba Fett</td>
<td style="text-align: right;">0.0023351</td>
</tr>
<tr>
<td style="text-align: left;">IG-88</td>
<td style="text-align: right;">0.0035000</td>
</tr>
<tr>
<td style="text-align: left;">Bossk</td>
<td style="text-align: right;">0.0031302</td>
</tr>
<tr>
<td style="text-align: left;">Lando Calrissian</td>
<td style="text-align: right;">0.0025216</td>
</tr>
<tr>
<td style="text-align: left;">Lobot</td>
<td style="text-align: right;">0.0025796</td>
</tr>
<tr>
<td style="text-align: left;">Ackbar</td>
<td style="text-align: right;">0.0025617</td>
</tr>
<tr>
<td style="text-align: left;">Mon Mothma</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Arvel Crynyd</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Wicket Systri Warrick</td>
<td style="text-align: right;">0.0025826</td>
</tr>
<tr>
<td style="text-align: left;">Nien Nunb</td>
<td style="text-align: right;">0.0026563</td>
</tr>
<tr>
<td style="text-align: left;">Qui-Gon Jinn</td>
<td style="text-align: right;">0.0023893</td>
</tr>
<tr>
<td style="text-align: left;">Nute Gunray</td>
<td style="text-align: right;">0.0024670</td>
</tr>
<tr>
<td style="text-align: left;">Finis Valorum</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Padmé Amidala</td>
<td style="text-align: right;">0.0013148</td>
</tr>
<tr>
<td style="text-align: left;">Jar Jar Binks</td>
<td style="text-align: right;">0.0017180</td>
</tr>
<tr>
<td style="text-align: left;">Roos Tarpals</td>
<td style="text-align: right;">0.0016342</td>
</tr>
<tr>
<td style="text-align: left;">Rugor Nass</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Ric Olié</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Watto</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Sebulba</td>
<td style="text-align: right;">0.0031888</td>
</tr>
<tr>
<td style="text-align: left;">Quarsh Panaka</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Shmi Skywalker</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Darth Maul</td>
<td style="text-align: right;">0.0026122</td>
</tr>
<tr>
<td style="text-align: left;">Bib Fortuna</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Ayla Secura</td>
<td style="text-align: right;">0.0017359</td>
</tr>
<tr>
<td style="text-align: left;">Ratts Tyerel</td>
<td style="text-align: right;">0.0024035</td>
</tr>
<tr>
<td style="text-align: left;">Dud Bolt</td>
<td style="text-align: right;">0.0050928</td>
</tr>
<tr>
<td style="text-align: left;">Gasgano</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Ben Quadinaros</td>
<td style="text-align: right;">0.0024465</td>
</tr>
<tr>
<td style="text-align: left;">Mace Windu</td>
<td style="text-align: right;">0.0023766</td>
</tr>
<tr>
<td style="text-align: left;">Ki-Adi-Mundi</td>
<td style="text-align: right;">0.0020916</td>
</tr>
<tr>
<td style="text-align: left;">Kit Fisto</td>
<td style="text-align: right;">0.0022647</td>
</tr>
<tr>
<td style="text-align: left;">Eeth Koth</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Adi Gallia</td>
<td style="text-align: right;">0.0014768</td>
</tr>
<tr>
<td style="text-align: left;">Saesee Tiin</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Yarael Poof</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Plo Koon</td>
<td style="text-align: right;">0.0022635</td>
</tr>
<tr>
<td style="text-align: left;">Mas Amedda</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Gregar Typho</td>
<td style="text-align: right;">0.0024836</td>
</tr>
<tr>
<td style="text-align: left;">Cordé</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Cliegg Lars</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Poggle the Lesser</td>
<td style="text-align: right;">0.0023888</td>
</tr>
<tr>
<td style="text-align: left;">Luminara Unduli</td>
<td style="text-align: right;">0.0019446</td>
</tr>
<tr>
<td style="text-align: left;">Barriss Offee</td>
<td style="text-align: right;">0.0018145</td>
</tr>
<tr>
<td style="text-align: left;">Dormé</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Dooku</td>
<td style="text-align: right;">0.0021477</td>
</tr>
<tr>
<td style="text-align: left;">Bail Prestor Organa</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Jango Fett</td>
<td style="text-align: right;">0.0023590</td>
</tr>
<tr>
<td style="text-align: left;">Zam Wesell</td>
<td style="text-align: right;">0.0019487</td>
</tr>
<tr>
<td style="text-align: left;">Dexter Jettster</td>
<td style="text-align: right;">0.0026018</td>
</tr>
<tr>
<td style="text-align: left;">Lama Su</td>
<td style="text-align: right;">0.0016781</td>
</tr>
<tr>
<td style="text-align: left;">Taun We</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Jocasta Nu</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">R4-P17</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Wat Tambor</td>
<td style="text-align: right;">0.0012886</td>
</tr>
<tr>
<td style="text-align: left;">San Hill</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Shaak Ti</td>
<td style="text-align: right;">0.0017990</td>
</tr>
<tr>
<td style="text-align: left;">Grievous</td>
<td style="text-align: right;">0.0034079</td>
</tr>
<tr>
<td style="text-align: left;">Tarfful</td>
<td style="text-align: right;">0.0024837</td>
</tr>
<tr>
<td style="text-align: left;">Raymus Antilles</td>
<td style="text-align: right;">0.0022352</td>
</tr>
<tr>
<td style="text-align: left;">Sly Moore</td>
<td style="text-align: right;">0.0015150</td>
</tr>
<tr>
<td style="text-align: left;">Tion Medon</td>
<td style="text-align: right;">0.0018852</td>
</tr>
<tr>
<td style="text-align: left;">Finn</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Rey</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Poe Dameron</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">BB8</td>
<td style="text-align: right;">NA</td>
</tr>
<tr>
<td style="text-align: left;">Captain Phasma</td>
<td style="text-align: right;">NA</td>
</tr>
</tbody>
</table>

#### Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по отношению массы (mass) к росту (height) персонажей

``` r
starwars %>% mutate(., vut = mass / height) %>% select(., name, vut) %>% arrange(., vut) %>% slice(., 1:10) |> knitr::kable(format = "markdown")
```

<table>
<thead>
<tr>
<th style="text-align: left;">name</th>
<th style="text-align: right;">vut</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Ratts Tyerel</td>
<td style="text-align: right;">0.1898734</td>
</tr>
<tr>
<td style="text-align: left;">Wicket Systri Warrick</td>
<td style="text-align: right;">0.2272727</td>
</tr>
<tr>
<td style="text-align: left;">Padmé Amidala</td>
<td style="text-align: right;">0.2432432</td>
</tr>
<tr>
<td style="text-align: left;">Wat Tambor</td>
<td style="text-align: right;">0.2487047</td>
</tr>
<tr>
<td style="text-align: left;">Yoda</td>
<td style="text-align: right;">0.2575758</td>
</tr>
<tr>
<td style="text-align: left;">Sly Moore</td>
<td style="text-align: right;">0.2696629</td>
</tr>
<tr>
<td style="text-align: left;">Adi Gallia</td>
<td style="text-align: right;">0.2717391</td>
</tr>
<tr>
<td style="text-align: left;">Barriss Offee</td>
<td style="text-align: right;">0.3012048</td>
</tr>
<tr>
<td style="text-align: left;">Ayla Secura</td>
<td style="text-align: right;">0.3089888</td>
</tr>
<tr>
<td style="text-align: left;">Shaak Ti</td>
<td style="text-align: right;">0.3202247</td>
</tr>
</tbody>
</table>

#### Найти средний возраст персонажей каждой расы вселенной Звездных войн

``` r
starwars %>% filter(!is.na(birth_year)) %>% filter(!is.na(species)) %>% group_by(., species) %>% summarize(mean_age = mean(100 - birth_year)) %>% select(., species, mean_age) |> knitr::kable(format = "markdown")
```

<table>
<thead>
<tr>
<th style="text-align: left;">species</th>
<th style="text-align: right;">mean_age</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Cerean</td>
<td style="text-align: right;">8.00000</td>
</tr>
<tr>
<td style="text-align: left;">Droid</td>
<td style="text-align: right;">46.66667</td>
</tr>
<tr>
<td style="text-align: left;">Ewok</td>
<td style="text-align: right;">92.00000</td>
</tr>
<tr>
<td style="text-align: left;">Gungan</td>
<td style="text-align: right;">48.00000</td>
</tr>
<tr>
<td style="text-align: left;">Human</td>
<td style="text-align: right;">46.25769</td>
</tr>
<tr>
<td style="text-align: left;">Hutt</td>
<td style="text-align: right;">-500.00000</td>
</tr>
<tr>
<td style="text-align: left;">Kel Dor</td>
<td style="text-align: right;">78.00000</td>
</tr>
<tr>
<td style="text-align: left;">Mirialan</td>
<td style="text-align: right;">51.00000</td>
</tr>
<tr>
<td style="text-align: left;">Mon Calamari</td>
<td style="text-align: right;">59.00000</td>
</tr>
<tr>
<td style="text-align: left;">Rodian</td>
<td style="text-align: right;">56.00000</td>
</tr>
<tr>
<td style="text-align: left;">Trandoshan</td>
<td style="text-align: right;">47.00000</td>
</tr>
<tr>
<td style="text-align: left;">Twi’lek</td>
<td style="text-align: right;">52.00000</td>
</tr>
<tr>
<td style="text-align: left;">Wookiee</td>
<td style="text-align: right;">-100.00000</td>
</tr>
<tr>
<td style="text-align: left;">Yoda’s species</td>
<td style="text-align: right;">-796.00000</td>
</tr>
<tr>
<td style="text-align: left;">Zabrak</td>
<td style="text-align: right;">46.00000</td>
</tr>
</tbody>
</table>

#### Найти самый распространенный цвет глаз персонажей вселенной Звездных войн.

``` r
starwars %>% group_by(., eye_color) %>% summarize(count_colors = n()) %>% arrange(., desc(count_colors)) %>% select(., eye_color) %>% slice(1)
```

    # A tibble: 1 × 1
      eye_color
      <chr>    
    1 brown    

#### Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.

``` r
starwars %>% filter(!is.na(species)) %>% group_by(., species) %>% summarize(mean_len = mean(nchar(name))) %>% select(., species, mean_len) |> knitr::kable(format = "markdown")
```

<table>
<thead>
<tr>
<th style="text-align: left;">species</th>
<th style="text-align: right;">mean_len</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Aleena</td>
<td style="text-align: right;">12.000000</td>
</tr>
<tr>
<td style="text-align: left;">Besalisk</td>
<td style="text-align: right;">15.000000</td>
</tr>
<tr>
<td style="text-align: left;">Cerean</td>
<td style="text-align: right;">12.000000</td>
</tr>
<tr>
<td style="text-align: left;">Chagrian</td>
<td style="text-align: right;">10.000000</td>
</tr>
<tr>
<td style="text-align: left;">Clawdite</td>
<td style="text-align: right;">10.000000</td>
</tr>
<tr>
<td style="text-align: left;">Droid</td>
<td style="text-align: right;">4.833333</td>
</tr>
<tr>
<td style="text-align: left;">Dug</td>
<td style="text-align: right;">7.000000</td>
</tr>
<tr>
<td style="text-align: left;">Ewok</td>
<td style="text-align: right;">21.000000</td>
</tr>
<tr>
<td style="text-align: left;">Geonosian</td>
<td style="text-align: right;">17.000000</td>
</tr>
<tr>
<td style="text-align: left;">Gungan</td>
<td style="text-align: right;">11.666667</td>
</tr>
<tr>
<td style="text-align: left;">Human</td>
<td style="text-align: right;">11.342857</td>
</tr>
<tr>
<td style="text-align: left;">Hutt</td>
<td style="text-align: right;">21.000000</td>
</tr>
<tr>
<td style="text-align: left;">Iktotchi</td>
<td style="text-align: right;">11.000000</td>
</tr>
<tr>
<td style="text-align: left;">Kaleesh</td>
<td style="text-align: right;">8.000000</td>
</tr>
<tr>
<td style="text-align: left;">Kaminoan</td>
<td style="text-align: right;">7.000000</td>
</tr>
<tr>
<td style="text-align: left;">Kel Dor</td>
<td style="text-align: right;">8.000000</td>
</tr>
<tr>
<td style="text-align: left;">Mirialan</td>
<td style="text-align: right;">14.000000</td>
</tr>
<tr>
<td style="text-align: left;">Mon Calamari</td>
<td style="text-align: right;">6.000000</td>
</tr>
<tr>
<td style="text-align: left;">Muun</td>
<td style="text-align: right;">8.000000</td>
</tr>
<tr>
<td style="text-align: left;">Nautolan</td>
<td style="text-align: right;">9.000000</td>
</tr>
<tr>
<td style="text-align: left;">Neimodian</td>
<td style="text-align: right;">11.000000</td>
</tr>
<tr>
<td style="text-align: left;">Pau’an</td>
<td style="text-align: right;">10.000000</td>
</tr>
<tr>
<td style="text-align: left;">Quermian</td>
<td style="text-align: right;">11.000000</td>
</tr>
<tr>
<td style="text-align: left;">Rodian</td>
<td style="text-align: right;">6.000000</td>
</tr>
<tr>
<td style="text-align: left;">Skakoan</td>
<td style="text-align: right;">10.000000</td>
</tr>
<tr>
<td style="text-align: left;">Sullustan</td>
<td style="text-align: right;">9.000000</td>
</tr>
<tr>
<td style="text-align: left;">Tholothian</td>
<td style="text-align: right;">10.000000</td>
</tr>
<tr>
<td style="text-align: left;">Togruta</td>
<td style="text-align: right;">8.000000</td>
</tr>
<tr>
<td style="text-align: left;">Toong</td>
<td style="text-align: right;">14.000000</td>
</tr>
<tr>
<td style="text-align: left;">Toydarian</td>
<td style="text-align: right;">5.000000</td>
</tr>
<tr>
<td style="text-align: left;">Trandoshan</td>
<td style="text-align: right;">5.000000</td>
</tr>
<tr>
<td style="text-align: left;">Twi’lek</td>
<td style="text-align: right;">11.000000</td>
</tr>
<tr>
<td style="text-align: left;">Vulptereen</td>
<td style="text-align: right;">8.000000</td>
</tr>
<tr>
<td style="text-align: left;">Wookiee</td>
<td style="text-align: right;">8.000000</td>
</tr>
<tr>
<td style="text-align: left;">Xexto</td>
<td style="text-align: right;">7.000000</td>
</tr>
<tr>
<td style="text-align: left;">Yoda’s species</td>
<td style="text-align: right;">4.000000</td>
</tr>
<tr>
<td style="text-align: left;">Zabrak</td>
<td style="text-align: right;">9.500000</td>
</tr>
</tbody>
</table>

### Шаг 2

Отчёт написан и оформлен
