# Практичне заняття 2. Кириченко Ангеліна Вадимівна
# Запуск із кореня проєкту: Rscript scripts/main_analysis.R --cached
# Оновлення даних: Rscript scripts/main_analysis.R --refresh

required_packages <- c("quantmod", "PerformanceAnalytics", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                             logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

# RStudio відкриває .Rproj у корені. Для Rscript знаходимо корінь за --file.
if (!file.exists("risk-management-hw1.Rproj")) {
  script_arg <- grep("^--file=", commandArgs(), value = TRUE)
  if (length(script_arg)) {
    setwd(dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1])))))
  }
}
if (!file.exists("risk-management-hw1.Rproj")) {
  stop("Відкрийте risk-management-hw1.Rproj або запустіть скрипт із кореня проєкту.")
}
dir.create("data", showWarnings = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %in% c("--cached", "--refresh")) ||
    all(c("--cached", "--refresh") %in% args)) {
  stop("Оберіть один режим: --cached або --refresh.")
}
symbols <- c("SP500", "NASDAQCOM")
labels <- c(SP500 = "S&P 500", NASDAQCOM = "NASDAQ Composite")
paths <- file.path("data", paste0(symbols, "_prices.csv"))
metadata_path <- "data/analysis_metadata.csv"
use_cache <- !"--refresh" %in% args && all(file.exists(paths)) &&
  file.exists(metadata_path)
if ("--cached" %in% args && !use_cache) {
  stop("Немає повного знімка даних. Запустіть із --refresh за наявності інтернету.")
}
if (use_cache) {
  metadata <- read.csv(metadata_path, stringsAsFactors = FALSE)
  as_of <- as.Date(metadata$as_of[1])
} else {
  as_of <- Sys.Date()
}
# Не включаємо незавершену поточну торгову сесію.
date_from <- seq(as_of, by = "-3 years", length.out = 2)[2]
date_to <- as_of - 1
if (!use_cache) {
  manifest <- list()
  for (symbol in symbols) {
    url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", symbol,
                  "&cosd=", date_from, "&coed=", date_to)
    download_method <- "quantmod::getSymbols(src=FRED)"
    x <- tryCatch(
      quantmod::getSymbols(symbol, src = "FRED", auto.assign = FALSE),
      error = function(e) NULL
    )
    if (is.null(x)) {
      # Прямий CSV того самого провайдера; джерело та актив не підмінюються.
      message("getSymbols недоступний для ", symbol, "; використовую FRED CSV.")
      raw <- read.csv(url, na.strings = c("", ".", "NA"))
      x <- xts::xts(as.numeric(raw[[2]]), order.by = as.Date(raw[[1]]))
      download_method <- "FRED CSV (fallback)"
    }
    x <- x[paste0(date_from, "/", date_to)]
    raw <- data.frame(observation_date = as.Date(zoo::index(x)),
                      value = as.numeric(x))
    names(raw)[2] <- symbol
    write.csv(raw, file.path("data", paste0(symbol, "_prices.csv")),
              row.names = FALSE, na = "")
    manifest[[symbol]] <- data.frame(
      symbol = symbol, source = "FRED", url = url,
      download_method = download_method,
      fetched_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  }
  write.csv(do.call(rbind, manifest), "data/input_manifest.csv", row.names = FALSE)
  write.csv(data.frame(as_of = as_of, requested_from = date_from,
                       requested_to = date_to), metadata_path, row.names = FALSE)
}

# FRED містить NA у свята. Видаляємо неторгові записи ДО обчислення
# дохідностей, щоб не втратити дохідність першої сесії після свята.
# Інтерполяцію та заповнення нулями не застосовуємо.
series <- list()
quality <- list()
for (symbol in symbols) {
  file <- file.path("data", paste0(symbol, "_prices.csv"))
  raw <- read.csv(file, na.strings = c("", ".", "NA"))
  raw$observation_date <- as.Date(raw[[1]])
  raw <- raw[raw$observation_date >= date_from &
             raw$observation_date <= date_to, ]
  raw <- raw[order(raw$observation_date), ]
  if (anyDuplicated(raw$observation_date)) stop("Повторні дати у ", symbol)
  values <- as.numeric(raw[[symbol]])
  good <- is.finite(values) & values > 0 & !is.na(raw$observation_date)
  prices <- xts::xts(values[good], order.by = raw$observation_date[good])
  if (NROW(prices) < 500) stop("Недостатньо спостережень за три роки: ", symbol)
  ret <- na.omit(PerformanceAnalytics::Return.calculate(prices, method = "log"))
  colnames(ret) <- symbol
  series[[symbol]] <- ret
  quality[[symbol]] <- data.frame(
    symbol = symbol, raw_rows = NROW(raw), removed_missing_or_invalid = sum(!good),
    price_observations = NROW(prices), first_price = as.Date(first(zoo::index(prices))),
    last_price = as.Date(last(zoo::index(prices))),
    first_return = as.Date(first(zoo::index(ret))),
    last_return = as.Date(last(zoo::index(ret)))
  )
}
# Єдина вибірка спільних торгових дат для коректного порівняння.
returns <- na.omit(merge(series[[1]], series[[2]], join = "inner"))
if (!all(is.finite(returns))) stop("Дохідності містять нечислові значення.")
write.csv(data.frame(date = as.Date(zoo::index(returns)), zoo::coredata(returns)),
          "data/log_returns.csv", row.names = FALSE)
quality <- do.call(rbind, quality)
quality$common_return_observations <- NROW(returns)
quality$input_md5 <- unname(tools::md5sum(paths))
write.csv(quality, "data/data_quality.csv", row.names = FALSE)

# invert=FALSE: додатне значення показує величину втрати в одиницях
# логарифмічної дохідності. На графіку поріг дохідності має знак мінус.
results <- list()
for (symbol in symbols) {
  x <- returns[, symbol]
  metric <- function(fun, p, method) {
    as.numeric(fun(x, p = p, method = method, invert = FALSE))
  }
  results[[symbol]] <- data.frame(
    symbol = symbol, observations = NROW(x),
    historical_VaR_95 = metric(PerformanceAnalytics::VaR, .95, "historical"),
    historical_VaR_99 = metric(PerformanceAnalytics::VaR, .99, "historical"),
    gaussian_VaR_95 = metric(PerformanceAnalytics::VaR, .95, "gaussian"),
    gaussian_VaR_99 = metric(PerformanceAnalytics::VaR, .99, "gaussian"),
    historical_ES_95 = metric(PerformanceAnalytics::ES, .95, "historical"),
    gaussian_ES_95 = metric(PerformanceAnalytics::ES, .95, "gaussian")
  )
}
results <- do.call(rbind, results)
rownames(results) <- NULL
write.csv(results, "data/risk_metrics.csv", row.names = FALSE)

# Змістовна перевірка: незалежна формула історичного квантиля,
# впорядкованість VaR та перевищення ES над відповідним VaR.
for (i in seq_along(symbols)) {
  x <- as.numeric(returns[, symbols[i]])
  expected <- -as.numeric(quantile(x, c(.05, .01), type = 7))
  stopifnot(isTRUE(all.equal(expected,
    unname(unlist(results[i, c("historical_VaR_95", "historical_VaR_99")])),
    tolerance = 1e-10)))
  mu <- mean(x)
  sigma <- sqrt(mean((x - mu)^2)) # centered moment, denominator n
  expected_normal <- -mu - sigma * qnorm(c(.05, .01))
  expected_es_hist <- -mean(x[x < quantile(x, .05, type = 7)])
  expected_es_normal <- -mu + sigma * dnorm(qnorm(.05)) / .05
  stopifnot(isTRUE(all.equal(expected_normal,
    unname(unlist(results[i, c("gaussian_VaR_95", "gaussian_VaR_99")])),
    tolerance = 1e-10)),
    isTRUE(all.equal(expected_es_hist, results$historical_ES_95[i], tolerance = 1e-10)),
    isTRUE(all.equal(expected_es_normal, results$gaussian_ES_95[i], tolerance = 1e-10)))
  stopifnot(results$historical_VaR_99[i] >= results$historical_VaR_95[i],
            results$gaussian_VaR_99[i] >= results$gaussian_VaR_95[i],
            results$historical_ES_95[i] >= results$historical_VaR_95[i],
            results$gaussian_ES_95[i] >= results$gaussian_VaR_95[i])
}

plot_data <- do.call(rbind, lapply(symbols, function(s) {
  data.frame(asset = unname(labels[s]), return_pct = 100 * as.numeric(returns[, s]))
}))
lines <- do.call(rbind, lapply(seq_along(symbols), function(i) {
  data.frame(asset = unname(labels[symbols[i]]),
    threshold = -100 * as.numeric(results[i, c("historical_VaR_95",
      "historical_VaR_99", "gaussian_VaR_95", "gaussian_VaR_99")]),
    method = rep(c("Historical", "Gaussian"), each = 2),
    confidence = rep(c("95%", "99%"), 2))
}))
draw_chart <- function(d, l, subtitle) {
  ggplot2::ggplot(d, ggplot2::aes(return_pct)) +
    ggplot2::geom_histogram(bins = 70, fill = "#AECBDD", color = "white",
                           linewidth = .2) +
    ggplot2::geom_vline(data = l, ggplot2::aes(xintercept = threshold,
      color = confidence, linetype = method), linewidth = .8) +
    ggplot2::facet_wrap(~asset, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(values = c("95%" = "#D17A00", "99%" = "#AE2141")) +
    ggplot2::scale_linetype_manual(values = c(Historical = "solid", Gaussian = "dashed")) +
    ggplot2::labs(title = "Daily log returns and Value at Risk", subtitle = subtitle,
      x = "Daily log return (%)", y = "Trading sessions", color = "Confidence",
      linetype = "Method", caption = "Source: FRED. Lines show negative return thresholds, not positive loss magnitudes.") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      plot.caption = ggplot2::element_text(size = 8))
}
period <- paste(format(first(zoo::index(returns))), "to",
                format(last(zoo::index(returns))), "|", NROW(returns), "observations each")
chart <- draw_chart(plot_data, lines, period)
ggplot2::ggsave("data/var_chart.png", chart, width = 10, height = 8, dpi = 180, bg = "white")
for (symbol in symbols) {
  chart_one <- draw_chart(plot_data[plot_data$asset == labels[symbol], ],
                          lines[lines$asset == labels[symbol], ], period)
  ggplot2::ggsave(file.path("data", paste0("var_chart_", symbol, ".png")),
                  chart_one, width = 10, height = 5, dpi = 180, bg = "white")
}

# README автоматично оновлюється з фактичних результатів цього запуску.
fmt <- function(v) sprintf("%.3f%%", 100 * v)
table_rows <- vapply(seq_along(symbols), function(i) {
  paste0("| ", labels[symbols[i]], " | ", paste(fmt(as.numeric(results[i, 3:8])),
                                              collapse = " | "), " |")
}, character(1))
more_risky <- labels[results$symbol[which.max(results$historical_ES_95)]]
readme <- c(
  "# risk-management-hw1", "",
  "Практичне заняття №2. Методичний інструментарій ризик-орієнтованого управління фінансовими установами.",
  "", "**Виконала:** Кириченко Ангеліна Вадимівна.", "",
  "## Мета та обрані ряди", "",
  "Розрахувати одноденні VaR та Expected Shortfall для S&P 500 і NASDAQ Composite за останні три роки. Індекси використано як два ринкові об’єкти аналізу; вони не є окремими акціями чи безпосередньо придбаними інструментами.",
  "Джерело щоденних рівнів закриття — FRED (первинні джерела: S&P Dow Jones Indices LLC та Nasdaq, Inc.). Ряди відображають рух цін, без реінвестування дивідендів. Yahoo Finance повертав HTTP 429 під час підготовки, тому використано інше відкрите джерело; це не дані AAPL.", "",
  paste0("Дата зрізу: **", as_of, "**. Запитаний інтервал цін: **", date_from,
         " — ", date_to, "**. Поточна незавершена сесія виключена."),
  paste0("Фактичний спільний інтервал дохідностей: **", first(zoo::index(returns)),
         " — ", last(zoo::index(returns)), "**, по **", NROW(returns), "** спостережень на індекс."), "",
  "## Структура", "",
  "- `scripts/main_analysis.R` — завантаження, очищення, розрахунки, перевірки, графіки та оновлення README.",
  "- `data/*_prices.csv` — знімок вхідних рівнів закриття з пропусками у неторгові дні.",
  "- `data/log_returns.csv` — очищені логарифмічні дохідності на спільних датах.",
  "- `data/risk_metrics.csv` — метрики без округлення, у частках одиниці.",
  "- `data/data_quality.csv` — дати, кількість рядків, вилучені пропуски та контрольні суми входів.",
  "- `data/input_manifest.csv`, `data/analysis_metadata.csv` — джерело, спосіб завантаження та параметри вибірки.",
  "- `data/var_chart.png`, `data/var_chart_*.png` — спільна й окремі гістограми з чотирма лініями VaR.",
  "- `data/sessionInfo.txt` — версії R і пакетів; `.gitignore` — шаблон R; `.Rproj` — проєкт RStudio.", "",
  "## Запуск", "",
  "Встановіть R, RStudio та Git. Відкрийте `risk-management-hw1.Rproj`. Перший запуск за потреби встановить `quantmod`, `PerformanceAnalytics` та `ggplot2` із CRAN, тому потрібен інтернет. Далі для збереженого знімка даних мережа не потрібна.", "",
  "У консолі RStudio з кореня проєкту:", "", "```r", 'source("scripts/main_analysis.R", encoding = "UTF-8")', "```", "",
  "Або в терміналі:", "", "```sh", "Rscript scripts/main_analysis.R --cached",
  "# Оновити з FRED до останньої завершеної дати та перерахувати все:",
  "Rscript scripts/main_analysis.R --refresh", "```", "",
  "Режим за замовчуванням повторно використовує повний знімок. `--cached` вимагає його наявності; `--refresh` завантажує дані наново та оновлює дату зрізу. Для відтворення наведених нижче чисел використовуйте `--cached`.", "",
  "## Методика", "",
  "Логарифмічна дохідність: `r_t = ln(P_t / P_(t-1))`. Неторгові записи NA вилучаються перед розрахунком, після чого дохідності очищаються та узгоджуються за спільними торговими датами. Нулі замість пропусків не підставляються, викиди не видаляються.", "",
  "Позначимо рівень довіри як p, а лівохвостову ймовірність як alpha = 1 - p:", "",
  "- Historical VaR: `-quantile(r, alpha)`, квантиль R type=7; виклик `VaR(..., method=\"historical\", invert=FALSE)`.",
  "- Gaussian VaR: `-mu - sigma * qnorm(alpha)`; `VaR(..., method=\"gaussian\", invert=FALSE)`.",
  "- Historical ES: оцінка середньої втрати в лівому хвості за історичним розподілом; `ES(..., p=.95, method=\"historical\", invert=FALSE)`.",
  "- Gaussian ES: `-mu + sigma * dnorm(qnorm(alpha)) / alpha`; доданий для порівняння двох методів ES.", "",
  "Тут `mu = mean(r)`, а `sigma = sqrt(mean((r - mu)^2))`: реалізація PerformanceAnalytics використовує другий центральний момент із дільником n. Історичний ES у пакеті — мінус середнє спостережень, строго менших за історичний квантиль 5%. У скрипті всі шість метрик перевіряються також прямими формулами.", "",
  "У таблиці всі метрики подано як **додатну величину одноденної втрати у відсотках логарифмічної дохідності**. Це не максимальний можливий збиток. На гістограмі відповідні пороги від’ємні. Точне переведення величини v у відсоток падіння вартості: `100 * (1 - exp(-v))`, де v — метрика у частках одиниці; таблиця цього перетворення не застосовує.", "",
  "## Результати", "",
  "| Індекс | Hist VaR 95% | Hist VaR 99% | Normal VaR 95% | Normal VaR 99% | Hist ES 95% | Normal ES 95% |",
  "|---|---:|---:|---:|---:|---:|---:|", table_rows, "",
  "![Гістограми дохідностей і пороги VaR](data/var_chart.png)", "",
  "## Висновки та обмеження", "",
  paste0("За історичним ES 95% більший хвостовий ризик у цій вибірці має **", more_risky,
         "**. Це висновок лише для наведеного періоду й обраної метрики."),
  "VaR 99% перевищує VaR 95%, а ES 95% перевищує відповідний VaR 95%: ES враховує тяжкість втрат за порогом. Нормальна й історична оцінки різняться, бо перша припускає нормальний розподіл, а друга спирається на фактичну вибірку.",
  "VaR 95% означає, що за моделлю поріг може бути перевищено приблизно в 5% торгових днів; це не гарантія майбутньої частоти. ES 95% характеризує середню хвостову втрату, а не ще один рівень довіри. За три роки хвіст 1% містить мало спостережень, тому VaR 99% чутливий до окремих кризових днів.",
  "Показники не річні та не портфельні: кожний індекс оцінено окремо. Не враховано комісії, ліквідність, валютний ризик та зміну режиму ринку. Формальний бек-тест на незалежній вибірці не проводився; модель нормальності статистично не доводилася.", "",
  "## Git, GitHub і здача", "",
  "Репозиторій: https://github.com/AnGELina-Kyrychenko/risk-management-hw1", "",
  "README, R-скрипт, дані та графіки збережено у Git. Надсилання посилання викладачу виконується окремо після власного перегляду роботи.", "",
  "Для клонування на особистому ПК: RStudio → File → New Project → Version Control → Git. Вставте URL нижче, оберіть папку й натисніть Create Project:", "",
  "```text", "https://github.com/AnGELina-Kyrychenko/risk-management-hw1.git", "```", "",
  "Після змін позначте потрібні файли Staged у панелі Git, виконайте Commit зі змістовним повідомленням і Push. Аналогічно у терміналі:", "",
  "```sh", "git status", "git add scripts/main_analysis.R data README.md",
  'git commit -m \"feat: refresh VaR and ES analysis\"', "git push", "```", "",
  "## Джерела", "",
  "1. S&P Dow Jones Indices LLC. S&P 500 [SP500], FRED: https://fred.stlouisfed.org/series/SP500", "2. Nasdaq, Inc. NASDAQ Composite [NASDAQCOM], FRED: https://fred.stlouisfed.org/series/NASDAQCOM",
  "3. quantmod: https://search.r-project.org/CRAN/refmans/quantmod/html/getSymbols.FRED.html",
  "4. PerformanceAnalytics, VaR: https://search.r-project.org/CRAN/refmans/PerformanceAnalytics/html/VaR.html",
  "5. PerformanceAnalytics, ES: https://search.r-project.org/CRAN/refmans/PerformanceAnalytics/html/ES.html",
  "6. Шаблон .gitignore для R: https://github.com/github/gitignore/blob/main/R.gitignore", "",
  "Права на вхідні індексні дані належать їхнім правовласникам; відкритий доступ до FRED не означає передачі цих прав. Проєкт є навчальним. AI використано як помічника для підготовки коду й пояснень; наведені числа отримано виконанням R-скрипту, а не згенеровано як приклад."
)
writeLines(enc2utf8(readme), "README.md", useBytes = TRUE)
capture.output(sessionInfo(), file = "data/sessionInfo.txt")
cat("\nМетрики (частки одиниці, додатні втрати):\n")
print(results, row.names = FALSE)
cat("\nУспішно. Збережено дані, README, метрики та 3 PNG.\n")
