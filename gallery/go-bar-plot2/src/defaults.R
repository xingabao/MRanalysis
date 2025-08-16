suppressWarnings(suppressMessages(library(glue)))

#' Title
#'
#' @param strs 
#' @param len 
#'
#' @return
#' @export
#'
#' @examples
truncate.strings <- function(strs, len) {
  result <- character(length(strs))
  
  for (i in seq_along(strs)) {
    str <- strs[i]
    
    if (nchar(str) > len) {
      result[i] <- paste0(substr(str, 1, len), " ...")
    } else {
      result[i] <- str
    }
  }
  
  return(result)
}

# 可选的线段样式
lineTypes <- list(0, 1, 2, 3, 4, 5, 6)

# 可选线段的末端样式
lineEnds <- list("butt", "round", "square")

# 可选文本字体样式
faces <- list("plain", "italic", "bold", "bold.italic")

# 可选字体类型
font.families <- list()
suppressMessages(suppressWarnings(
  for (font.family in c('none', sysfonts::font_families(), extrafont::fonts())) {
    font.families[[truncate.strings(font.family, 35)]] = font.family
  }
))

# 可选自选主题类型
themes <- list(
  "none",
  "ggplot2::bw",
  "ggplot2::classic",
  "ggplot2::grey",
  "ggplot2::void",
  "ggplot2::dark",
  "ggplot2::light",
  "ggplot2::linedraw",
  "ggplot2::minimal",
  "ggthemes::base",
  "ggthemes::calc",
  "ggthemes::clean",
  "ggthemes::economist",
  "ggthemes::economist_white",
  "ggthemes::excel",
  "ggthemes::excel_new",
  "ggthemes::few",
  "ggthemes::fivethirtyeight",
  "ggthemes::foundation",
  "ggthemes::gdocs",
  "ggthemes::hc",
  "ggthemes::igray",
  "ggthemes::map",
  "ggthemes::solarized",
  "ggthemes::solid",
  "ggthemes::tufte",
  "ggthemes::wsj"
)

base_size <- 11
theme_default <- function(base_size = 11, base_family = NULL) {
  base_line_size = base_size / 22
  base_rect_size = base_size / 22
  half_line <- base_size / 2

  t <- theme(

    line = element_line(colour = "black", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", linewidth = base_rect_size, linetype = 1),
    text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
    title = element_text(family = base_family, face = "plain", colour = "black", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),

    axis.line = element_blank(),
    axis.line.x = NULL,
    axis.line.y = NULL,
    axis.line.x.top = element_line(colour = "black", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.line.x.bottom = element_line(colour = "black", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.line.y.left = element_line(colour = "black", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.line.y.right = element_line(colour = "black", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.text = element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 0, angle = 0, lineheight = 0.9, margin = margin(t = 0.8 * half_line / 2, r = 0, b = 0, l = 0, unit = 'pt')),
    axis.text.x.top = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 0, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 0.8 * half_line / 2, l = 0, unit = 'pt')),
    axis.text.x.bottom = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 0, angle = 0, lineheight = 0.9, margin = margin(t = 0.8 * half_line / 2, r = 0, b = 0, l = 0, unit = 'pt')),
    axis.text.y = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0.8 * half_line / 2, b = 0, l = 0, unit = 'pt')),
    axis.text.y.left = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0.8 * half_line / 2, b = 0, l = 0, unit = 'pt')),
    axis.text.y.right = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 0, l = 0.8 * half_line / 2, unit = 'pt')),

    axis.ticks = element_line(colour = "grey20"),
    axis.ticks.x.top = element_line(colour = "grey20", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.ticks.x.bottom = element_line(colour = "grey20", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.ticks.y.left = element_line(colour = "grey20", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.ticks.y.right = element_line(colour = "grey20", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    axis.ticks.length = unit(half_line / 2, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = unit(half_line / 2, "pt"),
    axis.ticks.length.x.bottom = unit(half_line / 2, "pt"),
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = unit(half_line / 2, "pt"),
    axis.ticks.length.y.right = unit(half_line / 2, "pt"),
    axis.title.x = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 1, angle = 0, lineheight = 0.9, margin = margin(t = half_line / 2, r = 0, b = 0, l = 0, unit = 'pt')),
    axis.title.x.top = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 1, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0, b = half_line / 2, l = 0, unit = 'pt')),
    axis.title.x.bottom = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 1, angle = 0, lineheight = 0.9, margin = margin(t = half_line / 2, r = 0, b = 0, l = 0, unit = 'pt')),
    axis.title.y = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 1, angle = 90, lineheight = 0.9, margin = margin(t = 0, r = half_line / 2, b = 0, l = 0, unit = 'pt')),
    axis.title.y.left = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 1, angle = 90, lineheight = 0.9, margin = margin(t = 0, r = half_line / 2, b = 0, l = 0, unit = 'pt')),
    axis.title.y.right = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0.5, vjust = 1, angle = -90, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 0, l = half_line / 2, unit = 'pt')),

    legend.background = element_rect(fill = "white", colour = NA, linewidth = base_rect_size, linetype = 1),
    legend.spacing = unit(2 * half_line, "pt"),
    legend.spacing.x = grid::unit(2 * half_line, "pt"),
    legend.spacing.y = grid::unit(2 * half_line, "pt"),
    legend.margin = margin(t = half_line, r = half_line, b = half_line, l = half_line, unit = 'pt'),
    legend.key = element_rect(fill = "grey95", colour = NA, linewidth = base_rect_size, linetype = 1),
    legend.key.size = unit(1.2, "lines"),
    legend.key.height = grid::unit(half_line / 2, units = 'pt'),
    legend.key.width = grid::unit(half_line / 2, units = 'pt'),
    legend.text = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(0.8), hjust = 0, vjust = 0.5, angle = 0.5, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt')),
    legend.text.align = NULL,
    legend.title = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt')),
    legend.title.align = NULL,
    legend.position = "right",
    legend.direction = 'vertical',
    legend.justification = "center",
    legend.box = NULL,
    legend.box.margin = margin(t = 0, l = 0, b = 0, r = 0, unit = "pt"),
    legend.box.background = element_rect(fill = "white", colour = NA, linewidth = base_rect_size, linetype = 1),
    legend.box.spacing = unit(2 * half_line, "pt"),

    panel.background = element_rect(fill = "grey92", colour = NA, linewidth = base_rect_size, linetype = 1),
    panel.border = element_rect(fill = "white", colour = "black", linewidth = base_rect_size, linetype = 1),
    panel.grid = element_line(colour = "white"),
    panel.grid.minor = element_line(linewidth = rel(0.5)),
    panel.grid.major.x = element_line(colour = "white", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    panel.grid.major.y = element_line(colour = "white", linewidth = base_line_size, linetype = 1, lineend = "butt"),
    panel.grid.minor.x = element_line(colour = "white", linewidth = base_line_size * 0.5, linetype = 1, lineend = "butt"),
    panel.grid.minor.y = element_line(colour = "white", linewidth = base_line_size * 0.5, linetype = 1, lineend = "butt"),
    panel.spacing = unit(half_line, "pt"),
    panel.spacing.x = unit(half_line, "pt"),
    panel.spacing.y = unit(half_line, "pt"),
    panel.ontop = FALSE,

    strip.background = element_rect(fill = "grey85", colour = NA),
    strip.background.x = element_rect(fill = "grey85", colour = NA, linewidth = base_rect_size, linetype = 1),
    strip.background.y = element_rect(fill = "grey85", colour = NA, linewidth = base_rect_size, linetype = 1),
    strip.clip = "inherit",
    strip.text = element_text(colour = "grey10", size = rel(0.8), margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)),
    strip.text.x = NULL,
    strip.text.x.top = element_text(family = NA, face = 'plain', colour = 'grey10', size = rel(0.8), hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(t = 0.8 * half_line, r = 0.8 * half_line, b = 0.8 * half_line, l = 0.8 * half_line, unit = 'pt')),
    strip.text.x.bottom = element_text(family = NA, face = 'plain', colour = 'grey10', size = rel(0.8), hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9, margin = margin(t = 0.8 * half_line, r = 0.8 * half_line, b = 0.8 * half_line, l = 0.8 * half_line, unit = 'pt')),
    strip.text.y = element_text(angle = -90),
    strip.text.y.left = element_text(family = NA, face = 'plain', colour = 'grey10', size = rel(0.8), hjust = 0.5, vjust = 0.5, angle = 90, lineheight = 0.9, margin = margin(t = 0.8 * half_line, r = 0.8 * half_line, b = 0.8 * half_line, l = 0.8 * half_line, unit = 'pt')),
    strip.text.y.right = element_text(family = NA, face = 'plain', colour = 'grey10', size = rel(0.8), hjust = 0.5, vjust = 0.5, angle = -90, lineheight = 0.9, margin = margin(t = 0.8 * half_line, r = 0.8 * half_line, b = 0.8 * half_line, l = 0.8 * half_line, unit = 'pt')),
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    strip.switch.pad.grid = unit(half_line / 2, "pt"),
    strip.switch.pad.wrap = unit(half_line / 2, "pt"),

    plot.background = element_rect(fill = "white", colour = "white", linewidth = base_rect_size, linetype = 1),
    plot.title = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1.2), hjust = 0, vjust = 1, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0, b = half_line, l = 0, unit = 'pt')),
    plot.title.position = "panel",
    plot.subtitle = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1), hjust = 0, vjust = 1, angle = 0, lineheight = 0.9, margin = margin(t = 0, r = 0, b = half_line, l = 0, unit = 'pt')),
    plot.caption = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(0.8), hjust = 0, vjust = 1, angle = 1, lineheight = 0.9, margin = margin(t = 0, r = 0, b = half_line, l = 0, unit = 'pt')),
    plot.caption.position = "panel",
    plot.tag = ggtext::element_markdown(family = NA, face = 'plain', colour = '#000000', size = rel(1.2), hjust = 0, vjust = 0.5, angle = 0.5, lineheight = 0.9, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = 'pt')),
    plot.tag.position = 'topleft',
    plot.margin = margin(t = half_line, l = half_line, b = half_line, r = half_line, unit = "pt"),

    complete = TRUE,
    validate = TRUE
  )

  ggplot2:::ggplot_global$theme_all_null %+replace% t
}

# 是否开启静默模式
QUITE = TRUE

# 限制多少秒内的修改为一次
DEBOUNCE.0 = 0
DEBOUNCE.A = 500
DEBOUNCE.L = 1000
DEBOUNCE.P = 1000  # for wIDTH & HEIGHT only
DEBOUNCE.M = 2000
DEBOUNCE.H = 3000
DEBOUNCE.Z = 5000

# Confidence Level
confidence.level.z = list(
  '90%' = 1.645,
  '95%' = 1.960,
  '99%' = 2.575,
  '99.9%' = 3.291,
  '99.99%' = 3.819
)

# 网站流量统计
web.statistic.baidu <- htmltools::HTML(
  ' <!-- Baidu tag (hm.js) -->
		<script>
			var _hmt = _hmt || [];
			(function() {
				var hm = document.createElement("script");
				hm.src = "https://hm.baidu.com/hm.js?5b3d8cf0e6ebd9194c1ad25d9448e7ec";
				var s = document.getElementsByTagName("script")[0];
				s.parentNode.insertBefore(hm, s);
			})();
		</script>')

web.statistic.google <- htmltools::HTML(
  '	<!-- Google tag (gtag.js) -->
		<script async src="https://www.googletagmanager.com/gtag/js?id=G-J5S64KQ4ME"></script>
		<script>
			window.dataLayer = window.dataLayer || [];
			function gtag() {
				dataLayer.push(arguments);
			}
			gtag("js", new Date());
			gtag("config", "G-J5S64KQ4ME");
		</script>')


BCFTOOLS <- '/tools/bcftools-1.22/bcftools'

# MR HOME
MR.HOME <- 'http://mranalysis.cn/'
URL.2SMR.L = glue('<a href="{MR.HOME}analysis/2SMR-local" target="_blank">2SMR local</a>')
URL.MVMR.L = glue('<a href="{MR.HOME}analysis/MVMR-local" target="_blank">MVMR local</a>')
URL.MMR.L = glue('<a href="{MR.HOME}analysis/MMR-local" target="_blank">MMR local</a>')
URL.kit.L = '<a href="https://github.com/Li-OmicsLab-MPU/GWASkit" target="_blank">GWASkit</a>'
TIP.2SMR.L = glue('Online access to the interested exposure and outcome data may encounter error messages due to the extremely high server traffic to the IEU open GWAS project or you want to use GWAS summary data from other sources. It is recommended to download GWAS summary data and use {URL.kit.L} to preprocess GWAS data into standardized VCF format, and then upload this VCF file to our {URL.2SMR.L} mode for analysis.')
TIP.MVMR.L = glue('Online access to the interested exposure and outcome data may encounter error messages due to the extremely high server traffic to the IEU open GWAS project or you want to use GWAS summary data from other sources. It is recommended to download GWAS summary data and use {URL.kit.L} to preprocess GWAS data into standardized VCF format, and then upload this VCF file our {URL.MVMR.L} mode for analysis.')
TIP.MMR.L = glue('Online access to the interested exposure and outcome data may encounter error messages due to the extremely high server traffic to the IEU open GWAS project or you want to use GWAS summary data from other sources. It is recommended to download GWAS summary data and use {URL.kit.L} to preprocess GWAS data into standardized VCF format, and then upload this VCF file our {URL.MMR.L} mode for analysis.')




