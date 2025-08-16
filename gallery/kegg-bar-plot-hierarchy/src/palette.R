# ggsci
if (TRUE) {
  #	AAAS Journal Color Palettes
  pal_aaas <- c("#3B4992FF", "#EE0000FF", "#008B45FF", "#631879FF", "#008280FF", "#BB0021FF", "#5F559BFF", "#A20056FF", "#808180FF", "#1B1919FF")
  
  # COSMIC Color Palettes
  pal_cosmic_hallmarks_light <- c("#2E2A2BFF", "#CF4E9CFF", "#8C57A2FF", "#358DB9FF", "#82581FFF", "#2F509EFF", "#E5614CFF", "#97A1A7FF", "#3DA873FF", "#DC9445FF")
  pal_cosmic_hallmarks_dark <- c("#171717FF", "#7D0226FF", "#300049FF", "#165459FF", "#3F2327FF", "#0B1948FF", "#E71012FF", "#555555FF", "#193006FF", "#A8450CFF")
  pal_cosmic_signature_substitutions <- c("#5ABCEBFF", "#050708FF", "#D33C32FF", "#CBCACBFF", "#ABCD72FF", "#E7C9C6FF")
  
  # D3.js Color Palettes
  pal_d3_category20 <- c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF", "#D62728FF", "#9467BDFF", "#8C564BFF", "#E377C2FF", "#7F7F7FFF", "#BCBD22FF", "#17BECFFF", "#AEC7E8FF", "#FFBB78FF", "#98DF8AFF",  "#FF9896FF", "#C5B0D5FF", "#C49C94FF", "#F7B6D2FF", "#C7C7C7FF", "#DBDB8DFF", "#9EDAE5FF")
  pal_d3_category20b <- c("#393B79FF", "#637939FF", "#8C6D31FF", "#843C39FF", "#7B4173FF", "#5254A3FF", "#8CA252FF", "#BD9E39FF", "#AD494AFF", "#A55194FF", "#6B6ECFFF", "#B5CF6BFF", "#E7BA52FF", "#D6616BFF", "#CE6DBDFF", "#9C9EDEFF", "#CEDB9CFF", "#E7CB94FF", "#E7969CFF", "#DE9ED6FF")
  pal_d3_category20c <- c("#3182BDFF", "#E6550DFF", "#31A354FF", "#756BB1FF", "#636363FF", "#6BAED6FF", "#FD8D3CFF", "#74C476FF", "#9E9AC8FF", "#969696FF", "#9ECAE1FF", "#FDAE6BFF", "#A1D99BFF", "#BCBDDCFF", "#BDBDBDFF", "#C6DBEFFF", "#FDD0A2FF", "#C7E9C0FF", "#DADAEBFF", "#D9D9D9FF")
  
  # Flat UI Color Palettes
  pal_flatui_default <- c("#C0392BFF", "#D35400FF", "#F39C12FF", "#27AE60FF", "#16A085FF", "#2980B9FF", "#8E44ADFF", "#2C3E50FF", "#7F8C8DFF", "#BDC3C7FF")
  pal_flatui_flattastic <- c("#DA4453FF", "#E95546FF", "#F6BA59FF", "#8BC163FF", "#34BC9DFF", "#3BB0D6FF", "#4B8AD6FF", "#977BD5FF", "#D870A9FF", "#E6E9EDFF", "#AAB2BCFF", "#434A53FF")
  pal_flatui_aussie <- c("#F9CA24FF", "#F0932BFF", "#EB4D4BFF", "#6AB04CFF", "#C7ECEEFF", "#22A6B3FF", "#BE2EDDFF", "#4834D4FF", "#130F40FF", "#535C68FF")
  
  # Frontiers Color Palettes
  pal_frontiers_default <- c("#D51317FF", "#F39200FF", "#EFD500FF", "#95C11FFF", "#007B3DFF", "#31B7BCFF", "#0094CDFF", "#164194FF", "#6F286AFF", "#706F6FFF")
  
  # The Futurama Color Palettes
  pal_futurama_planetexpress <- c("#FF6F00FF", "#C71000FF", "#008EA0FF", "#8A4198FF", "#5A9599FF", "#FF6348FF", "#84D7E1FF", "#FF95A8FF", "#3D3B25FF", "#ADE2D0FF", "#1A5354FF", "#3F4041FF")
  
  # The GSEA GenePattern Color Palettes
  pal_gsea_default <- c("#4500ACFF", "#2600D1FF", "#6B58EEFF", "#8787FFFF", "#C6C0FFFF", "#D4D4FFFF", "#FFBFE5FF", "#FF8888FF", "#FF707FFF", "#FF5959FF", "#EE3F3FFF", "#D60C00FF")
  
  # Integrative Genomics Viewer (IGV) Color Palettes
  pal_igv_default <- c("#5050FFFF", "#CE3D32FF", "#749B58FF", "#F0E685FF", "#466983FF", "#BA6338FF", "#5DB1DDFF", "#802268FF", "#6BD76BFF", "#D595A7FF", "#924822FF", "#837B8DFF",
                       "#C75127FF", "#D58F5CFF", "#7A65A5FF", "#E4AF69FF", "#3B1B53FF", "#CDDEB7FF", "#612A79FF", "#AE1F63FF", "#E7C76FFF", "#5A655EFF", "#CC9900FF", "#99CC00FF",
                       "#A9A9A9FF", "#CC9900FF", "#99CC00FF", "#33CC00FF", "#00CC33FF", "#00CC99FF", "#0099CCFF", "#0A47FFFF", "#4775FFFF", "#FFC20AFF", "#FFD147FF", "#990033FF",
                       "#991A00FF", "#996600FF", "#809900FF", "#339900FF", "#00991AFF", "#009966FF", "#008099FF", "#003399FF", "#1A0099FF", "#660099FF", "#990080FF", "#D60047FF",
                       "#FF1463FF", "#00D68FFF", "#14FFB1FF")
  pal_igv_alternating <- c("#5773CCFF", "#FFB900FF")
  
  # Journal of the American Medical Association Color Palettes
  pal_jama_default <- c("#374E55FF", "#DF8F44FF", "#00A1D5FF", "#B24745FF", "#79AF97FF", "#6A6599FF", "#80796BFF")
  
  # Journal of Clinical Oncology Color Palettes
  pal_jco_default <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#7AA6DCFF", "#003C67FF", "#8F7700FF", "#3B3B3BFF", "#A73030FF", "#4A6990FF")
  
  # Lancet Journal Color Palettes
  pal_lancet_lanonc <- c("#00468BFF", "#ED0000FF", "#42B540FF", "#0099B4FF", "#925E9FFF", "#FDAF91FF", "#AD002AFF", "#ADB6B6FF", "#1B1919FF")
  
  # LocusZoom Color Palette
  pal_locuszoom_default <- c("#D43F3AFF", "#EEA236FF", "#5CB85CFF", "#46B8DAFF", "#357EBDFF", "#9632B8FF", "#B8B8B8FF")
  
  # Material Design Color Palettes
  pal_material_indigo <- c("#FF0000FF", "#FFC0CBFF", "#A020F0FF", "#0000FFFF", "#00FFFFFF", "#00FF00FF", "#FFFF00FF", "#FFA500FF", "#A52A2AFF", "#BEBEBEFF", "#7985CBFF", "#19227EFF")
  
  # NEJM Color Palettes
  pal_nejm_default <- c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "#7876B1FF", "#6F99ADFF", "#FFDC91FF", "#EE4C97FF")
  
  # NPG Journal Color Palettes
  pal_npg_nrc <- c("#E64B35FF", "#4DBBD5FF", "#00A087FF", "#3C5488FF", "#F39B7FFF", "#8491B4FF", "#91D1C2FF", "#DC0000FF", "#7E6148FF", "#B09C85FF")
  
  # Rick and Morty Color Palettes
  pal_rickandmorty_schwifty <- c("#FAFD7CFF", "#82491EFF", "#24325FFF", "#B7E4F9FF", "#FB6467FF", "#526E2DFF", "#E762D7FF", "#E89242FF", "#FAE48BFF", "#A6EEE6FF", "#917C5DFF", "#69C8ECFF")
  
  # The Simpsons Color Palettes
  pal_simpsons_springfield <- c("#FED439FF", "#709AE1FF", "#8A9197FF", "#D2AF81FF", "#FD7446FF", "#D5E4A2FF", "#197EC0FF", "#F05C3BFF", "#46732EFF", "#71D0F5FF", "#370335FF", "#075149FF", "#C80813FF", "#91331FFF", "#1A9993FF", "#FD8CC1FF")
  
  # Star Trek Color Palettes
  pal_startrek_uniform <- c("#CC0C00FF", "#5C88DAFF", "#84BD00FF", "#FFCD00FF", "#7C878EFF", "#00B5E2FF", "#00AF66FF")
  
  # Tron Legacy Color Palettes
  pal_tron_legacy <- c("#FF410DFF", "#6EE2FFFF", "#F7C530FF", "#95CC5EFF", "#D0DFE6FF", "#F79D1EFF", "#748AA6FF")
  
  # The University of Chicago Color Palettes
  pal_uchicago_default <- c("#800000FF", "#767676FF", "#FFA319FF", "#8A9045FF", "#155F83FF", "#C16622FF", "#8F3931FF", "#58593FFF", "#350E20FF")
  pal_uchicago_light <- c("#800000FF", "#D6D6CEFF", "#FFB547FF", "#ADB17DFF", "#5B8FA8FF", "#D49464FF", "#B1746FFF", "#8A8B79FF", "#725663FF")
  pal_uchicago_dark <- c("#800000FF", "#767676FF", "#CC8214FF", "#616530FF", "#0F425CFF", "#9A5324FF", "#642822FF", "#3E3E23FF", "#350E20FF")
  
  # UCSC Genome Browser Color Palette
  pal_ucscgb_default <- c("#FF0000FF", "#FF9900FF", "#FFCC00FF", "#00FF00FF", "#6699FFFF", "#CC33FFFF", "#99991EFF", "#999999FF", "#FF00CCFF", "#CC0000FF", "#FFCCCCFF", "#FFFF00FF",
                          "#CCFF00FF", "#358000FF", "#0000CCFF", "#99CCFFFF", "#00FFFFFF", "#CCFFFFFF", "#9900CCFF", "#CC99FFFF", "#996600FF", "#666600FF", "#666666FF", "#CCCCCCFF",
                          "#79CC3DFF", "#CCCC99FF" )
  
  # The GSEA GenePattern Color Palettes
  pal_gsea_default <- c("#4500ACFF", "#2600D1FF", "#6B58EEFF", "#8787FFFF", "#C6C0FFFF", "#D4D4FFFF", "#FFBFE5FF", "#FF8888FF", "#FF707FFF", "#FF5959FF", "#EE3F3FFF", "#D60C00FF")
}

# ggthemes
if (TRUE) {
  # Calc Color Palette (discrete)
  calc_pal <- c("#004586", "#ff420e", "#ffd320", "#579d1c", "#7e0021", "#83caff", "#314004", "#aecf00", "#4b1f6f", "#ff950e", "#c5000b", "#0084d1")
  
  # Colorblind Color Palette
  colorblind_pal <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # Economist Color Palette
  economist_pal <- c("#6794a7", "#014d64", "#01a2d9", "#7ad2f6", "#00887d", "#76c0c1", "#7c260b", "#ee8f71", "#adadad")
  
  # Color Palettes Few
  few_pal <- c("#5DA5DA", "#FAA43A", "#60BD68", "#F17CB0", "#B2912F", "#B276B2", "#DECF3F")
  few_pal_dark <- c("#265DAB", "#DF5C24", "#059748", "#E5126F", "#9D722A", "#7B3A96", "#C7B42E")
  few_pal_light <- c("#88BDE6", "#FBB258", "#90CD97", "#F6AAC9", "#BFA554", "#BC99C7", "#EDDD46")
  
  # FiveThirtyEight Color Palette
  fivethirtyeight_pal <- c("#008FD5", "#FF2700", "#77AB43")
  
  # Google Docs Color Palette
  gdocs_pal <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477", "#66aa00", "#b82e2e", "#316395")
  
  # Highcharts Color Palette
  hc_pal <- c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#e4d354", "#8085e8", "#8d4653", "#91e8e1")
  hc_pal_dark_unica <- c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9", "#f15c80", "#e4d354", "#8085e8", "#8d4653", "#91e8e1")
  
  # Color Palettes from Paul Tol's "Colour Schemes"
  ptol_pal <- c("#332288", "#6699CC", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#CC6677", "#AA4466", "#882255", "#AA4499")
  
  # Solarized Color Palette
  solarized_pal <- c("#268bd2", "#b58900", "#cb4b16", "#dc322f", "#d33682", "#6c71c4", "#2aa198", "#859900")
  
  # Stata Color Palettes
  stata_pal_s2color <- c("#1a476f", "#90353b", "#55752f", "#e37e00", "#6e8e84", "#c10534", "#938dd2", "#cac27e", "#a0522d", "#7b92a8", "#2d6d66", "#9c8847", "#bfa19c", "#ffd200", "#d9e6eb")
  stata_pal_s1rcolor <- c("#ffff00", "#00ff00", "#0080ff", "#ff00ff", "#ff7f00", "#ff0000", "#add8e6", "#ffe474", "#00ff80", "#c0dcc0", "#ff4500", "#0000ff", "#ff0080", "#6e8e84", "#a0522d")
  stata_pal_s1color <- c("#006000", "#ff4500", "#1a476f", "#90353b", "#6e8e84", "#a0522d", "#ff7f00", "#ff00ff", "#00ffff", "#ff0000", "#00ff00", "#9c8847", "#800080", "#c0dcc0", "#add8e6")
  stata_pal_mono <- c("#606060", "#a0a0a0", "#808080", "#404040", "#000000", "#e0e0e0", "#202020", "#707070", "#909090", "#b0b0b0", "#d0d0d0", "#f0f0f0", "#303030", "#c0c0c0", "#505050")
  
  # Wall Street Journal Color Palette
  wsj_pal_colors6 <- c("#c72e29", "#016392", "#be9c2e", "#098154", "#fb832d", "#000000")
  wsj_pal_rgby <- c("#d3ba68", "#d5695d", "#5d8ca8", "#65a479")
  wsj_pal_red_green <- c("#088158", "#ba2f2a")
  wsj_pal_black_green <- c("#000000", "#595959", "#59a77f", "#008856")
  wsj_pal_dem_rep <- c("#006a8e", "#b1283a", "#a8a6a7")
}

if (TRUE) {
  # Grey - Red - Green (3)
  self_pal_grg <- c('#a0a0a0', '#FF0000', '#00FF00')
  self_pal <- c("#E96479", "#F5E9CF", "#7DB9B6", "#223D6C", "#D20A13", "#FFD121", "#088247", "#58CDD9", "#7A142C", "#5D90BA", "#431A3D", "#91612D", 
                "#6E568C", "#E0367A", "#D8D155", "#64495D", "#7CC767")
}

pal_yr <- c('#FFFF00', '#EE0000')
pal_rgb <- c('#F8766D', '#7F7F7F', '#00BFC4')

default.colours.abb <- list(
  'default' = 'default',
  'AAAS (10)' = 'pal_aaas',
  'COSMIC Light (10)' = 'pal_cosmic_hallmarks_light',
  'COSMIC Dark (10)' = 'pal_cosmic_hallmarks_dark',
  'COSMIC Signature (6)' = 'pal_cosmic_signature_substitutions',
  'D3.js A (20)' = 'pal_d3_category20',
  'D3.js B (20)' = 'pal_d3_category20b',
  'D3.js C (20)' = 'pal_d3_category20c',
  'Flat UI A (10)' = 'pal_flatui_default',
  'Flat UI B (10)' = 'pal_flatui_flattastic',
  'Flat UI C (10)' = 'pal_flatui_aussie',
  'Frontiers (10)' = 'pal_frontiers_default',
  'Futurama (12)' = 'pal_futurama_planetexpress',
  'GSEA (12)' = 'pal_gsea_default',
  'IGV (51)' = 'pal_igv_default',
  'IGV B (2)' = 'pal_igv_alternating',
  'JAMA (7)' = 'pal_jama_default',
  'JCO (10)' = 'pal_jco_default',
  'Lancet (9)' = 'pal_lancet_lanonc',
  'LocusZoom (7)' = 'pal_locuszoom_default',
  'Material (12)' = 'pal_material_indigo',
  'NEJM (8)' = 'pal_nejm_default',
  'NPG (10)' = 'pal_npg_nrc',
  'Rick and Morty (12)' = 'pal_rickandmorty_schwifty',
  'Simpsons (16)' = 'pal_simpsons_springfield',
  'Star Trek (7)' = 'pal_startrek_uniform',
  'Tron Legacy (7)' = 'pal_tron_legacy',
  'uchicago default (9)' = 'pal_uchicago_default',
  'uchicago light (9)' = 'pal_uchicago_light',
  'uchicago dark (9)' = 'pal_uchicago_dark',
  'UCSC (26)' = 'pal_ucscgb_default'
)

default.colours <- list(
  'default' = 'default',
  'AAAS Journal Color Palettes (10)' = 'pal_aaas',
  'COSMIC Color Palettes - Light (10)' = 'pal_cosmic_hallmarks_light',
  'COSMIC Color Palettes - Dark (10)' = 'pal_cosmic_hallmarks_dark',
  'COSMIC Color Palettes - Signature (6)' = 'pal_cosmic_signature_substitutions',
  'D3.js Color Palettes - category20 (20)' = 'pal_d3_category20',
  'D3.js Color Palettes - category20b (20)' = 'pal_d3_category20b',
  'D3.js Color Palettes - category20c (20)' = 'pal_d3_category20c',
  'Flat UI Color Palettes - default (10)' = 'pal_flatui_default',
  'Flat UI Color Palettes - flattastic (10)' = 'pal_flatui_flattastic',
  'Flat UI Color Palettes - aussie (10)' = 'pal_flatui_aussie',
  'Frontiers Color Palettes (10)' = 'pal_frontiers_default',
  'The Futurama Color Palettes (12)' = 'pal_futurama_planetexpress',
  'The GSEA GenePattern Color Palettes (12)' = 'pal_gsea_default',
  'Integrative Genomics Viewer (IGV) Color Palettes - default (51)' = 'pal_igv_default',
  'Integrative Genomics Viewer (IGV) Color Palettes - alternating (2)' = 'pal_igv_alternating',
  'Journal of the American Medical Association Color Palettes (7)' = 'pal_jama_default',
  'Journal of Clinical Oncology Color Palettes (10)' = 'pal_jco_default',
  'Lancet Journal Color Palettes (9)' = 'pal_lancet_lanonc',
  'LocusZoom Color Palette (7)' = 'pal_locuszoom_default',
  'Material Design Color Palettes (12)' = 'pal_material_indigo',
  'NEJM Color Palettes (8)' = 'pal_nejm_default',
  'NPG Journal Color Palettes (10)' = 'pal_npg_nrc',
  'Rick and Morty Color Palettes (12)' = 'pal_rickandmorty_schwifty',
  'The Simpsons Color Palettes (16)' = 'pal_simpsons_springfield',
  'Star Trek Color Palettes (7)' = 'pal_startrek_uniform',
  'Tron Legacy Color Palettes (7)' = 'pal_tron_legacy',
  'The University of Chicago Color Palettes - default (9)' = 'pal_uchicago_default',
  'The University of Chicago Color Palettes - light (9)' = 'pal_uchicago_light',
  'The University of Chicago Color Palettes - uchicago dark (9)' = 'pal_uchicago_dark',
  'UCSC Genome Browser Color Palette (26)' = 'pal_ucscgb_default',
  'Calc Color Palette (12)' = 'calc_pal',
  'Colorblind Color Palette (8)' = 'colorblind_pal',
  'Economist Color Palette (9)' = 'economist_pal',
  'Color Palettes Few (7)' = 'few_pal',
  'Color Palettes Few - light (7)' = 'few_pal_light',
  'Color Palettes Few - dark (7)' = 'few_pal_dark',
  'FiveThirtyEight Color Palette (3)' = 'fivethirtyeight_pal',
  'Google Docs Color Palette (10)' = 'gdocs_pal',
  'Highcharts Color Palette (10)' = 'hc_pal',
  'Highcharts Color Palette - dark (10)' = 'hc_pal_dark_unica',
  'Paul Tol Color Palettes (12)' = 'ptol_pal',
  'Solarized Color Palette (8)' = 'solarized_pal',
  'Stata Color Palettes - s2color (15)' = 'stata_pal_s2color',
  'Stata Color Palettes - s1rcolor (15)' = 'stata_pal_s1rcolor',
  'Stata Color Palettes - s1color (15)' = 'stata_pal_s1color',
  'Stata Color Palettes - mono (15)' = 'stata_pal_mono',
  'Wall Street Journal Color Palette - colors6 (6)' = 'wsj_pal_colors6',
  'Wall Street Journal Color Palette - RGBY (4)' = 'wsj_pal_rgby',
  'Wall Street Journal Color Palette - RG (2)' = 'wsj_pal_red_green',
  'Wall Street Journal Color Palette - BG (4)' = 'wsj_pal_black_green',
  'Wall Street Journal Color Palette - dem (3)' = 'wsj_pal_dem_rep',
  'Grey - Red - Green (3)' = 'self_pal_grg',
  'albert.xn (17)' = 'self_pal',
  'Yellow - Red (2)' = 'pal_yr',
  'Red - Grey - Blue (3)' = 'pal_rgb'
)



a.palettes <- unlist(sapply(2:length(unlist(default.colours)), function(i) { get(unlist(default.colours)[i]) }))

# tableau_color_pal('regular')()
# unlist(palettes$`Tableau 10`)
# palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
# https://www.jianshu.com/p/4af3b009df20
# "#f98866" "#ff420e" "#80bd9e" "#89da59"
# canva_pal("Fresh and bright")(14)
# 
# ggthemes::canva_palettes()
# canva_palettes

# https://github.com/EmilHvitfeldt/r-color-palettes
