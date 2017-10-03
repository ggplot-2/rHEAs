library(ggplot2)
library(dplyr)
library(grid)
library(XML)
library(httr)
library(rvest)
library(magrittr)
library(stringr)
library(knitr)
# html' is deprecated. Use 'read_html' instead.
#element_data <- html("http://periodictable.com/Elements/001/data.html")
element_data <- read_html("http://periodictable.com/Elements/001/data.html")
extract raw links and place it in a dataframe together with the link text
elemental_properties <-
  data.frame(property = element_data %>%
               html_nodes(xpath = "//a") %>%
               html_text())
elemental_properties$links_raw <-
  element_data %>%
  html_nodes(xpath = "//a") %>%
  html_attr("href")
# drop any rows that do not begin "../../"
elemental_properties <-
  elemental_properties[
    grep(pattern = "^\\.\\./\\.\\./",
         x = elemental_properties$links_raw), ]
# remove the leading ../.. (relative link signifier)
elemental_properties$links_trail <-
  gsub(pattern = "^\\.\\./\\.\\./",
       replacement = "",
       x = elemental_properties$links_raw)
# keep only the rows that begin with "Properties"
elemental_properties <-
  elemental_properties[
    grep(pattern = "^Properties",
         x = elemental_properties$links_trail), ]
# make full urls
elemental_properties$url <-
  paste0("http://periodictable.com/",
         elemental_properties$links_trail)
# modify the URL so it sorts the table by atomic number
# (this is a feature of periodictable.com, they offer lists sortered in different ways
# for each property)
elemental_properties$url <-
  sub(pattern = "\\.html$",
      replacement = ".an.html",
      x = elemental_properties$url)
# we are done, go ahead and drop the no longer required columns
# links_raw, links_trail
elemental_properties <-
  elemental_properties[, -c(2,3)]
# oh, and add a sanitised version of the property names
elemental_properties$sanitized <-
  # replace spaces with underscores
  gsub("\\s+", "_",
       # replace "%" with "percent"
       gsub("%", "Percent",
            # remove parentheses and dashes
            gsub("[()-]", "",
                 # remove apostrophes
                 gsub("'", "", elemental_properties$property))))
# NOTE: Melting point and Boiling point are duplicated because they are displayed twice
#       on the data page for Hydrogen, both under "Overview" and "Thermal properties".
#       So we should deduplicate the dataframe.
elemental_properties <- unique(elemental_properties)
# reset the row numbering
row.names(elemental_properties) <- seq(1, dim(elemental_properties)[1])
print(elemental_properties$property)
# get the name of all the elements (two-step process)
element_names <-
  read_html(elemental_properties$url[1]) %>%
  html_nodes("table") %>%
  extract2(1) %>%
  html_nodes("table") %>%
  extract2(8) %>%
  html_nodes("td") %>%
  html_text()
element_names <-
  element_names[
    read_html(elemental_properties$url[1]) %>%
      html_nodes("table") %>%
      extract2(1) %>%
      html_nodes("table") %>%
      extract2(8) %>%
      html_nodes("td") %>%
      html_attr("align") == "left"
    ]
# create a properly oriented elements dataframe
elements_raw <-
  data.frame(matrix(data = "",
                    ncol = dim(elemental_properties)[1],
                    nrow = length(element_names),
                    byrow = TRUE))
names(elements_raw) <- elemental_properties$sanitized
elements_raw$Name <- element_names
skip_properties <-
  c("Ionization_Energies",
    "NFPA_Hazards",
    "NFPA_Label",
    "Names_of_Allotropes",
    "Discovery",
    "Crystal_Structure",
    "Lattice_Angles",
    "Lattice_Constants",
    "Known_Isotopes",
    "Stable_Isotopes",
    "Isotopic_Abundances")
# drop those rows from elemental_properties
elemental_properties <-
  elemental_properties[-which(elemental_properties$sanitized %in% skip_properties), ]
# drop those columns from elements_raw
elements_raw <-
  elements_raw[, -which(names(elements_raw) %in% skip_properties)]
# re-read only if rda file does not exist (saves time when compiling)
if (!file.exists("element-data-raw.rda")) {
  for (k in 2:length(elemental_properties$property)) {
    message(paste0("Reading property page (", k-1, " of ", length(elemental_properties$property)-1, "): ", elemental_properties$property[k]))
    property <-
      read_html(elemental_properties$url[k]) %>%
      html_nodes("table") %>%
      extract2(1) %>%
      html_nodes("table") %>%
      extract2(8) %>%
      html_nodes("td") %>%
      html_text()
    property <-
      property[
        read_html(elemental_properties$url[k]) %>%
          html_nodes("table") %>%
          extract2(1) %>%
          html_nodes("table") %>%
          extract2(8) %>%
          html_nodes("td") %>%
          html_attr("align") == "left"
        ]
    # for debugging purposes
    # print(paste(elemental_properties$property[k], ":", length(property)))
    # assign to elements_raw
    elements_raw[, k] <- property
  }
  # save this dataframe to file
  # (it is not large, but re-scraping the contents takes time)
  save(elements_raw, file = "element-data-raw.rda")
} else {
  load(file = "element-data-raw.rda")
}
creating new dataframes, templated on elements_raw
# notes is not used
values <- units <- notes <-
  data.frame(matrix(nrow = dim(elements_raw)[1],
                    ncol = dim(elements_raw)[2],
                    dimnames = list(seq(1, dim(elements_raw)[1]),
                                    names(elements_raw)),
                    byrow = TRUE))
# create a work-copy of elements_raw
elements_tmp <- elements_raw
non-numeric columns to copy requiring only minor cleanup
# col numbers: 1  2  8 17 39 43 44 45 48 49 50 51 52 53 54 55 59 73 77 78
# 73 (Space_Group_Name, may require some fixing of notation)
cols <-
  c("Name",
    "Symbol",
    "Phase",
    "Adiabatic_Index",
    "EU_Number",
    "RTECS_Number",
    "Alternate_Names",
    "Block",
    "Group",
    "Period",
    "Electron_Configuration",
    "Color",
    "Gas_phase",
    "CAS_Number",
    "CID_Number",
    "Gmelin_Number",
    "NSC_Number",
    "Electrical_Type",
    "Magnetic_Type",
    "Space_Group_Name",
    "Decay_Mode",
    "Quantum_Numbers")
for (k in 1:length(cols)) {
  values[, which(names(values) == cols[k])] <-
    gsub("^None$", "",
         gsub("^N/A$", "",
              gsub("\\[note\\]",
                   "",
                   elements_tmp[, which(names(elements_tmp) == cols[k])])
         )
    )
  # set empty strings to NA (proper NA, not the string "NA")
  values[which(values[, which(names(values) == cols[k])] == ""),
         which(names(values) == cols[k])] <- NA
}
# numbers only cols
cols <-
  c("Atomic_Number",
    "Atomic_Weight",
    "Molar_Volume",
    "Poisson_Ratio",
    "Refractive_Index",
    "Valence",
    "Electronegativity",
    "DOT_Hazard_Class",
    "DOT_Numbers",
    "NFPA_Fire_Rating",
    "NFPA_Health_Rating",
    "NFPA_Reactivity_Rating",
    "Superconducting_Point",
    "Mass_Magnetic_Susceptibility",
    "Molar_Magnetic_Susceptibility",
    "Volume_Magnetic_Susceptibility",
    "Space_Group_Number",
    "Neutron_Cross_Section",
    "Neutron_Mass_Absorption")
for (k in 1:length(cols)) {
  values[, which(names(values) == cols[k])] <-
    as.numeric(gsub("×10", "E",
                    gsub("\\[note\\]",
                         "",
                         elements_tmp[, which(names(elements_tmp) == cols[k])])
    )
    )
}
# numbers and units cols
cols <-
  c("Density",
    "Melting_Point",
    "Boiling_Point",
    "Absolute_Melting_Point",
    "Absolute_Boiling_Point",
    "Critical_Pressure", # contains converted values in parentheses
    "Critical_Temperature",
    "Heat_of_Fusion",
    "Heat_of_Vaporization",
    "Heat_of_Combustion",
    "Specific_Heat", # complex unit J/(Kg K)
    "Neel_Point",
    "Thermal_Conductivity", # complex unit W/(m K)
    "Thermal_Expansion",
    "Density_Liquid",
    "Brinell_Hardness",
    "Mohs_Hardness",
    "Vickers_Hardness",
    "Bulk_Modulus",
    "Shear_Modulus",
    "Young_Modulus",
    "Speed_of_Sound",
    "ElectronAffinity",
    "Autoignition_Point",
    "Flashpoint",
    "Electrical_Conductivity",
    "Resistivity",
    "Curie_Point",
    "Percent_in_Universe",
    "Percent_in_Sun", # makes use of "None" in lieu of zero
    "Percent_in_Meteorites", # makes use of "None" in lieu of zero
    "Percent_in_Earths_Crust", # makes use of "None" in lieu of zero
    "Percent_in_Oceans", # makes use of "None" in lieu of zero
    "Percent_in_Humans", # makes use of "None" in lieu of zero
    "Atomic_Radius",
    "Covalent_Radius",
    "Van_der_Waals_Radius",
    "HalfLife", # mix between "Stable" and num + unit
    "Lifetime") # mix between "Stable" and num + unit
for (k in 1:length(cols)) {
  for (i in 1:length(elements_tmp[, which(names(elements_tmp) %in% cols[k])])) {
    # replace the string "None" with actual zero
    elements_tmp[, which(names(elements_tmp) %in% cols)][i, k] <-
      ifelse(elements_tmp[, which(names(elements_tmp) %in% cols)][i, k] == "None",
             "0",
             elements_tmp[, which(names(elements_tmp) %in% cols)][i, k])
    # replace the string "Stable" with "Inf"
    elements_tmp[, which(names(elements_tmp) %in% cols)][i, k] <-
      ifelse(elements_tmp[, which(names(elements_tmp) %in% cols)][i, k] == "Stable",
             "Inf",
             elements_tmp[, which(names(elements_tmp) %in% cols)][i, k])
    # look for numbers (also for "Inf")
    mtch <-
      regexpr(pattern = "Inf|[-×\\.0-9]+",
              text = elements_tmp[, which(names(elements_tmp) %in% cols)][i, k])
    # assign numeric value to "values"
    values[i, which(names(values) == cols[k])] <-
      as.numeric(
        sub(
          pattern = "×10",
          replacement = "E",
          x = substr(x = elements_tmp[, which(names(elements_tmp) %in% cols)][i, k],
                     start = 1,
                     stop = attr(mtch, "match.length"))))
    # assign unit part to "units" by eliminating the numeric part
    units[i, which(names(values) == cols[k])] <-
      # remove leading or trailing spaces
      sub("^\\s+", "", sub("\\s+$", "",
                           # remove the numeric part
                           sub("Inf|[-×\\.0-9]+", "",
                               # remove any numbers+units within parentheses
                               sub("\\([.0-9]+\\s[A-Za-z]+\\)", "",
                                   # remove notes
                                   sub("\\[note\\]", "",
                                       # remove "N/A" strings
                                       sub("N/A",
                                           "",
                                           elements_tmp[, which(
                                             names(elements_tmp) %in% cols)][i, k]
                                       )
                                   )
                               )
                           )
      )
      )
  }
  # set empty strings to NA (proper NA, not the string "NA")
  values[which(values[, which(names(values) == cols[k])] == ""),
         which(names(values) == cols[k])] <- NA
}
tail(sapply(units, unique), 10)
cat(paste(sort(unique(unlist(sapply(units, unique)))), collapse = "\n"))
# Build up a match-and-convert dataframe based on the list above
pcf <-
  # find any values with these units...
  data.frame(pattern = c("GPa",
                         "KJ/mol",
                         "MPa",
                         "d",
                         "g/cc",
                         "g/cm3",
                         "g/l",
                         "h",
                         "ms",
                         "pm",
                         "y",
                         "°C"),
             # ... and convert them into these units
             convert = c("Pa",
                         "kJ/mol",
                         "Pa",
                         "s",
                         "kg/m^3",
                         "kg/m^3",
                         "kg/m^3",
                         "s",
                         "s",
                         "m",
                         "s",
                         "K"),
             # using these conversion factors
             factor = c("1E9",
                        "1",
                        "1E6",
                        "86400",
                        "1E3",
                        "1E3",
                        "1",
                        "3600",
                        "1E-3",
                        "1E-12",
                        "3.154E7",
                        "+273.15"))
# Here we replace the values and the units according to pcf
for (k in 1:dim(units)[2]) {
  # if the entire column is NA, move to the next one
  if (all(is.na(units[, k]))) {next}
  for (i in 1:dim(units)[1]) {
    # jump to the next cell immediately if unit is empty
    if (units[i, k] == "") {next}
    # for each cell, compare the unit to pcf$pattern,
    # and if they match, replace it with pcf$convert
    # and apply the conversion factor on the value
    # match() returns the position of the match
    # in pcf$pattern, or NA if no match was found
    hit <- match(units[i, k], pcf$pattern)
    if (!is.na(hit)) {
      # go ahead and replace unit and convert value
      units[i, k] <- pcf$convert[hit]
      # had to find a way to handle the odd addition operation,
      # opted to do it with a string operation and this if-else
      if (substr(pcf$factor[hit], 1, 1) %in% c("+", "-")) {
        if (substr(pcf$factor[hit], 1, 1) == "-") {
          values[i, k] <-
            values[i, k] + as.numeric(pcf$factor[hit])
        } else {
          values[i, k] <-
            values[i, k] + as.numeric(sub("^\\+", "", pcf$factor[hit]))
        }
      } else {
        # not addition/subtraction operation
        values[i, k] <-
          values[i, k] * as.numeric(pcf$factor[hit])
      }
    }
  }
}

# rearrange values dataframe by atomic number
# simply to make assignments based on atomic number (below) possible
values <- arrange(values, Atomic_Number)
values$Graph.Period <- values$Period
units$Graph.Period <- "" # to maintain same dims as values
values$Graph.Group <- values$Group
units$Graph.Group <- "" # to maintain same dims as values
# lanthanoids 57-71: Period = 8, Group = seq(3, 17)
values$Graph.Period[seq(57,71)] <- 8.5
values$Graph.Group[seq(57,71)] <- seq(3, 17)
# actinoids 89-103: Period = 9, Group = seq(3, 17)
# increase period to increase the gap up to the transition block
values$Graph.Period[seq(89,103)] <- 9.5
values$Graph.Group[seq(89,103)] <- seq(3, 17)
# make graphical Group and Period numeric
values$Graph.Period <- as.numeric(values$Graph.Period)
values$Graph.Group <- as.numeric(values$Graph.Group)
elementFre <- read.csv("./Data/elementFre.csv")
elementFre <- elementFre %>%
  select(2:3)
values <- values %>%
  left_join(elementFre, by = "Symbol")
ggplot() +
  scale_y_reverse() +
  geom_point(data = values,
             size = 14,
             shape = 15,
             aes(y = Graph.Period,
                 x = Graph.Group,
                 color = Fre)) +
  geom_text(data = values,
            aes(label = Symbol,
                y = Graph.Period,
                x = Graph.Group),
            # color = "cyan",
            color = "white")  +  
  # set breaks and labels in the colourbar legend
  # scale_colour_continuous(breaks = c(0, 0.05, 0.10, 0.15, 0.20),
  #                         labels = c(0, 0.05, 0.10, 0.15, 0.20),
  #                         # range of colour
  #                         low = "blue", high = "red",
  #                         # colour if value is NA
  #                         na.value = "grey70") 
  scale_colour_gradientn(limits = c(0, 0.15),
                         na.value = "grey70",
                         colours = c("blue", "#9933FA", "cyan", "#7CFC00", "#FFD700", "#FF6103", "red"),
                         # values = c(0, 0.5, 1),
                         breaks = c(0, 0.05, 0.10, 0.15),
                         labels = c(0, 5, 10, 15))  +
  # plot title (usually property and unit)
  annotate("text", x = 8, y = 0.6,
           vjust = 0.5,
           label = 'bold("Usage frequency/%")',
           parse = TRUE,
           size = 5) +
  # set the size of the legend boxes independent of geom_point's aes
  # guides(colour = guide_legend(override.aes = list(size = 5),
  #                              title = "Usage frequency",
  #                              nrow = 2,
  #                              title.hjust = 0.5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(0, 0, -0.85, -0.85), "line"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        # center (roughly) over transition metal block
        legend.position = c(0.42, 0.92),
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal",
        # make the legend colourbar a little longer
        legend.key.width = unit(2.5, "line"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent"))
ggsave("./Figures/Element/elementFre_periodicTable.pdf",
       width = 7.2625,
       height = 5.75,
       dpi = 600)
ggsave("./Figures/Element/elementFre_periodicTable.tiff",
       width = 7.2625,
       height = 4)


