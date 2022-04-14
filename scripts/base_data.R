#### SETUP #### ----------------------------------------------------------------
### LOAD PACKAGES --------------------------------------------------------------
library(vitae)
library(rorcid)
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(sfnetworks)
library(ggrepel)
library(emojifont)
library(fontawesome)
library(kableExtra)
library(lubridate)
library(huxtable)
library(gridExtra)
library(rsvg)
library(htmltools)
library(tibble)
library(stringr)
library(here)
library(xtable)
library(rlang)
#### HELPER FUNCTIONS ----------------------------------------------------------
## Bold text in vitae entries
embolden <- function(x) {
    glue::glue("\\textbf{<x>}",
               .open = "<",
               .close = ">")
}
## Filter to most recent years
## N.B. Useful for resume more-so than CV
filter_latest <- function(x, col, n = 5) {
  x |>
    dplyr::filter(!! rlang::sym(col) >= lubridate::year(Sys.Date()) - n)
}
## Order by dates
order_by_date <- function(x) {

  if ("DATE_START" %in% colnames(x)) {
  x |>
    ## Add date range
    dplyr::mutate(
      DATES = ifelse(!is.na(DATE_END),
                     paste0(DATE_START, " - ", DATE_END),
                     paste0(DATE_START, " - present")),
      YEAR_START = as.numeric(substr(DATE_START, 1, 4)),
      YEAR_END = ifelse(is.na(DATE_END),
                        lubridate::year(Sys.Date()),
                        as.numeric(substr(DATE_END, 1, 4))),
      DATE_START_SORT = lubridate::as_date(paste0(DATE_START, "-01"),
                                           format = "%Y-%m-%d"),
      DATE_END_SORT = ifelse(!is.na(DATE_END),
                           lubridate::as_date(paste0(DATE_END, "-01"),
                                              format = "%Y-%m-%d"),
                           lubridate::today())
    ) |>
    dplyr::arrange(desc(DATE_END_SORT), desc(DATE_START_SORT))
  } else if ("DATE" %in% colnames(x)) {
    x |>
      dplyr::mutate(DATE_SORT = lubridate::as_date(paste0(DATE, "-01"),
                                                   format = "%Y-%m-%d")) |>
      dplyr::arrange(desc(DATE_SORT))
  } else {
    print("No recognised date column.")
  }
}
## Formatted tabular data
## N.B. Cut -- but interesting. Borrowed from `vitae` creator examples via GitHub.
# baretable <- function(tbl, digits = 0,
#                       include.colnames = FALSE, include.rownames = FALSE,
#                       hline.after = NULL,
#                       size = getOption("xtable.size", NULL),
#                       add.to.row = getOption("xtable.add.to.row", NULL),
#                       longtable = FALSE,
#                       ...) {
#   xtable::xtable(tbl, digits = digits, ...) %>%
#     print(
#       include.colnames = include.colnames,
#       include.rownames = include.rownames,
#       hline.after = hline.after,
#       comment = FALSE,
#       tabular.environment = if_else(longtable, "longtable", "tabular"),
#       floating = FALSE,
#       size = size,
#       add.to.row = add.to.row,
#       sanitize.text.function = function(x) {
#         x
#       }
#     )
# }
## -----------------------------------------------------------------------------
#### DATA ----------------------------------------------------------------------
### EDUCATION ------------------------------------------------------------------
EDUCATION <-
  ## Raw data
  tibble::tribble(
    ~ INSTITUTION, ~ DATE_START, ~ DATE_END, ~ LOCATION, ~ DEGREE, ~ THESIS,
    "University of Tasmania", 2019, NA, "Sandy Bay, TAS, Australia", "Doctor of Philosophy, Biological Sciences", c("Dissertation: Dynamic mapping and analysis of New South Wales fire regimes: Past, present, and future",
                                                                                                                    "Supervisors: Drs. Grant J. Williamson and David M.J.S. Bowman"),
    "Western Washington University", 2013, 2016, "Bellingham, WA, USA", "Master of Science, Geography", c("Thesis: Climatic drivers of western spruce budworm outbreaks in the Okanogan Highlands",
                                                                                                          "Supervisor: Dr. Aquila Flower"),
    "Texas State University", 2007, 2012, "San Marcos, TX, USA", "Bachelor of Science, Physical Geography / Geology", "Capstone: Wildfire hazard data for the Yellowstone National Park Ecosystem: 1987 - 1988"
  ) |>
  ## Add date range
  dplyr::mutate(
    DATES = ifelse(!is.na(DATE_END),
                   paste0(DATE_START, " - ", DATE_END),
                   paste0(DATE_START, " - present"))
  )
## -----------------------------------------------------------------------------
### PROFESSIONAL EXPERIENCE ----------------------------------------------------
PROFESSIONAL_EXPERIENCE <-
  tibble::tribble(
    ~ INSTITUTION, ~ DATE_START, ~ DATE_END, ~ LOCATION, ~ POSITION, ~ DEPARTMENT, ~ TYPE, ~ DESCRIPTION,
    "University of Tasmania", "2021-02", NA, "Sandy Bay, TAS, Australia", "Data Analyst", "Division of Future Students", "part-time / full-time", c(
      "Developing statistical models and tools for meeting target university milestones",
      "Managing design and implementation of broad-scale university ML models for directing future university marketing, funding, and course creation",
      "Designing and implementing programming and data analytics training seminars for institution staff",
      "Overseeing development of internal reporting and statistical packages",
      "Processing, analysing and reporting data to promote and review university programs"
    ),
    "Self-Employed", "2018-07", "2020-09", NA, "Independent Research Contractor", NA, "contract", c(
      "Developed large-scale data-processing methodologies for resource and hazard management",
      "Produced interactive, dynamic dashboard products for exploratory data analysis and modelling"
      # "Notable contracts incl. U.S. Forest Service (2018-08, 2019-06) and Frontier Development Lab (2020-08)"
    ),
    "University of Tasmania", "2019-09", NA, "Sandy Bay, TAS, Australia", "Adjunct Researcher", "College of Sciences and Engineering", "full-time", c(
      "Developing complex data-processing and analysis pipelines using high-performance computing and big data",
      "Overseeing multi-year projct design and implementation addressing wildfire and global climate change challenges",
      "Managing custom platform for interactive web-mapping, on-the-fly statistical analyses, and novel data visualisation",
      "Collaborating with national and international research teams tackling climate change risk and mitigation"
    ),
    "University of Tasmania", "2019-10", "2021-02", "Sandy Bay, TAS, Australia", "Sustainability Analyst", "Division of Future Students", "casual", c(
      "Processed, analysed, and reported data to promote and review institution programmes",
      "Developed pipelines for automating data cleaning and collation",
      "Prepared data and briefings for global institution rankings",
      "Promoted nation-wide sustainability targets using data dashboards and reports"
    ),
    "Apex Systems", "2018-12", "2019-05", "Austin, TX, USA", "Data \\& GIS Analyst", "Apple, Inc.", "contract", c(
      "Developed and standardised data-processing methods for vector and multispectral raster datasets",
      "Managed and processed corporate GIS databases using data-driven software and programming languages",
      "Prepared statistical summaries, models, and analytical reports",
      "Led research and development projects to define novel methodologies for geospatial data",
      "Collaboratd with a global network of analysts and technicians"
    ),
    "Oregon State University", "2017-11", "2018-06", "Wenatchee, WA, USA", "Faculty Research Assistant", "USDA Forest Service Forestry Sciences Laboratory", "contract", c(
      "Converted complex data and findings into academic and government reports",
      "Developed explanatory models for sustainable forest management under climate change",
      "Created and maintained relational databases for government research",
      "Gathered and analysed ecological data using statistical software"
    ),
    "Washington Department of Fish and Wildlife", "2017-08", "2017-11", "Ridgefield, WA, USA", "Biological Science Technician II", "Region 5", "contract", c(
      "Applied QA/QC to develop electronic biological data collection system and relational database",
      "Analysed biological fisheries data for state government research reporting",
      "Collected and recorded biological data for government research and both commercial and tribal fisheries",
      "Designed, built, and maintained department, fisheries, and commercial research equipment",
      "Conducted outreach to the public, providing information on fisheries research, management principles, and local regulations",
      "Monitord critical fish migrations using DNA sampling and the PTAGIS coordination project",
      "Received training in fish, plant, and organism identification, government safety procedures, boat operation, and defensive driving"
    ),
    "Kelso School District", "2017-01", "2017-08", "Kelso, WA, USA", "Substitute Teacher", "Beacon Hill Elementary School", "casual", c(
      "Taught a variety of classrooms, with a focus on K-5 primary education, special education, and LAP classrooms using specialised learning plans",
      "Engaged young students with an interest in scientific research, giving special outreach presentations",
      "Helped integrate and refine special education lesson plans as an instructional assistant"
    ),
    "Western Washington University", "2013-08", "2016-06", "Bellingham, WA, USA", "Graduate Teaching Assistant", "Huxley College of the Environment", "full-time", c(
      "Conceptualised, developed, and published novel dendroecological research", ## RA
      "Presented research findings at national academic conferences", ## RA
      "Coordinated and taught laboratory classrooms for analytical GIS, remote sensing, and research methodology", ## TA
      "Trained and led teams of students in the safe use of field and laboratory equipment in inclement weather and rough terrain", ## TA/RA
      "Scripted statistical methods for identifying complex ecological and climatological relationships", ## RA
      "Presided over and maintained the Huxley Tree Ring Laboratory", ## RA
      # "Led overnight field exercises in inclement weather and rough terrain", ## TA
      "Coordinated international travel and classrooms for students and participating agencies",
      # "Created and maintained university GIS geodatabses and topologies", ## TA/RA
      "Edited and performed technical reviews of student research papers at the developmental, substantive, and line level", ## TA
      "Classes taught: Remote Sensing (ESCI 442), Introduction to GIS (ENVS 320), Computer Cartography (ENVS 321), Research and Writing (ENVS 319), Environmental Data and Information (ENVS 201), Environmental History and Ethics (ENVS 305), Physical Geography (ENVS 203), the Soil Environment and Landscapes (ENVS 427), Water Resources (ENVS 427), Agroecology and Sustainable Agriculture (ENVS 410), Food Cultures of Italy (ENVS 497), and Ecogastronomy (ENVS 110)" ## TA
    ),
    "The University of Texas at Austin", "2013-04", "2013-08", "Austin, TX, USA", "Research Engineer", "Texas Natural Science Center", "contract", c(
      "Compiled, georeferences, and managed academic collections and geospatial databaes",
      "Contributed historial species range data to statewide virtual biodiversity museum",
      "Reconstructed historical biodata collections and events"
    ),
    "Student Conservation Association", "2013-01", "2013-04", "Angeles National Forest, CA, USA", "Trail Management Intern", "USDA Forest Service, Reigon 5", "internship", c(
      "Co-managed Trails Assessment and Conditions Survey (TrACS) team",
      "Conducted detailed survey of fire damage to public roads and trail systems",
      "Assisted in infrastructure restoration projects to maximise public access benefits",
      "Received national government-sponsored training in defensive driving"
    ),
    "The University of Texas at Austin", "2012-01", "2013-01", "Austin, TX, USA", "Laboratory Assistant", "Texas Natual Science Center", "internship", c(
      "Catalogued, conserved, photographed, and archived acadmic fish and fossils collections",
      "Maintained and updated relational database management system for institution collections",
      "Trained volunteers and students in laboratory safety and inventory procedures"
    ),
    "Texas State University", "2011-03", "2012-12", "San Marcos, TX, USA", "Laboratory Assistant", "Department of Geography", "internship", c(
      "Established geology and palaeontology laboratories for the university",
      "Prepared vertebrate fossils for laboratory use and public display",
      "Trained volunteers in laboratory procedures and sample preparation techniques",
      "Organised and led field exercises for sample collections and cleanings",
      "Captured and processed ichnological and palaeontological data using Structure from Motion",
      "Curated departmental fossils and rock collections"
    )
  ) |>
  order_by_date()
## -----------------------------------------------------------------------------
### TEACHING EXPERIENCE ----------------------------------------------------
TEACHING_EXPERIENCE <-
  tibble::tribble(
    ~ INSTITUTION, ~ DATE_START, ~ DATE_END, ~ LOCATION, ~ POSITION, ~ DEPARTMENT, ~ TYPE, ~ DESCRIPTION,
    "University of Tasmania", "2021-02", NA, "Sandy Bay, TAS, Australia", "Data Analyst", "Division of Future Students", "part-time / full-time", c(
      "Designing and implementing programming and data analytics training seminars for institution staff",
      "Overseeing and contributing weekly learning and showcasing workshops"
    ),
    "Kelso School District", "2017-01", "2017-08", "Kelso, WA, USA", "Substitute Teacher", "Beacon Hill Elementary School", "casual", c(
      "Taught a variety of classrooms, with a focus on K-5 primary education, special education, and LAP classrooms using specialised learning plans",
      "Engaged young students with an interest in scientific research, giving special outreach presentations",
      "Helped integrate and refine special education lesson plans as an instructional assistant"
    ),
    "Western Washington University", "2013-08", "2016-06", "Bellingham, WA, USA", "Graduate Teaching Assistant", "Huxley College of the Environment", "full-time", c(
      "Coordinated and taught laboratory classrooms for analytical GIS, remote sensing, and research methodology", ## TA
      "Trained and led teams of students in the safe use of field and laboratory equipment in inclement weather and rough terrain", ## TA/RA
      # "Led overnight field exercises in inclement weather and rough terrain", ## TA
      "Coordinated international travel and classrooms for students and participating agencies",
      # "Created and maintained university GIS geodatabses and topologies", ## TA/RA
      "Edited and performed technical reviews of student research papers at the developmental, substantive, and line level", ## TA
      "Classes taught: Remote Sensing (ESCI 442), Introduction to GIS (ENVS 320), Computer Cartography (ENVS 321), Research and Writing (ENVS 319), Environmental Data and Information (ENVS 201), Environmental History and Ethics (ENVS 305), Physical Geography (ENVS 203), the Soil Environment and Landscapes (ENVS 427), Water Resources (ENVS 427), Agroecology and Sustainable Agriculture (ENVS 410), Food Cultures of Italy (ENVS 497), and Ecogastronomy (ENVS 110)" ## TA
    )
  ) |>
  order_by_date()

## VOLUNTEER EXPERIENCE -----
VOLUNTEER_EXPERIENCE <-
  tibble::tribble(
    ~ POSITION, ~ DATE_START, ~ DATE_END, ~ INSTITUTION, ~ LOCATION,
    "Foster Parent", "2020-05", NA, "Ten Lives Cat Centre", "Hobart, TAS, Australia",
    "Animal Handler", "2018-08", "2019-01", "Humane Society of the New Braunfels Area", "New Braunfels, TX, USA",
    "Technology Tutor", "2018-01", "2018-08", "North Central Regional Library", "Wenatchee, WA, USA",
    "Animal Handler", "2018-03", "2018-08", "Wenatchee Valley Humane Society", "Wenatchee, WA, USA",
    "Geographic Information Systems Technician", "2017-01", "2017-11", "Portland State University", "Portland, OR, USA",
    "Assistant Librarian", "2017-01", "2017-11", "Beacon Hill Elementary School", "Kelso, WA, USA",
    "Technology Tutor", "2014-07", "2016-08", "Bellingham Public Library", "Bellingham, WA, USA",
    "Laboratory Assistant", "2011-03", "2012-12", "Texas State University", "San Marcos, TX, USA"
  ) |>
  order_by_date()

CONSULTING_EXPERIENCE <-
  tibble::tribble(
    ~ DATE, ~ AGENCY, ~ PROJECT, ~ AMOUNT,
    "2020-08", "Frontier Development Lab", "Predicting fire behaviour using historical satellite imagery", "AUD\\$1,000",
    "2019-06", "USDA Forest Service", "Post-fire logging effects on reburn severity", "USD\\$2,500",
    "2018-08", "USDA Forest Service", "Restoration treatment effects on ponderosa pine growth sensitivity to climate", "USD\\$2,500"
  ) |>
  order_by_date()


HONOURS <-
  tibble::tribble(
    ~ DATE, ~ AGENCY, ~ HONOUR, ~ AMOUNT, ~ TYPE,
    "2021-10", "Australian Museum", "NSW Environment, Energy and Science (DPIE) Eureka Prize for Applied Environmental Research", "AUD\\$10,000", "award",
    "2021-09", "University of Tasmania", "Sustainability Award", NA_character_, "award",
    "2021-08", "Ten Lives Cat Centre", "Certifi-Cat of Appreciation", NA_character_, "award",
    "2020-08", "Frontier Development Lab", "Bushfire Data Quest 2020", "AUD\\$1,000", "grant",
    "2019-09", "NSW Bushfire Risk Management Research Hub", "Tasmania Graduate Research Scholarship (3.5x)", "AUD\\$28,000 p.a.", "scholarship",
    "2019-09", "NSW Bushfire Risk Management Research Hub", "NSW Bushfire Risk Management Research Hub Top-Up (3.5x)", "AUD\\$5,000 p.a.", "scholarship",
    "2018-06", "Washington Department of Fish and Wildlife", "Best Fish and Wildlife Science", NA_character_, "award",
    "2017-10","Washington Department of Fish and Wildlife", "Conservation Award", NA_character_, "award",
    "2016-04", "Western Washington University", "Dean and Sandy Blinn Travel and Research Fund", "USD\\$450", "scholarship",
    "2016-04", "Western Washington University", "Dean's Fund for Sustainability Studies", "USD\\$500", "scholarship",
    "2014-07", "North American Dendroecological Fieldweek", "North American Dendroecological Fieldweek research fellowship", "USD\\$3,000", "scholarship",
    "2014-05", "Association of Washington Geographers", "Award for Outstanding Poster Presentation by a Graduate Student", "USD\\$125", "award"
  ) |>
  order_by_date()

OUTREACH <-
  tibble::tribble(
    ~ DATE, ~ TYPE, ~ TITLE, ~ EVENT, ~ LOCATION, ~ AGENCY, ~ URL, ~ URL_TYPE,
    "2021-10-12", "press", "Bushfire research team awarded prestigious Eureka Prize", NA_character_, NA_character_, "University of Tasmania", "https://www.utas.edu.au/communications/general-news/all-news/bushfire-research-team-awarded-prestigious-eureka-prize", "article",
    "2021-10", "professional", "Tasmanian Analytics Project", "University of Tasmania and Department of Education Lunch \\& Learn", "Sandy Bay, TAS, Australia", "Department of Education", NA_character_, NA_character_,
    "2020-07", "coursework", "Dynamic mapping and analysis of New South Wales fire regimes: Past, present and future", "University of Tasmania Graduate Seminars", "Sandy Bay, TAS, Australia", "University of Tasmania", NA_character_, NA_character_,
    "2020-03", "academic", "Changes in moisture availability drive fire risk", "Bushfire Risk Management Research Hub Researchers' Meeting 2020", "Wollongong, NSW, Australia", "Bushfire Risk Management Research Hub", "https://toddmellis.files.wordpress.com/2020/03/firehub_meet_20200302.pdf", "presentation",
    "2016-06", "coursework", "Climatic Drivers of western spruce budworm outbreaks in the Okanogan Highlands", "Western Washington University Graduate Defense", "Bellingham, WA, USA", "Western Washington University", "https://toddmellis.files.wordpress.com/2016/10/ellis_thesis_defense.pdf", "presentation",
    "2016-04", "conference", "Controlling factors of spruce budworm outbreaks in the Okanogan Highlands", "American Association of Geographers Annual Meeting 2016", "San Francisco, CA, USA", "American Association of Geographers", "https://toddmellis.files.wordpress.com/2016/10/ellis-2016_aag_pres.pdf", "presentation",
    "2014-07", "academic", "Climate responses of \\textit{Pseudotsuga menziesii} and \\textit{Pinus flexilis} in the Greater Yellowstone (USA)", "North American Dendroecological Fieldweek 2014", "Cody, WY, USA", "North American Dendroecological Fieldweek", c("https://toddmellis.files.wordpress.com/2016/10/nadef2014_climresponsepres.pdf", "https://toddmellis.files.wordpress.com/2016/10/nadef2014_climresponse.pdf"), c("presentation", "paper"),
    "2014-05", "coursework", "Controlling factors of spruce budworm outbreaks in the Okanogan Highlands", "Western Washington University Graduate Seminars", "Bellingham, WA, USA", "Western Washington University", NA_character_, NA_character_,
    "2014-05", "coursework", "Mapping alpine treeline: A comparative analysis of Structure-from-Motion and LiDAR techniques using digital aerial imagery", "Western Washington University EGEO 504: Geographic Methods \\& Techniques", "Bellingham, WA, USA", "Western Washington University", c("https://toddmellis.files.wordpress.com/2016/10/ellis_2014_mapalpinetreepres.pdf", "https://toddmellis.files.wordpress.com/2016/10/ellis_2014_mapalpinetree.pdf"), c("presentation", "paper"),
    "2014-04", "conference", "Perceptions of risk in Bellingham, Washington", "Association of Washington Geographers Spring Meeting 2014", "Seattle, WA, USA", "Association of Washington Geographers", NA_character_, NA_character_,
    "2013-12", "coursework", "Biogeographic implications of Structure-from-Motion imagery", "Western Washington University EGEO 501: History and Philosophy of Geography", "Bellingham, WA, USA", "Western Washington University", "https://toddmellis.files.wordpress.com/2016/10/ellis_2013_sfm.pdf", "paper",
    "2012-05", "coursework", "Wildfire hazard data for the Yellowstone National Park Ecosystem: 1987-1988", "Texas State University GEO 4412: Digital Remote Sensing", "San Marcos, TX, USA", "Texas State University", c("https://toddmellis.files.wordpress.com/2016/10/ellis_2012_gyehazardpres.pdf", "https://toddmellis.files.wordpress.com/2016/10/ellis_2012_gyehazard.pdf"), c("presentation", "paper"),
    "2012-04", "coursework", "Cretaceous-aged fossiliferous outcrops of the Big Bend region", "Texas State University GEO 4310: Regional Field Studies", "San Marcos, TX, USA", "Texas State University", "https://toddmellis.files.wordpress.com/2017/11/ellis-20120301-cretaceous-aged-fossiliferous-outcrops-of-the-big-bend-region.pdf", "paper",
    "2011-12", "coursework", "Predator-prey relations in the Greater Yellowstone Ecosystem", "Texas State University GEO 4316: Landscape Biogeography", "San Marcos, TX, USA", "Texas State University", c("https://toddmellis.files.wordpress.com/2016/10/ellis-and-mcdonnell-2011-predator-prey-relations-in-the-gye-presentation.pdf", "https://toddmellis.files.wordpress.com/2016/10/ellis-and-mcdonnell-2011-predator-prey-relations-in-the-gye.pdf"), c("presentation", "paper")
  ) |>
  order_by_date()

BIBLIO_R <-
  tibble::tribble(
    ~ TITLE, ~ DATE_START, ~ DATE_END, ~ ROLE, ~ DESCRIPTION, ~ URL, ~ TYPE, ~ SRC,
    "miao", "2021-08", "2022-02", "Lead Developer", "Custom exploratory data analysis and statistical tools", "https://github.com/toddellis/miao", "public", "GitHub",
    "utaspptx", "2021-11", "2022-01", "Lead Developer", "Simple methods for producing themed reports and presentations in Microsoft Office software", "https://github.com/utas-analytics/utaspptx", "private", "GitHub",
    "utastoolkit", "2021-05", "2022-03", "Lead Developer", "Tools for accessing and reporting internal University data from the Enterprise Data Warehouse", "https://github.com/utas-analytics/utastoolkit", "private", "GitHub"
  ) |>
  order_by_date()

# rorcid::orcid_auth()
## Add identifier to the R environment
# usethis::edit_r_environ()

BIBLIOGRAPHY <-
  rorcid::works("0000-0002-4410-8676") |>
  tibble::as_tibble() |>
  tidyr::unnest(`external-ids.external-id`)  |>
  dplyr::filter(`external-id-type` == 'doi') |>
  tidyr::replace_na(list(`publication-date.day.value` = "01")) |>
  dplyr::transmute(TYPE = type,
                   TITLE = `title.title.value`,
                   JOURNAL = ifelse(!is.na(`journal-title.value`),
                                    `journal-title.value`,
                                    stringr::str_extract(`url.value`, "(figshare)")),
                   URL = `url.value`,
                   DATE = paste0(`publication-date.year.value`, "-",
                                 `publication-date.month.value`, "-",
                                 `publication-date.day.value`) |>
                     lubridate::as_date(),
                   ## 6th edition APA DOI
                   DOI = paste0("doi:", `external-id-value`),
                   ## 7th edition APA DOI
                   DOI_URL = ifelse(!is.na(`external-id-value`),
                                    paste0("https://doi.org/", `external-id-value`),
                                    NA_real_),
                   .SRC = `source.source-name.value`)
