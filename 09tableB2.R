# Reproduce Table B2 from the metadata reported in the revised manuscript.
# The published dimensions, OOB scores and exclusions describe the original run.

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(this.path, knitr)

path_code <- this.dir()
out_dir <- file.path(path_code, "output")
dir.create(out_dir, showWarnings = FALSE)

# Variable counts include the outcome. NA marks unavailable information.
table_b2 <- read.delim(
  text = "Task name	Task ID	n orig.	p orig.	n after QC	p after QC	Variable types	R² of RF	Used	Reason, if not used
diamonds	233211	53940	10	53940	10	num., cat.	0.895	yes	
us_crime	359945	1994	128	123	128	num.	0.688	yes	
space_ga	359933	3107	7	3106	7	num.	0.551	yes	
wine_quality	359935	6497	12	6497	12	num.	0.298	no	RF out-of-bag R² < 0.3
house_16H	359952	22784	17	22307	17	num.	0.457	yes	
yprop_4_1	359940	8885	252	8884	213	num.	0.053	no	RF out-of-bag R² < 0.3
sensory	359931	576	12	576	12	cat.	0.109	no	RF out-of-bag R² < 0.3
tecator	359934	240	125	240	125	num.	0.900	yes	
SAT11-HAND-runtime-regression	359948	4440	118	1725	116	num., cat.	0.544	yes	
abalone	359944	4177	9	4170	9	num., cat.	0.467	yes	
MIP-2016-regression	360945	1090	145	872	119	num., cat.	0.030	no	RF out-of-bag R² < 0.3
socmob	359932	1156	6	1087	6	num., cat.	0.680	yes	
quake	359930	2178	4	2178	4	num.	-0.027	no	RF out-of-bag R² < 0.3
boston	359950	506	14	506	14	num., cat.	0.799	yes	
Moneyball	167210	1232	15	114	14	num., cat.	0.818	yes	
Mercedes_Benz_Greener_Manufacturing	233215	4209	378	4208	366	num., cat.	0.573	yes	
Santander_transaction_value	233214	4459	4993	4417	4737	num.	0.147	no	RF out-of-bag R² < 0.3
Yolanda	317614	400000	101	399558	101	num.	0.126	no	RF out-of-bag R² < 0.3
house_sales	359949	21613	23	21422	23	num., cat.	0.660	yes	
Brazilian_houses	359938	10692	13	10671	13	num., cat.	0.917	yes	
topo_2_1	359939	8885	267	8884	262	num.	0.031	no	RF out-of-bag R² < 0.3
black_friday	359937	166821	10	166821	10	num., cat.	0.367	yes	
elevators	359936	16599	19	16361	19	num.	0.434	yes	
Allstate_Claims_Severity	233212	188318	132	187027	132	num., cat.	0.371	yes	
nyc-taxi-green-dec-2016	359943	581835	19	577601	19	num., cat.	0.477	yes	
Buzzinsocialmedia_Twitter	233213	583250	78	541091	78	num.	0.841	yes	
pol	359946	15000	49	15000	27	num.	0.885	yes	
colleges	359942	7063	45	NA	NA	num., cat.	NA	no	too many missings
house_prices	359951	1460	80	NA	NA	num., cat.	NA	no	too many missings
QSAR-TID-11	360932	5742	1026	NA	NA	num., cat.	NA	no	problems with OpenML R package
QSAR-TID-10980	360933	5766	1026	NA	NA	num., cat.	NA	no	problems with OpenML R package
Airlines_DepDelay_10M	359929	10000000	10	9374500	10	num., cat.	0.037	no	RF out-of-bag R² < 0.3
OnlineNews	359941	39644	60	NA	NA	num.	NA	no	problems with OpenML R package",
  sep = "\t", quote = "", check.names = FALSE, stringsAsFactors = FALSE
)
stopifnot(nrow(table_b2) == 33L, !anyDuplicated(table_b2[["Task ID"]]))

write.csv(table_b2, file.path(out_dir, "benchmark_data_overview.csv"),
          row.names = FALSE, na = "NA")
latex_table <- table_b2
latex_table[["R² of RF"]] <- ifelse(is.na(table_b2[["R² of RF"]]), "NA",
                                   sprintf("%.3f", table_b2[["R² of RF"]]))
names(latex_table)[8L] <- "OOB R-squared"
latex <- knitr::kable(
  latex_table, format = "latex", booktabs = TRUE, longtable = TRUE, row.names = FALSE,
  align = c("l", rep("r", 5L), "l", "r", "c", "l"),
  caption = "Overview of all datasets from OpenML AutoML Benchmark Regression Collection 269.",
  label = "benchmark_data_overview", linesep = "", escape = TRUE
)
latex_lines <- strsplit(as.character(latex), "\n", fixed = TRUE)[[1L]]
latex_lines <- sub("{lrrrrrlrcl}", "{p{3.6cm}rrrrrp{1.2cm}rcp{3cm}}", latex_lines, fixed = TRUE)
latex_lines <- gsub("\\_", "\\_\\allowbreak ", latex_lines, fixed = TRUE)
latex_lines <- sub("RF out-of-bag R² < 0.3", "RF out-of-bag $R^2 < 0.3$", latex_lines, fixed = TRUE)
top_rule <- match("\\toprule", latex_lines)
head <- latex_lines[seq.int(top_rule, top_rule + 2L)]
latex_lines <- append(latex_lines, c("\\endfirsthead", head, "\\endhead"),
                      after = top_rule + 2L)
writeLines(c("\\begin{landscape}", "\\begingroup", "\\scriptsize",
             "\\setlength{\\tabcolsep}{3pt}", latex_lines,
             "\\endgroup", "\\end{landscape}"),
           file.path(out_dir, "benchmark_data_overview.tex"))
