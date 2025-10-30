# env_marsh_bef
Analyses for: Environmental effects on constructed wetland microbial diversity and function in the context of wastewater management


# Scripts

# Data tables
Organized in two folders depending on whether they were generated directly (Input data) or whether they were obtained from sequence data processed through dada2 and phyloseq as described in the manuscript main text.
## Input data
Metadata_marshstudy.csv: original metadata with molecular information. Contains 121 samples and 14 attributes. Attributes include sample name, barcode sequence, linker primer sequence, Plate number and well, date sampled, sample location, type, date extracted, extraction tube, (duplicate well number and location code).
Envdata_marshstudy.csv: metadata with environmental measurements. Includes 152 samples and 12 attributes. Attributes describe the sample: Date, type (aqueous or sludge), Location, DO, Ammonia, BOD, cBOD, Water temperature, air temperature, precipitation, sample name, dataframe(equivalent to type).
TableDeltaNH3_Div.summary.211.csv
## Phyloseq output files
These files include sequence data used in this manuscript as well as sequence data from a mesocosm described in Grandmont-Lemire 2022 (Masters Thesis).
ASVtable_Marsh12021-07-20.txt: ASV table with sample names in the columns and ASV sequences in the rows. Includes 136 samples and 19951 ASVs
METAtable_Marsh12021-07-20.txt: Metadata table with data from the molecular processing including: barcodes, plate well, but also linked to sample name. Includes 136 samples and 12 attributes.
TAXtable_Marsh12021-07-20.txt: Taxonomic information for each ASV. Includes 19951 ASVs and 6 taxonomic categories (Kingdom, Phylum, Class, Order, Family, Genus)

