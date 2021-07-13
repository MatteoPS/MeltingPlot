# MeltingPlot v2.5
### R script for strains clustering/typing and epidemiological investigation using High Resolution Melting (HRM) temperatures.

![MelingPlot](https://skynet.unimi.it/wp-content/uploads/Melting_plot-04_flatten-1024x562.png "MeltingPLot")

### Novel features of v2.5
- dual typing method: HLMT-clustering groups the strains on the basis of melting temperatures without a reference dataset. HLMT-assignment classifies each strain into a Melting Type (MT) by comparing the strains melting temperatures to a reference dataset.
- new isolate barplots divided by cluster
- clearer design



Download the full USER MANUAL:
https://skynet.unimi.it/wp-content/uploads/MeltingPlot/MeltingPlot_Manual.pdf

MeltingPlot can be used online at:
https://skynet.unimi.it/index.php/tools/meltingplot/

MeltingPlot is an R script and requires the R libraries igraph, gplots, xlsx, ggplot2, scales.

To run MeltingPlot on the example file download the example file and the script at:
https://github.com/MatteoPS/MeltingPlot

then launch the command:

``Rscript MeltingPlot_v2.5.R -in MeltingPlot_Example_v2.5.xls -meta y -ref y``


If you are using MeltingPlot for a scientific publication, 
### please cite:
*Perini, M., MeltingPlot, a user-friendly online tool for epidemiological investigation using High Resolution Melting data. BMC Bioinformatics 22, 76 (2021). https://doi.org/10.1186/s12859-021-04020-y*


