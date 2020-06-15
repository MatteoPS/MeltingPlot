# MeltingPlot
### R script for strains clustering/typing and epidemiological investigation using High Resolution Melting (HRM) temperatures.

Citing MeltingPlot:

If you are using MeltingPlot for a scientific publication, please cite:

Perini, M. et al. MeltingPlot: a user-friendly online tool for epidemiological investigation using High Resolution Melting data, 2020
On-line MeltingPlot


Download the full USER MANUAL:
https://skynet.unimi.it/wp-content/uploads/MeltingPlot/MeltingPlot_Manual.pdf
Load the file on the MeltingPlot web page:
https://skynet.unimi.it/index.php/tools/meltingplot/
Download from the web page the .zip file containing all the results or check you email.

Stand-alone MeltingPlot

You can download MeltingPlot tool R script, the example and the template file at:
https://github.com/MatteoPS/MeltingPlot

MeltingPlot is an R script and requires the R libraries igraph, gplots, xlsx, ggplot2, scales.
You can install R as follow:
Windows: Go to https://cloud.r-project.org/bin/windows/base/; Download the installer for the latest R version with the link at the top of the page (.exe file); lunch the R installer.
MacOS: Go to https://cloud.r-project.org/bin/macosx/ Download the installer for the latest R version with the link under “Latest release” (.pkg file); lunch the R installer.
Linux: Go to https://cloud.r-project.org/bin/linux/, select the folder relative to your distribution (Debian, RedHat, Suse…), open the README and follow the instructions.

To run MeltingPlot on the example file download the example file and the script at:
https://github.com/MatteoPS/MeltingPlot
then launch the command:
Rscript MeltingPlot.R -in MeltingPlot_Example.xls -meta y -ref y
