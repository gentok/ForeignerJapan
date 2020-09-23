# Can University Education Cultivate Immigrant Integration? The Case of Local Enfranchisement for Foreign Residents in Japan

[Working paper (9/23/2020)](paper/Kato2020caun_v4.2_090320.pdf)

[Online Appendix (9/3/2020)](paper/Kato2020caun_v4.1_090320_appendix.pdf)

[Presentation Slides (9/12/2020)](paper/Kato2020caun_v4.1_slides_apsa_handout_091220.pdf)

### Authors 

Gento Kato (Nazarbayev University) *gento.katoATnu.edu.kz*

Fan Lu (Queen's University) *fan.luATquensu.ca*

### Abstract
While there is lively debate on whether higher education cultivates support for admitting foreigners into North America and Europe, there is little discussion on the extent to which this relationship generalizes beyond these continents. In light of Japan's growing reliance on foreign workers to solve its labor shortage problem and the public’s divided opinion on integrating foreigners, we examine the relationship between university education and Japanese attitudes toward local enfranchisement for foreign residents. We use matching techniques that incorporate individual attributes as well as residential zip codes to analyze public opinion surveys fielded between 2009 and 2014. Our findings suggest Japanese university education has limited effect on support for enfranchising permanent resident foreigners, and if there is any, it is more visible among females than males. Furthermore, it is mediated through improved attitudes toward Korea, the country of origin for most permanent resident foreigners, rather than attitudes toward China or more liberal political ideology.

### Dataset

* **Survey on the Image of Foreign Countries and Current Topics (SIFCCT)**
    * The original datasets are available from SSJDA website ([https://csrda.iss.u-tokyo.ac.jp/en/](https://csrda.iss.u-tokyo.ac.jp/en/)), survey number 0979 and 0980.
    * Zip code dataset is not published through SSJDA. Available upon request from the authors.

* **UTokyo-Asahi Survey (UTAS)**
    * The original datasets are available from the survey website ([http://www.masaki.j.u-tokyo.ac.jp/utas/utasindex.html](http://www.masaki.j.u-tokyo.ac.jp/utas/utasindex.html)). 2009, 2012, and 2014 waves are used in this study.

To make replication codes work, original datasets must be stored under <code>data/original/</code> with names <code>panel_wave1-12.csv</code>, <code>panel_wave13-24.csv</code> (for SIFCCT), <code>2009_2010utas130816.sav</code>, <code>2012-2013UTASV131129.csv</code>, and <code>2014_2016UTASV20161004.csv</code> (for UTAS).

### Code Files (in R)

To replicate analysis, clone (or download and extract the zip folder of) repository and run each file (working directory will be automatically set for most of editors):

* Data Modification (SIFCCT)

    1. Recoding: [Code](data_sifcct_1_recode_v4.R)|[Output](data_sifcct_1_recode_v4.md)

    2. Matching (Young): [Code](data_sifcct_2_matching_young_v4.R)|[Output](data_sifcct_2_matching_young_v4.md)

    3. Matching Visualization: [Code](data_sifcct_3_matching_young_visual_v4.R)|[Output](data_sifcct_3_matching_young_visual_v4.md)

* Data Modification (UTAS)

    1. Recoding: [Code](data_utas_1_recode_v4.R)|[Output]((data_utas_1_recode_v4.md))

* Analysis 

    1. Main Descriptive Statistics: [Code](src/analysis_0_descriptives_v4.R)|[Output](src/analysis_0_descriptives_v4.md)
    
    2. Main Analysis with Unmatched Data: [Code](src/analysis_1_original_v4.R)|[Output](src/analysis_1_original_v4.md)
    
    3. Main Analysis with Matched Data: [Code](src/analysis_2_matched_v4.R)|[Output](src/analysis_2_matched_v4.md)  

    4. Mediation Analysis with Feeling Towards Korea: [Code](src/analysis_3_mediationKOR_v4.R)|[Output](src/analysis_3_mediationKOR_v4.md) 

    5. Mediation Analysis with Feeling Towards China: [Code](src/analysis_3b_mediationCHN_v4.R)|[Output](src/analysis_3b_mediationCHN_v4.md)

    6. Mediation Analysis with Ideology: [Code](src/analysis_4_mediationIDE_v4.R)|[Output](src/analysis_4_mediationIDE_v4.md) 
 
    7. Mediation Analysis with LDP Feeling Thermometer: [Code](src/analysis_5_mediationLDP_v4.R)|[Output](src/analysis_5_mediationLDP_v4.md) 

    8. Mediation Analysis with Right Party Support: [Code](src/analysis_6_mediationRPS_v4.R)|[Output](src/analysis_6_mediationRPS_v4.md) 
    
    9. Mediation Analysis with Left Party Support: [Code](src/analysis_7_mediationLPS_v4.R)|[Output](src/analysis_7_mediationLPS_v4.md) 


### Project Structure

* <code>data</code>: Contains datasets (currently empty).
* <code>src</code>: Contains code files and their direct outputs.
* <code>out</code>: Contains output plots and tables.
* <code>paper</code>: Contains working paper.
