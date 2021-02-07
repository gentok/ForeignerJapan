# In Japan, University Education Does Not Increase  Support for Immigrant Integration

[Working paper (2/7/2020)](paper/Kato2021inja_v5.0_020721.pdf)

[Online Appendix (2/7/2020)](paper/Kato2021inja_v5.0_020721_appendix.pdf)

<!-- [Presentation Slides (9/12/2020)](paper/Kato2020caun_v4.1_slides_apsa_handout_091220.pdf) -->

### Authors 

Gento Kato (Nazarbayev University) *gento.katoATnu.edu.kz*

Fan Lu (Queen's University) *fan.luATquensu.ca*

### Abstract
While there is lively debate on whether higher education cultivates support for admitting foreigners into North America and Europe, there is little discussion on the extent to which this relationship generalizes beyond these continents. In light of Japan's growing reliance on foreign workers to solve its labor shortage problem and the public’s divided opinion on integrating foreigners, we examine the relationship between university education and Japanese attitudes toward local enfranchisement for foreign residents. Using a large-scale online survey with corresponding ZIP codes, we isolate the causal effects of education independent of residential contexts. We further assess mechanisms underlying such effects through causal mediation analysis. Our findings show Japanese university education has limited effect on support for enfranchising permanent resident foreigners. Even though Japan is also a modern democracy with well-developed institutions of higher education that lead to greater financial security, these institutions do not lead to more supportive attitudes toward immigrant integration because they are neither vehicles for liberalization in ideology nor positive affect toward foreigners.

### Dataset

* **Survey on the Image of Foreign Countries and Current Topics (SIFCCT)**
    * The original datasets are available from SSJDA website ([https://csrda.iss.u-tokyo.ac.jp/en/](https://csrda.iss.u-tokyo.ac.jp/en/)), survey number 0979 and 0980.
    * ZIP code and mail-in datasets are not published through SSJDA. Available upon request from the authors.

To make replication codes work, original datasets must be stored under <code>data/original/</code> with names <code>panel_wave1-12.csv</code>,  <code>panel_wave13-24.csv</code>, and <code>sifcct_mail.csv</code>.

### Code Files (in R)

To replicate analysis, clone (or download and extract the zip folder of) repository and run each file (working directory will be automatically set for most of editors):

* Data Modification (SIFCCT, Online)

    1. Recoding: [Code (Main)](data_sifcct_1_recode_v5.R)|[Output (Main)](data_sifcct_1_recode_v5.md);[Code (Movers)](data_sifcct_1x_recode_v5.R)|[Output (Movers)](data_sifcct_1x_recode_v5.md) 

    2. Matching: [Code (Main)](data_sifcct_2_matching_all_v5.R); [Code (Movers)](data_sifcct_3_matching_moved_v5.R)

* Data Modification (SIFCCT, Mail-In)

    1. Recoding: [Code (Main)](data_mail_1_recode_v5.R)|[Output (Main)](data_mail_1_recode_v5.md);[Code (Movers)](data_mail_1x_recode_v5.R)|[Output (Movers)](data_mail_1x_recode_v5.md)

* Analysis (SIFCCT, Online)
    
    1. Main Analysis: [Code (Main)](src/analysis_2_matched_v5.R)|[Output (Main)](src/analysis_2_matched_v5.md); [Code (Movers)](src/analysis_2x_matched_v5.R)|[Output (Movers)](src/analysis_2x_matched_v5.md)

    2. Mediation Analysis with Unmatched Data: [Code](src/analysis_3_mediation_unmatched_v5.R)|[Output](src/analysis_3_mediation_unmatched_v5.md) 

    3. Mediation Analysis with Standard Matching Data: [Code](src/analysis_4_mediation_matchednoL_v5.R)|[Output](src/analysis_4_mediation_matchednoL_v5.md) 

    4. Mediation Analysis with Distance-Adjusted Matching Data (Lambda = 50km): [Code](src/analysis_5_mediation_matchedL50_v5.R)|[Output](src/analysis_5_mediation_matchedL50_v5.md) 

    5. Mediation Analysis with Distance-Adjusted Matching Data (Lambda = 100km): [Code](src/analysis_6_mediation_matchedL100_v5.R)|[Output](src/analysis_6_mediation_matchedL100_v5.md) 

    6. Mediation Analysis with Distance-Adjusted Matching Data (Lambda = 200km): [Code](src/analysis_7_mediation_matchedL200_v5.R)|[Output](src/analysis_7_mediation_matchedL200_v5.md) 
    
    7. Mediation Analysis with Distance-Adjusted Matching Data (Lambda = 350km): [Code](src/analysis_8_mediation_matchedL350_v5.R)|[Output](src/analysis_8_mediation_matchedL350_v5.md) 

* Analysis (SIFCCT, Unmatched Online & Mail-In Combined)

    1. Main Analysis: [Code (Main)](src/analysis_1_original_mail_v5.R)|[Output (Main)](src/analysis_1_original_mail_v5.md); [Code (Movers)](src/analysis_1x_original_mail_v5.R)|[Output (Movers)](src/analysis_1x_original_mail_v5.md)


* Visualization 

    1. Main Descriptive Statistics: [Code](src/visualization_1_descriptive_v5.R)|[Output](src/visualization_1_descriptive_v5.md)

    2. Matching: [Code](src/visualization_2_matching_v5.R)|[Output](src/visualization_2_matching_v5.md)

    3. Analysis: [Code](src/visualization_3_analysis_v5.R)|[Output](src/visualization_3_analysis_v5.md)

### Project Structure

* <code>data</code>: Contains datasets (currently empty).
* <code>src</code>: Contains code files and their direct outputs.
* <code>out</code>: Contains output plots and tables.
* <code>paper</code>: Contains working paper.
