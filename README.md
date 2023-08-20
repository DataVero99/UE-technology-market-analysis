# Comparative analysis of European Union countries in terms of the level of technological development in the years: 2011, 2015, 2019.


## Introduction

Every year, the European Commission prepares the European Innovation Scoreboard (EIS), aiming to compare the countries belonging to the European Union in terms of their best innovations. Despite numerous efforts to implement technology and promote technological market development, EU institutions confine the EIS to innovation and rely on various data sources. The objective of this study is to demonstrate the data provided by Eurostat regarding the analyzed market, and based on the collected data, to create rankings of EU countries according to their technological level. Furthermore, the study involves the classification of countries using a specially constructed taxonomic measure and the application of the Ward clustering analysis.

Using data available from Eurostat, potential variables were selected, focusing on the most important ones, as well as those with complete data. These selected variables were first used to construct a taxonomic measure of technological advancement (TMTA). The analysis was conducted for three years: 2011, 2015, and 2019.

Main Hypothesis: Western European countries hold top positions in rankings concerning the level of technological market development.
Specific Hypotheses:
1. Western European countries possess the highest quality products in the technology market.
2. Expenditures on research and development (R&D) serve as the primary indicators of technological advancement in the European market.
3. Germany is the leader in the technology market in Europe.


## Data Characteristics

In order to gather data for the study, two databases directly related to the technological market were utilized. The first database pertains to the digital economy and digital society (Eurostat Database). This implies that the data contained therein primarily concern statistics related to information and communication technologies.

The following variables have been designated for the study as a result:
• 𝑋1 – percentage of households with Internet access. All forms of Internet usage have been taken into account in data acquisition;
• 𝑋2 – a variable analogous to X1, i.e., pertaining to households without Internet access;
• 𝑋3 – percentage of households with broadband Internet access, indicating the possibility to connect to a central hub adapted for xDSL technology; both fixed and mobile connections have been included in the data;
• 𝑋4 – percentage of individuals who have ever used the Internet for private or professional purposes;
• 𝑋5 – conversely to 𝑋4, the percentage of individuals who have never used the Internet for private or professional purposes;
• 𝑋6 – percentage of individuals engaging in online commerce, such as ordering goods or services;
• 𝑋7 – percentage of individuals using government online platforms, e.g., mCitizen, official websites, or electronic patient accounts;
• 𝑋8 – percentage of large, medium-sized, and small enterprises engaging in e-commerce.
Eurostat's statistics in the field of science, technology, and innovation mainly focus on R&D, human resources in technology, and changes occurring in the high-tech industry.

From the second database (Eurostat Database), the following variables have been designated:
• 𝑋9 – variable concerning state expenditures on research and development (R&D) in BERD, presented as a percentage of GDP;
• 𝑋10 – total researchers presented as a percentage of the total employed population; the data pertains to researchers working full-time, i.e., 40 hours per week;
• 𝑋11 – percentage of research and development personnel – representing employment in the technology sector;
• 𝑋12 – share of state budget funds or expenditures on research and development dependent on the income plans of respective countries;
• 𝑋13 – percentage of individuals employed in high-tech manufacturing sectors and knowledge-intensive service sectors; these sectors are characterized by a high level of innovation;
• 𝑋14 – human resources in science and technology – the percentage of individuals employed in fields involving the creation or application of knowledge in scientific and technical areas.


# Empirical Analysis Results

## Country Rankings and Their Consistency Over Time

The first method applied in the study involves calculating the taxonomic measure of technological advancement - TMTA. This process resulted in rankings of European Union countries based on technological advancement for the years 2011, 2015, and 2019.

The interpretation of the results primarily focused on references to Western European countries, with a particular emphasis on Germany, to address the validity of the previously formulated research hypotheses.

In connection with the above, it's important to list the Western European countries according to the United Nations Statistics Division. The Western European countries include (Statistics Division of the United Nations): Austria, Belgium, France, Germany, Liechtenstein, Luxembourg, Monaco, Netherlands, and Switzerland.

Rankings of European Union Countries Using Ordered TMTA Values
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/table1.png)

The top three positions in the rankings remained consistently occupied by Germany, Denmark, and Sweden in the examined years. Undoubtedly, these countries are widely recognized as highly developed in the field of technology in Europe.

Germany consistently held the first place in the ranking throughout the studied period. This indicates no grounds for rejecting the third specific hypothesis. Germany is the leader in the technology market in Europe.

The last two positions alternately belong to Bulgaria and Romania. This is likely influenced by the fact that they are Eastern European countries, indicating that the systemic transformation after 1989 was ineffective. This undoubtedly had a negative impact on their technological development trajectory. Bulgaria and Romania are also considered the poorest countries in Europe.

According to the United Nations division, the ranking of countries simultaneously belonging to the Union and Western Europe is as follows:
- In 2011: 1. Germany, 5. France, 6. Netherlands, 8. Belgium, 9. Austria, 10. Luxembourg,
- In 2015: 1. Germany, 6. Netherlands, 7. France, 8. Austria, 9. Belgium, 12. Luxembourg,
- In 2019: 1. Germany, 6. Netherlands, 7. Belgium, 8. Austria, 9. France, 15. Luxembourg.

The above-mentioned countries consistently maintained similar or identical positions in the rankings over the examined years. An exception is Luxembourg, which dropped by two positions in 2015 compared to 2011 and by three positions in 2019 compared to 2015.

Therefore, there is no basis for rejecting the main hypothesis. Western European countries occupy top positions in rankings concerning the level of technological market development.


## Spatial Distributions of TMTA

Another aspect of the conducted analysis is the spatial distributions of the taxonomic measure of technological advancement among European Union countries in the examined years.

A map depicting the spatial distribution and clustering of TMTA values in the year 2011.
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/TMTA2011.png)

On the presented clustering map of TMTA values in 2011, countries were assigned to specific groups as follows:

- Group 1 (Less than Median-Q): Romania, Bulgaria, Greece, Cyprus
- Group 2 (From Median-Q to Median): Portugal, Italy, Malta, Croatia, Hungary, Poland, Lithuania, Latvia
- Group 3 (From Median to Median+Q): Spain, Ireland, Czech Republic, Slovakia, Slovenia, Estonia
- Group 4 (More than Median+Q): United Kingdom, France, Belgium, Luxembourg, Netherlands, Denmark, Austria, Sweden, Finland.

An uncolored area appeared on the map, indicating that the TMTA index (i.e., 0.7138) for Germany is significantly higher than the index values of other countries. This area should be considered as an outlier from the rest.

A map depicting the spatial distribution and clustering of TMTA values in the year 2015.
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/TMTA2015.png)

In the clustering map of TMTA values for 2015, countries were assigned to specific groups as follows:

- Group 1 (Less than Median-Q): Portugal, Romania, Bulgaria, Greece, Cyprus
- Group 2 (From Median-Q to Median): Italy, Malta, Poland, Lithuania, Latvia, Hungary, Croatia
- Group 3 (From Median to Median+Q): Spain, Czech Republic, Slovakia, Slovenia, Luxembourg
- Group 4 (More than Median+Q): Ireland, United Kingdom, France, Belgium, Netherlands, Denmark, Sweden, Finland, Estonia, Austria.

Similarly, a 'white' area representing Germany appeared on the map for 2015. In this case, the TMTA index (i.e., 0.7174) for Germany is significantly higher than that of the other countries. This way, an outlier observation has been identified.

A map depicting the spatial distribution and clustering of TMTA values in the year 2019.
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/TMTA2019.png)

In the clustering map of TMTA values for the year 2019, countries were assigned to specific groups as follows:

Group 1 (Less than Median-Q): Portugal, Romania, Bulgaria, Greece, Cyprus
Group 2 (From Median-Q to Median): Italy, Malta, Slovakia, Hungary, Croatia, Lithuania, Latvia
Group 3 (From Median to Median+Q): Ireland, Spain, Poland, Slovenia, Estonia, Luxembourg
Group 4 (More than Median+Q): United Kingdom, France, Belgium, Netherlands, Denmark, Sweden, Finland, Czech Republic, Austria.
Also, in this year, Germany emerged as a leader in the market.

Classification of European Union countries based on TMTA
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/TMTAtable.png)

Based on the classification results of EU countries according to TMTA, it can be concluded that there is a very high convergence between the years 2015 and 2019. The size and composition of all four groups are nearly identical. However, when considering 2015 vs. 2011 and similarly 2019 vs. 2011, the convergence of groups is significantly lower.

The countries that did not change their groups in the analyzed years are:
• Group 1 – Bulgaria, Cyprus, Greece;
• Group 2 – Croatia, Hungary, Italy, Latvia, Lithuania, Malta;
• Group 3 – Czech Republic, Slovenia, Spain;
• Group 4 – Austria, Belgium, Denmark, Finland, France, Netherlands, Sweden, United Kingdom.


## Cluster Analysis using Ward's Method

In Ward's Method, clusters of within-group variance of variables describing objects are formed. The goal is to minimize this within-group variance as much as possible.

In the conducted analysis of the European technology market's level of development, Ward's Method was applied to achieve homogeneous clusters based on established country characteristics. Similar to the clustering based on TMTA values, references were made to the years 2011, 2015, and 2019. Alongside the graphical presentation of the results in the form of dendrograms, the spatial distributions of the determined clusters were shown on maps. The dendrograms illustrate divisions into two, three, and four groups, while the maps highlight divisions into four groups. Both in the description and interpretation of the dendrograms and maps, the focus was limited to the division into the aforementioned four groups.

Clustering of European Union Countries using Ward's Method for the Year 2011
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/dendrogram2011.png)

Classification of European Union Countries Obtained by Ward's Method for the Year 2011
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/Ward2011.png)

On the map and dendrogram of the Ward's Method classification for the year 2011, the countries of the European Union have been assigned to specific groups as follows:

- Group 1: Austria, Belgium, Czech Republic, Denmark, Finland, France, Netherlands, Ireland, Luxembourg, Germany, Sweden, United Kingdom;
- Group 2: Bulgaria, Cyprus, Greece, Romania, Italy;
- Group 3: Croatia, Spain, Lithuania, Malta, Poland, Portugal, Slovakia;
- Group 4: Estonia, Latvia, Slovenia, Hungary.

The first group is characterized by high values of the number of individuals who have ever used the Internet and low values of the number of households without Internet access. In the second group, high values appear in data related to the number of individuals who have never used the Internet, and low values represent the number of enterprises engaging in e-commerce. In the third group, high values dominate in data concerning enterprises engaged in e-commerce and low expenditures on R&D. The fourth group encompasses countries where high values are observed for the number of households without Internet access and low values for households with broadband Internet access. Additionally, low values are present for the number of individuals engaging in online commerce and enterprises involved in e-commerce.

Clustering of European Union Countries using Ward's Method for the Year 2015
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/dendrogram2015.png)

Classification of European Union Countries Obtained by Ward's Method for the Year 2015
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/Ward2015.png)

On the map and dendrogram, the divisions of EU countries for the year 2015 have been marked. The classification of countries obtained by Ward's Method for the year 2015 is as follows:

- Group 1: Austria, Belgium, Czech Republic, Denmark, Estonia, Finland, France, Netherlands, Ireland, Luxembourg, Germany, Sweden, United Kingdom;
- Group 2: Bulgaria, Greece, Poland, Romania, Italy;
- Group 3: Croatia, Cyprus, Lithuania, Portugal, Hungary;
- Group 4: Spain, Latvia, Malta, Slovakia, Slovenia.

The first group brings together countries based on high values of the number of households with Internet access and low values related to employment in technology-related sectors. The next group (Group 2) is characterized by high values regarding the number of households without Internet access and low values in terms of the share of state budget expenditures in R&D. Group 3 is distinguished by high values of the second variable, namely the number of households without Internet access, and low values related to R&D expenditures. The final fourth group has been classified based on high values of the third variable, namely the number of households with broadband Internet access, and low values concerning the number of personnel in R&D.

Clustering of European Union Countries using Ward's Method for the Year 2019
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/dendrogram2019.png)

Classification of European Union Countries Obtained by Ward's Method for the Year 2019
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/Ward2019.png)

The map and dendrogram of the Ward's Method classification for the year 2019 show the assignment to the following individual groups:

- Group 1: Austria, Belgium, Czech Republic, Estonia, France, Spain, Ireland, Luxembourg, Slovenia;
- Group 2: Bulgaria, Greece, Poland, Romania, Italy;
- Group 3: Croatia, Cyprus, Lithuania, Latvia, Malta, Portugal, Slovakia, Hungary;
- Group 4: Denmark, Finland, Netherlands, Germany, Sweden, United Kingdom.

The first group gathers countries due to high values of the sixth variable (number of individuals using online commerce) and low values of the number of individuals who have never used the Internet. In the second group, high values of the number of individuals who have never used the Internet dominate, along with low values of enterprises engaged in e-commerce. The third group is distinguished by high values of the number of households without Internet access and low values concerning the number of individuals using online commerce. In the last group, high values are significantly attributed to the number of households with Internet access and low values to the number of individuals who have never used the Internet.

Based on the above analysis, the second specific hypothesis can be rejected. Research and development (R&D) expenditures are not the main determinant of technological advancement in the European market. Undoubtedly, R&D expenditure is helpful in analyzing the technology market, but a more accurate assertion would be that the Internet is the primary indicator of technological advancement. The vast number of users and the utilization of the Internet in every aspect of human life, as well as socio-economic activities, have made it one of the most crucial modern-day media. Therefore, it's not surprising that the technology market in Europe is evolving thanks to the Internet. Consequently, the European Union conducts Internet Governance, a process involving the development of standards and values for the future development in collaboration with all member states.

The analysis of country classification using the Ward's Method in the three examined years reveals a significant similarity. The highest consistency over the studied period is observed for the first two groups, while the lowest consistency is found in the remaining two.

Countries that remained in the same group throughout the analyzed years are:
- Group 1: Austria, Belgium, Czech Republic, France, Ireland, Luxembourg;
- Group 2: Bulgaria, Greece, Romania, Italy;
- Group 3: Croatia, Lithuania, Portugal.

From this, it can be deduced that most of the movement occurred within the fourth group.


## Cluster analysis using the Ward's Method based on merged cross-sectional time-series data involves grouping entities according to their similarities across various time points.

Based on the merged spatial and temporal data from the three analyzed years, 2011, 2015, and 2019, a dendrogram was constructed. This dendrogram illustrates the classification of EU countries based on variables characterizing the level of development of the European technology market in a spatial-temporal context.

Clustering of European Union countries in the years 2011, 2015, and 2019 using the Ward's Method.
![Table1](https://github.com/DataVero99/UE-technology-market-analysis/blob/main/images/dendrogram.png)

The above analysis indicates that European Union countries are more diverse over time than in space in terms of the examined characteristics. This is due to the fact that quite rarely do the same spatial objects from different years appear in one group. This is a logical conclusion, as the changes occurring in the technology market emerge over time.


# Summary

The overall aim of the study was a comparative analysis of European Union (EU) countries concerning the spatial and temporal development level of the technology market. Employing selected methods, namely the Taxonomic Measure of Technological Advancement (TMTA) and Ward's method, using Eurostat data on EU countries, allowed for the verification of prior hypotheses. This was facilitated by achieving specific goals within the study.

The first specific goal involved creating country rankings for each year within the considered timeframe. The TMTA index revealed that the highest technological advancement was attributed to three countries: Germany, Denmark, and Sweden. These countries consistently held the top three positions throughout the study period. Notably, the 2 lowest ranks were alternated by Bulgaria and Romania. This ranking effectively portrayed the technology market in the European Union.

The subsequent goal focused on assessing changes over time in the established country rankings. Analysis from the third chapter demonstrated that the most significant changes occurred during the 2011-2015 transition, with considerably smaller changes observed between 2015 and 2019. This divergence likely stemmed from European Union funding discussed in the study's introductory chapter. These programs supporting the technology market began to take effect after 2010.

The third detailed goal aimed to classify countries based on taxonomic values and clustering analysis. The main hypothesis positing that Western European countries lead in technology market development was confirmed. This analysis encompassed UN-defined Western European countries, including Austria, Belgium, France, Netherlands, Luxembourg, and Germany, which consistently held positions above 15 in the rankings. Notably, these Western European Union members performed well in the analysis.

The first specific sub-hypothesis, stating that Western European countries offer the highest-quality technology products, found support in the rankings. However, the second hypothesis, linking research and development (R&D) expenditures to technological advancement, wasn't definitively confirmed. While expenditures contribute, they're just one of several factors influencing technological development. The analysis revealed that the main contributing factor was the Internet.

The final hypothesis posited that Germany is the leader in the European technology market, a claim validated by the study. Germany consistently ranked first across the studied years, solidifying its reputation as a technology pioneer in Europe.

The TMTA method effectively identified top countries representing the international technology market. The rankings remained largely consistent across various years (consistent outcomes were identified). While larger changes occurred in clustering, the chi-square test indicated no significant statistical variance. The study's findings offer a broad view of the technology market using Eurostat data. Different datasets, like those from the European Central Bank, might yield different results. Additionally, subjectivity in selecting diagnostic variables arises due to data limitations for EU member states. Expanding the analysis to variables such as artificial intelligence, the sharing economy, or biotechnology would be valuable.
