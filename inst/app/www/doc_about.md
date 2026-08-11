## About
---

*This website and its contents herein, including all data, mapping, charts, and analysis, are provided strictly for educational reasons and research purposes.*

#### **Cause of death changes and their effect on life expectancy**
The life expectancy monitoring tool allows the user to select mortality changes over the entire lifespan or at specific ages, as well as for overall mortality or for specific causes of death. For example, how would life expectancy look if cardiovascular mortality were to be reduced by 50%? Or how would life expectancy look if infant mortality were eliminated? The tool facilitates assessing changes and comparisons in life expectancy under those selected scenarios of mortality change. Furthermore, the tool lets the user compare cause-of-death profiles and life expectancies across time, countries and sexes.

#### **Life expectancy changes in achieving SDG3**
The Sustainable Development Goals (SDGs) were set in 2015 with many specific targets to be achieved by 2030. The third SDG refers to “Ensure healthy lives and promote well-being for all at all ages” [https://sdgs.un.org/goals/goal3]. The targets of this goal refer to several actions of either eliminating or reducing mortality from certain diseases that are amenable to health interventions. Hence, the possible effects on life expectancy of achieving those mortality reductions and eliminations are highly relevant. The life expectancy monitor tool allows analysts to evaluate countries’ progress in achieving the SDGs as well as assessing possible specific scenarios or targets that a population wishes to achieve. 

---
#### Updates and news

**Monitor Version:** 1.6.0

**Last Update:** 2026-08-12

**News in previous versions:**
- 2026-08-12 - v1.6.0 - Figure 3 panel labels and colour consistency across figures 3 and 4; figure 4 is now built entirely with native plotly, dropping the ggplot2 dependency;
- 2026-08-11 - v1.5.0 - Performance release: the app is leaner under the hood and all charts now render natively with plotly for smoother interactivity;
- 2026-08-11 - v1.4.1 - Bug fix: the app now opens reliably even in a fresh R session;
- 2026-08-10 - v1.4.0 - Added an automated testing suite and fixed two bugs in the data processing and decomposition results;
- 2026-08-10 - v1.3.1 - Data access release: the datasets are now loaded via accessor functions, cutting the app size roughly in half;
- 2026-08-10 - v1.3.0 - Startup performance release: the app now starts and loads data about 18x faster, with a loading indicator shown while the data loads;
- 2026-08-10 - v1.2.0 - Native plotly rendering for figures 2-4 (faster and smoother), dashboard charts fill the window and resize responsively, figure 3 labels spaced off the y-axis;
- 2026-08-10 - v1.1.0 - Performance release: single-debounce pipeline, data.table pre-conversion, cached queries, faster interactions;
- 2025-12-22 - v1.0.5 - Improve dashboard stability;
- 2025-12-22 - v1.0.0 - First stable release;
- 2025-10-19 - v0.27.1 - Implement Shiny Dependency Management;
- 2025-06-11 - v0.25.0 - Implement Shiny Bootstrap 5 user interface for improved security;
- 2025-05-05 - v0.23.0 - Update application and library to GBD2021 data;
- 2024-01-16 - v0.15.1 - experiment with a second SDG mode that allows changes in individual causes of death; 
- 2023-11-15 - v0.14.2 - UI bug fix; 
- 2023-11-01 - v0.14.1 - Fix bug related under five and maternal mortality data; 
- 2022-06-02 - v0.13.0 - Include the maternal and neonatal mortality in the SDG section;
- 2022-04-26 - v0.12.2 - Develop server side scaling solution, Postgres SQL etc;
- 2022-03-17 - v0.11.0 - Add new functionality (bookmarks, reset) and provide a more consistent arrangement of the cod with the ICD classification; 
- 2022-02-23 - v0.10.0 - Create datatab corresponding to dashboard figures (life tables, cod distributions and decomposition values); 
- 2022-02-10 - v0.9.0 - Update database by adding macro-regions; 
- 2021-12-09 - v0.8.0 - Change the name of the R library from {MortalityCauses} to {lemur} and add dashboard documentation.
- 2021-06-03 - v0.4.0 - Important advance in app functionality and figure coordination.
- 2021-05-12 - v0.3.0 - Start dashboard development.
- 2021-04-08 - v0.2.0 - Add the life expectancy decomposition method.
- 2021-03-29 - v0.1.0 - Add the modified life table method.
- 2021-03-01 - v0.0.1 - Start of the project implementation.


The source code and the development repository can be found on GitHub [@mpascariu/lemur](https://github.com/mpascariu/lemur) under the [GNU GPLv3](https://github.com/mpascariu/lemur/blob/main/LICENSE) license.