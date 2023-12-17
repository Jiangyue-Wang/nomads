---
output:
  pdf_document: default
  html_document: default
---
# nomads

Exploring the pattern and drivers of nomadism in large solitary carnivores

## Function explanation

### [*step1*](src/FUNCTION-step1.R) Fix time window length

-   **Description**

This function is to find the fixed time window length for a resident movement track.

-   **Usage**

time_lag_var \<- step1(mvtrk, window = 1:30, method = "smooth")

-   **Arguments**

**mvtrk:** A dataframe for one animal individual, containing columns named "x", "y", and "Time". "x" and "y" should be numeric vectors, representing the coordinates of tracking locations. "Time" should be a lubricate format datetime object containg year, month, day, hour, minute, second.

**window:** Numeric vector, the time window lengths to be calculated.

**method:** Character, specifying the method used to find the fixed time window. (This function is still under construction)

-   **Value**

Returned dataframe will contain two columns, "time_lag" and "var". (This will be further improved, will return a fixed time window length only)

-   **Authors**

J. W. wrote the function; H. B. and J. W. conceptualized the idea.

### [*step2*](src/FUNCTION-step2.R) Calculate oscillation values based on fixed time window length

-   **Description**

This function serves for the plotting the oscillation of used area size for one movement track. Better used with further plotting.

-   **Usage**

disp_outl \<- step2(mvtrk, STBL_PRED = 7)

ggplot(disp_outl, aes(x = time, y = Var, color = as.factor(clusterID))) + geom_point(alpha = 0.5, size = 0.5) + theme_bw() + xlab("Time") + ylab("Variance")

-   **Arguments**

**mvtrk:** A dataframe for one animal individual, containing columns named "x", "y", and "Time". "x" and "y" should be numeric vectors, representing the coordinates of tracking locations. "Time" should be a lubricate format datetime object containg year, month, day, hour, minute, second.

**STBL_PRED:** A number indicating fixed time window. Default set as 7 (days).

-   **Value**

Returned dataframe will classify all movement points to residence or non-residence phase, excluding points within the last 7 (or other values set for PRED STBL) days. Columnes include "time" and "clusterID". Points representing residence phase will be assigned 0 for cluster ID, while other non-residence phases will each have a distinct ID.

-   **Authors**

J. W. wrote the function; H. B. and J. W. conceptulized the idea.

### [*step3*](src/FUNCTION-step3.R) Calculate movement features for each phase

-   **Description**

This function is to calculate the intended movement features of each phase. Residence phase will have resident patch size and resident period. Non-resident phase will have displacement, distance (accumulated steps), and period.

-   **Usage**

features \<- (mvtrk, disp_outl, STBL_PRED = 7)

#see residence phase features 

features[[1]]

#see non-residence phase features 

features[[2]]

-   **Arguments**

**mvtrk:** A dataframe for one animal individual, containing columns named "x", "y", and "Time". "x" and "y" should be numeric vectors, representing the coordinates of tracking locations. "Time" should be a lubricate format datetime object containg year, month, day, hour, minute, second.

**disp_outl:** the returned dataframe of function *step2*, should be in accordance with **mvtrk**, belonging to the same track.

**STBL_PRED:** A number indicating fixed time window. Default set as 7 (days).

-   **Values**

The returned list will have 2 elements. The first one stands for the residence phases, while the second for the non-residence phases.

-   **Authors**

J. W.

### [*fix_step2*](src/FUNCTION-fix_step2.R) fix peak identification caveats

-   **Description**

As there are peaks identified may not be true non-residence phases (negative period value), this function is to delete those "false peaks" and only remain the true non-residence phases.

-   **Usage**

outl_fixed \<- fix_step2(features, disp_outl, STBL_PRED = 7)

features_fixed \<- step3(mvtrk, disp_outl = outl_fixed, STBL_PRED = 7)

-   **Arguments**

**features:** object returned by initial *step3* function.

**disp_outl:** object returned by initial *step2* function.

**STBL_PRED:** A number indicating fixed time window. Default set as 7 (days).

-   **Values**

Returned dataframe will classify all movement points to residence or non-residence phase, excluding points within the last 7 (or other values set for PRED STBL) days. Columnes include "time" and "clusterID_fixed". Points representing residence phase will be assigned 0 for the fixed cluster ID, while other non-residence phases will each have a distinct ID.

-   **Authors**

J. W.
