# Behavioural Classification for Vultures

MoveApps

Github repository: *https://github.com/dmpstats/Behavioural_Classification_for_Vultures*

## Description

This MoveApp applies a movement-based behavioural classification to bird tracking data, categorizing location events into one of four classes: `Feeding`, `Roosting`, `Resting` and `Travelling`. These four behaviours may be used in an downstream App for clustering locations (e.g. to find feeding spots).

## Documentation

*[A detailed description of the classification process will soon be provided here]*


<!-- Behavioural classification is performed on GPS location data with the option to incorporate altitude data. The first stage of classification is based on speed, and predicts one of three behaviours:

-   Speed below the travelling threshold and timestamp within roosting hours: `Roosting`
-   Speed below the travelling threshold and timestamp outside of roosting hours: `Resting`
-   Speed above the travelling threshold: `Travelling`

Roosting hours are determined either by sunrise-sunset data generated previously by the **Add Local and Solar Time** MoveApp (using setting `Use Provided Sunrise Hours = TRUE`) or, alternatively, using the `Start/End of Roosting Hours` settings.

No `Feeding` behaviour is classified within this first stage of classification.

If altitude information is available, reclassification is performed. This requires the input data to contain a column named `altitude` - if not present, please use the [`Standardise Formats and Calculate Basic Statistics`](https://github.com/callumjclarke/Standardise_Formats_and_Calculate_Basic_Statistics.git) MoveApp to rename or create this column. Altitude reclassification is performed as follows:

-   A location that is initially assigned to `Resting` but whose altitude change to the next location is increasing is reclassified to `Travelling`
-   A location that is initially assigned to `Resting` but whose altitude change to the next location is decreasing is reclassified to `Travelling` *if* the next location involves further ascent or descent. Otherwise, it remains `Resting`

`Feeding` behaviour is exclusively classified based on runs of stationary behaviour. Locations within the highest 5th percentile of cumulative time spent stationary are reclassified as `Feeding`.

-->

<!-- ### Future plans -->

<!-- There are additional components to this application that will become available once additional MoveApps are created. -->

<!-- 1.  If non-location (accelerometer data) is available, the app will use this information to distinguish resting behaviour from feeding behaviour and re-classify resting accordingly. This will require the user to add an additional app to the workflow between the pre-processing (`Standardise Formats and Calculate Basic Statistics`) and this classification app.-->

<!-- 2.  Individual based behaviour will be used to ascertain if the behaviour at a given time of day is unusual. This is achieved by using a model for each individual based on speed and time of day to make predictions and locations within the lowest 5th percentile of predicted movement based on time of day will be reclassified as `Feeding`. An app will be published to create these individual models and the output object uploaded manually to the workflow to include it here. -->



### MoveApps Worflow Dependencies

This App is strictly dependent on the prior deployment of the App ['Add Local and Solar Time'](https://www.moveapps.org/apps/browser/43272925-cd24-466f-bcb9-844a09f1806b) ([Readme](https://github.com/movestore/Convert-Times)) in the workflow.



### Input data

Move2 location object


### Output data

Move2 location object, with the following key columns in the event dataset:

- `behav`: the estimated behaviour at each given location

- `RULE`: The classification rule determining the behaviour assigned to each given location

- `stationary`: 0 if the bird is estimated to perform a stationary behaviour, 1 otherwise



### Artefacts

-   `behavsummary.csv` - A .csv object summarising the classification output. For each track/bird, the total number of locations assigned to each behavioural group (`SFeeding`, `SResting`, `SRoosting`, `STravelling`).

- If Setting `Create Plots` is selected, two files are generated for each track/bird ID whithin the input data:

  1. *birdtrack_(track ID).png* - a plot of the animal's movements (with behavioural classification overlaid);
  
  2. *speed_hrs_diagnostics - (track ID).png* - a set of diagnostic plots assessing the fitted model describing the stationary-speed given hours-since-sunrise relationship required for the speed-time classification step.



### Settings

**Upper speed bound for Stationary Behaviour** (`travelcut`): Numeric, the speed beyond which this species is assumed to be travelling (unit: km/h). Default is 3 km/h.

**Altitude change threshold** (`altbound`): Numeric, absolute change in altitude between consecutive locations (in meters) beyond which a bird is considered as ascending/descending. For example, setting this to 25 means that the bird is `ascending` when its change in altitude exceeds 25m, and `descending` when it falls below -25m. This setting is used only if column `altitude` is present in the input data.

**Sunrise leeway** (`sunrise_leeway`): Integer, defines the number of minutes before/after **sunrise** marking the start of the 'daytime' period. Negative values (e.g. -5 minutes) move this window before sunrise, while positive numbers indicate a shift after. Used to determine the potential end of the roosting period.

**Sunset leeway** (`sunset_leeway`): Integer, defines the number of minutes before/after **sunset** marking the start of the 'night-time' period. Negative numbers move this window before sunset; positive numbers move it after. Used to define the potential start of the roosting period. Stationary behaviour after this time, and before the next determined *daytime*, is considered as roosting.

**Create Plots** (`create_plots`): Select this option to generate, as artefacts, bird-specific graphs with location plots of behaviourally-classified movements, and diagnostic plots for the stationary-speed given hours-since-sunrise model.

**Keep all generated columns** (`keepAllCols`): Select this option to keep all columns created during the classification process. Recommend for debugging purposes only.



<!-- #### Further Considerations
-->


### Most common errors

-   This MoveApp is designed for species that roost at night. If the species is nocturnal, classifications will be inaccurate or unable to be calculated.

-   This App relies completely on eApp ['Add Local and Solar Time MoveApp'](https://github.com/movestore/Convert-Times/tree/20a1370a9cc3668a2e2034eb49d4155038cb2182) being deployed earlier in the Workflow to ensure essential columns  `sunrise_timestamp` and `sunset_timestamp` available for the classification process. If these columns are not detected in the input dataset, the App will exit abruptly and throw an error.


### Null or error handling

- Setting **Upper Speed Bound for Stationary Behaviour** or **Altitude change threshold** to `NULL` or less than zero, the App exits abruptly and throws an error.

- Empty datasets are returned with a warning (there is nothing to classify).

<!-- -   If an individual has fewer than 10 associated locations within the input data, the second-stage classification is not performed. More data is required for accurate classification, and small datasets can cause inconsistencies during reclassification -->
