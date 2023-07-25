# Behavioural Classification for Vultures

MoveApps

Github repository: *github.com/callumjclarke/Behavioural_Classification_for_Vultures*

## Description

This MoveApp applies a simple behavioural classification to bird data. It classifies behaviour into one of four classes: `Feeding`, `Roosting`, `Resting` and `Travelling`. These four behaviours may be used in an additional MoveApp for clustering (e.g. to find feeding locations).

## Documentation

Behavioural classification is performed on GPS location data with the option to incorporate altitude data. The first stage of classification is based on speed, and predicts one of three behaviours:

-   Speed below the travelling threshold and timestamp within roosting hours: `Roosting`
-   Speed below the travelling threshold and timestamp outside of roosting hours: `Resting`
-   Speed above the travelling threshold: `Travelling`

Roosting hours are determined either by sunrise-sunset data generated previously by the **Add Local and Solar Time** MoveApp (using setting `Use Provided Sunrise Hours = TRUE`) or, alternatively, using the `Start/End of Roosting Hours` settings.

No `Feeding` behaviour is classified within this first stage of classification.

If altitude information is available, reclassification is performed. This requires the input data to contain a column named `altitude` - if not present, please use the [`Standardise Formats and Calculate Basic Statistics`](https://github.com/callumjclarke/Standardise_Formats_and_Calculate_Basic_Statistics.git) MoveApp to rename or create this column. Altitude reclassification is performed as follows:

-   A location that is initially assigned to `Resting` but whose altitude change to the next location is increasing is reclassified to `Travelling`
-   A location that is initially assigned to `Resting` but whose altitude change to the next location is decreasing is reclassified to `Travelling` *if* the next location involves further ascent or descent. Otherwise, it remains `Resting`

`Feeding` behaviour is exclusively classified based on runs of stationary behaviour. Locations within the highest 5th percentile of cumulative time spent stationary are reclassified as `Feeding`.

### Future plans

There are additional components to this application that will become available once additional MoveApps are created.

1.  If non-location (accelerometer data) is available, the app will use this information to distinguish resting behaviour from feeding behaviour and re-classify resting accordingly. This will require the user to add an additional app to the workflow between the pre-processing (`Standardise Formats and Calculate Basic Statistics`) and this classification app.
2.  Individual based behaviour will be used to ascertain if the behaviour at a given time of day is unusual. This is achieved by using a model for each individual based on speed and time of day to make predictions and locations within the lowest 5th percentile of predicted movement based on time of day will be reclassified as `Feeding`. An app will be published to create these individual models and the output object uploaded manually to the workflow to include it here.

### Input data

Move2 location object

### Output data

Move2 location object, with additional columns:

-   `hourmin`: the time of day provided as a double

-   `yearmonthday`: the date provided in `timestamp` with hyphens removed

-   `dist_m`: the distance travelled between consecutive locations

-   `kmph`: the speed between consecutive locations

-   `timediff_hrs`: the time difference between consecutive locations

-   `behav`: an estimated behaviour at each given location

-   `stationary`: 0 if the bird is estimated to perform a stationary behaviour, 1 otherwise

### Artefacts

-   `behavsummary.csv` - A .csv object summarising the classification output. For each ID, the total number of locations assigned to each behavioural group (`Unknown, SFeeding, SResting, SRoosting, STravelling`). **NOTE:** Only one location should be classified as `Unknown` for each ID. If more than one location is classified as `Unknown`, please report this bug
-   `birdtrack.png` - for each ID within the input data, a plot of the animal's movements (with behavioural classification overlaid) is created as an artefact

### Settings

`Start of Roosting Hours` (integer): The predicted hour beyond which this species will be roosting.

`End of Roosting Hours` (integer): The predicted hour at which the species' night-roost will end.

`Upper speed bound for Stationary Behaviour`: The speed (in km/h) beyond which this species is assumed to be travelling. Default is 3 km/h.

`Create Plots` (logical): Select this option to generate a `birdtrack.png` plot artefact for each ID within the input data. See below for details.

`Use Provided Sunrise Hours`: Determines whether to use sunrise-sunset timestamps provided within the input data. **Warning: You *must* have used the 'Add Local and Solar Time' MoveApp earlier in the workflow to create this data.**

`Sunrise leeway`: The number of minutes after (or before, if negative) that *daytime* is considered to begin. *Daytime* is the period in which stationary behaviour is **not** classified as *Roosting*. For example `Sunrise leeway = 10` means that the bird's roost is assumed to end 10 minutes *after* sunrise.

`Sunset leeway`: The equivalent of `Sunrise leeway` for sunset, and determines what time *night-time,* or roosting hours, begin. Stationary behaviour after this time, and before the next determined *daytime*, is classified as *Roosting.*

### Most common errors

-   This MoveApp is designed for species that roost at night. If the species is nocturnal (i.e. the provided *start* of roosting hours is earlier than the provided *end* of roosting hours), classifications will be inaccurate or unable to be calculated.

-   Selecting to `Use Provided Sunrise Hours` without having previously generated the hours using the **Add Local and Solar Time MoveApp** will throw an error

-   The first location associated with each ID cannot be classified, as there is no lagged event to allow speed or time calculations. Therefore, one location (the earliest) for each ID will be classified as `Unknown`

### Null or error handling

-   Settings `Start/End of Roosting Hours`: If not provided, an estimated start time of 6pm and end time of 7am will be assumed. If these are provided but invalid hours (i.e. \<0 or \>24), the input is returned with a warning

-   Selecting to `Use Provided Sunrise Hours` without having previously generated the data using the **Add Local and Solar Time MoveApp** will force this App to default to classifying day and night using the `Start/End of Roosting Hours` settings.

-   Setting `Upper Speed Bound for Stationary Behaviour`: If less than zero or non-numeric, the input is returned with a warning

-   Empty datasets are returned with a warning (there is nothing to classify)

-   If an individual has fewer than 10 associated locations within the input data, the second-stage classification is not performed. More data is required for accurate classification, and small datasets can cause inconsistencies during reclassification
