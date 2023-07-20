# Behavioural Classification for Vultures

MoveApps

Github repository: *github.com/callumjclarke/Behavioural_Classification_for_Vultures*

## Description

This MoveApp applies a simple behavioural classification model to bird data. It classifies behaviour into one of four classes: `Feeding`, `Roosting`, `Resting` and `Travelling`. Subdivision of these four behaviours assists in clustering processes.

There are plans to allow the user to upload classification models trained using data specific to the animals in the input data. These models will be generated using the `Fit Speed-as-Time-of-Day Model` MoveApp [to be published soon] and uploaded manually.

## Documentation

This MoveApp is designed primarily for use with carcass-scavenging birds of prey. Provided models are based on *Gyps africanus* (white-backed vulture) data; if applying this MoveApp to data related to a different species, it is strongly recommended that you use the **Fit Speed-as-Time-of-Day Model** MoveApp to generate suitable models. [This MoveApp is due for release shortly. For now, its components have been removed from this App.]

Behavioural classification is performed on GPS data only with the option to incorporate altitude data. The first stage of classification is based on speed (calculated using the lagged event), and predicts one of three behaviours:

-   Speed below the travelling threshold and timestamp within roosting hours: `Roosting`

-   Speed below the travelling threshold and timestamp outside of roosting hours: `Resting`

-   Speed above the travelling threshold: `Travelling`

No `Feeding` behaviour is predicted within the first stage of classification.

Reclassification is performed if altitude data is available. This requires a column named `altitude` - if not present, please use the [`Standardise Formats and Calculate Basic Statistics`](https://github.com/callumjclarke/Standardise_Formats_and_Calculate_Basic_Statistics.git) MoveApp to rename or create this column. Altitude reclassification is performed as follows:

-   An individual that is initially assigned to `Resting` but whose altitude to the next location is increasing is reclassified to `Travelling`

-   An individual that is initially assigned to `Resting` but whose altitude to the next location is decreasing is reclassified to `Travelling` *if* the next location involves further ascent or desccent. Otherwise, it remains `Resting`

`Feeding` behaviour is then classified based on runs of stationary behaviour. Locations within the highest 5 percentiles of cumulative time spent stationary are reclassified as `Feeding`.

In future iterations, reclassification will be performed to identify further `Feeding` behaviour: locations within the lowest 5 percentiles of predicted movement based on time of day will be reclassified as `Feeding`. This will become available when the `Fit Speed-as-Time-of-Day Model` MoveApp is published.

### Input data

Move2 location object

### Output data

Move2 location object

### Artefacts

-   `behavsummary.csv` - A .csv object containing a table of the track IDs within the dataset against the number of each behaviour classified

### Settings

`Start of Roosting Hours` (integer): The predicted hour beyond which this species will be roosting.

`End of Roosting Hours` (integer): The predicted hour at which the species' night-roost will end.

`Upper speed bound for Stationary Behaviour`: The speed (in km/h) beyond which this species is assumed to be travelling. Default is 3 km/h.

### Most common errors

-   This MoveApp is designed for species that roost at night. If the species is nocturnal (i.e. the provided *start* of roosting hours is earlier than the provided *end* of roosting hours), classifications will be inaccurate and errors may be thrown

-   The first tag associated with each ID cannot be classified, as there is no lagged event to allow speed or time calculations. One location (the earliest) for each ID will be classified as `Unknown`

### Null or error handling

-   Settings `Start/End of Roosting Hours`: If not provided, an estimated start time of 6pm and end time of 7am will be assumed. If these are provided but invalid hours (i.e. \<0 or \>24), the input is returned with a warning

-   Setting `Upper Speed Bound for Stationary Behaviour`: If less than zero or non-numeric, the input is returned with a warning

-   Empty datasets are returned with a warning (there is nothing to classify)

-   If an individual has fewer than 10 associated locations, the second-stage classification is not performed. More data is required for accurate classification, and small datasets can cause bugs during reclassification
