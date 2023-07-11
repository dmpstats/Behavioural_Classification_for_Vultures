# Behavioural Classification for Avian Species

MoveApps

Github repository: *github.com/callumjclarke/Behavioural_Classification_for_Avian_Species*

## Description

This MoveApp applies a simple behavioural classification model to bird data. It classifies behaviour into one of four classes: `Feeding`, `Roosting`, `Resting` and `Travelling`. Subdivision of these four behaviours assists in clustering processes.

Although this App by default uses a standardised model trained on *Gyps africanus* vulture data, the option is provided to upload models trained using data specific to the animals in the input data. This model can be generated using the `Fit Speed-as-Time-of-Day Model` MoveApp and uploaded manually.

## Documentation

This MoveApp is designed primarily for use with carcass-scavenging birds of prey. Provided models are based on *Gyps africanus* (white-backed vulture) data; if applying this MoveApp to data related to a different species, it is strongly recommended that you use the **Fit Speed-as-Time-of-Day Model** MoveApp to generate suitable models.

Behavioural classification is performed on GPS data only. The first stage of classification is based on speed (calculated using the lagged event), and predicts one of three behaviours:

-   Speed below the travellinng threshold and timestamp within roosting hours: `Roosting`

-   Speed below the travelling threshold and timestamp outside of roosting hours: `Resting`

-   Speed above the travelling threshold: `Travelling`

No `Feeding` behaviour is predicted within the first stage of classification.

The second stage of classification calls the (provided *or* uploaded `Fit Speed-as-Time-of-Day` models and calculates cumulative runs of stationary behaviours (`Resting` and `Roosting`). Reclassification is performed to generate `Feeding` behaviour:

-   Points within the lowest 5 percentiles of predicted movement based on time of day are reclassified as `Feeding`

-   Points within the highest 5 percentiles of time spent stationary are reclassified as `Feeding`

### Input data

Move2 location object

### Output data

Move2 location object

### Artefacts

*None.*

### Settings

`Start of Roosting Hours` (integer): The predicted hour beyond which this species will be roosting.

`End of Roosting Hours` (integer): The predicted hour at which the species' night-roost will end.

`Upper speed bound for Stationary Behaviour`: The speed (in km/h) beyond which this species is assumed to be travelling. Default is 3 km/h.

### Most common errors

-   This MoveApp is designed for species that roost at night. If the species is nocturnal (i.e. the provided *start* of roosting hours is earlier than the provided *end* of roosting hours), classifications will be inaccurate

-   The first tag associated with each ID cannot be classified, as there is no lagged event to allow speed or time calculations

### Null or error handling

-   Settings `Start/End of Roosting Hours`: If not provided, an estimated start time of 6pm and end time of 7am will be assumed. If these are provided but invalid hours (i.e. \<0 or \>24), the input is returned with a warning

-   Setting `Upper Speed Bound for Stationary Behaviour`: If less than zero or not a double, the input is returned with a warning

-   Empty datasets are returned with a warning (there is nothing to classify)
