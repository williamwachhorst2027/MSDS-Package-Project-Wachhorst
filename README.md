# footballAnalytics

An R package for evaluating college football wide receivers,
with a focus on Group-of-Six (G6) to Power-Four (P4) transfers.

## Features

- Calculate catch rate and yards per reception
- Calculate wide receiver evaluation scores
- Compare prospects with historical G6-to-P4 transfers
- Evaluate receiving production and peer percentiles
- Generate HTML scouting reports and visualizations

## Historical Data

Includes 67 historical G6-to-P4 wide receiver transfers
from 2023 through 2025.

## Installation

```r
devtools::install_github(
  "williamwachhorst2027/MSDS-Package-Project-Wachhorst"
)
```

## Example

```r
library(footballAnalytics)

data(model_data)

evaluate_transfer_wr(
  player_name = "Example Receiver",
  grade = 82.5,
  yards_per_snap = 2.75,
  receiving_snaps = 425
)
```

## Limitations

Historical comparisons provide context rather than
precise predictions of future P4 production.
The historical sample is relatively small, and
player evaluation should also incorporate film scouting.
