{- What is a Project? -}

import Data.Time.Calendar

class DatePeriod p where
  startDay:: p -> Day
  startDay = diffDays endDay  durationDays
  endDay:: p -> Day
  endDay   = addDays startDay durationDays
  durationDays:: p -> Day
  durationDays = diffDays endDay  startDay


