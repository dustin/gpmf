# gopro metadata parser for haskell

Recent GoPro cameras record a telemetry stream along with video that
contains quite a rich selection of data.

This library parses that stream and provides low level access to that
data (as well as some high level access to come common/tedious
parts).  [GoPro's own project][gpmfdocs] documents the format and
features therein.

[gpmfdocs]: https://github.com/gopro/gpmf-parser
