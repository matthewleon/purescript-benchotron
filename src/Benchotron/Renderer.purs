
module Benchotron.Renderer where

import Benchotron.Core (BenchmarkResult)

foreign import drawGraph :: BenchmarkResult -> String
