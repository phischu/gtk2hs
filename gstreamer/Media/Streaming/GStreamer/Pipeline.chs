-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
module Media.Streaming.GStreamer.Pipeline (
  Pipeline,
  PipelineClass,
  pipelineNew,
  pipelineGetBus,
  pipelineSetClock,
  pipelineUseClock,
  pipelineSetNewStreamTime,
  pipelineGetLastStreamTime,
  pipelineSetAutoFlushBus,
  pipelineGetAutoFlushBus,
  pipelineSetDelay,
  pipelineGetDelay
  ) where

import Control.Monad (liftM)
{#import Media.Streaming.GStreamer.Types#}
import System.Glib.UTFString
import System.Glib.FFI

{# context lib = "gstreamer" prefix = "gst" #}

pipelineNew :: String
            -> IO Element
pipelineNew name =
    withUTFString name {# call pipeline_new #} >>=
        newElement

pipelineGetBus :: PipelineClass pipeline
               => pipeline
               -> IO Bus
pipelineGetBus pipeline =
    {# call pipeline_get_bus #} (toPipeline pipeline) >>= newBus

pipelineSetClock :: (PipelineClass pipeline, ClockClass clock)
                 => pipeline
                 -> clock
                 -> IO Bool
pipelineSetClock pipeline clock =
    liftM toBool $
        {# call pipeline_set_clock #} (toPipeline pipeline) (toClock clock)

pipelineUseClock :: (PipelineClass pipeline, ClockClass clock)
                 => pipeline
                 -> clock
                 -> IO ()
pipelineUseClock pipeline clock =
    {# call pipeline_use_clock #} (toPipeline pipeline) (toClock clock)

pipelineSetNewStreamTime :: PipelineClass pipeline
                         => pipeline
                         -> ClockTime
                         -> IO ()
pipelineSetNewStreamTime pipeline time =
    {# call pipeline_set_new_stream_time #} (toPipeline pipeline) time

pipelineGetLastStreamTime :: PipelineClass pipeline
                          => pipeline
                          -> IO ClockTime
pipelineGetLastStreamTime =
    {# call pipeline_get_last_stream_time #} . toPipeline

pipelineSetAutoFlushBus :: PipelineClass pipeline
                        => pipeline
                        -> Bool
                        -> IO ()
pipelineSetAutoFlushBus pipeline autoFlush =
    {# call pipeline_set_auto_flush_bus #} (toPipeline pipeline) $ fromBool autoFlush

pipelineGetAutoFlushBus :: PipelineClass pipeline
                        => pipeline
                        -> IO Bool
pipelineGetAutoFlushBus pipeline =
    liftM toBool $
        {# call pipeline_get_auto_flush_bus #} (toPipeline pipeline)

pipelineSetDelay :: PipelineClass pipeline
                 => pipeline
                 -> ClockTime
                 -> IO ()
pipelineSetDelay pipeline delay =
    {# call pipeline_set_delay #} (toPipeline pipeline) delay

pipelineGetDelay :: PipelineClass pipeline
                 => pipeline
                 -> IO ClockTime
pipelineGetDelay pipeline =
    {# call pipeline_get_delay #} (toPipeline pipeline)
