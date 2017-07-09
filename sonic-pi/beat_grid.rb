# Welcome to Sonic Pi v2.11.1
use_bpm 120
set_sched_ahead_time! 0
use_osc "192.168.8.110", 7123
# TODO Can we sync on a startup message here?

# TODO Does  preloading these live_loops like this work?
# Will these all start before we sync on the 'begin' message below?

live_loop :sample8 do
  times = sync "/osc/beatgrid/sample/8"
  sync :control
  at times do
    sample :bd_haus
  end
end
live_loop :sample7 do
  times = sync "/osc/beatgrid/sample/7"
  sync :control
  at times do
    sample :perc_bell
  end
end
live_loop :sample6 do
  times = sync "/osc/beatgrid/sample/6"
  sync :control
  at times do
    sample :bass_drop_c
  end
end

# TODO Maybe we need to sync the Time.now value (current_time)
sync "/osc/beatgrid/begin"

live_loop :control do
  osc "/beatgrid/loop"
  sync "/osc/beatgrid/loopgo"
  8.times do
    # TODO Maybe we can use 'beat' here?
    osc "/beatgrid/tick", tick
    sleep 1
  end
  ##| sync "/osc/beatgrid/loopend"
end
