valves pumping: the valves you want to trigger in the first part of each sequence in order
valves purging: the valves you want to trigger in the 2nd part of each sequence in order
pump times: how long the pump should run from the pumping valve to the outlet valve
measurement times: how long the program should hold between pumping and purging
purge times: how long the pump should run in reverse at the end of the sequence

Steps: If you want to trigger multiple cycles per cleaning signal, set steps to be greater than 1. Useful for automatic cleaning. You might for example use steps = 2 to to run: sample valve, purge, acid wash, purge on each cleaning signal.

Log int: The number of seconds between measurements

clean int: how frequently the cleaning signal fires relative to measurements. Set this to two to have the cleaning signal fire for only half the measurements

clean dur: How long the cleaning pulse lasts. Leave it at 5.

clean wait. How long before the measurement the cleaning signal fires. This should be roughly equal to the longest pump time in the program.

mux on: leave this at 1 if running a multiplexer. set to  0 if running just a remote monitor.

AutoReset: if set to 1, the system will reboot itself at midnight. If set to 0 it will not. UNTESTED. For FCR, leave at 0 because Mia will handle the rebooting signal.