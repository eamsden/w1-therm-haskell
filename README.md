# Haskell w1-therm Reader

This library provides a routing to read 1-Wire thermal sensors supported by the w1-therm Linux kernel module.
It can list the available thermal sensors and query their data.

The included executable gives an example of the library use, and the module documentation should be complete.

The w1-therm module supports sensors such as the ds1820b connected to GPIO pin 4 on e.g. the Raspberry Pi.
This GPIO pin is hardcoded into the kernel module, and changing it requires changing the module source and rebuilding
at least the module. It is not possible to change it from a Haskell library. However, multiple 1-Wire devices may be
attached to the same GPIO pin.
