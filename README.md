# Concept
This project aim is to develop a smart environment that controls industrial machinery and dynamically adjusts environmental control tools through actuators.

In AI-Enhanced Industry sensors are a main tool to gather information about single-machine state, this information can be refined and used by intelligent system not only to control the environment but also to prevent malfunctions to happen. This project connects machine-specific ContikiOS-Supported sensors to a Java-based Server that, basing on sensors state decides which actuators to activate (machine cooling level, air conditioning in heat/cooling mode) and what to switch-off for energy saving.

This project simulates a sensor-based environment using Contiki-OS simulation tools and randomly generated data, to trigger virtual actuators symbolically representing those devices. Different LEDs combinations are used as a representation of the state for machines and actuators.

For further details check the [project's documentation](IoT_Report.pdf)