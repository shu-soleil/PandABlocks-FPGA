create_generated_clock -name fmc_dac427_inst/THE_ACQ427FMC_DAC_INTERFACE/I -source [get_pins {ps/processing_system7_0/inst/PS7_i/FCLKCLK[0]}] -divide_by 2 [get_pins fmc_dac427_inst/THE_ACQ427FMC_DAC_INTERFACE/clk_62_5M_raw_reg/Q]
set_false_path -from [get_clocks clk_fpga_0] -through [get_pins -hierarchical -filter {NAME=~ "*ACQ427*SEL_CLK_SEL/O"}] -to [get_clocks clk_fpga_0]
