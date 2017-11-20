rm -rf ./*.o
ghdl -a --ieee=synopsys -fexplicit pP_defs.vhd
ghdl -a --ieee=synopsys -fexplicit pP_defs-body.vhd
ghdl -a --ieee=synopsys -fexplicit pP.vhd
ghdl -a --ieee=synopsys -fexplicit pP-behav.vhd
ghdl -a --ieee=synopsys -fexplicit pP-unpipelined_single_cycle_rtl.vhd
ghdl -a --ieee=synopsys -fexplicit test.vhd
ghdl -a --ieee=synopsys -fexplicit test-bench.vhd
ghdl -a --ieee=synopsys -fexplicit test_bench_behav-config.vhd
ghdl -a --ieee=synopsys -fexplicit  test_bench_unpipelined_single_cycle_rtl.vhd
ghdl -e --ieee=synopsys -fexplicit  test_bench_unpipelined_single_cycle_rtl

./test_bench_unpipelined_single_cycle_rtl --stop-time=1000ns --vcd=teste.vcd