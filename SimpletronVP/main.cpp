#include <systemc.h>
#include <vector>

#include "ram.h"
#include "rom.h"
#include "mux.h"
#include "gpio.h"

#include "simpletron.h"

std::vector<unsigned> prog1 = {
    0x1499, // l00 READ      0x499 - Read N1 from console to address 0x0499 (RAM)
    0x1498, // l01 READ      0x498 - Read N2 from console to address 0x0498 (RAM)
    0x3499, // l02 LOAD      0x499 - Load N1 from RAM to acc
    0x6498, // l03 SUB       0x498 - Subtract N2 (RAM) from N1
    0xA007, // l04 BLZ       0x007 - If N2 > N1, goto 0x007
    0x2499, // l05 WRITE     0x499 - Write N1 to console
    0x9008, // l06 JMP       0x008 - Go to end
    0x2498, // l07 WRITE     0x498 - Write N2 to console
    0x4F00, // l08 STORE     0xF00 - Write acc to gpio
    0xC300  // l09 HALT
};

int sc_main(int argc, char* argv[]) {
   
    sc_clock clk("clock", 10, sc_core::SC_US, 0.5, 10, sc_core::SC_US);
    sc_signal<int> address;
    sc_signal<int, SC_MANY_WRITERS> data;
    sc_signal<bool> ram_rw; // WE = 0 -> Read, WE = 1 -> Write
    sc_signal<bool> ram_ce;
    sc_signal<bool> rom_ce;
    sc_signal<bool> gpio_ce;
    sc_signal<int> gpio_output;
    sc_signal<int> simp_state;

    // Debug signals
    sc_signal<int> acc_debug;
    sc_signal<int> state_debug;
    sc_signal<int> opcode_debug;
    sc_signal<int> ip_debug;
    
    // Open VCD file
    sc_trace_file *wf = sc_create_vcd_trace_file("sim_out");

    // Dump the desired signals
    sc_trace(wf, clk, "clock");
    sc_trace(wf, address, "address");
    sc_trace(wf, data, "data");
    sc_trace(wf, ram_rw, "ram_we");
    sc_trace(wf, rom_ce, "rom_ce");
    sc_trace(wf, ram_ce, "ram_ce");
    sc_trace(wf, gpio_output, "gpio_output");
    sc_trace(wf, gpio_ce, "gpio_ce");

    // Debug signals
    sc_trace(wf, acc_debug, "accumulator_debug");
    sc_trace(wf, state_debug, "state_debug");
    sc_trace(wf, opcode_debug, "opcode_debug");
    sc_trace(wf, ip_debug, "ip_debug");

    MemoryMux memoryMux("mmux");
    memoryMux.address(address);
    memoryMux.rom_ce(rom_ce);
    memoryMux.ram_ce(ram_ce);
    memoryMux.gpio_ce(gpio_ce);

    Gpio gpio("gpio1");
    gpio.ce(gpio_ce);
    gpio.clk(clk);
    gpio.address(address);
    gpio.data(data);
    gpio.output_pins(gpio_output);

    Simpletron simpletron("simpletron1");
    simpletron.clk(clk);
    simpletron.address(address);
    simpletron.data(data);
    simpletron.ram_rw(ram_rw);

    // Debug signals
    simpletron.acc_debug(acc_debug);
    simpletron.state_debug(state_debug);
    simpletron.opcode_debug(opcode_debug);
    simpletron.ip_debug(ip_debug);

    Rom rom("rom1");
    rom.address(address);
    rom.data(data);
    rom.clk(clk);
    rom.ce(rom_ce);
    rom_ce.write(false);

    rom.load_prog(prog1);

    Ram ram("ram1");
    ram.address(address);
    ram.data(data);
    ram.write_enable(ram_rw);
    ram.clk(clk);
    ram.ce(ram_ce);
    ram_ce.write(false);

    std::cout << "Starting simulation" << std::endl;

    sc_core::sc_start(500, sc_core::SC_US);
    sc_close_vcd_trace_file(wf);

    return 0;
}
