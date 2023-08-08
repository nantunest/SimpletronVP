#include <systemc.h>
#include <vector>

#include "ram.h"
#include "rom.h"
#include "mux.h"
#include "gpio.h"
#include "timer.h"
#include "pwm.h"

#include "simpletron.h"

// Basic RAM and ROM usage
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

// Timer usage
std::vector<unsigned> prog2 = {
    0x1499, // l00 READ     0x499 - Read Timer modulus (Tm)
    0x1498, // l01 READ     0x498 - Read Timer mode of operation (1 or 2) (To)
    0x3499, // l02 LOAD     0x499 - Load Tm to acc
    0x4F12, // l03 STORE    0xF12 - Store Tm to modulus register
    0x3498, // l04 LOAD     0x498 - Load To to acc
    0x4F10, // l05 STORE    0xF10 - Store To to timer status register (start timer)
    0x2F13, // l06 WRITE    0xF13 - Write timer counterval to console
    0x9006  // l07 JMP      0x006 - Loop WRITE
};

// Timer and PWM
std::vector<unsigned> prog3 = {
    0x1499, // l00 READ     0x499 - Read Timer modulus (Tm) <= 10
    0x1498, // l01 READ     0x498 - Read Timer mode of operation (1 or 2) (To) <= 2
    0x1497, // l02 READ     0x497 - Read PWM width (Pw) <= 7
    0x1496, // l03 READ     0x496 - Read PWM mode of operation (Pm = 1 start) <= 1

    0x3499, // l04 LOAD     0x499 - Load Tm to acc
    0x4F12, // l05 STORE    0xF12 - Store Tm to modulus register

    0x3497, // l06 LOAD     0x497 - Load Pw to acc
    0x4F21, // l07 STORE    0xF21 - Store Pw to PWM width

    0x3496, // l08 LOAD     0x496 - Load Po to acc
    0x4F20, // l09 STORE    0xF20 - Store Po to PWM mode

    0x3498, // l0A LOAD     0x498 - Load To to acc
    0x4F10, // l0B STORE    0xF10 - Store To to timer status register (start timer)

    0x900C  // l0C JMP      0x00C - Loop forever 
};

int sc_main(int argc, char* argv[]) {
   
    sc_clock clk("clock", 10, sc_core::SC_US, 0.5, 10, sc_core::SC_US);
    sc_signal<int> address;
    sc_signal<int, SC_MANY_WRITERS> data;
    sc_signal<bool> ram_rw; // WE = 0 -> Read, WE = 1 -> Write

    sc_signal<bool> ram_ce;
    sc_signal<bool> rom_ce;
    sc_signal<bool> gpio_ce;
    sc_signal<bool> timer_ce;
    sc_signal<bool> pwm_ce;

    sc_signal<int> gpio_output;
    sc_signal<bool> timer_tick;
    sc_signal<int> timer_counter;
    sc_signal<bool> pwm_out;

    // Open VCD file
    sc_trace_file *wf = sc_create_vcd_trace_file("sim_out");

    // Dump the desired signals
    sc_trace(wf, clk, "sys.clock");
    sc_trace(wf, address, "sys.address");
    sc_trace(wf, data, "sys.data");
    sc_trace(wf, ram_rw, "sys.ram_we");
    sc_trace(wf, rom_ce, "sys.rom_ce");
    sc_trace(wf, ram_ce, "sys.ram_ce");
    sc_trace(wf, gpio_output, "sys.gpio_output");
    sc_trace(wf, gpio_ce, "sys.gpio_ce");
    sc_trace(wf, timer_ce, "sys.timer_ce");
    sc_trace(wf, timer_tick, "sys.timer_tick");
    sc_trace(wf, pwm_out, "sys.pwm_out");

    
    MemoryMux memoryMux("mmux");
    memoryMux.address(address);
    memoryMux.rom_ce(rom_ce);
    memoryMux.ram_ce(ram_ce);
    memoryMux.gpio_ce(gpio_ce);
    memoryMux.timer_ce(timer_ce);
    memoryMux.pwm_ce(pwm_ce);

    Gpio gpio("gpio1");
    gpio.ce(gpio_ce);
    gpio.clk(clk);
    gpio.address(address);
    gpio.data(data);
    gpio.output_pins(gpio_output);

    Timer timer("timer1");
    timer.address(address);
    timer.data(data);
    timer.clk(clk);
    timer.ce(timer_ce);
    timer.tick(timer_tick);
    timer.counter(timer_counter);

    Pwm pwm("pwm1");
    pwm.address(address);
    pwm.data(data);
    pwm.clk(clk);
    pwm.ce(pwm_ce);
    pwm.out(pwm_out);
    pwm.timer_tick(timer_tick);
    pwm.timer_counter(timer_counter);

    Simpletron simpletron("simpletron1");
    simpletron.clk(clk);
    simpletron.address(address);
    simpletron.data(data);
    simpletron.ram_rw(ram_rw);

    Rom rom("rom1");
    rom.address(address);
    rom.data(data);
    rom.clk(clk);
    rom.ce(rom_ce);
    rom_ce.write(false);

    rom.load_prog(prog3);

    Ram ram("ram1");
    ram.address(address);
    ram.data(data);
    ram.write_enable(ram_rw);
    ram.clk(clk);
    ram.ce(ram_ce);
    ram_ce.write(false);

    // Debug signals
    sc_trace(wf, simpletron.accumulator, "simpletron.accumulator");
    sc_trace(wf, simpletron.state, "simpletron.state");
    sc_trace(wf, simpletron.opcode, "simpletron.opcode");
    sc_trace(wf, simpletron.instruction_pointer, "simpletron.instruction_pointer");

    sc_trace(wf, timer.reg_countval, "timer.reg_countval");
    sc_trace(wf, timer.reg_status, "timer.reg_status");

    std::cout << "Starting simulation" << std::endl;

    sc_core::sc_start(1000, sc_core::SC_US);
    sc_close_vcd_trace_file(wf);

    return 0;
}
