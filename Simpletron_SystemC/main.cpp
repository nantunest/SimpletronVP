#include <systemc.h>
#include <vector>
#include <fstream>

#include "ram.h"
#include "rom.h"
#include "mux.h"
#include "gpio.h"
#include "timer.h"
#include "pwm.h"
#include "spi.h"
#include "spi_device.h"
#include "mpu6000.h"

#include "simpletron.h"

// Basic RAM and ROM usage
std::vector<unsigned short> prog1 = {
    0x1499, // l00 READ      0x499 - Read N1 from console to address 0x0499 (RAM)
    0x1498, // l01 READ      0x498 - Read N2 from console to address 0x0498 (RAM)
    0x3499, // l02 LOAD      0x499 - Load N1 from RAM to acc
    0x6498, // l03 SUB       0x498 - Subtract N2 (RAM) from N1
    0xA007, // l04 BLZ       0x007 - If N2 > N1, goto 0x007
    0x2499, // l05 WRITE     0x499 - Write N1 to console
    0x9008, // l06 JMP       0x008 - Go to end
    0x2498, // l07 WRITE     0x498 - Write N2 to console
    0xC300  // l09 HALT
};

// Timer usage
std::vector<unsigned short> prog2 = {
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
std::vector<unsigned short> prog3 = {
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

// SPI
std::vector<unsigned short> prog4 = {
    0x1499, // l00 READ     0x499 - Read Shift (Sh)
    0x1498, // l01 READ     0x498 - Read Prescalar (Ps)
    0x1497, // l02 READ     0x497 - Read Cmd (Cmd)

    0x3499, // l03 LOAD     0x499 - Load Sh to acc
    0x4F33, // l04 STORE    0xF33 - Store Sh to shift reg 

    0x3498, // l05 LOAD     0x498 - Load Ps to acc
    0x4F31, // l06 STORE    0xF31 - Store Ps to prescalar reg 

    0x3497, // l07 LOAD     0x497 - Load Cmd to acc
    0x4F32, // l08 STORE    0xF32 - Store Cmd to prescalar reg 

    0x9009  // l0C JMP      0x009 - Loop forever 
};

std::vector<unsigned short> read_prog(std::string prog_name)
{
    std::ifstream file(prog_name, std::ios::binary);
    if (!file.is_open())
    {
        std::cerr << "Failed to read the file." << std::endl;
        exit(1);
    }

    std::vector<unsigned short> prog;
    unsigned char buffer[2];  // Buffer to store 2 bytes (1 unsigned short)

    while (file.read(reinterpret_cast<char*>(buffer), sizeof(buffer))) {
        unsigned short value = (static_cast<unsigned short>(buffer[1]) << 8) | buffer[0];
        std::cout << std::hex << value;
        prog.push_back(value);
    }
    std::cout << std::endl;

    file.close();

    return prog;
}

int sc_main(int argc, char* argv[]) {

    if (argc <= 1){
        std::cerr << "Usage simpletron [prog_name]" << std::endl;
    }

    auto prog = read_prog(std::string(argv[1]));
   
    sc_clock clk("clock", 10, sc_core::SC_US, 0.5, 10, sc_core::SC_US);
    sc_signal<unsigned short> address;
    sc_signal<unsigned short, SC_MANY_WRITERS> data;
    sc_signal<bool> simp_we; // WE = 0 -> Read, WE = 1 -> Write

    sc_signal<bool> ram_ce;
    sc_signal<bool> rom_ce;
    sc_signal<bool> gpio_ce;
    sc_signal<bool> timer_ce;
    sc_signal<bool> pwm_ce;
    sc_signal<bool> spi_ce;
    sc_signal<unsigned short> gpio_output;
    sc_signal<bool> timer_tick;
    sc_signal<unsigned short> timer_counter;
    sc_signal<bool> pwm_out;

    sc_signal<bool> miso;
    sc_signal<bool> mosi;
    sc_signal<bool> ss;
    sc_signal<bool> sclk;

    // Open VCD file
    sc_trace_file *wf = sc_create_vcd_trace_file("sim_out");

    // Dump the desired signals
    sc_trace(wf, clk, "sys.clock");
    sc_trace(wf, address, "sys.address");
    sc_trace(wf, data, "sys.data");
    sc_trace(wf, simp_we, "sys.");
    sc_trace(wf, rom_ce, "sys.rom_ce");
    sc_trace(wf, ram_ce, "sys.ram_ce");
    sc_trace(wf, gpio_output, "sys.gpio_output");
    sc_trace(wf, gpio_ce, "sys.gpio_ce");
    sc_trace(wf, timer_ce, "sys.timer_ce");
    sc_trace(wf, timer_tick, "sys.timer_tick");
    sc_trace(wf, pwm_out, "sys.pwm_out");
    sc_trace(wf, miso, "sys.miso");
    sc_trace(wf, mosi, "sys.mosi");
    sc_trace(wf, ss, "sys.ss");
    sc_trace(wf, sclk, "sys.sclk");
    sc_trace(wf, simp_we, "sys.simp_we");

    MemoryMux memoryMux("mmux");
    memoryMux.address(address);
    memoryMux.rom_ce(rom_ce);
    memoryMux.ram_ce(ram_ce);
    memoryMux.gpio_ce(gpio_ce);
    memoryMux.timer_ce(timer_ce);
    memoryMux.pwm_ce(pwm_ce);
    memoryMux.spi_ce(spi_ce);

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
    pwm.timer_counter(timer_counter);

    Spi spi("spi1");
    spi.address(address);
    spi.data(data);
    spi.clk(clk);
    spi.ce(spi_ce);
    spi.miso(miso);
    spi.mosi(mosi);
    spi.ss(ss);
    spi.sclk(sclk);
    spi.we(simp_we);

//    SpiDevice spi_device("spidev1");
//    spi_device.ce(ss);
//    spi_device.sclk(sclk);
//    spi_device.miso(miso);
//    spi_device.mosi(mosi);


    Mpu6000 mpu_6000("mpu_6000");
    mpu_6000.ce(ss);
    mpu_6000.sclk(sclk);
    mpu_6000.miso(miso);
    mpu_6000.mosi(mosi);

    Simpletron simpletron("simpletron1");
    simpletron.clk(clk);
    simpletron.address(address);
    simpletron.data(data);
    simpletron.we(simp_we);

    Rom rom("rom1");
    rom.address(address);
    rom.data(data);
    rom.clk(clk);
    rom.ce(rom_ce);
    rom_ce.write(false);

    rom.load_prog(prog);

    Ram ram("ram1");
    ram.address(address);
    ram.data(data);
    ram.write_enable(simp_we);
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

    sc_trace(wf, spi.prescalar_ce, "spi.prescalar_ce");
    sc_trace(wf, spi.shifter_ce, "spi.shifter_ce");
    sc_trace(wf, spi.prescalar.prescalar, "spi.prescalar.prescalar");
    sc_trace(wf, spi.prescalar.sclk, "spi.prescalar.sclk");
    sc_trace(wf, spi.shifter.sclk, "spi.shifter.sclk");
    sc_trace(wf, spi.shifter.mosi, "spi.shifter.mosi");
    sc_trace(wf, spi.shifter.miso, "spi.shifter.miso");
    sc_trace(wf, spi.shifter.shift_reg, "spi.shifter.shift_reg");
    sc_trace(wf, spi.shifter.shift_counter, "spi.shifter.shift_counter");
    sc_trace(wf, spi.shifter.wshift, "spi.shifter.wshift");
    sc_trace(wf, spi.shifter.rshift, "spi.shifter.rshift");
    sc_trace(wf, spi.shifter.busy, "spi.shifter.busy");


    sc_trace(wf, memoryMux.spi_ce, "memoryMux.spi_ce");
    sc_trace(wf, memoryMux.rom_ce, "memoryMux.rom_ce");
    sc_trace(wf, memoryMux.ram_ce, "memoryMux.ram_ce");
    sc_trace(wf, memoryMux.gpio_ce, "memoryMux.gpio_ce");
    sc_trace(wf, memoryMux.timer_ce, "memoryMux.timer_ce");

    sc_trace(wf, mpu_6000.shift_reg, "mpu_6000.shift_reg");
    sc_trace(wf, mpu_6000.miso, "mpu_6000.miso");
    sc_trace(wf, mpu_6000.mosi, "mpu_6000.mosi");

    std::cout << "Starting simulation" << std::endl;

    sc_core::sc_start(6000000, sc_core::SC_US);

    std::cout << "End of Simulation." << std::endl;
    sc_close_vcd_trace_file(wf);

    return 0;
}