#pragma once
#include <systemc.h>

SC_MODULE(Spi)
{
    sc_in<short> address;
    sc_inout<short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_in<bool> miso;
    sc_out<bool> mosi;
    sc_out<bool> sclk;
    sc_out<bool> ss;
    
    static constexpr short base_address = MemoryMux::timer_addr;

    static constexpr short status_addr = base_address + 0x00;
    static constexpr short prescalar_addr = base_address + 0x01;
    static constexpr short command_addr = base_addr + 0x02;
    static constexpr short shift_addr = base_addr + 0x03;
 
    void update()
    {
        if (ce)
        {
            // Configuration
            if (address == command_addr){
                command_addr = data;
            } 
        }

        // Operation
        if (command_addr == 0x01) // Shifting 
        {

        }
    }

    SC_CTOR(Spi)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

}