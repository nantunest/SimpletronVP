#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Pwm)
{
    sc_in<unsigned short> address;
    sc_inout<unsigned short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_in<unsigned short> timer_counter;

    sc_out<bool> out;

    static constexpr unsigned short base_address = MemoryMux::pwm_addr;

    static constexpr unsigned short status_addr = base_address + 0x00;
    static constexpr unsigned short width_addr = base_address + 0x01;

    unsigned short reg_status = 0;
    unsigned short reg_width = 0;

    // Not addressable
    unsigned short reg_countclk = 0;

    void update()
    {
        if (ce)
        {
            // Configuration
            if (address == status_addr){
                if (data == 0x01){
                    reg_status = data;
                    out = true;
                }
            } 
            else if (address == width_addr)
            {
                reg_width = data;
            }
        }

        // Operation
        // TODO: Synchronize start of operation with start of timer? 
        if (reg_status == 0x01) // Switching 
        {
            if (timer_counter < reg_width)
            {
                out = true;
            }
            else
            {
                out = false;
            }
        }
    }

    SC_CTOR(Pwm)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};

