#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Pwm)
{
    sc_in<short> address;
    sc_inout<short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_in<short> timer_counter;

    sc_out<bool> out;

    static constexpr short base_address = MemoryMux::pwm_addr;

    static constexpr short status_addr = base_address + 0x00;
    static constexpr short width_addr = base_address + 0x01;

    short reg_status = 0;
    short reg_width = 0;

    // Not addressable
    short reg_countclk = 0;

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

