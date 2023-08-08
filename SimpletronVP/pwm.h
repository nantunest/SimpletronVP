#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Pwm)
{
    sc_in<int> address;
    sc_inout<int> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_in<bool> timer_tick;
    sc_in<int> timer_counter;

    sc_out<bool> out;

    static constexpr int base_address = MemoryMux::pwm_addr;

    static constexpr int status_addr = base_address + 0x00;
    static constexpr int width_addr = base_address + 0x01;

    int reg_status = 0;
    int reg_width = 0;

    // Not addressable
    int reg_countclk = 0;

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
        if (reg_status == 0x01) // Switching 
        {
            if (reg_width == timer_counter)
            {
                out = false;
            }
            
            if (timer_tick)
            {
                out = true;
            }
        }
    }

    SC_CTOR(Pwm)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};

