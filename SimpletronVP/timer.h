#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Timer)
{
    sc_in<short> address;
    sc_inout<short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_out<bool> tick;
    sc_out<short> counter;

    static constexpr short base_address = MemoryMux::timer_addr;

    static constexpr short status_addr = base_address + 0x00;
    static constexpr short prescalar_addr = base_address + 0x01;
    static constexpr short modulus_addr = base_address + 0x02;
    static constexpr short countval_addr = base_address + 0x03;

    short reg_status = 0;
    short reg_prescalar = 1;
    short reg_modulus = 0;
    short reg_countval = 0;

    // Not addressable
    short reg_countclk = 0;

    void update()
    {
        tick = false;

        if (ce)
        {
            // Configuration
            if (address == status_addr){
                reg_status = data;
                reg_countval = 0;
                reg_countclk = 0;
            } 
            else if (address == prescalar_addr)
            {
                reg_prescalar = data;
            }
            else if (address == modulus_addr)
            {
                reg_modulus = data;
            }
            else if (address == countval_addr)
            {
                data = reg_countval;
            }
        }

        // Operation
        if (reg_status > 0x00) // Counting
        {
            reg_countclk += 1;

            if (reg_countclk % reg_prescalar == 0)
            {
                reg_countval++;
            }

            if (reg_countval == reg_modulus)
            {
                tick = true;

                if (reg_status == 0x01)
                {
                    reg_status = 0x00;
                }
                else if (reg_status == 0x02)
                {
                    // Restart the counter
                    reg_countval = 0;
                    reg_countclk = 0;
                }
            }

            counter = reg_countval;
        }

    }

    SC_CTOR(Timer)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};
