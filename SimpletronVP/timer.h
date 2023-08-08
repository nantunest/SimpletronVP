#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Timer)
{
    sc_in<int> address;
    sc_inout<int> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    sc_out<bool> tick;
    sc_out<int> counter;

    static constexpr int base_address = MemoryMux::timer_addr;

    static constexpr int status_addr = base_address + 0x00;
    static constexpr int prescalar_addr = base_address + 0x01;
    static constexpr int modulus_addr = base_address + 0x02;
    static constexpr int countval_addr = base_address + 0x03;

    int reg_status = 0;
    int reg_prescalar = 1;
    int reg_modulus = 0;
    int reg_countval = 0;

    // Not addressable
    int reg_countclk = 0;

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
