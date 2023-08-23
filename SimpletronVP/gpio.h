#pragma once
#include <systemc.h>
#include "mux.h"

SC_MODULE(Gpio)
{
    sc_in<unsigned short> address;
    sc_in<unsigned short> data;
    sc_in<bool> clk;
    sc_in<bool> ce;
    sc_out<unsigned short> output_pins;

    void update()
    {
        if (ce)
        {
           output_pins = data;
        }
    }

    SC_CTOR(Gpio)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};
