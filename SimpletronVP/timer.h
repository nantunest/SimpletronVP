#include <systemc.h>
#include "mux.h"

SC_MODULE(Timer)
{
    sc_in<int> address;
    sc_in<int> data;
    sc_in<bool> clk;
    sc_in<bool> ce;

    int base_address = MemoryMux.timer_addr;

    int reg_prescalar = 1;
    int reg_modulus = 0;
    int reg_count_value = 0;

    void update()
    {
        if (ce)
        {
        }
    }

    SC_CTOR(Timer)
    {
        SC_METHOD(update);
        sensitive << clk.neg();
    }

};
