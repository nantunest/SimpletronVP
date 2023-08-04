#include <systemc.h>

SC_MODULE(MemoryMux)
{
    sc_in<int> address;
    sc_out<bool> rom_ce;
    sc_out<bool> ram_ce;
    sc_out<bool> gpio_ce;

    void update()
    {
        rom_ce = false;
        ram_ce = false;
        gpio_ce = false;

        if (address < 0x400)
        {
            rom_ce = true;
        }
        else if (address >= 0x400 && address < 0xF00)
        {
           ram_ce = true;
        }
        else if (address == 0xF00)
        {
            gpio_ce = true;
        }
    }

    SC_CTOR(MemoryMux)
    {
        SC_METHOD(update);
        sensitive << address;
    }

};
