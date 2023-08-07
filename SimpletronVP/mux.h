#include <systemc.h>

SC_MODULE(MemoryMux)
{
    sc_in<int> address;
    sc_out<bool> rom_ce;
    sc_out<bool> ram_ce;
    sc_out<bool> gpio_ce;

    static constexpr int rom_addr = 0x000;
    static constexpr int rom_size = 0x400;
    static constexpr int ram_addr = 0x400;
    static constexpr int ram_size = 0xB00;
    static constexpr int gpio_addr = 0xF00;
    static constexpr int gpio_size = 0x010;
    static constexpr int timer_addr = 0xF10;
    static constexpr int timer_size = 0x010;

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