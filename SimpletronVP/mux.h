#pragma once
#include <systemc.h>

SC_MODULE(MemoryMux)
{
    sc_in<int> address;
    sc_out<bool> rom_ce;
    sc_out<bool> ram_ce;
    sc_out<bool> gpio_ce;
    sc_out<bool> timer_ce;
    sc_out<bool> pwm_ce;

    static constexpr int rom_addr = 0x000;
    static constexpr int rom_size = 0x400;
    static constexpr int ram_addr = 0x400;
    static constexpr int ram_size = 0xB00;
    static constexpr int gpio_addr = 0xF00;
    static constexpr int gpio_size = 0x010;
    static constexpr int timer_addr = 0xF10;
    static constexpr int timer_size = 0x010;
    static constexpr int pwm_addr = 0xF20;
    static constexpr int pwm_size = 0x010;

    static constexpr int ram_addr_end = ram_addr + ram_size;
    static constexpr int rom_addr_end = rom_addr + rom_size;
    static constexpr int gpio_addr_end = gpio_addr + gpio_size;
    static constexpr int timer_addr_end = timer_addr + timer_size;
    static constexpr int pwm_addr_end = pwm_addr + pwm_size;

    void update()
    {
        rom_ce = false;
        ram_ce = false;
        gpio_ce = false;

        if (address < rom_size)
        {
            rom_ce = true;
        }
        else if ((address >= ram_addr) && (address < ram_addr_end))
        {
            ram_ce = true;
        }
        else if ((address >= gpio_addr) && (address < gpio_addr_end))
        {
            gpio_ce = true;
        }
        else if ((address >= timer_addr) && (address < timer_addr_end))
        {
            timer_ce = true;
        }
        else if ((address >= pwm_addr) && (address < pwm_addr_end))
        {
            pwm_ce = true;
        }
    }

    SC_CTOR(MemoryMux)
    {
        SC_METHOD(update);
        sensitive << address;
    }

};