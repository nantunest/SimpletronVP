#pragma once
#include <systemc.h>

SC_MODULE(MemoryMux)
{
    sc_in<unsigned short> address;
    sc_out<bool> rom_ce;
    sc_out<bool> ram_ce;
    sc_out<bool> gpio_ce;
    sc_out<bool> timer_ce;
    sc_out<bool> pwm_ce;
    sc_out<bool> spi_ce;

    static constexpr unsigned short rom_addr = 0x000;
    static constexpr unsigned short rom_size = 0x400;
    static constexpr unsigned short ram_addr = 0x400;
    static constexpr unsigned short ram_size = 0xB00;
    static constexpr unsigned short gpio_addr = 0xF00;
    static constexpr unsigned short gpio_size = 0x010;
    static constexpr unsigned short timer_addr = 0xF10;
    static constexpr unsigned short timer_size = 0x010;
    static constexpr unsigned short pwm_addr = 0xF20;
    static constexpr unsigned short pwm_size = 0x010;
    static constexpr unsigned short spi_addr = 0xF30;
    static constexpr unsigned short spi_size = 0x010;

    static constexpr unsigned short ram_addr_end = ram_addr + ram_size;
    static constexpr unsigned short rom_addr_end = rom_addr + rom_size;
    static constexpr unsigned short gpio_addr_end = gpio_addr + gpio_size;
    static constexpr unsigned short timer_addr_end = timer_addr + timer_size;
    static constexpr unsigned short pwm_addr_end = pwm_addr + pwm_size;
    static constexpr unsigned short spi_addr_end = spi_addr + spi_size;

    void update()
    {
        rom_ce = false;
        ram_ce = false;
        gpio_ce = false;
        timer_ce = false;
        pwm_ce = false;
        spi_ce = false;

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
        else if ((address >= spi_addr) && (address < spi_addr_end))
        {
            spi_ce = true;
        }
    }

    SC_CTOR(MemoryMux)
    {
        SC_METHOD(update);
        sensitive << address;
    }

};
